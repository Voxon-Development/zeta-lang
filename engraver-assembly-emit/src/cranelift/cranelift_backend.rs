//! SSA Backend switched to Cranelift code generation with full ADT and interpolation support

use std::cell::RefCell;
use crate::backend::Backend;
use crate::cranelift::{clif_type, cranelift_intrinsics};
use cranelift::prelude::EntityRef;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, AbiParam, Block as ClifBlock, Block, BlockArg, BlockCall, Function as ClifFunction, InstBuilder, JumpTableData, MemFlags, Signature, ValueListPool};
use cranelift_codegen::isa;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as ClifModule};
use cranelift_object::{ObjectBuilder, ObjectModule};
use ir::context::Context;
use ir::hir::StrId;
use ir::ssa_ir::{inst_is_terminator, BasicBlock, BinOp, BlockId, Function, Instruction, InterpolationOperand, Module, Operand, SsaType, Value};
use leapfrog::LeapMap;
use std::collections::HashMap;
use std::rc::Rc;
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::string_pool::VmString;
use crate::cranelift::cranelift_intrinsics::{TargetInfo};

pub struct CraneliftBackend<'a> {
    module: ObjectModule,
    string_data: LeapMap<VmString, ZetaDataId>,
    interp_func: FuncId,
    enum_new: FuncId,
    enum_tag: FuncId,
    func_ids: HashMap<StrId, FuncId, FxHashBuilder>,
    context: Rc<RefCell<Context<'a>>>,
    target: TargetInfo,
    emit_asm: bool
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ZetaDataId(DataId);

impl leapfrog::Value for ZetaDataId {
    fn is_redirect(&self) -> bool {
        false // or whatever your semantics are
    }

    fn is_null(&self) -> bool {
        self.0.index() == 0 // for example
    }

    fn redirect() -> Self {
        ZetaDataId(DataId::new(usize::MAX))
    }

    fn null() -> Self {
        ZetaDataId(DataId::new(0))
    }
}

impl<'a> CraneliftBackend<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>) -> Self {
        let isa = isa::lookup_by_name("x86_64").unwrap()
            .finish(cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder()));
        let builder = ObjectBuilder::new(isa.unwrap(), "zetamir", cranelift_module::default_libcall_names()).unwrap();
        let mut module = ObjectModule::new(builder);

        // Declare runtime helpers
        let mut sig_interp = Signature::new(module.isa().default_call_conv());
        sig_interp.params.push(AbiParam::new(types::I64));
        sig_interp.params.push(AbiParam::new(types::I64));
        sig_interp.returns.push(AbiParam::new(types::I64));
        let interp_func = module.declare_function("__interp", Linkage::Import, &sig_interp).unwrap();

        let mut sig_enum_new = Signature::new(module.isa().default_call_conv());
        sig_enum_new.params.push(AbiParam::new(types::I64));
        sig_enum_new.returns.push(AbiParam::new(types::I64));
        let enum_new = module.declare_function("__enum_new", Linkage::Import, &sig_enum_new).unwrap();

        let mut sig_enum_tag = Signature::new(module.isa().default_call_conv());
        sig_enum_tag.params.push(AbiParam::new(types::I64));
        sig_enum_tag.returns.push(AbiParam::new(types::I64));
        let enum_tag = module.declare_function("__enum_tag", Linkage::Import, &sig_enum_tag).unwrap();

        CraneliftBackend {
            module,
            string_data: LeapMap::new(),
            interp_func,
            enum_new,
            enum_tag,
            func_ids: HashMap::with_hasher(FxHashBuilder),
            context,
            target: TargetInfo { ptr_bytes: 8 },
            emit_asm: true
        }
    }

    fn lower_basic_inst(
        &mut self,
        inst: &Instruction,
        curr_bb: BlockId,
        builder: &mut FunctionBuilder,
        var_map: &mut HashMap<Value, Variable, FxHashBuilder>,
        block_map: &HashMap<BlockId, ClifBlock, FxHashBuilder>,
        phi_param_map: &HashMap<(BlockId, Value), usize, FxHashBuilder>,
        func: &Function,
    ) {
        match inst {
            Instruction::Const { dest, ty: _ty, value } => {
                let var = builder.declare_var(types::I64);
                let val = match value {
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    Operand::ConstBool(b) => builder.ins().iconst(types::I64, if *b { 1 } else { 0 }),
                    Operand::ConstString(s) => {
                        let did = self.get_or_create_string(s);
                        let gv = self.module.declare_data_in_func(did, &mut builder.func);
                        builder.ins().global_value(types::I64, gv)
                    }
                    Operand::Value(v) => {
                        let vv = var_map.get(v).expect("use of undefined value in Const operand");
                        builder.use_var(*vv)
                    }
                    &Operand::FunctionRef(_) | &Operand::GlobalRef(_) => todo!(),
                };
                builder.def_var(var, val);
                var_map.insert(*dest, var);
            }

            Instruction::Binary { dest, op, left, right } => {
                let var = builder.declare_var(types::I64);
                let l = match left {
                    Operand::Value(v) => {
                        let vv = var_map.get(v).expect("use of undefined value in binary left");
                        builder.use_var(*vv)
                    }
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => todo!(),
                };
                let r = match right {
                    Operand::Value(v) => {
                        let vv = var_map.get(v).expect("use of undefined value in binary right");
                        builder.use_var(*vv)
                    }
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!(),
                };
                let res = match op {
                    BinOp::Add => builder.ins().iadd(l, r),
                    BinOp::Sub => builder.ins().isub(l, r),
                    BinOp::Mul => builder.ins().imul(l, r),
                    BinOp::Div => builder.ins().sdiv(l, r),
                    BinOp::Mod => builder.ins().srem(l, r),
                    BinOp::Eq => builder.ins().icmp(IntCC::Equal, l, r),
                    BinOp::Ne => builder.ins().icmp(IntCC::NotEqual, l, r),
                    BinOp::Lt => builder.ins().icmp(IntCC::SignedLessThan, l, r),
                    BinOp::Le => builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r),
                    BinOp::Gt => builder.ins().icmp(IntCC::SignedGreaterThan, l, r),
                    BinOp::Ge => builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, l, r),
                    BinOp::BitAnd => builder.ins().band(l, r),
                    BinOp::BitOr => builder.ins().bor(l, r),
                    BinOp::BitXor => builder.ins().bxor(l, r),
                    BinOp::ShiftLeft => builder.ins().ishl(l, r),
                    BinOp::ShiftRight => builder.ins().sshr(l, r),
                };
                builder.def_var(var, res);
                var_map.insert(*dest, var);
            }

            Instruction::Phi { dest, incomings: _ } => {
                // For phi lowering we map (block, dest) -> param index, and we read that param
                let idx = phi_param_map
                    .get(&(curr_bb, *dest))
                    .or_else(|| phi_param_map.get(&(curr_bb, *dest))) // redundant but explicit
                    .expect("phi param mapping missing for dest");
                let clif_bb = block_map
                    .get(&curr_bb)
                    .expect("current block missing in block_map for Phi lowering");
                let param = builder.block_params(*clif_bb)[*idx];
                let var = builder.declare_var(types::I64);
                builder.def_var(var, param);
                var_map.insert(*dest, var);
            }

            Instruction::Jump { target } => {
                // collect BlockArg values for target's phi params
                let mut args: Vec<BlockArg> = Vec::new();
                let target_bb_info = func
                    .blocks
                    .iter()
                    .find(|b| b.id == *target)
                    .expect("target block missing");
                for inst in &target_bb_info.instructions {
                    if let Instruction::Phi { incomings, .. } = inst {
                        // find incoming value from curr_bb
                        let mut found = false;
                        for (pred, val) in incomings {
                            if pred == &curr_bb {
                                let v = *val;
                                let var = var_map.get(&v).expect("phi incoming value not lowered");
                                let valv = builder.use_var(*var);
                                args.push(BlockArg::Value(valv));
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            // invalid IR; use zero to avoid panic
                            let z = builder.ins().iconst(types::I64, 0);
                            args.push(BlockArg::Value(z));
                        }
                    } else {
                        break; // phis are expected first
                    }
                }
                builder.ins().jump(block_map[target], &args);
            }

            Instruction::Branch { cond, then_bb, else_bb } => {
                let c = match cond {
                    Operand::Value(v) => {
                        let vv = var_map.get(v).expect("undefined cond value");
                        builder.use_var(*vv)
                    }
                    _ => unimplemented!(),
                };

                // then args
                let mut then_args: Vec<BlockArg> = Vec::new();
                let then_info = func.blocks.iter().find(|b| b.id == *then_bb).expect("then block missing");
                for inst in &then_info.instructions {
                    if let Instruction::Phi { incomings, .. } = inst {
                        let mut found = false;
                        for (pred, val) in incomings {
                            if pred == &curr_bb {
                                let v = *val;
                                let var = var_map.get(&v).expect("phi incoming value not lowered (then)");
                                let valv = builder.use_var(*var);
                                then_args.push(BlockArg::Value(valv));
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            then_args.push(BlockArg::Value(builder.ins().iconst(types::I64, 0)));
                        }
                    } else {
                        break;
                    }
                }

                // else args
                let mut else_args: Vec<BlockArg> = Vec::new();
                let else_info = func.blocks.iter().find(|b| b.id == *else_bb).expect("else block missing");
                for inst in &else_info.instructions {
                    if let Instruction::Phi { incomings, .. } = inst {
                        let mut found = false;
                        for (pred, val) in incomings {
                            if pred == &curr_bb {
                                let v = *val;
                                let var = var_map.get(&v).expect("phi incoming value not lowered (else)");
                                let valv = builder.use_var(*var);
                                else_args.push(BlockArg::Value(valv));
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            else_args.push(BlockArg::Value(builder.ins().iconst(types::I64, 0)));
                        }
                    } else {
                        break;
                    }
                }

                builder.ins().brif(c, block_map[then_bb], &then_args, block_map[else_bb], &else_args);
            }

            Instruction::Ret { value } => {
                let rv = match value {
                    Some(Operand::Value(v)) => {
                        let var = var_map.get(v).expect("ret references undefined value");
                        builder.use_var(*var)
                    }
                    Some(Operand::ConstInt(i)) => builder.ins().iconst(types::I64, *i),
                    _ => builder.ins().iconst(types::I64, 0),
                };
                builder.ins().return_(&[rv]);
            }
            Instruction::Alloc { dest, ty, count } => {
                let size_bytes = cranelift_intrinsics::sizeof(ty, &self.target).unwrap();
                println!("Allocating {:?} (size = {} bytes)", ty, size_bytes);
                let ptr = cranelift_intrinsics::stack_alloc(builder, &mut self.module, size_bytes as usize);
                let var = builder.declare_var(types::I64);
                builder.def_var(var, ptr);
                var_map.insert(*dest, var);
            }
            Instruction::LoadField { dest, base, offset } => {
                // Get base pointer value
                let base_val = match base {
                    Operand::Value(bv) => {
                        let vref = var_map.get(bv).expect("LoadField: base value undefined");
                        builder.use_var(*vref)
                    }
                    _ => panic!("LoadField base must be a Value"),
                };

                // Compute address = base + offset_in_bytes
                // ASSUMPTION: each field occupies 8 bytes (pointer/word size).
                let offset_bytes = (*offset as i64) * 8;
                let off_val = builder.ins().iconst(types::I64, offset_bytes);
                let addr = builder.ins().iadd(base_val, off_val);

                // Load an i64 from addr with zero offset
                let flags = MemFlags::new();
                // If field is narrower than i64 (e.g., i32), change types::I64 -> types::I32 and extend if needed.
                let loaded = builder.ins().load(types::I64, flags, addr, 0);

                let var = builder.declare_var(types::I64);
                builder.def_var(var, loaded);
                var_map.insert(*dest, var);
            }
            Instruction::StoreField { base, offset, value } => {
                // base pointer
                let base_val = match base {
                    Operand::Value(bv) => {
                        let vref = var_map.get(bv).expect("StoreField: base value undefined");
                        builder.use_var(*vref)
                    }
                    _ => panic!("StoreField base must be a Value"),
                };

                // value to store
                let value_val = match value {
                    Operand::Value(vv) => {
                        let vref = var_map.get(vv).expect("StoreField: value undefined");
                        builder.use_var(*vref)
                    }
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!("StoreField: unsupported value operand"),
                };

                let offset_bytes = (*offset as i64) * 8;
                let off_val = builder.ins().iconst(types::I64, offset_bytes);
                let addr = builder.ins().iadd(base_val, off_val);

                let flags = MemFlags::new();
                // store value_val at addr + 0
                builder.ins().store(flags, value_val, addr, 0);
            }
            Instruction::Call { dest, func, args } => {
                // Resolve callee name
                let mut func_name_id: Option<&StrId> = None;
                let func_name = match func {
                    Operand::FunctionRef(s) => {
                        func_name_id = Some(s);
                        self.context.borrow_mut().string_pool.resolve_string(&*s)
                    },
                    _ => panic!("Call target must be a FunctionRef in current lowering"),
                };

                let func_name_id = func_name_id.unwrap();

                // Build arg vals
                let mut arg_vals = Vec::new();
                for a in args {
                    let val = match a {
                        Operand::Value(v) => {
                            let var = var_map.get(v).expect("undefined call arg");
                            builder.use_var(*var)
                        }
                        Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                        _ => unimplemented!("call arg type not supported"),
                    };
                    arg_vals.push(val);
                }

                // Intrinsic dispatch: strip module qualifiers and check known intrinsics
                let short_name = func_name
                    .rsplit(|c| c == ':' || c == '.')
                    .next()
                    .unwrap_or(func_name);
                if let Some(intr) = crate::cranelift::cranelift_intrinsics::resolve_intrinsic(short_name) {
                    // Lower intrinsic directly (including stack alloc and zeroed)
                    use crate::cranelift::cranelift_intrinsics::codegen_intrinsic;
                    let ret_val_opt = codegen_intrinsic(intr, &arg_vals, &[], builder, self.module.isa(), Some(func_name));
                    if let Some(d) = dest {
                        let res_val = ret_val_opt.expect("intrinsic expected to return a value but returned None");
                        let var = builder.declare_var(types::I64);
                        builder.def_var(var, res_val);
                        var_map.insert(*d, var);
                    }
                    // Find or declare FuncId
                    let func_id = if let Some(fid) = self.func_ids.get(func_name_id) {
                        *fid
                    } else {
                        // Not declared yet: create an import with a guess signature.
                        // Signature: argc i64 params, returns 1 i64 if dest is Some, else returns 0.
                        let mut sig = Signature::new(self.module.isa().default_call_conv());
                        for _ in &arg_vals {
                            sig.params.push(AbiParam::new(types::I64));
                        }
                        if dest.is_some() {
                            sig.returns.push(AbiParam::new(types::I64));
                        }
                        let fid = self.module.declare_function(func_name, Linkage::Import, &sig)
                            .unwrap_or_else(|e| panic!("failed to declare import {}: {:?}", func_name, e));
                        self.func_ids.insert(*func_name_id, fid);
                        fid
                    };

                    // Turn FuncId -> FuncRef inside this function
                    let callee = self.module.declare_func_in_func(func_id, &mut builder.func);

                    // Emit call
                    let call_inst = builder.ins().call(callee, &arg_vals);

                    // Handle results depending on dest
                    let results = builder.inst_results(call_inst);
                    if let Some(d) = dest {
                        if results.is_empty() {
                            panic!("IR expects a return value from call to {}, but callee produces none", func_name);
                        }
                        let res_val = results[0];
                        let var = builder.declare_var(types::I64);
                        builder.def_var(var, res_val);
                        var_map.insert(*d, var);
                    } else {
                        // callee produced results but IR ignores them => OK, drop them
                    }
                }
            }


            _ => {
                // unhandled instructions left as TODO / unimplemented
            }
        }
    }

    fn get_or_create_string(&mut self, s: &StrId) -> DataId {
        let vm_str = **s;
        // Check if the string already exists in the LeapMap
        if let Some(mut id) = self.string_data.get(&vm_str) {
            if let Some(value) = id.value() {
                return value.0;   
            }
        }

        // Otherwise, declare new data
        let id = self
            .module
            .declare_data(&format!(".str.{}", self.string_data.len()), Linkage::Local, false, false)
            .unwrap();

        let mut data_ctx = DataDescription::new();
        data_ctx.define(self.context.borrow_mut().string_pool.resolve_bytes(s).to_vec().into_boxed_slice());
        self.module.define_data(id, &data_ctx).unwrap();

        // Insert into LeapMap
        self.string_data.insert(vm_str, ZetaDataId(id));

        id
    }
}

impl<'a> Backend for CraneliftBackend<'a> {
    fn emit_module(&mut self, module: &Module) {
        for (name, func) in &module.funcs {
            let mut sig = Signature::new(self.module.isa().default_call_conv());
            for param in &func.params {
                sig.params.push(AbiParam::new(clif_type(&param.1)));
            }
            sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));

            let resolved_name = self.context.borrow_mut().string_pool.resolve_string(&*name);
            let linkage = if resolved_name == "main" { Linkage::Export } else { Linkage::Local };
            println!("declaring function: {} with linkage {:?}", resolved_name, linkage);
            let fid = self.module.declare_function(resolved_name, linkage, &sig)
                .expect(&format!("failed to declare function {}", name));
            self.func_ids.insert(name.clone(), fid);
        }

        for func in module.funcs.values() {
            self.emit_function(func);
        }
    }

    fn emit_extern(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(clif_type(&param.1)));
        }
        sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));

        self.module.declare_function(
            self.context.borrow_mut().string_pool.resolve_string(&*func.name),
            Linkage::Import,
            &sig,
        ).unwrap();
    }

    fn finish(self) {
        let obj = self.module.finish();
        std::fs::write("out.o", obj.emit().unwrap()).unwrap();
    }

    fn emit_function(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(clif_type(&param.1)));
        }
        sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));

        let resolved_name = self.context.borrow_mut().string_pool.resolve_string(&*func.name);
        let linkage = if resolved_name == "main" { Linkage::Export } else { Linkage::Local };
        let fid = self.module.declare_function(resolved_name, linkage, &sig).unwrap();
        let mut ctx = self.module.make_context();
        ctx.func = ClifFunction::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, fid.as_u32()),
            sig,
        );

        let mut fbctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fbctx);

        let mut block_map = HashMap::with_hasher(FxHashBuilder);
        for bb in &func.blocks {
            block_map.insert(bb.id, builder.create_block());
        }

        let mut phi_param_map: HashMap<(BlockId, Value), usize, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder);
        for bb in &func.blocks {
            let clif_bb = block_map[&bb.id];
            for inst in &bb.instructions {
                if let Instruction::Phi { dest, .. } = inst {
                    builder.append_block_param(clif_bb, types::I64);
                    let idx = builder.block_params(clif_bb).len() - 1;
                    phi_param_map.insert((bb.id, *dest), idx);
                } else {
                    // continue scanning; robust even if phis are not strictly first
                }
            }
        }

        let entry = block_map[&func.entry];
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let mut var_map = HashMap::with_hasher(FxHashBuilder);
        for (i, &(v, _)) in func.params.iter().enumerate() {
            let var = builder.declare_var(types::I64);
            let pv = builder.block_params(entry)[i];
            builder.def_var(var, pv);
            var_map.insert(v, var);
        }

        for bb in &func.blocks {
            let clif_bb = block_map[&bb.id];
            builder.switch_to_block(clif_bb);

            self.lower_instructions(func, &mut builder, &mut block_map, &mut phi_param_map, &mut var_map, &bb);

            let last_was_terminator = bb.instructions.last().map_or(false, |i| inst_is_terminator(i));

            if !last_was_terminator {
                match func.ret_type {
                    SsaType::Void => {
                        builder.ins().return_(&[]);
                    }
                    _ => {
                        let z = builder.ins().iconst(types::I64, 0);
                        builder.ins().return_(&[z]);
                    }
                }
            }

        }

        builder.seal_all_blocks();
        builder.finalize();
        self.module.define_function(fid, &mut ctx).unwrap();

        if self.emit_asm {
            println!("==========================");
            println!("Assembly for function `{}`:", resolved_name);
            println!("==========================");

            // 🟩 Show the high-level Cranelift IR
            println!("--- Cranelift IR ---");
            println!("{}", ctx.func.display());
        }
        self.module.clear_context(&mut ctx);
    }
}

impl<'a> CraneliftBackend<'a> {
    fn lower_instructions(&mut self, func: &Function, mut builder: &mut FunctionBuilder, mut block_map: &mut HashMap<BlockId, Block, FxHashBuilder>, phi_param_map: &mut HashMap<(BlockId, Value), usize, FxHashBuilder>, mut var_map: &mut HashMap<Value, Variable, FxHashBuilder>, bb: &&BasicBlock) {
        for inst in &bb.instructions {
            match inst {
                Instruction::Interpolate { dest, parts } => {
                    let var = builder.declare_var(types::I64);
                    let count = parts.len() as i64;
                    let mut args = Vec::new();
                    for p in parts {
                        match p {
                            InterpolationOperand::Literal(s) => {
                                let did = self.get_or_create_string(s);
                                let gv = self.module.declare_data_in_func(did, &mut builder.func);
                                args.push(builder.ins().global_value(types::I64, gv));
                            }
                            InterpolationOperand::Value(v) => {
                                let vref = var_map.get(v).expect("undefined interpolation value");
                                args.push(builder.use_var(*vref));
                            }
                        }
                    }
                    let arr_ptr = args[0];

                    let func_ref = self.module.declare_func_in_func(self.enum_new, &mut builder.func);
                    let x = &[arr_ptr, builder.ins().iconst(types::I64, count)];
                    let call = builder.ins().call(func_ref, x);

                    let res = builder.inst_results(call)[0];
                    builder.def_var(var, res);
                    var_map.insert(*dest, var);
                }

                Instruction::EnumConstruct { dest, variant, .. } => {
                    let var = builder.declare_var(types::I64);
                    let tag = self.context.borrow_mut().string_pool.resolve_string(&*variant).parse::<i64>().unwrap_or(0);

                    let func_ref = self.module.declare_func_in_func(self.enum_new, &mut builder.func);
                    let x = &[builder.ins().iconst(types::I64, tag)];
                    let call = builder.ins().call(func_ref, x);
                    let res = builder.inst_results(call)[0];
                    builder.def_var(var, res);
                    var_map.insert(*dest, var);
                }

                Instruction::MatchEnum { value, arms } => {
                    let val = {
                        let vv = var_map.get(value).expect("undefined value in match enum");
                        builder.use_var(*vv)
                    };

                    let func_ref = self.module.declare_func_in_func(self.enum_tag, &mut builder.func);
                    let call = builder.ins().call(func_ref, &[val]);
                    let tag = builder.inst_results(call)[0];

                    let targets: Vec<_> = arms.iter().map(|(_, bbid)| block_map[bbid]).collect();

                    let mut value_list_pool = ValueListPool::new();
                    let mut block_calls: Vec<BlockCall> = Vec::new();

                    for target in &targets {
                        block_calls.push(BlockCall::new(*target, core::iter::empty(), &mut value_list_pool));
                    }

                    let jt_data = JumpTableData::new(
                        BlockCall::new(
                            *targets.first().unwrap(),
                            core::iter::once(BlockArg::Value(val)),
                            &mut value_list_pool,
                        ),
                        &block_calls,
                    );

                    let jt = builder.func.create_jump_table(jt_data);
                    builder.ins().br_table(tag, jt);
                }

                other => {
                    self.lower_basic_inst(other, bb.id, &mut builder, &mut var_map, &block_map, &phi_param_map, func);
                }
            }
        }
    }
}