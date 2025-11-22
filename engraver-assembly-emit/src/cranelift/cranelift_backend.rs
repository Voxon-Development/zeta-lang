//! SSA Backend switched to Cranelift code generation with full ADT and interpolation support

use std::io::Write;
use std::io::BufWriter;
use std::fs::File;
use smallvec::SmallVec;
use crate::backend::Backend;
use crate::cranelift::{clif_type, cranelift_intrinsics};
use cranelift::prelude::EntityRef;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, AbiParam, Block as ClifBlock, Block, BlockArg, BlockCall, Function as ClifFunction, InstBuilder, JumpTableData, MemFlags, Signature, ValueListPool};
use cranelift_codegen::isa;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as ClifModule};
use cranelift_object::{object, ObjectBuilder, ObjectModule};
use ir::hir::StrId;
use ir::ssa_ir::{inst_is_terminator, BasicBlock, BinOp, BlockId, Function, Instruction, InterpolationOperand, Module, Operand, SsaType, Value};
use std::collections::HashMap;
use std::error::Error;
use std::{fmt, io};
use std::sync::Arc;
use cranelift_codegen::entity::ListPool;
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::string_pool::{StringPool, VmString};
use crate::cranelift::cranelift_intrinsics::{TargetInfo};

pub struct CraneliftBackend {
    module: ObjectModule,
    string_data: HashMap<VmString, ZetaDataId>,
    interp_func: FuncId,
    enum_new: FuncId,
    enum_tag: FuncId,
    func_ids: HashMap<StrId, FuncId, FxHashBuilder>,
    context: Arc<StringPool>,
    target: TargetInfo,
    emit_asm: bool
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ZetaDataId(DataId);

impl leapfrog::Value for ZetaDataId {
    fn is_redirect(&self) -> bool {
        false
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

impl CraneliftBackend {
    pub fn new(context: Arc<StringPool>) -> Self {
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
            string_data: HashMap::new(),
            interp_func,
            enum_new,
            enum_tag,
            func_ids: HashMap::with_hasher(FxHashBuilder),
            context,
            target: TargetInfo { ptr_bytes: 8 },
            emit_asm: false
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
                let idx = phi_param_map
                    .get(&(curr_bb, *dest))
                    .or_else(|| phi_param_map.get(&(curr_bb, *dest)))
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
                let mut args: Vec<BlockArg> = Vec::new();
                let target_bb_info = func
                    .blocks
                    .iter()
                    .find(|b| b.id == *target)
                    .expect("target block missing");
                for inst in &target_bb_info.instructions {
                    if let Instruction::Phi { incomings, .. } = inst {
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
                            let z = builder.ins().iconst(types::I64, 0);
                            args.push(BlockArg::Value(z));
                        }
                    } else {
                        break;
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
            Instruction::Alloc { dest, ty, count: _ } => {
                let size_bytes = cranelift_intrinsics::sizeof(ty, &self.target).unwrap();
                println!("Allocating {:?} (size = {} bytes)", ty, size_bytes);
                let ptr = cranelift_intrinsics::stack_alloc(builder, &mut self.module, size_bytes as usize);
                let var = builder.declare_var(types::I64);
                builder.def_var(var, ptr);
                var_map.insert(*dest, var);
            }
            Instruction::LoadField { dest, base, offset } => {
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

                // Load an i64 because that's the only meaningful type
                let flags = MemFlags::new()
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
                builder.ins().store(flags, value_val, addr, 0);
            }
            Instruction::Call { dest, func, args } => {
                let (func_name_id, func_name): (Option<&StrId>, &str) = match func {
                    Operand::FunctionRef(s) => {
                        (Some(s), self.context.resolve_string(&*s))
                    },
                    _ => panic!("Call target must be a FunctionRef in current lowering"),
                };

                let func_name_id = func_name_id.unwrap();

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

            Instruction::Unary { .. } => {}
            Instruction::ClassCall { .. } => {}
            Instruction::InterfaceDispatch { .. } => {}
            Instruction::UpcastToInterface { .. } => {}
            Instruction::Interpolate { .. } => {}
            Instruction::EnumConstruct { .. } => {}
            Instruction::MatchEnum { .. } => {}
        }
    }

    fn get_or_create_string(&mut self, s: &StrId) -> DataId {
        let vm_str = **s;
        // Check if the string already exists in the LeapMap
        if let Some(id) = self.string_data.get(&vm_str) {
            return id.0;
        }

        // Otherwise, declare new data
        let id = self
            .module
            .declare_data(&format!(".str.{}", self.string_data.len()), Linkage::Local, false, false)
            .unwrap();

        let mut data_ctx = DataDescription::new();
        data_ctx.define(self.context.resolve_bytes(s).to_vec().into_boxed_slice());
        self.module.define_data(id, &data_ctx).unwrap();

        // Insert into LeapMap
        self.string_data.insert(vm_str, ZetaDataId(id));

        id
    }

    fn emit_main_wrapper(&mut self, zeta_main_fid: FuncId) {
        // Create C-compatible main signature: int main(int argc, char** argv)
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        sig.params.push(AbiParam::new(types::I32)); // argc
        sig.params.push(AbiParam::new(types::I64)); // argv (pointer)
        sig.returns.push(AbiParam::new(types::I32)); // return int

        // Declare the C main function
        let main_fid = self.module
            .declare_function("main", Linkage::Export, &sig)
            .expect("failed to declare main wrapper");

        let mut ctx = self.module.make_context();
        ctx.func = ClifFunction::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, main_fid.as_u32()),
            sig,
        );

        let mut fbctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fbctx);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let zeta_main_ref = self.module.declare_func_in_func(zeta_main_fid, &mut builder.func);
        let call = builder.ins().call(zeta_main_ref, &[]);

        // Get return value from Zeta main (if any) or default to 0
        let results: Vec<cranelift_codegen::ir::Value> = builder.inst_results(call).to_vec();
        let ret_val = if !results.is_empty() {
            // Truncate i64 to i32 for C main return
            builder.ins().ireduce(types::I32, results[0])
        } else {
            builder.ins().iconst(types::I32, 0)
        };

        builder.ins().return_(&[ret_val]);
        builder.seal_all_blocks();
        builder.finalize();

        self.module
            .define_function(main_fid, &mut ctx)
            .expect("failed to define main wrapper");

        self.module.clear_context(&mut ctx);
    }
}

impl Backend for CraneliftBackend {
    fn emit_module(&mut self, module: &Module) {
        let mut zeta_main_fid = None;

        for (name, func) in &module.funcs {
            let mut sig = Signature::new(self.module.isa().default_call_conv());
            for param in &func.params {
                sig.params.push(AbiParam::new(clif_type(&param.1)));
            }

            if func.ret_type != SsaType::Void {
                sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));
            }

            let resolved_name = self.context.resolve_string(&*name);

            // Rename user's "main" to "_zeta_main" to avoid conflicts
            let (actual_name, linkage) = if resolved_name == "main" {
                zeta_main_fid = Some(name.clone());
                ("_zeta_main", Linkage::Local)
            } else {
                (resolved_name, Linkage::Local)
            };

            let fid = self.module.declare_function(actual_name, linkage, &sig)
                .unwrap_or_else(|_| panic!("failed to declare function {}", actual_name));
            self.func_ids.insert(name.clone(), fid);
        }

        for func in module.funcs.values() {
            self.emit_function(func);
        }

        if let Some(zeta_main_name) = zeta_main_fid {
            let fid = self.func_ids[&zeta_main_name];
            self.emit_main_wrapper(fid);
        }
    }

    // Update emit_function to use the renamed main
    fn emit_function(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(clif_type(&param.1)));
        }
        if func.ret_type != SsaType::Void {
            sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));
        }

        let resolved_name = self.context.resolve_string(&*func.name);

        // Use renamed main
        let (actual_name, linkage) = if resolved_name == "main" {
            ("_zeta_main", Linkage::Local)
        } else {
            (resolved_name, Linkage::Local)
        };

        let fid = self.module.declare_function(actual_name, linkage, &sig).unwrap();
        let mut ctx = self.module.make_context();
        ctx.func = ClifFunction::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, fid.as_u32()),
            sig,
        );

        if func.noinline {
            // Cranelift hint for noinline
        }

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
            println!("Assembly for function `{}`:", actual_name);
            println!("==========================");
            println!("--- Cranelift IR ---");
            println!("{}", ctx.func.display());
        }
        self.module.clear_context(&mut ctx);
    }

    fn emit_extern(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(clif_type(&param.1)));
        }
        // Only add return type if function is not void
        if func.ret_type != SsaType::Void {
            sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));
        }

        self.module.declare_function(
            self.context.resolve_string(&*func.name),
            Linkage::Import,
            &sig,
        ).unwrap();
    }

    fn finish(self) -> Result<(), EmitError> {
        let obj = self.module.finish();
        let data = obj.emit().map_err(|e| EmitError::Emit(e))?;

        let file = File::create("out.o").map_err(|e| EmitError::Io(e))?;
        let mut writer = BufWriter::with_capacity(4 * 1024 * 1024, file); // 4 MB buffer
        writer.write_all(&data).map_err(|e| EmitError::Io(e))?;
        writer.flush().map_err(|e| EmitError::Io(e))?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum EmitError {
    Io(io::Error),
    Emit(object::write::Error)
}

impl fmt::Display for EmitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EmitError::Io(e) => write!(f, "IO error: {}", e),
            EmitError::Emit(e) => write!(f, "Emit error: {}", e),
        }
    }
}

impl Error for EmitError {}

impl CraneliftBackend {
    fn lower_instructions(
        &mut self,
        func: &Function,
        mut builder: &mut FunctionBuilder,
        block_map: &mut HashMap<BlockId, Block, FxHashBuilder>,
        phi_param_map: &mut HashMap<(BlockId, Value), usize, FxHashBuilder>,
        mut var_map: &mut HashMap<Value, Variable, FxHashBuilder>,
        bb: &&BasicBlock
    ) {
        for inst in &bb.instructions {
            match inst {
                Instruction::Interpolate { dest, parts } => {
                    self.process_interpolate_instruction(&mut builder, &mut var_map, dest, parts);
                }

                Instruction::EnumConstruct { dest, variant, .. } => {
                    self.process_enum_construct_instruction(&mut builder, &mut var_map, dest, variant);
                }

                Instruction::MatchEnum { value, arms } => {
                    self.process_match_instruction(&mut builder, block_map, &mut var_map, value, arms);
                }

                other => {
                    self.lower_basic_inst(other, bb.id, &mut builder, &mut var_map, &block_map, &phi_param_map, func);
                }
            }
        }
    }

    fn process_match_instruction(&mut self, builder: &mut FunctionBuilder, block_map: &mut HashMap<BlockId, Block, FxHashBuilder>, var_map: &mut &mut HashMap<Value, Variable, FxHashBuilder>, value: &Value, arms: &SmallVec<[(StrId, BlockId); 8]>) {
        let val = {
            let vv = var_map.get(value)
                .unwrap_or_else(|| panic!("undefined value in match enum"));
            builder.use_var(*vv)
        };

        let func_ref = self.module.declare_func_in_func(self.enum_tag, &mut builder.func);
        let call = builder.ins().call(func_ref, &[val]);
        let tag = builder.inst_results(call)[0];

        let targets: Vec<Block> = arms.iter().map(|(_, bbid)| block_map[bbid]).collect();

        let mut value_list_pool: ListPool<cranelift_codegen::ir::Value> = ValueListPool::new();
        let mut block_calls: Vec<BlockCall> = Vec::new();

        for target in &targets {
            block_calls.push(BlockCall::new(*target, core::iter::empty(), &mut value_list_pool));
        }

        let jt_data: JumpTableData = JumpTableData::new(
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

    fn process_enum_construct_instruction(&mut self, builder: &mut FunctionBuilder, var_map: &mut &mut HashMap<Value, Variable, FxHashBuilder>, dest: &Value, variant: &StrId) {
        let var = builder.declare_var(types::I64);
        let tag = self.context.resolve_string(&*variant).parse::<i64>().unwrap_or(0);

        let func_ref = self.module.declare_func_in_func(self.enum_new, &mut builder.func);
        let x = &[builder.ins().iconst(types::I64, tag)];
        let call = builder.ins().call(func_ref, x);
        let res = builder.inst_results(call)[0];
        builder.def_var(var, res);
        var_map.insert(*dest, var);
    }

    fn process_interpolate_instruction(&mut self, builder: &mut FunctionBuilder, var_map: &mut HashMap<Value, Variable, FxHashBuilder>, dest: &Value, parts: &SmallVec<[InterpolationOperand; 4]>) {
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

        let func_ref = self.module.declare_func_in_func(self.interp_func, &mut builder.func);
        let x = &[arr_ptr, builder.ins().iconst(types::I64, count)];
        let call = builder.ins().call(func_ref, x);

        let res = builder.inst_results(call)[0];
        builder.def_var(var, res);
        var_map.insert(*dest, var);
    }
}
