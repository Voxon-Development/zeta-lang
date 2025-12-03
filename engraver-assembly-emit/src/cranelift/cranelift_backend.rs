use std::path::PathBuf;
use ir::layout::TargetInfo;
use std::io::Write;
use std::io::BufWriter;
use std::fs::File;
use smallvec::SmallVec;
use crate::backend::Backend;
use crate::cranelift::{clif_type, cranelift_intrinsics};
use cranelift::prelude::EntityRef;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::Block;
use cranelift_codegen::ir::Block as ClifBlock;
use cranelift_codegen::ir::AbiParam;
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::BlockArg;
use cranelift_codegen::ir::Function as ClifFunction;
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::MemFlags;
use cranelift_codegen::ir::Signature;
use cranelift_codegen::ir::Type;
use cranelift_codegen::isa;
use cranelift_frontend::{FuncInstBuilder, FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as ClifModule};
use cranelift_object::{object, ObjectBuilder, ObjectModule, ObjectProduct};
use ir::hir::StrId;
use ir::ssa_ir::{inst_is_terminator, BasicBlock, BinOp, BlockId, Function, Instruction, InterpolationOperand, Module, Operand, SsaType, Value, UnOp};
use std::collections::HashMap;
use std::error::Error;
use std::{fmt, io};
use std::sync::Arc;
use cranelift_codegen::settings::Configurable;
use cranelift_codegen::timing::Pass::unreachable_code;
use ir::ir_hasher::FxHashBuilder;
use ir::layout::sizeof_ssa;
use zetaruntime::string_pool::{StringPool, VmString};

pub struct CraneliftBackend {
    module: ObjectModule,
    string_data: HashMap<VmString, ZetaDataId>,
    interp_func: FuncId,
    enum_new: FuncId,
    enum_tag: FuncId,
    func_ids: HashMap<StrId, FuncId, FxHashBuilder>,
    context: Arc<StringPool>,
    target: TargetInfo,
    emit_asm: bool,
    optimize: bool,
    verbose: bool,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ZetaDataId(DataId);

impl leapfrog::Value for ZetaDataId {
    fn is_redirect(&self) -> bool {
        false
    }

    fn is_null(&self) -> bool {
        false
    }

    fn redirect() -> Self {
        ZetaDataId(DataId::new(usize::MAX))
    }

    fn null() -> Self {
        ZetaDataId(DataId::new(usize::MAX - 1))
    }
}

impl CraneliftBackend {
    pub fn new(context: Arc<StringPool>, optimize: bool, verbose: bool) -> Self {
        let mut cranelift_builder = cranelift_codegen::settings::builder();
        cranelift_builder.set("opt_level", if optimize { "speed_and_size" } else { "none" }).unwrap();
        let flags = cranelift_codegen::settings::Flags::new(cranelift_builder);
        if verbose { flags.regalloc_verbose_logs(); }
        let isa = isa::lookup_by_name("x86_64").unwrap()
            .finish(flags);
        let builder = ObjectBuilder::new(isa.unwrap(), "zetamir", cranelift_module::default_libcall_names()).unwrap();
        let mut module = ObjectModule::new(builder);

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
            emit_asm: true,
            optimize,
            verbose
        }
    }

    fn make_and_declare_var(builder: &mut FunctionBuilder, next_var_idx: &mut usize, ty: Type) -> Variable {
        let var = Variable::new(*next_var_idx);
        *next_var_idx = next_var_idx.wrapping_add(1);
        var
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
            Instruction::Const { dest, ty, value } => {
                let clif_ty = clif_type(ty);
                let var = builder.declare_var(clif_ty);
                let val = match value {
                    Operand::ConstInt(i) => {
                        if clif_ty == types::F32 {
                            builder.ins().f32const(*i as f32)
                        } else if clif_ty == types::F64 {
                            builder.ins().f64const(*i as f64)
                        } else {
                            builder.ins().iconst(clif_ty, *i)
                        }
                    }
                    Operand::ConstBool(b) => {
                        builder.ins().iconst(types::I8, if *b { 1 } else { 0 })
                    }
                    Operand::ConstString(s) => {
                        let did = self.get_or_create_string(s);
                        let gv = self.module.declare_data_in_func(did, &mut builder.func);
                        builder.ins().global_value(types::I64, gv)
                    }
                    Operand::Value(v) => {
                        let vv = var_map.get(v).expect("use of undefined value in Const operand");
                        builder.use_var(*vv)
                    }
                    Operand::ConstFloat(i) => {
                        if clif_ty == types::F32 {
                            builder.ins().f32const(*i as f32)
                        } else if clif_ty == types::F64 {
                            builder.ins().f64const(*i as f64)
                        } else {
                            unreachable!("ConstFloat with non-float type")
                        }
                    }
                    _ => unimplemented!(),
                };
                builder.def_var(var, val);
                var_map.insert(*dest, var);
            }

            Instruction::Binary { dest, op, left, right } => {
                let l = match left {
                    Operand::Value(v) => builder.use_var(*var_map.get(v).expect("binary left undefined")),
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!(),
                };
                let r = match right {
                    Operand::Value(v) => builder.use_var(*var_map.get(v).expect("binary right undefined")),
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
                let var = builder.declare_var(types::I64);
                let res_i64 = if builder.func.dfg.value_type(res) != types::I64 {
                    builder.ins().uextend(types::I64, res)
                } else { res };
                builder.def_var(var, res_i64);
                var_map.insert(*dest, var);
            }

            Instruction::Phi { dest, .. } => {
                let clif_bb = block_map.get(&curr_bb).expect("current block missing for Phi");
                let idx = phi_param_map.get(&(curr_bb, *dest)).expect("phi param missing");
                let param = builder.block_params(*clif_bb)[*idx];
                let var = builder.declare_var(types::I64);
                let param_i64 = if builder.func.dfg.value_type(param) != types::I64 {
                    builder.ins().uextend(types::I64, param)
                } else { param };
                builder.def_var(var, param_i64);
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
                    if let Instruction::Phi { incoming: incomings, .. } = inst {
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

                let c_type = builder.func.dfg.value_type(c);
                let cond_bool = if c_type != types::I8 {
                    let zero = builder.ins().iconst(c_type, 0);
                    let cmp = builder.ins().icmp(IntCC::NotEqual, c, zero);
                    cmp
                } else {
                    c
                };

                let mut then_args: Vec<BlockArg> = Vec::new();
                let then_info = func.blocks.iter().find(|b| b.id == *then_bb).expect("then block missing");
                for inst in &then_info.instructions {
                    if let Instruction::Phi { incoming: incomings, .. } = inst {
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
                    if let Instruction::Phi { incoming: incomings, .. } = inst {
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

                builder.ins().brif(cond_bool, block_map[then_bb], &then_args, block_map[else_bb], &else_args);
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
                let size_bytes = sizeof_ssa(ty, self.target).unwrap();
                let ptr: cranelift_codegen::ir::Value = cranelift_intrinsics::stack_alloc(builder, &mut self.module, size_bytes as usize);
                let var: Variable = Variable::new(var_map.len());
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

                let field_ty = func.value_types.get(dest).expect("LoadField: dest type unknown");
                let clif_ty = clif_type(field_ty);

                let offset_bytes = (*offset as i64) * (self.target.ptr_bytes as i64);
                let off_val = builder.ins().iconst(types::I64, offset_bytes);
                let addr = builder.ins().iadd(base_val, off_val);

                let flags = MemFlags::new();
                let loaded = builder.ins().load(clif_ty, flags, addr, 0);

                let var = Variable::new(var_map.len());
                builder.def_var(var, loaded);
                var_map.insert(*dest, var);
            }
            Instruction::StoreField { base, offset, value } => {
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

                let offset_bytes = (*offset as i64) * (self.target.ptr_bytes as i64);
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
                            let var = var_map.get(v).unwrap_or_else(|| {
                                println!("undefined call arg: {:?}", v);
                                println!("var map {:?}", var_map);
                                panic!("undefined call arg")
                            });
                            let val = builder.use_var(*var);
                            let val_type = builder.func.dfg.value_type(val);
                            if val_type != types::I64 {
                                builder.ins().uextend(types::I64, val)
                            } else {
                                val
                            }
                        }
                        Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                        _ => unimplemented!("call arg type not supported"),
                    };
                    arg_vals.push(val);
                }

                let short_name = func_name
                    .rsplit(|c| c == ':' || c == '.')
                    .next()
                    .unwrap_or(func_name);

                if let Some(intr) = crate::cranelift::cranelift_intrinsics::resolve_intrinsic(short_name) {
                    let ret_val_opt = cranelift_intrinsics::codegen_intrinsic(intr, &arg_vals, &[], builder, &self.module, Some(func_name), self.target);
                    if let Some(d) = dest {
                        let res_val = ret_val_opt.expect("intrinsic expected to return a value but returned None");
                        let var = Variable::new(var_map.len());
                        builder.def_var(var, res_val);
                        var_map.insert(*d, var);
                    }
                } else {
                    let func_id = if let Some(fid) = self.func_ids.get(func_name_id) {
                        *fid
                    } else {
                        let mut sig = Signature::new(self.module.isa().default_call_conv());
                        for _ in &arg_vals {
                            sig.params.push(AbiParam::new(types::I64));
                        }
                        if dest.is_some() {
                            sig.returns.push(AbiParam::new(types::I64));
                        }
                        let fid = ObjectModule::declare_function(&mut self.module, func_name, Linkage::Import, &sig)
                            .unwrap_or_else(|e| panic!("failed to declare import {}: {:?}", func_name, e));
                        self.func_ids.insert(*func_name_id, fid);
                        fid
                    };

                    let callee = self.module.declare_func_in_func(func_id, &mut builder.func);
                    let call_inst = FuncInstBuilder::call(builder.ins(), callee, &arg_vals);

                    let results = builder.inst_results(call_inst);
                    if let Some(d) = dest {
                        if results.is_empty() {
                            let var = Variable::new(var_map.len());
                            let dummy_val = builder.ins().iconst(types::I64, 0);
                            builder.def_var(var, dummy_val);
                            var_map.insert(*d, var);
                        } else {
                            let res_val = results[0];
                            let res_type = builder.func.dfg.value_type(res_val);
                            let var = Variable::new(var_map.len());
                            builder.def_var(var, res_val);
                            var_map.insert(*d, var);
                        }
                    } else {
                        // callee produced results but IR ignores them => OK, drop them
                    }
                }
            }

            Instruction::Unary { dest, op, operand } => {
                let val = match operand {
                    Operand::Value(v) => {
                        let var = var_map.get(v).expect("unary operand undefined");
                        builder.use_var(*var)
                    }
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!(),
                };

                let res = match op {
                    UnOp::Not => {
                        let zero = builder.ins().iconst(types::I64, 0);
                        builder.ins().icmp(IntCC::Equal, val, zero)
                    }
                };

                let var = builder.declare_var(types::I64);
                builder.def_var(var, res);
                var_map.insert(*dest, var);
            }

            Instruction::ClassCall { dest, object, method_id, args } => {
                let obj = match Operand::Value(*object) {
                    Operand::Value(v) => {
                        let var = var_map.get(&v).expect("object undefined");
                        builder.use_var(*var)
                    }
                    _ => unimplemented!(),
                };

                let mut arg_vals: Vec<cranelift_codegen::ir::Value> = vec![obj];
                for arg in args {
                    let val = match arg {
                        Operand::Value(v) => {
                            let var = var_map.get(v).expect("argument undefined");
                            builder.use_var(*var)
                        }
                        Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                        _ => unimplemented!(),
                    };
                    arg_vals.push(val);
                }

                // TODO: needs proper vtable implementation
                if let Some(d) = dest {
                    let var = builder.declare_var(types::I64);
                    let dummy = builder.ins().iconst(types::I64, 0);
                    builder.def_var(var, dummy);
                    var_map.insert(*d, var);
                }
            }

            Instruction::InterfaceDispatch { .. } => {}
            Instruction::UpcastToInterface { .. } => {}
            Instruction::Interpolate { .. } => {}
            Instruction::EnumConstruct { .. } => {}
            Instruction::MatchEnum { .. } => {}
        }
    }

    fn get_or_create_string(&mut self, s: &StrId) -> DataId {
        let vm_str = **s;
        if let Some(id) = self.string_data.get(&vm_str) {
            return id.0;
        }

        let id = self
            .module
            .declare_data(&format!(".str.{}", self.string_data.len()), Linkage::Local, false, false)
            .unwrap();

        let mut data_ctx = DataDescription::new();
        data_ctx.define(self.context.resolve_bytes(s).to_vec().into_boxed_slice());
        self.module.define_data(id, &data_ctx).unwrap();

        self.string_data.insert(vm_str, ZetaDataId(id));
        id
    }

    fn emit_main_wrapper(&mut self, zeta_main_fid: FuncId) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        sig.params.push(AbiParam::new(types::I32)); // argc
        sig.params.push(AbiParam::new(types::I64)); // argv (pointer)
        sig.returns.push(AbiParam::new(types::I32)); // return int

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

        let results: Vec<cranelift_codegen::ir::Value> = builder.inst_results(call).to_vec();
        let ret_val = if !results.is_empty() {
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

        for (name, func) in &module.functions {
            let mut sig = Signature::new(self.module.isa().default_call_conv());
            for param in &func.params {
                sig.params.push(AbiParam::new(clif_type(&param.1)));
            }

            if func.ret_type != SsaType::Void {
                sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));
            }

            let resolved_name = self.context.resolve_string(&*name);

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

        for func in module.functions.values() {
            self.emit_function(func);
        }

        if let Some(zeta_main_name) = zeta_main_fid {
            let fid = self.func_ids[&zeta_main_name];
            self.emit_main_wrapper(fid);
        }
    }

    fn emit_function(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(types::I64));
        }
        if func.ret_type != SsaType::Void {
            sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));
        }

        let resolved_name = self.context.resolve_string(&*func.name);

        let (actual_name, linkage) = if resolved_name == "main" {
            ("_zeta_main", Linkage::Local)
        } else {
            (resolved_name, Linkage::Local)
        };

        let fid = if let Some(existing) = self.func_ids.get(&func.name) {
            *existing
        } else {
            let fid = self.module.declare_function(actual_name, linkage, &sig).unwrap();
            self.func_ids.insert(func.name.clone(), fid);
            fid
        };

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
            let mut phi_index = 0usize;
            for inst in &bb.instructions {
                if let Instruction::Phi { dest, .. } = inst {
                    builder.append_block_param(clif_bb, types::I64);
                    phi_param_map.insert((bb.id, *dest), phi_index);
                    phi_index += 1;
                } else {
                    break;
                }
            }
        }

        let entry = block_map[&func.entry];
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);

        let mut var_map: HashMap<Value, Variable, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder);

        let mut next_var_idx: usize = 0;
        for (i, &(v, _)) in func.params.iter().enumerate() {
            let var = CraneliftBackend::make_and_declare_var(&mut builder, &mut next_var_idx, types::I64);
            let pv = builder.block_params(entry)[i];
            let pv_i64 = if builder.func.dfg.value_type(pv) != types::I64 {
                builder.ins().uextend(types::I64, pv)
            } else {
                pv
            };
            builder.def_var(var, pv_i64);
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
                    SsaType::I32 | SsaType::U32 => {
                        let z = builder.ins().iconst(types::I32, 0);
                        builder.ins().return_(&[z]);
                    }
                    SsaType::I64 | SsaType::U64 => {
                        let z = builder.ins().iconst(types::I64, 0);
                        builder.ins().return_(&[z]);
                    }
                    SsaType::F32 => {
                        let z = builder.ins().f32const(0.0);
                        builder.ins().return_(&[z]);
                    }
                    SsaType::F64 => {
                        let z = builder.ins().f64const(0.0);
                        builder.ins().return_(&[z]);
                    }
                    SsaType::Bool => {
                        let z = builder.ins().iconst(types::I8, 0);
                        builder.ins().return_(&[z]);
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
        self.module.define_function(fid, &mut ctx)
            .unwrap();

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

        if func.ret_type != SsaType::Void {
            sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));
        }

        self.module.declare_function(
            self.context.resolve_string(&*func.name),
            Linkage::Import,
            &sig,
        ).unwrap();
    }

    fn finish(self, out_dir: &PathBuf) -> Result<PathBuf, EmitError> {
        let obj: ObjectProduct = self.module.finish();

        let data: Vec<u8> = obj.emit().map_err(|e| EmitError::Emit(e))?;

        std::fs::create_dir_all(&out_dir).map_err(|e| EmitError::Io(e))?;

        let out_path = out_dir.join("out.o");

        let file: File = File::create(&out_path).map_err(|e| EmitError::Io(e))?;
        let mut writer: BufWriter<File> = BufWriter::with_capacity(4 * 1024 * 1024, file);

        writer.write_all(&data).map_err(|e| EmitError::Io(e))?;
        writer.flush().map_err(|e| EmitError::Io(e))?;

        Ok(out_path)
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
                    self.process_match_instruction(&mut builder, &block_map, &mut var_map, value, arms);
                }

                other => {
                    self.lower_basic_inst(other, bb.id, &mut builder, &mut var_map, &block_map, &phi_param_map, func);
                }
            }
        }
    }

    fn process_interpolate_instruction(
        &mut self,
        builder: &mut FunctionBuilder,
        var_map: &mut HashMap<Value, Variable, FxHashBuilder>,
        dest: &Value,
        parts: &SmallVec<InterpolationOperand, 4>
    ) {
        let var = builder.declare_var(types::I64);
        let count = parts.len() as i64;

        let arr_size = self.target.ptr_bytes as i64 * count;
        let arr_ptr = cranelift_intrinsics::stack_alloc(builder, &mut self.module, arr_size as usize);

        for (i, part) in parts.iter().enumerate() {
            let offset = i as i64 * self.target.ptr_bytes as i64;
            let val = match part {
                InterpolationOperand::Literal(s) => {
                    let did = self.get_or_create_string(&s);
                    let gv = self.module.declare_data_in_func(did, &mut builder.func);
                    builder.ins().global_value(types::I64, gv)
                }
                InterpolationOperand::Value(v) => builder.use_var(*var_map.get(v).expect("undefined interpolation value")),
            };
            let addr = builder.ins().iadd_imm(arr_ptr, offset);
            builder.ins().store(MemFlags::new(), val, addr, 0);
        }

        let func_ref = self.module.declare_func_in_func(self.interp_func, &mut builder.func);
        let count_val = builder.ins().iconst(types::I64, count);
        let call = builder.ins().call(func_ref, &[arr_ptr, count_val]);
        let res = builder.inst_results(call)[0];
        builder.def_var(var, res);
        var_map.insert(*dest, var);
    }

    fn process_enum_construct_instruction(
        &mut self,
        builder: &mut FunctionBuilder,
        var_map: &mut HashMap<Value, Variable, FxHashBuilder>,
        dest: &Value,
        variant: &StrId,
    ) {
        let var = builder.declare_var(types::I64);
        let tag = self.context.resolve_string(variant)
            .parse::<i64>()
            .expect("enum variant must be numeric for now");

        let func_ref = self.module.declare_func_in_func(self.enum_new, &mut builder.func);
        let tag_val = builder.ins().iconst(types::I64, tag);
        let call = builder.ins().call(func_ref, &[tag_val]);
        let res = builder.inst_results(call)[0];
        builder.def_var(var, res);
        var_map.insert(*dest, var);
    }

    fn process_match_instruction(
        &mut self,
        builder: &mut FunctionBuilder,
        block_map: &HashMap<BlockId, ClifBlock, FxHashBuilder>,
        var_map: &mut HashMap<Value, Variable, FxHashBuilder>,
        value: &Value,
        arms: &SmallVec<(StrId, BlockId), 8>,
    ) {
        let val = builder.use_var(*var_map.get(value).expect("undefined match value"));

        let func_ref = self.module.declare_func_in_func(self.enum_tag, &mut builder.func);
        let call = builder.ins().call(func_ref, &[val]);
        let tag_val = builder.inst_results(call)[0];

        for (arm_tag, target_bbid) in arms {
            let target_blk = block_map.get(target_bbid).expect("target block missing");
            let tag = self.context.resolve_string(arm_tag)
                .parse::<i64>()
                .expect("enum tag must be numeric for br_table");

            let cmp = builder.ins().icmp_imm(IntCC::Equal, tag_val, tag);
            let current_blk = builder.current_block().expect("no current block");
            builder.ins().brif(cmp, *target_blk, &[], current_blk, &[]);
        }
    }
}