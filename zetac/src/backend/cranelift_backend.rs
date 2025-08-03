//! SSA Backend switched to Cranelift code generation with full ADT and interpolation support

use std::collections::HashMap;
use std::env::var;
use cranelift::prelude::EntityRef;
use cranelift_codegen::ir::{AbiParam, Function as ClifFunction, InstBuilder, Signature, Block as ClifBlock, types, JumpTable, JumpTableData, BlockCall, ValueListPool, BlockArg, Type};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::isa;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{Module as ClifModule, FuncId, Linkage, DataId, DataDescription};
use cranelift_object::{ObjectBuilder, ObjectModule};
use crate::backend::compiler::Backend;
use crate::midend::ir::ssa_ir::{BinOp, BlockId, Function, Instruction, InterpolationOperand, Module, Operand, SsaType, Value};

pub struct CraneliftBackend {
    module: ObjectModule,
    string_data: HashMap<String, DataId>,
    interp_func: FuncId,
    enum_new: FuncId,
    enum_tag: FuncId,
}

impl Backend for CraneliftBackend {
    fn emit_module(&mut self, module: &Module) {
        for func in &module.funcs {
            self.emit_function(func);
        }
    }

    fn emit_extern(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(clif_type(&param.1)));
        }
        sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));

        // Declare the function without defining it
        self.module.declare_function(
            &func.name,
            Linkage::Import, // Import = external
            &sig,
        ).unwrap();
    }

    fn emit_function(&mut self, func: &Function) {
        let mut sig = Signature::new(self.module.isa().default_call_conv());
        for param in &func.params {
            sig.params.push(AbiParam::new(clif_type(&param.1)));
        }
        sig.returns.push(AbiParam::new(clif_type(&func.ret_type)));

        let fid = self.module.declare_function(&func.name, Linkage::Local, &sig).unwrap();
        let mut ctx = self.module.make_context();
        ctx.func = ClifFunction::with_name_signature(
            cranelift_codegen::ir::UserFuncName::user(0, fid.as_u32()), sig);

        let mut fbctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fbctx);

        let mut block_map = HashMap::new();
        for bb in &func.blocks {
            block_map.insert(bb.id, builder.create_block());
        }
        let entry = block_map[&func.entry];
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let mut var_map = HashMap::new();
        for (i, &(v, _)) in func.params.iter().enumerate() {
            let var = builder.declare_var(types::I64);
            let pv = builder.block_params(entry)[i];
            builder.def_var(var, pv);
            var_map.insert(v, var);
        }

        for bb in &func.blocks {
            builder.switch_to_block(block_map[&bb.id]);
            builder.seal_block(block_map[&bb.id]);
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
                                    args.push(builder.use_var(var_map[v]));
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
                        let tag = variant.parse::<i64>().unwrap_or(0);

                        let func_ref = self.module.declare_func_in_func(self.enum_new, &mut builder.func);
                        let x = &[builder.ins().iconst(types::I64, tag)];
                        let call = builder.ins().call(func_ref, x);
                        let res = builder.inst_results(call)[0];
                        builder.def_var(var, res);
                        var_map.insert(*dest, var);
                    }
                    Instruction::MatchEnum { value, arms } => {
                        let val = builder.use_var(var_map[value]);

                        let func_ref = self.module.declare_func_in_func(self.enum_tag, &mut builder.func);

                        let call = builder.ins().call(func_ref, &[val]);
                        let tag = builder.inst_results(call)[0];
                        let targets: Vec<_> = arms.iter().map(|(_, bbid)| block_map[bbid]).collect();

                        let mut value_list_pool = ValueListPool::new();
                        let mut block_calls: Vec<BlockCall> = Vec::new();

                        for target in &targets {
                            block_calls.push(BlockCall::new(*target, core::iter::empty(), &mut value_list_pool));
                        }

                        let jt_data = JumpTableData::new(BlockCall::new(
                            *targets.first().unwrap(),
                            core::iter::once(BlockArg::Value(val)),
                            &mut value_list_pool
                        ), &block_calls);

                        // Register jump table in the function
                        let jt = builder.func.create_jump_table(jt_data);
                        builder.ins().br_table(tag, jt);
                    }
                    other => {
                        self.lower_basic_inst(other, &mut builder, &mut var_map, &block_map);
                    }
                }
            }
        }

        builder.seal_all_blocks();
        builder.finalize();
        self.module.define_function(fid, &mut ctx).unwrap();
        self.module.clear_context(&mut ctx);
    }

    fn finish(self) {
        let obj = self.module.finish();
        std::fs::write("out.o", obj.emit().unwrap()).unwrap();
    }
}

fn clif_type(param: &SsaType) -> Type {
    match param {
        SsaType::I8 | SsaType::Bool => types::I8,
        SsaType::I16 => types::I16,
        SsaType::I32 => types::I32,
        SsaType::I64 => types::I64,
        SsaType::U8 => types::I8,
        SsaType::U16 => types::I16,
        SsaType::U32 => types::I32,
        SsaType::U64 => types::I64,
        SsaType::I128 => types::I128,
        SsaType::F32 => types::F32,
        SsaType::F64 => types::F64,
        SsaType::Void => types::I8,
        SsaType::Ptr(_) => types::I64,
        SsaType::String => types::I64, // Pointer
        SsaType::User(_) => types::I64 // Pointer
    }
}

impl CraneliftBackend {
    pub fn new() -> Self {
        let isa = isa::lookup_by_name("x86_64").unwrap()
            .finish(cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder()));
        let builder = ObjectBuilder::new(isa.unwrap(), "ssair", cranelift_module::default_libcall_names()).unwrap();
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

        CraneliftBackend { module, string_data: HashMap::new(), interp_func, enum_new, enum_tag }
    }

    fn get_or_create_string(&mut self, s: &str) -> DataId {
        if let Some(&id) = self.string_data.get(s) { return id; }
        let id = self.module.declare_data(&format!(".str.{}", self.string_data.len()), Linkage::Local, false, false).unwrap();
        let mut data_ctx = DataDescription::new();
        data_ctx.define(s.as_bytes().to_vec().into_boxed_slice());
        self.module.define_data(id, &data_ctx).unwrap();
        self.string_data.insert(s.to_string(), id);
        id
    }

    fn lower_basic_inst(&mut self, inst: &Instruction, builder: &mut FunctionBuilder, var_map: &mut HashMap<Value, Variable>, block_map: &HashMap<BlockId, ClifBlock>) {
        match inst {
            Instruction::Const { dest, ty, value } => {
                let var = builder.declare_var(types::I64);
                let val = match value {
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    Operand::ConstBool(b) => builder.ins().iconst(types::I64, if *b {1} else {0}),
                    Operand::ConstString(s) => {
                        let did = self.get_or_create_string(s);
                        let gv = self.module.declare_data_in_func(did, &mut builder.func);
                        builder.ins().global_value(types::I64, gv)
                    }
                    Operand::Value(v) => builder.use_var(var_map[v]),
                };
                builder.def_var(var, val);
                var_map.insert(*dest, var);
            }
            Instruction::Binary { dest, op, left, right } => {
                let var = builder.declare_var(types::I64);
                let l = match left {
                    Operand::Value(v) => builder.use_var(var_map[v]),
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!(),
                };
                let r = match right {
                    Operand::Value(v) => builder.use_var(var_map[v]),
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!(),
                };
                let res = match op {
                    BinOp::Add => builder.ins().iadd(l, r),
                    BinOp::Sub => builder.ins().isub(l, r),
                    BinOp::Mul => builder.ins().imul(l, r),
                    BinOp::Div => builder.ins().sdiv(l, r),
                    BinOp::Eq  => builder.ins().icmp(IntCC::Equal, l, r),
                    BinOp::Ne  => builder.ins().icmp(IntCC::NotEqual, l, r),
                    BinOp::Lt  => builder.ins().icmp(IntCC::SignedLessThan, l, r),
                    BinOp::Le  => builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r),
                    BinOp::Gt  => builder.ins().icmp(IntCC::SignedGreaterThan, l, r),
                    BinOp::Ge  => builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, l, r),
                };
                builder.def_var(var, res);
                var_map.insert(*dest, var);
            }
            Instruction::Phi { dest, .. } => {
                let bb = builder.current_block().unwrap();
                let idx = builder.block_params(bb).len() - 1;
                let param = builder.block_params(bb)[idx];
                let var = builder.declare_var(types::I64);
                builder.def_var(var, param);
                var_map.insert(*dest, var);
            }
            Instruction::Jump { target } => {
                builder.ins().jump(block_map[target], &[]);
            }
            Instruction::Branch { cond, then_bb, else_bb } => {
                let c = match cond {
                    Operand::Value(v) => builder.use_var(var_map[v]),
                    _ => unimplemented!(),
                };
                builder.ins().brif(c, block_map[then_bb], &[], block_map[else_bb], &[]);
            }
            Instruction::Ret { value } => {
                let rv = match value {
                    Some(Operand::Value(v)) => builder.use_var(var_map[v]),
                    Some(Operand::ConstInt(i)) => builder.ins().iconst(types::I64, *i),
                    _ => builder.ins().iconst(types::I64, 0),
                };
                builder.ins().return_(&[rv]);
            }
            Instruction::Call { dest, func, args } => {
                let sig = Signature::new(self.module.isa().default_call_conv());
                let func_id = self.module.declare_function(func, Linkage::Local, &sig).unwrap();
                let callee = self.module.declare_func_in_func(func_id, &mut builder.func);
                let arg_vals: Vec<_> = args.iter().map(|a| match a {
                    Operand::Value(v) => builder.use_var(var_map[v]),
                    Operand::ConstInt(i) => builder.ins().iconst(types::I64, *i),
                    _ => unimplemented!(),
                }).collect();
                let call = builder.ins().call(callee, &arg_vals);
                let res = builder.inst_results(call)[0];
                let var = builder.declare_var(types::I64);
                builder.def_var(var, res);
                var_map.insert(dest.clone().unwrap(), var);
            }
            _ => {}
        }
    }
}
