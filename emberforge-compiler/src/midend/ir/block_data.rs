use ir::{
    ir_hasher::FxHashBuilder,
    ssa_ir::{BasicBlock, BlockId, Function, SsaType, Value},
};
use std::collections::HashMap;

pub struct CurrentBlockData {
    pub func: Function,
    pub current_block: BlockId,
    pub next_value: usize,
    pub next_block: usize,
    pub value_types: HashMap<Value, SsaType, FxHashBuilder>,
}

impl CurrentBlockData {
    pub fn new(
        func: Function,
        current_block: BlockId,
        next_value: usize,
        next_block: usize,
        value_types: HashMap<Value, SsaType, FxHashBuilder>,
    ) -> Self {
        Self {
            func,
            current_block,
            next_value,
            next_block,
            value_types,
        }
    }

    pub fn fresh_value(&mut self) -> Value {
        let v = Value(self.next_value);
        self.next_value += 1;
        v
    }

    pub fn fresh_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        id
    }

    pub fn bb(&mut self) -> &mut BasicBlock {
        self.func
            .blocks
            .iter_mut()
            .find(|b| b.id == self.current_block)
            .unwrap()
    }

    pub fn finish(mut self) -> Function {
        self.func.value_types = self.value_types.clone();
        self.func
    }
}
