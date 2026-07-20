use ir::{
    ir_hasher::FxHashBuilder,
    ssa_ir::{BasicBlock, BlockId, Function, SsaType, Value},
};
use std::collections::HashMap;

pub struct CurrentBlockData<'f> {
    pub func: &'f mut Function,
    pub current_block: BlockId,
    pub next_value: usize,
    pub next_block: usize,
    pub value_types: HashMap<Value, SsaType, FxHashBuilder>,
}

impl<'f> CurrentBlockData<'f> {
    pub fn new(
        func: &'f mut Function,
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

    pub fn switch_to(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub fn value_type(&self, v: Value) -> Option<&SsaType> {
        self.value_types.get(&v)
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

    pub fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        self.func.blocks.push(BasicBlock {
            id,
            instructions: Vec::new(),
        });
        id
    }

    pub fn bb(&mut self) -> &mut BasicBlock {
        self.func
            .blocks
            .iter_mut()
            .find(|b| b.id == self.current_block)
            .unwrap()
    }

    pub fn finish(self) {
        self.func.value_types.extend(self.value_types);
    }
}
