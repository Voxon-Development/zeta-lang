use std::marker::PhantomData;

use ir::{
    hir::{DropKind, HirExpr, StrId},
    ir_hasher::{HashMap, HashSet},
};

#[derive(Clone, Debug)]
pub struct DropLocal<'a, 'bump> {
    pub name: StrId,
    pub kind: DropKind<'a, 'bump>,
}

#[derive(Clone, Debug)]
pub struct DropScope<'a, 'bump> {
    pub locals: Vec<DropLocal<'a, 'bump>>,
}

#[derive(Default, Clone, Debug)]
pub struct DropLocalState {
    pub moved_whole: bool,
    pub moved_fields: HashSet<StrId>,
}

#[derive(Default, Clone, Debug)]
pub struct DropMoveState<'a, 'bump> {
    pub locals: HashMap<StrId, DropLocalState>,
    phantom_data: PhantomData<&'bump &'a ()>,
}

impl<'a, 'bump> DropMoveState<'a, 'bump> {
    pub fn mark_whole_moved(&mut self, name: StrId) {
        self.locals.entry(name).or_default().moved_whole = true;
    }

    pub fn mark_field_moved(&mut self, name: StrId, field: StrId) {
        self.locals
            .entry(name)
            .or_default()
            .moved_fields
            .insert(field);
    }

    pub fn has_any_field_moves(&self, name: StrId) -> bool {
        self.locals
            .get(&name)
            .map_or(false, |s| !s.moved_fields.is_empty())
    }

    pub fn is_whole_moved(&self, name: StrId) -> bool {
        self.locals.get(&name).map_or(false, |l| l.moved_whole)
    }

    pub fn is_field_moved(&self, name: StrId, field: StrId) -> bool {
        self.locals
            .get(&name)
            .map_or(false, |l| l.moved_whole || l.moved_fields.contains(&field))
    }
}

pub(crate) fn local_is_droppable<'a, 'bump>(
    scope_stack: &[DropScope<'a, 'bump>],
    name: StrId,
) -> Option<DropKind<'a, 'bump>> {
    scope_stack
        .iter()
        .rev()
        .flat_map(|s| s.locals.iter())
        .find(|l| l.name == name)
        .map(|l| l.kind.clone())
}

pub fn record_move_if_any<'a, 'bump>(
    scope_stack: &[DropScope<'a, 'bump>],
    drop_state: &mut DropMoveState<'a, 'bump>,
    expr: &HirExpr,
) {
    match expr {
        HirExpr::Ident(name, _) => {
            if local_is_droppable(scope_stack, *name).is_some() {
                drop_state.mark_whole_moved(*name);
            }
        }
        HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
            if let HirExpr::Ident(root, _) = &**object {
                // Only Struct locals have fields to partially move out of.
                // `p.field` where `p: ^T` is a move *through* the pointer
                // (of the pointee's field), not a move of the pointer
                // binding
                if let Some(_) = local_is_droppable(scope_stack, *root) {
                    drop_state.mark_field_moved(*root, *field);
                }
            }
        }
        _ => {}
    }
}
