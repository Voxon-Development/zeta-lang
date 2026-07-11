use ir::hir::{HirExpr, StrId};

#[derive(Clone, Copy)]
pub enum DropKind {
    /// A struct-typed local; freed via its own drop-glue function.
    Struct(StrId),
    /// An owned-pointer-typed local (`^T`). Freed by dropping the pointee
    /// (if it needs glue) then deallocating the backing allocation.
    /// `pointee_struct` is `Some` only when the pointee is itself a
    /// droppable struct, primitive/non-droppable pointees still need the
    /// dealloc call but skip the recursive drop-glue call.
    OwnedPointer { pointee_struct: Option<StrId> },
}

#[derive(Clone, Copy)]
pub struct DropLocal {
    pub name: StrId,
    pub kind: DropKind,
}

#[derive(Clone)]
pub struct DropScope {
    pub locals: Vec<DropLocal>,
}

#[derive(Default, Clone)]
pub struct DropLocalState {
    pub moved_whole: bool,
    pub moved_fields: std::collections::HashSet<StrId>,
}

#[derive(Default, Clone)]
pub struct DropMoveState {
    pub locals: std::collections::HashMap<StrId, DropLocalState>,
}

impl DropMoveState {
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
    pub fn is_whole_moved(&self, name: StrId) -> bool {
        self.locals.get(&name).map_or(false, |l| l.moved_whole)
    }
    pub fn is_field_moved(&self, name: StrId, field: StrId) -> bool {
        self.locals
            .get(&name)
            .map_or(false, |l| l.moved_whole || l.moved_fields.contains(&field))
    }
}

pub(crate) fn local_is_droppable(scope_stack: &[DropScope], name: StrId) -> Option<DropKind> {
    scope_stack
        .iter()
        .rev()
        .flat_map(|s| s.locals.iter())
        .find(|l| l.name == name)
        .map(|l| l.kind)
}

pub fn record_move_if_any(
    scope_stack: &[DropScope],
    drop_state: &mut DropMoveState,
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
                if let Some(DropKind::Struct(_)) = local_is_droppable(scope_stack, *root) {
                    drop_state.mark_field_moved(*root, *field);
                }
            }
        }
        _ => {}
    }
}
