use ir::hir::{HirExpr, StrId};

#[derive(Clone, Copy)]
pub struct DropLocal {
    pub name: StrId,
    pub struct_name: StrId,
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

pub(crate) fn local_is_droppable(scope_stack: &[DropScope], name: StrId) -> Option<StrId> {
    scope_stack
        .iter()
        .rev()
        .flat_map(|s| s.locals.iter())
        .find(|l| l.name == name)
        .map(|l| l.struct_name)
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
                if local_is_droppable(scope_stack, *root).is_some() {
                    drop_state.mark_field_moved(*root, *field);
                }
            }
        }
        _ => {}
    }
}
