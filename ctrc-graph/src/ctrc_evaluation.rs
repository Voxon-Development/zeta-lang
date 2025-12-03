use ir::hir::{Hir, HirExpr, HirModule, HirType, StrId};
use ir::ir_hasher::HashMap;
use crate::{AliasID, AliasSet, CTRCAnalysisResult, CTRCGraph, ConstraintReason, DestructorCall, DestructorCallType, DropInsertion, PVGNode, ProgramPoint, ValueKind};

impl CTRCGraph {
    pub fn is_droppable_type(&self, ty: &HirType<'_, '_>) -> bool {
        match ty {
            // Primitive types are Copy, not droppable
            HirType::I8 | HirType::I16 | HirType::I32 | HirType::I64 |
            HirType::U8 | HirType::U16 | HirType::U32 | HirType::U64 |
            HirType::F32 | HirType::F64 | HirType::Boolean | HirType::Void => false,

            // Complex types are potentially droppable
            HirType::String => true,
            HirType::Struct(_, _) => true,
            HirType::Interface(_, _) => true,
            HirType::Enum(_, _) => true,
            HirType::Lambda { .. } => true,

            // Generic types are conservatively droppable
            HirType::Generic(_) => true,

            _ => false,
        }
    }

    pub fn new() -> Self {
        Self {
            aliases: HashMap::default(),
            pvg_nodes: Vec::new(),
            pvg_edges: Vec::new(),
            constraints: Vec::new(),
            solutions: HashMap::default(),
            next_alias_id: 0,
            next_node_id: 0,
            next_program_point: 0,
            drop_points: HashMap::default(),
        }
    }

    pub fn analyze_hir_module(&mut self, module: &HirModule<'_, '_>) -> CTRCAnalysisResult {
        let mut result = CTRCAnalysisResult::new();

        for item in module.items {
            match item {
                Hir::Func(func) => self.analyze_function(func, &mut result),
                Hir::Struct(struct_def) => self.analyze_struct(struct_def, &mut result),
                _ => {} // Skip other items for now
            }
        }

        self.solve_constraints(&mut result);
        self.insert_destructor_calls(&mut result);

        result
    }

    pub(crate) fn analyze_droppable_type(&self, ty: &HirType<'_, '_>) -> bool {
        self.is_droppable_type(ty)
    }

    pub(crate) fn analyze_expression(&mut self, expr: &HirExpr<'_, '_>, result: &mut CTRCAnalysisResult) {
        self.analyze_expression_iterative(expr, result);
    }

    pub(crate) fn create_alias_for_variable(&mut self, name: StrId, ty: HirType<'_, '_>) -> AliasID {
        let alias_id = self.next_alias_id;
        self.next_alias_id += 1;

        let alias_set = AliasSet {
            id: alias_id,
            type_info: Some(unsafe { std::mem::transmute(ty) }), // Lifetime extension hack
            escape: false,
            concurrent: false,
            origin: Some(self.next_program_point),
        };

        self.aliases.insert(alias_id, alias_set);

        let node = PVGNode {
            id: self.next_node_id,
            kind: ValueKind::Var,
            alias: Some(alias_id),
            name: Some(name),
            program_point: self.next_program_point,
        };

        self.pvg_nodes.push(node);
        self.next_node_id += 1;

        alias_id
    }

    pub(crate) fn create_alias_for_allocation(&mut self, class_name: StrId) -> AliasID {
        let alias_id = self.next_alias_id;
        self.next_alias_id += 1;

        let alias_set = AliasSet {
            id: alias_id,
            type_info: None, // Would need type resolution
            escape: false,
            concurrent: false,
            origin: Some(self.next_program_point),
        };

        self.aliases.insert(alias_id, alias_set);

        let node = PVGNode {
            id: self.next_node_id,
            kind: ValueKind::Alloc,
            alias: Some(alias_id),
            name: Some(class_name),
            program_point: self.next_program_point,
        };

        self.pvg_nodes.push(node);
        self.next_node_id += 1;

        alias_id
    }

    pub(crate) fn insert_block_end_drops(&mut self, block_start: ProgramPoint, result: &mut CTRCAnalysisResult) {
        let current_point: ProgramPoint = self.next_program_point;

        let mut nodes_to_drop: Vec<(AliasID, StrId)> = Vec::new();

        // I can't split this into methods because it makes ownership and borrowing trickier
        // Sorry for all those never nesters raiding `issues` and `pull requests`
        for node in &self.pvg_nodes {
            if node.program_point >= block_start && node.program_point < current_point {
                if let (Some(alias_id), Some(var_name)) = (node.alias, node.name) {
                    if let Some(alias_set) = self.aliases.get(&alias_id) {
                        if alias_set.type_info.as_ref().map_or(false, |ty| self.is_droppable_type(ty)) {
                            nodes_to_drop.push((alias_id, var_name));
                        }
                    }
                }
            }
        }

        // Now process the drops
        for (alias_id, var_name) in nodes_to_drop {
            self.drop_points.entry(alias_id).or_default().push(current_point);
            self.add_constraint(alias_id, current_point, -1, ConstraintReason::Drop);

            result.drop_insertions.push(DropInsertion {
                program_point: current_point,
                variable_name: var_name,
                alias_id,
            });
        }
    }

    fn is_droppable_type_static(&self, ty: &HirType<'static, 'static>) -> bool {
        match ty {
            HirType::I8 | HirType::I16 | HirType::I32 | HirType::I64 |
            HirType::U8 | HirType::U16 | HirType::U32 | HirType::U64 |
            HirType::F32 | HirType::F64 | HirType::Boolean | HirType::Void => false,
            _ => true,
        }
    }

    fn insert_destructor_calls(&mut self, result: &mut CTRCAnalysisResult) {
        for (alias_id, drop_points) in &self.drop_points {
            for &drop_point in drop_points {
                result.destructor_calls.push(DestructorCall {
                    program_point: drop_point,
                    alias_id: *alias_id,
                    call_type: DestructorCallType::AutoDrop,
                });
            }
        }

        for (var_name, alias_id) in &result.variable_aliases {
            if let Some(drop_insertion) = result.drop_insertions.iter().find(|d| d.variable_name == *var_name) {
                let already_exists = result.destructor_calls.iter().any(|dc|
                    dc.alias_id == *alias_id && dc.program_point == drop_insertion.program_point
                );

                if !already_exists {
                    result.destructor_calls.push(DestructorCall {
                        program_point: drop_insertion.program_point,
                        alias_id: *alias_id,
                        call_type: DestructorCallType::ScopeDrop,
                    });
                }
            }
        }
    }
}