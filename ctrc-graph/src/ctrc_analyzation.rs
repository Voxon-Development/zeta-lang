use ir::hir::HirExpr;
use ir::hir::HirStmt;
use ir::hir::HirStruct;
use ir::hir::HirFunc;
use crate::ConstraintReason;
use crate::CTRCAnalysisResult;
use crate::CTRCGraph;

impl CTRCGraph {
    pub(crate) fn analyze_function(&mut self, func: &HirFunc<'_, '_>, result: &mut CTRCAnalysisResult) {
        if let Some(body) = &func.body {
            self.analyze_statement(body, result);
        }
    }

    pub(crate) fn analyze_struct(&mut self, struct_def: &HirStruct<'_, '_>, result: &mut CTRCAnalysisResult) {
        if struct_def.destructor.is_some() {
            result.structs_with_destructors.insert(struct_def.name);
        }

        for field in struct_def.fields {
            if self.analyze_droppable_type(&field.field_type) {
                result.droppable_fields.insert((struct_def.name, field.name));
            }
        }
    }

    pub(crate) fn analyze_statement(&mut self, stmt: &HirStmt<'_, '_>, result: &mut CTRCAnalysisResult) {
        let mut work_queue = std::collections::VecDeque::new();
        work_queue.push_back(stmt);

        while let Some(current_stmt) = work_queue.pop_front() {
            match current_stmt {
                HirStmt::Let { name, ty, value, .. } => {
                    self.analyze_expression_iterative(value, result);

                    if self.analyze_droppable_type(ty) {
                        let alias_id = self.create_alias_for_variable(*name, ty.clone());
                        self.add_constraint(alias_id, self.next_program_point, 1, ConstraintReason::Copy);
                        result.variable_aliases.insert(*name, alias_id);
                    }

                    self.next_program_point += 1;
                }
                HirStmt::Return(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.analyze_expression_iterative(expr, result);
                    }
                    self.next_program_point += 1;
                }
                HirStmt::Expr(expr) => {
                    self.analyze_expression_iterative(expr, result);
                    self.next_program_point += 1;
                }
                HirStmt::Block { body } => {
                    let block_start = self.next_program_point;

                    for stmt in *body {
                        self.analyze_statement_single(stmt, result);
                    }

                    self.insert_block_end_drops(block_start, result);
                }
                HirStmt::If { cond, then_block, else_block } => {
                    self.analyze_expression_iterative(cond, result);

                    for stmt in then_block.iter().rev() {
                        work_queue.push_front(stmt);
                    }

                    if let Some(else_stmt) = else_block {
                        work_queue.push_front(else_stmt);
                    }

                    self.next_program_point += 1;
                }
                HirStmt::While { cond, body } => {
                    self.analyze_expression_iterative(cond, result);
                    work_queue.push_front(body);
                    self.next_program_point += 1;
                }
                _ => {
                    self.next_program_point += 1;
                }
            }
        }
    }

    pub(crate) fn analyze_statement_single(&mut self, stmt: &HirStmt<'_, '_>, result: &mut CTRCAnalysisResult) {
        match stmt {
            HirStmt::Let { name, ty, value, .. } => {
                self.analyze_expression_iterative(value, result);

                if self.analyze_droppable_type(ty) {
                    let alias_id = self.create_alias_for_variable(*name, ty.clone());
                    self.add_constraint(alias_id, self.next_program_point, 1, ConstraintReason::Copy);
                    result.variable_aliases.insert(*name, alias_id);
                }

                self.next_program_point += 1;
            }
            HirStmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expression_iterative(expr, result);
                }
                self.next_program_point += 1;
            }
            HirStmt::Expr(expr) => {
                self.analyze_expression_iterative(expr, result);
                self.next_program_point += 1;
            }
            HirStmt::Block { body } => {
                let block_start = self.next_program_point;

                for stmt in *body {
                    self.analyze_statement_single(stmt, result);
                }

                self.insert_block_end_drops(block_start, result);
            }
            HirStmt::If { cond, then_block, else_block } => {
                self.analyze_expression_iterative(cond, result);

                for stmt in *then_block {
                    self.analyze_statement_single(stmt, result);
                }

                if let Some(else_stmt) = else_block {
                    self.analyze_statement_single(else_stmt, result);
                }

                self.next_program_point += 1;
            }
            HirStmt::While { cond, body } => {
                self.analyze_expression_iterative(cond, result);
                self.analyze_statement_single(body, result);
                self.next_program_point += 1;
            }
            _ => {
                self.next_program_point += 1;
            }
        }
    }

    pub(crate) fn analyze_expression_iterative(&mut self, expr: &HirExpr<'_, '_>, result: &mut CTRCAnalysisResult) {
        let mut work_queue = std::collections::VecDeque::new();
        work_queue.push_back(expr);

        while let Some(current_expr) = work_queue.pop_front() {
            match current_expr {
                HirExpr::ClassInit { name, args, .. } => {
                    if let HirExpr::Ident(class_name) = name {
                        let alias_id = self.create_alias_for_allocation(*class_name);
                        self.add_constraint(alias_id, self.next_program_point, 1, ConstraintReason::Copy);
                        result.allocation_sites.insert(self.next_program_point, alias_id);
                    }

                    for arg in args.iter().rev() {
                        work_queue.push_front(arg);
                    }
                }
                
                HirExpr::Call { callee, args } => {
                    work_queue.push_front(callee);
                    for arg in args.iter().rev() {
                        work_queue.push_front(arg);
                    }
                }
                
                HirExpr::Assignment { target, value, .. } => {
                    work_queue.push_front(target);
                    work_queue.push_front(value);
                }
                
                HirExpr::FieldAccess { object, .. } => {
                    work_queue.push_front(object);
                }
                
                HirExpr::Binary { left, right, .. } => {
                    work_queue.push_front(left);
                    work_queue.push_front(right);
                }
                _ => {} // Other expressions don't affect ownership
            }
        }
    }
}