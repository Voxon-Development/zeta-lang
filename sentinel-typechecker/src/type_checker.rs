use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::move_state::MoveState;
use crate::type_context::TypeContext;
use codex_dependency_graph::DepGraph;
use ir::analysis_context::CopyAnalysisCtx;
use ir::ast::{FuncSafety, MutabilityState};
use ir::auto_imports::AutoImportRegistry;
use ir::borrow_checker::{
    BorrowChecker, BorrowError, BorrowKind, Bound, IndexContainer, IndexTemplate, Interval, LoanId,
    MemoryRelation, PlaceId, ReadTemplate, RefTemplate, TemplateBase, TemplateProjection,
};
use ir::errors::type_error::{TypeCheckResult, TypeError, TypeErrorKind};
use ir::hir::{
    Hir, HirErrorHandlerPattern, HirExpr, HirFunc, HirMatchArm, HirModule, HirParam, HirPattern,
    HirStmt, HirType, InterpolationPart, IntrinsicKind, Operator, ProvenanceAnnotation,
    ProvenancePathSegment, ProvenanceRoot, StrId, ThisPassingKind, Visibility,
};
use ir::ir_hasher::{FxHashMap, HashSet};
use ir::nll_cfg::{Cfg, CfgBuilder, PointId};
use ir::span::SourceSpan;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalSymbolId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolId {
    /// let-binding, parameter, or `this`, identified by mint order, since
    /// name alone isn't unique across scopes.
    Local(LocalSymbolId),
    /// struct field, identified directly by (declaring struct, field
    /// name), already globally unique, no minting needed.
    Field {
        struct_name: StrId,
        field_name: StrId,
    },
    /// top-level declaration, same coordinate DepGraph already uses.
    /// Not populated yet, reserved for when function/struct/enum *name*
    /// occurrences get recorded (go-to-def on the declaration side).
    Item {
        module_idx: usize,
        item_idx: usize,
        tag: &'static str,
    },
    Method {
        module_idx: usize,
        item_idx: usize,
        method_idx: usize,
    },
}

#[derive(Clone, Copy)]
enum BareImportKind {
    Struct,
    Enum,
}

impl BareImportKind {
    fn as_str(&self) -> &'static str {
        match self {
            BareImportKind::Struct => "struct",
            BareImportKind::Enum => "enum",
        }
    }
}

struct ModuleImports {
    /// `import foo::bar.Baz;`, Baz becomes usable bare in this module.
    named: FxHashMap<StrId, usize>, // item name -> resolved declaring module_idx
    /// `import foo::bar;`, foo::bar::whatever() becomes usable qualified,
    /// but nothing from it becomes usable bare.
    modules: std::collections::HashSet<usize>,
    module_aliases: FxHashMap<StrId, usize>,
    wildcard: Vec<usize>,
}

pub struct TypeChecker<'a, 'bump> {
    context: TypeContext<'a, 'bump>,
    errors: Vec<TypeError<'a>>,
    current_span: SourceSpan<'a>,
    copy_analysis: Rc<RefCell<CopyAnalysisCtx<'a, 'bump>>>,
    move_state: MoveState,
    borrow_checker: BorrowChecker,
    this_id: StrId,
    suppress_errors: bool,
    ref_templates: FxHashMap<StrId, RefTemplate>,
    read_templates: FxHashMap<StrId, Vec<ReadTemplate>>,
    next_symbol_id: u32,
    occurrences: Vec<(
        SourceSpan<'a>,
        StrId,
        HirType<'a, 'bump>,
        usize,
        SymbolId,
        bool,
    )>,
    imports_by_module: FxHashMap<usize, ModuleImports>,
    functions_by_module: FxHashMap<usize, HashSet<StrId>>,
    structs_by_module: FxHashMap<usize, HashSet<StrId>>,
    enums_by_module: FxHashMap<usize, HashSet<StrId>>,
    generic_instance_args: FxHashMap<usize, Vec<HirType<'a, 'bump>>>,
    loan_owners: FxHashMap<LoanId, StrId>,
    local_provenance_place: FxHashMap<StrId, PlaceId>,
    call_loans: FxHashMap<usize, LoanId>,
    next_opaque_id: u32,
    unsafe_depth: usize,
    auto_imports: Rc<RefCell<AutoImportRegistry>>,
    cfg: Cfg,
    stmt_points: FxHashMap<usize, PointId>,
    stmt_after_points: FxHashMap<usize, PointId>,
    point_locals_used: FxHashMap<PointId, HashSet<StrId>>,
    current_point: PointId,
}

impl<'a, 'bump> TypeChecker<'a, 'bump> {
    pub fn new(
        dep_graph: &'a RefCell<DepGraph>,
        bump: &'bump GrowableBump<'bump>,
        copy_analysis: Rc<RefCell<CopyAnalysisCtx<'a, 'bump>>>,
        string_pool: Arc<StringPool>,
        auto_imports: Rc<RefCell<AutoImportRegistry>>,
    ) -> Self {
        Self {
            this_id: StrId(string_pool.intern("this")),
            context: TypeContext::new(dep_graph, bump, string_pool),
            errors: Vec::new(),
            current_span: SourceSpan::default(),
            copy_analysis,
            move_state: MoveState::new(),
            borrow_checker: BorrowChecker::new(),
            suppress_errors: false,
            ref_templates: FxHashMap::default(),
            read_templates: FxHashMap::default(),
            next_symbol_id: 0,
            occurrences: Vec::new(),
            functions_by_module: FxHashMap::default(),
            imports_by_module: FxHashMap::default(),
            enums_by_module: FxHashMap::default(),
            structs_by_module: FxHashMap::default(),
            generic_instance_args: FxHashMap::default(),
            loan_owners: FxHashMap::default(),
            local_provenance_place: FxHashMap::default(),
            call_loans: FxHashMap::default(),
            next_opaque_id: 0,
            unsafe_depth: 0,
            auto_imports,
            cfg: Cfg::default(),
            stmt_points: HashMap::default(),
            stmt_after_points: HashMap::default(),
            point_locals_used: HashMap::default(),
            current_point: PointId::default(),
        }
    }

    fn check_unsafe_call(&mut self, func: &HirFunc<'a, 'bump>, display_name: &str) {
        if matches!(func.function_metadata.func_safety, FuncSafety::Unsafe) && !self.in_unsafe() {
            self.record(TypeErrorKind::Generic(format!(
                "call to unsafe function `{}` requires an `unsafe` block",
                display_name
            )));
        }
    }

    fn analyze_read_templates(&mut self, func: &HirFunc<'a, 'bump>) -> Vec<ReadTemplate> {
        if let Some(t) = self.read_templates.get(&func.name) {
            return t.clone();
        }
        let templates = Self::build_read_templates(func);
        self.read_templates.insert(func.name, templates.clone());
        templates
    }

    fn build_read_templates(func: &HirFunc<'a, 'bump>) -> Vec<ReadTemplate> {
        let Some(params) = func.params else {
            return Vec::new();
        };

        let mut param_index: FxHashMap<StrId, usize> = FxHashMap::default();
        let mut has_this = false;
        let mut normal_idx = 0usize;
        for p in params.iter() {
            match p {
                HirParam::Normal { name, .. } => {
                    param_index.insert(*name, normal_idx);
                    normal_idx += 1;
                }
                HirParam::This { .. } => has_this = true,
            }
        }

        let mut templates = vec![ReadTemplate::Paths(Vec::new()); normal_idx];
        if let Some(body) = func.body {
            Self::collect_param_reads_stmt(&body, &param_index, has_this, &mut templates);
        }
        templates
    }

    fn record_param_read(
        base: TemplateBase,
        projections: Vec<TemplateProjection>,
        templates: &mut [ReadTemplate],
    ) {
        let TemplateBase::Param(i) = base else {
            return; // TemplateBase::This: nothing to do for parameter tracking
        };
        let Some(slot) = templates.get_mut(i) else {
            return;
        };
        if projections.is_empty() {
            *slot = ReadTemplate::Opaque;
        } else if let ReadTemplate::Paths(paths) = slot {
            paths.push(projections);
        }
        // already Opaque: stays Opaque
    }

    fn collect_param_reads_expr(
        expr: &HirExpr<'a, 'bump>,
        param_index: &FxHashMap<StrId, usize>,
        has_this: bool,
        templates: &mut [ReadTemplate],
    ) {
        if let Some((base, projections)) = Self::expr_to_template(expr, param_index, has_this) {
            Self::record_param_read(base, projections, templates);
            return;
        }

        match expr {
            HirExpr::Match { expr, arms, .. } => {
                Self::collect_param_reads_expr(expr, param_index, has_this, templates);
                for arm in arms.iter() {
                    if let Some(guard) = arm.guard {
                        Self::collect_param_reads_expr(guard, param_index, has_this, templates);
                    }
                    Self::collect_param_reads_stmt(arm.body, param_index, has_this, templates);
                }
            }
            HirExpr::Block { body, .. } => {
                for s in body.iter() {
                    Self::collect_param_reads_stmt(s, param_index, has_this, templates);
                }
            }
            HirExpr::Range { start, end, .. } => {
                Self::collect_param_reads_expr(start, param_index, has_this, templates);
                Self::collect_param_reads_expr(end, param_index, has_this, templates);
            }
            HirExpr::Slice {
                object, start, end, ..
            } => {
                Self::collect_param_reads_expr(object, param_index, has_this, templates);
                Self::collect_param_reads_expr(start, param_index, has_this, templates);
                Self::collect_param_reads_expr(end, param_index, has_this, templates);
            }
            HirExpr::Tuple(exprs, _)
            | HirExpr::ArrayLiteral {
                elements: exprs, ..
            } => {
                for e in exprs.iter() {
                    Self::collect_param_reads_expr(e, param_index, has_this, templates);
                }
            }
            HirExpr::Binary { left, right, .. } | HirExpr::Comparison { left, right, .. } => {
                Self::collect_param_reads_expr(left, param_index, has_this, templates);
                Self::collect_param_reads_expr(right, param_index, has_this, templates);
            }
            HirExpr::Call { callee, args, .. } | HirExpr::InterfaceCall { callee, args, .. } => {
                Self::collect_param_reads_expr(callee, param_index, has_this, templates);
                for a in args.iter() {
                    Self::collect_param_reads_expr(a, param_index, has_this, templates);
                }
            }
            HirExpr::FieldAccess { object, .. } | HirExpr::Get { object, .. } => {
                Self::collect_param_reads_expr(object, param_index, has_this, templates);
            }
            HirExpr::Assignment { target, value, .. } => {
                Self::collect_param_reads_expr(target, param_index, has_this, templates);
                Self::collect_param_reads_expr(value, param_index, has_this, templates);
            }
            HirExpr::StructInit { args, .. } => {
                for f in args.iter() {
                    Self::collect_param_reads_expr(&f.value, param_index, has_this, templates);
                }
            }
            HirExpr::EnumInit { args, .. } => {
                for a in args.iter() {
                    Self::collect_param_reads_expr(a, param_index, has_this, templates);
                }
            }
            HirExpr::ExprList { list, .. } => {
                for e in list.iter() {
                    Self::collect_param_reads_expr(e, param_index, has_this, templates);
                }
            }
            HirExpr::Deref { expr, .. }
            | HirExpr::Cast { expr, .. }
            | HirExpr::Ref { expr, .. } => {
                Self::collect_param_reads_expr(expr, param_index, has_this, templates);
            }
            HirExpr::Index { object, index, .. } => {
                Self::collect_param_reads_expr(object, param_index, has_this, templates);
                Self::collect_param_reads_expr(index, param_index, has_this, templates);
            }
            HirExpr::InterpolatedString(parts) => {
                for p in parts.iter() {
                    if let InterpolationPart::Expr(e) = p {
                        Self::collect_param_reads_expr(e, param_index, has_this, templates);
                    }
                }
            }
            HirExpr::If { if_stmt, .. } => {
                Self::collect_param_reads_stmt(if_stmt, param_index, has_this, templates);
            }
            HirExpr::Intrinsic { args, .. } => {
                for a in args.iter() {
                    Self::collect_param_reads_expr(a, param_index, has_this, templates);
                }
            }
            HirExpr::Lambda { .. } => {
                // TODO: not descending
                // into closure bodies means a captured parameter used
                // inside one is silently treated as unread rather than
                // Opaque.
            }
            // Ident/This/ModuleAccess/GenericIdent/literals/Undefined/
            // UnknownIntrinsic: either already handled by the
            // expr_to_template attempt above, or carry no sub-expressions.
            _ => {}
        }
    }

    fn collect_param_reads_stmt(
        stmt: &HirStmt<'a, 'bump>,
        param_index: &FxHashMap<StrId, usize>,
        has_this: bool,
        templates: &mut [ReadTemplate],
    ) {
        match stmt {
            HirStmt::Let {
                value,
                else_block,
                catch_pattern,
                ..
            } => {
                Self::collect_param_reads_expr(value, param_index, has_this, templates);
                if let Some(b) = else_block {
                    Self::collect_param_reads_stmt(b, param_index, has_this, templates);
                }
                if let Some(pattern) = catch_pattern {
                    match pattern {
                        HirErrorHandlerPattern::Single { body, .. } => {
                            for s in body.iter() {
                                Self::collect_param_reads_stmt(s, param_index, has_this, templates);
                            }
                        }
                        HirErrorHandlerPattern::Multiple { branches } => {
                            for branch in branches.iter() {
                                for s in branch.body.iter() {
                                    Self::collect_param_reads_stmt(
                                        s,
                                        param_index,
                                        has_this,
                                        templates,
                                    );
                                }
                            }
                        }
                    }
                }
            }
            HirStmt::Const(c) => {
                Self::collect_param_reads_expr(&c.value, param_index, has_this, templates)
            }
            HirStmt::Return(Some(e)) | HirStmt::Break(Some(e), _) => {
                Self::collect_param_reads_expr(e, param_index, has_this, templates)
            }
            HirStmt::Return(None)
            | HirStmt::Break(None, _)
            | HirStmt::Continue(_)
            | HirStmt::Import(..)
            | HirStmt::Package(..) => {}
            HirStmt::Expr(e) => Self::collect_param_reads_expr(e, param_index, has_this, templates),
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                Self::collect_param_reads_expr(cond, param_index, has_this, templates);
                for s in then_block.iter() {
                    Self::collect_param_reads_stmt(s, param_index, has_this, templates);
                }
                if let Some(e) = else_block {
                    Self::collect_param_reads_stmt(e, param_index, has_this, templates);
                }
            }
            HirStmt::While { cond, body } => {
                Self::collect_param_reads_expr(cond, param_index, has_this, templates);
                Self::collect_param_reads_stmt(body, param_index, has_this, templates);
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(i) = init {
                    Self::collect_param_reads_stmt(i, param_index, has_this, templates);
                }
                if let Some(c) = condition {
                    Self::collect_param_reads_expr(c, param_index, has_this, templates);
                }
                if let Some(inc) = increment {
                    Self::collect_param_reads_expr(inc, param_index, has_this, templates);
                }
                Self::collect_param_reads_stmt(body, param_index, has_this, templates);
            }
            HirStmt::Block { body } => {
                for s in body.iter() {
                    Self::collect_param_reads_stmt(s, param_index, has_this, templates);
                }
            }
            HirStmt::Match { expr, arms } => {
                Self::collect_param_reads_expr(expr, param_index, has_this, templates);
                for arm in arms.iter() {
                    if let Some(g) = arm.guard {
                        Self::collect_param_reads_expr(g, param_index, has_this, templates);
                    }
                    Self::collect_param_reads_stmt(arm.body, param_index, has_this, templates);
                }
            }
            HirStmt::UnsafeBlock { body } | HirStmt::Defer(body) => {
                Self::collect_param_reads_stmt(body, param_index, has_this, templates)
            }
        }
    }

    fn resolve_template_place_from(
        &mut self,
        mut place: PlaceId,
        projections: &[TemplateProjection],
        call_args: &[HirExpr<'a, 'bump>],
    ) -> PlaceId {
        for proj in projections {
            place = match proj {
                TemplateProjection::Field(f) => self.borrow_checker.project_field(place, *f),
                TemplateProjection::Deref => self.borrow_checker.project_deref(place),
                TemplateProjection::Index(idx_template) => {
                    let bound = match idx_template {
                        IndexTemplate::Const(c) => Bound::Const(*c),
                        IndexTemplate::Param(j) => match call_args.get(*j) {
                            Some(a) => self.expr_to_bound(a),
                            None => return place,
                        },
                        IndexTemplate::Opaque => return place,
                    };
                    let interval = Interval {
                        lower: bound.clone(),
                        upper: bound,
                    };
                    self.borrow_checker
                        .project_index(place, interval, IndexContainer::Primitive)
                }
            };
        }
        place
    }

    /// Checks a `&expr` argument being passed to a parameter with the given
    /// read-template against whatever loans the caller currently holds on
    /// the same root. See param_read_templates.md for the two branches'
    /// rationale.
    fn check_call_arg_read_effects(
        &mut self,
        inner: &HirExpr<'a, 'bump>,
        read_template: &ReadTemplate,
        call_args: &[HirExpr<'a, 'bump>],
    ) {
        let Some(base_place) = self.resolve_place(inner) else {
            return;
        };
        let Some(&root) = self.borrow_checker.place_roots.get(&base_place) else {
            return;
        };
        let Some(loan_ids) = self.borrow_checker.root_loans.get(&root).cloned() else {
            return;
        };
        if loan_ids.is_empty() {
            return;
        }

        match read_template {
            ReadTemplate::Opaque => {
                for loan_id in &loan_ids {
                    let Some(loan) = self.borrow_checker.active_loans.get(loan_id) else {
                        continue;
                    };
                    if loan.kind != BorrowKind::Mutable {
                        continue;
                    }
                    if let Ok(MemoryRelation::Overlap) =
                        self.borrow_checker.overlaps(base_place, loan.place)
                    {
                        self.record(TypeErrorKind::Generic(
                            "cannot pass this reference here: it may alias a value that's still \
                             mutably borrowed, and this call's effect on it isn't provably disjoint \
                             (the callee's parameter usage couldn't be bounded)"
                                .to_string(),
                        ));
                    }
                }
            }
            ReadTemplate::Paths(paths) => {
                for projections in paths {
                    let read_place =
                        self.resolve_template_place_from(base_place, projections, call_args);
                    for loan_id in &loan_ids {
                        let Some(loan) = self.borrow_checker.active_loans.get(loan_id) else {
                            continue;
                        };
                        if loan.kind != BorrowKind::Mutable {
                            continue;
                        }
                        if let Ok(MemoryRelation::Overlap) =
                            self.borrow_checker.overlaps(read_place, loan.place)
                        {
                            self.record(TypeErrorKind::Generic(
                                "cannot pass this reference here: the callee reads a part of it \
                                 that's still mutably borrowed"
                                    .to_string(),
                            ));
                        }
                    }
                }
            }
        }
    }

    fn in_unsafe(&self) -> bool {
        self.unsafe_depth != 0
    }

    fn check_borrow_use_shell(
        &mut self,
        expr: &HirExpr<'a, 'bump>,
        place: PlaceId,
        kind: BorrowKind,
    ) {
        if let Err(e) = self.borrow_checker.check_use_shell(place, kind) {
            let provenance = self.infer_provenance(expr);
            let msg = self.describe_borrow_error(&e, provenance.as_ref());
            self.record(TypeErrorKind::Generic(msg));
        }
    }

    fn record_item_occurrence(
        &mut self,
        span: SourceSpan<'a>,
        name: StrId,
        ty: HirType<'a, 'bump>,
        declaring_module_idx: usize,
    ) {
        let Some((m, item_idx, tag)) = self
            .context
            .dep_graph
            .borrow()
            .resolve_item_in_module(declaring_module_idx, name)
        else {
            return;
        };
        self.occurrences.push((
            span,
            name,
            ty,
            self.context.current_module_idx,
            SymbolId::Item {
                module_idx: m,
                item_idx,
                tag,
            },
            false,
        ));
    }

    fn record_method_occurrence(
        &mut self,
        span: SourceSpan<'a>,
        name: StrId,
        ty: HirType<'a, 'bump>,
        target_type: StrId,
    ) {
        let Some((module_idx, item_idx, method_idx)) = self
            .context
            .dep_graph
            .borrow()
            .resolve_method(target_type, name)
        else {
            return;
        };
        self.occurrences.push((
            span,
            name,
            ty,
            self.context.current_module_idx,
            SymbolId::Method {
                module_idx,
                item_idx,
                method_idx,
            },
            false,
        ));
    }

    fn expr_references_local(&self, expr: &HirExpr<'a, 'bump>, local: StrId) -> bool {
        match expr {
            HirExpr::Match { expr, arms, .. } => {
                self.expr_references_local(expr, local)
                    || arms.iter().any(|arm| {
                        arm.guard.is_some_and(|g| self.expr_references_local(g, local))
                            || self.stmt_references_local(arm.body, local)
                    })
            }
            HirExpr::Block { body, .. } => body.iter().any(|s| self.stmt_references_local(s, local)),
            HirExpr::Range { start, end, .. } => {
                self.expr_references_local(start, local) || self.expr_references_local(end, local)
            }
            HirExpr::Slice { object, start, end, .. } => {
                self.expr_references_local(object, local)
                    || self.expr_references_local(start, local)
                    || self.expr_references_local(end, local)
            }
            HirExpr::Ident(name, _) => *name == local,
            HirExpr::Tuple(exprs, _) | HirExpr::ArrayLiteral { elements: exprs, .. } =>
                exprs.iter().any(|e| self.expr_references_local(e, local)),
            HirExpr::Binary { left, right, .. } | HirExpr::Comparison { left, right, .. } =>
                self.expr_references_local(left, local) || self.expr_references_local(right, local),
            HirExpr::Call { callee, args, .. } | HirExpr::InterfaceCall { callee, args, .. } =>
                self.expr_references_local(callee, local) || args.iter().any(|a| self.expr_references_local(a, local)),
            HirExpr::FieldAccess { object, .. } | HirExpr::Get { object, .. } =>
                self.expr_references_local(object, local),
            HirExpr::Assignment { target, value, .. } =>
                self.expr_references_local(target, local) || self.expr_references_local(value, local),
            HirExpr::StructInit { args, .. } => args.iter().any(|f| self.expr_references_local(&f.value, local)),
            HirExpr::EnumInit { args, .. } => args.iter().any(|a| self.expr_references_local(a, local)),
            HirExpr::ExprList { list, .. } => list.iter().any(|e| self.expr_references_local(e, local)),
            HirExpr::Deref { expr, .. } | HirExpr::Ref { expr, .. } | HirExpr::Cast { expr, .. } =>
                self.expr_references_local(expr, local),
            HirExpr::Index { object, index, .. } =>
                self.expr_references_local(object, local) || self.expr_references_local(index, local),
            HirExpr::Lambda { body, .. } => self.stmt_references_local(body, local),
            HirExpr::InterpolatedString(parts) => parts.iter().any(|p| {
                matches!(p, ir::hir::InterpolationPart::Expr(e) if self.expr_references_local(e, local))
            }),
            HirExpr::This { .. } | HirExpr::ModuleAccess(_) | HirExpr::GenericIdent(..)
            | HirExpr::Number(..) | HirExpr::Decimal(..) | HirExpr::String(..)
            | HirExpr::Boolean(..) | HirExpr::Null(_) | HirExpr::Undefined { .. } | HirExpr::Char(_, _) => false,
            HirExpr::Intrinsic { args, .. } => {
                args.iter().any(|a| self.expr_references_local(a, local))
            }
            HirExpr::UnknownIntrinsic { .. } => unimplemented!(),
            HirExpr::If { if_stmt, span: _ } => self.stmt_references_local(*if_stmt, local)
        }
    }

    fn stmt_references_local(&self, stmt: &HirStmt<'a, 'bump>, local: StrId) -> bool {
        match stmt {
            HirStmt::Let {
                value, else_block, ..
            } => {
                self.expr_references_local(value, local)
                    || else_block.is_some_and(|b| self.stmt_references_local(b, local))
            }
            HirStmt::Return(Some(e)) | HirStmt::Break(Some(e), _) => {
                self.expr_references_local(e, local)
            }
            HirStmt::Return(None)
            | HirStmt::Break(None, _)
            | HirStmt::Continue(_)
            | HirStmt::Import(..)
            | HirStmt::Package(..) => false,
            HirStmt::Expr(e) => self.expr_references_local(e, local),
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.expr_references_local(cond, local)
                    || then_block
                        .iter()
                        .any(|s| self.stmt_references_local(s, local))
                    || else_block.is_some_and(|s| self.stmt_references_local(s, local))
            }
            HirStmt::While { cond, body } => {
                self.expr_references_local(cond, local) || self.stmt_references_local(body, local)
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                init.is_some_and(|s| self.stmt_references_local(s, local))
                    || condition.is_some_and(|c| self.expr_references_local(c, local))
                    || increment.is_some_and(|c| self.expr_references_local(c, local))
                    || self.stmt_references_local(body, local)
            }
            HirStmt::Block { body } => body.iter().any(|s| self.stmt_references_local(s, local)),
            HirStmt::Const(c) => self.expr_references_local(&c.value, local),
            HirStmt::Match { expr, arms } => {
                self.expr_references_local(expr, local)
                    || arms.iter().any(|arm| {
                        arm.guard
                            .is_some_and(|g| self.expr_references_local(g, local))
                            || self.stmt_references_local(arm.body, local)
                    })
            }
            HirStmt::UnsafeBlock { body } | HirStmt::Defer(body) => {
                self.stmt_references_local(body, local)
            }
        }
    }

    fn mint_symbol_id(&mut self) -> SymbolId {
        let id = LocalSymbolId(self.next_symbol_id);
        self.next_symbol_id += 1;
        SymbolId::Local(id)
    }

    fn expr_key(expr: &HirExpr<'a, 'bump>) -> usize {
        expr as *const HirExpr<'a, 'bump> as usize
    }

    fn record_instance_args(&mut self, expr: &HirExpr<'a, 'bump>, args: &[HirType<'a, 'bump>]) {
        self.generic_instance_args
            .insert(Self::expr_key(expr), args.to_vec());
    }

    #[allow(dead_code)]
    fn lookup_instance_args(&self, expr: &HirExpr<'a, 'bump>) -> Option<&[HirType<'a, 'bump>]> {
        self.generic_instance_args
            .get(&Self::expr_key(expr))
            .map(|v| v.as_slice())
    }

    pub fn occurrences(
        &self,
    ) -> &[(
        SourceSpan<'a>,
        StrId,
        HirType<'a, 'bump>,
        usize,
        SymbolId,
        bool,
    )] {
        &self.occurrences
    }

    pub fn context(&self) -> &TypeContext<'a, 'bump> {
        &self.context
    }

    pub fn errors(&self) -> &[TypeError<'a>] {
        &self.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn take_errors(&mut self) -> Vec<TypeError<'a>> {
        std::mem::take(&mut self.errors)
    }

    fn set_span(&mut self, span: SourceSpan<'a>) {
        self.current_span = span;
    }

    fn record(&mut self, kind: TypeErrorKind) {
        if self.suppress_errors {
            return;
        }
        self.errors.push(kind.at(self.current_span));
    }

    fn recover<T>(&mut self, result: TypeCheckResult<'a, T>, fallback: T) -> T {
        match result {
            Ok(v) => v,
            Err(e) => {
                if !self.suppress_errors {
                    self.errors.push(e);
                }
                fallback
            }
        }
    }

    fn with_suppressed_errors<F: FnOnce(&mut Self)>(&mut self, f: F) {
        let prev = self.suppress_errors;
        self.suppress_errors = true;
        f(self);
        self.suppress_errors = prev;
    }

    fn converge_loop_move_state(
        &mut self,
        body: &HirStmt<'a, 'bump>,
        entry_state: MoveState,
    ) -> MoveState {
        let saved_move_state = self.move_state.clone();
        let saved_context = self.context.clone();

        let mut converged = entry_state;

        self.with_suppressed_errors(|this| loop {
            this.move_state = converged.clone();
            this.check_stmt(body);
            let next = MoveState::join(&converged, &this.move_state);

            let stable = converged.is_superset_of(&next);
            converged = next;
            if stable {
                break;
            }
        });

        self.move_state = saved_move_state;
        self.context = saved_context;
        converged
    }

    fn is_zeroable(&self, ty: &HirType<'a, 'bump>) -> bool {
        match ty {
            HirType::I8
            | HirType::I16
            | HirType::I32
            | HirType::I64
            | HirType::I128
            | HirType::U8
            | HirType::U16
            | HirType::U32
            | HirType::U64
            | HirType::U128
            | HirType::F32
            | HirType::F64 => true,

            HirType::Array(inner, _) => self.is_zeroable(inner),

            HirType::Tuple(elems) => elems.iter().all(|e| self.is_zeroable(e)),

            HirType::Struct { name, .. } => {
                let name_str = self.str_id_to_string(*name);
                match self.context.get_struct(&name_str) {
                    Some(def) => def.fields.iter().all(|f| self.is_zeroable(&f.field_type)),
                    None => false, // unresolved struct
                }
            }

            HirType::Nullable(inner) => match **inner {
                // Pointer-shaped: all-zero bits legitimately means "null". Safe to zero-init.
                HirType::SafePointer { .. }
                | HirType::UnsafePointer { .. }
                | HirType::OwnedPointer { .. }
                | HirType::Ref { .. } => true,
                // Non-pointer nullable (e.g. i32?) needs a discriminant/tag, not just zero
                // bits
                // We could probably add some optimizations like `NonZero<u32>` like in Rust, but for now, this is good enough.
                _ => false,
            },

            // Impermissible: bool, char, string, enums, interfaces/dyn, lambdas,
            // pointers/refs, nullable, slices, generics, void/null/this/unknown.
            // Zeroing these either produces an invalid bit pattern (bool/char/enum
            // discriminants), a dangling/null reference where one shouldn't
            // silently appear (pointers), or is simply meaningless (lambda, dyn).
            _ => false,
        }
    }

    pub fn register_module(&mut self, module: &HirModule<'a, 'bump>, module_idx: usize) {
        let prev_module_idx = self.context.current_module_idx;
        self.context.current_module_idx = module_idx;

        for item in module.items {
            match item {
                Hir::Struct(s) => {
                    let name = s.name.to_string();
                    self.context.add_struct(module_idx, name.clone(), **s);
                    self.structs_by_module
                        .entry(module_idx)
                        .or_default()
                        .insert(s.name);
                }
                Hir::Impl(i) => {
                    let target = i.target.to_string();
                    if let Some(methods) = i.methods {
                        self.context.add_impl_methods(&target, methods);
                    }
                    if let Some(interface) = i.interface {
                        self.context
                            .add_struct_interface(&target, interface.to_string());
                    }
                }
                Hir::Interface(i) => {
                    let name = i.name.to_string();
                    self.context.add_interface(module_idx, name, **i);
                }
                Hir::Enum(e) => {
                    let name = e.name.to_string();
                    self.context.add_enum(module_idx, name, **e);
                    self.enums_by_module
                        .entry(module_idx)
                        .or_default()
                        .insert(e.name);
                }
                Hir::Func(f) => {
                    let mangled_name = f.name.to_string();
                    let unmangled_name = f.unmangled_name.to_string();
                    self.context
                        .add_function(module_idx, unmangled_name.clone(), **f);
                    if mangled_name != unmangled_name {
                        self.context.add_function(module_idx, mangled_name, **f);
                    }
                    self.functions_by_module
                        .entry(module_idx)
                        .or_default()
                        .insert(f.name);
                }
                _ => {}
            }
        }

        let mut imports = ModuleImports {
            named: FxHashMap::default(),
            modules: std::collections::HashSet::new(),
            module_aliases: std::collections::HashMap::default(),
            wildcard: Vec::new(),
        };
        for import_path in module.imports {
            let Some(target_module) = self
                .context
                .dep_graph
                .borrow()
                .resolve_module_path(import_path.path)
            else {
                let path_str = import_path
                    .path
                    .iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("::");
                self.record(TypeErrorKind::Generic(format!(
                    "cannot resolve imported module `{}`",
                    path_str
                )));
                continue;
            };
            match import_path.member {
                Some(name) => {
                    imports.named.insert(name, target_module);
                }
                None => {
                    imports.modules.insert(target_module);
                    if let Some(&last_seg) = import_path.path.iter().last() {
                        imports.module_aliases.insert(last_seg, target_module);
                    }
                }
            }
        }

        // Auto-imports: (1) bare item names, (2) module-level short aliases.
        // Both are checked for ambiguity between different auto-imported
        // packages, an explicit `import` always wins and needs no check.
        let mut alias_targets: FxHashMap<StrId, usize> = FxHashMap::default();
        for auto_path in self.auto_imports.borrow().paths() {
            let segments: Vec<StrId> = auto_path
                .iter()
                .map(|s| StrId(self.context.string_pool.intern(s)))
                .collect();
            let Some(target_module) = self
                .context
                .dep_graph
                .borrow()
                .resolve_module_path(segments.as_slice())
            else {
                continue;
            };
            if target_module != module_idx && !imports.wildcard.contains(&target_module) {
                imports.wildcard.push(target_module);
            }

            if let Some(&last) = segments.last() {
                match alias_targets.get(&last) {
                    Some(&existing) if existing != target_module => {
                        let existing_pkg = self
                            .context
                            .dep_graph
                            .borrow()
                            .get_module_package(existing)
                            .map(|p| p.to_string())
                            .unwrap_or_default();
                        let new_pkg = self
                            .context
                            .dep_graph
                            .borrow()
                            .get_module_package(target_module)
                            .map(|p| p.to_string())
                            .unwrap_or_default();

                        // The fastest and most efficient way to avoid a borrow checker issue is to inline `self.record` :P
                        // Unlike Zeta, Rust cannot prove that this is safe and is too conservative.
                        // Don't use rust, use zeta!
                        if self.suppress_errors {
                            return;
                        }
                        self.errors.push(
                            TypeErrorKind::Generic(format!(
                                "auto-imported packages `{}` and `{}` both alias to `{}`; \
                             add an explicit `import` to disambiguate",
                                existing_pkg, new_pkg, last,
                            ))
                            .at(self.current_span),
                        );
                    }
                    _ => {
                        alias_targets.insert(last, target_module);
                    }
                }
            }
        }

        self.imports_by_module.insert(module_idx, imports);
        self.context.current_module_idx = prev_module_idx;
    }

    pub fn check_module_body(&mut self, module: &HirModule<'a, 'bump>, module_idx: usize) {
        self.occurrences
            .retain(|(_, _, _, m, _, _)| *m != module_idx);
        self.context.current_module_idx = module_idx;
        for item in module.items {
            if let Hir::Func(func) = item {
                self.check_function(func);
            }
            if let Hir::Struct(ty_struct) = item {
                let Some(struct_interfaces) = self
                    .context
                    .struct_interfaces
                    .get(&ty_struct.name.to_string())
                else {
                    continue;
                };

                if struct_interfaces.contains("Copy") && struct_interfaces.contains("Drop") {
                    self.record(TypeErrorKind::Generic(format!(
                        "{} should not implement Copy and Drop at the same time",
                        ty_struct.name
                    )));
                }
            }
        }
    }

    fn check_name_import_visibility(&mut self, name: StrId, name_str: &str) {
        let current = self.context.current_module_idx;

        if self
            .functions_by_module
            .get(&current)
            .is_some_and(|s| s.contains(&name))
        {
            return;
        }

        if let Some(imports) = self.imports_by_module.get(&current) {
            if let Some(&target_module) = imports.named.get(&name) {
                if self
                    .functions_by_module
                    .get(&target_module)
                    .is_some_and(|s| s.contains(&name))
                {
                    return;
                }
            }

            let candidates: Vec<usize> = imports
                .wildcard
                .iter()
                .copied()
                .filter(|m| {
                    self.functions_by_module
                        .get(m)
                        .is_some_and(|s| s.contains(&name))
                })
                .collect();

            if candidates.len() == 1 {
                return;
            }
            if candidates.len() > 1 {
                let candidate_pkgs: Vec<String> = candidates
                    .iter()
                    .filter_map(|&m| self.context.dep_graph.borrow().get_module_package(m))
                    .map(|p| p.to_string())
                    .collect();
                self.record(TypeErrorKind::Generic(format!(
                    "`{}` is ambiguous: it is auto-imported from multiple packages ({}); \
                     add an explicit `import` to disambiguate",
                    name_str,
                    candidate_pkgs.join(", "),
                )));
                return;
            }
        }

        self.record(TypeErrorKind::Generic(format!(
            "`{}` is not declared in this module and has not been imported",
            name_str,
        )));
    }

    /// Qualified-path visibility: `foo::bar::baz()` needs `import foo::bar;`
    /// (or the current module IS foo::bar) even though the path is fully written out.
    fn check_module_path_imported(&mut self, path_segments: &[StrId]) {
        let current = self.context.current_module_idx;
        let Some(target) = self
            .context
            .dep_graph
            .borrow()
            .resolve_module_path(path_segments)
        else {
            return; // unresolved path already reported elsewhere (UndefinedFunctionWithSuggestion etc.)
        };
        if target == current {
            return;
        }

        let imported = self
            .imports_by_module
            .get(&current)
            .is_some_and(|imp| imp.modules.contains(&target));
        if !imported {
            let path_str = path_segments
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join("::");
            self.record(TypeErrorKind::Generic(format!(
                "module `{}` used without an `import {};` declaration",
                path_str, path_str,
            )));
        }
    }

    fn check_function(&mut self, func: &HirFunc<'a, 'bump>) {
        let mut func_context = self.context.create_child_scope();

        self.borrow_checker = BorrowChecker::new();
        self.borrow_checker.begin_scope();

        if let Some(params) = func.params {
            for param in params {
                match param {
                    HirParam::Normal {
                        name,
                        param_type,
                        span,
                    } => {
                        let param_name = self.str_id_to_string(*name);
                        let symbol_id = self.mint_symbol_id();
                        func_context.add_variable(param_name, *param_type, symbol_id);
                        self.borrow_checker.declare_local(*name);
                        self.occurrences.push((
                            *span,
                            *name,
                            *param_type,
                            self.context.current_module_idx,
                            symbol_id,
                            true,
                        ));
                    }
                    HirParam::This { .. } => {
                        self.borrow_checker.declare_local(self.this_id);
                    }
                }
            }
        }

        func_context.current_return_type = func.return_type;

        if let Some(body) = func.body {
            let (cfg, points) = CfgBuilder::new().build(&body);
            self.cfg = cfg;
            self.stmt_points = points.stmt_points;
            self.stmt_after_points = points.stmt_after_points;
            self.point_locals_used = FxHashMap::default();
            self.current_point = self.cfg.entry.unwrap_or_default();

            let old_context = std::mem::replace(&mut self.context, func_context);
            self.check_stmt(&body);
            self.context = old_context;
        }

        self.check_return_provenance(func);

        self.borrow_checker.end_scope();
    }

    fn stmt_key(stmt: &HirStmt<'a, 'bump>) -> usize {
        stmt as *const HirStmt<'a, 'bump> as usize
    }

    fn set_point(&mut self, stmt: &HirStmt<'a, 'bump>) {
        if let Some(&point) = self.stmt_points.get(&Self::stmt_key(stmt)) {
            self.current_point = point;
        }
    }

    fn check_stmt(&mut self, stmt: &HirStmt<'a, 'bump>) -> Option<HirType<'a, 'bump>> {
        self.set_point(stmt);
        match stmt {
            HirStmt::Let {
                name,
                ty,
                value,
                mutable,
                else_block,
                span,
                is_static: _,
                catch_pattern: _,
            } => {
                let var_name = self.str_id_to_string(*name);
                let is_wildcard = var_name == "_";

                if !is_wildcard && self.context.variables.contains_key(&var_name) {
                    self.record(TypeErrorKind::VariableAlreadyExists {
                        var_name: var_name.clone(),
                    });
                }

                let value_type = self.check_expr_expected(value, ty);

                self.check_and_record_value_use(value, &value_type);

                if let Some(else_block) = else_block {
                    match &value_type {
                        HirType::Nullable(inner) => {
                            let inner = **inner;
                            let result = self.types_compatible(ty, &inner);
                            self.recover(result, ());

                            let else_context = self.context.create_child_scope();
                            let old_context = std::mem::replace(&mut self.context, else_context);
                            self.check_stmt(else_block);
                            self.context = old_context;
                        }
                        _ => {
                            self.record(TypeErrorKind::Generic(format!(
                                "`? else` used on non-nullable type `{}`",
                                self.type_to_string(&value_type)
                            )));
                        }
                    }
                } else {
                    let result = self.types_compatible(ty, &value_type);
                    self.recover(result, ());
                }

                if self.expr_is_dangling(value) {
                    self.context.mark_dangling(var_name.clone());
                }

                let symbol_id = self.mint_symbol_id();
                self.context
                    .add_variable_with_mutability(var_name, *ty, *mutable, symbol_id);
                self.borrow_checker.declare_local(*name);
                self.occurrences.push((
                    *span,
                    *name,
                    *ty,
                    self.context.current_module_idx,
                    symbol_id,
                    true,
                ));

                if matches!(
                    ty,
                    HirType::SafePointer { .. } | HirType::UnsafePointer { .. }
                ) {
                    if let Some(place) = self.resolve_place(value) {
                        if let Some(&(base, ref offset)) = self.borrow_checker.pointee_of(place) {
                            let declared = *self.borrow_checker.local_place(*name).unwrap();
                            self.borrow_checker
                                .record_pointee(declared, base, offset.clone());
                        }
                    }
                }

                if let Some(loan_id) = self.call_loans.remove(&Self::expr_key(value)) {
                    self.loan_owners.insert(loan_id, *name);
                    if let Some(loan) = self.borrow_checker.loan(loan_id) {
                        self.local_provenance_place.insert(*name, loan.place);
                    }
                } else if let HirExpr::Ref {
                    expr: ref_target, ..
                } = value
                {
                    if let Some(place) = self.resolve_place(ref_target) {
                        self.local_provenance_place.insert(*name, place);
                        if let Some(&loan_id) = self.borrow_checker.loan_for_place(place) {
                            self.loan_owners.insert(loan_id, *name);
                        }
                    }
                }

                None
            }
            HirStmt::Return(expr) => {
                if let Some(e) = expr {
                    let expected_return = self.context.current_return_type;
                    let expr_type = match expected_return {
                        Some(ret) => self.check_expr_expected(e, &ret),
                        None => self.check_expr(e),
                    };
                    self.check_and_record_value_use(e, &expr_type);
                    let dangling = self.check_no_dangling_pointer(e);
                    self.recover(dangling, ());
                    if let Some(expected_return) = expected_return {
                        self.recover(self.types_compatible(&expected_return, &expr_type), ());
                    }
                } else if let Some(expected_return) = self.context.current_return_type {
                    if expected_return != HirType::Void {
                        self.record(TypeErrorKind::InvalidReturnType {
                            expected: self.type_to_string(&expected_return),
                            found: "void".to_string(),
                        });
                    }
                }
                None
            }
            HirStmt::Expr(e) => Some(self.check_expr(e)),
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_type = self.check_expr(cond);
                if cond_type != HirType::Boolean {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: "bool".to_string(),
                        found: self.type_to_string(&cond_type),
                    });
                }

                let move_state_before = self.move_state.clone();

                let fact = self.condition_to_fact(cond);
                let place_fact = self.condition_to_place_fact(cond);

                self.borrow_checker.begin_scope();
                if let Some((lhs, rhs, is_equal)) = &fact {
                    if *is_equal {
                        self.borrow_checker
                            .assume_equal_scoped(lhs.clone(), rhs.clone());
                    } else {
                        self.borrow_checker
                            .assume_not_equal_scoped(lhs.clone(), rhs.clone());
                    }
                }

                if let Some((lhs, rhs, is_equal)) = place_fact {
                    if !is_equal {
                        self.borrow_checker.assume_places_not_equal_scoped(lhs, rhs);
                    }
                }

                let mut then_context = self.context.create_child_scope();
                for stmt in *then_block {
                    let old_context = std::mem::replace(&mut self.context, then_context);
                    self.check_stmt(stmt);
                    then_context = self.context.clone();
                    self.context = old_context;
                }
                self.borrow_checker.end_scope();
                let then_move_state = self.move_state.clone();

                self.move_state = move_state_before.clone();
                if let Some(else_stmt) = else_block {
                    self.borrow_checker.begin_scope();
                    if let Some((lhs, rhs, is_equal)) = &fact {
                        if *is_equal {
                            self.borrow_checker
                                .assume_not_equal_scoped(lhs.clone(), rhs.clone());
                        } else {
                            self.borrow_checker
                                .assume_equal_scoped(lhs.clone(), rhs.clone());
                        }
                    }
                    if let Some((lhs, rhs, is_equal)) = place_fact {
                        if is_equal {
                            self.borrow_checker.assume_places_not_equal_scoped(lhs, rhs);
                        }
                    }
                    let else_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, else_context);
                    self.check_stmt(else_stmt);
                    self.context = old_context;
                    self.borrow_checker.end_scope();
                }
                let else_move_state = self.move_state.clone();

                self.move_state = MoveState::join(&then_move_state, &else_move_state);

                None
            }
            HirStmt::While { cond, body } => {
                let cond_type = self.check_expr(cond);
                if cond_type != HirType::Boolean {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: "bool".to_string(),
                        found: self.type_to_string(&cond_type),
                    });
                }

                self.context.enter_loop();

                let entry_state = self.move_state.clone();
                let converged_entry = self.converge_loop_move_state(body, entry_state);

                self.move_state = converged_entry;
                self.check_stmt(body);

                self.context.exit_loop();
                None
            }

            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init_stmt) = init {
                    self.check_stmt(init_stmt);
                }

                if let Some(cond) = condition {
                    let cond_type = self.check_expr(cond);
                    if cond_type != HirType::Boolean {
                        self.record(TypeErrorKind::TypeMismatch {
                            expected: "bool".to_string(),
                            found: self.type_to_string(&cond_type),
                        });
                    }
                }

                self.context.enter_loop();

                let entry_state = self.move_state.clone();
                let converged_entry = self.converge_loop_move_state(body, entry_state);

                self.move_state = converged_entry;
                self.check_stmt(body);

                self.context.exit_loop();

                if let Some(inc) = increment {
                    self.check_expr(inc);
                }

                None
            }
            HirStmt::Block { body } => {
                self.borrow_checker.begin_scope();
                let mut block_context = self.context.create_child_scope();

                let local_names: Vec<StrId> = body
                    .iter()
                    .filter_map(|s| match s {
                        HirStmt::Let { name, .. } => Some(*name),
                        _ => None,
                    })
                    .collect();

                let mut value = None;
                for (i, stmt) in body.iter().enumerate() {
                    let old_context = std::mem::replace(&mut self.context, block_context);
                    value = self.check_stmt(stmt);
                    block_context = self.context.clone();
                    self.context = old_context;

                    let after_point = self.stmt_after_points.get(&Self::stmt_key(stmt)).copied();

                    let dead_loans: Vec<LoanId> = self
                        .loan_owners
                        .iter()
                        .filter(|(_, owner)| local_names.contains(owner))
                        .filter(|(_, &owner)| match after_point {
                            Some(p) => !self.local_used_after(p, owner),
                            None => !body[(i + 1)..]
                                .iter()
                                .any(|s| self.stmt_references_local(s, owner)),
                        })
                        .map(|(&loan_id, _)| loan_id)
                        .collect();
                    for loan_id in dead_loans {
                        self.borrow_checker.end_loan_now(loan_id);
                        self.loan_owners.remove(&loan_id);
                    }
                }

                self.borrow_checker.end_scope();
                // sweep anything that reached scope-end without an early kill
                self.loan_owners
                    .retain(|_, owner| !local_names.contains(owner));
                value
            }
            HirStmt::Break(expr, span) => {
                self.set_span(*span);
                if !self.context.in_loop {
                    self.record(TypeErrorKind::BreakOutsideLoop);
                }
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e);
                    self.check_and_record_value_use(e, &expr_type);
                    if let Some(expected_return) = self.context.current_return_type {
                        let result = self.types_compatible(&expected_return, &expr_type);
                        self.recover(result, ());
                    }
                }
                None
            }
            HirStmt::Continue(span) => {
                self.set_span(*span);
                if !self.context.in_loop {
                    self.record(TypeErrorKind::ContinueOutsideLoop);
                }
                None
            }
            HirStmt::Const(const_stmt) => {
                let value_type = self.check_expr(&const_stmt.value);
                let result = self.types_compatible(&const_stmt.ty, &value_type);
                self.recover(result, ());
                let var_name = self.str_id_to_string(const_stmt.name);
                let symbol_id = self.mint_symbol_id();
                self.context
                    .add_variable(var_name, const_stmt.ty, symbol_id);
                None
            }
            HirStmt::Match { expr, arms } => {
                let scrutinee_ty = self.check_expr(expr);

                self.check_match_exhaustiveness(&scrutinee_ty, arms);

                let move_state_before = self.move_state.clone();
                let mut arm_move_states: Vec<MoveState> = Vec::with_capacity(arms.len());

                for arm in *arms {
                    self.move_state = move_state_before.clone();
                    self.borrow_checker.begin_scope();

                    let arm_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, arm_context);

                    self.check_pattern_against_type(&arm.pattern, &scrutinee_ty);
                    self.register_pattern_bindings(&arm.pattern, &scrutinee_ty);

                    if let Some(guard) = arm.guard {
                        let guard_type = self.check_expr(guard);
                        if guard_type != HirType::Boolean {
                            self.record(TypeErrorKind::TypeMismatch {
                                expected: "bool".to_string(),
                                found: self.type_to_string(&guard_type),
                            });
                        }
                    }

                    self.check_stmt(arm.body);

                    self.context = old_context;
                    self.borrow_checker.end_scope();
                    arm_move_states.push(self.move_state.clone());
                }

                self.move_state = arm_move_states
                    .into_iter()
                    .fold(move_state_before, |acc, arm_state| {
                        MoveState::join(&acc, &arm_state)
                    });

                None
            }
            HirStmt::UnsafeBlock { body } => {
                self.unsafe_depth += 1;
                self.check_stmt(body);
                self.unsafe_depth -= 1;
                None
            }
            HirStmt::Defer(hir_stmt) => {
                self.check_stmt(hir_stmt);
                None
            }
            // TODO: Check if they reference valid paths
            HirStmt::Import(_path, span) => {
                self.set_span(*span);
                None
            }
            HirStmt::Package(_path, span) => {
                self.set_span(*span);
                None
            }
        }
    }

    fn condition_to_fact(&mut self, cond: &HirExpr<'a, 'bump>) -> Option<(Bound, Bound, bool)> {
        if let HirExpr::Comparison {
            left, op, right, ..
        } = cond
        {
            let is_equal = match op {
                Operator::Equals => true,
                Operator::NotEquals => false,
                _ => return None,
            };
            Some((
                self.expr_to_bound(left),
                self.expr_to_bound(right),
                is_equal,
            ))
        } else {
            None
        }
    }

    fn leaf_span(expr: &HirExpr<'a, 'bump>) -> Option<SourceSpan<'a>> {
        match expr {
            HirExpr::Number(_, s)
            | HirExpr::Null(s)
            | HirExpr::Decimal(_, s)
            | HirExpr::Boolean(_, s)
            | HirExpr::String(_, s)
            | HirExpr::Char(_, s)
            | HirExpr::Ident(_, s) => Some(*s),
            _ => None,
        }
    }

    fn check_expr_expected(
        &mut self,
        expr: &HirExpr<'a, 'bump>,
        expected: &HirType<'a, 'bump>,
    ) -> HirType<'a, 'bump> {
        match expr {
            HirExpr::Number(_, span) if self.is_integer(expected) => {
                self.set_span(*span);
                *expected
            }
            HirExpr::Decimal(_, span) if matches!(expected, HirType::F32 | HirType::F64) => {
                self.set_span(*span);
                *expected
            }
            _ => self.check_expr(expr),
        }
    }

    fn check_expr(&mut self, expr: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        if let Some(span) = Self::leaf_span(expr) {
            self.set_span(span);
        }
        match expr {
            HirExpr::Number(_, _) => HirType::I64,
            HirExpr::Null(_) => HirType::Null,
            HirExpr::Decimal(_, _) => HirType::F64,
            HirExpr::Boolean(_, _) => HirType::Boolean,
            HirExpr::String(_, _) => HirType::String,
            HirExpr::Undefined { span, ty } => {
                self.set_span(*span);
                match ty {
                    HirType::Unknown => {
                        self.record(TypeErrorKind::TypeCannotBeInferred);
                        HirType::Unknown
                    }
                    other_type => {
                        if !self.is_zeroable(other_type) {
                            self.record(TypeErrorKind::Generic(format!(
                                "`undefined` cannot be used for type `{}`: it cannot be safely zero-initialized",
                                self.type_to_string(other_type)
                            )));
                            return HirType::Unknown;
                        }
                        *other_type
                    }
                }
            }
            HirExpr::Ident(name, span) => {
                let var_name = self.str_id_to_string(*name);
                let (symbol_id, ty) = match self.context.get_variable(&var_name) {
                    Some(ty) => ty,
                    None => {
                        self.record(TypeErrorKind::UndefinedVariable(var_name));
                        (SymbolId::Local(LocalSymbolId(u32::MAX)), HirType::Unknown)
                    }
                };
                self.point_locals_used
                    .entry(self.current_point)
                    .or_default()
                    .insert(*name);
                self.occurrences.push((
                    *span,
                    *name,
                    ty,
                    self.context.current_module_idx,
                    symbol_id,
                    false,
                ));
                ty
            }
            HirExpr::Tuple(exprs, _span) => {
                let mut types = Vec::new();
                for e in *exprs {
                    types.push(self.check_expr(e));
                }
                HirType::Tuple(self.context.bump.alloc_slice_copy(types.as_slice()))
            }
            HirExpr::Binary {
                left,
                op,
                right,
                span,
            } => {
                self.set_span(*span);
                let left_type = self.check_expr(left);
                let right_type = self.check_expr(right);
                let result = self.check_binary_op(&left_type, op, &right_type);
                self.recover(result, HirType::Unknown)
            }
            HirExpr::Intrinsic {
                kind,
                type_args,
                args,
                span,
            } => {
                self.set_span(*span);

                match kind {
                    IntrinsicKind::Reinterpret => {
                        if type_args.len() != 1 {
                            self.record(TypeErrorKind::Generic(format!(
                                "$reinterpret expects exactly 1 type argument, found {}",
                                type_args.len()
                            )));
                            return HirType::Unknown;
                        }
                        if args.len() != 1 {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args: 1,
                                found_args: args.len(),
                            });
                            return HirType::Unknown;
                        }
                        let target_ty = type_args[0];
                        let source_ty = self.check_expr(&args[0]);
                        self.check_and_record_value_use(&args[0], &source_ty);
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                            "$reinterpret requires an unsafe block: it reinterprets a value's bit \
                             representation as a different type with no conversion or validation"
                                .to_string(),
                        ));
                        }
                        target_ty
                    }
                    IntrinsicKind::Unreachable => {
                        if !type_args.is_empty() {
                            self.record(TypeErrorKind::Generic(
                                "$unreachable takes no type arguments".to_string(),
                            ));
                        }
                        if !args.is_empty() {
                            self.record(TypeErrorKind::Generic(
                                "$unreachable takes no value arguments".to_string(),
                            ));
                        }
                        HirType::Never
                    }
                    IntrinsicKind::SizeOf | IntrinsicKind::AlignOf | IntrinsicKind::TypeName => {
                        if type_args.len() != 1 {
                            self.record(TypeErrorKind::Generic(format!(
                                "intrinsic expects exactly 1 type argument, found {}",
                                type_args.len()
                            )));
                        }
                        if !args.is_empty() {
                            self.record(TypeErrorKind::Generic(
                                "this intrinsic takes no value arguments".to_string(),
                            ));
                        }
                        match kind {
                            IntrinsicKind::SizeOf | IntrinsicKind::AlignOf => HirType::Usize,
                            IntrinsicKind::TypeName => HirType::String,
                            _ => unreachable!(),
                        }
                    }
                    IntrinsicKind::Own => {
                        if !type_args.is_empty() {
                            self.record(TypeErrorKind::Generic(
                                "$own takes no type arguments".to_string(),
                            ));
                        }
                        if args.is_empty() || args.len() > 4 {
                            self.record(TypeErrorKind::Generic(format!(
                                "$own expects 1 argument (ptr), 2 (ptr, allocator or len), 3 (ptr, allocator, len), \
                                 or 4 (ptr, allocator, len, cap) for owned slices, found {}",
                                args.len()
                            )));
                            return HirType::Unknown;
                        }

                        let ptr_ty = self.check_expr(&args[0]);
                        self.check_and_record_value_use(&args[0], &ptr_ty);
                        let pointee = match Self::strip_ref(&ptr_ty) {
                            HirType::SafePointer { inner, .. }
                            | HirType::UnsafePointer { inner, .. } => *inner,
                            _ => {
                                self.record(TypeErrorKind::Generic(format!(
                                    "$own expects a `*T` or `[*]T`, found `{}`",
                                    self.type_to_string(&ptr_ty)
                                )));
                                return HirType::Unknown;
                            }
                        };

                        let (alloc_arg, len_arg, cap_arg) = if args.len() == 4 {
                            (Some(&args[1]), Some(&args[2]), Some(&args[3]))
                        } else if args.len() == 3 {
                            (Some(&args[1]), Some(&args[2]), None)
                        } else if args.len() == 2 {
                            let arg1_ty = self.check_expr(&args[1]);
                            if self.is_integer(&arg1_ty) {
                                (None, Some(&args[1]), None)
                            } else {
                                (Some(&args[1]), None, None)
                            }
                        } else {
                            (None, None, None)
                        };

                        let allocator = if let Some(alloc_expr) = alloc_arg {
                            let alloc_ty = self.check_expr(alloc_expr);
                            self.check_and_record_value_use(alloc_expr, &alloc_ty);
                            let Some(allocator) = self.infer_provenance(alloc_expr) else {
                                self.record(TypeErrorKind::Generic(
                                    "$own's allocator argument must be a place with stable provenance (a global, `this`, a param, or a projection through them)".to_string(),
                                ));
                                return HirType::Unknown;
                            };

                            let alloc_struct_name = match Self::strip_ref(&alloc_ty) {
                                HirType::Struct { name, .. } => self.str_id_to_string(*name),
                                _ => {
                                    self.record(TypeErrorKind::Generic(
                                        "$own's allocator argument must be a struct implementing RawAllocator".to_string(),
                                    ));
                                    return HirType::Unknown;
                                }
                            };
                            if !self
                                .context
                                .struct_implements(&alloc_struct_name, "RawAllocator")
                            {
                                self.record(TypeErrorKind::Generic(format!(
                                    "`{}` does not implement `RawAllocator`",
                                    alloc_struct_name
                                )));
                            }

                            if !matches!(allocator.root, ProvenanceRoot::Global { .. }) {
                                if let Some(place) = self.resolve_place(alloc_expr) {
                                    match self.borrow_checker.borrow_shared(place) {
                                        Ok(loan_id) => {
                                            self.call_loans.insert(Self::expr_key(expr), loan_id);
                                        }
                                        Err(e) => {
                                            let msg =
                                                self.describe_borrow_error(&e, Some(&allocator));
                                            self.record(TypeErrorKind::Generic(msg));
                                        }
                                    }
                                }
                            }
                            allocator
                        } else {
                            let this_expr = HirExpr::This {
                                span: self.current_span,
                            };
                            let Some(allocator) = self.infer_provenance(&this_expr) else {
                                self.record(TypeErrorKind::Generic(
                                    "$own without explicit allocator requires a `this` allocator in scope".to_string(),
                                ));
                                return HirType::Unknown;
                            };
                            allocator
                        };

                        let result_inner = if let Some(cap_expr) = cap_arg {
                            let len_expr = len_arg.expect(
                                "cap_arg implies len_arg due to the 4-arg-only branch above",
                            );
                            let len_ty = self.check_expr(len_expr);
                            self.check_and_record_value_use(len_expr, &len_ty);
                            if !self.is_integer(&len_ty) {
                                self.record(TypeErrorKind::Generic(format!(
                                    "$own's len argument must be an integer, found `{}`",
                                    self.type_to_string(&len_ty)
                                )));
                            }
                            let cap_ty = self.check_expr(cap_expr);
                            self.check_and_record_value_use(cap_expr, &cap_ty);
                            if !self.is_integer(&cap_ty) {
                                self.record(TypeErrorKind::Generic(format!(
                                    "$own's cap argument must be an integer, found `{}`",
                                    self.type_to_string(&cap_ty)
                                )));
                            }
                            HirType::Slice(self.context.bump.alloc_value(pointee))
                        } else if let Some(len_expr) = len_arg {
                            // Old 3-arg / 2-arg count-only shape with no cap: reject for
                            // slice pointees now, since there's no way to recover a correct
                            // cap. Non-slice owned pointers never take this branch (pointee
                            // wouldn't be wrapped in Slice), so this only fires for the
                            // ambiguous case this change intentionally closes off.
                            let len_ty = self.check_expr(len_expr);
                            self.check_and_record_value_use(len_expr, &len_ty);
                            if !self.is_integer(&len_ty) {
                                self.record(TypeErrorKind::Generic(format!(
                                    "$own's argument must be an integer, found `{}`",
                                    self.type_to_string(&len_ty)
                                )));
                            }
                            self.record(TypeErrorKind::Generic(
                                "$own for an owned slice requires both `len` and `cap`: use \
                                 `$own(ptr, allocator, len, cap)`"
                                    .to_string(),
                            ));
                            HirType::Slice(self.context.bump.alloc_value(pointee))
                        } else {
                            *pointee
                        };

                        HirType::OwnedPointer {
                            inner: self.context.bump.alloc_value(result_inner),
                            allocator: Some(allocator),
                        }
                    }
                    IntrinsicKind::AssertAlign => {
                        if !type_args.is_empty() {
                            self.record(TypeErrorKind::Generic(
                                "$assert_align takes no type arguments".to_string(),
                            ));
                        }
                        if args.len() != 2 {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args: 2,
                                found_args: args.len(),
                            });
                        } else {
                            let ptr_ty = self.check_expr(&args[0]);
                            self.check_and_record_value_use(&args[0], &ptr_ty);
                            if !matches!(
                                Self::strip_ref(&ptr_ty),
                                HirType::SafePointer { .. }
                                    | HirType::UnsafePointer { .. }
                                    | HirType::OwnedPointer { .. }
                            ) {
                                self.record(TypeErrorKind::Generic(format!(
                                    "$assert_align expects a pointer, found `{}`",
                                    self.type_to_string(&ptr_ty)
                                )));
                            }

                            let align_ty = self.check_expr(&args[1]);
                            if !self.is_integer(&align_ty) {
                                self.record(TypeErrorKind::Generic(format!(
                                    "$assert_align expects an integer alignment, found `{}`",
                                    self.type_to_string(&align_ty)
                                )));
                            }
                            if let HirExpr::Number(n, _) = &args[1] {
                                if *n <= 0 || (*n as u64) & ((*n as u64) - 1) != 0 {
                                    self.record(TypeErrorKind::Generic(format!(
                                        "alignment must be a positive power of two, found {}",
                                        n
                                    )));
                                }
                            }
                        }
                        HirType::Void
                    }
                    IntrinsicKind::AtomicCasU32 => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "$atomic_cas_u32 requires an unsafe block".to_string(),
                            ));
                        }
                        if args.len() != 3 {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args: 3,
                                found_args: args.len(),
                            });
                            return HirType::U32;
                        }
                        let ptr_ty = self.check_expr(&args[0]);
                        self.check_and_record_value_use(&args[0], &ptr_ty);
                        let points_to_u32 = matches!(
                            Self::strip_ref(&ptr_ty),
                            HirType::SafePointer { inner, .. } | HirType::UnsafePointer { inner, .. }
                                if matches!(**inner, HirType::U32)
                        );
                        if !points_to_u32 {
                            self.record(TypeErrorKind::Generic(format!(
                                "$atomic_cas_u32 expects a pointer to `u32`, found `{}`",
                                self.type_to_string(&ptr_ty)
                            )));
                        }
                        for a in &args[1..] {
                            let t = self.check_expr(a);
                            self.check_and_record_value_use(a, &t);
                            if !matches!(t, HirType::U32) {
                                self.record(TypeErrorKind::TypeMismatch {
                                    expected: "u32".to_string(),
                                    found: self.type_to_string(&t),
                                });
                            }
                        }
                        HirType::U32
                    }

                    IntrinsicKind::AtomicLoadU32 => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "$atomic_load_u32 requires an unsafe block".to_string(),
                            ));
                        }
                        if args.len() != 1 {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args: 1,
                                found_args: args.len(),
                            });
                            return HirType::U32;
                        }
                        let ptr_ty = self.check_expr(&args[0]);
                        self.check_and_record_value_use(&args[0], &ptr_ty);
                        let points_to_u32 = matches!(
                            Self::strip_ref(&ptr_ty),
                            HirType::SafePointer { inner, .. } | HirType::UnsafePointer { inner, .. }
                                if matches!(**inner, HirType::U32)
                        );
                        if !points_to_u32 {
                            self.record(TypeErrorKind::Generic(format!(
                                "$atomic_load_u32 expects a pointer to `u32`, found `{}`",
                                self.type_to_string(&ptr_ty)
                            )));
                        }
                        HirType::U32
                    }

                    IntrinsicKind::AtomicStoreU32 => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "$atomic_store_u32 requires an unsafe block".to_string(),
                            ));
                        }
                        if args.len() != 2 {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args: 2,
                                found_args: args.len(),
                            });
                            return HirType::Void;
                        }
                        let ptr_ty = self.check_expr(&args[0]);
                        self.check_and_record_value_use(&args[0], &ptr_ty);
                        let points_to_u32 = matches!(
                            Self::strip_ref(&ptr_ty),
                            HirType::SafePointer { inner, .. } | HirType::UnsafePointer { inner, .. }
                                if matches!(**inner, HirType::U32)
                        );
                        if !points_to_u32 {
                            self.record(TypeErrorKind::Generic(format!(
                                "$atomic_store_u32 expects a pointer to `u32`, found `{}`",
                                self.type_to_string(&ptr_ty)
                            )));
                        }
                        let val_ty = self.check_expr(&args[1]);
                        self.check_and_record_value_use(&args[1], &val_ty);
                        if !matches!(val_ty, HirType::U32) {
                            self.record(TypeErrorKind::TypeMismatch {
                                expected: "u32".to_string(),
                                found: self.type_to_string(&val_ty),
                            });
                        }
                        HirType::Void
                    }

                    IntrinsicKind::CpuRelax => {
                        if !args.is_empty() {
                            self.record(TypeErrorKind::Generic(
                                "$cpu_relax takes no arguments".to_string(),
                            ));
                        }
                        HirType::Void
                    }
                }
            }
            HirExpr::If { if_stmt, span } => {
                self.set_span(*span);
                let HirStmt::If { else_block, .. } = if_stmt else {
                    unreachable!()
                };
                if else_block.is_none() {
                    self.record(TypeErrorKind::Generic(
                        "if used as an expression must have an else branch".to_string(),
                    ));
                }
                self.check_stmt(if_stmt).unwrap_or(HirType::Void)
            }

            HirExpr::Match { expr, arms, span } => {
                self.set_span(*span);
                let scrutinee_ty = self.check_expr(expr);

                self.check_match_exhaustiveness(&scrutinee_ty, arms);

                let move_state_before = self.move_state.clone();
                let mut arm_types = Vec::with_capacity(arms.len());
                let mut arm_move_states = Vec::with_capacity(arms.len());

                for arm in *arms {
                    self.move_state = move_state_before.clone();
                    self.borrow_checker.begin_scope();
                    let arm_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, arm_context);

                    self.check_pattern_against_type(&arm.pattern, &scrutinee_ty);
                    self.register_pattern_bindings(&arm.pattern, &scrutinee_ty);

                    if let Some(guard) = arm.guard {
                        let guard_type = self.check_expr(guard);
                        if guard_type != HirType::Boolean {
                            self.record(TypeErrorKind::TypeMismatch {
                                expected: "bool".to_string(),
                                found: self.type_to_string(&guard_type),
                            });
                        }
                    }

                    let arm_ty = self.check_stmt(arm.body).unwrap_or(HirType::Void);
                    self.context = old_context;
                    self.borrow_checker.end_scope();
                    arm_move_states.push(self.move_state.clone());
                    arm_types.push(arm_ty);
                }

                self.move_state = arm_move_states
                    .into_iter()
                    .fold(move_state_before, |acc, s| MoveState::join(&acc, &s));
                self.join_value_types(&arm_types)
            }

            HirExpr::Block {
                body,
                is_unsafe,
                span,
            } => {
                self.set_span(*span);
                if *is_unsafe {
                    self.unsafe_depth += 1;
                }

                self.borrow_checker.begin_scope();
                let mut block_context = self.context.create_child_scope();
                let mut value = HirType::Void;
                for stmt in *body {
                    let old_context = std::mem::replace(&mut self.context, block_context);
                    value = self.check_stmt(stmt).unwrap_or(HirType::Void);
                    block_context = self.context.clone();
                    self.context = old_context;
                }
                self.borrow_checker.end_scope();

                if *is_unsafe {
                    self.unsafe_depth -= 1;
                }
                value
            }

            HirExpr::Range {
                start,
                end,
                inclusive,
                span,
            } => {
                self.set_span(*span);
                let start_ty = self.check_expr(start);
                let end_ty = self.check_expr(end);
                self.check_and_record_value_use(start, &start_ty);
                self.check_and_record_value_use(end, &end_ty);
                if !self.is_integer(&start_ty) {
                    self.record(TypeErrorKind::Generic(format!(
                        "range bounds must be integers, found `{}`",
                        self.type_to_string(&start_ty)
                    )));
                }
                let result = self.types_compatible(&start_ty, &end_ty);
                self.recover(result, ());
                HirType::Range {
                    elem: self.context.bump.alloc_value(start_ty),
                    inclusive: *inclusive,
                }
            }

            HirExpr::Slice {
                object,
                start,
                end,
                inclusive: _,
                span,
            } => {
                self.set_span(*span);
                let object_ty = self.check_expr(object);
                let start_ty = self.check_expr(start);
                let end_ty = self.check_expr(end);
                if !self.is_integer(&start_ty) || !self.is_integer(&end_ty) {
                    self.record(TypeErrorKind::Generic(
                        "slice bounds must be integers".to_string(),
                    ));
                }
                match *Self::strip_ref(&object_ty) {
                    HirType::Array(inner, _) | HirType::Slice(inner) => HirType::Slice(inner),
                    _ => {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot slice type `{}`",
                            self.type_to_string(&object_ty)
                        )));
                        HirType::Unknown
                    }
                }
            }
            HirExpr::Call {
                callee,
                args,
                span,
                type_args,
            } => match &callee {
                HirExpr::Ident(func_name, ident_span) => {
                    self.set_span(*ident_span);
                    let lookup_name = self.str_id_to_string(*func_name);
                    let func = match self.context.get_function(&lookup_name) {
                        Some(f) => f,
                        None => {
                            self.record(TypeErrorKind::UndefinedFunction(lookup_name));
                            return HirType::Unknown;
                        }
                    };
                    self.check_unsafe_call(&func, &lookup_name);

                    self.record_item_occurrence(
                        *ident_span,
                        *func_name,
                        func.return_type.unwrap_or(HirType::Void),
                        func.declaring_module_idx,
                    );

                    self.recover(
                        self.check_visibility(
                            func.function_metadata.visibility,
                            func.declaring_module_idx,
                            "function",
                            &lookup_name,
                        ),
                        (),
                    );
                    self.check_name_import_visibility(*func_name, &lookup_name);

                    let substitutions: FxHashMap<StrId, HirType<'a, 'bump>> = match func.generics {
                        Some(tp) if !tp.is_empty() => match type_args {
                            Some(ta) => {
                                if ta.len() != tp.len() {
                                    self.record(TypeErrorKind::Generic(format!(
                                        "function `{}` expects {} type argument(s), found {}",
                                        lookup_name,
                                        tp.len(),
                                        ta.len()
                                    )));
                                }
                                let mut map = FxHashMap::default();
                                tp.iter().zip(ta.iter()).map(|(&p, &a)| (p, a)).for_each(
                                    |(p, a)| {
                                        map.insert(p.name, a);
                                    },
                                );
                                map
                            }
                            None => {
                                self.record(TypeErrorKind::Generic(format!(
                                "generic function `{}` requires explicit type arguments, e.g. `{}<Type>(...)`",
                                lookup_name, lookup_name
                            )));
                                FxHashMap::default()
                            }
                        },
                        _ => {
                            if type_args.is_some() {
                                self.record(TypeErrorKind::Generic(format!(
                                    "function `{}` is not generic; no type arguments expected",
                                    lookup_name
                                )));
                            }
                            FxHashMap::default()
                        }
                    };

                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }

                    let Some(params) = func.params else {
                        let ret_ty = func.return_type.unwrap_or(HirType::Void);
                        return if substitutions.is_empty() {
                            ret_ty
                        } else {
                            self.substitute_type_local(&ret_ty, &substitutions)
                        };
                    };

                    let params = if substitutions.is_empty() {
                        params
                    } else {
                        self.substitute_params_local(params, &substitutions)
                    };

                    let unsubstituted_ret_ty = func.return_type.unwrap_or(HirType::Void);
                    let ret_ty = if substitutions.is_empty() {
                        unsubstituted_ret_ty
                    } else {
                        self.substitute_type_local(&unsubstituted_ret_ty, &substitutions)
                    };

                    if let Some(value) =
                        self.check_potential_this_param_for_move(args, func, params, ret_ty)
                    {
                        return value;
                    }

                    let read_templates = self.analyze_read_templates(&func);
                    for (arg_idx, arg) in args.iter().enumerate() {
                        if let HirExpr::Ref {
                            expr: inner,
                            mutable: false,
                            ..
                        } = arg
                        {
                            if let Some(template) = read_templates.get(arg_idx) {
                                self.check_call_arg_read_effects(inner, template, args);
                            }
                        }
                    }

                    let arg_loans = self.check_all_func_args(args, params, None);

                    if !self.return_type_may_alias(&ret_ty) {
                        for loan in arg_loans {
                            self.borrow_checker.end_loan_now(loan);
                        }
                    }

                    ret_ty
                }

                HirExpr::FieldAccess {
                    object,
                    field,
                    span,
                } => {
                    self.set_span(*span);
                    let obj_type = self.check_expr(object);
                    let stripped = Self::strip_ref(&obj_type);

                    let interface_name = match stripped {
                        HirType::DynInterface(name, _) => Some(name.to_string()),
                        HirType::Dyn { bounds } => bounds.iter().find_map(|b| match b {
                            HirType::DynInterface(name, _) => Some(name.to_string()),
                            HirType::Struct { name, .. } => {
                                let name_str = name.to_string();
                                self.context.get_interface(&name_str).map(|_| name_str)
                            }
                            _ => None,
                        }),
                        _ => None,
                    };

                    if let Some(iface_name) = interface_name {
                        let method_name = field.to_string();
                        let iface = match self.context.get_interface(&iface_name) {
                            Some(i) => i,
                            None => {
                                self.record(TypeErrorKind::UndefinedType(iface_name));
                                return HirType::Unknown;
                            }
                        };

                        let method = iface.methods.and_then(|methods| {
                            methods
                                .iter()
                                .find(|m| m.unmangled_name.to_string() == method_name)
                        });
                        let Some(method) = method else {
                            self.record(TypeErrorKind::Generic(format!(
                                "no method `{}` on interface `{}`",
                                method_name, iface_name
                            )));
                            return HirType::Unknown;
                        };

                        let total_params = method.params.map(|p| p.len()).unwrap_or(0);
                        let expected_args = total_params.saturating_sub(1);
                        if args.len() != expected_args {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args,
                                found_args: args.len(),
                            });
                        }

                        if let Some(params) = method.params {
                            if let Some(HirParam::This { kind, span: _ }) = params.first() {
                                if matches!(kind, ThisPassingKind::Move | ThisPassingKind::MoveMut)
                                {
                                    self.check_and_record_value_use(object, &obj_type);
                                }
                            }
                            for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                                let arg_type = self.check_expr(arg);
                                self.check_and_record_value_use(arg, &arg_type);
                                if let Some(param_type) = param.get_type() {
                                    let result = self.types_compatible(param_type, &arg_type);
                                    self.recover(result, ());
                                }
                            }
                        }

                        return method.return_type.unwrap_or(HirType::Void);
                    }

                    let (struct_name_id, type_name, func) =
                        match self.resolve_callable_method(stripped, &field.to_string()) {
                            Some(found) => found,
                            None => {
                                self.record(TypeErrorKind::Generic(format!(
                                    "no method `{}` on `{}`",
                                    field,
                                    self.type_to_string(&obj_type)
                                )));
                                return HirType::Unknown;
                            }
                        };
                    self.check_unsafe_call(&func, &format!("{}.{}", type_name, field));

                    let total_params = func.params.map(|p| p.len()).unwrap_or(0);
                    let expected_args = total_params.saturating_sub(1);
                    if args.len() != expected_args {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }

                    let ret_ty = func.return_type.unwrap_or(HirType::Void);
                    self.record_method_occurrence(*span, *field, ret_ty, struct_name_id);

                    let template = if self.return_type_may_alias(&ret_ty) {
                        Some(self.analyze_ref_template(&func))
                    } else {
                        None
                    };

                    if let Some(params) = func.params {
                        if let Some(HirParam::This { kind, span: _ }) = params.first() {
                            let requires_mut = matches!(
                                kind,
                                ThisPassingKind::RefMut
                                    | ThisPassingKind::MutSafePtr
                                    | ThisPassingKind::MoveMut
                            );
                            if requires_mut {
                                let result = self.check_receiver_is_mutable(object, field.as_str());
                                self.recover(result, ());
                            }

                            let has_precise_template =
                                matches!(template, Some(RefTemplate::Path { .. }));

                            if matches!(kind, ThisPassingKind::Move | ThisPassingKind::MoveMut) {
                                self.check_and_record_value_use(object, &obj_type);
                            } else if !has_precise_template {
                                if let Some(place) = self.resolve_place(object) {
                                    let borrow_kind = if requires_mut {
                                        BorrowKind::Mutable
                                    } else {
                                        BorrowKind::Shared
                                    };

                                    if !requires_mut && !self.return_type_may_alias(&ret_ty) {
                                        self.check_borrow_use_shell(expr, place, borrow_kind);
                                    } else {
                                        self.check_borrow_use(expr, place, borrow_kind);
                                    }
                                }
                            }

                            // defer entirely to finalize_call_loans below, which checks
                            // the precise resolved place (such as ptr[Const(1)] vs
                            // ptr[Const(2)]) and can prove index-disjointness that the
                            // whole-receiver check can't.
                        }

                        for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                            let param_type = param.get_type();
                            let arg_type = match param_type {
                                Some(pt) => self.check_expr_expected(arg, pt),
                                None => self.check_expr(arg),
                            };
                            self.check_and_record_value_use(arg, &arg_type);
                            if let Some(pt) = param_type {
                                self.recover(self.types_compatible(pt, &arg_type), ());
                            }
                        }

                        if let Some(loan_id) = self.finalize_call_loans(
                            Some(object),
                            args,
                            Vec::new(),
                            &ret_ty,
                            template,
                        ) {
                            self.call_loans.insert(Self::expr_key(expr), loan_id);
                        }
                    }

                    ret_ty
                }
                HirExpr::ModuleAccess(access) => {
                    self.set_span(access.span);
                    let member_name = access.member.to_string();

                    // First: is access.path a single-segment local alias registered via
                    // `import foo::bar.Alias;`? Check that before treating path as a
                    // literal package path.
                    let is_named_import = if access.path.len() == 1 {
                        self.imports_by_module
                            .get(&self.context.current_module_idx)
                            .map(|imp| imp.named.contains_key(&access.path[0]))
                            .unwrap_or(false)
                    } else {
                        false
                    };

                    let alias_module_idx: Option<usize> = if access.path.len() == 1 {
                        self.imports_by_module
                            .get(&self.context.current_module_idx)
                            .and_then(|imp| {
                                imp.named
                                    .get(&access.path[0])
                                    .or_else(|| imp.module_aliases.get(&access.path[0]))
                            })
                            .copied()
                    } else {
                        None
                    };

                    let (resolved_module_idx, assoc_type_name): (Option<usize>, Option<StrId>) =
                        if let Some(midx) = alias_module_idx {
                            if is_named_import {
                                (Some(midx), Some(access.path[0]))
                            } else {
                                (Some(midx), None)
                            }
                        } else {
                            match self
                                .context
                                .dep_graph
                                .borrow()
                                .resolve_module_path(access.path)
                            {
                                Some(midx) => (Some(midx), None),
                                None => match access.path.split_last() {
                                    Some((&type_seg, module_path)) => {
                                        let midx = self
                                            .context
                                            .dep_graph
                                            .borrow()
                                            .resolve_module_path(module_path);
                                        (midx, midx.map(|_| type_seg))
                                    }
                                    None => (None, None),
                                },
                            }
                        };

                    // free_func now needs to try BOTH interpretations when alias-resolved,
                    // since a named import could name either a type or a free function.
                    let free_func = resolved_module_idx
                        .and_then(|midx| self.context.get_module_function(midx, &member_name));

                    let mangled_type_name: Option<String> = assoc_type_name.and_then(|t| {
                        let midx = resolved_module_idx?;
                        let pkg = self.context.dep_graph.borrow().get_module_package(midx)?;
                        Some(format!("{}_{}", pkg.to_string(), t.to_string()))
                    });

                    let method_func = if free_func.is_none() {
                        mangled_type_name
                            .or_else(|| access.path.last().map(|s| s.to_string()))
                            .and_then(|tn| self.context.get_method(&tn, &member_name).copied())
                    } else {
                        None
                    };

                    let func = match free_func.or(method_func) {
                        Some(f) => f,
                        None => {
                            let path_str = access
                                .path
                                .iter()
                                .map(|s| s.to_string())
                                .collect::<Vec<_>>()
                                .join("::");
                            let qualified_name = format!("{}::{}", path_str, member_name);
                            let candidate_modules = self
                                .context
                                .dep_graph
                                .borrow()
                                .find_function_by_name_anywhere(access.member);
                            if candidate_modules.is_empty() {
                                self.record(TypeErrorKind::UndefinedFunction(qualified_name));
                            } else {
                                let suggestion_paths: Vec<String> = candidate_modules
                                    .iter()
                                    .filter_map(|&midx| {
                                        self.context.dep_graph.borrow().get_module_package(midx)
                                    })
                                    .map(|pkg| pkg.to_string())
                                    .collect();
                                self.record(TypeErrorKind::UndefinedFunctionWithSuggestion {
                                    name: qualified_name,
                                    suggested_modules: suggestion_paths,
                                });
                            }

                            return HirType::Unknown;
                        }
                    };

                    self.check_module_path_imported(access.path);

                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }
                    if let Some(params) = func.params {
                        let read_templates = self.analyze_read_templates(&func);
                        for (arg_idx, arg) in args.iter().enumerate() {
                            if let HirExpr::Ref {
                                expr: inner,
                                mutable: false,
                                ..
                            } = arg
                            {
                                if let Some(template) = read_templates.get(arg_idx) {
                                    self.check_call_arg_read_effects(inner, template, args);
                                }
                            }
                        }

                        let arg_loans = self.check_all_func_args(args, params, None);

                        let ret_ty = func.return_type.unwrap_or(HirType::Void);
                        if !self.return_type_may_alias(&ret_ty) {
                            for loan in arg_loans {
                                self.borrow_checker.end_loan_now(loan);
                            }
                        }
                    }
                    let ret_ty = func.return_type.unwrap_or(HirType::Void);
                    if let Some(midx) = resolved_module_idx {
                        if free_func.is_some() {
                            self.record_item_occurrence(access.span, access.member, ret_ty, midx);
                        } else if let Some(target_type) = assoc_type_name {
                            self.record_method_occurrence(
                                access.span,
                                access.member,
                                ret_ty,
                                target_type,
                            );
                        }
                    }
                    ret_ty
                }

                other => {
                    self.set_span(*span);
                    let callee_type = self.check_expr(other);
                    match callee_type {
                        HirType::Lambda { return_type, .. } => *return_type,
                        _ => {
                            self.record(TypeErrorKind::Generic(format!(
                                "Expression of type `{}` is not callable",
                                self.type_to_string(&callee_type)
                            )));
                            HirType::Unknown
                        }
                    }
                }
            },
            HirExpr::FieldAccess {
                object,
                field,
                span,
            } => {
                self.set_span(*span);
                self.check_field_access(object, *field)
            }
            HirExpr::StructInit {
                name,
                args,
                span,
                type_args,
            } => {
                self.set_span(*span);
                let HirExpr::Ident(struct_name_id, name_span) = name else {
                    eprintln!("name failed {name:?}");
                    return HirType::Void;
                };
                let struct_name_str = self.str_id_to_string(*struct_name_id);
                let Some(ty_struct) = self.context.get_struct(&struct_name_str) else {
                    self.record(TypeErrorKind::UndefinedType(struct_name_str));
                    return HirType::Struct {
                        name: *struct_name_id,
                        field_types: &[],
                        type_args: type_args.unwrap_or(&[]),
                    };
                };
                self.check_bare_name_import(
                    self.context.struct_owner(&struct_name_str),
                    *struct_name_id,
                    &struct_name_str,
                    BareImportKind::Struct,
                );

                let is_generic_decl = ty_struct.generics.is_some_and(|g| !g.is_empty());

                let resolved_field_types: Vec<HirType<'a, 'bump>> = match (
                    is_generic_decl,
                    type_args,
                ) {
                    (true, Some(ta)) => match self.instantiate_struct(*struct_name_id, ta) {
                        Some(fields) => fields.to_vec(),
                        None => {
                            self.record(TypeErrorKind::Generic(format!(
                                "struct `{}` expects {} type argument(s), found {}",
                                struct_name_str,
                                ty_struct.generics.map(|g| g.len()).unwrap_or(0),
                                ta.len(),
                            )));
                            ty_struct.fields.iter().map(|f| f.field_type).collect()
                        }
                    },
                    (true, None) => {
                        self.record(TypeErrorKind::Generic(format!(
                            "struct `{}` is generic and requires explicit type arguments, e.g. `{}<Type> {{ .. }}`",
                            struct_name_str, struct_name_str,
                        )));
                        ty_struct.fields.iter().map(|f| f.field_type).collect()
                    }
                    (false, Some(_)) => {
                        self.record(TypeErrorKind::Generic(format!(
                            "struct `{}` is not generic; no type arguments expected",
                            struct_name_str,
                        )));
                        ty_struct.fields.iter().map(|f| f.field_type).collect()
                    }
                    (false, None) => ty_struct.fields.iter().map(|f| f.field_type).collect(),
                };

                let mut seen: std::collections::HashSet<StrId> = std::collections::HashSet::new();

                for field_init in *args {
                    let field_name_str = self.str_id_to_string(field_init.name);
                    let field_idx = ty_struct
                        .fields
                        .iter()
                        .position(|f| self.str_id_to_string(f.name) == field_name_str);

                    let Some(field_idx) = field_idx else {
                        self.record(TypeErrorKind::FieldNotFound {
                            struct_name: struct_name_str.clone(),
                            field: field_name_str,
                        });
                        self.check_expr(&field_init.value);
                        continue;
                    };
                    let field_type = resolved_field_types[field_idx];

                    if !seen.insert(field_init.name) {
                        self.record(TypeErrorKind::Generic(format!(
                            "field `{}` initialized more than once",
                            field_name_str
                        )));
                    }

                    let arg_type = self.check_expr(&field_init.value);
                    self.check_and_record_value_use(&field_init.value, &arg_type);
                    let result = self.types_compatible(&field_type, &arg_type);
                    self.recover(result, ());

                    self.occurrences.push((
                        field_init.name_span,
                        field_init.name,
                        field_type,
                        self.context.current_module_idx,
                        SymbolId::Field {
                            struct_name: *struct_name_id,
                            field_name: field_init.name,
                        },
                        false,
                    ));
                }

                let missing: Vec<&str> = ty_struct
                    .fields
                    .iter()
                    .filter(|f| !args.iter().any(|a| a.name == f.name))
                    .map(|f| f.name.as_str())
                    .collect();
                if !missing.is_empty() {
                    self.record(TypeErrorKind::Generic(format!(
                        "missing field(s) in struct init: {}",
                        missing.join(", ")
                    )));
                }

                let result_ty = HirType::Struct {
                    name: *struct_name_id,
                    field_types: self.context.bump.alloc_slice(&resolved_field_types),
                    type_args: type_args.unwrap_or(&[]),
                };
                if let Some(owner) = self.context.struct_owner(&struct_name_str) {
                    self.record_item_occurrence(*name_span, *struct_name_id, result_ty, owner);
                }
                result_ty
            }
            HirExpr::InterfaceCall {
                callee,
                interface,
                args,
                ..
            } => {
                let _ = self.check_expr(callee);
                let iface_name = interface.to_string();
                for arg in *args {
                    let arg_type = self.check_expr(arg);
                    self.check_and_record_value_use(arg, &arg_type);
                }

                match self.context.get_interface(&iface_name) {
                    Some(iface) => iface
                        .methods
                        .and_then(|methods| methods.first())
                        .and_then(|m| m.return_type)
                        .unwrap_or(HirType::Void),
                    None => {
                        self.record(TypeErrorKind::UndefinedType(iface_name));
                        HirType::Unknown
                    }
                }
            }

            HirExpr::Assignment {
                target,
                op,
                value,
                span,
            } => {
                self.set_span(*span);
                let value_type = self.check_expr(value);
                let target_type = self.check_expr(target);

                // Borrow-check the write target uniformly: *p = .., obj.field = ..,
                // arr[i] = .. all need the same overlap check, Special-casing only
                // Deref here
                if let Some(place) = self.resolve_place(target) {
                    self.check_borrow_use(target, place, BorrowKind::Mutable);
                }

                if let HirExpr::Ident(name, _) = target {
                    let var_name = self.str_id_to_string(*name);
                    if self.context.is_local_binding(&var_name)
                        && !self.context.is_mutable(&var_name)
                    {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot assign to `{}`: it is not declared `mut`",
                            var_name
                        )));
                    }
                }

                use ir::hir::AssignmentOperator::*;
                let bin_result = match op {
                    Assign => self.types_compatible(&target_type, &value_type),
                    AddAssign => self
                        .check_binary_op(&target_type, &Operator::Add, &value_type)
                        .map(|_| ()),
                    SubtractAssign => self
                        .check_binary_op(&target_type, &Operator::Subtract, &value_type)
                        .map(|_| ()),
                    MultiplyAssign => self
                        .check_binary_op(&target_type, &Operator::Multiply, &value_type)
                        .map(|_| ()),
                    DivideAssign => self
                        .check_binary_op(&target_type, &Operator::Divide, &value_type)
                        .map(|_| ()),
                    ModuloAssign => self
                        .check_binary_op(&target_type, &Operator::Modulo, &value_type)
                        .map(|_| ()),
                    BitAndAssign => self
                        .check_binary_op(&target_type, &Operator::BitAnd, &value_type)
                        .map(|_| ()),
                    BitOrAssign => self
                        .check_binary_op(&target_type, &Operator::BitOr, &value_type)
                        .map(|_| ()),
                    BitXorAssign => self
                        .check_binary_op(&target_type, &Operator::BitXor, &value_type)
                        .map(|_| ()),
                    ShiftLeftAssign => self
                        .check_binary_op(&target_type, &Operator::ShiftLeft, &value_type)
                        .map(|_| ()),
                    ShiftRightAssign => self
                        .check_binary_op(&target_type, &Operator::ShiftRight, &value_type)
                        .map(|_| ()),
                };
                self.recover(bin_result, ());

                target_type
            }
            HirExpr::InterpolatedString(parts) => {
                for part in *parts {
                    if let ir::hir::InterpolationPart::Expr(e) = part {
                        self.check_expr(e);
                    }
                }
                HirType::String
            }
            HirExpr::EnumInit {
                enum_name,
                variant,
                args,
                type_args,
                span: _,
            } => {
                let enum_name_str = self.str_id_to_string(*enum_name);
                let Some(enum_def) = self.context.get_enum(&enum_name_str) else {
                    self.record(TypeErrorKind::UndefinedType(enum_name_str));
                    return HirType::Unknown;
                };
                self.check_bare_name_import(
                    self.context.enum_owner(&enum_name_str),
                    *enum_name,
                    &enum_name_str,
                    BareImportKind::Enum,
                );

                let variant_name = self.str_id_to_string(*variant);
                let variant_def = enum_def
                    .variants
                    .iter()
                    .find(|v| self.str_id_to_string(v.name) == variant_name);
                let Some(variant_def) = variant_def else {
                    self.record(TypeErrorKind::Generic(format!(
                        "enum `{}` has no variant `{}`",
                        enum_name_str, variant_name
                    )));
                    return HirType::Unknown;
                };

                let is_generic_decl = enum_def.generics.is_some_and(|g| !g.is_empty());

                let resolved_field_types: Vec<HirType<'a, 'bump>> = match (
                    is_generic_decl,
                    type_args,
                ) {
                    (true, Some(ta)) => match self.instantiate_enum(*enum_name, ta) {
                        Some(variants) => variants
                            .iter()
                            .find(|(name, _)| *name == *variant)
                            .map(|(_, fields)| fields.to_vec())
                            .unwrap_or_else(|| {
                                variant_def.fields.iter().map(|f| f.field_type).collect()
                            }),
                        None => {
                            self.record(TypeErrorKind::Generic(format!(
                                "enum `{}` expects {} type argument(s), found {}",
                                enum_name_str,
                                enum_def.generics.map(|g| g.len()).unwrap_or(0),
                                ta.len(),
                            )));
                            variant_def.fields.iter().map(|f| f.field_type).collect()
                        }
                    },
                    (true, None) => {
                        self.record(TypeErrorKind::Generic(format!(
                            "enum `{}` is generic and requires explicit type arguments, e.g. `{}<Type>::{}(..)`",
                            enum_name_str, enum_name_str, variant_name,
                        )));
                        variant_def.fields.iter().map(|f| f.field_type).collect()
                    }
                    (false, Some(_)) => {
                        self.record(TypeErrorKind::Generic(format!(
                            "enum `{}` is not generic; no type arguments expected",
                            enum_name_str,
                        )));
                        variant_def.fields.iter().map(|f| f.field_type).collect()
                    }
                    (false, None) => variant_def.fields.iter().map(|f| f.field_type).collect(),
                };

                if args.len() != resolved_field_types.len() {
                    self.record(TypeErrorKind::InvalidFunctionCall {
                        expected_args: resolved_field_types.len(),
                        found_args: args.len(),
                    });
                }

                let mut arg_types: Vec<HirType<'a, 'bump>> = Vec::with_capacity(args.len());
                for (arg, field_type) in args.iter().zip(resolved_field_types.iter()) {
                    let arg_type = self.check_expr_expected(arg, field_type);
                    self.check_and_record_value_use(arg, &arg_type);
                    self.recover(self.types_compatible(field_type, &arg_type), ());
                    arg_types.push(arg_type);
                }

                if let Some(ta) = type_args {
                    self.record_instance_args(expr, ta);
                }

                let final_type_args: &'bump [HirType<'a, 'bump>] = if let Some(ta) = type_args {
                    ta
                } else if is_generic_decl {
                    let generics = enum_def.generics.unwrap_or(&[]);
                    let mut subs: FxHashMap<StrId, HirType<'a, 'bump>> = FxHashMap::default();
                    for (declared_field, actual_ty) in
                        variant_def.fields.iter().zip(arg_types.iter())
                    {
                        self.unify_generic(&declared_field.field_type, actual_ty, &mut subs);
                    }
                    let inferred: Vec<HirType<'a, 'bump>> = generics
                        .iter()
                        .map(|g| subs.get(&g.name).copied().unwrap_or(HirType::Unknown))
                        .collect();
                    self.context.bump.alloc_slice_copy(&inferred)
                } else {
                    &[]
                };

                HirType::Enum(*enum_name, final_type_args)
            }
            HirExpr::ExprList { list, span } => {
                self.set_span(*span);
                let mut last = HirType::Void;
                for e in *list {
                    last = self.check_expr(e);
                }
                last
            }
            HirExpr::Get {
                object,
                field,
                span,
            } => {
                self.set_span(*span);
                self.check_field_access(object, *field)
            }
            HirExpr::Comparison {
                left,
                op,
                right,
                span,
            } => {
                self.set_span(*span);
                let left_type = self.check_expr(left);
                let right_type = self.check_expr(right);
                let result = self.check_binary_op(&left_type, op, &right_type);
                self.recover(result, HirType::Unknown)
            }
            HirExpr::Deref { expr, span } => {
                self.set_span(*span);
                let inner_ty = self.check_expr(expr);
                if let Some(base) = self.resolve_place(expr) {
                    let place = self.borrow_checker.project_deref(base);
                    self.check_borrow_use(expr, place, BorrowKind::Shared);
                }
                match inner_ty {
                    HirType::Ref { inner, .. } => *inner,
                    HirType::SafePointer { inner, .. } => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "dereferencing a raw pointer requires an unsafe block".into(),
                            ));
                        }

                        *inner
                    }

                    HirType::UnsafePointer { inner, .. } => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "dereferencing an unsafe pointer requires an unsafe block".into(),
                            ));
                        }

                        *inner
                    }
                    HirType::OwnedPointer { inner, .. } => *inner,
                    _ => {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot dereference non-pointer type `{}`",
                            self.type_to_string(&inner_ty)
                        )));
                        HirType::Unknown
                    }
                }
            }
            HirExpr::Ref {
                expr,
                mutable,
                span,
            } => self.check_ref_expr(expr, *mutable, *span, true),
            HirExpr::This { span } => {
                self.set_span(*span);
                let (symbol_id, ty) = self
                    .context
                    .get_variable("this")
                    .unwrap_or((SymbolId::Local(LocalSymbolId(u32::MAX)), HirType::This));
                self.occurrences.push((
                    *span,
                    self.this_id,
                    ty,
                    self.context.current_module_idx,
                    symbol_id,
                    false,
                ));
                ty
            }
            HirExpr::ModuleAccess(access) => {
                let member_name = access.member.to_string();

                // is access.path a single-segment local alias registered via
                // `import foo::bar.Alias;`? Check that before treating path as a
                // literal package path.
                let is_named_import = if access.path.len() == 1 {
                    self.imports_by_module
                        .get(&self.context.current_module_idx)
                        .map(|imp| imp.named.contains_key(&access.path[0]))
                        .unwrap_or(false)
                } else {
                    false
                };

                let alias_module_idx: Option<usize> = if access.path.len() == 1 {
                    self.imports_by_module
                        .get(&self.context.current_module_idx)
                        .and_then(|imp| {
                            imp.named
                                .get(&access.path[0])
                                .or_else(|| imp.module_aliases.get(&access.path[0]))
                        })
                        .copied()
                } else {
                    None
                };

                let (resolved_module_idx, assoc_type_name): (Option<usize>, Option<StrId>) =
                    if let Some(midx) = alias_module_idx {
                        if is_named_import {
                            (Some(midx), Some(access.path[0]))
                        } else {
                            (Some(midx), None)
                        }
                    } else {
                        match self
                            .context
                            .dep_graph
                            .borrow()
                            .resolve_module_path(access.path)
                        {
                            Some(midx) => (Some(midx), None),
                            None => match access.path.split_last() {
                                Some((&type_seg, module_path)) => {
                                    let midx = self
                                        .context
                                        .dep_graph
                                        .borrow()
                                        .resolve_module_path(module_path);
                                    (midx, midx.map(|_| type_seg))
                                }
                                None => (None, None),
                            },
                        }
                    };

                let free_func = resolved_module_idx
                    .and_then(|midx| self.context.get_module_function(midx, &member_name));

                let mangled_type_name: Option<String> = assoc_type_name.and_then(|t| {
                    let midx = resolved_module_idx?;
                    let pkg = self.context.dep_graph.borrow().get_module_package(midx)?;
                    Some(format!("{}_{}", pkg.to_string(), t.to_string()))
                });

                let method_func = if free_func.is_none() {
                    mangled_type_name
                        .or_else(|| access.path.last().map(|s| s.to_string()))
                        .and_then(|tn| self.context.get_method(&tn, &member_name).copied())
                } else {
                    None
                };

                let func = match free_func.or(method_func) {
                    Some(f) => f,
                    None => {
                        let path_str = access
                            .path
                            .iter()
                            .map(|s| s.to_string())
                            .collect::<Vec<_>>()
                            .join("::");
                        let qualified_name = format!("{}.{}", path_str, member_name);
                        let candidate_modules = self
                            .context
                            .dep_graph
                            .borrow()
                            .find_function_by_name_anywhere(access.member);
                        if candidate_modules.is_empty() {
                            self.record(TypeErrorKind::UndefinedFunction(qualified_name));
                        } else {
                            let suggestion_paths: Vec<String> = candidate_modules
                                .iter()
                                .filter_map(|&midx| {
                                    self.context.dep_graph.borrow().get_module_package(midx)
                                })
                                .map(|pkg| pkg.to_string())
                                .collect();
                            self.record(TypeErrorKind::UndefinedFunctionWithSuggestion {
                                name: qualified_name,
                                suggested_modules: suggestion_paths,
                            });
                        }

                        return HirType::Unknown;
                    }
                };

                self.check_unsafe_call(&func, &member_name);

                self.check_module_path_imported(access.path);

                let param_types: Vec<HirType<'a, 'bump>> = func
                    .params
                    .unwrap_or(&[])
                    .iter()
                    .filter_map(|p| p.get_type().copied())
                    .collect();
                HirType::Lambda {
                    params: self.context.bump.alloc_slice(&param_types),
                    return_type: self
                        .context
                        .bump
                        .alloc_value(func.return_type.unwrap_or(HirType::Void)),
                }
            }
            HirExpr::Lambda {
                params,
                return_type,
                body,
                span,
                ..
            } => {
                self.set_span(*span);
                let mut lambda_context = self.context.create_child_scope();
                for p in *params {
                    let param_name = self.str_id_to_string(p.name);
                    let param_ty = p.param_type.unwrap_or(HirType::Unknown);
                    let symbol_id = self.mint_symbol_id();
                    lambda_context.add_variable(param_name, param_ty, symbol_id);
                }

                let old_context = std::mem::replace(&mut self.context, lambda_context);
                self.check_stmt(body);
                self.context = old_context;

                let param_types: Vec<HirType<'a, 'bump>> = params
                    .iter()
                    .map(|p| p.param_type.unwrap_or(HirType::Unknown))
                    .collect();

                HirType::Lambda {
                    params: self.context.bump.alloc_slice(&param_types),
                    return_type: self.context.bump.alloc_value(*return_type),
                }
            }
            HirExpr::Index {
                object,
                index,
                span,
            } => {
                self.set_span(*span);

                let object_ty = self.check_expr(object);
                let index_ty = self.check_expr(index);

                self.recover(self.types_compatible(&HirType::I64, &index_ty), ());

                match object_ty {
                    HirType::SafePointer { inner, .. } => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "indexing a raw pointer requires an unsafe block".to_string(),
                            ));
                        }
                        *inner
                    }

                    HirType::UnsafePointer { inner, .. } => {
                        if !self.in_unsafe() {
                            self.record(TypeErrorKind::Generic(
                                "indexing an unsafe pointer requires an unsafe block".to_string(),
                            ));
                        }
                        *inner
                    }

                    _ => match *Self::strip_ref(&object_ty) {
                        HirType::Array(inner, _) => *inner,
                        HirType::Slice(inner) => *inner,

                        _ => {
                            self.record(TypeErrorKind::Generic(format!(
                                "cannot index type `{}`",
                                self.type_to_string(&object_ty)
                            )));
                            HirType::Unknown
                        }
                    },
                }
            }
            HirExpr::ArrayLiteral { elements, span } => {
                self.set_span(*span);

                if elements.is_empty() {
                    self.record(TypeErrorKind::TypeCannotBeInferred);
                    return HirType::Unknown;
                }

                let first_ty = self.check_expr(&elements[0]);
                self.check_and_record_value_use(&elements[0], &first_ty);

                for elem in &elements[1..] {
                    let elem_ty = self.check_expr(elem);
                    self.check_and_record_value_use(elem, &elem_ty);
                    let result = self.types_compatible(&first_ty, &elem_ty);
                    self.recover(result, ());
                }

                HirType::Array(self.context.bump.alloc_value(first_ty), elements.len())
            }
            HirExpr::GenericIdent(..) => todo!(),
            HirExpr::Cast {
                expr,
                target_type,
                span,
            } => {
                self.set_span(*span);
                let source_type = self.check_expr(expr);
                self.check_and_record_value_use(expr, &source_type);
                let result = self.check_cast_legality(&source_type, target_type);
                self.recover(result, ());
                *target_type
            }
            HirExpr::Char(_, _) => HirType::Char,
            HirExpr::UnknownIntrinsic { span, name } => {
                self.recover(
                    Err(TypeErrorKind::Generic(format!("Unknown intrinsic {}", name)).at(*span)),
                    (),
                );
                HirType::Unknown
            }
        }
    }

    fn join_value_types(&mut self, branches: &[HirType<'a, 'bump>]) -> HirType<'a, 'bump> {
        let mut result: Option<HirType<'a, 'bump>> = None;
        for ty in branches {
            if matches!(ty, HirType::Never) {
                continue;
            }
            match result {
                None => result = Some(*ty),
                Some(expected) => {
                    let check = self.types_compatible(&expected, ty);
                    self.recover(check, ());
                }
            }
        }
        result.unwrap_or(HirType::Never) // every branch diverged
    }

    fn check_cast_legality(
        &self,
        source: &HirType<'a, 'bump>,
        target: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<'a, ()> {
        if self.types_structurally_equal(source, target) {
            return Ok(());
        }

        let is_ptr = |t: &HirType<'a, 'bump>| {
            matches!(
                t,
                HirType::SafePointer { .. }
                    | HirType::UnsafePointer { .. }
                    | HirType::OwnedPointer { .. }
            )
        };

        let ok = match (source, target) {
            (s, t) if self.is_numeric(s) && self.is_numeric(t) => true,

            (HirType::Boolean, t) if self.is_numeric(t) => true,

            (
                HirType::SafePointer { inner: src, .. },
                HirType::UnsafePointer { inner: dst, .. },
            ) => self.types_structurally_equal(src, dst),

            (HirType::Slice(src), HirType::SafePointer { inner: dst, .. }) => {
                self.types_structurally_equal(src, dst)
            }

            (HirType::Slice(src), HirType::UnsafePointer { inner: dst, .. }) => {
                self.types_structurally_equal(src, dst)
            }

            (HirType::Array(src, _), HirType::SafePointer { inner: dst, .. }) => {
                self.types_structurally_equal(src, dst)
            }

            (HirType::Array(src, _), HirType::UnsafePointer { inner: dst, .. }) => {
                self.types_structurally_equal(src, dst)
            }

            (
                HirType::OwnedPointer { inner: owned, .. },
                HirType::SafePointer { inner: dst, .. },
            ) => match owned {
                HirType::Slice(src) => self.types_structurally_equal(src, dst),
                _ => false,
            },

            (
                HirType::OwnedPointer { inner: owned, .. },
                HirType::UnsafePointer { inner: dst, .. },
            ) => match owned {
                HirType::Slice(src) => self.types_structurally_equal(src, dst),
                _ => false,
            },

            (s, t) if is_ptr(s) && self.is_integer(t) => true,
            (s, t) if self.is_integer(s) && is_ptr(t) => true,
            _ => false,
        };

        if ok {
            Ok(())
        } else {
            Err(TypeErrorKind::Generic(format!(
                "cannot cast `{}` as `{}`: no defined conversion between these types",
                self.type_to_string(source),
                self.type_to_string(target),
            ))
            .at(self.current_span))
        }
    }

    fn infer_provenance(&self, expr: &HirExpr<'a, 'bump>) -> Option<ProvenanceAnnotation<'bump>> {
        let mut segments = Vec::new();
        let root = self.infer_provenance_root(expr, &mut segments)?;
        segments.reverse();
        Some(ProvenanceAnnotation {
            root,
            path: self.context.bump.alloc_slice(&segments),
        })
    }

    fn infer_provenance_root(
        &self,
        expr: &HirExpr<'a, 'bump>,
        segments: &mut Vec<ProvenancePathSegment>,
    ) -> Option<ProvenanceRoot> {
        match expr {
            HirExpr::Ident(name, _) => Some(ProvenanceRoot::Var(*name)),
            HirExpr::This { .. } => Some(ProvenanceRoot::ThisRoot),

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                segments.push(ProvenancePathSegment::Field(*field));
                self.infer_provenance_root(object, segments)
            }

            HirExpr::Deref { expr: inner, .. } => {
                segments.push(ProvenancePathSegment::Deref);
                self.infer_provenance_root(inner, segments)
            }

            // Indexing loses static field-path precision (the index is runtime
            // data), but the borrowed region is still rooted in `object`, keep
            // the root so diagnostics can still say "derived from `x`" instead
            // of dropping to nothing
            HirExpr::Index { object, .. } => self.infer_provenance_root(object, segments),

            HirExpr::ModuleAccess(access) => {
                let module_idx = self
                    .context
                    .dep_graph
                    .borrow()
                    .resolve_module_path(access.path)?;
                self.context
                    .dep_graph
                    .borrow()
                    .resolve_global_const(module_idx, access.member)?;
                Some(ProvenanceRoot::Global {
                    module_idx,
                    name: access.member,
                })
            }

            _ => None,
        }
    }

    fn check_ref_expr(
        &mut self,
        expr: &HirExpr<'a, 'bump>,
        mutable: bool,
        span: SourceSpan<'a>,
        register_loan: bool,
    ) -> HirType<'a, 'bump> {
        self.set_span(span);
        let inner_ty = self.check_expr(expr);
        let provenance = self.infer_provenance(expr);

        if register_loan {
            if let Some(place) = self.resolve_place(expr) {
                let result = if mutable {
                    self.borrow_checker.borrow_mut(place)
                } else {
                    self.borrow_checker.borrow_shared(place)
                };
                if let Err(e) = result {
                    let msg = self.describe_borrow_error(&e, provenance.as_ref());
                    self.record(TypeErrorKind::Generic(msg));
                }
            }
        }

        HirType::Ref {
            inner: self.context.bump.alloc_value(inner_ty),
            mutability_state: if mutable {
                MutabilityState::Mut
            } else {
                MutabilityState::Const
            },
            provenance,
        }
    }

    fn check_pattern_against_type(
        &mut self,
        pattern: &HirPattern<'bump>,
        scrutinee_ty: &HirType<'a, 'bump>,
    ) {
        match pattern {
            HirPattern::EnumVariant {
                variant, bindings, ..
            } => {
                let HirType::Enum(enum_name, _) = scrutinee_ty else {
                    return;
                };
                let enum_name_str = self.str_id_to_string(*enum_name);
                let Some(def) = self.context.get_enum(&enum_name_str) else {
                    return;
                };
                let Some(variant_def) = def.variants.iter().find(|v| v.name == *variant) else {
                    self.record(TypeErrorKind::Generic(format!(
                        "enum `{}` has no variant `{}`",
                        enum_name_str, variant
                    )));
                    return;
                };
                if bindings.len() != variant_def.fields.len() {
                    self.record(TypeErrorKind::Generic(format!(
                        "variant `{}::{}` has {} field(s), but the pattern binds {}",
                        enum_name_str,
                        variant,
                        variant_def.fields.len(),
                        bindings.len()
                    )));
                }
            }
            HirPattern::Boolean(_) => {
                if !matches!(scrutinee_ty, HirType::Boolean) {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: self.type_to_string(scrutinee_ty),
                        found: "bool".to_string(),
                    });
                }
            }
            HirPattern::Number(_) => {
                if !self.is_integer(scrutinee_ty) {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: self.type_to_string(scrutinee_ty),
                        found: "integer".to_string(),
                    });
                }
            }
            HirPattern::String(_) => {
                if !matches!(scrutinee_ty, HirType::String) {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: self.type_to_string(scrutinee_ty),
                        found: "str".to_string(),
                    });
                }
            }

            HirPattern::Ident(_) | HirPattern::Wildcard => {}

            HirPattern::Tuple(patterns) => match scrutinee_ty {
                HirType::Tuple(elems) => {
                    if patterns.len() != elems.len() {
                        self.record(TypeErrorKind::Generic(format!(
                            "tuple pattern has {} element(s), but the scrutinee has {}",
                            patterns.len(),
                            elems.len()
                        )));
                    }
                    for (sub_pattern, elem_ty) in patterns.iter().zip(elems.iter()) {
                        self.check_pattern_against_type(sub_pattern, elem_ty);
                    }
                }
                _ => {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: self.type_to_string(scrutinee_ty),
                        found: format!("tuple pattern with {} element(s)", patterns.len()),
                    });
                }
            },

            HirPattern::Array(patterns) => match scrutinee_ty {
                HirType::Array(inner, len) => {
                    if patterns.len() != *len {
                        self.record(TypeErrorKind::Generic(format!(
                            "array pattern has {} element(s), but the array type `{}` has length {}",
                            patterns.len(),
                            self.type_to_string(scrutinee_ty),
                            len
                        )));
                    }
                    for sub_pattern in patterns.iter() {
                        self.check_pattern_against_type(sub_pattern, inner);
                    }
                }
                HirType::Slice(inner) => {
                    for sub_pattern in patterns.iter() {
                        self.check_pattern_against_type(sub_pattern, inner);
                    }
                }
                _ => {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: self.type_to_string(scrutinee_ty),
                        found: format!("array pattern with {} element(s)", patterns.len()),
                    });
                }
            },

            HirPattern::Struct { name, fields } => match scrutinee_ty {
                HirType::Enum(enum_name, _) => {
                    let enum_name_str = self.str_id_to_string(*enum_name);
                    let Some(def) = self.context.get_enum(&enum_name_str) else {
                        return;
                    };
                    let Some(variant_def) = def.variants.iter().find(|v| v.name == *name) else {
                        self.record(TypeErrorKind::Generic(format!(
                            "enum `{}` has no variant `{}`",
                            enum_name_str, name
                        )));
                        return;
                    };

                    let mut seen: std::collections::HashSet<StrId> =
                        std::collections::HashSet::new();
                    for (field_name, sub_pattern) in fields.iter() {
                        if !seen.insert(*field_name) {
                            self.record(TypeErrorKind::Generic(format!(
                                "field `{}` matched more than once in this pattern",
                                self.str_id_to_string(*field_name)
                            )));
                            continue;
                        }
                        let Some(field_def) =
                            variant_def.fields.iter().find(|f| f.name == *field_name)
                        else {
                            self.record(TypeErrorKind::Generic(format!(
                                "variant `{}::{}` has no field `{}`",
                                enum_name_str,
                                name,
                                self.str_id_to_string(*field_name)
                            )));
                            continue;
                        };
                        self.check_pattern_against_type(sub_pattern, &field_def.field_type);
                    }

                    let missing: Vec<&str> = variant_def
                        .fields
                        .iter()
                        .filter(|f| !fields.iter().any(|(fname, _)| fname == &f.name))
                        .map(|f| f.name.as_str())
                        .collect();
                    if !missing.is_empty() {
                        self.record(TypeErrorKind::Generic(format!(
                            "pattern doesn't bind field(s) {} of variant `{}::{}`",
                            missing.join(", "),
                            enum_name_str,
                            name
                        )));
                    }
                }

                // A real struct scrutinee matched by field name.
                HirType::Struct {
                    name: struct_name,
                    field_types,
                    ..
                } => {
                    let struct_name_str = self.str_id_to_string(*struct_name);
                    let Some(def) = self.context.get_struct(&struct_name_str) else {
                        return;
                    };

                    let mut seen: std::collections::HashSet<StrId> =
                        std::collections::HashSet::new();
                    for (field_name, sub_pattern) in fields.iter() {
                        if !seen.insert(*field_name) {
                            self.record(TypeErrorKind::Generic(format!(
                                "field `{}` matched more than once in this pattern",
                                self.str_id_to_string(*field_name)
                            )));
                            continue;
                        }
                        let Some(field_idx) = def.fields.iter().position(|f| f.name == *field_name)
                        else {
                            self.record(TypeErrorKind::FieldNotFound {
                                struct_name: struct_name_str.clone(),
                                field: self.str_id_to_string(*field_name),
                            });
                            continue;
                        };
                        let field_ty = field_types
                            .get(field_idx)
                            .copied()
                            .unwrap_or(def.fields[field_idx].field_type);
                        self.check_pattern_against_type(sub_pattern, &field_ty);
                    }
                }

                _ => {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: self.type_to_string(scrutinee_ty),
                        found: format!("named-field pattern `{}`", name),
                    });
                }
            },

            HirPattern::Or(patterns) => {
                if patterns.is_empty() {
                    self.record(TypeErrorKind::Generic(
                        "or-pattern must have at least one alternative".to_string(),
                    ));
                    return;
                }

                for sub_pattern in patterns.iter() {
                    self.check_pattern_against_type(sub_pattern, scrutinee_ty);
                }

                let mut first_bindings: Option<Vec<(StrId, HirType<'a, 'bump>)>> = None;
                for sub_pattern in patterns.iter() {
                    let mut bindings = Vec::new();
                    self.collect_pattern_bindings(sub_pattern, scrutinee_ty, &mut bindings);
                    bindings.sort_by(|(na, _), (nb, _)| {
                        self.str_id_to_string(*na).cmp(&self.str_id_to_string(*nb))
                    });

                    match &first_bindings {
                        None => first_bindings = Some(bindings),
                        Some(expected) => {
                            if !self.bindings_match(expected, &bindings) {
                                let names = |v: &[(StrId, HirType<'a, 'bump>)]| {
                                    v.iter()
                                        .map(|(n, _)| self.str_id_to_string(*n))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                };
                                self.record(TypeErrorKind::Generic(format!(
                                    "all alternatives of an or-pattern must bind the same names with the same types: \
                                     found `{}` in one alternative but `{}` in another",
                                    names(expected),
                                    names(&bindings),
                                )));
                            }
                        }
                    }
                }
            }
        }
    }

    fn resolve_callable_method(
        &self,
        ty: &HirType<'a, 'bump>,
        method_name: &str,
    ) -> Option<(StrId, String, HirFunc<'a, 'bump>)> {
        let try_name = |n: String| -> Option<(StrId, String, HirFunc<'a, 'bump>)> {
            let id = StrId(self.context.string_pool.intern(&n));
            self.context
                .get_method(&n, method_name)
                .map(|f| (id, n, *f))
        };

        match ty {
            HirType::Struct { name, .. } => {
                let struct_name_str = name.to_string();
                if let Some(hit) = try_name(struct_name_str.clone()) {
                    return Some(hit);
                }
                self.resolve_default_interface_method(*name, &struct_name_str, method_name)
            }
            HirType::Slice(elem) | HirType::Array(elem, _) => {
                if let Some(elem_name) = self.builtin_element_name(elem) {
                    if let Some(hit) = try_name(format!("slice_{}", elem_name)) {
                        return Some(hit);
                    }
                }
                try_name("slice".to_string())
            }
            other => try_name(self.builtin_element_name(other)?),
        }
    }

    fn resolve_default_interface_method(
        &self,
        struct_name: StrId,
        struct_name_str: &str,
        method_name: &str,
    ) -> Option<(StrId, String, HirFunc<'a, 'bump>)> {
        let interfaces = self.context.struct_interfaces.get(struct_name_str)?;
        for iface_name in interfaces {
            let Some(iface) = self.context.get_interface(iface_name) else {
                continue;
            };
            let Some(methods) = iface.methods else {
                continue;
            };
            if let Some(m) = methods
                .iter()
                .find(|m| m.unmangled_name.as_str() == method_name && m.body.is_some())
            {
                return Some((struct_name, struct_name_str.to_string(), *m));
            }
        }
        None
    }

    fn builtin_element_name(&self, ty: &HirType<'a, 'bump>) -> Option<String> {
        Some(match ty {
            HirType::I8 => "i8".into(),
            HirType::I16 => "i16".into(),
            HirType::I32 => "i32".into(),
            HirType::I64 => "i64".into(),
            HirType::I128 => "i128".into(),
            HirType::U8 => "u8".into(),
            HirType::U16 => "u16".into(),
            HirType::U32 => "u32".into(),
            HirType::U64 => "u64".into(),
            HirType::U128 => "u128".into(),
            HirType::Usize => "usize".into(),
            HirType::Isize => "isize".into(),
            HirType::F32 => "f32".into(),
            HirType::F64 => "f64".into(),
            HirType::Boolean => "bool".into(),
            HirType::String => "str".into(),
            HirType::Char => "char".into(),
            HirType::Struct { name, .. } => name.to_string(),
            _ => return None,
        })
    }

    fn register_pattern_bindings(
        &mut self,
        pattern: &HirPattern<'bump>,
        scrutinee_ty: &HirType<'a, 'bump>,
    ) {
        match pattern {
            HirPattern::Ident(name) => {
                let var_name = self.str_id_to_string(*name);
                let symbol_id = self.mint_symbol_id();
                self.context
                    .add_variable(var_name, *scrutinee_ty, symbol_id);
            }

            HirPattern::EnumVariant {
                variant, bindings, ..
            } => {
                let HirType::Enum(enum_name, _) = scrutinee_ty else {
                    return;
                };
                let enum_name_str = self.str_id_to_string(*enum_name);
                let Some(def) = self.context.get_enum(&enum_name_str) else {
                    return;
                };
                let Some(variant_def) = def.variants.iter().find(|v| v.name == *variant) else {
                    return;
                };
                for (binding_name, field) in bindings.iter().zip(variant_def.fields.iter()) {
                    let var_name = self.str_id_to_string(*binding_name);
                    let symbol_id = self.mint_symbol_id();
                    self.context
                        .add_variable(var_name, field.field_type, symbol_id);
                }
            }

            HirPattern::Tuple(patterns) => {
                if let HirType::Tuple(elems) = scrutinee_ty {
                    for (sub_pattern, elem_ty) in patterns.iter().zip(elems.iter()) {
                        self.register_pattern_bindings(sub_pattern, elem_ty);
                    }
                }
            }

            HirPattern::Array(patterns) => {
                let elem_ty = match scrutinee_ty {
                    HirType::Array(inner, _) | HirType::Slice(inner) => Some(**inner),
                    _ => None,
                };
                if let Some(elem_ty) = elem_ty {
                    for sub_pattern in patterns.iter() {
                        self.register_pattern_bindings(sub_pattern, &elem_ty);
                    }
                }
            }

            HirPattern::Struct { name, fields } => match scrutinee_ty {
                HirType::Enum(enum_name, _) => {
                    let enum_name_str = self.str_id_to_string(*enum_name);
                    let Some(def) = self.context.get_enum(&enum_name_str) else {
                        return;
                    };
                    let Some(variant_def) = def.variants.iter().find(|v| v.name == *name) else {
                        return;
                    };
                    for (field_name, sub_pattern) in fields.iter() {
                        let Some(field_def) =
                            variant_def.fields.iter().find(|f| f.name == *field_name)
                        else {
                            continue;
                        };
                        self.register_pattern_bindings(sub_pattern, &field_def.field_type);
                    }
                }
                HirType::Struct {
                    name: struct_name,
                    field_types,
                    ..
                } => {
                    let struct_name_str = self.str_id_to_string(*struct_name);
                    let Some(def) = self.context.get_struct(&struct_name_str) else {
                        return;
                    };
                    for (field_name, sub_pattern) in fields.iter() {
                        let Some(field_idx) = def.fields.iter().position(|f| f.name == *field_name)
                        else {
                            continue;
                        };
                        let field_ty = field_types
                            .get(field_idx)
                            .copied()
                            .unwrap_or(def.fields[field_idx].field_type);
                        self.register_pattern_bindings(sub_pattern, &field_ty);
                    }
                }
                _ => {}
            },

            HirPattern::Or(patterns) => {
                if let Some(first) = patterns.first() {
                    self.register_pattern_bindings(first, scrutinee_ty);
                }
            }

            _ => {} // Wildcard, Number, String, Boolean bind nothing
        }
    }

    fn collect_pattern_bindings(
        &self,
        pattern: &HirPattern<'bump>,
        scrutinee_ty: &HirType<'a, 'bump>,
        out: &mut Vec<(StrId, HirType<'a, 'bump>)>,
    ) {
        match pattern {
            HirPattern::Ident(name) => {
                out.push((*name, *scrutinee_ty));
            }

            HirPattern::EnumVariant {
                variant, bindings, ..
            } => {
                let HirType::Enum(enum_name, _) = scrutinee_ty else {
                    return;
                };
                let enum_name_str = self.str_id_to_string(*enum_name);
                let Some(def) = self.context.get_enum(&enum_name_str) else {
                    return;
                };
                let Some(variant_def) = def.variants.iter().find(|v| v.name == *variant) else {
                    return;
                };
                for (binding_name, field) in bindings.iter().zip(variant_def.fields.iter()) {
                    out.push((*binding_name, field.field_type));
                }
            }

            HirPattern::Tuple(patterns) => {
                if let HirType::Tuple(elems) = scrutinee_ty {
                    for (sub_pattern, elem_ty) in patterns.iter().zip(elems.iter()) {
                        self.collect_pattern_bindings(sub_pattern, elem_ty, out);
                    }
                }
            }

            HirPattern::Array(patterns) => {
                let elem_ty = match scrutinee_ty {
                    HirType::Array(inner, _) | HirType::Slice(inner) => Some(**inner),
                    _ => None,
                };
                if let Some(elem_ty) = elem_ty {
                    for sub_pattern in patterns.iter() {
                        self.collect_pattern_bindings(sub_pattern, &elem_ty, out);
                    }
                }
            }

            HirPattern::Struct { name, fields } => match scrutinee_ty {
                HirType::Enum(enum_name, _) => {
                    let enum_name_str = self.str_id_to_string(*enum_name);
                    let Some(def) = self.context.get_enum(&enum_name_str) else {
                        return;
                    };
                    let Some(variant_def) = def.variants.iter().find(|v| v.name == *name) else {
                        return;
                    };
                    for (field_name, sub_pattern) in fields.iter() {
                        let Some(field_def) =
                            variant_def.fields.iter().find(|f| f.name == *field_name)
                        else {
                            continue;
                        };
                        self.collect_pattern_bindings(sub_pattern, &field_def.field_type, out);
                    }
                }
                HirType::Struct {
                    name: struct_name,
                    field_types,
                    ..
                } => {
                    let struct_name_str = self.str_id_to_string(*struct_name);
                    let Some(def) = self.context.get_struct(&struct_name_str) else {
                        return;
                    };
                    for (field_name, sub_pattern) in fields.iter() {
                        let Some(field_idx) = def.fields.iter().position(|f| f.name == *field_name)
                        else {
                            continue;
                        };
                        let field_ty = field_types
                            .get(field_idx)
                            .copied()
                            .unwrap_or(def.fields[field_idx].field_type);
                        self.collect_pattern_bindings(sub_pattern, &field_ty, out);
                    }
                }
                _ => {}
            },

            HirPattern::Or(patterns) => {
                // Nested or-pattern: consistency across its own alternatives is
                // enforced separately wherever *this* pattern is itself checked
                // via check_pattern_against_type. Any one alternative gives the
                // right binding set here.
                if let Some(first) = patterns.first() {
                    self.collect_pattern_bindings(first, scrutinee_ty, out);
                }
            }

            HirPattern::Wildcard
            | HirPattern::Number(_)
            | HirPattern::String(_)
            | HirPattern::Boolean(_) => {}
        }
    }

    /// Order-independent comparison of two binding sets: same names, same
    /// types, regardless of the order each pattern happened to declare them in.
    fn bindings_match(
        &self,
        a: &[(StrId, HirType<'a, 'bump>)],
        b: &[(StrId, HirType<'a, 'bump>)],
    ) -> bool {
        if a.len() != b.len() {
            return false;
        }
        a.iter()
            .zip(b.iter())
            .all(|((na, ta), (nb, tb))| na == nb && self.types_structurally_equal(ta, tb))
    }

    fn check_potential_this_param_for_move(
        &mut self,
        args: &[HirExpr<'a, 'bump>],
        func: HirFunc<'a, 'bump>,
        params: &[HirParam<'a, 'bump>],
        ret_ty: HirType<'a, 'bump>,
    ) -> Option<HirType<'a, 'bump>> {
        if let Some(this_param) = params.first() {
            if matches!(this_param, HirParam::This { .. }) {
                self.record(TypeErrorKind::IllegalThisParam {
                    func_name: func.unmangled_name.to_string(),
                });
                return Some(ret_ty);
            }

            let template = if self.return_type_may_alias(&ret_ty) {
                Some(self.analyze_ref_template(&func))
            } else {
                None
            };

            let templated_base_param = match &template {
                Some(RefTemplate::Path {
                    base: TemplateBase::Param(i),
                    ..
                }) => Some(*i),
                _ => None,
            };

            let read_templates = self.analyze_read_templates(&func);
            for (arg_idx, arg) in args.iter().enumerate() {
                if let HirExpr::Ref {
                    expr: inner,
                    mutable: false,
                    ..
                } = arg
                {
                    if let Some(template) = read_templates.get(arg_idx) {
                        self.check_call_arg_read_effects(inner, template, args);
                    }
                }
            }

            let arg_loans = self.check_all_func_args(args, params, templated_base_param);
            self.finalize_call_loans(None, args, arg_loans, &ret_ty, template);

            return Some(ret_ty);
        }
        Some(ret_ty)
    }

    fn instantiate_enum(
        &self,
        name: StrId,
        args: &[HirType<'a, 'bump>],
    ) -> Option<&'bump [(StrId, &'bump [HirType<'a, 'bump>])]> {
        if let Some(cached) = self.context.get_enum_instantiation(name, args) {
            return Some(cached);
        }

        let name_str = self.str_id_to_string(name);
        let def = self.context.get_enum(&name_str)?;
        let generics = def.generics?;
        if generics.len() != args.len() {
            return None;
        }

        let mut subs = FxHashMap::default();
        for (param, arg) in generics.iter().zip(args.iter()) {
            subs.insert(param.name, *arg);
        }

        let mut resolved_variants = Vec::with_capacity(def.variants.len());
        for variant in def.variants.iter() {
            let field_types: Vec<_> = variant
                .fields
                .iter()
                .map(|f| self.substitute_type_local(&f.field_type, &subs))
                .collect();
            resolved_variants.push((
                variant.name,
                self.context.bump.alloc_slice_copy(&field_types),
            ));
        }
        let result = self.context.bump.alloc_slice_copy(&resolved_variants);

        self.context
            .cache_enum_instantiation(name, args.to_vec(), result);
        Some(result)
    }

    fn check_all_func_args(
        &mut self,
        args: &[HirExpr<'a, 'bump>],
        params: &[HirParam<'a, 'bump>],
        templated_base_param: Option<usize>,
    ) -> Vec<LoanId> {
        let mut arg_loans: Vec<LoanId> = Vec::new();

        for (i, (arg, param)) in args.iter().zip(params.iter()).enumerate() {
            let param_type = param.get_type();
            if Some(i) == templated_base_param {
                if let HirExpr::Ref {
                    expr,
                    mutable,
                    span,
                } = arg
                {
                    let arg_type = self.check_ref_expr(expr, *mutable, *span, false);
                    if let Some(pt) = param_type {
                        self.recover(self.types_compatible(pt, &arg_type), ());
                    }
                } else {
                    let arg_type = match param_type {
                        Some(pt) => self.check_expr_expected(arg, pt),
                        None => self.check_expr(arg),
                    };
                    self.check_and_record_value_use(arg, &arg_type);
                    if let Some(pt) = param_type {
                        self.recover(self.types_compatible(pt, &arg_type), ());
                    }
                }
                continue;
            }

            let arg_type = match param_type {
                Some(pt) => self.check_expr_expected(arg, pt),
                None => self.check_expr(arg),
            };
            self.check_and_record_value_use(arg, &arg_type);
            if let Some(pt) = param_type {
                self.recover(self.types_compatible(pt, &arg_type), ());
            }

            if let HirExpr::Ref { expr, .. } = arg {
                if let Some(place) = self.resolve_place(expr) {
                    if let Some(&loan_id) = self.borrow_checker.loan_for_place(place) {
                        arg_loans.push(loan_id);
                    }
                }
            }
        }

        arg_loans
    }

    /// True if a value of this type could itself hold or be a borrowed
    /// reference, i.e. calling a function returning this type might hand
    /// back something that aliases one of its ref-typed arguments.
    /// struct/enum/tuple types that might *contain* a
    /// reference field also count, since e.g. `struct Pair { r: &mut i64 }`
    /// returned by value still carries the alias forward.
    fn return_type_may_alias(&self, ty: &HirType<'a, 'bump>) -> bool {
        match ty {
            // Direct reference-like types.
            HirType::Ref { .. }
            | HirType::SafePointer { .. }
            | HirType::UnsafePointer { .. }
            | HirType::OwnedPointer { .. } => true,

            HirType::Nullable(inner) => self.return_type_may_alias(inner),

            HirType::Array(inner, _) => self.return_type_may_alias(inner),

            HirType::Tuple(elems) => elems.iter().any(|e| self.return_type_may_alias(e)),

            HirType::Struct { name, .. } => {
                let name_str = self.str_id_to_string(*name);
                match self.context.get_struct(&name_str) {
                    Some(def) => def
                        .fields
                        .iter()
                        .any(|f| self.return_type_may_alias(&f.field_type)),
                    None => true, // unresolved
                }
            }

            HirType::Enum(name, _) => {
                let name_str = self.str_id_to_string(*name);
                match self.context.get_enum(&name_str) {
                    Some(def) => def
                        .variants
                        .iter()
                        .flat_map(|v| v.fields.iter())
                        .any(|f| self.return_type_may_alias(&f.field_type)),
                    None => true, // unresolved
                }
            }

            HirType::Dyn { .. } | HirType::DynInterface(..) => true,

            // Primitive/value-only types.
            _ => false,
        }
    }

    fn finalize_call_loans(
        &mut self,
        receiver: Option<&HirExpr<'a, 'bump>>,
        args: &[HirExpr<'a, 'bump>],
        arg_loans: Vec<LoanId>,
        ret_ty: &HirType<'a, 'bump>,
        template: Option<RefTemplate>,
    ) -> Option<LoanId> {
        let Some(template) = template else {
            for loan in arg_loans {
                self.borrow_checker.end_loan_now(loan);
            }
            return None;
        };

        if matches!(template, RefTemplate::Path { .. }) {
            for &loan in &arg_loans {
                self.borrow_checker.end_loan_now(loan);
            }
        }

        let place = self.resolve_template_place(&template, receiver, args)?;

        let mutable = matches!(
            ret_ty,
            HirType::Ref {
                mutability_state: MutabilityState::Mut,
                ..
            }
        );
        let result = if mutable {
            self.borrow_checker.borrow_mut(place)
        } else {
            self.borrow_checker.borrow_shared(place)
        };

        match result {
            Ok(loan_id) => Some(loan_id),
            Err(e) => {
                let provenance = self.provenance_from_template(&template, receiver, args);
                let msg = self.describe_borrow_error(&e, provenance.as_ref());
                self.record(TypeErrorKind::Generic(msg));
                None
            }
        }
    }

    fn analyze_ref_template(&mut self, func: &HirFunc<'a, 'bump>) -> RefTemplate {
        if let Some(t) = self.ref_templates.get(&func.name) {
            return t.clone();
        }

        self.ref_templates.insert(func.name, RefTemplate::Opaque);

        let template = Self::build_ref_template(func);
        self.ref_templates.insert(func.name, template.clone());
        template
    }

    fn check_return_provenance(&mut self, func: &HirFunc<'a, 'bump>) {
        let Some(HirType::Ref {
            provenance: Some(ann),
            ..
        }) = func.return_type
        else {
            return;
        };

        let template = Self::build_ref_template(func);
        let RefTemplate::Path { base, .. } = template else {
            self.record(TypeErrorKind::Generic(format!(
                "return type declares provenance `{}` but the body's returned reference isn't a simple projection",
                self.provenance_to_string(&ann)
            )));
            return;
        };

        let root_matches = match (ann.root, base) {
            (ProvenanceRoot::Var(name), TemplateBase::Param(idx)) => func
                .params
                .map(|p| {
                    p.iter()
                        .filter(|pp| matches!(pp, HirParam::Normal { .. }))
                        .collect::<Vec<_>>()
                })
                .and_then(|normals| normals.get(idx).copied())
                .is_some_and(
                    |p| matches!(p, HirParam::Normal { name: pname, .. } if name == *pname),
                ),
            (ProvenanceRoot::ThisRoot, TemplateBase::This) => true,
            _ => false,
        };

        if !root_matches {
            self.record(TypeErrorKind::Generic(format!(
                "declared provenance `{}` doesn't match the parameter the returned reference is actually rooted in",
                self.provenance_to_string(&ann)
            )));
        }
    }

    fn build_ref_template(func: &HirFunc<'a, 'bump>) -> RefTemplate {
        let Some(params) = func.params else {
            return RefTemplate::Opaque;
        };

        let mut param_index: FxHashMap<StrId, usize> = FxHashMap::default();
        let mut has_this = false;
        let mut normal_idx = 0usize;
        for p in params.iter() {
            match p {
                HirParam::Normal { name, .. } => {
                    param_index.insert(*name, normal_idx);
                    normal_idx += 1;
                }
                HirParam::This { .. } => has_this = true,
            }
        }

        let Some(HirStmt::Block { body }) = func.body else {
            return RefTemplate::Opaque;
        };

        let [HirStmt::Return(Some(ret_expr))] = body else {
            return RefTemplate::Opaque;
        };
        let (expr, mutable) = match ret_expr {
            HirExpr::Ref { expr, mutable, .. } => (*expr, *mutable),
            HirExpr::Intrinsic {
                kind: IntrinsicKind::Own,
                args,
                ..
            } if args.len() == 2 => (&args[0], true),
            _ => return RefTemplate::Opaque,
        };

        let Some((base, projections)) = Self::expr_to_template(expr, &param_index, has_this) else {
            return RefTemplate::Opaque;
        };

        RefTemplate::Path {
            base,
            mutable,
            projections,
        }
    }

    fn expr_to_template(
        expr: &HirExpr<'a, 'bump>,
        param_index: &FxHashMap<StrId, usize>,
        has_this: bool,
    ) -> Option<(TemplateBase, Vec<TemplateProjection>)> {
        match expr {
            HirExpr::Ident(name, _) => {
                Some((TemplateBase::Param(*param_index.get(name)?), Vec::new()))
            }

            HirExpr::This { .. } if has_this => Some((TemplateBase::This, Vec::new())),

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                let (base, mut proj) = Self::expr_to_template(object, param_index, has_this)?;
                proj.push(TemplateProjection::Field(*field));
                Some((base, proj))
            }

            HirExpr::Deref { expr, .. } => {
                let (base, mut proj) = Self::expr_to_template(expr, param_index, has_this)?;
                proj.push(TemplateProjection::Deref);
                Some((base, proj))
            }

            HirExpr::Index { object, index, .. } => {
                let (base, mut proj) = Self::expr_to_template(object, param_index, has_this)?;
                let idx = match &**index {
                    HirExpr::Number(n, _) => IndexTemplate::Const(*n),
                    HirExpr::Ident(name, _) => param_index
                        .get(name)
                        .map(|&i| IndexTemplate::Param(i))
                        .unwrap_or(IndexTemplate::Opaque),
                    _ => IndexTemplate::Opaque,
                };
                proj.push(TemplateProjection::Index(idx));
                Some((base, proj))
            }

            _ => None,
        }
    }

    fn provenance_from_template(
        &self,
        template: &RefTemplate,
        receiver: Option<&HirExpr<'a, 'bump>>,
        args: &[HirExpr<'a, 'bump>],
    ) -> Option<ProvenanceAnnotation<'bump>> {
        let RefTemplate::Path {
            base, projections, ..
        } = template
        else {
            return None;
        };

        let base_expr = match base {
            TemplateBase::This => receiver?,
            TemplateBase::Param(i) => {
                let arg = args.get(*i)?;
                match arg {
                    HirExpr::Ref { expr, .. } => expr,
                    other => other,
                }
            }
        };

        let mut base_provenance = self.infer_provenance(base_expr)?;

        let mut path: Vec<ProvenancePathSegment> = base_provenance.path.to_vec();
        for proj in projections {
            match proj {
                TemplateProjection::Field(f) => path.push(ProvenancePathSegment::Field(*f)),
                TemplateProjection::Deref => path.push(ProvenancePathSegment::Deref),
                TemplateProjection::Index(_) => {}
            }
        }
        base_provenance.path = self.context.bump.alloc_slice(&path);
        Some(base_provenance)
    }

    fn resolve_template_place(
        &mut self,
        template: &RefTemplate,
        receiver: Option<&HirExpr<'a, 'bump>>,
        args: &[HirExpr<'a, 'bump>],
    ) -> Option<PlaceId> {
        let RefTemplate::Path {
            base, projections, ..
        } = template
        else {
            return None;
        };

        let base_expr = match base {
            TemplateBase::This => receiver?,
            TemplateBase::Param(i) => {
                let arg = args.get(*i)?;
                match arg {
                    HirExpr::Ref { expr, .. } => expr,
                    other => other,
                }
            }
        };

        let mut place = self.resolve_place(base_expr)?;

        for proj in projections {
            place = match proj {
                TemplateProjection::Field(f) => self.borrow_checker.project_field(place, *f),
                TemplateProjection::Deref => self.borrow_checker.project_deref(place),
                TemplateProjection::Index(idx_template) => {
                    let bound = match idx_template {
                        IndexTemplate::Const(c) => Bound::Const(*c),
                        IndexTemplate::Param(i) => self.expr_to_bound(args.get(*i)?),
                        IndexTemplate::Opaque => Bound::Opaque(0),
                    };
                    let interval = Interval {
                        lower: bound.clone(),
                        upper: bound,
                    };
                    self.borrow_checker
                        .project_index(place, interval, IndexContainer::Primitive)
                }
            };
        }

        Some(place)
    }

    fn check_receiver_is_mutable(
        &self,
        receiver: &HirExpr<'a, 'bump>,
        method_name: &str,
    ) -> TypeCheckResult<'a, ()> {
        let Some(root_name) = self.find_root_local_ident(receiver) else {
            return Ok(());
        };

        if !self.context.is_mutable(&root_name) {
            return Err(TypeErrorKind::Generic(format!(
                "cannot call `{}` on `{}`: `{}` is not declared `mut`",
                method_name, root_name, root_name
            ))
            .at(self.current_span));
        }

        Ok(())
    }

    fn expr_is_dangling(&self, expr: &HirExpr<'a, 'bump>) -> bool {
        match expr {
            HirExpr::Ref { expr: inner, .. } => match self.find_root_local_ident(inner) {
                Some(root_name) => match self.context.get_variable(&root_name) {
                    Some((_, root_type)) => !root_type.is_pointer_semantics(),
                    None => false,
                },
                None => false,
            },
            HirExpr::Ident(name, _) => {
                let var_name = self.str_id_to_string(*name);
                self.context.is_dangling(&var_name)
            }
            _ => false,
        }
    }

    fn check_no_dangling_pointer(&self, expr: &HirExpr<'a, 'bump>) -> TypeCheckResult<'a, ()> {
        if let HirExpr::Ref { expr: inner, .. } = expr {
            if let Some(root_name) = self.find_root_local_ident(inner) {
                if let Some((_, root_type)) = self.context.get_variable(&root_name) {
                    if !root_type.is_pointer_semantics() {
                        return Err(TypeErrorKind::Generic(format!(
                            "cannot return a pointer to local variable `{}`: its storage does not outlive this function",
                            root_name
                        )).at(self.current_span));
                    }
                }
            }
            return Ok(());
        }

        if let HirExpr::Ident(name, _) = expr {
            let var_name = self.str_id_to_string(*name);
            if self.context.is_dangling(&var_name) {
                return Err(TypeErrorKind::Generic(format!(
                    "cannot return `{}`: it holds a pointer to local stack memory that does not outlive this function",
                    var_name
                )).at(self.current_span));
            }
        }

        Ok(())
    }

    fn find_root_local_ident(&self, expr: &HirExpr<'a, 'bump>) -> Option<String> {
        match expr {
            HirExpr::Ident(name, _) => Some(self.str_id_to_string(*name)),
            HirExpr::FieldAccess { object, .. } | HirExpr::Get { object, .. } => {
                self.find_root_local_ident(object)
            }
            HirExpr::Deref { expr: inner, .. } => self.find_root_local_ident(inner),
            _ => None,
        }
    }

    fn check_binary_op(
        &self,
        left: &HirType<'a, 'bump>,
        op: &Operator,
        right: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<'a, HirType<'a, 'bump>> {
        use Operator::*;

        match op {
            Add | Subtract | Multiply | Divide | Modulo => {
                if self.is_numeric(left) && self.is_numeric(right) {
                    Ok(*left)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: self.operator_symbol(op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            Equals | NotEquals => {
                if self.is_comparable(left) && self.is_comparable(right) {
                    Ok(HirType::Boolean)
                } else if self.is_reference_like(left)
                    && self.is_reference_like(right)
                    && self.types_structurally_equal(left, right)
                {
                    // Raw pointer address comparison
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: self.operator_symbol(op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => {
                if self.is_comparable(left) && self.is_comparable(right) {
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: self.operator_symbol(op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            LogicalAnd | LogicalOr => {
                if *left == HirType::Boolean && *right == HirType::Boolean {
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: self.operator_symbol(op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            BitAnd | BitOr | BitXor | ShiftLeft | ShiftRight => {
                if self.is_integer(left) && self.is_integer(right) {
                    Ok(*left)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: self.operator_symbol(op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            _ => Err(TypeErrorKind::Generic(format!(
                "operator `{}` cannot appear in this position",
                self.operator_symbol(op)
            ))
            .at(self.current_span)),
        }
    }

    fn operator_symbol(&self, op: &Operator) -> String {
        use Operator::*;
        match op {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            Equals => "==",
            NotEquals => "!=",
            LessThan => "<",
            LessThanOrEqual => "<=",
            GreaterThan => ">",
            GreaterThanOrEqual => ">=",
            LogicalAnd => "&&",
            LogicalOr => "||",
            BitAnd => "&",
            BitOr => "|",
            BitXor => "^",
            ShiftLeft => "<<",
            ShiftRight => ">>",
            _ => return format!("{:?}", op), // TODO finish
        }
        .to_string()
    }

    fn is_reference_like(&self, ty: &HirType<'a, 'bump>) -> bool {
        matches!(
            ty,
            HirType::Ref { .. }
                | HirType::SafePointer { .. }
                | HirType::UnsafePointer { .. }
                | HirType::OwnedPointer { .. }
        )
    }

    fn instantiate_struct(
        &self,
        name: StrId,
        args: &[HirType<'a, 'bump>],
    ) -> Option<&'bump [HirType<'a, 'bump>]> {
        if let Some(cached) = self.context.get_struct_instantiation(name, args) {
            return Some(cached);
        }

        let name_str = self.str_id_to_string(name);
        let def = self.context.get_struct(&name_str)?;
        let generics = def.generics?;
        if generics.len() != args.len() {
            return None;
        }

        let mut subs = FxHashMap::default();
        for (param, arg) in generics.iter().zip(args.iter()) {
            subs.insert(param.name, *arg);
        }

        let field_types: Vec<_> = def
            .fields
            .iter()
            .map(|f| self.substitute_type_local(&f.field_type, &subs))
            .collect();
        let result = self.context.bump.alloc_slice_copy(&field_types);

        self.context
            .cache_struct_instantiation(name, args.to_vec(), result);
        Some(result)
    }

    fn check_field_access(
        &mut self,
        object: &HirExpr<'a, 'bump>,
        field: StrId,
    ) -> HirType<'a, 'bump> {
        let obj_type = self.check_expr(object);
        let stripped = *Self::strip_ref(&obj_type);

        let HirType::Struct {
            name: struct_name,
            type_args,
            ..
        } = stripped
        else {
            self.record(TypeErrorKind::Generic(format!(
                "Cannot access field on non-struct type: {}",
                self.type_to_string(&obj_type)
            )));
            return HirType::Unknown;
        };

        let struct_name_str = self.str_id_to_string(struct_name);
        let Some(struct_def) = self.context.get_struct(&struct_name_str) else {
            self.record(TypeErrorKind::UndefinedType(struct_name_str));
            return HirType::Unknown;
        };

        self.check_bare_name_import(
            self.context.struct_owner(&struct_name_str),
            struct_name,
            &struct_name_str,
            BareImportKind::Struct,
        );

        let field_name = self.str_id_to_string(field);
        let field_idx = struct_def
            .fields
            .iter()
            .position(|f| self.str_id_to_string(f.name) == field_name);

        let Some(field_idx) = field_idx else {
            self.record(TypeErrorKind::FieldNotFound {
                struct_name: struct_name_str,
                field: field_name,
            });
            return HirType::Unknown;
        };

        let ty = if type_args.is_empty() {
            struct_def.fields[field_idx].field_type
        } else {
            match self.instantiate_struct(struct_name, type_args) {
                Some(fields) => fields[field_idx],
                None => struct_def.fields[field_idx].field_type,
            }
        };

        self.occurrences.push((
            self.current_span,
            field,
            ty,
            self.context.current_module_idx,
            SymbolId::Field {
                struct_name,
                field_name: field,
            },
            false,
        ));

        ty
    }

    fn record_move(
        &mut self,
        root: StrId,
        field: Option<StrId>,
        field_ty: &HirType<'a, 'bump>,
        container_ty: Option<&HirType<'a, 'bump>>,
    ) {
        if self.copy_analysis.borrow().type_is_copy(field_ty) {
            return;
        }

        if let Some(&base_place) = self.borrow_checker.local_place(root) {
            let moved_place = match field {
                None => base_place,
                Some(f) => self.borrow_checker.project_field(base_place, f),
            };
            if let Err(e) = self.borrow_checker.check_move(moved_place) {
                let path = match field {
                    None => &[][..],
                    Some(f) => &[ProvenancePathSegment::Field(f)][..],
                };
                let provenance = ProvenanceAnnotation {
                    root: ProvenanceRoot::Var(root),
                    path: self.context.bump.alloc_slice(path),
                };
                let msg = self.describe_borrow_error(&e, Some(&provenance));
                self.record(TypeErrorKind::Generic(msg));
            }
        }

        match field {
            None => self.move_state.mark_whole_moved(root),
            Some(f) => {
                let blocks_partial_move = container_ty.is_some_and(|cty| match cty {
                    HirType::Struct { name, .. } | HirType::Enum(name, _) => {
                        self.copy_analysis.borrow().implements_drop(*name)
                    }
                    _ => false,
                });

                if blocks_partial_move {
                    self.record(TypeErrorKind::Generic(format!(
                        "cannot partially move out of `{}`, which implements `Drop`",
                        container_ty
                            .map(|t| self.type_to_string(t))
                            .unwrap_or_default()
                    )));
                    return;
                }

                self.move_state.mark_field_moved(root, f);
            }
        }
    }

    fn check_use(&mut self, root: StrId, field: Option<StrId>, root_ty: &HirType<'a, 'bump>) {
        if self.copy_analysis.borrow().type_is_copy(root_ty) {
            return;
        }
        let name = self.str_id_to_string(root);
        match field {
            None => {
                if self.move_state.blocks_whole_use(root) {
                    self.record(TypeErrorKind::Generic(format!(
                        "use of moved value: `{}`",
                        name
                    )));
                }
            }
            Some(f) => {
                if self.move_state.is_field_moved(root, f) {
                    self.record(TypeErrorKind::Generic(format!(
                        "use of moved value: `{}.{}`",
                        name,
                        self.str_id_to_string(f)
                    )));
                }
            }
        }
    }

    fn check_and_record_value_use(&mut self, expr: &HirExpr<'a, 'bump>, ty: &HirType<'a, 'bump>) {
        if let Some(place) = self.resolve_place(expr) {
            self.check_borrow_use(expr, place, BorrowKind::Shared);
        }
        match expr {
            HirExpr::Ident(name, _) => {
                self.check_use(*name, None, ty);
                self.record_move(*name, None, ty, None);
            }
            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                if let HirExpr::Ident(root, _) = &**object {
                    let root_name = self.str_id_to_string(*root);
                    let container_ty = self.context.get_variable(&root_name);
                    self.check_use(*root, Some(*field), ty);
                    self.record_move(*root, Some(*field), ty, container_ty.map(|f| f.1).as_ref());
                }
            }
            _ => {}
        }
    }

    fn types_structurally_equal(&self, a: &HirType<'a, 'bump>, b: &HirType<'a, 'bump>) -> bool {
        use HirType::*;
        if matches!(a, Generic(_)) || matches!(b, Generic(_)) {
            return true;
        }
        match (a, b) {
            (
                Struct {
                    name: na,
                    type_args: ta,
                    ..
                },
                Struct {
                    name: nb,
                    type_args: tb,
                    ..
                },
            ) => {
                na == nb
                    && ta.len() == tb.len()
                    && ta
                        .iter()
                        .zip(tb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
            }
            (
                Ref {
                    inner: ia,
                    mutability_state: ma,
                    ..
                },
                Ref {
                    inner: ib,
                    mutability_state: mb,
                    ..
                },
            ) => {
                // Deliberately ignore provenance here, it's borrow-checker
                // bookkeeping about where a reference came from, not part of
                // the reference's type identity. Two `&mut i64` are the same
                // type regardless of which place each one happens to alias.
                ma == mb && self.types_structurally_equal(ia, ib)
            }
            (Nullable(ia), Nullable(ib)) => self.types_structurally_equal(ia, ib),
            (
                SafePointer {
                    inner: ia,
                    mutability_state: ma,
                },
                SafePointer {
                    inner: ib,
                    mutability_state: mb,
                },
            ) => ma == mb && self.types_structurally_equal(ia, ib),
            (
                UnsafePointer {
                    inner: ia,
                    mutability_state: ma,
                },
                UnsafePointer {
                    inner: ib,
                    mutability_state: mb,
                },
            ) => ma == mb && self.types_structurally_equal(ia, ib),
            (
                OwnedPointer {
                    inner: ia,
                    allocator: aa,
                },
                OwnedPointer {
                    inner: ib,
                    allocator: ab,
                },
            ) => aa == ab && self.types_structurally_equal(ia, ib),
            (Array(ia, la), Array(ib, lb)) => la == lb && self.types_structurally_equal(ia, ib),
            (Slice(ia), Slice(ib)) => self.types_structurally_equal(ia, ib),
            (Tuple(ta), Tuple(tb)) => {
                ta.len() == tb.len()
                    && ta
                        .iter()
                        .zip(tb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
            }
            (Dyn { bounds: ba }, Dyn { bounds: bb }) => {
                ba.len() == bb.len()
                    && ba
                        .iter()
                        .zip(bb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
            }
            (
                Lambda {
                    params: pa,
                    return_type: ra,
                },
                Lambda {
                    params: pb,
                    return_type: rb,
                },
            ) => {
                pa.len() == pb.len()
                    && pa
                        .iter()
                        .zip(pb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
                    && self.types_structurally_equal(ra, rb)
            }
            (Enum(na, ta), Enum(nb, tb)) => {
                na == nb
                    && (ta.is_empty()
                        || tb.is_empty()
                        || (ta.len() == tb.len()
                            && ta
                                .iter()
                                .zip(tb.iter())
                                .all(|(x, y)| self.types_structurally_equal(x, y))))
            }
            _ => a == b,
        }
    }

    fn substitute_type_local(
        &self,
        ty: &HirType<'a, 'bump>,
        subs: &FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> HirType<'a, 'bump> {
        match ty {
            HirType::Generic(name) => subs.get(name).copied().unwrap_or(*ty),
            HirType::Nullable(inner) => HirType::Nullable(
                self.context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
            ),
            HirType::Array(inner, len) => HirType::Array(
                self.context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
                *len,
            ),
            HirType::Slice(inner) => HirType::Slice(
                self.context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
            ),
            HirType::SafePointer {
                inner,
                mutability_state,
            } => HirType::SafePointer {
                inner: self
                    .context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
                mutability_state: *mutability_state,
            },
            HirType::UnsafePointer {
                inner,
                mutability_state,
            } => HirType::UnsafePointer {
                inner: self
                    .context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
                mutability_state: *mutability_state,
            },
            HirType::OwnedPointer { inner, allocator } => HirType::OwnedPointer {
                inner: self
                    .context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
                allocator: *allocator,
            },
            HirType::Ref {
                inner,
                mutability_state,
                provenance,
            } => HirType::Ref {
                inner: self
                    .context
                    .bump
                    .alloc_value(self.substitute_type_local(inner, subs)),
                mutability_state: *mutability_state,
                provenance: *provenance,
            },
            HirType::Tuple(elems) => {
                let new_elems: Vec<_> = elems
                    .iter()
                    .map(|e| self.substitute_type_local(e, subs))
                    .collect();
                HirType::Tuple(self.context.bump.alloc_slice_copy(&new_elems))
            }

            HirType::Struct {
                name,
                field_types,
                type_args,
            } => {
                let new_fields: Vec<_> = field_types
                    .iter()
                    .map(|f| self.substitute_type_local(f, subs))
                    .collect();
                let new_args: Vec<_> = type_args
                    .iter()
                    .map(|a| self.substitute_type_local(a, subs))
                    .collect();
                HirType::Struct {
                    name: *name,
                    field_types: self.context.bump.alloc_slice_copy(&new_fields),
                    type_args: self.context.bump.alloc_slice_copy(&new_args),
                }
            }
            HirType::Enum(name, type_args) => {
                let new_args: Vec<_> = type_args
                    .iter()
                    .map(|a| self.substitute_type_local(a, subs))
                    .collect();
                HirType::Enum(*name, self.context.bump.alloc_slice_copy(&new_args))
            }
            HirType::Dyn { bounds } => {
                let new_bounds: Vec<_> = bounds
                    .iter()
                    .map(|b| self.substitute_type_local(b, subs))
                    .collect();
                HirType::Dyn {
                    bounds: self.context.bump.alloc_slice_copy(&new_bounds),
                }
            }
            HirType::Lambda {
                params,
                return_type,
            } => {
                let new_params: Vec<_> = params
                    .iter()
                    .map(|p| self.substitute_type_local(p, subs))
                    .collect();
                HirType::Lambda {
                    params: self.context.bump.alloc_slice_copy(&new_params),
                    return_type: self
                        .context
                        .bump
                        .alloc_value(self.substitute_type_local(return_type, subs)),
                }
            }

            _ => *ty,
        }
    }

    ///   Public:   visible everywhere.
    ///   Module:   visible only within the declaring module (DOESN'T WORK NOW)
    ///   Private:  visibile only within the same file
    ///   Internal: visible anywhere in the same package, not outside it.
    fn check_visibility(
        &self,
        visibility: Visibility,
        declaring_module_idx: usize,
        item_kind: &str,
        item_name: &str,
    ) -> TypeCheckResult<'a, ()> {
        let visible = match visibility {
            Visibility::Public => true,
            Visibility::Private => self.context.current_module_idx == declaring_module_idx,
            Visibility::Module => {
                todo!("Implement visibility check for the module itself, similar to how Rust crates work")
            }
            Visibility::Internal => {
                let dep_graph = self.context.dep_graph.borrow();
                dep_graph.get_module_package(self.context.current_module_idx)
                    == dep_graph.get_module_package(declaring_module_idx)
            }
        };

        if visible {
            Ok(())
        } else {
            Err(TypeErrorKind::Generic(format!(
                "{} `{}` is not visible from this module",
                item_kind, item_name,
            ))
            .at(self.current_span))
        }
    }

    fn substitute_params_local(
        &self,
        params: &[HirParam<'a, 'bump>],
        subs: &FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> &'bump [HirParam<'a, 'bump>] {
        let new_params: Vec<HirParam> = params
            .iter()
            .map(|p| match p {
                HirParam::Normal {
                    name,
                    param_type,
                    span,
                } => HirParam::Normal {
                    name: *name,
                    param_type: self.substitute_type_local(param_type, subs),
                    span: *span,
                },
                HirParam::This { kind, span } => HirParam::This {
                    kind: *kind,
                    span: *span,
                },
            })
            .collect();
        self.context.bump.alloc_slice(&new_params)
    }

    fn types_compatible(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<'a, ()> {
        if matches!(found, HirType::Never) {
            return Ok(());
        }
        if self.types_structurally_equal(expected, found) {
            return Ok(());
        }

        if *expected == HirType::Unknown || *found == HirType::Unknown {
            return Ok(());
        }

        if self.struct_satisfies_interface_type(expected, found)
            || self.struct_satisfies_interface_type(found, expected)
        {
            return Ok(());
        }

        if let HirType::Nullable(_) = expected {
            if found == &HirType::Null {
                return Ok(());
            }
        }

        Err(TypeErrorKind::TypeMismatch {
            expected: self.type_to_string(expected),
            found: self.type_to_string(found),
        }
        .at(self.current_span))
    }

    fn struct_satisfies_interface_type(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> bool {
        let expected_inner = Self::strip_ref(expected);
        let found_inner = Self::strip_ref(found);

        let interface_name = match expected_inner {
            HirType::DynInterface(name, _) => Some(name.to_string()),
            HirType::Dyn { bounds } => bounds.iter().find_map(|b| match b {
                HirType::DynInterface(name, _) => Some(name.to_string()),
                HirType::Struct { name, .. } => {
                    let name_str = name.to_string();
                    if self.context.get_interface(&name_str).is_some() {
                        Some(name_str)
                    } else {
                        None
                    }
                }
                _ => None,
            }),
            _ => None,
        };

        let Some(interface_name) = interface_name else {
            return false;
        };

        let struct_name = match found_inner {
            HirType::Struct { name, .. } => name.to_string(),
            _ => return false,
        };

        self.context
            .struct_implements(&struct_name, &interface_name)
    }

    fn strip_ref<'x>(ty: &'x HirType<'a, 'bump>) -> &'x HirType<'a, 'bump> {
        match ty {
            HirType::Ref { inner, .. } => inner,
            HirType::SafePointer { inner, .. } => inner,
            HirType::OwnedPointer { inner, .. } => inner,
            _ => ty,
        }
    }

    fn is_numeric(&self, ty: &HirType<'a, 'bump>) -> bool {
        matches!(
            ty,
            HirType::I8
                | HirType::I16
                | HirType::I32
                | HirType::I64
                | HirType::U8
                | HirType::U16
                | HirType::U32
                | HirType::U64
                | HirType::F32
                | HirType::F64
                | HirType::I128
                | HirType::U128
                | HirType::Usize
                | HirType::Isize
        )
    }

    fn is_integer(&self, ty: &HirType<'a, 'bump>) -> bool {
        matches!(
            ty,
            HirType::I8
                | HirType::I16
                | HirType::I32
                | HirType::I64
                | HirType::U8
                | HirType::U16
                | HirType::U32
                | HirType::U64
                | HirType::I128
                | HirType::U128
                | HirType::Usize
                | HirType::Isize
        )
    }

    fn is_comparable(&self, ty: &HirType<'a, 'bump>) -> bool {
        self.is_numeric(ty) || matches!(ty, HirType::Boolean | HirType::String)
    }

    fn str_id_to_string(&self, id: StrId) -> String {
        format!("{}", id)
    }

    fn type_to_string(&self, ty: &HirType<'a, 'bump>) -> String {
        match ty {
            HirType::I8 => "i8".to_string(),
            HirType::I16 => "i16".to_string(),
            HirType::I32 => "i32".to_string(),
            HirType::I64 => "i64".to_string(),
            HirType::U8 => "u8".to_string(),
            HirType::U16 => "u16".to_string(),
            HirType::U32 => "u32".to_string(),
            HirType::U64 => "u64".to_string(),
            HirType::F32 => "f32".to_string(),
            HirType::F64 => "f64".to_string(),
            HirType::I128 => "i128".to_string(),
            HirType::U128 => "u128".to_string(),
            HirType::Boolean => "bool".to_string(),
            HirType::String => "str".to_string(),
            HirType::Void => "void".to_string(),
            HirType::Unknown => "<unknown>".to_string(),
            HirType::Struct {
                name, type_args, ..
            } => {
                if type_args.is_empty() {
                    format!("struct {}", self.str_id_to_string(*name))
                } else {
                    let args = type_args
                        .iter()
                        .map(|t| self.type_to_string(t))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("struct {}<{}>", self.str_id_to_string(*name), args)
                }
            }
            HirType::DynInterface(name, _) => format!("interface {}", self.str_id_to_string(*name)),
            HirType::Enum(name, type_args) => {
                if type_args.is_empty() {
                    format!("enum {}", self.str_id_to_string(*name))
                } else {
                    let args = type_args
                        .iter()
                        .map(|t| self.type_to_string(t))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("enum {}<{}>", self.str_id_to_string(*name), args)
                }
            }
            HirType::Generic(name) => format!("generic {}", self.str_id_to_string(*name)),
            HirType::SafePointer {
                inner,
                mutability_state,
            } => format!("*{} {}", mutability_state, inner),
            HirType::UnsafePointer {
                inner,
                mutability_state,
            } => format!("[*]{} {}", mutability_state, inner),
            HirType::Lambda { .. } => "lambda".to_string(),
            HirType::This => "this".to_string(),
            HirType::Null => "null".to_string(),
            HirType::Char => "char".to_string(),
            HirType::Ref {
                inner,
                mutability_state,
                provenance,
            } => {
                let displayed_provenance = if let Some(provenance) = provenance {
                    format!("{}", provenance)
                } else {
                    String::new()
                };
                if let MutabilityState::Mut = mutability_state {
                    format!("&{}mut {}", displayed_provenance, inner)
                } else {
                    format!("&{}{}", displayed_provenance, inner)
                }
            }
            HirType::Nullable(hir_type) => format!("{}?", hir_type),
            HirType::Dyn { bounds } => {
                let mut bounds_str = String::new();
                let mut start = true;
                for bound in *bounds {
                    bounds_str.push_str(&bound.to_string());
                    if start {
                        start = false;
                    } else {
                        bounds_str.push_str(" + ");
                    }
                }
                format!("dyn {}", bounds_str)
            }
            HirType::Tuple(hir_types) => format!(
                "({})",
                hir_types
                    .iter()
                    .map(|t| self.type_to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            HirType::Array(inner, len) => format!("[{}]{}", len, self.type_to_string(inner)),
            HirType::Slice(inner) => format!("[]{}", self.type_to_string(inner)),
            HirType::OwnedPointer { inner, allocator } => {
                format!(
                    "^{} {}",
                    allocator
                        .map(|all| self.provenance_to_string(&all))
                        .unwrap_or(String::from("")),
                    self.type_to_string(inner)
                )
            }
            HirType::Usize => "usize".to_string(),
            HirType::Isize => "isize".to_string(),
            HirType::Never => "never".to_string(),
            HirType::Range { elem, inclusive } => format!(
                "range<{}>{}",
                self.type_to_string(elem),
                if *inclusive { " (inclusive)" } else { "" }
            ),
        }
    }

    fn describe_borrow_error(
        &self,
        err: &BorrowError,
        provenance: Option<&ProvenanceAnnotation>,
    ) -> String {
        let base = match err {
            BorrowError::UseAfterMove { .. } => "use of a value after it was moved".to_string(),
            BorrowError::MutablyBorrowed { .. } => {
                "cannot borrow: value is already mutably borrowed".to_string()
            }
            BorrowError::AlreadyMutablyBorrowed { .. } => {
                "cannot borrow as mutable: already mutably borrowed elsewhere".to_string()
            }
            BorrowError::Borrowed { .. } => {
                "cannot borrow as mutable: value is already borrowed".to_string()
            }
            BorrowError::InvalidMove { .. } => "invalid move".to_string(),
            BorrowError::InvalidWrite { .. } => "invalid write".to_string(),
            BorrowError::InvalidRead { .. } => "invalid read".to_string(),
            BorrowError::CannotMoveBorrowed { .. } => {
                "cannot move out of a value while it is borrowed".to_string()
            }
            BorrowError::UnknownAlias { .. } => {
                "cannot prove these two accesses don't overlap".to_string()
            }
            BorrowError::LoanNotFound(_)
            | BorrowError::PlaceNotFound(_)
            | BorrowError::ProvenanceNotFound(_) => "internal borrow-checker error".to_string(),
        };

        match provenance {
            Some(p) => format!("{} (via {})", base, self.provenance_to_string(p)),
            None => base.to_string(),
        }
    }

    fn provenance_to_string(&self, p: &ProvenanceAnnotation) -> String {
        let root = match p.root {
            ProvenanceRoot::Var(name) => self.str_id_to_string(name),
            ProvenanceRoot::ThisRoot => "this".to_string(),
            ProvenanceRoot::Global {
                module_idx: _,
                name,
            } => self.str_id_to_string(name),
            ProvenanceRoot::ImplicitParam(_) => todo!(),
        };
        p.path.iter().fold(root, |acc, seg| match seg {
            ProvenancePathSegment::Field(f) => format!("{}.{}", acc, self.str_id_to_string(*f)),
            ProvenancePathSegment::Deref => format!("*{}", acc),
        })
    }

    fn peek_type(&self, expr: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        match expr {
            HirExpr::Ident(name, _) => {
                let var_name = self.str_id_to_string(*name);
                self.context
                    .get_variable(&var_name)
                    .unwrap_or((SymbolId::Local(LocalSymbolId(u32::MAX)), HirType::Unknown))
                    .1
            }
            HirExpr::This { .. } => {
                self.context
                    .get_variable("this")
                    .unwrap_or((SymbolId::Local(LocalSymbolId(u32::MAX)), HirType::This))
                    .1
            }
            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                match self.peek_type(object) {
                    HirType::Struct {
                        name: struct_name,
                        type_args,
                        ..
                    } => {
                        let struct_name_str = self.str_id_to_string(struct_name);
                        let field_name = self.str_id_to_string(*field);
                        let Some(struct_def) = self.context.get_struct(&struct_name_str) else {
                            return HirType::Unknown;
                        };
                        let Some(field_idx) = struct_def
                            .fields
                            .iter()
                            .position(|f| self.str_id_to_string(f.name) == field_name)
                        else {
                            return HirType::Unknown;
                        };
                        if type_args.is_empty() {
                            struct_def.fields[field_idx].field_type
                        } else {
                            self.instantiate_struct(struct_name, type_args)
                                .map(|fields| fields[field_idx])
                                .unwrap_or(struct_def.fields[field_idx].field_type)
                        }
                    }
                    _ => HirType::Unknown,
                }
            }
            HirExpr::Deref { expr, .. } => match self.peek_type(expr) {
                HirType::Ref { inner, .. } => *inner,
                HirType::SafePointer { inner, .. } => *inner,
                HirType::UnsafePointer { inner, .. } => *inner,
                HirType::OwnedPointer { inner, .. } => *inner,
                _ => HirType::Unknown,
            },
            HirExpr::Index { object, .. } => match self.peek_type(object) {
                HirType::Array(inner, _) => *inner,
                HirType::Slice(inner) => *inner,
                _ => HirType::Unknown,
            },
            HirExpr::Cast { target_type, .. } => *target_type,
            _ => HirType::Unknown,
        }
    }

    fn resolve_place(&mut self, expr: &HirExpr<'a, 'bump>) -> Option<PlaceId> {
        match expr {
            HirExpr::Ident(name, _) => self
                .local_provenance_place
                .get(name)
                .copied()
                .or_else(|| self.borrow_checker.local_place(*name).copied()),

            HirExpr::This { .. } => self.borrow_checker.local_place(self.this_id).copied(),

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                let base = self.resolve_place(object)?;
                let base = match self.peek_type(object) {
                    HirType::Ref { .. }
                    | HirType::SafePointer { .. }
                    | HirType::UnsafePointer { .. }
                    | HirType::OwnedPointer { .. } => self.borrow_checker.project_deref(base),
                    _ => base,
                };
                Some(self.borrow_checker.project_field(base, *field))
            }

            HirExpr::Deref { expr, .. } => {
                let base = self.resolve_place(expr)?;
                Some(self.borrow_checker.project_deref(base))
            }

            HirExpr::Index { object, index, .. } => match self.peek_type(object) {
                HirType::Array(_, _) | HirType::Slice(_) => {
                    let base = self.resolve_place(object)?;
                    let bound = self.expr_to_bound(index);
                    let interval = Interval {
                        lower: bound.clone(),
                        upper: bound,
                    };
                    Some(self.borrow_checker.project_index(
                        base,
                        interval,
                        IndexContainer::Primitive,
                    ))
                }
                HirType::SafePointer { .. } | HirType::UnsafePointer { .. } => {
                    let ptr_place = self.resolve_place(object)?;
                    let (base, cur) = self.borrow_checker.pointee_of(ptr_place)?.clone();
                    let idx_bound = self.expr_to_bound(index);
                    let combined = Bound::Sum(Box::new(cur.lower.clone()), Box::new(idx_bound));
                    let interval = Interval {
                        lower: combined.clone(),
                        upper: combined,
                    };
                    Some(self.borrow_checker.project_index(
                        base,
                        interval,
                        IndexContainer::Primitive,
                    ))
                }
                _ => None,
            },

            HirExpr::Binary {
                left,
                op: op @ (Operator::Add | Operator::Subtract),
                right,
                ..
            } => {
                if !matches!(
                    self.peek_type(left),
                    HirType::SafePointer { .. } | HirType::UnsafePointer { .. }
                ) {
                    return None;
                }
                let ptr_place = self.resolve_place(left)?;
                let (base, cur) = self.borrow_checker.pointee_of(ptr_place)?.clone();
                let delta = self.expr_to_bound(right);
                let signed = match op {
                    Operator::Subtract => Bound::Scale {
                        base: Box::new(delta),
                        factor: -1,
                    },
                    _ => delta,
                };
                let combined = Bound::Sum(Box::new(cur.lower.clone()), Box::new(signed));
                let interval = Interval {
                    lower: combined.clone(),
                    upper: combined,
                };
                Some(
                    self.borrow_checker
                        .project_index(base, interval, IndexContainer::Primitive),
                )
            }

            _ => None,
        }
    }

    fn fresh_opaque(&mut self) -> Bound {
        self.next_opaque_id += 1;
        Bound::Opaque(self.next_opaque_id)
    }

    fn expr_to_bound(&mut self, expr: &HirExpr<'a, 'bump>) -> Bound {
        match expr {
            HirExpr::Number(value, _) => Bound::Const(*value),
            HirExpr::Ident(name, _) => Bound::Symbol(*name),

            HirExpr::Binary {
                left,
                op: Operator::Add,
                right,
                ..
            } => match (self.expr_to_bound(left), self.expr_to_bound(right)) {
                (base, Bound::Const(c)) | (Bound::Const(c), base) => Bound::Offset {
                    base: Box::new(base),
                    offset: c,
                },
                _ => self.fresh_opaque(),
            },

            HirExpr::Binary {
                left,
                op: Operator::Subtract,
                right,
                ..
            } => match (self.expr_to_bound(left), self.expr_to_bound(right)) {
                (base, Bound::Const(c)) => Bound::Offset {
                    base: Box::new(base),
                    offset: -c,
                },
                _ => self.fresh_opaque(),
            },

            HirExpr::Binary {
                left,
                op: Operator::Multiply,
                right,
                ..
            } => match (self.expr_to_bound(left), self.expr_to_bound(right)) {
                (base, Bound::Const(c)) | (Bound::Const(c), base) => Bound::Scale {
                    base: Box::new(base),
                    factor: c,
                },
                _ => self.fresh_opaque(),
            },

            _ => self.fresh_opaque(),
        }
    }

    /// If `expr` is a local currently holding a live loan (i.e. a `&`/`&mut`
    /// this-code created earlier), returns the place that loan actually
    /// covers.
    fn loan_referent_place(&self, expr: &HirExpr<'a, 'bump>) -> Option<PlaceId> {
        let HirExpr::Ident(name, _) = expr else {
            return None;
        };
        let loan_id = self
            .loan_owners
            .iter()
            .find(|(_, &owner)| owner == *name)
            .map(|(&id, _)| id)?;
        self.borrow_checker.loan(loan_id).map(|loan| loan.place)
    }

    fn condition_to_place_fact(
        &self,
        cond: &HirExpr<'a, 'bump>,
    ) -> Option<(PlaceId, PlaceId, bool)> {
        let HirExpr::Comparison {
            left, op, right, ..
        } = cond
        else {
            return None;
        };
        let is_equal = match op {
            Operator::Equals => true,
            Operator::NotEquals => false,
            _ => return None,
        };
        let lp = self.loan_referent_place(left)?;
        let rp = self.loan_referent_place(right)?;
        Some((lp, rp, is_equal))
    }

    fn check_borrow_use(&mut self, expr: &HirExpr<'a, 'bump>, place: PlaceId, kind: BorrowKind) {
        if let Err(e) = self.borrow_checker.check_use(place, kind) {
            let provenance = self.infer_provenance(expr);
            let msg = self.describe_borrow_error(&e, provenance.as_ref());
            self.record(TypeErrorKind::Generic(msg));
        }
    }

    fn check_bare_name_import(
        &mut self,
        declaring_module: Option<usize>,
        name: StrId,
        name_str: &str,
        kind: BareImportKind,
    ) {
        let Some(declaring_module) = declaring_module else {
            return;
        };
        let current = self.context.current_module_idx;
        if declaring_module == current {
            return;
        }

        let explicitly_imported = self.imports_by_module.get(&current).is_some_and(|imp| {
            imp.modules.contains(&declaring_module)
                || imp.named.values().any(|&m| m == declaring_module)
        });
        if explicitly_imported {
            return;
        }

        let wildcard_modules: Vec<usize> = self
            .imports_by_module
            .get(&current)
            .map(|imp| imp.wildcard.clone())
            .unwrap_or_default();

        if !wildcard_modules.contains(&declaring_module) {
            self.record(TypeErrorKind::Generic(format!(
                "{} `{}` is declared in another module and has not been imported",
                kind.as_str(),
                name_str,
            )));
            return;
        }

        let by_module: &FxHashMap<usize, HashSet<StrId>> = match kind {
            BareImportKind::Struct => &self.structs_by_module,
            BareImportKind::Enum => &self.enums_by_module,
        };
        let candidates: Vec<usize> = wildcard_modules
            .iter()
            .copied()
            .filter(|m| by_module.get(m).is_some_and(|set| set.contains(&name)))
            .collect();

        if candidates.len() > 1 {
            let candidate_pkgs: Vec<String> = candidates
                .iter()
                .filter_map(|&m| self.context.dep_graph.borrow().get_module_package(m))
                .map(|p| p.to_string())
                .collect();
            self.record(TypeErrorKind::Generic(format!(
                "`{}` is ambiguous: it is auto-imported from multiple packages ({}); \
                 add an explicit `import` to disambiguate",
                name_str,
                candidate_pkgs.join(", "),
            )));
        }
    }

    fn check_match_exhaustiveness(
        &mut self,
        scrutinee_ty: &HirType<'a, 'bump>,
        arms: &[HirMatchArm<'a, 'bump>],
    ) {
        if matches!(scrutinee_ty, HirType::Unknown) {
            return; // already errored elsewhere; don't cascade
        }

        let has_catch_all = arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(arm.pattern, HirPattern::Wildcard | HirPattern::Ident(_))
        });
        if has_catch_all {
            return;
        }

        match scrutinee_ty {
            HirType::Enum(enum_name, _) => {
                let enum_name_str = self.str_id_to_string(*enum_name);
                let Some(def) = self.context.get_enum(&enum_name_str) else {
                    return;
                };
                let covered: std::collections::HashSet<StrId> = arms
                    .iter()
                    .filter(|arm| arm.guard.is_none())
                    .filter_map(|arm| match &arm.pattern {
                        HirPattern::EnumVariant { variant, .. } => Some(*variant),
                        _ => None,
                    })
                    .collect();
                let missing: Vec<&str> = def
                    .variants
                    .iter()
                    .filter(|v| !covered.contains(&v.name))
                    .map(|v| v.name.as_str())
                    .collect();
                if !missing.is_empty() {
                    self.record(TypeErrorKind::Generic(format!(
                        "non-exhaustive match on enum `{}`: missing variant(s) {}",
                        enum_name_str,
                        missing.join(", ")
                    )));
                }
            }

            HirType::Boolean => {
                let mut has_true = false;
                let mut has_false = false;
                for arm in arms.iter().filter(|a| a.guard.is_none()) {
                    match &arm.pattern {
                        HirPattern::Boolean(true) => has_true = true,
                        HirPattern::Boolean(false) => has_false = true,
                        _ => {}
                    }
                }
                if !(has_true && has_false) {
                    self.record(TypeErrorKind::Generic(
                        "non-exhaustive match on `bool`: requires a wildcard (`_`) arm or both `true` and `false` arms".to_string()
                    ));
                }
            }

            HirType::I8
            | HirType::I16
            | HirType::I32
            | HirType::I64
            | HirType::I128
            | HirType::U8
            | HirType::U16
            | HirType::U32
            | HirType::U64
            | HirType::U128
            | HirType::Usize
            | HirType::Isize
            | HirType::String
            | HirType::Char => {
                self.record(TypeErrorKind::Generic(format!(
                    "non-exhaustive match on `{}`: requires a wildcard (`_`) or binding (catch-all) arm",
                    self.type_to_string(scrutinee_ty)
                )));
            }

            _ => {} // structs/tuples/etc: not enforced yet
        }
    }

    fn local_used_after(&self, point: PointId, local: StrId) -> bool {
        let mut stack: Vec<PointId> = vec![point];
        let mut visited: HashSet<PointId> = HashSet::default();

        while let Some(p) = stack.pop() {
            if !visited.insert(p) {
                continue;
            }
            if self
                .point_locals_used
                .get(&p)
                .is_some_and(|set| set.contains(&local))
            {
                return true;
            }
            if let Some(succs) = self.cfg.successors.get(&p) {
                stack.extend(succs.iter().copied());
            }
        }
        false
    }

    fn unify_generic(
        &self,
        declared: &HirType<'a, 'bump>,
        actual: &HirType<'a, 'bump>,
        subs: &mut FxHashMap<StrId, HirType<'a, 'bump>>,
    ) {
        match declared {
            HirType::Generic(name) => {
                subs.entry(*name).or_insert(*actual);
            }
            HirType::Nullable(inner) => {
                if let HirType::Nullable(actual_inner) = actual {
                    self.unify_generic(inner, actual_inner, subs);
                }
            }
            HirType::Array(inner, _) => {
                if let HirType::Array(actual_inner, _) = actual {
                    self.unify_generic(inner, actual_inner, subs);
                }
            }
            HirType::Slice(inner) => {
                if let HirType::Slice(actual_inner) = actual {
                    self.unify_generic(inner, actual_inner, subs);
                }
            }
            HirType::Ref { inner, .. } => {
                self.unify_generic(inner, Self::strip_ref(actual), subs);
            }
            _ => {}
        }
    }
}
