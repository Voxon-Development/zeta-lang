use ir::ast::{
    Block, ConstStmt, DeferAction, ElseBranch, EnumDecl, ErrorHandlerPattern, Expr, Field, ForKind,
    FuncDecl, ImplDecl, InterfaceDecl, Param, Path, Stmt, StructDecl, Type, TypeKind,
};
use ir::hir::StrId;
use ir::ir_hasher::{HashMap, HashSet};
use std::collections::VecDeque;
use zetaruntime::string_pool::StringPool;

pub type NodeIdx = usize;

#[derive(Copy, Clone)]
pub struct AstModule<'a, 'bump> {
    pub name: StrId,
    pub stmts: &'bump [Stmt<'a, 'bump>],
}

#[derive(Clone, Debug)]
pub enum NodeKind {
    Module { module_idx: usize },
    TypeDecl { module_idx: usize, item_idx: usize },
    FuncSig { module_idx: usize, item_idx: usize },
    FuncBody { module_idx: usize, item_idx: usize },
    ConstDecl { module_idx: usize, item_idx: usize },
    TraitDecl { module_idx: usize, item_idx: usize },
    TraitImpl { module_idx: usize, item_idx: usize },
}

impl NodeKind {
    pub fn is_func_body(&self) -> bool {
        matches!(self, NodeKind::FuncBody { .. })
    }
    pub fn is_type_decl(&self) -> bool {
        matches!(self, NodeKind::TypeDecl { .. })
    }
    pub fn module_idx(&self) -> Option<usize> {
        match self {
            NodeKind::Module { module_idx }
            | NodeKind::TypeDecl { module_idx, .. }
            | NodeKind::FuncSig { module_idx, .. }
            | NodeKind::FuncBody { module_idx, .. }
            | NodeKind::ConstDecl { module_idx, .. }
            | NodeKind::TraitDecl { module_idx, .. }
            | NodeKind::TraitImpl { module_idx, .. } => Some(*module_idx),
        }
    }
}

#[derive(Debug)]
pub struct DepNode {
    pub idx: NodeIdx,
    pub kind: NodeKind,
    /// Human-readable name hint (the declared identifier), used for debugging.
    pub hint: Option<StrId>,
    /// Outgoing edges: this node depends on these.
    pub deps: Vec<NodeIdx>,
    /// Reverse edges: these nodes depend on this one.
    pub rev_deps: Vec<NodeIdx>,
}

impl DepNode {
    fn new(idx: NodeIdx, kind: NodeKind, hint: Option<StrId>) -> Self {
        DepNode {
            idx,
            kind,
            hint,
            deps: Vec::new(),
            rev_deps: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedImport {
    /// The module that contains the import statement.
    pub from_module_idx: usize,
    /// The full path segments as written in source, e.g. `["zeta", "io", "File"]`.
    pub path: Vec<StrId>,
}

/// Three-part key used to intern item nodes: (module, position-in-module, role).
/// `role` is one of the static strings `"func_sig"`, `"func_body"`, `"type"`,
/// `"const"`, `"trait"`, `"impl"`, `"module"`.
type ItemKey = (usize, usize, &'static str);

#[derive(Default, Debug)]
struct PathIndex {
    /// key: Vec<StrId> representing the full qualified path of a module,
    /// value: module_idx
    map: HashMap<Vec<StrId>, usize>,
}

impl PathIndex {
    fn new() -> Self {
        PathIndex {
            map: HashMap::default(),
        }
    }

    /// Register a module under its full path.
    fn insert(&mut self, path: Vec<StrId>, module_idx: usize) {
        self.map.insert(path, module_idx);
    }

    /// Resolve a path slice to a module index, if known.
    fn resolve(&self, path: &[StrId]) -> Option<usize> {
        self.map.get(path).copied()
    }
}

#[derive(Default, Debug)]
pub struct DepGraph {
    nodes: Vec<DepNode>,

    /// (module_idx, item_idx, role) -> NodeIdx
    item_index: HashMap<ItemKey, NodeIdx>,

    /// (name StrId, module_idx) -> (module_idx, item_idx, role)
    /// Used for name-based resolution within / across modules.
    symbol_table: HashMap<(StrId, usize), (usize, usize, &'static str)>,

    /// module_idx -> package path StrId (the `package a::b::c` declaration)
    package_hierarchy: HashMap<usize, StrId>,

    /// Fast path-segment lookup built from package declarations.
    path_index: PathIndex,

    /// Imports that could not be resolved during graph construction.
    pub unresolved_imports: Vec<UnresolvedImport>,
}

impl DepGraph {
    pub fn new() -> Self {
        DepGraph {
            nodes: Vec::new(),
            item_index: HashMap::default(),
            symbol_table: HashMap::default(),
            package_hierarchy: HashMap::default(),
            path_index: PathIndex::new(),
            unresolved_imports: Vec::new(),
        }
    }

    /// Resolve a `::`-segmented path to a module index, using the same
    /// path_index built from `package` declarations during graph construction.
    pub fn resolve_module_path(&self, path: &[StrId]) -> Option<usize> {
        self.path_index.resolve(path)
    }

    /// Resolve `name` as a function declared directly in `module_idx`,
    /// returning its (module_idx, item_idx) if found, restricted to function
    /// declarations (func_sig/func_body), not types/consts/traits.
    pub fn resolve_function_in_module(
        &self,
        module_idx: usize,
        name: StrId,
    ) -> Option<(usize, usize)> {
        let &(m, i, tag) = self.symbol_table.get(&(name, module_idx))?;
        if tag == "func_sig" || tag == "func_body" {
            Some((m, i))
        } else {
            None
        }
    }

    /// Find all (module_idx, name) pairs across every module where a function
    /// of this name exists. Used to build "did you mean...?" suggestions.
    pub fn find_function_by_name_anywhere(&self, name: StrId) -> Vec<usize> {
        self.symbol_table
            .iter()
            .filter_map(|(&(sym_name, module_idx), &(_, _, tag))| {
                if sym_name == name && (tag == "func_sig" || tag == "func_body") {
                    Some(module_idx)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn push_node(&mut self, kind: NodeKind, hint: Option<StrId>) -> NodeIdx {
        let idx = self.nodes.len();
        self.nodes.push(DepNode::new(idx, kind, hint));
        idx
    }

    pub fn register_item_node(
        &mut self,
        module_idx: usize,
        item_idx: usize,
        tag: &'static str,
        node_idx: NodeIdx,
    ) {
        self.item_index
            .insert((module_idx, item_idx, tag), node_idx);
    }

    pub fn lookup_item_node(
        &self,
        module_idx: usize,
        item_idx: usize,
        tag: &'static str,
    ) -> Option<NodeIdx> {
        self.item_index.get(&(module_idx, item_idx, tag)).copied()
    }

    /// Add a directed edge `a -> b` meaning "a depends on b".
    pub fn add_edge(&mut self, a: NodeIdx, b: NodeIdx) {
        if a >= self.nodes.len() || b >= self.nodes.len() || a == b {
            return;
        }
        if !self.nodes[a].deps.contains(&b) {
            self.nodes[a].deps.push(b);
        }
        if !self.nodes[b].rev_deps.contains(&a) {
            self.nodes[b].rev_deps.push(a);
        }
    }

    pub fn nodes(&self) -> &[DepNode] {
        &self.nodes
    }

    pub fn build_from_ast<'a, 'bump>(
        &mut self,
        modules: &[AstModule<'a, 'bump>],
        pool: &StringPool,
    ) {
        self.phase_a_create_nodes(modules, pool);
        self.phase_b_populate_symbol_table();
        self.phase_c_extract_edges(modules, pool);
    }

    fn phase_a_create_nodes<'a, 'bump>(
        &mut self,
        modules: &[AstModule<'a, 'bump>],
        pool: &StringPool,
    ) {
        for (midx, module) in modules.iter().enumerate() {
            let module_node =
                self.push_node(NodeKind::Module { module_idx: midx }, Some(module.name));
            self.register_item_node(midx, 0, "module", module_node);

            for stmt in module.stmts {
                if let Stmt::Package(pkg) = stmt {
                    let path_str = path_to_strid(&pkg.path, pool);
                    self.package_hierarchy.insert(midx, path_str);

                    let seg_vec: Vec<StrId> = pkg.path.path.to_vec();

                    self.path_index.insert(seg_vec, midx);
                }
            }

            for (item_idx, stmt) in module.stmts.iter().enumerate() {
                self.create_node_for_stmt(midx, item_idx, stmt);
            }
        }
    }

    fn create_node_for_stmt<'a, 'bump>(
        &mut self,
        module_idx: usize,
        item_idx: usize,
        stmt: &Stmt<'a, 'bump>,
    ) {
        match stmt {
            Stmt::FuncDecl(f) => {
                let sig = self.push_node(
                    NodeKind::FuncSig {
                        module_idx,
                        item_idx,
                    },
                    Some(f.name),
                );
                self.register_item_node(module_idx, item_idx, "func_sig", sig);

                let body = self.push_node(
                    NodeKind::FuncBody {
                        module_idx,
                        item_idx,
                    },
                    Some(f.name),
                );
                self.register_item_node(module_idx, item_idx, "func_body", body);
            }
            Stmt::StructDecl(s) => {
                let td = self.push_node(
                    NodeKind::TypeDecl {
                        module_idx,
                        item_idx,
                    },
                    Some(s.name),
                );
                self.register_item_node(module_idx, item_idx, "type", td);
            }
            Stmt::EnumDecl(e) => {
                let td = self.push_node(
                    NodeKind::TypeDecl {
                        module_idx,
                        item_idx,
                    },
                    Some(e.name),
                );
                self.register_item_node(module_idx, item_idx, "type", td);
            }
            Stmt::InterfaceDecl(i) => {
                let td = self.push_node(
                    NodeKind::TraitDecl {
                        module_idx,
                        item_idx,
                    },
                    Some(i.name),
                );
                self.register_item_node(module_idx, item_idx, "trait", td);
            }
            Stmt::ImplDecl(i) => {
                let td = self.push_node(
                    NodeKind::TraitImpl {
                        module_idx,
                        item_idx,
                    },
                    Some(i.target),
                );
                self.register_item_node(module_idx, item_idx, "impl", td);
            }
            Stmt::Const(c) => {
                let cd = self.push_node(
                    NodeKind::ConstDecl {
                        module_idx,
                        item_idx,
                    },
                    Some(c.ident),
                );
                self.register_item_node(module_idx, item_idx, "const", cd);
            }
            // Import / Package / Let / other control-flow stmts at module
            // scope do not produce independent graph nodes.
            _ => {}
        }
    }

    fn phase_b_populate_symbol_table(&mut self) {
        let entries: Vec<(ItemKey, NodeIdx)> =
            self.item_index.iter().map(|(&k, &v)| (k, v)).collect();

        for ((module_idx, item_idx, tag), node_idx) in entries {
            if let Some(node) = self.nodes.get(node_idx) {
                if let Some(hint) = node.hint {
                    // Last writer win, for most declarations there is only
                    // one entry per (name, module) pair.
                    self.symbol_table
                        .insert((hint, module_idx), (module_idx, item_idx, tag));
                }
            }
        }
    }

    fn phase_c_extract_edges<'a, 'bump>(
        &mut self,
        modules: &[AstModule<'a, 'bump>],
        pool: &StringPool,
    ) {
        for (midx, module) in modules.iter().enumerate() {
            for (item_idx, stmt) in module.stmts.iter().enumerate() {
                self.extract_edges_for_stmt(midx, item_idx, stmt, pool);
            }
        }
    }

    fn extract_edges_for_stmt<'a, 'bump>(
        &mut self,
        module_idx: usize,
        item_idx: usize,
        stmt: &Stmt<'a, 'bump>,
        pool: &StringPool,
    ) {
        match stmt {
            Stmt::Import(imp) => {
                let seg_vec: Vec<StrId> = imp.path.path.to_vec();
                if let Some(target_module_idx) = self.path_index.resolve(&seg_vec) {
                    self.register_import(module_idx, target_module_idx);
                } else {
                    self.unresolved_imports.push(UnresolvedImport {
                        from_module_idx: module_idx,
                        path: seg_vec,
                    });
                }
            }

            Stmt::FuncDecl(f) => {
                if let Some(sig_node) = self.lookup_item_node(module_idx, item_idx, "func_sig") {
                    self.walk_func_signature(f, sig_node, module_idx, pool);
                }
                if let Some(body_node) = self.lookup_item_node(module_idx, item_idx, "func_body") {
                    self.walk_func_body(f, body_node, module_idx, pool);
                    // Body always depends on its own signature.
                    if let Some(sig_node) = self.lookup_item_node(module_idx, item_idx, "func_sig")
                    {
                        self.add_edge(body_node, sig_node);
                    }
                }
            }

            Stmt::StructDecl(s) => {
                if let Some(type_node) = self.lookup_item_node(module_idx, item_idx, "type") {
                    self.walk_struct_decl(s, type_node, module_idx, pool);
                }
            }

            Stmt::EnumDecl(e) => {
                if let Some(type_node) = self.lookup_item_node(module_idx, item_idx, "type") {
                    self.walk_enum_decl(e, type_node, module_idx, pool);
                }
            }

            Stmt::InterfaceDecl(i) => {
                if let Some(trait_node) = self.lookup_item_node(module_idx, item_idx, "trait") {
                    self.walk_interface_decl(i, trait_node, module_idx, pool);
                }
            }

            Stmt::ImplDecl(i) => {
                if let Some(impl_node) = self.lookup_item_node(module_idx, item_idx, "impl") {
                    self.walk_impl_decl(i, impl_node, module_idx, pool);
                }
            }

            Stmt::Const(c) => {
                if let Some(const_node) = self.lookup_item_node(module_idx, item_idx, "const") {
                    self.walk_const_stmt(c, const_node, module_idx, pool);
                }
            }

            // Everything else at module scope (Package, Module nested decl,
            // stray Let, etc.) is ignored for dependency purposes.
            _ => {}
        }
    }

    fn walk_func_signature<'a, 'bump>(
        &mut self,
        f: &FuncDecl<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        if let Some(params) = f.params {
            for param in params {
                match param {
                    Param::Normal(np) => {
                        self.add_ast_type_dep(from_node, &np.type_annotation, module_idx, pool);
                    }
                    Param::This(_) => {}
                }
            }
        }
        if let Some(ret) = &f.return_type {
            self.add_ast_type_dep(from_node, ret, module_idx, pool);
        }
        if let Some(generics) = f.generics {
            for g in generics {
                for constraint in g.constraints {
                    self.add_ast_type_dep(from_node, constraint, module_idx, pool);
                }
            }
        }
    }

    fn walk_func_body<'a, 'bump>(
        &mut self,
        f: &FuncDecl<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        if let Some(body) = f.body {
            self.walk_block(body, from_node, module_idx, pool);
        }
    }

    fn walk_struct_decl<'a, 'bump>(
        &mut self,
        s: &StructDecl<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        if let Some(generics) = s.generics {
            for g in generics {
                for c in g.constraints {
                    self.add_ast_type_dep(from_node, c, module_idx, pool);
                }
            }
        }
        if let Some(params) = s.params {
            for param in params {
                if let Param::Normal(np) = param {
                    self.add_ast_type_dep(from_node, &np.type_annotation, module_idx, pool);
                }
            }
        }
    }

    fn walk_enum_decl<'a, 'bump>(
        &mut self,
        e: &EnumDecl<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        if let Some(generics) = e.generics {
            for g in generics {
                for c in g.constraints {
                    self.add_ast_type_dep(from_node, c, module_idx, pool);
                }
            }
        }
        for variant in e.variants {
            for field in variant.fields {
                self.walk_field(field, from_node, module_idx, pool);
            }
        }
    }

    fn walk_interface_decl<'a, 'bump>(
        &mut self,
        i: &InterfaceDecl<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        if let Some(generics) = i.generics {
            for g in generics {
                for c in g.constraints {
                    self.add_ast_type_dep(from_node, c, module_idx, pool);
                }
            }
        }
        if let Some(permits) = i.permits {
            for ty in permits.types {
                self.add_ast_type_dep(from_node, ty, module_idx, pool);
            }
        }
        if let Some(methods) = i.methods {
            for m in methods {
                self.walk_func_signature(m, from_node, module_idx, pool);
                if let Some(body) = m.body {
                    self.walk_block(body, from_node, module_idx, pool);
                }
            }
        }
    }

    fn walk_impl_decl<'a, 'bump>(
        &mut self,
        i: &ImplDecl<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        // The type being implemented
        self.resolve_name_to_edge(i.target, from_node, module_idx);
        // The interface being implemented, if any
        if let Some(iface) = i.interface {
            self.resolve_name_to_edge(iface, from_node, module_idx);
        }
        if let Some(generics) = i.generics {
            for g in generics {
                for c in g.constraints {
                    self.add_ast_type_dep(from_node, c, module_idx, pool);
                }
            }
        }
        if let Some(methods) = i.methods {
            for m in methods {
                self.walk_func_signature(m, from_node, module_idx, pool);
                if let Some(body) = m.body {
                    self.walk_block(body, from_node, module_idx, pool);
                }
            }
        }
        if let Some(constants) = i.constants {
            for c in constants {
                self.walk_const_stmt(c, from_node, module_idx, pool);
            }
        }
    }

    fn walk_const_stmt<'a, 'bump>(
        &mut self,
        c: &ConstStmt<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        self.add_ast_type_dep(from_node, &c.type_annotation, module_idx, pool);
        self.walk_expr(c.value, from_node, module_idx, pool);
    }

    fn walk_field<'a, 'bump>(
        &mut self,
        field: &Field<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        self.add_ast_type_dep(from_node, &field.field_type, module_idx, pool);
        if let Some(generics) = field.generics {
            for ty in generics {
                self.add_ast_type_dep(from_node, ty, module_idx, pool);
            }
        }
    }

    fn walk_block<'a, 'bump>(
        &mut self,
        block: &Block<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        for stmt in block.block {
            self.walk_stmt(stmt, from_node, module_idx, pool);
        }
    }

    fn walk_stmt<'a, 'bump>(
        &mut self,
        stmt: &Stmt<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        match stmt {
            Stmt::Let(l) => {
                self.add_ast_type_dep(from_node, &l.type_annotation, module_idx, pool);
                self.walk_expr(l.value, from_node, module_idx, pool);
            }
            Stmt::Const(c) => {
                self.walk_const_stmt(c, from_node, module_idx, pool);
            }
            Stmt::Return(r) => {
                if let Some(val) = r.value {
                    self.walk_expr(val, from_node, module_idx, pool);
                }
            }
            Stmt::If(i) => {
                self.walk_expr(i.condition, from_node, module_idx, pool);
                self.walk_block(i.then_branch, from_node, module_idx, pool);
                if let Some(else_branch) = i.else_branch {
                    match else_branch {
                        ElseBranch::If(nested) => {
                            // Recurse by reconstructing as a temporary Stmt reference.
                            // We borrow nested directly.
                            self.walk_expr(nested.condition, from_node, module_idx, pool);
                            self.walk_block(nested.then_branch, from_node, module_idx, pool);
                            if let Some(eb) = nested.else_branch {
                                self.walk_else_branch(eb, from_node, module_idx, pool);
                            }
                        }
                        ElseBranch::Else(block) => {
                            self.walk_block(block, from_node, module_idx, pool);
                        }
                    }
                }
            }
            Stmt::While(w) => {
                self.walk_expr(w.condition, from_node, module_idx, pool);
                self.walk_block(w.block, from_node, module_idx, pool);
            }
            Stmt::For(f) => {
                match &f.kind {
                    ForKind::CStyle {
                        let_stmt,
                        condition,
                        increment,
                    } => {
                        if let Some(ls) = let_stmt {
                            self.add_ast_type_dep(from_node, &ls.type_annotation, module_idx, pool);
                            self.walk_expr(ls.value, from_node, module_idx, pool);
                        }
                        if let Some(cond) = condition {
                            self.walk_expr(cond, from_node, module_idx, pool);
                        }
                        if let Some(inc) = increment {
                            self.walk_expr(inc, from_node, module_idx, pool);
                        }
                    }
                    ForKind::RangeBased { iterable, .. } => {
                        self.walk_expr(iterable, from_node, module_idx, pool);
                    }
                }
                self.walk_block(f.block, from_node, module_idx, pool);
            }
            Stmt::Match(m) => {
                self.walk_expr(m.expr, from_node, module_idx, pool);
                for arm in m.arms {
                    if let Some(guard) = arm.guard {
                        self.walk_expr(guard, from_node, module_idx, pool);
                    }
                    self.walk_block(arm.block, from_node, module_idx, pool);
                }
            }
            Stmt::UnsafeBlock(u) => {
                self.walk_block(u.block, from_node, module_idx, pool);
            }
            Stmt::Block(b) => {
                self.walk_block(b, from_node, module_idx, pool);
            }
            Stmt::Defer(d) => match d.action {
                DeferAction::Block(b) => self.walk_block(b, from_node, module_idx, pool),
                DeferAction::Stmt(s) => self.walk_stmt(s, from_node, module_idx, pool),
            },
            Stmt::ExprStmt(e) => {
                self.walk_expr(e.expr, from_node, module_idx, pool);
            }
            // Nested inline function / type declarations inside a body, treat
            // them as inline dependencies of the enclosing item.
            Stmt::FuncDecl(f) => {
                self.walk_func_signature(f, from_node, module_idx, pool);
                if let Some(body) = f.body {
                    self.walk_block(body, from_node, module_idx, pool);
                }
            }
            Stmt::StructDecl(s) => {
                self.walk_struct_decl(s, from_node, module_idx, pool);
            }
            Stmt::EnumDecl(e) => {
                self.walk_enum_decl(e, from_node, module_idx, pool);
            }
            Stmt::Break(Some(expr), _) => {
                self.walk_expr(expr, from_node, module_idx, pool);
            }
            // Import / Package / Continue / Break(None), no deps to extract
            // from inside a function body.
            _ => {}
        }
    }

    fn walk_else_branch<'a, 'bump>(
        &mut self,
        branch: &ElseBranch<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        match branch {
            ElseBranch::If(i) => {
                self.walk_expr(i.condition, from_node, module_idx, pool);
                self.walk_block(i.then_branch, from_node, module_idx, pool);
                if let Some(eb) = i.else_branch {
                    self.walk_else_branch(eb, from_node, module_idx, pool);
                }
            }
            ElseBranch::Else(b) => {
                self.walk_block(b, from_node, module_idx, pool);
            }
        }
    }

    fn walk_expr<'a, 'bump>(
        &mut self,
        expr: &Expr<'a, 'bump>,
        from_node: NodeIdx,
        module_idx: usize,
        pool: &StringPool,
    ) {
        match expr {
            Expr::Ident { name, .. } => {
                // A bare identifier may refer to a top-level declaration.
                self.resolve_name_to_edge(*name, from_node, module_idx);
            }
            Expr::GenericIdent {
                name, generic_args, ..
            } => {
                self.resolve_name_to_edge(*name, from_node, module_idx);
                for ty in *generic_args {
                    self.add_ast_type_dep(from_node, ty, module_idx, pool);
                }
            }
            Expr::Call {
                callee,
                generic_args,
                arguments,
                ..
            } => {
                self.walk_expr(callee, from_node, module_idx, pool);
                for ty in *generic_args {
                    self.add_ast_type_dep(from_node, ty, module_idx, pool);
                }
                for arg in *arguments {
                    self.walk_expr(arg, from_node, module_idx, pool);
                }
            }
            Expr::StructInit {
                callee, arguments, ..
            } => {
                self.walk_expr(callee, from_node, module_idx, pool);
                for arg in *arguments {
                    self.walk_expr(arg, from_node, module_idx, pool);
                }
            }
            Expr::FieldAccess { object, .. } | Expr::Get { object, .. } => {
                self.walk_expr(object, from_node, module_idx, pool);
            }
            Expr::Binary { left, right, .. }
            | Expr::Comparison {
                lhs: left,
                rhs: right,
                ..
            } => {
                self.walk_expr(left, from_node, module_idx, pool);
                self.walk_expr(right, from_node, module_idx, pool);
            }
            Expr::Assignment { lhs, rhs, .. } => {
                self.walk_expr(lhs, from_node, module_idx, pool);
                self.walk_expr(rhs, from_node, module_idx, pool);
            }
            Expr::Unary { operand, .. } => {
                self.walk_expr(operand, from_node, module_idx, pool);
            }
            Expr::ArrayIndex { expr, index, .. } => {
                self.walk_expr(expr, from_node, module_idx, pool);
                self.walk_expr(index, from_node, module_idx, pool);
            }
            Expr::FieldInit { expr, .. } => {
                self.walk_expr(expr, from_node, module_idx, pool);
            }
            Expr::ExprList { expressions, .. } => {
                for e in *expressions {
                    self.walk_expr(e, from_node, module_idx, pool);
                }
            }
            Expr::If { if_stmt, .. } => {
                self.walk_expr(if_stmt.condition, from_node, module_idx, pool);
                self.walk_block(if_stmt.then_branch, from_node, module_idx, pool);
                if let Some(eb) = if_stmt.else_branch {
                    self.walk_else_branch(eb, from_node, module_idx, pool);
                }
            }
            Expr::Match { match_stmt, .. } => {
                self.walk_expr(match_stmt.expr, from_node, module_idx, pool);
                for arm in match_stmt.arms {
                    if let Some(g) = arm.guard {
                        self.walk_expr(g, from_node, module_idx, pool);
                    }
                    self.walk_block(arm.block, from_node, module_idx, pool);
                }
            }
            Expr::ElseExpr { expr, pattern, .. } => {
                self.walk_expr(expr, from_node, module_idx, pool);
                match pattern {
                    ErrorHandlerPattern::Single { body, .. } => {
                        self.walk_block(body, from_node, module_idx, pool);
                    }
                    ErrorHandlerPattern::Multiple { branches } => {
                        for branch in *branches {
                            self.walk_block(branch.body, from_node, module_idx, pool);
                        }
                    }
                }
            }
            Expr::Deref { expr, .. } | Expr::Ref { expr, .. } => {
                self.walk_expr(expr, from_node, module_idx, pool);
            }
            Expr::Null { .. }
            | Expr::Number { .. }
            | Expr::Decimal { .. }
            | Expr::String { .. }
            | Expr::Boolean { .. }
            | Expr::Char { .. }
            | Expr::This { .. } => {}
            Expr::Lambda {
                params,
                return_type,
                body,
                ..
            } => {
                for p in *params {
                    if let Some(ty) = &p.type_annotation {
                        self.add_ast_type_dep(from_node, ty, module_idx, pool);
                    }
                }
                if let Some(ret) = return_type {
                    self.add_ast_type_dep(from_node, ret, module_idx, pool);
                }
                self.walk_block(body, from_node, module_idx, pool);
            }
            Expr::ModulePath { segments, .. } => {
                self.add_module_path_dep(from_node, segments);
            }

            Expr::ModuleAccess { segments, .. } => {
                // `member` itself isn't resolved here, that requires knowing whether
                // it names a type, function, or static field inside the target module,
                // which is symbol-table-across-modules work better left to the
                // type-checker. The module-level edge is still useful for compilation
                // ordering and import-cycle detection, so record it.
                self.add_module_path_dep(from_node, segments);
            }
            Expr::ArrayLiteral { elements, span: _ } => {
                for element in *elements {
                    self.walk_expr(element, from_node, module_idx, pool);
                }
            }
            Expr::Undefined { .. } => {
                // Nothing to do.
            }
        }
    }

    /// Best-effort fine-grained edge: if `segments` resolves to a known module
    /// via the path index, add a direct edge from `from_node` to that module's
    /// node. This is additive to the blanket per-file import edges created by
    /// `Stmt::Import`, it lets `get_function_callees`-style queries see which
    /// specific declarations reference a module, not just which files import it.
    fn add_module_path_dep(&mut self, from_node: NodeIdx, segments: &[StrId]) {
        let Some(target_module_idx) = self.path_index.resolve(segments) else {
            return;
        };
        let target_node = self.get_or_create_module_node(target_module_idx);
        self.add_edge(from_node, target_node);
    }

    /// Add an edge from `from_node` to whatever `ty` names, if it resolves to
    /// a known declaration.
    fn add_ast_type_dep<'a, 'bump>(
        &mut self,
        from_node: NodeIdx,
        ty: &Type<'a, 'bump>,
        module_idx: usize,
        pool: &StringPool,
    ) {
        match ty.kind {
            TypeKind::Struct { name, generics } => {
                self.resolve_name_to_edge(name, from_node, module_idx);
                for g in generics {
                    self.add_ast_type_dep(from_node, g, module_idx, pool);
                }
            }
            TypeKind::SafePointer { inner, .. }
            | TypeKind::UnsafePointer { inner, .. }
            | TypeKind::Array { inner, .. }
            | TypeKind::Slice { inner }
            | TypeKind::Ref { inner, .. } => {
                self.add_ast_type_dep(from_node, inner, module_idx, pool);
            }
            TypeKind::Lambda {
                params,
                return_type,
            } => {
                for p in params {
                    self.add_ast_type_dep(from_node, p, module_idx, pool);
                }
                self.add_ast_type_dep(from_node, return_type, module_idx, pool);
            }
            TypeKind::Dyn { bounds } => {
                for b in bounds {
                    self.add_ast_type_dep(from_node, b, module_idx, pool);
                }
            }
            // Primitive / infer / void / this, no named dependency.
            _ => {}
        }
    }

    /// Try to resolve a bare `StrId` name to a declaration node and add an
    /// edge.  Searches the current module first, then any imported modules.
    fn resolve_name_to_edge(&mut self, name: StrId, from_node: NodeIdx, module_idx: usize) {
        if let Some(&(m, i, tag)) = self.symbol_table.get(&(name, module_idx)) {
            if let Some(to) = self.lookup_item_node(m, i, tag) {
                self.add_edge(from_node, to);
                return;
            }
        }
        let imports = self.get_module_imports(module_idx);
        for imp_idx in imports {
            if let Some(&(m, i, tag)) = self.symbol_table.get(&(name, imp_idx)) {
                if let Some(to) = self.lookup_item_node(m, i, tag) {
                    self.add_edge(from_node, to);
                    return;
                }
            }
        }
    }

    pub fn register_import(&mut self, current_module_idx: usize, imported_module_idx: usize) {
        let cur = self.get_or_create_module_node(current_module_idx);
        let imp = self.get_or_create_module_node(imported_module_idx);
        self.add_edge(cur, imp);
    }

    fn get_or_create_module_node(&mut self, module_idx: usize) -> NodeIdx {
        if let Some(idx) = self.lookup_item_node(module_idx, 0, "module") {
            return idx;
        }
        let idx = self.push_node(NodeKind::Module { module_idx }, None);
        self.register_item_node(module_idx, 0, "module", idx);
        idx
    }

    pub fn register_package(
        &mut self,
        module_idx: usize,
        path_segments: Vec<StrId>,
        path_strid: StrId,
    ) {
        self.package_hierarchy.insert(module_idx, path_strid);
        self.path_index.insert(path_segments, module_idx);
        if let Some(node_idx) = self.lookup_item_node(module_idx, 0, "module") {
            if let Some(node) = self.nodes.get_mut(node_idx) {
                node.hint = Some(path_strid);
            }
        }
    }

    /// Link stdlib modules to user modules: user code gets an implicit import
    /// edge to each stdlib module.
    pub fn link_stdlib_to_user(&mut self, stdlib_module_idx: usize, user_module_indices: &[usize]) {
        for &user_idx in user_module_indices {
            self.register_import(user_idx, stdlib_module_idx);
        }
    }

    pub fn get_module_imports(&self, module_idx: usize) -> Vec<usize> {
        let Some(node_idx) = self.lookup_item_node(module_idx, 0, "module") else {
            return Vec::new();
        };
        let Some(node) = self.nodes.get(node_idx) else {
            return Vec::new();
        };
        node.deps
            .iter()
            .filter_map(|&dep_idx| {
                let dep = self.nodes.get(dep_idx)?;
                if let NodeKind::Module { module_idx: imp } = dep.kind {
                    Some(imp)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_module_importers(&self, module_idx: usize) -> Vec<usize> {
        let Some(node_idx) = self.lookup_item_node(module_idx, 0, "module") else {
            return Vec::new();
        };
        let Some(node) = self.nodes.get(node_idx) else {
            return Vec::new();
        };
        node.rev_deps
            .iter()
            .filter_map(|&rev_idx| {
                let rev = self.nodes.get(rev_idx)?;
                if let NodeKind::Module { module_idx: imp } = rev.kind {
                    Some(imp)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_module_package(&self, module_idx: usize) -> Option<StrId> {
        self.package_hierarchy.get(&module_idx).copied()
    }

    pub fn build_symbol_table(&self) -> HashMap<(StrId, usize), (usize, usize, &'static str)> {
        let mut table: HashMap<(StrId, usize), (usize, usize, &'static str)> = HashMap::default();
        for (&(module_idx, item_idx, tag), &node_idx) in &self.item_index {
            let Some(node) = self.nodes.get(node_idx) else {
                continue;
            };
            let Some(hint) = node.hint else { continue };
            table.insert((hint, module_idx), (module_idx, item_idx, tag));
        }
        table
    }

    pub fn resolve_name(
        &self,
        name: StrId,
        current_module_idx: usize,
        symbol_table: &HashMap<(StrId, usize), (usize, usize, &'static str)>,
    ) -> Option<NodeIdx> {
        if let Some(&(m, i, tag)) = symbol_table.get(&(name, current_module_idx)) {
            return self.lookup_item_node(m, i, tag);
        }
        for imp_idx in self.get_module_imports(current_module_idx) {
            if let Some(&(m, i, tag)) = symbol_table.get(&(name, imp_idx)) {
                return self.lookup_item_node(m, i, tag);
            }
        }
        None
    }

    pub fn tarjan_scc(&self) -> Vec<Vec<NodeIdx>> {
        let n = self.nodes.len();
        let mut index: Vec<Option<usize>> = vec![None; n];
        let mut lowlink: Vec<usize> = vec![0; n];
        let mut onstack: Vec<bool> = vec![false; n];
        let mut stack: Vec<NodeIdx> = Vec::new();
        let mut counter = 0usize;
        let mut sccs: Vec<Vec<NodeIdx>> = Vec::new();

        for v in 0..n {
            if index[v].is_none() {
                strongconnect(
                    v,
                    &mut index,
                    &mut lowlink,
                    &mut onstack,
                    &mut stack,
                    &mut counter,
                    &self.nodes,
                    &mut sccs,
                );
            }
        }
        sccs
    }

    pub fn scc_topo_order(&self) -> Vec<Vec<NodeIdx>> {
        let sccs = self.tarjan_scc();
        let n = self.nodes.len();
        let mut node_to_scc = vec![0usize; n];
        for (sidx, comp) in sccs.iter().enumerate() {
            for &v in comp {
                node_to_scc[v] = sidx;
            }
        }

        let scc_count = sccs.len();
        let mut scc_adj: Vec<HashSet<usize>> = vec![HashSet::default(); scc_count];
        let mut indeg: Vec<usize> = vec![0usize; scc_count];

        for (u, node) in self.nodes.iter().enumerate() {
            let su = node_to_scc[u];
            for &v in &node.deps {
                let sv = node_to_scc[v];
                if su == sv {
                    continue;
                }
                if scc_adj[su].insert(sv) {
                    indeg[sv] += 1;
                }
            }
        }

        let mut q: VecDeque<usize> = (0..scc_count).filter(|&s| indeg[s] == 0).collect();
        let mut ordered: Vec<Vec<NodeIdx>> = Vec::with_capacity(scc_count);
        while let Some(s) = q.pop_front() {
            ordered.push(sccs[s].clone());
            for &nbr in &scc_adj[s] {
                indeg[nbr] -= 1;
                if indeg[nbr] == 0 {
                    q.push_back(nbr);
                }
            }
        }
        ordered
    }

    pub fn detect_circular_imports(&self) -> Vec<Vec<usize>> {
        self.tarjan_scc()
            .into_iter()
            .filter_map(|scc| {
                let module_indices: Vec<usize> = scc
                    .iter()
                    .filter_map(|&ni| {
                        let node = self.nodes.get(ni)?;
                        if let NodeKind::Module { module_idx } = node.kind {
                            // multi-node SCC = definite cycle; single-node =
                            // cycle only if self-loop present
                            if scc.len() > 1 || node.deps.contains(&ni) {
                                Some(module_idx)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();
                if module_indices.is_empty() {
                    None
                } else {
                    Some(module_indices)
                }
            })
            .collect()
    }

    pub fn detect_recursive_cycles(&self) -> Vec<Vec<(usize, usize)>> {
        self.tarjan_scc()
            .into_iter()
            .filter_map(|scc| {
                let func_refs: Vec<(usize, usize)> = scc
                    .iter()
                    .filter_map(|&ni| {
                        let node = self.nodes.get(ni)?;
                        if let NodeKind::FuncBody {
                            module_idx,
                            item_idx,
                        } = node.kind
                        {
                            if scc.len() > 1 || node.deps.contains(&ni) {
                                Some((module_idx, item_idx))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();
                if func_refs.is_empty() {
                    None
                } else {
                    Some(func_refs)
                }
            })
            .collect()
    }

    pub fn get_compilation_order(&self) -> Vec<Vec<(usize, usize)>> {
        self.scc_topo_order()
            .iter()
            .rev()
            .filter_map(|scc| {
                let refs: Vec<(usize, usize)> = scc
                    .iter()
                    .filter_map(|&ni| {
                        let node = self.nodes.get(ni)?;
                        if let NodeKind::FuncBody {
                            module_idx,
                            item_idx,
                        } = node.kind
                        {
                            Some((module_idx, item_idx))
                        } else {
                            None
                        }
                    })
                    .collect();
                if refs.is_empty() { None } else { Some(refs) }
            })
            .collect()
    }

    pub fn get_module_compilation_order(&self) -> Vec<usize> {
        let mut seen: HashSet<usize> = HashSet::default();
        self.scc_topo_order()
            .iter()
            .rev()
            .flat_map(|scc| scc.iter().copied())
            .filter_map(|ni| {
                let node = self.nodes.get(ni)?;
                if let NodeKind::Module { module_idx } = node.kind {
                    if seen.insert(module_idx) {
                        Some(module_idx)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_function_callers(
        &self,
        target_module: usize,
        target_item: usize,
    ) -> Vec<(usize, usize)> {
        let Some(target_node) = self.lookup_item_node(target_module, target_item, "func_body")
        else {
            return Vec::new();
        };
        let Some(node) = self.nodes.get(target_node) else {
            return Vec::new();
        };
        node.rev_deps
            .iter()
            .filter_map(|&ri| {
                let rn = self.nodes.get(ri)?;
                if let NodeKind::FuncBody {
                    module_idx,
                    item_idx,
                } = rn.kind
                {
                    Some((module_idx, item_idx))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_function_callees(
        &self,
        caller_module: usize,
        caller_item: usize,
    ) -> Vec<(usize, usize)> {
        let Some(caller_node) = self.lookup_item_node(caller_module, caller_item, "func_body")
        else {
            return Vec::new();
        };
        let Some(node) = self.nodes.get(caller_node) else {
            return Vec::new();
        };
        node.deps
            .iter()
            .filter_map(|&di| {
                let dn = self.nodes.get(di)?;
                if let NodeKind::FuncBody {
                    module_idx,
                    item_idx,
                } = dn.kind
                {
                    Some((module_idx, item_idx))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_function_type_deps(
        &self,
        module_idx: usize,
        item_idx: usize,
    ) -> Vec<(usize, usize)> {
        let Some(sig_node) = self.lookup_item_node(module_idx, item_idx, "func_sig") else {
            return Vec::new();
        };
        let Some(node) = self.nodes.get(sig_node) else {
            return Vec::new();
        };
        node.deps
            .iter()
            .filter_map(|&di| {
                let dn = self.nodes.get(di)?;
                if let NodeKind::TypeDecl {
                    module_idx: m,
                    item_idx: i,
                } = dn.kind
                {
                    Some((m, i))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn can_access(
        &self,
        from_module: usize,
        target_module: usize,
        _target_item_idx: usize,
    ) -> bool {
        from_module == target_module
            || self
                .get_module_imports(from_module)
                .contains(&target_module)
    }

    pub fn debug_node(&self, node_idx: NodeIdx, pool: &StringPool) -> String {
        let node = &self.nodes[node_idx];
        let kind_str = match &node.kind {
            NodeKind::Module { module_idx } => format!("Module({})", module_idx),
            NodeKind::TypeDecl {
                module_idx,
                item_idx,
            } => {
                format!("TypeDecl(m{}:i{})", module_idx, item_idx)
            }
            NodeKind::FuncSig {
                module_idx,
                item_idx,
            } => {
                format!("FuncSig(m{}:i{})", module_idx, item_idx)
            }
            NodeKind::FuncBody {
                module_idx,
                item_idx,
            } => {
                format!("FuncBody(m{}:i{})", module_idx, item_idx)
            }
            NodeKind::ConstDecl {
                module_idx,
                item_idx,
            } => {
                format!("Const(m{}:i{})", module_idx, item_idx)
            }
            NodeKind::TraitDecl {
                module_idx,
                item_idx,
            } => {
                format!("Trait(m{}:i{})", module_idx, item_idx)
            }
            NodeKind::TraitImpl {
                module_idx,
                item_idx,
            } => {
                format!("Impl(m{}:i{})", module_idx, item_idx)
            }
        };
        let hint_str = node
            .hint
            .map(|s| pool.resolve_string(&s).to_string())
            .unwrap_or_else(|| "<no-hint>".into());
        format!(
            "Node[{}] {} hint={} -> deps={:?}",
            node.idx, kind_str, hint_str, node.deps
        )
    }
}

fn strongconnect(
    v: NodeIdx,
    index: &mut Vec<Option<usize>>,
    lowlink: &mut Vec<usize>,
    onstack: &mut Vec<bool>,
    stack: &mut Vec<NodeIdx>,
    counter: &mut usize,
    nodes: &[DepNode],
    sccs: &mut Vec<Vec<NodeIdx>>,
) {
    index[v] = Some(*counter);
    lowlink[v] = *counter;
    *counter += 1;
    stack.push(v);
    onstack[v] = true;

    for &w in &nodes[v].deps {
        if index[w].is_none() {
            strongconnect(w, index, lowlink, onstack, stack, counter, nodes, sccs);
            lowlink[v] = lowlink[v].min(lowlink[w]);
        } else if onstack[w] {
            lowlink[v] = lowlink[v].min(index[w].unwrap());
        }
    }

    if lowlink[v] == index[v].unwrap() {
        let mut component = Vec::new();
        loop {
            let w = stack.pop().expect("tarjan: stack underflow");
            onstack[w] = false;
            component.push(w);
            if w == v {
                break;
            }
        }
        sccs.push(component);
    }
}

fn path_to_strid<'a, 'bump>(path: &Path<'a, 'bump>, pool: &StringPool) -> StrId {
    // e.g. ["zeta", "io", "files"] -> "zeta::io::files"
    let joined = path
        .path
        .iter()
        .map(|s| pool.resolve_string(s))
        .collect::<Vec<_>>()
        .join("_");
    StrId(pool.intern(&joined))
}
