use std::collections::{VecDeque};
use ir::ir_hasher::{HashMap, HashSet};

use ir::hir::{Hir, HirModule, StrId};
use zetaruntime::string_pool::StringPool;

pub type NodeIdx = usize;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeKey {
    pub repr: StrId,
}

/// Distinguish node kinds for lookups and classification.
#[derive(Clone, Debug)]
pub enum NodeKind {
    Module { module_idx: usize },
    TypeDecl { module_idx: usize, item_idx: usize },   // struct/enum/interface
    FuncSig { module_idx: usize, item_idx: usize },    // signature (params/returns)
    FuncBody { module_idx: usize, item_idx: usize },   // body
    ConstDecl { module_idx: usize, item_idx: usize },
    TraitDecl { module_idx: usize, item_idx: usize },
    TraitImpl { module_idx: usize, item_idx: usize },
    GenericTypeInstance { base: NodeIdx, args: Vec<TypeKey> },
    GenericFnInstance { base: NodeIdx, args: Vec<TypeKey> },
}

impl NodeKind {
    pub fn is_func_body(&self) -> bool {
        matches!(self, NodeKind::FuncBody { .. })
    }
    pub fn is_type_decl(&self) -> bool {
        matches!(self, NodeKind::TypeDecl { .. })
    }
    pub fn is_generic_instance(&self) -> bool {
        matches!(self, NodeKind::GenericTypeInstance { .. } | NodeKind::GenericFnInstance { .. })
    }
}

/// Single node in the dependency graph.
#[derive(Debug)]
pub struct DepNode {
    pub idx: NodeIdx,
    pub kind: NodeKind,
    pub hint: Option<StrId>,
    pub deps: Vec<NodeIdx>,  // outgoing edges: this -> dependency
    pub rev_deps: Vec<NodeIdx>, // reverse edges
}

impl DepNode {
    pub fn new(idx: NodeIdx, kind: NodeKind, hint: Option<StrId>) -> Self {
        DepNode {
            idx,
            kind,
            hint,
            deps: Vec::new(),
            rev_deps: Vec::new(),
        }
    }
}

/// Key used to intern created generic instances.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstanceKey {
    pub base: NodeIdx,
    pub args: Vec<TypeKey>,
}

/// The semantic dependency graph container.
pub struct DepGraph {
    nodes: Vec<DepNode>,
    /// fast lookup: (module_idx, item_idx, tag) -> NodeIdx
    item_index: HashMap<(usize, usize, &'static str), NodeIdx>,
    /// instances map: (base, args) -> node
    instance_map: HashMap<InstanceKey, NodeIdx>,
    /// instantiation queue
    inst_queue: VecDeque<InstanceKey>,
    /// symbol table: (name, module_idx) -> (module_idx, item_idx, tag)
    symbol_table: HashMap<(StrId, usize), (usize, usize, &'static str)>,
    /// package hierarchy: module_idx -> package_path
    package_hierarchy: HashMap<usize, StrId>,
}

impl DepGraph {
    pub fn new() -> Self {
        DepGraph {
            nodes: Vec::new(),
            item_index: HashMap::default(),
            instance_map: HashMap::default(),
            inst_queue: VecDeque::new(),
            symbol_table: HashMap::default(),
            package_hierarchy: HashMap::default(),
        }
    }

    /// Create a new node and return its index
    pub fn push_node(&mut self, kind: NodeKind, hint: Option<StrId>) -> NodeIdx {
        let idx = self.nodes.len();
        let node = DepNode::new(idx, kind, hint);
        self.nodes.push(node);
        idx
    }

    /// Register mapping for a declaration-like node so we can resolve later.
    pub fn register_item_node(&mut self, module_idx: usize, item_idx: usize, tag: &'static str, node_idx: NodeIdx) {
        self.item_index.insert((module_idx, item_idx, tag), node_idx);
    }

    /// Lookup existing item node produced in Phase A.
    pub fn lookup_item_node(&self, module_idx: usize, item_idx: usize, tag: &'static str) -> Option<NodeIdx> {
        self.item_index.get(&(module_idx, item_idx, tag)).copied()
    }

    /// Add an edge a -> b (a depends on b)
    pub fn add_edge(&mut self, a: NodeIdx, b: NodeIdx) {
        if a >= self.nodes.len() || b >= self.nodes.len() { return; }

        if !self.nodes[a].deps.contains(&b) {
            self.nodes[a].deps.push(b);
        }

        if !self.nodes[b].rev_deps.contains(&a) {
            self.nodes[b].rev_deps.push(a);
        }
    }

    /// Create or get an instance node for (base, args). If newly created, queue it for work.
    pub fn get_or_create_instance(&mut self, base: NodeIdx, args: Vec<TypeKey>, is_fn: bool) -> NodeIdx {
        let key = InstanceKey { base, args: args.clone() };
        if let Some(&idx) = self.instance_map.get(&key) {
            return idx;
        }
        let kind = if is_fn {
            NodeKind::GenericFnInstance { base, args: args.clone() }
        } else {
            NodeKind::GenericTypeInstance { base, args: args.clone() }
        };
        let hint = None;
        let idx = self.push_node(kind, hint);
        self.instance_map.insert(key.clone(), idx);
        self.inst_queue.push_back(key);
        idx
    }

    /// Drain instantiation queue and create edges for instantiated items.
    ///
    /// NOTE: This function contains the minimal skeleton. To be useful you must:
    /// - implement `instantiate_generic_body` to produce concrete edges from the generic body
    /// - implement resolution from type args to TypeDecl nodes
    pub fn drain_instantiation_queue<F>(&mut self, mut instantiate_generic_body: F)
    where
        F: FnMut(&InstanceKey, &mut DepGraph),
    {
        while let Some(key) = self.inst_queue.pop_front() {
            // find node idx
            if let Some(&node_idx) = self.instance_map.get(&key) {
                // call out to user-provided function to create edges for this instance
                instantiate_generic_body(&key, self);
                // ensure the instance node depends on base generic
                self.add_edge(node_idx, key.base);
                // also add edges to TypeDecl nodes for each type arg if resolver is available
                // (instantiate_generic_body likely does that)
            }
        }
    }

    /// Build graph nodes (Phase A) from HIR modules. This only creates nodes and populates `item_index`.
    /// It does NOT yet inspect bodies for edges.
    pub fn phase_a_create_nodes(&mut self, modules: &Vec<HirModule>) {
        for (midx, module) in modules.iter().enumerate() {
            let module_node = self.push_node(
                NodeKind::Module { module_idx: midx }, 
                Some(module.name)
            );
            
            let _ = module_node;

            // iterate items and create nodes per top-level item
            for (item_idx, item) in module.items.iter().enumerate() {
                match item {
                    Hir::Func(f) => {
                        // function signature node
                        let sig_node = self.push_node(
                            NodeKind::FuncSig { module_idx: midx, item_idx },
                            Some(f.name),
                        );
                        self.register_item_node(midx, item_idx, "func_sig", sig_node);

                        // function body node
                        let body_node = self.push_node(
                            NodeKind::FuncBody { module_idx: midx, item_idx },
                            Some(f.name),
                        );
                        self.register_item_node(midx, item_idx, "func_body", body_node);
                    }
                    Hir::Struct(s) => {
                        let td = self.push_node(
                            NodeKind::TypeDecl { module_idx: midx, item_idx },
                            Some(s.name),
                        );
                        self.register_item_node(midx, item_idx, "type", td);
                    }
                    Hir::Enum(e) => {
                        let td = self.push_node(
                            NodeKind::TypeDecl { module_idx: midx, item_idx },
                            Some(e.name),
                        );
                        self.register_item_node(midx, item_idx, "type", td);
                    }
                    Hir::Const(c) => {
                        let cd = self.push_node(
                            NodeKind::ConstDecl { module_idx: midx, item_idx },
                            Some(c.name),
                        );
                        self.register_item_node(midx, item_idx, "const", cd);
                    }
                    Hir::Interface(t) => {
                        let td = self.push_node(
                            NodeKind::TraitDecl { module_idx: midx, item_idx },
                            Some(t.name),
                        );
                        self.register_item_node(midx, item_idx, "trait", td);
                    }
                    Hir::Impl(_i) => {
                        let id = self.push_node(
                            NodeKind::TraitImpl { module_idx: midx, item_idx },
                            None,
                        );
                        self.register_item_node(midx, item_idx, "impl", id);
                    }
                    _ => {
                        // other top-level items - treat as unknown for now
                        let _id = self.push_node(NodeKind::ConstDecl { module_idx: midx, item_idx }, None);
                        // don't register by default
                    }
                }
            }
        }
    }

    pub fn phase_b_extract_edges<
        RN,
        RT,
        WSig,
        WBody,
    >(
        &mut self,
        modules: &Vec<HirModule>,
        _resolve_name: RN,
        resolve_typekey: RT,
        mut walk_signature_fn: WSig,
        mut walk_body_fn: WBody,
    )
    where
        RN: Fn(StrId, usize) -> Option<(usize, usize, &'static str)>,
        RT: Fn(&TypeKey, usize) -> Option<NodeIdx>,
        WSig: FnMut(&Hir, NodeIdx, &mut DepGraph, usize),
        WBody: FnMut(&Hir, NodeIdx, &mut DepGraph, usize, &mut dyn FnMut(NodeIdx, Vec<TypeKey>, bool)),
    {
        // queue is used even when not necessary to avoid borrowck issues
        let mut inst_queue: Vec<(NodeIdx, Vec<TypeKey>, bool)> = Vec::new();

        for (midx, module) in modules.iter().enumerate() {
            for (item_idx, item) in module.items.iter().enumerate() {
                if let Some(sig_node) = self.lookup_item_node(midx, item_idx, "func_sig") {
                    walk_signature_fn(item, sig_node, self, midx);
                }

                if let Some(body_node) = self.lookup_item_node(midx, item_idx, "func_body") {
                    // enqueue pushes into a queue instead of mutably borrowing self
                    let mut enqueue_instance = |base, args, is_fn| {
                        inst_queue.push((base, args, is_fn));
                    };
                    walk_body_fn(item, body_node, self, midx, &mut enqueue_instance);
                }

                if let Some(type_node) = self.lookup_item_node(midx, item_idx, "type") {
                    let _ = (type_node, &resolve_typekey);
                }
            }
        }

        // process instantiations after everything else
        for (base, args, is_fn) in inst_queue {
            let _ = self.get_or_create_instance(base, args, is_fn);
        }
    }

    /// Run Tarjan SCC and return components as Vec<Vec<NodeIdx>> (each Vec is a SCC).
    pub fn tarjan_scc(&self) -> Vec<Vec<NodeIdx>> {
        let n = self.nodes.len();
        let mut index: Vec<Option<usize>> = vec![None; n];
        let mut lowlink: Vec<usize> = vec![0; n];
        let mut onstack: Vec<bool> = vec![false; n];
        let mut stack: Vec<NodeIdx> = Vec::new();
        let mut indices = 0usize;
        let mut sccs: Vec<Vec<NodeIdx>> = Vec::new();

        fn strongconnect(
            v: NodeIdx,
            index: &mut Vec<Option<usize>>,
            lowlink: &mut Vec<usize>,
            onstack: &mut Vec<bool>,
            stack: &mut Vec<NodeIdx>,
            indices: &mut usize,
            nodes: &Vec<DepNode>,
            sccs: &mut Vec<Vec<NodeIdx>>,
        ) {
            index[v] = Some(*indices);
            lowlink[v] = *indices;
            *indices += 1;
            stack.push(v);
            onstack[v] = true;

            for &w in &nodes[v].deps {
                if index[w].is_none() {
                    strongconnect(w, index, lowlink, onstack, stack, indices, nodes, sccs);
                    lowlink[v] = std::cmp::min(lowlink[v], lowlink[w]);
                } else if onstack[w] {
                    let wi = index[w].unwrap();
                    lowlink[v] = std::cmp::min(lowlink[v], wi);
                }
            }

            if let Some(iv) = index[v] {
                if lowlink[v] == iv {
                    // start a new SCC
                    let mut component = Vec::new();
                    loop {
                        let w = stack.pop().expect("stack underflow in tarjan");
                        onstack[w] = false;
                        component.push(w);
                        if w == v { break; }
                    }
                    sccs.push(component);
                }
            }
        }

        for v in 0..n {
            if index[v].is_none() {
                strongconnect(
                    v,
                    &mut index,
                    &mut lowlink,
                    &mut onstack,
                    &mut stack,
                    &mut indices,
                    &self.nodes,
                    &mut sccs,
                );
            }
        }

        sccs
    }

    /// Build SCC DAG: collapse SCCs into nodes, provide topological order of SCCs.
    pub fn scc_topo_order(&self) -> Vec<Vec<NodeIdx>> {
        let sccs: Vec<Vec<NodeIdx>> = self.tarjan_scc();
        // map node -> scc_idx
        let mut node_to_scc: Vec<usize> = vec![0usize; self.nodes.len()];
        for (sidx, comp) in sccs.iter().enumerate() {
            for &n in comp {
                node_to_scc[n] = sidx;
            }
        }

        // build scc adjacency
        let scc_count = sccs.len();
        let mut scc_adj: Vec<HashSet<usize>> = vec![HashSet::default(); scc_count];
        let mut indeg: Vec<usize> = vec![0usize; scc_count];

        for (u, node) in self.nodes.iter().enumerate() {
            let su = node_to_scc[u];
            for &v in &node.deps {
                let sv = node_to_scc[v];
                if su == sv { continue; };
                if scc_adj[su].insert(sv) { indeg[sv] += 1; }
            }
        }

        // Kahn topo on SCC graph
        let mut q: VecDeque<usize> = VecDeque::new();
        for s in 0..scc_count {
            if indeg[s] == 0 { q.push_back(s); }
        }

        let mut ordered_sccs: Vec<Vec<NodeIdx>> = Vec::with_capacity(scc_count);
        while let Some(s) = q.pop_front() {
            ordered_sccs.push(sccs[s].clone());
            for &nbr in scc_adj[s].iter() {
                indeg[nbr] -= 1;
                if indeg[nbr] == 0 { q.push_back(nbr); }
            }
        }

        ordered_sccs
    }

    /// Public access to nodes for read-only use.
    pub fn nodes(&self) -> &Vec<DepNode> {
        &self.nodes
    }

    /// Convenience: pretty-print a node for debugging diagnostics (requires StringPool to turn StrId -> &str)
    pub fn debug_node(&self, node_idx: NodeIdx, pool: &StringPool) -> String {
        let node = &self.nodes[node_idx];
        let kind_str = match &node.kind {
            NodeKind::Module { module_idx } => format!("Module({})", module_idx),
            NodeKind::TypeDecl { module_idx, item_idx } => format!("TypeDecl(m{}:i{})", module_idx, item_idx),
            NodeKind::FuncSig { module_idx, item_idx } => format!("FuncSig(m{}:i{})", module_idx, item_idx),
            NodeKind::FuncBody { module_idx, item_idx } => format!("FuncBody(m{}:i{})", module_idx, item_idx),
            NodeKind::ConstDecl { module_idx, item_idx } => format!("Const(m{}:i{})", module_idx, item_idx),
            NodeKind::TraitDecl { module_idx, item_idx } => format!("Trait(m{}:i{})", module_idx, item_idx),
            NodeKind::TraitImpl { module_idx, item_idx } => format!("Impl(m{}:i{})", module_idx, item_idx),
            NodeKind::GenericTypeInstance { base, args } => format!("GenericTypeInst(base={}, args={:?})", base, args),
            NodeKind::GenericFnInstance { base, args } => format!("GenericFnInst(base={}, args={:?})", base, args),
        };
        let hint_str = node.hint.map(|s| pool.resolve_string(&s).to_string()).unwrap_or_else(|| "<no-hint>".into());
        format!("Node[{}] {} hint={} -> deps={:?}", node.idx, kind_str, hint_str, node.deps)
    }

    /// Register an import dependency: current_module imports imported_module
    /// This creates an edge from current_module to imported_module in the dependency graph
    pub fn register_import(&mut self, current_module_idx: usize, imported_module_idx: usize) {
        // Create module nodes if they don't exist
        let current_module_node: NodeIdx = self.lookup_item_node(current_module_idx, 0, "module")
            .unwrap_or_else(|| {
                let idx = self.push_node(NodeKind::Module { module_idx: current_module_idx }, None);
                self.register_item_node(current_module_idx, 0, "module", idx);
                idx
            });

        let imported_module_node: NodeIdx = self.lookup_item_node(imported_module_idx, 0, "module")
            .unwrap_or_else(|| {
                let idx = self.push_node(NodeKind::Module { module_idx: imported_module_idx }, None);
                self.register_item_node(imported_module_idx, 0, "module", idx);
                idx
            });

        // Add edge: current_module depends on imported_module
        self.add_edge(current_module_node, imported_module_node);
    }

    /// Register a package dependency: module belongs to package
    /// This creates a package hierarchy for namespace resolution
    pub fn register_package(&mut self, module_idx: usize, package_path: StrId, _pool: &StringPool) {
        self.package_hierarchy.insert(module_idx, package_path);
        
        // Also set the hint on the module node for diagnostics
        if let Some(module_node_idx) = self.lookup_item_node(module_idx, 0, "module") {
            if let Some(node) = self.nodes.get_mut(module_node_idx) {
                node.hint = Some(package_path);
            }
        }
    }
    
    /// Get the package path for a module
    pub fn get_module_package(&self, module_idx: usize) -> Option<&str> {
        self.package_hierarchy.get(&module_idx).map(|s| s.as_str())
    }

    /// Get all modules that a given module imports
    pub fn get_module_imports(&self, module_idx: usize) -> Vec<usize> {
        let mut imports: Vec<usize> = Vec::new();

        let Some(module_node_idx) = self.lookup_item_node(module_idx, 0, "module") else { return imports; };
        let Some(node) = self.nodes.get(module_node_idx) else { return imports; };

        for &dep_idx in &node.deps {
            let Some(dep_node) = self.nodes.get(dep_idx) else { continue; };
            let NodeKind::Module { module_idx: imported_idx } = dep_node.kind else { continue; };
            imports.push(imported_idx);
        }
        
        imports
    }

    /// Get all modules that import a given module (reverse dependencies)
    pub fn get_module_importers(&self, module_idx: usize) -> Vec<usize> {
        let mut importers = Vec::new();

        let Some(module_node_idx) = self.lookup_item_node(module_idx, 0, "module") else { return importers; };
        let Some(node) = self.nodes.get(module_node_idx) else { return importers; };

        for &rev_dep_idx in &node.rev_deps {
            let Some(rev_dep_node) = self.nodes.get(rev_dep_idx) else { continue; };
            let NodeKind::Module { module_idx: importer_idx } = rev_dep_node.kind else { continue; };
            importers.push(importer_idx);
        }
        
        importers
    }

    /// Detect circular imports: returns Vec of cycles (each cycle is a Vec of module indices)
    pub fn detect_circular_imports(&self) -> Vec<Vec<usize>> {
        let sccs: Vec<Vec<NodeIdx>> = self.tarjan_scc();
        let mut cycles: Vec<Vec<usize>> = Vec::new();

        for scc in sccs {
            // An SCC with more than one node is a cycle
            // Or a single node with a self-loop is also a cycle
            if scc.len() > 1 {
                let mut module_indices = Vec::new();
                for &node_idx in &scc {
                    if let Some(node) = self.nodes.get(node_idx) {
                        if let NodeKind::Module { module_idx } = node.kind {
                            module_indices.push(module_idx);
                        }
                    }
                }
                if !module_indices.is_empty() {
                    cycles.push(module_indices);
                }
            } else if scc.len() == 1 {
                let node_idx = scc[0];
                if let Some(node) = self.nodes.get(node_idx) {
                    // Check for self-loop
                    if node.deps.contains(&node_idx) {
                        if let NodeKind::Module { module_idx } = node.kind {
                            cycles.push(vec![module_idx]);
                        }
                    }
                }
            }
        }

        cycles
    }

    /// Build complete semantic dependency graph from HIR modules
    /// This is the main entry point that orchestrates all phases
    pub fn build_from_hir(&mut self, modules: &Vec<HirModule>, pool: &StringPool) {
        // Phase A: Create nodes for all top-level items
        self.phase_a_create_nodes(modules);

        // Phase A.5: Populate internal symbol table for name resolution
        self.populate_symbol_table();

        // Phase B: Extract edges by walking HIR
        self.phase_b_extract_edges_from_hir(modules, pool);

        // Phase C: Drain instantiation queue for generics
        self.phase_c_instantiate_generics();
    }
    
    /// Populate internal symbol table from item_index using node hints
    fn populate_symbol_table(&mut self) {
        for (key, &node_idx) in &self.item_index.clone() {
            let (module_idx, item_idx, tag): (usize, usize, &str) = *key;
            if let Some(node) = self.nodes.get(node_idx) {
                if let Some(hint) = node.hint {
                    self.symbol_table.insert((hint, module_idx), (module_idx, item_idx, tag));
                }
            }
        }
    }

    /// Phase B: Extract edges from HIR by walking function signatures and bodies
    fn phase_b_extract_edges_from_hir(&mut self, modules: &[HirModule], pool: &StringPool) {
        for (midx, module) in modules.iter().enumerate() {
            for (item_idx, item) in module.items.iter().enumerate() {
                match item {
                    Hir::Func(f) => {
                        // Function signature depends on parameter types and return type
                        if let Some(sig_node) = self.lookup_item_node(midx, item_idx, "func_sig") {
                            self.walk_function_signature(f, sig_node, midx, pool);
                        }
                        // Function body depends on called functions and accessed types
                        if let Some(body_node) = self.lookup_item_node(midx, item_idx, "func_body") {
                            self.walk_function_body(f, body_node, midx, pool);
                        }
                    }
                    Hir::Struct(s) => {
                        // Struct type depends on field types
                        if let Some(type_node) = self.lookup_item_node(midx, item_idx, "type") {
                            self.walk_struct_fields(s, type_node, midx, pool);
                        }
                    }
                    Hir::Enum(e) => {
                        // Enum type depends on variant field types
                        if let Some(type_node) = self.lookup_item_node(midx, item_idx, "type") {
                            self.walk_enum_variants(e, type_node, midx, pool);
                        }
                    }
                    Hir::Const(c) => {
                        // Const depends on its value expression
                        if let Some(const_node) = self.lookup_item_node(midx, item_idx, "const") {
                            self.walk_const_value(c, const_node, midx, pool);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Phase C: Process generic instantiation queue
    fn phase_c_instantiate_generics(&mut self) {
        while let Some(key) = self.inst_queue.pop_front() {
            if let Some(&node_idx) = self.instance_map.get(&key) {
                // Instance depends on its base generic
                self.add_edge(node_idx, key.base);
                
                // Instance also depends on the type arguments (if they're TypeDecl nodes)
                // This would require a type resolver to map TypeKey -> NodeIdx
                // For now, we just establish the base dependency
            }
        }
    }

    /// Walk function signature to extract type dependencies
    fn walk_function_signature(&mut self, func: &ir::hir::HirFunc, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        // Depend on parameter types
        if let Some(params) = func.params {
            for param in params {
                self.add_type_dependency(from_node, param.get_type(), module_idx, pool);
            }
        }

        // Depend on return type
        self.add_type_dependency(from_node, func.return_type.as_ref(), module_idx, pool);
    }

    /// Walk function body to extract call and type dependencies
    fn walk_function_body(&mut self, func: &ir::hir::HirFunc, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        if let Some(body) = func.body {
            self.walk_stmt_for_deps(&body, from_node, module_idx, pool);
        }
    }

    /// Walk struct fields to extract type dependencies
    fn walk_struct_fields(&mut self, s: &ir::hir::HirStruct, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        // Check if fields slice is valid before iterating
        if s.fields.is_empty() {
            return;
        }
        
        for field in s.fields {
            self.add_type_dependency(from_node, Some(&field.field_type), module_idx, pool);
        }
    }

    /// Walk enum variants to extract type dependencies
    fn walk_enum_variants(&mut self, e: &ir::hir::HirEnum, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        if e.variants.is_empty() {
            return;
        }
        
        for variant in e.variants {
            for field in variant.fields {
                self.add_type_dependency(from_node, Some(&field.field_type), module_idx, pool);
            }
        }
    }

    /// Walk const value to extract dependencies
    fn walk_const_value(&mut self, c: &ir::hir::ConstStmt, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        self.add_type_dependency(from_node, Some(&c.ty), module_idx, pool);
        self.walk_expr_for_deps(&c.value, from_node, module_idx, pool);
    }

    /// Recursively walk statements to extract dependencies
    fn walk_stmt_for_deps(&mut self, stmt: &ir::hir::HirStmt, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        use ir::hir::HirStmt;
        match stmt {
            HirStmt::Let { ty, value, .. } => {
                self.add_type_dependency(from_node, Some(ty), module_idx, pool);
                self.walk_expr_for_deps(value, from_node, module_idx, pool);
            }
            HirStmt::Return(Some(expr)) => {
                self.walk_expr_for_deps(expr, from_node, module_idx, pool);
            }
            HirStmt::Expr(expr) => {
                self.walk_expr_for_deps(expr, from_node, module_idx, pool);
            }
            HirStmt::If { cond, then_block, else_block } => {
                self.walk_expr_for_deps(cond, from_node, module_idx, pool);
                for s in *then_block {
                    self.walk_stmt_for_deps(s, from_node, module_idx, pool);
                }
                if let Some(else_stmt) = else_block {
                    self.walk_stmt_for_deps(else_stmt, from_node, module_idx, pool);
                }
            }
            HirStmt::While { cond, body } => {
                self.walk_expr_for_deps(cond, from_node, module_idx, pool);
                self.walk_stmt_for_deps(body, from_node, module_idx, pool);
            }
            HirStmt::For { init, condition, increment, body } => {
                if let Some(init_stmt) = init {
                    self.walk_stmt_for_deps(init_stmt, from_node, module_idx, pool);
                }
                if let Some(cond) = condition {
                    self.walk_expr_for_deps(cond, from_node, module_idx, pool);
                }
                if let Some(inc) = increment {
                    self.walk_expr_for_deps(inc, from_node, module_idx, pool);
                }
                self.walk_stmt_for_deps(body, from_node, module_idx, pool);
            }
            HirStmt::Match { expr, arms } => {
                self.walk_expr_for_deps(expr, from_node, module_idx, pool);
                for arm in *arms {
                    if let Some(guard) = arm.guard {
                        self.walk_expr_for_deps(guard, from_node, module_idx, pool);
                    }
                    self.walk_stmt_for_deps(&arm.body, from_node, module_idx, pool);
                }
            }
            HirStmt::UnsafeBlock { body } => {
                self.walk_stmt_for_deps(body, from_node, module_idx, pool);
            }
            HirStmt::Block { body } => {
                for s in *body {
                    self.walk_stmt_for_deps(s, from_node, module_idx, pool);
                }
            }
            _ => {}
        }
    }

    /// Recursively walk expressions to extract dependencies
    fn walk_expr_for_deps(&mut self, expr: &ir::hir::HirExpr, from_node: NodeIdx, module_idx: usize, pool: &StringPool) {
        use ir::hir::HirExpr;
        match expr {
            HirExpr::Call { callee, args } => {
                // Depend on called function
                self.walk_expr_for_deps(callee, from_node, module_idx, pool);
                // Depend on argument expressions
                for arg in *args {
                    self.walk_expr_for_deps(arg, from_node, module_idx, pool);
                }
            }
            HirExpr::InterfaceCall { callee, args, .. } => {
                self.walk_expr_for_deps(callee, from_node, module_idx, pool);
                for arg in *args {
                    self.walk_expr_for_deps(arg, from_node, module_idx, pool);
                }
            }
            HirExpr::StructInit { name, args, .. } => {
                self.walk_expr_for_deps(name, from_node, module_idx, pool);
                for arg in *args {
                    self.walk_expr_for_deps(arg, from_node, module_idx, pool);
                }
            }
            HirExpr::Binary { left, right, .. } => {
                self.walk_expr_for_deps(left, from_node, module_idx, pool);
                self.walk_expr_for_deps(right, from_node, module_idx, pool);
            }
            HirExpr::Comparison { left, right, .. } => {
                self.walk_expr_for_deps(left, from_node, module_idx, pool);
                self.walk_expr_for_deps(right, from_node, module_idx, pool);
            }
            HirExpr::FieldAccess { object, .. } => {
                self.walk_expr_for_deps(object, from_node, module_idx, pool);
            }
            HirExpr::Get { object, .. } => {
                self.walk_expr_for_deps(object, from_node, module_idx, pool);
            }
            HirExpr::Assignment { target, value, .. } => {
                self.walk_expr_for_deps(target, from_node, module_idx, pool);
                self.walk_expr_for_deps(value, from_node, module_idx, pool);
            }
            HirExpr::EnumInit { args, .. } => {
                for arg in *args {
                    self.walk_expr_for_deps(arg, from_node, module_idx, pool);
                }
            }
            HirExpr::Tuple(exprs) => {
                for e in *exprs {
                    self.walk_expr_for_deps(e, from_node, module_idx, pool);
                }
            }
            HirExpr::ExprList { list, .. } => {
                for e in *list {
                    self.walk_expr_for_deps(e, from_node, module_idx, pool);
                }
            }
            HirExpr::InterpolatedString(parts) => {
                for part in *parts {
                    if let ir::hir::InterpolationPart::Expr(e) = part {
                        self.walk_expr_for_deps(e, from_node, module_idx, pool);
                    }
                }
            }
            _ => {}
        }
    }

    /// Add a dependency edge for a type reference
    fn add_type_dependency(&mut self, from_node: NodeIdx, ty: Option<&ir::hir::HirType>, module_idx: usize, pool: &StringPool) {
        let ty = match ty {
            Some(t) => t,
            None => return,
        };
        
        use ir::hir::HirType;
        match ty {
            HirType::Struct(name, _) | HirType::Enum(name, _) | HirType::Interface(name, _) => {
                // Resolve the type name using the symbol table
                // Try to find in current module first
                if let Some(&(m, i, tag)) = self.symbol_table.get(&(name.clone(), module_idx)) {
                    if let Some(to_node) = self.lookup_item_node(m, i, tag) {
                        self.add_edge(from_node, to_node);
                    }
                    return;
                }
                
                // Try to find in imported modules
                let imports = self.get_module_imports(module_idx);
                for imported_idx in imports {
                    if let Some(&(m, i, tag)) = self.symbol_table.get(&(*name, imported_idx)) {
                        if let Some(to_node) = self.lookup_item_node(m, i, tag) {
                            self.add_edge(from_node, to_node);
                        }
                        return;
                    }
                }
            }
            HirType::Pointer(inner) => {
                // Recursively handle nested types
                self.add_type_dependency(from_node, Some(inner), module_idx, pool);
            }
            _ => {}
        }
    }

    /// Get all functions that call a given function
    pub fn get_function_callers(&self, target_module: usize, target_item: usize) -> Vec<(usize, usize)> {
        let mut callers: Vec<(usize, usize)> = Vec::new();

        let Some(target_node) = self.lookup_item_node(target_module, target_item, "func_body") else { return callers; };
        let Some(node) = self.nodes.get(target_node) else { return callers; };

        for &caller_idx in &node.rev_deps {
            let Some(caller_node) = self.nodes.get(caller_idx) else { continue; };
            let NodeKind::FuncBody { module_idx, item_idx } = caller_node.kind else { continue; };
            callers.push((module_idx, item_idx));
        }
        
        callers
    }

    /// Get all functions that a given function calls
    pub fn get_function_callees(&self, caller_module: usize, caller_item: usize) -> Vec<(usize, usize)> {
        let mut callees: Vec<(usize, usize)> = Vec::new();

        let Some(caller_node) = self.lookup_item_node(caller_module, caller_item, "func_body") else { return callees; };
        let Some(node) = self.nodes.get(caller_node) else { return callees; };

        for &callee_idx in &node.deps {
            let Some(callee_node) = self.nodes.get(callee_idx) else { continue; };
            let NodeKind::FuncBody { module_idx, item_idx } = callee_node.kind else { continue; };
            callees.push((module_idx, item_idx));
        }
        
        callees
    }

    /// Check if there are circular function calls (recursive cycles)
    pub fn detect_recursive_cycles(&self) -> Vec<Vec<(usize, usize)>> {
        let sccs = self.tarjan_scc();
        let mut cycles: Vec<Vec<(usize, usize)>> = Vec::new();

        for scc in sccs {
            if scc.len() < 1 { continue; }
            let mut func_refs: Vec<(usize, usize)> = Vec::new();
            for &node_idx in &scc {
                let Some(node) = self.nodes.get(node_idx) else { continue; };
                let NodeKind::FuncBody { module_idx, item_idx } = node.kind else { continue; };
                func_refs.push((module_idx, item_idx));
            }
            if !func_refs.is_empty() { cycles.push(func_refs); }
        }

        cycles
    }

    /// Get all types that a function depends on
    pub fn get_function_type_deps(&self, module_idx: usize, item_idx: usize) -> Vec<(usize, usize)> {
        let mut type_deps: Vec<(usize, usize)> = Vec::new();

        let Some(sig_node) = self.lookup_item_node(module_idx, item_idx, "func_sig") else { return type_deps; };
        let Some(node): Option<&DepNode> = self.nodes.get(sig_node) else { return type_deps; };

        for &dep_idx in &node.deps {
            let Some(dep_node): Option<&DepNode> = self.nodes.get(dep_idx) else { continue; };
            let NodeKind::TypeDecl { module_idx: m, item_idx: i } = dep_node.kind else { continue; };
            type_deps.push((m, i));
        }
        
        type_deps
    }

    /// Get compilation order: topologically sorted functions respecting dependencies
    /// Functions are ordered so that callees come before callers
    pub fn get_compilation_order(&self) -> Vec<Vec<(usize, usize)>> {
        let sccs: Vec<Vec<NodeIdx>> = self.scc_topo_order();
        let mut order: Vec<Vec<(usize, usize)>> = Vec::new();

        // Reverse the SCC order because scc_topo_order gives us callers first
        // We want callees first
        for scc in sccs.iter().rev() {
            let mut func_refs: Vec<(usize, usize)> = Vec::new();
            for node_idx in scc {
                let Some(node) = self.nodes.get(*node_idx) else { continue; };
                let NodeKind::FuncBody { module_idx, item_idx } = node.kind else { continue; };
                func_refs.push((module_idx, item_idx));
            }
            if !func_refs.is_empty() {
                order.push(func_refs);
            }
        }

        order
    }

    /// Link stdlib and user modules: create edges from user code to stdlib
    /// This enables user code to depend on stdlib items
    pub fn link_stdlib_to_user(&mut self, stdlib_module_idx: usize, user_module_indices: &[usize]) {
        // Create import edges from each user module to stdlib
        for &user_idx in user_module_indices {
            self.register_import(user_idx, stdlib_module_idx);
        }
    }

    /// Create a symbol table for name resolution
    /// Maps (name_str, module_idx) -> (target_module_idx, item_idx, tag)
    pub fn build_symbol_table(&self) -> HashMap<(StrId, usize), (usize, usize, &'static str)> {
        let mut table: HashMap<(StrId, usize), (usize, usize, &'static str)> = HashMap::default();

        // key: &(usize, usize, &str)
        for (key, &node_idx) in &self.item_index {
            let (module_idx, item_idx, tag): &(usize, usize, &str) = key;
            let Some(node) = self.nodes.get(node_idx) else { continue; };
            let Some(hint) = node.hint else { continue; };
            table.insert((hint, *module_idx), (*module_idx, *item_idx, tag));
        }

        table
    }

    /// Resolve a name to its definition node
    /// Searches in current module first, then in imported modules
    pub fn resolve_name(&self, name: StrId, current_module_idx: usize, symbol_table: &HashMap<(StrId, usize), (usize, usize, &'static str)>) -> Option<NodeIdx> {
        // First try current module
        if let Some(&(m, i, tag)) = symbol_table.get(&(name, current_module_idx)) {
            return self.lookup_item_node(m, i, tag);
        }

        // Then try imported modules
        let imports: Vec<usize> = self.get_module_imports(current_module_idx);
        for imported_idx in imports {
            let Some(&(m, i, tag)) = symbol_table.get(&(name, imported_idx)) else { continue; };
            return self.lookup_item_node(m, i, tag);
        }

        None
    }

    /// Get all modules in dependency order (topological sort)
    /// Dependencies are compiled first, then modules that depend on them
    pub fn get_module_compilation_order(&self) -> Vec<usize> {
        let sccs: Vec<Vec<NodeIdx>> = self.scc_topo_order();
        let mut order: Vec<usize> = Vec::new();
        let mut seen: HashSet<usize> = HashSet::default();

        // Reverse the SCC order because scc_topo_order gives us dependents first, we want dependencies first
        for scc in sccs.iter().rev() {
            for node_idx in scc {
                let Some(node) = self.nodes.get(*node_idx) else { continue; };
                let NodeKind::Module { module_idx } = node.kind else { continue; };
                if seen.insert(module_idx) { order.push(module_idx); }
            }
        }

        order
    }

    /// Check if user code can access a stdlib item (visibility check)
    pub fn can_access(&self, from_module: usize, target_module: usize, _target_item_idx: usize) -> bool {
        if from_module == target_module {
            return true;
        }

        let imports: Vec<usize> = self.get_module_imports(from_module);
        imports.contains(&target_module)
    }
}
