use std::collections::{VecDeque};
use crate::symbol_table::ModulesSoA;

pub fn topo_sort(modules: &ModulesSoA) -> Vec<usize> {
    let mut indegree: Vec<usize> = vec![0usize; modules.names.len()];

    for deps in &modules.deps {
        for &dep_idx in deps {
            indegree[dep_idx] += 1;
        }
    }

    let mut queue: VecDeque<usize> = VecDeque::new();
    for (i, &deg) in indegree.iter().enumerate() {
        if deg == 0 { queue.push_back(i); }
    }

    let mut sorted: Vec<usize> = Vec::with_capacity(modules.names.len());

    while let Some(idx) = queue.pop_front() {
        sorted.push(idx);

        for &dep_idx in &modules.deps[idx] {
            indegree[dep_idx] -= 1;
            if indegree[dep_idx] == 0 { queue.push_back(dep_idx); }
        }
    }

    sorted
}