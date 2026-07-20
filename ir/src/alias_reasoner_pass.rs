use std::collections::HashMap;

use crate::hir::StrId;
use crate::ir_hasher::FxHashMap;

use crate::borrow_checker::{
    AliasReasoner, Bound, IndexContainer, Interval, MemoryRelation, Place, PlaceId, Projection,
};

impl AliasReasoner {
    pub fn new() -> Self {
        Self {
            disjoint_indexable: HashMap::default(),
            equal: HashMap::default(),
            not_equal: HashMap::default(),
            less_than: HashMap::default(),
            ranges: HashMap::default(),
            disjoint_intervals: Vec::default(),
            place_not_equal: HashMap::default(),
        }
    }

    fn ancestor_chain(&self, places: &FxHashMap<PlaceId, Place>, start: PlaceId) -> Vec<PlaceId> {
        let mut chain = vec![start];
        let mut current = start;
        while let Some(place) = places.get(&current) {
            match place.parent {
                Some(parent) => {
                    chain.push(parent);
                    current = parent;
                }
                None => break,
            }
        }
        chain
    }

    pub fn relation(
        &self,
        places: &FxHashMap<PlaceId, Place>,
        place_to_local: &FxHashMap<PlaceId, StrId>,
        lhs: PlaceId,
        rhs: PlaceId,
    ) -> MemoryRelation {
        let lhs_chain = self.ancestor_chain(places, lhs);
        let rhs_chain = self.ancestor_chain(places, rhs);

        for &l in &lhs_chain {
            for &r in &rhs_chain {
                if self.place_not_equal.get(&l).is_some_and(|s| s.contains(&r)) {
                    return MemoryRelation::Disjoint;
                }
            }
        }

        if lhs == rhs {
            return MemoryRelation::Overlap;
        }

        let lhs_place = match places.get(&lhs) {
            Some(place) => place,
            None => return MemoryRelation::Unknown,
        };
        let rhs_place = match places.get(&rhs) {
            Some(place) => place,
            None => return MemoryRelation::Unknown,
        };

        // Completely different locals.
        if lhs_place.parent.is_none() && rhs_place.parent.is_none() {
            return MemoryRelation::Disjoint;
        }

        // Parent/child always overlap.
        if self.is_ancestor(places, lhs_place.id, rhs_place.id)
            || self.is_ancestor(places, rhs_place.id, lhs_place.id)
        {
            return MemoryRelation::Overlap;
        }

        // General field/index disjointness: find where the two ancestor
        // chains diverge (their lowest common ancestor's two children) and
        // compare THOSE, rather than requiring lhs/rhs to be direct siblings.
        // This is what makes `second.inner.tag` correctly compare against
        // `third`'s loan through several layers of field projection, not
        // just when the compared places are themselves the immediate
        // Index/Field nodes.
        if let Some((ldiv, rdiv)) = self.find_divergence(&lhs_chain, &rhs_chain) {
            if let (Some(ld), Some(rd)) = (places.get(&ldiv), places.get(&rdiv)) {
                match (&ld.projection, &rd.projection) {
                    (Some(Projection::Field(a)), Some(Projection::Field(b))) => {
                        return if a == b {
                            MemoryRelation::Overlap
                        } else {
                            MemoryRelation::Disjoint
                        };
                    }
                    (
                        Some(Projection::Index {
                            index: li,
                            container,
                        }),
                        Some(Projection::Index { index: ri, .. }),
                    ) => {
                        return self.reason_about_indices(li, ri, *container);
                    }
                    _ => {}
                }
            }
        }

        if let (Some(l), Some(r)) = (places.get(&lhs), places.get(&rhs)) {
            if let (Some(Projection::Deref), Some(Projection::Deref)) =
                (&l.projection, &r.projection)
            {
                if l.parent != r.parent {
                    if let (Some(lb), Some(rb)) = (
                        l.parent
                            .and_then(|p| place_to_local.get(&p))
                            .map(|&s| Bound::Symbol(s)),
                        r.parent
                            .and_then(|p| place_to_local.get(&p))
                            .map(|&s| Bound::Symbol(s)),
                    ) {
                        if self.not_equal.get(&lb).is_some_and(|s| s.contains(&rb)) {
                            return MemoryRelation::Disjoint;
                        }
                        if self.equal.get(&lb).is_some_and(|s| s.contains(&rb)) {
                            return MemoryRelation::Overlap;
                        }
                    }
                }
            }
        }

        MemoryRelation::Unknown
    }

    /// Given two ancestor chains (each ordered [place, parent, grandparent,
    /// ..., root]), find their lowest common ancestor and return the pair of
    /// children -- one from each chain -- that sit immediately below it. These
    /// are the two projections that actually need comparing (both Field or
    /// both Index) to determine disjointness. Returns None if the chains
    /// share no common ancestor, or if one chain is a prefix of the other
    /// (already handled by the ancestor/descendant check before this runs).
    fn find_divergence(
        &self,
        lhs_chain: &[PlaceId],
        rhs_chain: &[PlaceId],
    ) -> Option<(PlaceId, PlaceId)> {
        let lhs_rev: Vec<PlaceId> = lhs_chain.iter().rev().copied().collect();
        let rhs_rev: Vec<PlaceId> = rhs_chain.iter().rev().copied().collect();

        let mut i = 0;
        while i < lhs_rev.len() && i < rhs_rev.len() && lhs_rev[i] == rhs_rev[i] {
            i += 1;
        }

        if i == 0 || i >= lhs_rev.len() || i >= rhs_rev.len() {
            return None;
        }

        Some((lhs_rev[i], rhs_rev[i]))
    }

    pub fn retract_place_not_equal(&mut self, lhs: &PlaceId, rhs: &PlaceId) {
        if let Some(set) = self.place_not_equal.get_mut(lhs) {
            set.remove(rhs);
        }
        if let Some(set) = self.place_not_equal.get_mut(rhs) {
            set.remove(lhs);
        }
    }

    fn resolve_const(&self, bound: &Bound) -> Option<i64> {
        if let Bound::Const(v) = bound {
            return Some(*v);
        }
        self.equal.get(bound).and_then(|set| {
            set.iter().find_map(|b| match b {
                Bound::Const(v) => Some(*v),
                _ => None,
            })
        })
    }

    fn reason_about_indices(
        &self,
        lhs: &Interval,
        rhs: &Interval,
        container: IndexContainer,
    ) -> MemoryRelation {
        let trusted = match container {
            IndexContainer::Primitive => true,
            IndexContainer::UserDefined(ty) => {
                self.disjoint_indexable.get(&ty).copied().unwrap_or(false)
            }
        };

        if !trusted {
            return MemoryRelation::Unknown;
        }

        if lhs == rhs {
            return MemoryRelation::Overlap;
        }

        match (
            self.compare_linear(&lhs.lower, &rhs.lower),
            self.compare_linear(&lhs.upper, &rhs.upper),
        ) {
            (Some(true), Some(true)) => {
                return MemoryRelation::Overlap;
            }
            (Some(false), Some(false)) => {
                return MemoryRelation::Disjoint;
            }
            _ => {}
        }

        if lhs.lower == rhs.lower {
            return MemoryRelation::Overlap;
        }

        if self
            .equal
            .get(&lhs.lower)
            .is_some_and(|set| set.contains(&rhs.lower))
        {
            return MemoryRelation::Overlap;
        }

        if self
            .not_equal
            .get(&lhs.lower)
            .is_some_and(|set| set.contains(&rhs.lower))
        {
            return MemoryRelation::Disjoint;
        }

        if self
            .less_than
            .get(&lhs.lower)
            .is_some_and(|set| set.contains(&rhs.lower))
        {
            return MemoryRelation::Disjoint;
        }

        // LessEqual doesn't prove disjointness.

        for (left, right) in &self.disjoint_intervals {
            if self.interval_matches(lhs, rhs, left, right) {
                return MemoryRelation::Disjoint;
            }
        }

        MemoryRelation::Unknown
    }

    pub fn retract_equal(&mut self, lhs: &Bound, rhs: &Bound) {
        if let Some(set) = self.equal.get_mut(lhs) {
            set.remove(rhs);
        }
        if let Some(set) = self.equal.get_mut(rhs) {
            set.remove(lhs);
        }
    }

    pub fn retract_not_equal(&mut self, lhs: &Bound, rhs: &Bound) {
        if let Some(set) = self.not_equal.get_mut(lhs) {
            set.remove(rhs);
        }
        if let Some(set) = self.not_equal.get_mut(rhs) {
            set.remove(lhs);
        }
    }

    fn compare_linear(&self, lhs: &Bound, rhs: &Bound) -> Option<bool> {
        use Bound::*;

        if let (Some(a), Some(b)) = (self.resolve_const(lhs), self.resolve_const(rhs)) {
            return Some(a == b);
        }

        match (lhs, rhs) {
            (Const(a), Const(b)) => Some(a == b),
            (Symbol(a), Symbol(b)) if a == b => Some(true),
            (SelfField(a), SelfField(b)) => Some(a == b),

            (
                Scale {
                    base: lb,
                    factor: lf,
                },
                Scale {
                    base: rb,
                    factor: rf,
                },
            ) => {
                if lf != rf {
                    return None;
                }
                if *lf == 0 {
                    return Some(true);
                }
                self.compare_linear(lb, rb)
            }

            (FreshPerCall, FreshPerCall) => Some(false),

            // Different Opaque ids are unknown whether equal
            (Opaque(_), _) | (_, Opaque(_)) => None,

            (Sum(a1, b1), Sum(a2, b2)) => {
                match (self.compare_linear(a1, a2), self.compare_linear(b1, b2)) {
                    (Some(true), Some(true)) => Some(true),
                    _ => None,
                }
            }

            (Offset { .. }, _) | (_, Offset { .. }) => {
                let (lhs_base, lhs_off) = Self::decompose_offset(lhs);
                let (rhs_base, rhs_off) = Self::decompose_offset(rhs);
                match self.compare_linear(lhs_base, rhs_base) {
                    Some(true) => Some(lhs_off == rhs_off),
                    Some(false) | None => None,
                }
            }

            _ => self.constraint_verdict(lhs, rhs),
        }
    }

    fn decompose_offset(bound: &Bound) -> (&Bound, i64) {
        match bound {
            Bound::Offset { base, offset } => (base.as_ref(), *offset),
            other => (other, 0),
        }
    }

    fn interval_matches(
        &self,
        left: &Interval,
        right: &Interval,
        expected_left: &Interval,
        expected_right: &Interval,
    ) -> bool {
        (left == expected_left && right == expected_right)
            || (left == expected_right && right == expected_left)
    }

    fn constraint_verdict(&self, lhs: &Bound, rhs: &Bound) -> Option<bool> {
        if lhs == rhs {
            return Some(true);
        }

        if self.equal.get(lhs).is_some_and(|set| set.contains(rhs)) {
            return Some(true);
        }

        if self.not_equal.get(lhs).is_some_and(|set| set.contains(rhs)) {
            return Some(false);
        }

        None
    }

    fn is_ancestor(
        &self,
        places: &FxHashMap<PlaceId, Place>,
        ancestor: PlaceId,
        mut current: PlaceId,
    ) -> bool {
        while let Some(place) = places.get(&current) {
            if place.id == ancestor {
                return true;
            }

            match place.parent {
                Some(parent) => current = parent,
                None => return false,
            }
        }

        false
    }
}
