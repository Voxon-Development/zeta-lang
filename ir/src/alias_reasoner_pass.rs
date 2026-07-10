use std::collections::HashMap;

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
        }
    }

    pub fn relation(
        &self,
        places: &FxHashMap<PlaceId, Place>,
        lhs: PlaceId,
        rhs: PlaceId,
    ) -> MemoryRelation {
        if lhs == rhs {
            return MemoryRelation::Overlap;
        }

        let lhs = match places.get(&lhs) {
            Some(place) => place,
            None => return MemoryRelation::Unknown,
        };

        let rhs = match places.get(&rhs) {
            Some(place) => place,
            None => return MemoryRelation::Unknown,
        };

        // Completely different locals.
        if lhs.parent.is_none() && rhs.parent.is_none() {
            return MemoryRelation::Disjoint;
        }

        // Parent/child always overlap.
        if self.is_ancestor(places, lhs.id, rhs.id) || self.is_ancestor(places, rhs.id, lhs.id) {
            return MemoryRelation::Overlap;
        }

        // Same parent?
        if lhs.parent == rhs.parent {
            match (&lhs.projection, &rhs.projection) {
                (Some(Projection::Field(a)), Some(Projection::Field(b))) => {
                    return if a == b {
                        MemoryRelation::Overlap
                    } else {
                        MemoryRelation::Disjoint
                    };
                }
                (
                    Some(Projection::Index {
                        index: lhs,
                        container,
                    }),
                    Some(Projection::Index { index: rhs, .. }),
                ) => {
                    return self.reason_about_indices(lhs, rhs, *container);
                }
                _ => {}
            }
        }

        MemoryRelation::Unknown
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

        // Resolve known equality-to-constant facts first, so an assumed
        // `index == 4` lets `index` compare correctly against a literal `3`
        // even though they're structurally different Bound shapes.
        if let (Some(a), Some(b)) = (self.resolve_const(lhs), self.resolve_const(rhs)) {
            return Some(a == b);
        }

        match (lhs, rhs) {
            (Const(a), Const(b)) => Some(a == b),

            (Symbol(a), Symbol(b)) if a == b => Some(true),

            (SelfField(a), SelfField(b)) => Some(a == b),

            (
                Offset {
                    base: lhs_base,
                    offset: lhs_offset,
                },
                Offset {
                    base: rhs_base,
                    offset: rhs_offset,
                },
            ) => {
                if lhs_offset != rhs_offset {
                    return None;
                }

                self.compare_linear(lhs_base, rhs_base)
            }

            (
                Scale {
                    base: lhs_base,
                    factor: lhs_factor,
                },
                Scale {
                    base: rhs_base,
                    factor: rhs_factor,
                },
            ) => {
                if lhs_factor != rhs_factor {
                    return None;
                }

                if *lhs_factor == 0 {
                    return Some(true);
                }

                self.compare_linear(lhs_base, rhs_base)
            }

            (FreshPerCall, FreshPerCall) => Some(false),

            (Opaque, _) | (_, Opaque) => None,

            _ => self.constraint_verdict(lhs, rhs),
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
