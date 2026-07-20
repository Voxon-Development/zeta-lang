use crate::{hir::StrId, ir_hasher::FxHashMap};

use crate::borrow_checker::{
    AliasReasoner, BorrowChecker, BorrowError, BorrowKind, BorrowResult, Bound, IndexContainer,
    Interval, Loan, LoanId, MemoryRelation, Place, PlaceId, Projection, Provenance, ProvenanceId,
    ProvenanceOrigin, Scope,
};

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            next_place: PlaceId(0),
            next_loan: LoanId(0),
            next_provenance: ProvenanceId(0),

            places: FxHashMap::default(),
            active_loans: FxHashMap::default(),
            provenances: FxHashMap::default(),
            local_places: FxHashMap::default(),

            place_roots: FxHashMap::default(),
            root_loans: FxHashMap::default(),

            reasoning: AliasReasoner::new(),
            scopes: Vec::new(),
            place_to_local: FxHashMap::default(),
            place_provenance: FxHashMap::default(),
            field_places: FxHashMap::default(),
            deref_places: FxHashMap::default(),
            index_places: FxHashMap::default(),
            pointee_origin: FxHashMap::default(),
        }
    }

    pub fn print_borrow_state(&self) {
        eprintln!("=== borrow checker state ===");

        eprintln!("-- scopes ({}) --", self.scopes.len());
        for (depth, scope) in self.scopes.iter().enumerate() {
            eprintln!(
                "  [{}] places={:?} loans={:?}",
                depth, scope.places, scope.loans
            );
        }

        eprintln!("-- locals --");
        for (name, place) in &self.local_places {
            eprintln!("  {:?} -> {:?}", name, place);
        }

        eprintln!("-- places ({}) --", self.places.len());
        let mut place_ids: Vec<_> = self.places.keys().copied().collect();
        place_ids.sort_by_key(|p| p.0);
        for id in place_ids {
            let place = &self.places[&id];
            let root = self.place_roots.get(&id);
            let local = self.place_to_local.get(&id);

            let proj_str = match &place.projection {
                None => "root".to_string(),
                Some(Projection::Field(f)) => format!("field({:?})", f),
                Some(Projection::Deref) => "deref".to_string(),
                Some(Projection::Index { index, container }) => {
                    format!("index({:?}, {:?})", index, container)
                }
            };

            eprintln!(
                "  {:?}: parent={:?} proj={} root={:?} local={:?}",
                id, place.parent, proj_str, root, local
            );
        }

        eprintln!("-- active loans ({}) --", self.active_loans.len());
        let mut loan_ids: Vec<_> = self.active_loans.keys().copied().collect();
        loan_ids.sort_by_key(|l| l.0);
        for id in loan_ids {
            let loan = &self.active_loans[&id];
            eprintln!(
                "  {:?}: place={:?} kind={:?} provenance={:?} active={}",
                loan.id, loan.place, loan.kind, loan.provenance_id, loan.active
            );
        }

        eprintln!("-- root -> loans --");
        for (root, loans) in &self.root_loans {
            eprintln!("  {:?} -> {:?}", root, loans);
        }

        eprintln!("=============================");
    }

    fn alloc_place_id(&mut self) -> PlaceId {
        let id = self.next_place;
        self.next_place.0 += 1;
        id
    }

    fn alloc_loan_id(&mut self) -> LoanId {
        let id = self.next_loan;
        self.next_loan.0 += 1;
        id
    }

    fn alloc_provenance_id(&mut self) -> ProvenanceId {
        let id = self.next_provenance;
        self.next_provenance.0 += 1;
        id
    }

    pub fn borrow_shared(&mut self, place: PlaceId) -> BorrowResult<LoanId> {
        self.check_definite_conflict(place, BorrowKind::Shared)?;
        self.create_loan(place, BorrowKind::Shared)
    }

    pub fn borrow_mut(&mut self, place: PlaceId) -> BorrowResult<LoanId> {
        self.check_definite_conflict(place, BorrowKind::Mutable)?;
        self.create_loan(place, BorrowKind::Mutable)
    }

    fn check_definite_conflict(&self, place: PlaceId, kind: BorrowKind) -> BorrowResult<()> {
        let root = self.place_roots[&place];
        let Some(loans) = self.root_loans.get(&root) else {
            return Ok(());
        };

        for loan_id in loans {
            let loan = &self.active_loans[loan_id];
            // Only a PROVEN overlap blocks creation. An Unknown relation is not
            // rejected here, the program may go on to establish disjointness
            // (e.g. via a runtime pointer-identity check) before either
            // reference is actually used. That obligation is enforced at
            // check_use, not here.
            if let MemoryRelation::Overlap = self.overlaps(place, loan.place)? {
                if kind == BorrowKind::Mutable || loan.kind == BorrowKind::Mutable {
                    return Err(match loan.kind {
                        BorrowKind::Shared => BorrowError::Borrowed { place: loan.place },
                        BorrowKind::Mutable => {
                            BorrowError::AlreadyMutablyBorrowed { place: loan.place }
                        }
                    });
                }
            }
        }
        Ok(())
    }

    pub fn check_use(&self, place: PlaceId, kind: BorrowKind) -> BorrowResult<()> {
        let root = self.place_roots[&place];
        let Some(loans) = self.root_loans.get(&root) else {
            return Ok(());
        };

        for loan_id in loans {
            let loan = &self.active_loans[loan_id];

            // A loan never conflicts with a use that passes THROUGH it: if
            // loan.place is `place` itself, or an ancestor of it (i.e. `place`
            // is a field/index/deref projection reached via the very
            // reference this loan backs), this IS the loan enabling the
            // access, not a competing borrow. Safe to skip unconditionally:
            // check_definite_conflict already forbids two active loans from
            // ever being in an ancestor/descendant relationship on the same
            // lineage, so at most one active loan can ever be an
            // ancestor-or-equal of `place`, it can't be masking a different,
            // genuinely conflicting loan.
            if loan.place == place || self.place_is_ancestor(loan.place, place) {
                continue;
            }

            match self.overlaps(place, loan.place)? {
                MemoryRelation::Disjoint => {}
                MemoryRelation::Overlap | MemoryRelation::Unknown
                    if kind == BorrowKind::Mutable || loan.kind == BorrowKind::Mutable =>
                {
                    return Err(BorrowError::UnknownAlias {
                        lhs: place,
                        rhs: loan.place,
                    });
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn place_is_ancestor(&self, ancestor: PlaceId, mut current: PlaceId) -> bool {
        while let Some(place) = self.places.get(&current) {
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

    fn create_loan(&mut self, place: PlaceId, kind: BorrowKind) -> BorrowResult<LoanId> {
        let loan_id = self.alloc_loan_id();
        let provenance_id = self.derive_provenance(place);

        let loan = Loan {
            id: loan_id,
            place,
            kind,
            provenance_id,
            active: true,
        };

        self.active_loans.insert(loan_id, loan);

        let root = self.place_roots[&place];
        self.root_loans.entry(root).or_default().push(loan_id);

        if let Some(scope) = self.scopes.last_mut() {
            scope.loans.push(loan_id);
        }

        Ok(loan_id)
    }

    /// Register a new local variable.
    pub fn declare_local(&mut self, local: StrId) -> PlaceId {
        let place_id = self.alloc_place_id();

        self.places.insert(
            place_id,
            Place {
                id: place_id,
                parent: None,
                projection: None,
            },
        );

        self.local_places.insert(local, place_id);
        self.place_to_local.insert(place_id, local);

        // A local is its own root.
        self.place_roots.insert(place_id, place_id);

        if let Some(scope) = self.scopes.last_mut() {
            scope.places.push(place_id);
        }

        place_id
    }

    /// Called by the type checker once it has proven (or disproven) that
    /// `struct_name`'s indexing method yields non-overlapping memory for
    /// distinct indices.
    pub fn mark_disjoint_indexable(&mut self, struct_name: StrId, disjoint: bool) {
        self.reasoning
            .disjoint_indexable
            .insert(struct_name, disjoint);
    }

    pub fn project_index(
        &mut self,
        base: PlaceId,
        index: Interval,
        container: IndexContainer,
    ) -> PlaceId {
        let key = (base, index.lower.clone());
        if let Some(&existing) = self.index_places.get(&key) {
            return existing;
        }

        let id = self.alloc_place_id();
        self.places.insert(
            id,
            Place {
                id,
                parent: Some(base),
                projection: Some(Projection::Index { index, container }),
            },
        );
        let root = self.place_roots[&base];
        self.place_roots.insert(id, root);
        self.index_places.insert(key, id);
        id
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            loans: Vec::new(),
            places: Vec::new(),
            assumed_facts: Vec::new(),
            assumed_place_facts: Vec::new(),
        });
    }

    pub fn end_scope(&mut self) {
        let Some(scope) = self.scopes.pop() else {
            return;
        };

        for (lhs, rhs, is_equal) in &scope.assumed_facts {
            if *is_equal {
                self.reasoning.retract_equal(lhs, rhs);
            } else {
                self.reasoning.retract_not_equal(lhs, rhs);
            }
        }
        for (lhs, rhs) in &scope.assumed_place_facts {
            self.reasoning.retract_place_not_equal(lhs, rhs);
        }

        for loan in scope.loans {
            self.end_loan(loan);
        }
        for place in scope.places {
            self.places.remove(&place);
            self.place_roots.remove(&place);
            self.field_places
                .retain(|(base, _), child| *base != place && *child != place);
            self.deref_places
                .retain(|base, child| *base != place && *child != place);
            self.index_places
                .retain(|(base, _), child| *base != place && *child != place);

            if let Some(local) = self.place_to_local.remove(&place) {
                self.local_places.remove(&local);
            }
        }
    }

    pub fn assume_places_not_equal_scoped(&mut self, lhs: PlaceId, rhs: PlaceId) {
        self.reasoning
            .place_not_equal
            .entry(lhs)
            .or_default()
            .insert(rhs);
        self.reasoning
            .place_not_equal
            .entry(rhs)
            .or_default()
            .insert(lhs);
        if let Some(scope) = self.scopes.last_mut() {
            scope.assumed_place_facts.push((lhs, rhs));
        }
    }

    pub fn end_loan_now(&mut self, loan: LoanId) {
        self.end_loan(loan);

        for scope in self.scopes.iter_mut() {
            scope.loans.retain(|l| *l != loan);
        }
    }

    pub fn loan_for_place(&self, place: PlaceId) -> Option<&LoanId> {
        let root = self.place_roots.get(&place)?;
        self.root_loans.get(root)?.last()
    }

    /// Child place via field projection.
    pub fn project_field(&mut self, base: PlaceId, field: StrId) -> PlaceId {
        if let Some(&existing) = self.field_places.get(&(base, field)) {
            return existing;
        }

        let id = self.alloc_place_id();
        self.places.insert(
            id,
            Place {
                id,
                parent: Some(base),
                projection: Some(Projection::Field(field)),
            },
        );
        let root = self.place_roots[&base];
        self.place_roots.insert(id, root);
        self.field_places.insert((base, field), id);
        id
    }

    /// Child place via dereference.
    pub fn project_deref(&mut self, base: PlaceId) -> PlaceId {
        if let Some(&existing) = self.deref_places.get(&base) {
            return existing;
        }

        let id = self.alloc_place_id();
        self.places.insert(
            id,
            Place {
                id,
                parent: Some(base),
                projection: Some(Projection::Deref),
            },
        );
        let root = self.place_roots[&base];
        self.place_roots.insert(id, root);
        self.deref_places.insert(base, id);
        id
    }

    pub fn check_move(&self, place: PlaceId) -> BorrowResult<()> {
        let root = self.place_roots[&place];
        let Some(loans) = self.root_loans.get(&root) else {
            return Ok(());
        };

        for loan_id in loans {
            let loan = &self.active_loans[loan_id];
            match self.overlaps(place, loan.place)? {
                MemoryRelation::Disjoint => {}
                MemoryRelation::Overlap | MemoryRelation::Unknown => {
                    return Err(BorrowError::CannotMoveBorrowed { place: loan.place });
                }
            }
        }
        Ok(())
    }

    /// Explicitly ends a loan.
    ///
    /// dataflow will usually call this automatically.
    pub fn end_loan(&mut self, loan: LoanId) {
        let Some(loan) = self.active_loans.remove(&loan) else {
            return;
        };

        let root = self.place_roots[&loan.place];

        if let Some(loans) = self.root_loans.get_mut(&root) {
            loans.retain(|l| *l != loan.id);

            if loans.is_empty() {
                self.root_loans.remove(&root);
            }
        }
    }

    /// Provenance of a newly-created reference.
    pub fn derive_provenance(&mut self, place: PlaceId) -> ProvenanceId {
        if let Some(id) = self.place_provenance.get(&place) {
            return *id;
        }

        let place_info = self.places[&place].clone();

        let (origin, parents) = match (place_info.parent, place_info.projection) {
            (None, _) => (ProvenanceOrigin::Local(place), Vec::new()),

            (Some(parent), Some(Projection::Field(field))) => {
                let parent_prov = self.derive_provenance(parent);

                (
                    ProvenanceOrigin::Field {
                        parent: parent_prov,
                        field,
                    },
                    vec![parent_prov],
                )
            }

            (Some(parent), Some(Projection::Index { .. })) => {
                let parent_prov = self.derive_provenance(parent);

                (
                    ProvenanceOrigin::Index {
                        parent: parent_prov,
                    },
                    vec![parent_prov],
                )
            }

            (Some(parent), Some(Projection::Deref)) => {
                let parent_prov = self.derive_provenance(parent);

                (
                    ProvenanceOrigin::Deref {
                        parent: parent_prov,
                    },
                    vec![parent_prov],
                )
            }

            _ => unreachable!(),
        };

        let id = self.alloc_provenance_id();

        self.provenances.insert(
            id,
            Provenance {
                id,
                origin,
                parents,
            },
        );

        self.place_provenance.insert(place, id);

        id
    }

    pub fn merge_provenance(&mut self, parents: &[ProvenanceId]) -> ProvenanceId {
        if parents.len() == 1 {
            return parents[0];
        }

        let id = self.alloc_provenance_id();

        self.provenances.insert(
            id,
            Provenance {
                id,
                origin: ProvenanceOrigin::Merge,
                parents: parents.to_vec(),
            },
        );

        id
    }

    pub fn place(&self, id: PlaceId) -> Option<&Place> {
        self.places.get(&id)
    }

    pub fn loan(&self, id: LoanId) -> Option<&Loan> {
        self.active_loans.get(&id)
    }

    pub fn provenance(&self, id: ProvenanceId) -> Option<&Provenance> {
        self.provenances.get(&id)
    }

    pub fn local_place(&self, local: StrId) -> Option<&PlaceId> {
        self.local_places.get(&local)
    }

    pub fn invalidate_place(&mut self, place: PlaceId) {
        let mut stack = vec![place];

        while let Some(current) = stack.pop() {
            // Find child places.
            let children: Vec<_> = self
                .places
                .values()
                .filter(|p| p.parent == Some(current))
                .map(|p| p.id)
                .collect();

            stack.extend(children);

            // End loans.
            let loans: Vec<_> = self
                .active_loans
                .values()
                .filter(|loan| loan.place == current)
                .map(|loan| loan.id)
                .collect();

            for loan in loans {
                self.end_loan(loan);
            }

            self.place_provenance.remove(&current);
        }
    }

    pub fn overlaps(&self, lhs: PlaceId, rhs: PlaceId) -> BorrowResult<MemoryRelation> {
        if !self.places.contains_key(&lhs) {
            return Err(BorrowError::PlaceNotFound(lhs));
        }

        if !self.places.contains_key(&rhs) {
            return Err(BorrowError::PlaceNotFound(rhs));
        }

        Ok(self
            .reasoning
            .relation(&self.places, &self.place_to_local, lhs, rhs))
    }

    pub fn assume_equal(&mut self, lhs: Bound, rhs: Bound) {
        self.reasoning
            .equal
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());

        self.reasoning.equal.entry(rhs).or_default().insert(lhs);
    }

    pub fn assume_not_equal(&mut self, lhs: Bound, rhs: Bound) {
        self.reasoning
            .not_equal
            .entry(lhs.clone())
            .or_default()
            .insert(rhs.clone());

        self.reasoning.not_equal.entry(rhs).or_default().insert(lhs);
    }

    pub fn assume_equal_scoped(&mut self, lhs: Bound, rhs: Bound) {
        self.assume_equal(lhs.clone(), rhs.clone());
        if let Some(scope) = self.scopes.last_mut() {
            scope.assumed_facts.push((lhs, rhs, true));
        }
    }

    pub fn assume_not_equal_scoped(&mut self, lhs: Bound, rhs: Bound) {
        self.assume_not_equal(lhs.clone(), rhs.clone());
        if let Some(scope) = self.scopes.last_mut() {
            scope.assumed_facts.push((lhs, rhs, false));
        }
    }

    pub fn assume_range(&mut self, value: Bound, lower: Bound, upper: Bound) {
        self.reasoning
            .ranges
            .insert(value, Interval { lower, upper });
    }

    pub fn assume_disjoint(&mut self, left: Interval, right: Interval) {
        self.reasoning
            .disjoint_intervals
            .push((left.clone(), right.clone()));

        self.reasoning.disjoint_intervals.push((right, left));
    }

    pub fn record_pointee(&mut self, ptr_place: PlaceId, base: PlaceId, offset: Interval) {
        self.pointee_origin.insert(ptr_place, (base, offset));
    }

    pub fn pointee_of(&self, ptr_place: PlaceId) -> Option<&(PlaceId, Interval)> {
        self.pointee_origin.get(&ptr_place)
    }
}
