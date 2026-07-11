use fxhash::FxHashSet;

use crate::{hir::StrId, ir_hasher::FxHashMap};

/// Stable identifier for a memory location.
///
/// A place is something that can be borrowed:
///
///     x
///     player.inventory
///     world.players[0]
///
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct PlaceId(pub u32);

/// Stable identifier for one active borrow.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct LoanId(pub u32);

/// Provenance node.
///
/// Every reference carries exactly one provenance.
///
/// Provenances form a graph:
///
///     World.players
///           │
///           ▼
///
///           P0
///          /  \
///        P1    P2
///          \  /
///           P3
///
/// Unlike loans, provenances may outlive any individual borrow because they
/// describe *where a reference originated*, not whether the borrow is still
/// active.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ProvenanceId(pub u32);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ValueId(pub u32);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BorrowKind {
    Shared,
    Mutable,
}

#[derive(Clone, Debug)]
pub enum Projection {
    Field(StrId),

    Index {
        index: Interval,
        container: IndexContainer,
    },

    Deref,
}

/// What kind of thing is being indexed, and therefore how much we're
/// allowed to trust "different index => disjoint memory".
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IndexContainer {
    /// `[N]T` / `[]T`
    Primitive,

    /// A user struct's `get`/`get_mut`. Distinct indices are
    /// only disjoint if the type checker has proven the indexing method
    /// does plain `base + index * size` arithmetic with no internal
    /// aliasing (shared buffers, free lists, etc).
    UserDefined(StrId),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MemoryRelation {
    /// Guaranteed to touch different memory.
    Disjoint,

    /// Proven to overlap.
    Overlap,

    /// Could not prove either.
    Unknown,
}

#[derive(Clone, Debug)]
pub struct Place {
    pub id: PlaceId,

    /// Parent place.
    ///
    /// world.players[0]
    ///
    /// inventory
    ///   ^
    /// index
    ///   ^
    /// players
    ///   ^
    /// world
    pub parent: Option<PlaceId>,

    pub projection: Option<Projection>,
}

#[derive(Clone, Debug)]
pub struct Loan {
    pub id: LoanId,

    /// Memory location being borrowed.
    pub place: PlaceId,

    pub kind: BorrowKind,

    /// Provenance carried by this borrow.
    pub provenance_id: ProvenanceId,

    /// Whether this loan is currently active.
    ///
    /// During NLL/dataflow this changes as control flow progresses.
    pub active: bool,
}

#[derive(Clone, Debug)]
pub struct Provenance {
    pub id: ProvenanceId,

    /// Original owner.
    pub origin: ProvenanceOrigin,

    /// Provenances this one was derived from.
    ///
    /// Usually empty or one element.
    ///
    /// Join points produce multiple parents.
    pub parents: Vec<ProvenanceId>,
}

#[derive(Clone, Debug)]
pub enum ProvenanceOrigin {
    Local(PlaceId),

    Field { parent: ProvenanceId, field: StrId },

    Index { parent: ProvenanceId },

    Deref { parent: ProvenanceId },

    Merge,
}

/// Flow-sensitive borrow analysis.
///
/// It answers:
///
///  - may this place be read?
///  - may this place be written?
///  - may this place be moved?
///  - what provenance does this reference carry?
#[derive(Clone)]
pub struct BorrowChecker {
    pub(super) next_place: PlaceId,
    pub(super) next_loan: LoanId,
    pub(super) next_provenance: ProvenanceId,

    pub places: FxHashMap<PlaceId, Place>,
    pub active_loans: FxHashMap<LoanId, Loan>,
    pub provenances: FxHashMap<ProvenanceId, Provenance>,

    pub local_places: FxHashMap<StrId, PlaceId>,

    /// Root local that every place belongs to.
    pub place_roots: FxHashMap<PlaceId, PlaceId>,

    /// Active loans grouped by root local.
    pub root_loans: FxHashMap<PlaceId, Vec<LoanId>>,

    pub reasoning: AliasReasoner,
    pub scopes: Vec<Scope>,
    pub place_to_local: FxHashMap<PlaceId, StrId>,
    pub place_provenance: FxHashMap<PlaceId, ProvenanceId>,
    pub(super) field_places: FxHashMap<(PlaceId, StrId), PlaceId>,
    pub(super) deref_places: FxHashMap<PlaceId, PlaceId>,
    pub(super) index_places: FxHashMap<(PlaceId, Bound), PlaceId>,
    pub pointee_origin: FxHashMap<PlaceId, (PlaceId, Interval)>,
}

#[derive(Debug, Clone)]
pub enum RefTemplate {
    /// The returned reference is a projection chain starting from one
    /// specific parameter, e.g. `&mut arr[index]` -> Path { base_param: 0,
    /// mutable: true, projections: [Index(Param(1))] }.
    Path {
        base_param: usize,
        mutable: bool,
        projections: Vec<TemplateProjection>,
    },
    /// Couldn't symbolically pin down what's returned
    Opaque,
}

#[derive(Debug, Clone)]
pub enum TemplateProjection {
    Field(StrId),
    Deref,
    Index(IndexTemplate),
}

#[derive(Debug, Clone)]
pub enum IndexTemplate {
    Param(usize),
    Const(i64),
    Opaque,
}

#[derive(Clone)]
pub struct AliasReasoner {
    pub(crate) equal: FxHashMap<Bound, FxHashSet<Bound>>,
    pub(crate) not_equal: FxHashMap<Bound, FxHashSet<Bound>>,
    pub(crate) less_than: FxHashMap<Bound, FxHashSet<Bound>>,
    pub(crate) ranges: FxHashMap<Bound, Interval>,
    pub(crate) disjoint_intervals: Vec<(Interval, Interval)>,
    pub(crate) disjoint_indexable: FxHashMap<StrId, bool>,
}

#[derive(Clone)]
pub struct Scope {
    pub loans: Vec<LoanId>,
    pub places: Vec<PlaceId>,
    pub assumed_facts: Vec<(Bound, Bound, bool)>,
}

#[derive(Clone, Debug)]
pub enum Constraint {
    /// lhs == rhs
    Equal(Bound, Bound),

    /// lhs != rhs
    NotEqual(Bound, Bound),

    /// lhs < rhs
    LessThan(Bound, Bound),

    /// lhs <= rhs
    LessEqual(Bound, Bound),

    /// A symbolic expression lies inside an interval.
    ///
    /// Example:
    ///     0 <= i < len
    Contains {
        value: Bound,
        interval: Interval,
    },

    /// Two intervals are known not to overlap.
    ///
    /// Example:
    ///     [i, i + 16)
    ///     [j, j + 16)
    /// with proof that they never intersect.
    DisjointIntervals {
        left: Interval,
        right: Interval,
    },

    Custom(ReasoningFact),
}

/// A symbolic integer expression.
///
/// Used for interval reasoning.
///
/// Examples:
///
///     0
///     len
///     i
///     i + 1
///     j - 2
///
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Bound {
    /// Constant integer.
    Const(i64),

    /// A symbolic variable.
    ///
    /// Examples:
    ///
    ///     i
    ///     j
    ///     len
    Symbol(StrId),

    /// base + offset
    ///
    /// Examples:
    ///
    ///     i + 1
    ///     len - 4
    Offset {
        base: Box<Bound>,
        offset: i64,
    },

    /// base * factor, needed for `index * elem_size`.
    Scale {
        base: Box<Bound>,
        factor: i64,
    },

    /// A read of a field on the shared base object. Sound to treat as
    /// identical across both sides of a comparison ONLY because
    /// `reason_about_indices` is only reached when both places already
    /// share the same parent place (see `relation()`), i.e. same `self`.
    SelfField(StrId),

    /// Minimum of several bounds.
    ///
    /// Useful after CFG merges.
    Min(Vec<Bound>),

    /// Maximum of several bounds.
    ///
    /// Useful after CFG merges.
    Max(Vec<Bound>),

    /// Result whose exact value we can't pin down, but which is
    /// guaranteed distinct from every other result of the same method
    /// (bump-counter / fresh-id pattern, e.g. `alloc_id`, `alloc_place_id`).
    /// Doesn't compare by value, compares by call-site identity instead.
    FreshPerCall,

    /// Analysis gave up on this subexpression.
    Opaque,

    Sum(Box<Bound>, Box<Bound>),
}

#[derive(Clone, Debug)]
pub enum ReasoningFact {
    DistinctMethodResult {
        function: StrId,

        arguments: Vec<ValueId>,
    },

    UserDefined(StrId),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Interval {
    /// Inclusive lower bound.
    pub lower: Bound,

    /// Exclusive upper bound.
    pub upper: Bound,
}

pub type BorrowResult<T> = Result<T, BorrowError>;

#[derive(Debug)]
pub enum BorrowError {
    UseAfterMove { place: PlaceId },

    MutablyBorrowed { place: PlaceId },

    AlreadyMutablyBorrowed { place: PlaceId },

    Borrowed { place: PlaceId },

    InvalidMove { place: PlaceId },

    InvalidWrite { place: PlaceId },

    InvalidRead { place: PlaceId },

    LoanNotFound(LoanId),
    PlaceNotFound(PlaceId),
    ProvenanceNotFound(ProvenanceId),
    UnknownAlias { lhs: PlaceId, rhs: PlaceId },
    CannotMoveBorrowed { place: PlaceId },
}
