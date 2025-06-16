use super::equals;
use super::less_than_or_equals;
use super::Constraint;
use crate::predicates::PredicateConstructor;
use crate::proof::ConstraintTag;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::IntegerVariable;
use crate::variables::Literal;
use crate::variables::TransformableVariable;
use crate::ConstraintOperationError;
use crate::Solver;

/// Creates the [`Constraint`] `\sum weights_i * bools_i <= rhs`.
pub fn boolean_less_than_or_equals(
    weights: impl Into<Box<[i32]>>,
    bools: impl Into<Box<[Literal]>>,
    rhs: i32,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    BooleanLessThanOrEqual {
        weights: weights.into(),
        bools: bools.into(),
        rhs,
        constraint_tag,
    }
}

/// Creates the [`Constraint`] `\sum weights_i * bools_i == rhs`.
pub fn boolean_equals(
    weights: impl Into<Box<[i32]>>,
    bools: impl Into<Box<[Literal]>>,
    rhs: DomainId,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    BooleanEqual {
        weights: weights.into(),
        bools: bools.into(),
        rhs,
        constraint_tag,
    }
}

struct BooleanLessThanOrEqual {
    weights: Box<[i32]>,
    bools: Box<[Literal]>,
    rhs: i32,
    constraint_tag: ConstraintTag,
}

impl Constraint for BooleanLessThanOrEqual {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains();

        less_than_or_equals(domains, self.rhs, self.constraint_tag).post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains();

        less_than_or_equals(domains, self.rhs, self.constraint_tag)
            .implied_by(solver, reification_literal)
    }
}

impl BooleanLessThanOrEqual {
    fn create_domains(&self) -> Vec<impl IntegerVariable> {
        self.bools
            .iter()
            .enumerate()
            .map(|(index, bool)| bool.scaled(self.weights[index]))
            .collect()
    }
}

struct BooleanEqual {
    weights: Box<[i32]>,
    bools: Box<[Literal]>,
    rhs: DomainId,
    constraint_tag: ConstraintTag,
}

impl Constraint for BooleanEqual {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains();

        equals(domains, 0, self.constraint_tag).post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains();

        equals(domains, 0, self.constraint_tag).implied_by(solver, reification_literal)
    }
}

impl BooleanEqual {
    fn create_domains(&self) -> Vec<BoolOrIntVariable> {
        self.bools
            .iter()
            .enumerate()
            .map(|(index, bool)| bool.scaled(self.weights[index]).into())
            .chain(std::iter::once(self.rhs.scaled(-1).into()))
            .collect()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BoolOrIntVariable {
    Int(AffineView<DomainId>),
    Bool(AffineView<Literal>),
}

impl From<AffineView<Literal>> for BoolOrIntVariable {
    fn from(value: AffineView<Literal>) -> Self {
        todo!()
    }
}

impl From<AffineView<DomainId>> for BoolOrIntVariable {
    fn from(value: AffineView<DomainId>) -> Self {
        todo!()
    }
}

impl PredicateConstructor for BoolOrIntVariable {
    type Value = i32;

    fn lower_bound_predicate(&self, bound: Self::Value) -> crate::predicates::Predicate {
        todo!()
    }

    fn upper_bound_predicate(&self, bound: Self::Value) -> crate::predicates::Predicate {
        todo!()
    }

    fn equality_predicate(&self, bound: Self::Value) -> crate::predicates::Predicate {
        todo!()
    }

    fn disequality_predicate(&self, bound: Self::Value) -> crate::predicates::Predicate {
        todo!()
    }
}

impl TransformableVariable<BoolOrIntVariable> for BoolOrIntVariable {
    fn scaled(&self, scale: i32) -> BoolOrIntVariable {
        todo!()
    }

    fn offset(&self, offset: i32) -> BoolOrIntVariable {
        todo!()
    }
}

impl IntegerVariable for BoolOrIntVariable {
    type AffineView = BoolOrIntVariable;

    fn lower_bound(&self, assignment: &crate::engine::Assignments) -> i32 {
        todo!()
    }

    fn lower_bound_at_trail_position(
        &self,
        assignment: &crate::engine::Assignments,
        trail_position: usize,
    ) -> i32 {
        todo!()
    }

    fn upper_bound(&self, assignment: &crate::engine::Assignments) -> i32 {
        todo!()
    }

    fn upper_bound_at_trail_position(
        &self,
        assignment: &crate::engine::Assignments,
        trail_position: usize,
    ) -> i32 {
        todo!()
    }

    fn contains(&self, assignment: &crate::engine::Assignments, value: i32) -> bool {
        todo!()
    }

    fn contains_at_trail_position(
        &self,
        assignment: &crate::engine::Assignments,
        value: i32,
        trail_position: usize,
    ) -> bool {
        todo!()
    }

    fn iterate_domain<'a>(
        &self,
        assignment: &'a crate::engine::Assignments,
    ) -> impl Iterator<Item = i32> + 'a {
        match self {
            BoolOrIntVariable::Int(affine_view) => {
                DynIntIterator(Box::new(affine_view.iterate_domain(assignment)))
            }
            BoolOrIntVariable::Bool(affine_view) => {
                DynIntIterator(Box::new(affine_view.iterate_domain(assignment)))
            }
        }
    }

    fn watch_all(
        &self,
        watchers: &mut crate::engine::notifications::Watchers<'_>,
        events: enumset::EnumSet<crate::engine::notifications::DomainEvent>,
    ) {
        todo!()
    }

    fn watch_all_backtrack(
        &self,
        watchers: &mut crate::engine::notifications::Watchers<'_>,
        events: enumset::EnumSet<crate::engine::notifications::DomainEvent>,
    ) {
        todo!()
    }

    fn unpack_event(
        &self,
        event: crate::engine::notifications::OpaqueDomainEvent,
    ) -> crate::engine::notifications::DomainEvent {
        todo!()
    }
}

struct DynIntIterator<'a>(Box<dyn Iterator<Item = i32> + 'a>);

impl Iterator for DynIntIterator<'_> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
