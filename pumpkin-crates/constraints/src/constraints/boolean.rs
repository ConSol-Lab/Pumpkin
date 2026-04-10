use pumpkin_core::ConstraintOperationError;
use pumpkin_core::Solver;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::AffineView;
use pumpkin_core::variables::DomainId;
use pumpkin_core::variables::IntegerVariableEnum;
use pumpkin_core::variables::TransformableVariable;

use super::less_than_or_equals;
use crate::equals;

/// Creates the [`Constraint`] `∑ weights_i * bools_i <= rhs`.
pub fn boolean_less_than_or_equals(
    weights: impl Into<Box<[i32]>>,
    bools: impl Into<Box<[Predicate]>>,
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

/// Creates the [`Constraint`] `∑ weights_i * bools_i == rhs`.
pub fn boolean_equals(
    weights: impl Into<Box<[i32]>>,
    bools: impl Into<Box<[Predicate]>>,
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
    bools: Box<[Predicate]>,
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
        reification_literal: Predicate,
    ) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains();

        less_than_or_equals(domains, self.rhs, self.constraint_tag)
            .implied_by(solver, reification_literal)
    }
}

impl BooleanLessThanOrEqual {
    fn create_domains(&self) -> Vec<AffineView<Predicate>> {
        self.bools
            .iter()
            .enumerate()
            .map(|(index, bool)| bool.scaled(self.weights[index]))
            .collect()
    }
}

struct BooleanEqual {
    weights: Box<[i32]>,
    bools: Box<[Predicate]>,
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
        reification_literal: Predicate,
    ) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains();

        equals(domains, 0, self.constraint_tag).implied_by(solver, reification_literal)
    }
}

impl BooleanEqual {
    fn create_domains(&self) -> Vec<IntegerVariableEnum> {
        self.bools
            .iter()
            .enumerate()
            .map(|(index, bool)| bool.scaled(self.weights[index]).into())
            .chain(std::iter::once(self.rhs.scaled(-1).into()))
            .collect()
    }
}
