use super::Constraint;
use super::equals;
use super::less_than_or_equals;
use crate::ConstraintOperationError;
use crate::Solver;
use crate::proof::ConstraintTag;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::Literal;
use crate::variables::TransformableVariable;

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
    fn create_domains(&self) -> Vec<AffineView<DomainId>> {
        self.bools
            .iter()
            .enumerate()
            .map(|(index, bool)| bool.get_integer_variable().scaled(self.weights[index]))
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
    fn create_domains(&self) -> Vec<AffineView<DomainId>> {
        self.bools
            .iter()
            .enumerate()
            .map(|(index, bool)| bool.get_integer_variable().scaled(self.weights[index]))
            .chain(std::iter::once(self.rhs.scaled(-1)))
            .collect()
    }
}
