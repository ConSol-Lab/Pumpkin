use super::equals;
use super::less_than_or_equals;
use super::Constraint;
use crate::predicate;
use crate::variables::AffineView;
use crate::variables::DomainId;
use crate::variables::Literal;
use crate::variables::TransformableVariable;
use crate::ConstraintOperationError;
use crate::Solver;

/// Creates the constraint `\sum weights_i * bools_i <= rhs`.
pub fn boolean_less_than_or_equals(
    weights: impl Into<Box<[i32]>>,
    bools: impl Into<Box<[Literal]>>,
    rhs: i32,
) -> impl Constraint {
    BooleanLessThanOrEqual {
        weights: weights.into(),
        bools: bools.into(),
        rhs,
    }
}

/// Creates the constraint `\sum weights_i * bools_i <= rhs`.
pub fn boolean_equals(
    weights: impl Into<Box<[i32]>>,
    bools: impl Into<Box<[Literal]>>,
    rhs: DomainId,
) -> impl Constraint {
    BooleanEqual {
        weights: weights.into(),
        bools: bools.into(),
        rhs,
    }
}

struct BooleanLessThanOrEqual {
    weights: Box<[i32]>,
    bools: Box<[Literal]>,
    rhs: i32,
}

impl Constraint for BooleanLessThanOrEqual {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains(solver);

        less_than_or_equals(domains, self.rhs).post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains(solver);

        less_than_or_equals(domains, self.rhs).implied_by(solver, reification_literal)
    }
}

impl BooleanLessThanOrEqual {
    fn create_domains(&self, solver: &mut Solver) -> Vec<AffineView<DomainId>> {
        // todo: create vector with single type
        todo!()
        // let domains = self
        //     .bools
        //     .iter()
        //     .enumerate()
        //     .map(|(index, bool)| {
        //         bool.scaled(self.weights[index])
        //     })
        //     .collect::<Vec<_>>();
        // domains
    }
}

struct BooleanEqual {
    weights: Box<[i32]>,
    bools: Box<[Literal]>,
    rhs: DomainId,
}

impl Constraint for BooleanEqual {
    fn post(self, solver: &mut Solver) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains(solver);

        equals(domains, 0).post(solver)
    }

    fn implied_by(
        self,
        solver: &mut Solver,
        reification_literal: Literal,
    ) -> Result<(), ConstraintOperationError> {
        let domains = self.create_domains(solver);

        equals(domains, 0).implied_by(solver, reification_literal)
    }
}

impl BooleanEqual {
    fn create_domains(&self, solver: &mut Solver) -> Vec<AffineView<DomainId>> {
        // todo: create vector with single type
        todo!()
        // self.bools
        //     .iter()
        //     .enumerate()
        //     .map(|(index, bool)| bool.scaled(self.weights[index]))
        //     .chain(std::iter::once(self.rhs.scaled(-1)))
        //     .collect()
    }
}
