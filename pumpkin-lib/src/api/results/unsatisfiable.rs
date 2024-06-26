use crate::{
    branching::Brancher,
    engine::{variables::Literal, ConstraintSatisfactionSolver},
};

#[derive(Debug)]
#[allow(unused)]
pub struct UnsatisfiableUnderAssumptions<'solver, 'brancher, B> {
    solver: &'solver mut ConstraintSatisfactionSolver,
    brancher: &'brancher mut B,
}

impl<'solver, 'brancher, B: Brancher> UnsatisfiableUnderAssumptions<'solver, 'brancher, B> {
    /// Extract the unsatisfiable core in terms of the assumptions.
    pub fn extract_core(&mut self) -> Box<[Literal]> {
        self.solver
            .extract_clausal_core(self.brancher)
            .expect("expected consistent assumptions")
            .into()
    }
}
