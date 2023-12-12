use pumpkin_lib::{
    basic_types::variables::IntVar, engine::ConstraintSatisfactionSolver, propagators::LinearNe,
};

pub fn int_lin_ne<Var: IntVar + std::fmt::Debug + 'static>(
    solver: &mut ConstraintSatisfactionSolver,
    terms: Box<[Var]>,
    rhs: i32,
) {
    solver.add_propagator(LinearNe { terms, rhs });
}
