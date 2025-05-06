use mdd_compile::mdd::MddGraph;

use super::Constraint;
use crate::propagators::mdd::MddPropagator;
use crate::variables::IntegerVariable;

pub fn mdd<Var: std::fmt::Debug + IntegerVariable + std::hash::Hash + Eq + 'static>(
    mdd_graph: MddGraph<Var>,
) -> impl Constraint {
    MddPropagator::new(mdd_graph)
}
