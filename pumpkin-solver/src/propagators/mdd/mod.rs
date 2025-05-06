mod options;
use log::warn;
use mdd_compile::mdd::MddGraph;
pub use options::*;

use crate::engine::propagation::Propagator;

pub(crate) struct MddPropagator<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> {
    mdd: MddGraph<Var>,
}

impl<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> MddPropagator<Var> {
    pub(crate) fn new(mdd: MddGraph<Var>) -> Self {
        Self { mdd }
    }
}

impl<Var: std::fmt::Debug + Clone + std::hash::Hash + Eq + 'static> Propagator
    for MddPropagator<Var>
{
    fn name(&self) -> &str {
        "DecisionDiagram"
    }

    fn propagate(
        &mut self,
        _context: crate::engine::propagation::PropagationContextMut,
    ) -> crate::basic_types::PropagationStatusCP {
        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        _context: crate::engine::propagation::PropagationContextMut,
    ) -> crate::basic_types::PropagationStatusCP {
        Ok(())
    }

    fn initialise_at_root(
        &mut self,
        _context: &mut crate::engine::propagation::PropagatorInitialisationContext,
    ) -> Result<(), crate::predicates::PropositionalConjunction> {
        warn!(
            "MDD with variables {:?} and {} transitions; not implemented yet",
            self.mdd.layers,
            self.mdd.transitions.len()
        );
        Ok(())
    }
}
