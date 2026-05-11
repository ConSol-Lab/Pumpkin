use pumpkin_core::declare_inference_label;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::variables::IntegerVariable;

#[derive(Debug, Clone)]
pub struct AllDifferentConstructor<Var> {
    pub x: Box<[Var]>,
    pub constraint_tag: ConstraintTag,
}
declare_inference_label!(AllDifferent);

impl<Var: IntegerVariable + 'static> PropagatorConstructor for AllDifferentConstructor<Var> {
    type PropagatorImpl = AllDifferentPropagator<Var>;

    fn create(self, _context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        todo!()
    }

    fn add_inference_checkers(&self, _checkers: InferenceCheckers<'_>) {}
}

#[derive(Debug, Clone)]
pub struct AllDifferentPropagator<Var> {
    x: Box<[Var]>,
    inference_code: InferenceCode,
}

impl<Var: IntegerVariable + 'static> Propagator for AllDifferentPropagator<Var> {
    fn name(&self) -> &str {
        "AllDifferent"
    }

    fn propagate_from_scratch(
        &self,
        context: PropagationContext,
    ) -> pumpkin_core::state::PropagationStatusCP {
        todo!()
    }
}
