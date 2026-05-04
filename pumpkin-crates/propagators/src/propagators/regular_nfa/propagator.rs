use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

#[derive(Clone, Debug)]
pub struct RegularNfaPropagatorConstructor<Var> {
    pub sequence: Box<[Var]>,
    pub num_states: u32,
    pub num_inputs: u32,
    pub transition_matrix: Vec<Vec<Vec<i32>>>,
    pub initial_state: i32,
    pub accepting_states: Vec<i32>,
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor
    for RegularNfaPropagatorConstructor<Var>
{
    type PropagatorImpl = RegularNfaPropagator<Var>;

    fn create(self, context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct RegularNfaPropagator<Var> {
    sequence: Box<[Var]>,
    num_states: u32,
    num_inputs: u32,
    transition_matrix: Vec<Vec<Vec<i32>>>,
    initial_state: i32,
    accepting_states: Vec<i32>,
}

impl<Var: IntegerVariable + 'static> Propagator for RegularNfaPropagator<Var> {
    fn name(&self) -> &str {
        todo!()
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        todo!()
    }
}
