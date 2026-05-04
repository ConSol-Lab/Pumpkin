use pumpkin_core::constraints::Constraint;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::regular_nfa::RegularNfaPropagatorConstructor;

pub fn regular_nfa<Var: IntegerVariable + 'static>(
    sequence: impl Into<Box<[Var]>>,
    num_states: u32,
    num_inputs: u32,
    transition_matrix: Vec<Vec<Vec<i32>>>,
    initial_state: i32,
    accepting_states: Vec<i32>,
) -> impl Constraint {
    RegularNfaPropagatorConstructor {
        sequence: sequence.into(),
        num_states,
        num_inputs,
        transition_matrix,
        initial_state,
        accepting_states,
    }
}
