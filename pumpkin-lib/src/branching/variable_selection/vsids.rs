use log::warn;

use super::VariableSelector;
use crate::basic_types::KeyValueHeap;
use crate::basic_types::StorageKey;
use crate::branching::SelectionContext;
use crate::engine::variables::DomainId;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::pumpkin_assert_eq_simple;

/// A [`VariableSelector`] which implements [VSIDS \[1\]](https://dl.acm.org/doi/pdf/10.1145/378239.379017)
/// which determines which variables should be branched on based on how often it appears in
/// conflicts.
///
/// Intuitively, the more often a variable appears in *recent* conflicts, the more "important" it is
/// during the search process.
///
/// VSIDS is originally from the SAT field (see \[1\]) but we adapted it here to work on integer
/// variables as well. There are some details which do not necessarily translate 1-to-1; for
/// example, during conflict analysis the activity of an integer variable can be bumped multiple
/// times if multiple literals related to it are encountered.
///
/// # Bibliography
/// \[1\] M. W. Moskewicz, C. F. Madigan, Y. Zhao, L. Zhang, and S. Malik, ‘Chaff: Engineering an
/// efficient SAT solver’, in Proceedings of the 38th annual Design Automation Conference, 2001, pp.
/// 530–535.
#[derive(Debug)]
pub struct Vsids<Var: StorageKey> {
    heap: KeyValueHeap<Var, f64>,
    /// How much the activity of a variable is increased when it appears in a conflict.
    /// This value changes during search (see [`Vsids::decay_activities`]).
    increment: f64,
    /// The maximum allowed [`Vsids`] value, if this value is reached then all of the values are
    /// divided by this value. This value is constant.
    max_threshold: f64,
    /// Whenever a conflict is found, the [`Vsids::increment`] is multiplied by 1 /
    /// [`Vsids::decay_factor`] (this is synonymous with increasing the
    /// [`Vsids::increment`] since 0 <= [`Vsids::decay_factor`] <= 1).
    /// This value is constant.
    decay_factor: f64,
}

const DEFAULT_VSIDS_INCREMENT: f64 = 1.0;
const DEFAULT_VSIDS_MAX_THRESHOLD: f64 = 1e100;
const DEFAULT_VSIDS_DECAY_FACTOR: f64 = 0.95;
const DEFAULT_VSIDS_VALUE: f64 = 0.0;

impl<Var: StorageKey + Clone + Copy> Vsids<Var> {
    /// Creates a new instance of the [`Vsids`] [`VariableSelector`] with certain default values for
    /// the parameters (`1.0` for the increment, `1e100` for the max threshold,
    /// `0.95` for the decay factor and `0.0` for the initial VSIDS value).
    pub fn new(variables: &[Var]) -> Self {
        if variables.is_empty() {
            warn!("The VSIDS variable selector was not provided with any variables");
            return Vsids {
                heap: KeyValueHeap::default(),
                increment: DEFAULT_VSIDS_INCREMENT,
                max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
                decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
            };
        }
        let mut result = Vsids {
            heap: KeyValueHeap::default(),
            increment: DEFAULT_VSIDS_INCREMENT,
            max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
            decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
        };
        for index in 0..=variables
            .iter()
            .map(|variable| variable.index())
            .max()
            .unwrap()
        {
            result
                .heap
                .grow(Var::create_from_index(index), DEFAULT_VSIDS_VALUE);
        }

        result
    }

    /// Creates a new instance of the [`Vsids`] [`VariableSelector`] with certain default values for
    /// the parameters (`1.0` for the increment, `1e100` for the max threshold and
    /// `0.95` for the decay factor). It initialises the internal max-heap structure used for
    /// finding the maximum `Var` with the provided `initial_values`; this parameter can thus be
    /// used to guide the early search process of the selector.
    ///
    /// It is required that the length of `variables` is equal to the length of `initial_values`.
    pub fn with_initial_values(variables: &[Var], initial_values: &[f64]) -> Self {
        if variables.is_empty() {
            warn!("The VSIDS variable selector was not provided with any variables");
            return Vsids {
                heap: KeyValueHeap::default(),
                increment: DEFAULT_VSIDS_INCREMENT,
                max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
                decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
            };
        }
        pumpkin_assert_eq_simple!(variables.len(), initial_values.len());
        let mut result = Vsids {
            heap: KeyValueHeap::default(),
            increment: DEFAULT_VSIDS_INCREMENT,
            max_threshold: DEFAULT_VSIDS_MAX_THRESHOLD,
            decay_factor: DEFAULT_VSIDS_DECAY_FACTOR,
        };

        let mut sorted_indices = variables
            .iter()
            .map(|variable| variable.index())
            .collect::<Vec<_>>();
        sorted_indices.sort();

        let mut current_index = 0;

        for index in 0..=*sorted_indices.last().unwrap() {
            if index == sorted_indices[current_index] {
                result
                    .heap
                    .grow(Var::create_from_index(index), initial_values[current_index]);
                current_index += 1;
            } else {
                result
                    .heap
                    .grow(Var::create_from_index(index), DEFAULT_VSIDS_VALUE);
            }
        }

        result
    }

    /// Bumps the activity of a variable after it has been encountered during a conflict by
    /// [`Vsids::increment`]
    fn bump_activity(&mut self, variable: Var) {
        self.heap.accomodate(variable, DEFAULT_VSIDS_VALUE);
        // Scale the activities if the values are too large
        let activity = self.heap.get_value(variable);
        if activity + self.increment >= self.max_threshold {
            self.heap.divide_values(self.max_threshold);
            self.increment /= self.max_threshold;
        }
        // Now perform the standard bumping
        self.heap.increment(variable, self.increment);
    }

    /// Restores a variable under consideration after backtracking
    fn restore(&mut self, variable: Var) {
        self.heap.accomodate(variable, DEFAULT_VSIDS_VALUE);
        self.heap.restore_key(variable);
    }

    /// Decays the activities (i.e. increases the [`Vsids::increment`] by multiplying it
    /// with 1 / [`Vsids::decay_factor`]) such that future bumps (see
    /// [`Vsids::bump_activity`]) is more impactful.
    ///
    /// Doing it in this manner is cheaper than dividing each activity value eagerly.
    fn decay_activities(&mut self) {
        self.increment *= 1.0 / self.decay_factor;
    }
}

impl VariableSelector<DomainId> for Vsids<DomainId> {
    fn select_variable(&mut self, context: &SelectionContext) -> Option<DomainId> {
        loop {
            // We peek the first variable, note that we do not pop since we do not (yet) want to
            // remove the value from the heap
            if let Some((candidate, _)) = self.heap.peek_max() {
                if context.is_integer_fixed(*candidate) {
                    let _ = self.heap.pop_max();
                } else {
                    return Some(*candidate);
                }
            } else {
                return None;
            }
        }
    }

    fn on_conflict(&mut self) {
        self.decay_activities()
    }

    fn on_unassign_integer(&mut self, variable: DomainId, _value: i32) {
        self.restore(variable)
    }

    fn on_appearance_in_conflict_integer(&mut self, variable: DomainId) {
        self.bump_activity(variable)
    }
}

impl VariableSelector<PropositionalVariable> for Vsids<PropositionalVariable> {
    fn select_variable(&mut self, context: &SelectionContext) -> Option<PropositionalVariable> {
        loop {
            if let Some((candidate, _)) = self.heap.peek_max() {
                if context.is_propositional_variable_fixed(*candidate) {
                    let _ = self.heap.pop_max();
                } else {
                    return Some(*candidate);
                }
            } else {
                return None;
            }
        }
    }

    fn on_conflict(&mut self) {
        self.decay_activities()
    }

    fn on_unassign_literal(&mut self, literal: Literal) {
        self.restore(literal.get_propositional_variable())
    }

    fn on_appearance_in_conflict_literal(&mut self, literal: Literal) {
        self.bump_activity(literal.get_propositional_variable())
    }
}

#[cfg(test)]
mod tests {
    use super::Vsids;
    use crate::basic_types::tests::TestRandom;
    use crate::basic_types::StorageKey;
    use crate::branching::variable_selection::VariableSelector;
    use crate::branching::SelectionContext;
    use crate::engine::variables::PropositionalVariable;
    use crate::variables::Literal;

    #[test]
    fn vsids_bumped_var_is_max() {
        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(2, 0, None);
        let mut test_rng = TestRandom::default();
        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let domains = context.get_domains().collect::<Vec<_>>();

        let mut vsids = Vsids::new(&domains);
        vsids.bump_activity(domains[1]);

        let chosen = vsids.select_variable(&context);

        assert!(chosen.is_some());
        assert_eq!(chosen.unwrap(), domains[1]);
    }

    #[test]
    fn vsids_no_variables_will_return_none() {
        let mut vsids: Vsids<PropositionalVariable> = Vsids::new(&Vec::new());

        let (assignments_integer, assignments_propositional) =
            SelectionContext::create_for_testing(0, 0, None);
        let mut test_rng = TestRandom::default();
        let context = SelectionContext::new(
            &assignments_integer,
            &assignments_propositional,
            &mut test_rng,
        );
        let chosen = vsids.select_variable(&context);

        assert!(chosen.is_none());
    }

    #[test]
    fn does_not_panic_with_unknown_on_unassign() {
        let mut vsids: Vsids<PropositionalVariable> = Vsids::new(&[]);

        let variable = PropositionalVariable::create_from_index(0);

        assert_eq!(vsids.heap.len(), 0);
        vsids.on_unassign_literal(Literal::new(variable, true));
        assert_eq!(vsids.heap.len(), 1);
    }

    #[test]
    fn does_not_panic_with_unknown_on_appearance_in_conflict() {
        let mut vsids: Vsids<PropositionalVariable> = Vsids::new(&[]);

        let variable = PropositionalVariable::create_from_index(0);

        assert_eq!(vsids.heap.len(), 0);
        vsids.on_appearance_in_conflict_literal(Literal::new(variable, true));
        assert_eq!(vsids.heap.len(), 1);
    }
}
