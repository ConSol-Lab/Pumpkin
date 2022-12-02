use crate::{
    basic_types::{
        EnqueueStatus, IntegerVariable, Predicate, PropagationStatusCP, PropositionalConjunction,
    },
    engine::DomainManager,
    pumpkin_asserts::pumpkin_assert_simple,
};

use super::ConstraintProgrammingPropagator;

//propagator for the constraint \sum w_i * x_i >= c
pub struct SimpleLinearInequalityPropagator {
    terms: Vec<Term>,
    right_hand_side: i64,
    propagation_reasons: Vec<Vec<PropositionalConjunction>>, //propagation reasons is are stored eagerly into a direct hash, i.e., [var_id][value] = c, where c is the reason for propagating integer variable with id 'var_id' given the value 'value'
}

impl SimpleLinearInequalityPropagator {
    pub fn new(
        integer_variables: &[IntegerVariable],
        weights: &[i64],
        right_hand_side: i64,
    ) -> SimpleLinearInequalityPropagator {
        pumpkin_assert_simple!(!integer_variables.is_empty());
        pumpkin_assert_simple!(integer_variables.len() == weights.len());

        let terms = integer_variables
            .iter()
            .zip(weights.iter())
            .filter_map(|x| {
                if *x.1 == 0 {
                    None
                } else {
                    Some(Term {
                        weight: *x.1,
                        integer_variable: *x.0,
                    })
                }
            })
            .collect();

        SimpleLinearInequalityPropagator {
            terms,
            right_hand_side,
            propagation_reasons: vec![], //initialisation of propagation_reasons will be done in initialise_at_root method since only then do we have access to the domain info on variables
        }
    }

    fn create_naive_reason(
        &self,
        variable_to_exclude: Option<IntegerVariable>,
        domains: &DomainManager,
    ) -> PropositionalConjunction {
        self.terms
            .iter()
            .filter_map(|t| {
                if variable_to_exclude == Some(t.integer_variable) {
                    None
                } else if t.weight > 0 {
                    Some(domains.get_upper_bound_predicate(t.integer_variable))
                } else {
                    Some(domains.get_lower_bound_predicate(t.integer_variable))
                }
            })
            .collect::<Vec<Predicate>>()
            .into()
    }

    fn compute_slack_from_scratch(&self, domains: &DomainManager) -> i64 {
        //compute current slack from scratch
        //  this is done by setting the variables to their optimistic values (the value that helps satisfy the constraint the most)
        //  recall that the slack indicates how much can the variable differ from their ideal values before the constraint becomes violated
        let optimistic_left_hand_side_value: i64 = self
            .terms
            .iter()
            .map(|t| {
                if t.weight > 0 {
                    t.weight * (domains.get_upper_bound(t.integer_variable) as i64)
                } else {
                    t.weight * (domains.get_lower_bound(t.integer_variable) as i64)
                }
            })
            .sum();

        optimistic_left_hand_side_value - self.right_hand_side
    }
}

struct Term {
    pub weight: i64,
    pub integer_variable: IntegerVariable,
}

impl ConstraintProgrammingPropagator for SimpleLinearInequalityPropagator {
    fn propagate(&mut self, domains: &mut DomainManager) -> PropagationStatusCP {
        let slack: i64 = self.compute_slack_from_scratch(domains);

        //in case the slack is negative, the constraint cannot be satisfied
        if slack < 0 {
            return PropagationStatusCP::ConflictDetected {
                failure_reason: self.create_naive_reason(None, domains),
            };
        }

        //otherwise, the constraint may be satisfied
        //  now propagate bounds on the variables
        for term in &self.terms {
            //positive terms get their lower bounds constrained
            if term.weight > 0 {
                let lower_bound = domains.get_lower_bound(term.integer_variable) as i64;
                let upper_bound = domains.get_upper_bound(term.integer_variable) as i64;
                let new_lower_bound = upper_bound - (slack / term.weight);

                if new_lower_bound > lower_bound {
                    domains.tighten_lower_bound(term.integer_variable, new_lower_bound as i32);

                    self.propagation_reasons[term.integer_variable][new_lower_bound as usize] =
                        self.create_naive_reason(Some(term.integer_variable), domains);
                }
            //negative terms get their upper bounds contrained
            } else {
                let lower_bound = domains.get_lower_bound(term.integer_variable) as i64;
                let upper_bound = domains.get_upper_bound(term.integer_variable) as i64;
                let new_upper_bound = lower_bound + (slack / -term.weight); //note the minus in front of the weight!

                if new_upper_bound < upper_bound {
                    domains.tighten_upper_bound(term.integer_variable, new_upper_bound as i32);

                    self.propagation_reasons[term.integer_variable][new_upper_bound as usize] =
                        self.create_naive_reason(Some(term.integer_variable), domains);
                }
            }
        }
        PropagationStatusCP::NoConflictDetected
    }

    fn debug_propagate_from_scratch(&self, domains: &mut DomainManager) -> PropagationStatusCP {
        let slack: i64 = self.compute_slack_from_scratch(domains);

        //in case the slack is negative, the constraint cannot be satisfied
        if slack < 0 {
            return PropagationStatusCP::ConflictDetected {
                failure_reason: self.create_naive_reason(None, domains),
            };
        }

        //otherwise, the constraint may be satisfied
        //  now propagate bounds on the variables
        for term in &self.terms {
            //positive terms get their lower bounds constrained
            if term.weight > 0 {
                let lower_bound = domains.get_lower_bound(term.integer_variable) as i64;
                let upper_bound = domains.get_upper_bound(term.integer_variable) as i64;
                let new_lower_bound = upper_bound - (slack / term.weight);

                if new_lower_bound > lower_bound {
                    domains.tighten_lower_bound(term.integer_variable, new_lower_bound as i32);
                }
            //negative terms get their upper bounds contrained
            } else {
                let lower_bound = domains.get_lower_bound(term.integer_variable) as i64;
                let upper_bound = domains.get_upper_bound(term.integer_variable) as i64;
                let new_upper_bound = lower_bound + (slack / -term.weight); //note the minus in front of the weight!

                if new_upper_bound < upper_bound {
                    domains.tighten_upper_bound(term.integer_variable, new_upper_bound as i32);
                }
            }
        }

        PropagationStatusCP::NoConflictDetected
    }

    fn synchronise(&mut self, _domains: &DomainManager) {}

    fn notify_lower_bound_integer_variable_change(
        &mut self,
        _integer_variable: crate::basic_types::IntegerVariable,
        _old_lower_bound: i32,
        _new_lower_bound: i32,
        _domains: &DomainManager,
    ) -> EnqueueStatus {
        //in this naive implement we also enqueue
        //  in a better implementation, enqueueing would only be done if the slack changes enough so that there is potential for propagation
        EnqueueStatus::ShouldEnqueue
    }

    fn notify_upper_bound_integer_variable_change(
        &mut self,
        _integer_variable: crate::basic_types::IntegerVariable,
        _old_upper_bound: i32,
        _new_upper_bound: i32,
        _domains: &DomainManager,
    ) -> EnqueueStatus {
        //in this naive implement we also enqueue
        //  in a better implementation, enqueueing would only be done if the slack changes enough so that there is potential for propagation
        EnqueueStatus::ShouldEnqueue
    }

    fn notify_domain_hole_integer_variable_change(
        &mut self,
        _integer_variable: IntegerVariable,
        _removed_value_from_domain: i32,
        _domains: &DomainManager,
    ) -> EnqueueStatus {
        panic!("This propagator should not be subscribed to domain hole changes!");
    }

    fn get_reason_for_propagation(&mut self, predicate: Predicate) -> PropositionalConjunction {
        self.propagation_reasons[predicate.get_integer_variable()]
            [predicate.get_right_hand_side() as usize]
            .clone()
    }

    fn priority(&self) -> u32 {
        0
    }

    fn initialise_at_root(&mut self, domains: &mut DomainManager) -> PropagationStatusCP {
        let max_integer_id = self
            .terms
            .iter()
            .max_by_key(|x| x.integer_variable.id)
            .unwrap()
            .integer_variable
            .id as usize;

        self.propagation_reasons.resize(max_integer_id + 1, vec![]);
        for integer_variable in self.terms.iter().map(|x| x.integer_variable) {
            let upper_bound = domains.get_upper_bound(integer_variable) as usize;
            self.propagation_reasons[integer_variable]
                .resize(upper_bound + 1, PropositionalConjunction::new());
        }

        self.propagate(domains)
    }

    fn name(&self) -> &str {
        "Simple Linear Inequality Propagator"
    }

    fn get_integer_variables_to_watch_for_lower_bound_changes(&self) -> Vec<IntegerVariable> {
        //for variables with negative weights, changes in their lower bound result in changes to the slack
        //  so the propagator watches the changes in the lower bound
        self.terms
            .iter()
            .filter_map(|t| {
                if t.weight < 0 {
                    Some(t.integer_variable)
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_integer_variables_to_watch_for_upper_bound_changes(&self) -> Vec<IntegerVariable> {
        //for variables with positive weights, changes in their upper bound result in changes to the slack
        //  so the propagator watches the changes in the upper bound
        self.terms
            .iter()
            .filter_map(|t| {
                if t.weight > 0 {
                    Some(t.integer_variable)
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_integer_variables_to_watch_for_domain_hole_changes(&self) -> Vec<IntegerVariable> {
        //holes in the domain have no effect on propagation
        vec![]
    }
}
