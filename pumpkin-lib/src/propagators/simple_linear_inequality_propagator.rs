use crate::{
    basic_types::{variables::IntVar, Predicate, PropagationStatusCP, PropositionalConjunction},
    engine::{DomainManager, Watchers},
    pumpkin_assert_simple,
};

use super::ConstraintProgrammingPropagator;

//propagator for the constraint \sum w_i * x_i >= c
pub struct SimpleLinearInequalityPropagator<Var> {
    terms: Vec<Term<Var>>,
    right_hand_side: i64,
    propagation_reasons: Vec<Vec<PropositionalConjunction>>, //propagation reasons is are stored eagerly into a direct hash, i.e., [var_id][value] = c, where c is the reason for propagating integer variable with id 'var_id' given the value 'value'
}

impl<Var: IntVar> SimpleLinearInequalityPropagator<Var> {
    pub fn new(
        variables: &[Var],
        weights: &[i64],
        right_hand_side: i64,
    ) -> SimpleLinearInequalityPropagator<Var> {
        pumpkin_assert_simple!(!variables.is_empty());
        pumpkin_assert_simple!(variables.len() == weights.len());

        let terms = variables
            .iter()
            .zip(weights.iter())
            .filter_map(|x| {
                if *x.1 == 0 {
                    None
                } else {
                    Some(Term {
                        weight: *x.1,
                        variable: x.0.clone(),
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
        term_idx_to_exclude: Option<usize>,
        domains: &DomainManager,
    ) -> PropositionalConjunction {
        self.terms
            .iter()
            .enumerate()
            .filter_map(|(idx, t)| {
                if Some(idx) == term_idx_to_exclude {
                    None
                } else if t.weight > 0 {
                    Some(
                        t.variable
                            .upper_bound_predicate(t.variable.upper_bound(domains)),
                    )
                } else {
                    Some(
                        t.variable
                            .lower_bound_predicate(t.variable.lower_bound(domains)),
                    )
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
                    t.weight * (t.variable.upper_bound(domains) as i64)
                } else {
                    t.weight * (t.variable.lower_bound(domains) as i64)
                }
            })
            .sum();

        optimistic_left_hand_side_value - self.right_hand_side
    }
}

struct Term<Var> {
    pub weight: i64,
    pub variable: Var,
}

impl<Var: IntVar> ConstraintProgrammingPropagator for SimpleLinearInequalityPropagator<Var> {
    fn propagate(&mut self, domains: &mut DomainManager) -> PropagationStatusCP {
        let slack: i64 = self.compute_slack_from_scratch(domains);

        //in case the slack is negative, the constraint cannot be satisfied
        if slack < 0 {
            return Err(self.create_naive_reason(None, domains));
        }

        //otherwise, the constraint may be satisfied
        //  now propagate bounds on the variables
        for (idx, term) in self.terms.iter().enumerate() {
            //positive terms get their lower bounds constrained
            if term.weight > 0 {
                let lower_bound = term.variable.lower_bound(domains) as i64;
                let upper_bound = term.variable.upper_bound(domains) as i64;
                let new_lower_bound = upper_bound - (slack / term.weight);

                if new_lower_bound > lower_bound {
                    term.variable
                        .set_lower_bound(domains, new_lower_bound as i32);

                    self.propagation_reasons[idx][new_lower_bound as usize] =
                        self.create_naive_reason(Some(idx), domains);
                }
            //negative terms get their upper bounds contrained
            } else {
                let lower_bound = term.variable.lower_bound(domains) as i64;
                let upper_bound = term.variable.upper_bound(domains) as i64;
                let new_upper_bound = lower_bound + (slack / -term.weight); //note the minus in front of the weight!

                if new_upper_bound < upper_bound {
                    term.variable
                        .set_upper_bound(domains, new_upper_bound as i32);

                    self.propagation_reasons[idx][new_upper_bound as usize] =
                        self.create_naive_reason(Some(idx), domains);
                }
            }
        }
        Ok(())
    }

    fn debug_propagate_from_scratch(&self, domains: &mut DomainManager) -> PropagationStatusCP {
        let slack: i64 = self.compute_slack_from_scratch(domains);

        //in case the slack is negative, the constraint cannot be satisfied
        if slack < 0 {
            return Err(self.create_naive_reason(None, domains));
        }

        //otherwise, the constraint may be satisfied
        //  now propagate bounds on the variables
        for term in &self.terms {
            //positive terms get their lower bounds constrained
            if term.weight > 0 {
                let lower_bound = term.variable.lower_bound(domains) as i64;
                let upper_bound = term.variable.upper_bound(domains) as i64;
                let new_lower_bound = upper_bound - (slack / term.weight);

                if new_lower_bound > lower_bound {
                    term.variable
                        .set_lower_bound(domains, new_lower_bound as i32);
                }
            //negative terms get their upper bounds contrained
            } else {
                let lower_bound = term.variable.lower_bound(domains) as i64;
                let upper_bound = term.variable.upper_bound(domains) as i64;
                let new_upper_bound = lower_bound + (slack / -term.weight); //note the minus in front of the weight!

                if new_upper_bound < upper_bound {
                    term.variable
                        .set_upper_bound(domains, new_upper_bound as i32);
                }
            }
        }

        Ok(())
    }

    fn synchronise(&mut self, _domains: &DomainManager) {}

    fn get_reason_for_propagation(&mut self, predicate: Predicate) -> PropositionalConjunction {
        self.propagation_reasons[predicate.get_integer_variable()]
            [predicate.get_right_hand_side() as usize]
            .clone()
    }

    fn priority(&self) -> u32 {
        0
    }

    fn initialise_at_root(&mut self, domains: &mut DomainManager) -> PropagationStatusCP {
        self.propagation_reasons.resize(self.terms.len(), vec![]);
        for (idx, variable) in self.terms.iter().map(|x| &x.variable).enumerate() {
            let upper_bound = variable.upper_bound(domains) as usize;
            self.propagation_reasons[idx]
                .resize(upper_bound + 1, PropositionalConjunction::default());
        }

        self.propagate(domains)
    }

    fn name(&self) -> &str {
        "Simple Linear Inequality Propagator"
    }

    fn register_watches(&self, _watchers: &mut Watchers<'_>) {
        todo!()
    }
}
