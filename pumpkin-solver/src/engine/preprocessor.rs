use super::AssignmentsPropositional;
use crate::engine::variables::Literal;

#[derive(Debug, Copy, Clone)]
pub(crate) struct Preprocessor {}

impl Preprocessor {
    // does simple preprocessing, modifying the input vector of literals
    // 	removes duplicate literals
    // 	removes falsified literals at the root
    // 	if the same variable appears with both polarities or there is a literal that is true at the
    // root, removes all literals from the clause and adds a literal that is true at the root 	if
    // the clause is violated at the root, it will become empty  if the clause is satisfied at
    // the root, its content will be changed to only include the true_literal this preprocessing
    // is also for correctness, i.e., clauses should not have duplicated literals for instance
    pub(crate) fn preprocess_clause(
        mut literals: Vec<Literal>,
        assignments: &AssignmentsPropositional,
    ) -> Vec<Literal> {
        // the code below is broken down into several parts, could be done more efficiently but
        // probably makes no/little difference

        // remove literals that are falsified at the root level
        // also check if the clause has a true literal at the root level
        let mut satisfied_at_root = false;
        let mut next_location = 0;
        for i in 0..literals.len() {
            if assignments.is_literal_assigned_true(literals[i]) {
                satisfied_at_root = true;
                break;
            }
            // skip falsified literals, only keep unassigned literals
            else if assignments.is_literal_unassigned(literals[i]) {
                literals[next_location] = literals[i];
                next_location += 1;
            }
        }
        literals.truncate(next_location);

        // if satisfied at the root, then remove all literals, add a true literal to the clause, and
        // stop
        if satisfied_at_root {
            literals.resize(1, assignments.true_literal);
            literals[0] = assignments.true_literal;
            return literals;
        }
        // in case the literal is empty, it is unsatisfied at the root, stop
        else if literals.is_empty() {
            return literals;
        }

        // we now remove duplicated literals
        // 	the easiest way is to sort the literals and only keep one literal of the same type
        literals.sort_unstable_by_key(|a| a.to_u32());
        next_location = 1;
        for i in 1..literals.len() {
            if literals[i] != literals[next_location - 1] {
                literals[next_location] = literals[i];
                next_location += 1;
            }
        }
        literals.truncate(next_location);

        // check if the clause contains both polarities of the same variable
        // 	since we removed duplicates and the literals are sorted, it suffices to check if two
        // neighbouring literals share the same variable
        for i in 1..literals.len() {
            if literals[i - 1].get_propositional_variable()
                == literals[i].get_propositional_variable()
            {
                satisfied_at_root = true;
                break;
            }
        }

        if satisfied_at_root {
            literals.truncate(1);
            literals[0] = assignments.true_literal;
        }

        literals
    }
}
