use std::time::Instant;

use crate::{
    basic_types::{ClauseAdditionOutcome, Literal},
    encoders::UpperBoundEncoder,
    engine::ConstraintSatisfactionSolver,
    pumpkin_asserts::{pumpkin_assert_eq_simple, pumpkin_assert_simple},
};

use super::EncodingStatus;

/// An implementation of the cardinality network encoding for unweighted cardinality constraints in
/// the form `x1 + ... + xn <= k`. The encoding is arc-consistent and supports incremental
/// strengthening of the upper bound.
///
/// Reference:
/// Asín, Roberto, et al. Cardinality networks: a theoretical and empirical study.
/// Constraints, 2011, 16: 195-221.
pub struct CardinalityNetworkEncoder {
    literals: Vec<Literal>,
    output: Vec<Literal>,
    constant_term: u64,
    encoding_generated: bool,
    num_clauses_added: usize,
}

macro_rules! try_add_clause {
    ($self:ident, $csp_solver:ident, $e:expr) => {
        if $csp_solver.add_permanent_clause($e) == ClauseAdditionOutcome::Infeasible {
            return None;
        }
        $self.num_clauses_added += 1;
    };
}

impl UpperBoundEncoder for CardinalityNetworkEncoder {
    fn constrain_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> EncodingStatus {
        pumpkin_assert_simple!(
            k >= self.constant_term,
            "The upper bound ({k}) should never be less than the constant term {}.",
            self.constant_term
        );

        if !self.encoding_generated
            && self.create_encoding(k, csp_solver) == EncodingStatus::ConflictDetected
        {
            return EncodingStatus::ConflictDetected;
        }

        println!("c CNE k = {k}");

        let k = (k - self.constant_term) as usize;

        if k == 0 && self.literals.is_empty() {
            return EncodingStatus::NoConflictDetected;
        }

        pumpkin_assert_simple!(
            k < self.output.len(),
            "The upper bound cannot be larger than the greatest possible value."
        );

        if csp_solver.add_unit_clause(!self.output[k]) == ClauseAdditionOutcome::Infeasible {
            EncodingStatus::ConflictDetected
        } else {
            EncodingStatus::NoConflictDetected
        }
    }
}

impl CardinalityNetworkEncoder {
    /// Create a new encoder based on a function describing the cardinality constraint.
    ///
    /// Panics if the function does not have any variables on the left-hand side, or if the
    /// function is weighted (i.e. the terms do not have the same weight).
    pub fn from_function(
        function: &crate::basic_types::Function,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Self {
        let literals = function.get_function_as_weighted_literals_vector(csp_solver);

        let literals = match literals.first().map(|wlit| wlit.weight) {
            Some(weight) => {
                pumpkin_assert_simple!(
                    literals
                        .iter()
                        .all(|weighted_lit| weight == weighted_lit.weight),
                    "Sorting network encoding is only supported on unweighted instances."
                );

                literals
                    .into_iter()
                    .map(|wlit| wlit.literal)
                    .collect::<Vec<_>>()
            }
            None => vec![],
        };

        CardinalityNetworkEncoder::new(literals, function.get_constant_term())
    }

    /// Create a new encoder from the given literals which form the left-hand side, and a constant
    /// term. The constant term is used when constraining the sum of variables, and it is expected
    /// the right-hand side will never be below the constant term.
    pub fn new(literals: Vec<Literal>, constant_term: u64) -> Self {
        CardinalityNetworkEncoder {
            literals,
            output: vec![],
            constant_term,
            encoding_generated: false,
            num_clauses_added: 0,
        }
    }

    fn create_encoding(
        &mut self,
        p: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> EncodingStatus {
        pumpkin_assert_simple!(
            self.output.is_empty(),
            "Can only generate the encoding once."
        );

        let time_start = Instant::now();
        let result = self.generate_clauses(p, csp_solver);

        self.encoding_generated = true;

        println!(
            "c encoding added {} clauses to the solver.",
            self.num_clauses_added
        );

        println!(
            "c initial encoding took {} seconds.",
            time_start.elapsed().as_secs()
        );

        if result == EncodingStatus::ConflictDetected {
            println!("c encoding detected conflict at the root!");
        }

        result
    }

    fn generate_clauses(
        &mut self,
        p: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> EncodingStatus {
        let n = self.literals.len() as u64;

        if n == 0 {
            return EncodingStatus::NoConflictDetected;
        }

        let k = find_next_power_of_two(p);

        let num_padding_literals = round_up_to_multiple(n, k) - n;
        let padding_lits = csp_solver
            .new_literals()
            .take(num_padding_literals as usize)
            .collect::<Vec<_>>();

        for &lit in padding_lits.iter() {
            if csp_solver.add_unit_clause(!lit) == ClauseAdditionOutcome::Infeasible {
                return EncodingStatus::ConflictDetected;
            }
        }

        self.output = self
            .card(
                &[self.literals.as_slice(), padding_lits.as_slice()].concat(),
                k,
                csp_solver,
            )
            .unwrap_or_default();

        if self.output.is_empty() {
            EncodingStatus::ConflictDetected
        } else {
            EncodingStatus::NoConflictDetected
        }
    }

    fn s_merge(
        &mut self,
        a: &[Literal],
        b: &[Literal],
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Option<Vec<Literal>> {
        pumpkin_assert_eq_simple!(a.len(), b.len());

        if a.len() == 1 {
            let c = csp_solver.new_literals().take(2).collect::<Vec<_>>();

            let a = a[0];
            let b = b[0];

            try_add_clause!(self, csp_solver, vec![!a, !b, c[1]]);
            try_add_clause!(self, csp_solver, vec![!a, c[0]]);
            try_add_clause!(self, csp_solver, vec![!b, c[0]]);

            return Some(c);
        }

        pumpkin_assert_eq_simple!(0, a.len() & 1);
        let a_even = even_literals(a);
        let b_even = even_literals(b);
        let a_odd = odd_literals(a);
        let b_odd = odd_literals(b);

        let d = self.s_merge(&a_odd, &b_odd, csp_solver)?;
        let e = self.s_merge(&a_even, &b_even, csp_solver)?;

        pumpkin_assert_eq_simple!((a.len() >> 1) + 1, d.len());
        pumpkin_assert_eq_simple!((a.len() >> 1) + 1, e.len());

        let mut c = csp_solver.new_literals().take(a.len()).collect::<Vec<_>>();
        c.insert(0, d[0]);

        for i in 0..(a.len() >> 1) {
            try_add_clause!(self, csp_solver, vec![!d[i + 1], !e[i], c[2 * (i + 1)]]);
            try_add_clause!(self, csp_solver, vec![!d[i + 1], c[2 * (i + 1) - 1]]);
            try_add_clause!(self, csp_solver, vec![!e[i], c[2 * (i + 1) - 1]]);
        }

        Some(c)
    }

    fn h_sort(
        &mut self,
        seq: &[Literal],
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Option<Vec<Literal>> {
        pumpkin_assert_simple!(seq.len() & 1 == 0);

        let n = seq.len() >> 1;

        if n == 1 {
            return self.h_merge(&[seq[0]], &[seq[1]], csp_solver);
        }

        let d = self.h_sort(&seq[..n], csp_solver)?;
        let d_prime = self.h_sort(&seq[n..], csp_solver)?;

        self.h_merge(&d, &d_prime, csp_solver)
    }

    fn h_merge(
        &mut self,
        a: &[Literal],
        b: &[Literal],
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Option<Vec<Literal>> {
        pumpkin_assert_eq_simple!(a.len(), b.len());

        let n = a.len();

        if n == 1 {
            return self.s_merge(a, b, csp_solver);
        }

        let a_even = even_literals(a);
        let b_even = even_literals(b);
        let a_odd = odd_literals(a);
        let b_odd = odd_literals(b);

        let d = self.h_merge(&a_odd, &b_odd, csp_solver)?;
        let e = self.h_merge(&a_even, &b_even, csp_solver)?;

        let mut c = csp_solver
            .new_literals()
            .take(2 * n - 2)
            .collect::<Vec<_>>();

        c.insert(0, d[0]);
        c.push(e[e.len() - 1]);

        for i in 0..(n - 1) {
            try_add_clause!(self, csp_solver, vec![!d[i + 1], !e[i], c[2 * (i + 1)]]);
            try_add_clause!(self, csp_solver, vec![!d[i + 1], c[2 * (i + 1) - 1]]);
            try_add_clause!(self, csp_solver, vec![!e[i], c[2 * (i + 1) - 1]]);
        }

        Some(c)
    }

    fn card(
        &mut self,
        a: &[Literal],
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Option<Vec<Literal>> {
        let n = a.len() as u64;
        let m = n / k;
        pumpkin_assert_eq_simple!(n, m * k);

        if n == k {
            return self.h_sort(a, csp_solver);
        }

        let d = self.card(&a[..k as usize], k, csp_solver)?;
        let d_prime = self.card(&a[k as usize..], k, csp_solver)?;

        let mut c = self.s_merge(&d, &d_prime, csp_solver)?;
        c.remove(c.len() - 1);

        Some(c)
    }
}

/// Find a number p such that p is a power of two and p > k.
fn find_next_power_of_two(k: u64) -> u64 {
    // Perhaps a naive implementation, but this is not on a hot-path (yet), so
    // for now it will do.
    let mut result = 1;

    while result <= k {
        result <<= 1;
    }

    result
}

/// Round up 'k' to the nearest multiple of 'multiple'
fn round_up_to_multiple(k: u64, multiple: u64) -> u64 {
    let remainder = k % multiple;

    if remainder == 0 {
        k
    } else {
        k + multiple - remainder
    }
}

fn odd_literals(lits: &[Literal]) -> Vec<Literal> {
    // Note: in the literature, sequences start at 1, hence the odd literals
    // step by two but start at the first literal.
    lits.iter().copied().step_by(2).collect::<Vec<_>>()
}

fn even_literals(lits: &[Literal]) -> Vec<Literal> {
    // Note: in the literature, sequences start at 1, hence the even literals
    // skip the first and then step by two.
    lits.iter().copied().skip(1).step_by(2).collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use crate::basic_types::Function;

    use super::*;

    #[test]
    fn test_cardinality_constraint_no_input_literals() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let mut ub = CardinalityNetworkEncoder::new(vec![], 0);

        assert_eq!(
            EncodingStatus::NoConflictDetected,
            ub.constrain_at_most_k(0, &mut csp_solver)
        );
    }

    #[test]
    fn test_cardinality_constraint_empty_function() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let mut ub = CardinalityNetworkEncoder::from_function(&Function::new(), &mut csp_solver);

        assert_eq!(
            EncodingStatus::NoConflictDetected,
            ub.constrain_at_most_k(0, &mut csp_solver)
        );
    }

    #[test]
    fn test_smallest_cardinality_constraint() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let xs = create_variables(&mut csp_solver, 2);

        let mut ub = CardinalityNetworkEncoder::new(xs.clone(), 0);
        assert_eq!(
            EncodingStatus::NoConflictDetected,
            ub.constrain_at_most_k(1, &mut csp_solver)
        );

        assert_eq!(
            ClauseAdditionOutcome::NoConflictDetected,
            csp_solver.add_unit_clause(xs[0])
        );

        assert_eq!(
            ClauseAdditionOutcome::Infeasible,
            csp_solver.add_unit_clause(xs[1])
        );
    }

    #[test]
    fn test_small_cardinality_constraint() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let xs = create_variables(&mut csp_solver, 3);

        let mut ub = CardinalityNetworkEncoder::new(xs.clone(), 0);
        assert_eq!(
            EncodingStatus::NoConflictDetected,
            ub.constrain_at_most_k(2, &mut csp_solver)
        );

        assert_eq!(
            ClauseAdditionOutcome::NoConflictDetected,
            csp_solver.add_unit_clause(xs[0])
        );

        assert_eq!(
            ClauseAdditionOutcome::NoConflictDetected,
            csp_solver.add_unit_clause(xs[1])
        );

        assert_eq!(
            ClauseAdditionOutcome::Infeasible,
            csp_solver.add_unit_clause(xs[2])
        );
    }

    #[test]
    fn test_constant_term_offsets_k() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let xs = create_variables(&mut csp_solver, 3);

        let mut ub = CardinalityNetworkEncoder::new(xs.clone(), 3);
        assert_eq!(
            EncodingStatus::NoConflictDetected,
            ub.constrain_at_most_k(5, &mut csp_solver)
        );

        assert_eq!(
            ClauseAdditionOutcome::NoConflictDetected,
            csp_solver.add_unit_clause(xs[0])
        );

        assert_eq!(
            ClauseAdditionOutcome::NoConflictDetected,
            csp_solver.add_unit_clause(xs[1])
        );

        assert_eq!(
            ClauseAdditionOutcome::Infeasible,
            csp_solver.add_unit_clause(xs[2])
        );
    }

    fn create_variables(csp_solver: &mut ConstraintSatisfactionSolver, n: usize) -> Vec<Literal> {
        let xs = std::iter::from_fn(|| Some(csp_solver.create_new_propositional_variable()))
            .map(|var| Literal::new(var, true))
            .take(n)
            .collect::<Vec<_>>();
        xs
    }
}
