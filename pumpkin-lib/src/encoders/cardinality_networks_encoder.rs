use std::time::Instant;

use super::pseudo_boolean_constraint_encoder::EncodingError;
use super::PseudoBooleanConstraintEncoderInterface;
use crate::encoders::pseudo_boolean_constraint_encoder::EncodingError::CannotStrengthen;
use crate::engine::variables::Literal;
use crate::engine::ConstraintSatisfactionSolver;
use crate::pumpkin_assert_eq_simple;
use crate::pumpkin_assert_simple;

/// An implementation of the cardinality network encoding for unweighted cardinality constraints in
/// the form `x1 + ... + xn <= k`. The encoding is arc-consistent and supports incremental
/// strengthening of the upper bound.
///
/// Reference:
/// Asín, Roberto, et al. Cardinality networks: a theoretical and empirical study.
/// Constraints, 2011, 16: 195-221.
#[derive(Debug)]
pub struct CardinalityNetworkEncoder {
    literals: Vec<Literal>,
    output: Vec<Literal>,
    num_clauses_added: usize,
}

macro_rules! try_add_clause {
    ($self:ident, $csp_solver:ident, $e:expr) => {
        if $csp_solver.add_clause($e).is_err() {
            return None;
        }
        $self.num_clauses_added += 1;
    };
}

impl PseudoBooleanConstraintEncoderInterface for CardinalityNetworkEncoder {
    fn encode_at_most_k(
        weighted_literals: Vec<crate::basic_types::WeightedLiteral>,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<Self, EncodingError>
    where
        Self: Sized,
    {
        // The CNE only supports unweighted functions. This expression verifies that assumption is
        // met. If it is not met, and because we cannot return an error condition, we panic.
        let literals = match weighted_literals.first().map(|wlit| wlit.weight) {
            Some(weight) => {
                pumpkin_assert_simple!(
                    weighted_literals
                        .iter()
                        .all(|weighted_lit| weight == weighted_lit.weight),
                    "Sorting network encoding is only supported on unweighted instances."
                );

                weighted_literals
                    .into_iter()
                    .map(|wlit| wlit.literal)
                    .collect::<Vec<_>>()
            }
            None => vec![],
        };

        CardinalityNetworkEncoder::new(literals, k, csp_solver)
    }

    fn strengthen_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError> {
        if k == 0 && self.output.is_empty() {
            return Ok(());
        }

        pumpkin_assert_simple!(
            k < self.output.len() as u64,
            "The upper bound cannot be larger than the greatest possible value."
        );

        println!("c CNE k = {k}");

        if csp_solver.add_clause([!self.output[k as usize]]).is_err() {
            Err(CannotStrengthen)
        } else {
            Ok(())
        }
    }
}

impl CardinalityNetworkEncoder {
    /// Create a new encoder from the given literals which form the left-hand side.
    pub fn new(
        literals: Vec<Literal>,
        p: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<Self, EncodingError> {
        let mut encoder = CardinalityNetworkEncoder {
            literals,
            output: vec![],
            num_clauses_added: 0,
        };

        encoder.create_encoding(p, csp_solver)?;

        Ok(encoder)
    }

    fn create_encoding(
        &mut self,
        p: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError> {
        pumpkin_assert_simple!(
            self.output.is_empty(),
            "Can only generate the encoding once."
        );

        let time_start = Instant::now();
        let result = self.generate_clauses(p, csp_solver);

        println!(
            "c encoding added {} clauses to the solver.",
            self.num_clauses_added
        );

        println!(
            "c initial encoding took {} seconds.",
            time_start.elapsed().as_secs()
        );

        if result.is_err() {
            println!("c encoding detected conflict at the root!");
        } else if !self.output.is_empty() {
            let r = csp_solver.add_clause([!self.output[p as usize]]);
            if r.is_err() {
                return Err(EncodingError::RootPropagationConflict);
            }
        }

        result
    }

    fn generate_clauses(
        &mut self,
        p: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError> {
        let n = self.literals.len() as u64;

        if n == 0 {
            return Ok(());
        }

        let k = find_next_power_of_two(p);

        let num_padding_literals = round_up_to_multiple(n, k) - n;
        let padding_lits = csp_solver
            .new_literals()
            .take(num_padding_literals as usize)
            .collect::<Vec<_>>();

        for &lit in padding_lits.iter() {
            if csp_solver.add_clause([!lit]).is_err() {
                return Err(EncodingError::RootPropagationConflict);
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
            Err(EncodingError::RootPropagationConflict)
        } else {
            Ok(())
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
        let _ = c.remove(c.len() - 1);

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
    use super::*;

    #[test]
    fn test_cardinality_constraint_no_input_literals() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let mut ub =
            CardinalityNetworkEncoder::new(vec![], 0, &mut csp_solver).expect("valid encoding");

        ub.strengthen_at_most_k(0, &mut csp_solver)
            .expect("should not fail");
    }

    #[test]
    fn test_smallest_cardinality_constraint() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let xs = create_variables(&mut csp_solver, 2);

        let _ = CardinalityNetworkEncoder::new(xs.clone(), 1, &mut csp_solver);

        assert!(csp_solver.add_clause([xs[0]]).is_ok());
        assert!(csp_solver.add_clause([xs[1]]).is_err());
    }

    #[test]
    fn test_small_cardinality_constraint() {
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let xs = create_variables(&mut csp_solver, 3);

        let _ =
            CardinalityNetworkEncoder::new(xs.clone(), 2, &mut csp_solver).expect("valid encoding");

        assert!(csp_solver.add_clause([xs[0]]).is_ok());
        assert!(csp_solver.add_clause([xs[1]]).is_ok());
        assert!(csp_solver.add_clause([xs[2]]).is_err());
    }

    fn create_variables(csp_solver: &mut ConstraintSatisfactionSolver, n: usize) -> Vec<Literal> {
        std::iter::from_fn(|| Some(csp_solver.create_new_propositional_variable(None)))
            .map(|var| Literal::new(var, true))
            .take(n)
            .collect::<Vec<_>>()
    }
}
