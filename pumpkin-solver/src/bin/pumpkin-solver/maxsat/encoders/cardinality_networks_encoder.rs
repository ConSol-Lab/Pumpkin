use std::time::Instant;

use pumpkin_solver::proof::ConstraintTag;
use pumpkin_solver::pumpkin_assert_eq_simple;
use pumpkin_solver::pumpkin_assert_simple;
use pumpkin_solver::variables::Literal;
use pumpkin_solver::Solver;

use super::pseudo_boolean_constraint_encoder::EncodingError;
use super::PseudoBooleanConstraintEncoderInterface;
use super::WeightedLiteral;
use crate::maxsat::encoders::EncodingError::CannotStrengthen;

/// An implementation of the cardinality network encoding for unweighted cardinality constraints in
/// the form `x1 + ... + xn <= k`. The encoding is arc-consistent and supports incremental
/// strengthening of the upper bound.
///
/// Reference:
/// Asín, Roberto, et al. Cardinality networks: a theoretical and empirical study.
/// Constraints, 2011, 16: 195-221.
#[derive(Debug)]
pub(crate) struct CardinalityNetworkEncoder {
    literals: Vec<Literal>,
    output: Vec<Literal>,
    num_clauses_added: usize,
    /// Useless, since the encoder is only used when solving DIMACS problems, but required to add
    /// constraints.
    constraint_tag: ConstraintTag,
}

macro_rules! try_add_clause {
    ($self:ident, $csp_solver:ident, $e:expr, $tag:expr) => {
        if $csp_solver.add_clause($e, $tag).is_err() {
            return None;
        }
        $self.num_clauses_added += 1;
    };
}

impl PseudoBooleanConstraintEncoderInterface for CardinalityNetworkEncoder {
    fn encode_at_most_k(
        weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        solver: &mut Solver,
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

        CardinalityNetworkEncoder::new(literals, k, solver)
    }

    fn strengthen_at_most_k(&mut self, k: u64, solver: &mut Solver) -> Result<(), EncodingError> {
        if k == 0 && self.output.is_empty() {
            return Ok(());
        }

        pumpkin_assert_simple!(
            k < self.output.len() as u64,
            "The upper bound cannot be larger than the greatest possible value."
        );

        println!("c CNE k = {k}");

        if solver
            .add_clause(
                [(!self.output[k as usize]).to_predicate()],
                self.constraint_tag,
            )
            .is_err()
        {
            Err(CannotStrengthen)
        } else {
            Ok(())
        }
    }
}

impl CardinalityNetworkEncoder {
    /// Create a new encoder from the given literals which form the left-hand side.
    pub(crate) fn new(
        literals: Vec<Literal>,
        p: u64,
        solver: &mut Solver,
    ) -> Result<Self, EncodingError> {
        let constraint_tag = solver.new_constraint_tag();
        let mut encoder = CardinalityNetworkEncoder {
            literals,
            output: vec![],
            num_clauses_added: 0,
            constraint_tag,
        };

        encoder.create_encoding(p, solver)?;

        Ok(encoder)
    }

    fn create_encoding(&mut self, p: u64, solver: &mut Solver) -> Result<(), EncodingError> {
        pumpkin_assert_simple!(
            self.output.is_empty(),
            "Can only generate the encoding once."
        );

        let time_start = Instant::now();
        let result = self.generate_clauses(p, solver);

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
            let r = solver.add_clause(
                [(!self.output[p as usize]).to_predicate()],
                self.constraint_tag,
            );
            if r.is_err() {
                return Err(EncodingError::RootPropagationConflict);
            }
        }

        result
    }

    fn generate_clauses(&mut self, p: u64, solver: &mut Solver) -> Result<(), EncodingError> {
        let n = self.literals.len() as u64;

        if n == 0 {
            return Ok(());
        }

        let k = find_next_power_of_two(p);

        let num_padding_literals = round_up_to_multiple(n, k) - n;
        let padding_lits = (0..num_padding_literals)
            .map(|_| solver.new_literal())
            .collect::<Vec<_>>();

        for &lit in padding_lits.iter() {
            if solver
                .add_clause([(!lit).to_predicate()], self.constraint_tag)
                .is_err()
            {
                return Err(EncodingError::RootPropagationConflict);
            }
        }

        self.output = self
            .card(
                &[self.literals.as_slice(), padding_lits.as_slice()].concat(),
                k,
                solver,
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
        solver: &mut Solver,
    ) -> Option<Vec<Literal>> {
        pumpkin_assert_eq_simple!(a.len(), b.len());

        if a.len() == 1 {
            let c = vec![solver.new_literal(), solver.new_literal()];
            let a = a[0].to_predicate();
            let b = b[0].to_predicate();

            try_add_clause!(
                self,
                solver,
                vec![!a, !b, c[1].to_predicate()],
                self.constraint_tag
            );
            try_add_clause!(
                self,
                solver,
                vec![!a, c[0].to_predicate()],
                self.constraint_tag
            );
            try_add_clause!(
                self,
                solver,
                vec![!b, c[0].to_predicate()],
                self.constraint_tag
            );

            return Some(c);
        }

        pumpkin_assert_eq_simple!(0, a.len() & 1);
        let a_even = even_literals(a);
        let b_even = even_literals(b);
        let a_odd = odd_literals(a);
        let b_odd = odd_literals(b);

        let d = self.s_merge(&a_odd, &b_odd, solver)?;
        let e = self.s_merge(&a_even, &b_even, solver)?;

        pumpkin_assert_eq_simple!((a.len() >> 1) + 1, d.len());
        pumpkin_assert_eq_simple!((a.len() >> 1) + 1, e.len());

        let mut c = (0..a.len())
            .map(|_| solver.new_literal())
            .collect::<Vec<_>>();
        c.insert(0, d[0]);

        for i in 0..(a.len() >> 1) {
            try_add_clause!(
                self,
                solver,
                vec![
                    (!d[i + 1]).to_predicate(),
                    (!e[i]).to_predicate(),
                    (c[2 * (i + 1)]).to_predicate()
                ],
                self.constraint_tag
            );
            try_add_clause!(
                self,
                solver,
                vec![
                    (!d[i + 1]).to_predicate(),
                    (c[2 * (i + 1) - 1]).to_predicate()
                ],
                self.constraint_tag
            );
            try_add_clause!(
                self,
                solver,
                vec![(!e[i]).to_predicate(), (c[2 * (i + 1) - 1]).to_predicate()],
                self.constraint_tag
            );
        }

        Some(c)
    }

    fn h_sort(&mut self, seq: &[Literal], solver: &mut Solver) -> Option<Vec<Literal>> {
        pumpkin_assert_simple!(seq.len() & 1 == 0);

        let n = seq.len() >> 1;

        if n == 1 {
            return self.h_merge(&[seq[0]], &[seq[1]], solver);
        }

        let d = self.h_sort(&seq[..n], solver)?;
        let d_prime = self.h_sort(&seq[n..], solver)?;

        self.h_merge(&d, &d_prime, solver)
    }

    fn h_merge(
        &mut self,
        a: &[Literal],
        b: &[Literal],
        solver: &mut Solver,
    ) -> Option<Vec<Literal>> {
        pumpkin_assert_eq_simple!(a.len(), b.len());

        let n = a.len();

        if n == 1 {
            return self.s_merge(a, b, solver);
        }

        let a_even = even_literals(a);
        let b_even = even_literals(b);
        let a_odd = odd_literals(a);
        let b_odd = odd_literals(b);

        let d = self.h_merge(&a_odd, &b_odd, solver)?;
        let e = self.h_merge(&a_even, &b_even, solver)?;

        let mut c = (0..2 * n - 2)
            .map(|_| solver.new_literal())
            .collect::<Vec<_>>();

        c.insert(0, d[0]);
        c.push(e[e.len() - 1]);

        for i in 0..(n - 1) {
            try_add_clause!(
                self,
                solver,
                vec![
                    (!d[i + 1]).to_predicate(),
                    (!e[i]).to_predicate(),
                    (c[2 * (i + 1)]).to_predicate()
                ],
                self.constraint_tag
            );
            try_add_clause!(
                self,
                solver,
                vec![
                    (!d[i + 1]).to_predicate(),
                    (c[2 * (i + 1) - 1]).to_predicate()
                ],
                self.constraint_tag
            );
            try_add_clause!(
                self,
                solver,
                vec![(!e[i]).to_predicate(), (c[2 * (i + 1) - 1]).to_predicate()],
                self.constraint_tag
            );
        }

        Some(c)
    }

    fn card(&mut self, a: &[Literal], k: u64, solver: &mut Solver) -> Option<Vec<Literal>> {
        let n = a.len() as u64;
        let m = n / k;
        pumpkin_assert_eq_simple!(n, m * k);

        if n == k {
            return self.h_sort(a, solver);
        }

        let d = self.card(&a[..k as usize], k, solver)?;
        let d_prime = self.card(&a[k as usize..], k, solver)?;

        let mut c = self.s_merge(&d, &d_prime, solver)?;
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
        let mut solver = Solver::default();
        let mut ub =
            CardinalityNetworkEncoder::new(vec![], 0, &mut solver).expect("valid encoding");

        ub.strengthen_at_most_k(0, &mut solver)
            .expect("should not fail");
    }

    #[test]
    fn test_smallest_cardinality_constraint() {
        let mut solver = Solver::default();
        let constraint_tag = solver.new_constraint_tag();
        let xs = create_variables(&mut solver, 2);

        let _ = CardinalityNetworkEncoder::new(xs.clone(), 1, &mut solver);

        assert!(solver
            .add_clause([xs[0].to_predicate()], constraint_tag)
            .is_ok());
        assert!(solver
            .add_clause([xs[1].to_predicate()], constraint_tag)
            .is_err());
    }

    #[test]
    fn test_small_cardinality_constraint() {
        let mut solver = Solver::default();
        let constraint_tag = solver.new_constraint_tag();
        let xs = create_variables(&mut solver, 3);

        let _ = CardinalityNetworkEncoder::new(xs.clone(), 2, &mut solver).expect("valid encoding");

        assert!(solver
            .add_clause([xs[0].to_predicate()], constraint_tag)
            .is_ok());
        assert!(solver
            .add_clause([xs[1].to_predicate()], constraint_tag)
            .is_ok());
        assert!(solver
            .add_clause([xs[2].to_predicate()], constraint_tag)
            .is_err());
    }

    fn create_variables(solver: &mut Solver, n: usize) -> Vec<Literal> {
        std::iter::from_fn(|| Some(solver.new_literal()))
            .take(n)
            .collect::<Vec<_>>()
    }
}
