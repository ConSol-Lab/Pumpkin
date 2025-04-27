use std::io::BufWriter;
use std::io::Write;
use std::num::NonZeroU64;

use crate::engine::VariableNames;
use crate::predicates::Predicate;

#[derive(Debug)]
pub(crate) struct DimacsProof<W: Write> {
    writer: BufWriter<W>,
    next_clause_id: NonZeroU64,
}

impl<W: Write> DimacsProof<W> {
    pub(crate) fn new(writer: W) -> DimacsProof<W> {
        DimacsProof {
            writer: BufWriter::new(writer),
            next_clause_id: NonZeroU64::new(1).unwrap(),
        }
    }

    pub(crate) fn learned_clause(
        &mut self,
        predicates: impl IntoIterator<Item = Predicate>,
        variable_names: &VariableNames,
    ) -> std::io::Result<NonZeroU64> {
        for predicate in predicates.into_iter() {
            assert!(
                predicate.get_right_hand_side() <= 1,
                "in dimacs proofs all variables are Boolean (aka 0-1)"
            );
            assert!(
                predicate.get_right_hand_side() >= 0,
                "in dimacs proofs all variables are Boolean (aka 0-1)"
            );

            let variable_code = variable_names
                .get_int_name(predicate.get_domain())
                .expect("all variables are named in a DIMACS problem");
            let variable_prefix = match predicate {
                Predicate::LowerBound { lower_bound: 1, .. } => "",
                Predicate::Equal {
                    equality_constant: 1,
                    ..
                } => "",
                Predicate::NotEqual {
                    not_equal_constant: 0,
                    ..
                } => "",

                Predicate::UpperBound { upper_bound: 0, .. } => "-",
                Predicate::Equal {
                    equality_constant: 0,
                    ..
                } => "-",
                Predicate::NotEqual {
                    not_equal_constant: 1,
                    ..
                } => "-",

                other => panic!("Unexpected predicate {other:?} in learned clause for DIMACS"),
            };

            write!(self.writer, "{variable_prefix}{variable_code} ")?;
        }

        writeln!(self.writer, "0")?;

        let id = self.next_clause_id;
        self.next_clause_id = self
            .next_clause_id
            .checked_add(1)
            .expect("we are not adding u64::MAX clauses (hopefully)");

        Ok(id)
    }
}
