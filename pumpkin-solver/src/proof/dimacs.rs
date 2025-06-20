use std::io::BufWriter;
use std::io::Write;

use crate::engine::predicates::predicate::PredicateType;
use crate::engine::VariableNames;
use crate::predicates::Predicate;
use crate::pumpkin_assert_simple;

#[derive(Debug)]
pub(crate) struct DimacsProof<W: Write> {
    writer: BufWriter<W>,
}

impl<W: Write> DimacsProof<W> {
    pub(crate) fn new(writer: W) -> DimacsProof<W> {
        DimacsProof {
            writer: BufWriter::new(writer),
        }
    }

    pub(crate) fn learned_clause(
        &mut self,
        predicates: impl IntoIterator<Item = Predicate>,
        variable_names: &VariableNames,
    ) -> std::io::Result<()> {
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
            let value = predicate.get_right_hand_side();
            let variable_prefix = match predicate.get_predicate_type() {
                PredicateType::LowerBound => {
                    pumpkin_assert_simple!(value == 1);
                    ""
                }
                PredicateType::Equal => {
                    pumpkin_assert_simple!(value == 0 || value == 1);
                    if value == 0 {
                        "-"
                    } else {
                        ""
                    }
                }
                PredicateType::NotEqual => {
                    pumpkin_assert_simple!(value == 0 || value == 1);
                    if value == 0 {
                        ""
                    } else {
                        "-"
                    }
                }

                PredicateType::UpperBound => {
                    pumpkin_assert_simple!(value == 0);
                    "-"
                }
            };

            write!(self.writer, "{variable_prefix}{variable_code} ")?;
        }

        writeln!(self.writer, "0")?;

        Ok(())
    }
}
