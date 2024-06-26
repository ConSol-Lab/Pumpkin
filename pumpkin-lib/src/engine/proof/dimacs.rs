use std::io::BufWriter;
use std::io::Write;
use std::num::NonZeroU64;

use crate::basic_types::StorageKey;
use crate::engine::variables::Literal;

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
        literals: impl IntoIterator<Item = Literal>,
    ) -> std::io::Result<NonZeroU64> {
        for lit in literals.into_iter() {
            let prefix = if lit.is_negative() { "-" } else { "" };
            let code = lit.get_propositional_variable().index();

            write!(self.writer, "{prefix}{code} ")?;
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
