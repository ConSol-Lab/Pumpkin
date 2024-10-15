use std::collections::HashMap;
use std::io::Write;
use std::num::NonZeroU32;

use super::AtomicConstraint;

/// Associates literals used in the proof with the [`AtomicConstraint`] they correspond to.
///
/// The definitions can be serialized into a literal definition file with
/// [`LiteralDefinitions::write()`].
#[derive(Clone, Debug, Default)]
pub struct LiteralDefinitions<'a> {
    definitions: HashMap<NonZeroU32, Vec<AtomicConstraint<'a>>>,
}

impl<'a> LiteralDefinitions<'a> {
    /// Add a new definition to the literal definitions.
    pub fn add(&mut self, code: NonZeroU32, atomic: AtomicConstraint<'a>) {
        self.definitions.entry(code).or_default().push(atomic);
    }

    /// Write out all the definitions to the given sink.
    pub fn write(&mut self, mut sink: impl Write) -> std::io::Result<()> {
        for (code, atomics) in self.definitions.iter() {
            write!(sink, "{code}")?;

            for atomic in atomics {
                write!(sink, " {atomic}")?;
            }

            writeln!(sink)?;
        }

        Ok(())
    }
}
