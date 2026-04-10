use std::{
    fs::File,
    io::{BufWriter, Write},
};

/// A wrapper around a proof file
#[derive(Debug)]
pub struct Trace {
    /// The proof file.
    proof_file: Option<BufWriter<File>>,
}

impl Trace {
    /// Trace to the given file.
    pub fn to_file(file: File) -> Trace {
        Trace {
            proof_file: Some(BufWriter::new(file)),
        }
    }

    /// Discard what is written to the trace.
    pub fn discard() -> Trace {
        Trace { proof_file: None }
    }
}

impl Write for Trace {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(file) = self.proof_file.as_mut() {
            file.write(buf)
        } else {
            Ok(buf.len())
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if let Some(file) = self.proof_file.as_mut() {
            file.flush()?;
        }
        Ok(())
    }
}
