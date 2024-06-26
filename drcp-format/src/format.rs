/// The format of the proof.
///
/// The textual format takes more disk space, but may be easier to read when looking at the files
/// directly. On the other hand, the binary format is more compact and marginally faster to write.
#[derive(Clone, Copy, Debug, Default)]
pub enum Format {
    /// A textual UTF-8 encoded proof.
    #[default]
    Text,
    /// A binary encoded proof.
    Binary,
}
