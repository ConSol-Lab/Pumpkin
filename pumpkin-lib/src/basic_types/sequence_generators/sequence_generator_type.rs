#[derive(Clone, Copy, Debug)]
pub enum SequenceGeneratorType {
    Constant,
    Geometric,
    Luby,
}

impl std::fmt::Display for SequenceGeneratorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SequenceGeneratorType::Constant => write!(f, "constant"),
            SequenceGeneratorType::Geometric => write!(f, "geometric"),
            SequenceGeneratorType::Luby => write!(f, "luby"),
        }
    }
}
