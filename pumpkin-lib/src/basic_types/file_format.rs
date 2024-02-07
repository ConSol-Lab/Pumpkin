#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum FileFormat {
    CnfDimacsPLine,
    WcnfDimacsPLine,
    MaxSAT2022,
    FlatZinc,
}
