#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub(crate) enum FileFormat {
    CnfDimacsPLine,
    WcnfDimacsPLine,
    #[allow(unused)]
    MaxSAT2022,
    FlatZinc,
}
