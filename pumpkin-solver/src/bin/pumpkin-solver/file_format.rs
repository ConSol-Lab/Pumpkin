#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub(crate) enum FileFormat {
    CnfDimacsPLine,
    WcnfDimacsPLine,
    FlatZinc,
}
