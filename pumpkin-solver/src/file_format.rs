/// The input formats supported by the solver.
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum FileFormat {
    /// A CNF instance in DIMACS format.
    CnfDimacsPLine,
    /// A weighted CNF (MaxSAT) instance in DIMACS format.
    WcnfDimacsPLine,
    /// A FlatZinc instance.
    FlatZinc,
}
