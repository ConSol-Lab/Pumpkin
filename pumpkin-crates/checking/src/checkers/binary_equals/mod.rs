mod conflict;

#[derive(Clone, Debug)]
pub struct BinaryEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}
