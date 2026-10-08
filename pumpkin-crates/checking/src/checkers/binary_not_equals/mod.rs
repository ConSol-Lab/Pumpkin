mod conflict;

#[derive(Clone, Debug)]
pub struct BinaryNotEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}
