mod inference;

#[derive(Clone, Debug)]
pub struct IntegerDivisionChecker<VA, VB, VC> {
    pub numerator: VA,
    pub denominator: VB,
    pub rhs: VC,
}
