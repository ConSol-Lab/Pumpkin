mod inference;
mod retention;
#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub struct IntegerDivisionChecker<VA, VB, VC> {
    pub numerator: VA,
    pub denominator: VB,
    pub rhs: VC,
}
