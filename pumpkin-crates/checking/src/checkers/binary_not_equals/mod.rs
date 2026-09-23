mod inference;
mod retention;
#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub struct BinaryNotEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}
