mod inference;
mod retention;
#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub struct MaximumChecker<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}
