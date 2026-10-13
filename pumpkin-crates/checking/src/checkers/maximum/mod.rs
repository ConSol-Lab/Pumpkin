mod conflict;

#[derive(Clone, Debug)]
pub struct MaximumChecker<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}
