mod conflict;

#[derive(Debug, Clone)]
pub struct LinearNotEqualChecker<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}
