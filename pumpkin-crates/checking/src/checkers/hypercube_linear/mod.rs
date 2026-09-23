mod inference;

#[derive(Debug, Clone)]
pub struct HypercubeLinearChecker<Atomic, Var> {
    pub hypercube: Vec<Atomic>,
    pub terms: Vec<Var>,
    pub bound: i32,
}
