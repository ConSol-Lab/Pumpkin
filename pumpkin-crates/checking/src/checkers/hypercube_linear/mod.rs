mod inference;
mod retention;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct HypercubeLinearChecker<Atomic, Var> {
    pub hypercube: Vec<Atomic>,
    pub terms: Vec<Var>,
    pub bound: i32,
}
