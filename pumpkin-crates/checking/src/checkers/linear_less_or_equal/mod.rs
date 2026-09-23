mod inference;
mod retention;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LinearLessOrEqualChecker<Var> {
    pub terms: Box<[Var]>,
    pub bound: i32,
}

impl<Var> LinearLessOrEqualChecker<Var> {
    pub fn new(terms: Box<[Var]>, bound: i32) -> Self {
        LinearLessOrEqualChecker { terms, bound }
    }
}
