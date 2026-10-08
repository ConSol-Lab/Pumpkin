mod conflict;

#[derive(Debug, Clone)]
pub struct LinearLessOrEqualConflictChecker<Var> {
    terms: Box<[Var]>,
    bound: i32,
}

impl<Var> LinearLessOrEqualConflictChecker<Var> {
    pub fn new(terms: Box<[Var]>, bound: i32) -> Self {
        LinearLessOrEqualConflictChecker { terms, bound }
    }
}
