mod conflict;
mod retention;
#[cfg(test)]
mod tests;

/// The checker of the rule of `x <=lex y`, for two lists of equal length.
#[derive(Clone, Debug)]
pub struct LexLessOrEqualChecker<Var> {
    pub x: Box<[Var]>,
    pub y: Box<[Var]>,
}
