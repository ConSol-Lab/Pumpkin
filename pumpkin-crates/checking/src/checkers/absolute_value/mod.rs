mod inference;
mod retention;
#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub struct AbsoluteValueChecker<VA, VB> {
    pub signed: VA,
    pub absolute: VB,
}
