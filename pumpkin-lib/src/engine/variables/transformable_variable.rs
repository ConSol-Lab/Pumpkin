/// Trait for transforming a variable, at the moment this allows creating a scaled version of a
/// variable using [`TransformableVariable::scaled`] or creating a variable with a constant offset
/// based on the original variable using [`TransformableVariable::offset`].
pub trait TransformableVariable<View> {
    /// Get a variable which domain is scaled compared to the domain of self.
    ///
    /// The scaled domain will have holes in it. E.g. if we have `dom(x) = {1, 2}`, then
    /// `dom(x.scaled(2)) = {2, 4}` and *not* `dom(x.scaled(2)) = {1, 2, 3, 4}`.
    fn scaled(&self, scale: i32) -> View;

    /// Get a variable which domain has a constant offset to the domain of self.
    fn offset(&self, offset: i32) -> View;
}
