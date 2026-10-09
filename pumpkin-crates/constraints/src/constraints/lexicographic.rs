use pumpkin_core::constraints::Constraint;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_propagators::lexicographic::LexLessOrEqualArgs;

/// Creates the [`Constraint`] that `x` is lexicographically less than or equal to `y`.
///
/// Both vectors must have the same length.
pub fn lex_less_or_equal<Var: IntegerVariable + 'static>(
    x: impl IntoIterator<Item = Var>,
    y: impl IntoIterator<Item = Var>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    LexLessOrEqualArgs {
        x: x.into_iter().collect(),
        y: y.into_iter().collect(),
        constraint_tag,
    }
}
