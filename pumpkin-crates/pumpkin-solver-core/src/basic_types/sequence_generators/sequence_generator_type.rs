/// Specifies the type of sequence which is used to generate conflict limits before a restart
/// occurs.
#[derive(Default, Clone, Copy, Debug)]
pub enum SequenceGeneratorType {
    /// Indicates that the restart strategy should restart every `x` conflicts.
    #[default]
    Constant,
    /// Indicates that the restarts strategy should use geometric restarts.
    ///
    /// Given two constants `base` and `multiplicative_factor`, the i-th element `f(i)` in a
    /// geometric sequence is caluclated as follows:
    /// - `f(i) = f(i - 1) * multiplicative_factor`
    /// - `f(0) = base`
    ///
    /// When `multiplicative_factor` is not an integer, then the above formula is **not** the same
    /// as the formula `f(i) = a * m^i` since intermediate values are founded down.
    Geometric,
    /// Indicates that the restart strategy should use Luby restarts \[1\].
    ///
    ///  The Luby sequence is a recursive sequence of the form:
    /// 1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8, 1, 1, 2....
    ///
    /// # Bibliography
    /// \[1\] M. Luby, A. Sinclair, and D. Zuckerman, ‘Optimal speedup of Las Vegas algorithms’,
    /// Information Processing Letters, vol. 47, no. 4, pp. 173–180, 1993.
    Luby,
}
