//! Extensions for numbers that are not present in the stable standard library.

pub(crate) trait NumExt {
    /// Division with rounding up.
    fn div_ceil(self, other: Self) -> Self;

    /// Division with rounding down.
    ///
    /// Note this is different from truncating, which is rounding toward zero.
    fn div_floor(self, other: Self) -> Self;
}

impl NumExt for i32 {
    fn div_ceil(self, other: Self) -> Self {
        // TODO: The source is taken from the standard library nightly implementation of this
        // function and div_floor. Once they are stabilized, these definitions can be removed.
        // Tracking issue: https://github.com/rust-lang/rust/issues/88581
        let d = self / other;
        let r = self % other;
        if (r > 0 && other > 0) || (r < 0 && other < 0) {
            d + 1
        } else {
            d
        }
    }

    fn div_floor(self, other: Self) -> Self {
        // TODO: See todo in `div_ceil`.
        let d = self / other;
        let r = self % other;
        if (r > 0 && other < 0) || (r < 0 && other > 0) {
            d - 1
        } else {
            d
        }
    }
}
