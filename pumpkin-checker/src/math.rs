pub(crate) fn div_ceil(lhs: i32, other: i32) -> i32 {
    // TODO: The source is taken from the standard library nightly implementation of this
    // function and div_floor. Once they are stabilized, these definitions can be removed.
    // Tracking issue: https://github.com/rust-lang/rust/issues/88581
    let d = lhs / other;
    let r = lhs % other;
    if (r > 0 && other > 0) || (r < 0 && other < 0) {
        d + 1
    } else {
        d
    }
}

pub(crate) fn div_floor(lhs: i32, other: i32) -> i32 {
    // TODO: See todo in `div_ceil`.
    let d = lhs / other;
    let r = lhs % other;
    if (r > 0 && other < 0) || (r < 0 && other > 0) {
        d - 1
    } else {
        d
    }
}
