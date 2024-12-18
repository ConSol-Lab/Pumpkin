#[cfg(all(not(test), not(feature = "debug-checks")))]
pub const PUMPKIN_ASSERT_LEVEL_DEFINITION: u8 = PUMPKIN_ASSERT_SIMPLE;

#[cfg(any(test, feature = "debug-checks"))]
pub const PUMPKIN_ASSERT_LEVEL_DEFINITION: u8 = PUMPKIN_ASSERT_SIMPLE;

pub const PUMPKIN_ASSERT_SIMPLE: u8 = 1;
pub const PUMPKIN_ASSERT_MODERATE: u8 = 2;
pub const PUMPKIN_ASSERT_ADVANCED: u8 = 3;
pub const PUMPKIN_ASSERT_EXTREME: u8 = 4;

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_simple {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_SIMPLE {
            assert!($($arg)*);
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_eq_simple {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_SIMPLE {
            assert_eq!($($arg)*);
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_ne_simple {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_SIMPLE {
            assert_ne!($($arg)*);
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_moderate {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_MODERATE {
            assert!($($arg)*);
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_ne_moderate {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_MODERATE {
            assert_ne!($($arg)*);
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_advanced {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_ADVANCED {
            assert!($($arg)*);
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! pumpkin_assert_extreme {
    ($($arg:tt)*) => {
        if $crate::asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::asserts::PUMPKIN_ASSERT_EXTREME {
            assert!($($arg)*);
        }
    };
}
