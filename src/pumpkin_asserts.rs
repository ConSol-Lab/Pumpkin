pub(crate) const PUMPKIN_ASSERT_LEVEL_DEFINITION: u8 = PUMPKIN_ASSERT_SIMPLE;

pub(crate) const PUMPKIN_ASSERT_SIMPLE: u8 = 1;
pub(crate) const PUMPKIN_ASSERT_MODERATE: u8 = 2;
pub(crate) const PUMPKIN_ASSERT_ADVANCED: u8 = 3;
pub(crate) const PUMPKIN_ASSERT_EXTREME: u8 = 4;

macro_rules! print_pumpkin_assert_warning_message {
    () => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_MODERATE {
            warn!("Potential performance degradation: the Pumpkin assert level is set to {}, meaning many debug asserts are active which may result in performance degradation.", $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION);
        };
    };
}

macro_rules! pumpkin_assert_simple {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_SIMPLE {
            assert!($($arg)*);
        }
    };
}

macro_rules! pumpkin_assert_eq_simple {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_SIMPLE {
            assert_eq!($($arg)*);
        }
    };
}

macro_rules! pumpkin_assert_ne_simple {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_SIMPLE {
            assert_ne!($($arg)*);
        }
    };
}

macro_rules! pumpkin_assert_moderate {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_MODERATE {
            assert!($($arg)*);
        }
    };
}

macro_rules! pumpkin_assert_ne_moderate {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_MODERATE {
            assert_ne!($($arg)*);
        }
    };
}

macro_rules! pumpkin_assert_advanced {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_ADVANCED {
            assert!($($arg)*);
        }
    };
}

macro_rules! pumpkin_assert_extreme {
    ($($arg:tt)*) => {
        if $crate::pumpkin_asserts::PUMPKIN_ASSERT_LEVEL_DEFINITION >= $crate::pumpkin_asserts::PUMPKIN_ASSERT_EXTREME {
            assert!($($arg)*);
        }
    };
}

pub(crate) use print_pumpkin_assert_warning_message;
pub(crate) use pumpkin_assert_advanced;
pub(crate) use pumpkin_assert_eq_simple;
pub(crate) use pumpkin_assert_extreme;
pub(crate) use pumpkin_assert_moderate;
pub(crate) use pumpkin_assert_ne_moderate;
pub(crate) use pumpkin_assert_ne_simple;
pub(crate) use pumpkin_assert_simple;
