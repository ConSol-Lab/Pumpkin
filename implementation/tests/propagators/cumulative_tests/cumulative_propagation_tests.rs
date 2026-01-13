#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
use more_asserts::assert_ge;
use more_asserts::assert_le;

use crate::propagators::cumulative_tests::set_up_cumulative_state;

// 01.dzn
#[test]
fn cumulative_time_table_lower_bound_781637() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((33, 36), 10, 10), ((34, 71), 3, 10)], 19, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 43)
}

#[test]
fn cumulative_time_table_lower_bound_408943() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((43, 46), 10, 8), ((46, 46), 7, 6), ((45, 67), 2, 9)],
        14,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 53)
}

#[test]
fn cumulative_time_table_lower_bound_748556() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((34, 43), 10, 7),
            ((34, 43), 10, 4),
            ((42, 43), 2, 1),
            ((42, 65), 2, 8),
        ],
        19,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 44)
}

#[test]
fn cumulative_time_table_lower_bound_255324() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((40, 45), 10, 4), ((45, 45), 5, 9), ((43, 71), 3, 10)],
        19,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 50)
}

#[test]
fn cumulative_time_table_lower_bound_216625() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((31, 33), 5, 6), ((32, 65), 2, 9)], 13, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 36)
}

#[test]
fn cumulative_time_table_lower_bound_236582() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((33, 33), 2, 8), ((32, 65), 2, 9)], 13, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 35)
}

#[test]
fn cumulative_time_table_lower_bound_103142() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((35, 35), 5, 6), ((34, 65), 2, 9)], 13, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 40)
}

#[test]
fn cumulative_time_table_lower_bound_984553() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((35, 35), 5, 6), ((34, 65), 2, 9)], 13, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 40)
}

#[test]
fn cumulative_time_table_lower_bound_655362() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((60, 64), 6, 10), ((62, 71), 3, 9)], 14, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 66)
}

#[test]
fn cumulative_time_table_lower_bound_170672() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((33, 40), 9, 6), ((32, 40), 10, 10), ((38, 52), 3, 6)],
        17,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 42)
}

#[test]
fn cumulative_time_table_upper_bound_283384() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((56, 57), 4, 4),
            ((55, 57), 5, 8),
            ((55, 57), 5, 4),
            ((49, 59), 2, 3),
        ],
        17,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 55);
}

#[test]
fn cumulative_time_table_upper_bound_399247() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((57, 61), 5, 7), ((47, 61), 8, 7)], 13, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 53);
}

#[test]
fn cumulative_time_table_upper_bound_691616() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((55, 60), 6, 10), ((48, 60), 4, 9)], 14, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 56);
}

#[test]
fn cumulative_time_table_upper_bound_164601() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((48, 53), 8, 8), ((46, 55), 2, 8)], 14, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 51);
}

#[test]
fn cumulative_time_table_upper_bound_500334() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((58, 62), 5, 7), ((56, 62), 7, 6), ((50, 62), 6, 1)],
        13,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 56);
}

#[test]
fn cumulative_time_table_upper_bound_687951() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((56, 61), 6, 8), ((57, 61), 5, 8), ((37, 61), 4, 7)],
        19,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 57);
}

#[test]
fn cumulative_time_table_upper_bound_329136() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((59, 62), 6, 1), ((58, 62), 7, 6), ((48, 64), 5, 7)],
        13,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 57);
}

#[test]
fn cumulative_time_table_upper_bound_100171() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((52, 53), 7, 6), ((46, 58), 4, 9)], 14, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 49);
}

#[test]
fn cumulative_time_table_upper_bound_705892() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((43, 47), 10, 7), ((44, 47), 9, 9), ((37, 52), 5, 5)],
        19,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 42);
}

#[test]
fn cumulative_time_table_upper_bound_205174() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((60, 62), 6, 1), ((59, 62), 7, 6), ((49, 65), 5, 7)],
        13,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 57);
}

// J120_11_5.dzn
#[test]
fn cumulative_time_table_lower_bound_659546() {
    // Test case with 4 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((109, 112), 5, 4),
            ((106, 112), 8, 3),
            ((106, 112), 8, 2),
            ((104, 112), 10, 3),
            ((112, 227), 1, 7),
        ],
        18,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 114)
}

#[test]
fn cumulative_time_table_lower_bound_897461() {
    // Test case with 4 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((109, 114), 8, 3),
            ((109, 114), 8, 2),
            ((107, 114), 10, 3),
            ((111, 114), 6, 6),
            ((110, 237), 5, 7),
        ],
        18,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 117)
}

#[test]
fn cumulative_time_table_lower_bound_474694() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((112, 120), 10, 3), ((120, 120), 2, 9), ((112, 217), 9, 8)],
        16,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 122)
}

#[test]
fn cumulative_time_table_lower_bound_313000() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((118, 122), 5, 7), ((114, 122), 9, 6), ((115, 233), 8, 8)],
        18,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 123)
}

#[test]
fn cumulative_time_table_lower_bound_852925() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((122, 122), 9, 10), ((114, 235), 9, 10)], 16, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 131)
}

#[test]
fn cumulative_time_table_lower_bound_569402() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((133, 137), 8, 8), ((137, 137), 4, 10), ((132, 228), 6, 1)],
        18,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 141)
}

#[test]
fn cumulative_time_table_lower_bound_740055() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((135, 140), 8, 2), ((140, 140), 3, 10), ((138, 208), 3, 9)],
        17,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 143)
}

#[test]
fn cumulative_time_table_lower_bound_243778() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((141, 144), 7, 10), ((137, 211), 8, 10)], 18, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 148)
}

#[test]
fn cumulative_time_table_lower_bound_492482() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((161, 164), 9, 2),
            ((161, 164), 9, 1),
            ((164, 164), 6, 10),
            ((155, 222), 10, 9),
        ],
        17,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 170)
}

#[test]
fn cumulative_time_table_lower_bound_47610() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((170, 171), 8, 8), ((171, 221), 1, 9)], 16, false);

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 178)
}

#[test]
fn cumulative_time_table_upper_bound_505270() {
    // Test case with 4 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((223, 229), 7, 1),
            ((223, 229), 7, 1),
            ((223, 229), 7, 6),
            ((225, 229), 5, 4),
            ((220, 229), 9, 7),
        ],
        18,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 220);
}

#[test]
fn cumulative_time_table_upper_bound_690145() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((217, 225), 9, 8), ((220, 225), 6, 6), ((214, 225), 10, 3)],
        16,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 215);
}

#[test]
fn cumulative_time_table_upper_bound_840123() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((217, 222), 9, 2), ((216, 222), 10, 10), ((212, 225), 6, 7)],
        17,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 216);
}

#[test]
fn cumulative_time_table_upper_bound_20421() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((209, 216), 10, 10), ((208, 218), 2, 8)], 17, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 214);
}

#[test]
fn cumulative_time_table_upper_bound_621430() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((224, 229), 9, 10), ((215, 232), 9, 8)], 16, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 220);
}

#[test]
fn cumulative_time_table_upper_bound_470900() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((234, 238), 5, 10), ((215, 238), 5, 10)], 17, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 233);
}

#[test]
fn cumulative_time_table_upper_bound_621563() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((213, 216), 6, 8), ((212, 216), 7, 7), ((213, 218), 3, 5)],
        16,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 213);
}

#[test]
fn cumulative_time_table_upper_bound_776538() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((222, 229), 9, 10), ((210, 230), 7, 7)], 16, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 222);
}

#[test]
fn cumulative_time_table_upper_bound_406101() {
    // Test case with 1 variables
    let (solver, result, variables) =
        set_up_cumulative_state(&[((225, 229), 9, 10), ((213, 233), 6, 8)], 16, false);

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 223);
}

#[test]
fn cumulative_time_table_upper_bound_63452() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[((216, 223), 9, 2), ((219, 223), 6, 7), ((210, 224), 10, 10)],
        17,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 213);
}

// pack_d/pack030.dzn
#[test]
fn cumulative_time_table_lower_bound_878034() {
    // Test case with 4 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-20, 120), 239, 2),
            ((-3, 120), 222, 2),
            ((6, 120), 213, 1),
            ((119, 120), 100, 3),
            ((118, 628), 3, 3),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 219)
}

#[test]
fn cumulative_time_table_lower_bound_40161() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((92, 259), 239, 2),
            ((118, 259), 213, 3),
            ((160, 531), 100, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 331)
}

#[test]
fn cumulative_time_table_lower_bound_282825() {
    // Test case with 4 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-20, 120), 239, 2),
            ((-3, 120), 222, 2),
            ((6, 120), 213, 1),
            ((119, 120), 100, 3),
            ((118, 628), 3, 3),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 219)
}

#[test]
fn cumulative_time_table_lower_bound_489437() {
    // Test case with 4 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-20, 120), 239, 2),
            ((-3, 120), 222, 2),
            ((6, 120), 213, 1),
            ((119, 120), 100, 3),
            ((17, 527), 104, 1),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 219)
}

#[test]
fn cumulative_time_table_lower_bound_284650() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((2, 104), 113, 3),
            ((-107, 104), 222, 2),
            ((102, 628), 3, 4),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 115)
}

#[test]
fn cumulative_time_table_lower_bound_985128() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((92, 270), 239, 2),
            ((118, 270), 213, 3),
            ((268, 628), 3, 4),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 331)
}

#[test]
fn cumulative_time_table_lower_bound_431583() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((92, 270), 239, 2),
            ((118, 270), 213, 3),
            ((167, 527), 104, 4),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 331)
}

#[test]
fn cumulative_time_table_lower_bound_4326() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((92, 271), 239, 2),
            ((118, 271), 213, 3),
            ((269, 628), 3, 4),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 331)
}

#[test]
fn cumulative_time_table_lower_bound_773842() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((50, 120), 222, 2),
            ((33, 120), 239, 2),
            ((59, 120), 213, 3),
            ((21, 531), 100, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 272)
}

#[test]
fn cumulative_time_table_lower_bound_267296() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((54, 275), 239, 2),
            ((80, 275), 213, 3),
            ((163, 518), 113, 4),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 293)
}

#[test]
fn cumulative_time_table_upper_bound_164301() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-20, 126), 239, 2),
            ((-3, 126), 222, 2),
            ((106, 126), 113, 3),
            ((107, 218), 3, 2),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 123);
}

#[test]
fn cumulative_time_table_upper_bound_653103() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-20, 126), 239, 2),
            ((-3, 126), 222, 2),
            ((106, 126), 113, 3),
            ((108, 218), 3, 2),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 123);
}

#[test]
fn cumulative_time_table_upper_bound_936486() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((335, 531), 213, 3),
            ((444, 531), 104, 4),
            ((352, 547), 100, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 431);
}

#[test]
fn cumulative_time_table_upper_bound_746469() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((326, 522), 213, 3),
            ((426, 522), 113, 4),
            ((353, 538), 100, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 422);
}

#[test]
fn cumulative_time_table_upper_bound_253367() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((245, 466), 222, 2),
            ((298, 466), 169, 3),
            ((244, 466), 113, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 353);
}

#[test]
fn cumulative_time_table_upper_bound_76442() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-14, 217), 239, 2),
            ((3, 217), 222, 2),
            ((12, 217), 213, 3),
            ((104, 224), 113, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 104);
}

#[test]
fn cumulative_time_table_upper_bound_796026() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((104, 239), 222, 2),
            ((113, 239), 213, 3),
            ((116, 325), 100, 3),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 139);
}

#[test]
fn cumulative_time_table_upper_bound_58436() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((0, 10), 104, 3),
            ((-135, 10), 239, 2),
            ((-118, 10), 222, 2),
            ((7, 103), 3, 3),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 7);
}

#[test]
fn cumulative_time_table_upper_bound_944538() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-20, 126), 239, 2),
            ((-3, 126), 222, 2),
            ((106, 126), 113, 3),
            ((108, 218), 3, 2),
        ],
        8,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 123);
}

#[test]
fn cumulative_time_table_upper_bound_487576() {
    // Test case with 2 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((318, 466), 213, 3),
            ((362, 466), 169, 3),
            ((346, 530), 104, 4),
        ],
        7,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 362);
}

// la_x/la40_x3.dzn
#[test]
fn cumulative_time_table_lower_bound_63423() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((-15, 0), 64, 1),
            ((-38, 0), 87, 1),
            ((0, 0), 49, 1),
            ((-63, 33742), 64, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 49)
}

#[test]
fn cumulative_time_table_lower_bound_21641() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((0, 49), 64, 1),
            ((-23, 49), 87, 1),
            ((0, 49), 64, 1),
            ((1, 33518), 49, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 64)
}

#[test]
fn cumulative_time_table_lower_bound_816932() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((11, 96), 88, 1),
            ((87, 96), 12, 1),
            ((11, 96), 88, 1),
            ((90, 33750), 7, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 99)
}

#[test]
fn cumulative_time_table_lower_bound_430273() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((67, 119), 61, 1),
            ((107, 119), 21, 1),
            ((38, 119), 90, 1),
            ((99, 33745), 21, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 128)
}

#[test]
fn cumulative_time_table_lower_bound_895979() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((168, 222), 82, 1),
            ((173, 222), 77, 1),
            ((222, 222), 28, 1),
            ((141, 33937), 82, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 250)
}

#[test]
fn cumulative_time_table_lower_bound_76650() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((220, 244), 46, 1),
            ((244, 244), 22, 1),
            ((192, 244), 74, 1),
            ((230, 33870), 15, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 266)
}

#[test]
fn cumulative_time_table_lower_bound_850429() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((273, 327), 73, 1),
            ((271, 327), 75, 1),
            ((274, 327), 72, 1),
            ((253, 33928), 75, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 346)
}

#[test]
fn cumulative_time_table_lower_bound_22645() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((315, 346), 75, 1),
            ((318, 346), 72, 1),
            ((324, 346), 66, 1),
            ((274, 33868), 73, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 390)
}

#[test]
fn cumulative_time_table_lower_bound_234374() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((320, 407), 90, 1),
            ((404, 407), 6, 1),
            ((348, 407), 62, 1),
            ((341, 33865), 67, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 410)
}

#[test]
fn cumulative_time_table_lower_bound_91645() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((417, 437), 26, 1),
            ((429, 437), 14, 1),
            ((437, 437), 6, 1),
            ((393, 34052), 45, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 443)
}

#[test]
fn cumulative_time_table_upper_bound_725687() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1029, 1071), 44, 1),
            ((988, 1071), 85, 1),
            ((988, 1071), 85, 1),
            ((1029, 1072), 42, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1029);
}

#[test]
fn cumulative_time_table_upper_bound_671920() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1101, 1187), 94, 1),
            ((1134, 1187), 61, 1),
            ((1111, 1187), 84, 1),
            ((989, 1194), 94, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1093);
}

#[test]
fn cumulative_time_table_upper_bound_387037() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1100, 1187), 94, 1),
            ((1133, 1187), 61, 1),
            ((1110, 1187), 84, 1),
            ((989, 1193), 94, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1093);
}

#[test]
fn cumulative_time_table_upper_bound_48391() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1003, 1071), 85, 1),
            ((1044, 1071), 44, 1),
            ((1003, 1071), 85, 1),
            ((1024, 1087), 42, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1029);
}

#[test]
fn cumulative_time_table_upper_bound_765616() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((999, 1071), 85, 1),
            ((1005, 1071), 79, 1),
            ((999, 1071), 85, 1),
            ((1005, 1083), 44, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1027);
}

#[test]
fn cumulative_time_table_upper_bound_69165() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1096, 1187), 94, 1),
            ((1129, 1187), 61, 1),
            ((1106, 1187), 84, 1),
            ((994, 1189), 94, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1093);
}

#[test]
fn cumulative_time_table_upper_bound_26455() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1096, 1187), 94, 1),
            ((1129, 1187), 61, 1),
            ((1106, 1187), 84, 1),
            ((994, 1189), 94, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1093);
}

#[test]
fn cumulative_time_table_upper_bound_527108() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1060, 1089), 79, 1),
            ((1054, 1089), 85, 1),
            ((1084, 1089), 55, 1),
            ((1045, 1138), 42, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1047);
}

#[test]
fn cumulative_time_table_upper_bound_274579() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1001, 1071), 96, 1),
            ((1018, 1071), 79, 1),
            ((1012, 1071), 85, 1),
            ((1024, 1096), 44, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1027);
}

#[test]
fn cumulative_time_table_upper_bound_327708() {
    // Test case with 3 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((1156, 1217), 96, 1),
            ((1156, 1217), 96, 1),
            ((1156, 1217), 96, 1),
            ((1156, 1251), 55, 1),
        ],
        3,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 1162);
}

// J120_60_5.dzn
#[test]
fn cumulative_time_table_lower_bound_363043() {
    // Test case with 5 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((73, 79), 8, 8),
            ((71, 79), 10, 10),
            ((72, 79), 9, 7),
            ((77, 79), 4, 10),
            ((75, 79), 6, 8),
            ((78, 96), 2, 10),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 81)
}

#[test]
fn cumulative_time_table_lower_bound_997298() {
    // Test case with 7 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((67, 71), 6, 1),
            ((66, 71), 7, 7),
            ((70, 71), 3, 7),
            ((70, 71), 3, 7),
            ((64, 71), 9, 3),
            ((67, 71), 6, 8),
            ((71, 71), 2, 2),
            ((66, 87), 6, 7),
        ],
        35,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 73)
}

#[test]
fn cumulative_time_table_lower_bound_735309() {
    // Test case with 8 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((67, 71), 6, 5),
            ((66, 71), 7, 3),
            ((70, 71), 3, 8),
            ((64, 71), 9, 4),
            ((70, 71), 3, 4),
            ((64, 71), 9, 8),
            ((67, 71), 6, 4),
            ((71, 71), 2, 4),
            ((68, 73), 4, 5),
        ],
        44,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 73)
}

#[test]
fn cumulative_time_table_lower_bound_43689() {
    // Test case with 8 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((68, 73), 6, 5),
            ((67, 73), 7, 3),
            ((65, 73), 9, 4),
            ((64, 73), 10, 3),
            ((66, 73), 8, 9),
            ((71, 73), 3, 8),
            ((68, 73), 6, 5),
            ((70, 73), 4, 5),
            ((65, 76), 9, 8),
        ],
        44,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 74)
}

#[test]
fn cumulative_time_table_lower_bound_771104() {
    // Test case with 7 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((67, 71), 6, 6),
            ((66, 71), 7, 2),
            ((70, 71), 3, 8),
            ((70, 71), 3, 8),
            ((64, 71), 9, 4),
            ((64, 71), 9, 7),
            ((71, 71), 2, 1),
            ((69, 91), 3, 9),
        ],
        42,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 73)
}

#[test]
fn cumulative_time_table_lower_bound_99488() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((61, 70), 10, 8),
            ((65, 70), 6, 8),
            ((68, 70), 3, 7),
            ((68, 70), 3, 7),
            ((65, 70), 6, 6),
            ((62, 70), 9, 3),
            ((61, 84), 10, 10),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 71)
}

#[test]
fn cumulative_time_table_lower_bound_940534() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((68, 73), 6, 1),
            ((68, 73), 6, 8),
            ((65, 73), 9, 3),
            ((67, 73), 7, 7),
            ((70, 73), 4, 6),
            ((66, 73), 8, 3),
            ((65, 78), 9, 8),
        ],
        35,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 74)
}

#[test]
fn cumulative_time_table_lower_bound_898358() {
    // Test case with 8 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((66, 71), 6, 6),
            ((69, 71), 3, 2),
            ((69, 71), 3, 9),
            ((69, 71), 3, 8),
            ((63, 71), 9, 4),
            ((63, 71), 9, 7),
            ((66, 71), 6, 5),
            ((70, 71), 2, 1),
            ((65, 72), 7, 2),
        ],
        42,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 72)
}

#[test]
fn cumulative_time_table_lower_bound_41601() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((69, 77), 9, 8),
            ((69, 77), 9, 3),
            ((71, 77), 7, 7),
            ((70, 77), 8, 3),
            ((72, 77), 6, 6),
            ((72, 77), 6, 8),
            ((72, 87), 6, 7),
        ],
        35,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 78)
}

#[test]
fn cumulative_time_table_lower_bound_978770() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((69, 77), 9, 8),
            ((69, 77), 9, 3),
            ((71, 77), 7, 7),
            ((70, 77), 8, 3),
            ((72, 77), 6, 6),
            ((72, 77), 6, 8),
            ((75, 91), 3, 6),
        ],
        35,
        false,
    );

    assert!(result.is_ok());
    assert_ge!(solver.lower_bound(*variables.last().unwrap()), 78)
}

#[test]
fn cumulative_time_table_upper_bound_67131() {
    // Test case with 8 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((78, 87), 10, 2),
            ((83, 87), 5, 8),
            ((81, 87), 7, 5),
            ((78, 87), 10, 8),
            ((78, 87), 10, 4),
            ((84, 87), 4, 8),
            ((82, 87), 6, 4),
            ((81, 87), 7, 4),
            ((81, 87), 6, 5),
        ],
        44,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 81);
}

#[test]
fn cumulative_time_table_upper_bound_281875() {
    // Test case with 5 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((85, 94), 10, 10),
            ((90, 94), 5, 10),
            ((88, 94), 7, 3),
            ((85, 94), 10, 6),
            ((86, 94), 9, 10),
            ((91, 94), 3, 10),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 91);
}

#[test]
fn cumulative_time_table_upper_bound_349687() {
    // Test case with 8 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((83, 92), 10, 3),
            ((84, 92), 9, 4),
            ((83, 92), 10, 3),
            ((86, 92), 7, 5),
            ((88, 92), 5, 6),
            ((89, 92), 4, 4),
            ((86, 92), 7, 1),
            ((83, 92), 10, 9),
            ((85, 92), 5, 8),
        ],
        42,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 87);
}

#[test]
fn cumulative_time_table_upper_bound_601389() {
    // Test case with 5 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((81, 90), 10, 9),
            ((81, 90), 10, 10),
            ((84, 90), 7, 9),
            ((87, 90), 4, 10),
            ((86, 90), 5, 10),
            ((84, 90), 6, 4),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 84);
}

#[test]
fn cumulative_time_table_upper_bound_312900() {
    // Test case with 5 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((73, 79), 8, 9),
            ((72, 79), 9, 8),
            ((74, 79), 7, 10),
            ((77, 79), 4, 9),
            ((76, 79), 5, 8),
            ((78, 80), 1, 9),
        ],
        44,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 78);
}

#[test]
fn cumulative_time_table_upper_bound_772648() {
    // Test case with 5 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((90, 94), 5, 10),
            ((91, 94), 4, 10),
            ((88, 94), 7, 3),
            ((85, 94), 10, 6),
            ((86, 94), 9, 10),
            ((84, 94), 10, 10),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 84);
}

#[test]
fn cumulative_time_table_upper_bound_648178() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((77, 82), 6, 8),
            ((77, 82), 6, 6),
            ((79, 82), 4, 10),
            ((73, 82), 10, 3),
            ((73, 82), 10, 9),
            ((76, 82), 7, 9),
            ((81, 82), 1, 6),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 81);
}

#[test]
fn cumulative_time_table_upper_bound_72292() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((79, 87), 10, 5),
            ((82, 87), 7, 3),
            ((79, 87), 10, 7),
            ((79, 87), 10, 1),
            ((83, 87), 6, 7),
            ((82, 87), 7, 7),
            ((78, 88), 6, 6),
        ],
        35,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 81);
}

#[test]
fn cumulative_time_table_upper_bound_857576() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((78, 87), 10, 5),
            ((81, 87), 7, 3),
            ((78, 87), 10, 7),
            ((78, 87), 10, 1),
            ((82, 87), 6, 7),
            ((81, 87), 7, 7),
            ((77, 87), 6, 6),
        ],
        35,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 81);
}

#[test]
fn cumulative_time_table_upper_bound_468815() {
    // Test case with 6 variables
    let (solver, result, variables) = set_up_cumulative_state(
        &[
            ((83, 92), 10, 10),
            ((88, 92), 5, 10),
            ((88, 92), 5, 7),
            ((89, 92), 4, 10),
            ((86, 92), 7, 3),
            ((83, 92), 10, 6),
            ((82, 92), 6, 4),
        ],
        48,
        false,
    );

    assert!(result.is_ok());
    assert_le!(solver.upper_bound(*variables.last().unwrap()), 86);
}
