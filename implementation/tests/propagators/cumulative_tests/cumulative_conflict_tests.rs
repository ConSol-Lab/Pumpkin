#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
use crate::propagators::cumulative_tests::set_up_cumulative_state;
// 01.dzn
#[test]
fn cumulative_time_table_conflict_0() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((42, 50), 10, 10), ((45, 50), 7, 10)], 19, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 43), 10, 10), ((34, 43), 10, 7), ((34, 43), 10, 4)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((31, 38), 10, 7), ((32, 38), 9, 9), ((31, 38), 10, 4)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((36, 43), 10, 4), ((37, 43), 9, 9), ((36, 43), 10, 10)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_30() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((41, 46), 10, 8), ((42, 46), 9, 3), ((44, 46), 7, 6)],
        14,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_41() {
    // Test case with 2 variables
    let (_, result, _) = set_up_cumulative_state(&[((28, 30), 3, 10), ((27, 30), 4, 10)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_50() {
    // Test case with 2 variables
    let (_, result, _) = set_up_cumulative_state(&[((47, 53), 7, 10), ((47, 53), 7, 9)], 17, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_62() {
    // Test case with 2 variables
    let (_, result, _) = set_up_cumulative_state(&[((33, 40), 10, 8), ((40, 40), 3, 9)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_4() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 43), 10, 7), ((34, 43), 10, 10), ((34, 43), 10, 4)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_5() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((40, 49), 10, 4), ((41, 49), 9, 9), ((45, 49), 5, 9)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_23() {
    // Test case with 2 variables
    let (_, result, _) = set_up_cumulative_state(&[((41, 50), 10, 8), ((48, 50), 3, 9)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_6() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((42, 46), 10, 10), ((45, 46), 7, 10)], 19, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_7() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((41, 46), 10, 10), ((44, 46), 7, 10)], 19, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_8() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 43), 10, 4), ((35, 43), 9, 9), ((34, 43), 10, 10)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_9() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((41, 46), 10, 10), ((44, 46), 7, 10)], 19, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_10() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 43), 10, 7), ((34, 43), 10, 10), ((34, 43), 10, 4)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_11() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((35, 44), 10, 4), ((35, 44), 10, 7), ((36, 44), 9, 9)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_12() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 43), 10, 7), ((34, 43), 10, 10), ((34, 43), 10, 4)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_13() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 43), 10, 7), ((34, 43), 10, 10), ((34, 43), 10, 4)],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_109() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((23, 30), 8, 3),
            ((26, 30), 5, 5),
            ((27, 30), 4, 6),
            ((21, 30), 10, 7),
        ],
        19,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// J120_11_5.dzn
#[test]
fn cumulative_time_table_conflict_93() {
    // Test case with 5 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((225, 230), 7, 4),
            ((225, 230), 7, 1),
            ((225, 230), 7, 6),
            ((226, 230), 6, 1),
            ((223, 230), 9, 7),
        ],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_82() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((219, 226), 9, 8), ((221, 226), 7, 7), ((222, 226), 6, 6)],
        16,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_44() {
    // Test case with 5 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((225, 230), 7, 4),
            ((225, 230), 7, 1),
            ((225, 230), 7, 6),
            ((226, 230), 6, 1),
            ((223, 230), 9, 7),
        ],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_341() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((202, 209), 9, 1), ((206, 209), 5, 10), ((205, 209), 6, 7)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_17() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((225, 230), 6, 7), ((226, 230), 5, 8), ((222, 230), 9, 7)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_83() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((225, 230), 7, 7), ((226, 230), 6, 2), ((223, 230), 9, 10)],
        16,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_84() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((236, 241), 6, 5), ((237, 241), 5, 8), ((239, 241), 3, 5)],
        16,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_42() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((214, 220), 9, 2), ((217, 220), 6, 9), ((217, 220), 6, 7)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_18() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((206, 210), 6, 10), ((209, 210), 3, 10)], 17, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_85() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((216, 223), 9, 10), ((219, 223), 6, 7)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_43() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((205, 210), 6, 9), ((205, 210), 6, 7), ((202, 210), 9, 2)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_86() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((215, 223), 10, 3), ((218, 223), 7, 7), ((218, 223), 7, 7)],
        16,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_19() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((213, 222), 10, 5), ((217, 222), 6, 10), ((214, 222), 9, 7)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_87() {
    // Test case with 2 variables
    let (_, result, _) =
        set_up_cumulative_state(&[((217, 223), 9, 10), ((220, 223), 6, 7)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_434() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((211, 218), 9, 2), ((214, 218), 6, 7), ((214, 218), 6, 9)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_88() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((212, 217), 7, 7), ((209, 217), 10, 3), ((210, 217), 9, 10)],
        16,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_89() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((200, 208), 9, 9), ((203, 208), 6, 1), ((200, 208), 9, 10)],
        16,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_20() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((204, 207), 7, 5), ((205, 207), 6, 3), ((205, 207), 6, 10)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_21() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((205, 207), 6, 3), ((204, 207), 7, 5), ((205, 207), 6, 10)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_22() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[((202, 210), 10, 5), ((205, 210), 7, 5), ((209, 210), 3, 10)],
        17,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// pack_d/pack030.dzn
#[test]
fn cumulative_time_table_conflict_1922() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((349, 498), 169, 3),
            ((405, 498), 113, 4),
            ((418, 498), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1923() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((-113, 103), 222, 2),
            ((5, 103), 104, 1),
            ((-130, 103), 239, 2),
            ((-104, 103), 213, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1924() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((350, 498), 169, 3),
            ((406, 498), 113, 4),
            ((419, 498), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1925() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((352, 498), 169, 3),
            ((408, 498), 113, 4),
            ((421, 498), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1926() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((5, 217), 239, 2),
            ((22, 217), 222, 2),
            ((140, 217), 104, 4),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1927() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((350, 498), 169, 3),
            ((406, 498), 113, 4),
            ((419, 498), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1928() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((118, 329), 213, 3),
            ((225, 329), 106, 3),
            ((231, 329), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1929() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((4, 224), 239, 2),
            ((21, 224), 222, 2),
            ((30, 224), 213, 3),
            ((139, 224), 104, 4),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1930() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((2, 224), 239, 2),
            ((19, 224), 222, 2),
            ((28, 224), 213, 3),
            ((137, 224), 104, 4),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1931() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((351, 497), 169, 3),
            ((407, 497), 113, 4),
            ((420, 497), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1932() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((350, 497), 169, 3),
            ((406, 497), 113, 4),
            ((419, 497), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1933() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((3, 217), 239, 2),
            ((20, 217), 222, 2),
            ((138, 217), 104, 4),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1934() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((5, 217), 239, 2),
            ((22, 217), 222, 2),
            ((140, 217), 104, 4),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1935() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((-216, 5), 222, 2),
            ((2, 5), 4, 1),
            ((-233, 5), 239, 2),
            ((-107, 5), 113, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1936() {
    // Test case with 5 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((1, 3), 3, 2),
            ((2, 3), 2, 2),
            ((0, 3), 4, 1),
            ((-235, 3), 239, 2),
            ((-218, 3), 222, 2),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1937() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((118, 328), 213, 3),
            ((225, 328), 106, 3),
            ((231, 328), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_24() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((353, 497), 169, 2),
            ((300, 497), 222, 2),
            ((409, 497), 113, 2),
            ((422, 497), 100, 3),
        ],
        8,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1938() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((2, 217), 239, 2),
            ((19, 217), 222, 2),
            ((137, 217), 104, 4),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1939() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((-116, 80), 222, 2),
            ((2, 80), 104, 1),
            ((-107, 80), 213, 3),
            ((-133, 80), 239, 2),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1940() {
    // Test case with 3 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((118, 327), 213, 3),
            ((225, 327), 106, 3),
            ((231, 327), 100, 3),
        ],
        7,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// la_x/la40_x3.dzn
#[test]
fn cumulative_time_table_conflict_456() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13594, 13631), 42, 1),
            ((13595, 13631), 41, 1),
            ((13606, 13631), 30, 1),
            ((13574, 13631), 62, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_572() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13595, 13631), 41, 1),
            ((13594, 13631), 42, 1),
            ((13606, 13631), 30, 1),
            ((13574, 13631), 62, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_624() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13614, 13673), 62, 1),
            ((13634, 13673), 42, 1),
            ((13646, 13673), 30, 1),
            ((13646, 13673), 30, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_734() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13512, 13536), 30, 1),
            ((13515, 13536), 27, 1),
            ((13501, 13536), 41, 1),
            ((13500, 13536), 42, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_892() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13542, 13583), 42, 1),
            ((13542, 13583), 42, 1),
            ((13554, 13583), 30, 1),
            ((13557, 13583), 27, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_9321() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13331, 13388), 61, 1),
            ((13357, 13388), 35, 1),
            ((13330, 13388), 62, 1),
            ((13331, 13388), 61, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_10423() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13430, 13485), 62, 1),
            ((13430, 13485), 62, 1),
            ((13465, 13485), 27, 1),
            ((13465, 13485), 27, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_0432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13132, 13197), 70, 1),
            ((13132, 13197), 70, 1),
            ((13163, 13197), 39, 1),
            ((13167, 13197), 35, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_11432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13446, 13507), 62, 1),
            ((13466, 13507), 42, 1),
            ((13481, 13507), 27, 1),
            ((13481, 13507), 27, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1432423() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13217, 13255), 39, 1),
            ((13186, 13255), 70, 1),
            ((13232, 13255), 24, 1),
            ((13216, 13255), 40, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_12432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13312, 13364), 62, 1),
            ((13332, 13364), 42, 1),
            ((13312, 13364), 62, 1),
            ((13312, 13364), 62, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_0432432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13173, 13246), 92, 1),
            ((13173, 13246), 92, 1),
            ((13234, 13246), 31, 1),
            ((13234, 13246), 31, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_143243() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13093, 13188), 96, 1),
            ((13093, 13188), 96, 1),
            ((13093, 13188), 96, 1),
            ((13134, 13188), 55, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2432432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((13058, 13110), 59, 1),
            ((13077, 13110), 40, 1),
            ((13078, 13110), 39, 1),
            ((13093, 13110), 24, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_0434234() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12829, 12893), 75, 1),
            ((12832, 12893), 72, 1),
            ((12889, 12893), 15, 1),
            ((12889, 12893), 15, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12760, 12836), 79, 1),
            ((12830, 12836), 9, 1),
            ((12743, 12836), 96, 1),
            ((12743, 12836), 96, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_13432() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12914, 12951), 42, 1),
            ((12914, 12951), 42, 1),
            ((12915, 12951), 41, 1),
            ((12915, 12951), 41, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_14() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12899, 12925), 42, 1),
            ((12899, 12925), 42, 1),
            ((12914, 12925), 27, 1),
            ((12914, 12925), 27, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_15() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12880, 12921), 42, 1),
            ((12880, 12921), 42, 1),
            ((12895, 12921), 27, 1),
            ((12895, 12921), 27, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_16() {
    // Test case with 4 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12855, 12896), 42, 1),
            ((12855, 12896), 42, 1),
            ((12855, 12896), 42, 1),
            ((12870, 12896), 27, 1),
        ],
        3,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// J120_60_5.dzn
#[test]
fn cumulative_time_table_conflict_791() {
    // Test case with 6 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((74, 81), 8, 8),
            ((76, 81), 6, 6),
            ((76, 81), 6, 8),
            ((78, 81), 4, 10),
            ((76, 81), 6, 8),
            ((75, 81), 7, 9),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_792() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((67, 76), 10, 10),
            ((70, 76), 7, 5),
            ((68, 76), 9, 3),
            ((69, 76), 8, 8),
            ((71, 76), 6, 8),
            ((71, 76), 6, 4),
            ((68, 76), 9, 7),
            ((75, 76), 2, 7),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_286() {
    // Test case with 9 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((64, 73), 10, 4),
            ((71, 73), 3, 9),
            ((71, 73), 3, 8),
            ((65, 73), 9, 4),
            ((70, 73), 4, 7),
            ((71, 73), 3, 2),
            ((72, 73), 2, 1),
            ((65, 73), 9, 7),
            ((67, 73), 7, 2),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_479() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((68, 75), 9, 3),
            ((67, 75), 10, 2),
            ((70, 75), 7, 7),
            ((68, 75), 9, 8),
            ((73, 75), 4, 6),
            ((69, 75), 8, 3),
            ((74, 75), 3, 7),
        ],
        35,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_480() {
    // Test case with 6 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((62, 71), 10, 6),
            ((66, 71), 6, 6),
            ((68, 71), 4, 6),
            ((69, 71), 3, 7),
            ((69, 71), 3, 7),
            ((69, 71), 3, 6),
        ],
        35,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_793() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((69, 76), 9, 3),
            ((72, 76), 6, 7),
            ((70, 76), 8, 8),
            ((72, 76), 6, 8),
            ((72, 76), 6, 6),
            ((69, 76), 9, 7),
            ((68, 76), 10, 10),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_794() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((69, 75), 7, 5),
            ((67, 75), 9, 3),
            ((68, 75), 8, 8),
            ((70, 75), 6, 8),
            ((70, 75), 6, 7),
            ((70, 75), 6, 6),
            ((67, 75), 9, 7),
            ((74, 75), 2, 7),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_558() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((71, 76), 6, 5),
            ((68, 76), 9, 4),
            ((70, 76), 7, 3),
            ((69, 76), 8, 9),
            ((71, 76), 6, 8),
            ((71, 76), 6, 5),
            ((71, 76), 6, 5),
            ((68, 76), 9, 8),
        ],
        44,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_559() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((72, 80), 10, 3),
            ((74, 80), 8, 9),
            ((73, 80), 9, 8),
            ((76, 80), 6, 5),
            ((78, 80), 4, 9),
            ((72, 80), 10, 8),
            ((76, 80), 6, 4),
        ],
        44,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_287() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((70, 77), 9, 4),
            ((73, 77), 6, 6),
            ((72, 77), 7, 2),
            ((73, 77), 6, 7),
            ((71, 77), 8, 5),
            ((70, 77), 9, 7),
            ((69, 77), 10, 4),
            ((73, 77), 6, 8),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_288() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((73, 80), 8, 5),
            ((75, 80), 6, 5),
            ((71, 80), 10, 4),
            ((75, 80), 6, 7),
            ((72, 80), 9, 7),
            ((76, 80), 5, 8),
            ((75, 80), 6, 7),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_795() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((71, 79), 9, 3),
            ((72, 79), 8, 8),
            ((74, 79), 6, 8),
            ((74, 79), 6, 8),
            ((70, 79), 10, 10),
            ((74, 79), 6, 7),
            ((71, 79), 9, 7),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_289() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((78, 84), 7, 9),
            ((75, 84), 10, 4),
            ((76, 84), 9, 7),
            ((80, 84), 5, 8),
            ((79, 84), 6, 5),
            ((75, 84), 10, 2),
            ((78, 84), 7, 7),
            ((84, 84), 1, 3),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_796() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((73, 82), 10, 10),
            ((74, 82), 9, 7),
            ((77, 82), 6, 4),
            ((79, 82), 4, 10),
            ((78, 82), 5, 7),
            ((73, 82), 10, 9),
            ((77, 82), 6, 8),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_290() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((69, 76), 9, 4),
            ((72, 76), 6, 7),
            ((71, 76), 7, 2),
            ((72, 76), 6, 8),
            ((72, 76), 6, 6),
            ((70, 76), 8, 5),
            ((75, 76), 3, 9),
            ((72, 76), 6, 5),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_291() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((78, 84), 7, 9),
            ((75, 84), 10, 4),
            ((76, 84), 9, 7),
            ((80, 84), 5, 8),
            ((79, 84), 6, 5),
            ((75, 84), 10, 2),
            ((78, 84), 7, 7),
            ((84, 84), 1, 3),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_797() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((72, 81), 10, 10),
            ((74, 81), 8, 8),
            ((76, 81), 6, 4),
            ((78, 81), 4, 10),
            ((76, 81), 6, 8),
            ((76, 81), 6, 7),
            ((72, 81), 10, 3),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_292() {
    // Test case with 9 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((80, 88), 10, 3),
            ((80, 88), 10, 2),
            ((83, 88), 7, 9),
            ((83, 88), 7, 7),
            ((84, 88), 6, 7),
            ((80, 88), 10, 3),
            ((84, 88), 6, 6),
            ((84, 88), 6, 5),
            ((83, 88), 7, 5),
        ],
        42,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_483() {
    // Test case with 7 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((68, 74), 7, 7),
            ((69, 74), 6, 8),
            ((66, 74), 9, 3),
            ((69, 74), 6, 1),
            ((71, 74), 4, 6),
            ((72, 74), 3, 7),
            ((69, 74), 6, 6),
        ],
        35,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_799() {
    // Test case with 8 variables
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((68, 76), 9, 3),
            ((69, 76), 8, 8),
            ((71, 76), 6, 8),
            ((71, 76), 6, 7),
            ((71, 76), 6, 6),
            ((68, 76), 9, 7),
            ((75, 76), 2, 7),
            ((67, 76), 10, 10),
        ],
        48,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}
