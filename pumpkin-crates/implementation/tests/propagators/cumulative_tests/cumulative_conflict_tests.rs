#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
use crate::propagators::cumulative_tests::set_up_cumulative_state;

#[test]
fn cumulative_time_table_conflict_384() {
    let (_, result, _) = set_up_cumulative_state(&[((94, 100), 8, 8), ((96, 100), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_253() {
    let (_, result, _) =
        set_up_cumulative_state(&[((16, 23), 9, 9), ((16, 23), 9, 5), ((21, 23), 4, 6)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_773() {
    let (_, result, _) = set_up_cumulative_state(&[((48, 49), 2, 8), ((46, 49), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_862() {
    let (_, result, _) = set_up_cumulative_state(&[((59, 66), 8, 8), ((61, 66), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_241() {
    let (_, result, _) =
        set_up_cumulative_state(&[((56, 60), 5, 4), ((54, 60), 7, 9), ((54, 60), 7, 10)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1139() {
    let (_, result, _) = set_up_cumulative_state(&[((56, 57), 2, 8), ((52, 57), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1148() {
    let (_, result, _) = set_up_cumulative_state(&[((60, 66), 8, 8), ((62, 66), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_391() {
    let (_, result, _) =
        set_up_cumulative_state(&[((43, 49), 7, 10), ((47, 49), 3, 5), ((46, 49), 4, 4)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_401() {
    let (_, result, _) =
        set_up_cumulative_state(&[((48, 53), 7, 10), ((53, 53), 2, 7), ((53, 53), 2, 3)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1142() {
    let (_, result, _) = set_up_cumulative_state(&[((48, 50), 3, 7), ((49, 50), 2, 9)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1405() {
    let (_, result, _) = set_up_cumulative_state(&[((26, 31), 8, 8), ((30, 31), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1408() {
    let (_, result, _) = set_up_cumulative_state(&[((22, 29), 8, 8), ((26, 29), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1222() {
    let (_, result, _) =
        set_up_cumulative_state(&[((12, 20), 9, 4), ((18, 20), 3, 6), ((13, 20), 8, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_893() {
    let (_, result, _) = set_up_cumulative_state(
        &[((29, 33), 5, 5), ((30, 33), 4, 6), ((24, 33), 10, 10)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_914() {
    let (_, result, _) =
        set_up_cumulative_state(&[((21, 24), 4, 7), ((21, 24), 4, 6), ((15, 24), 10, 7)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1731() {
    let (_, result, _) = set_up_cumulative_state(&[((34, 36), 3, 9), ((35, 36), 2, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1705() {
    let (_, result, _) = set_up_cumulative_state(&[((25, 32), 8, 7), ((30, 32), 3, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1761() {
    let (_, result, _) = set_up_cumulative_state(&[((31, 38), 10, 8), ((38, 38), 3, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_751() {
    let (_, result, _) = set_up_cumulative_state(&[((42, 48), 7, 9), ((42, 48), 7, 10)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1828() {
    let (_, result, _) = set_up_cumulative_state(&[((38, 38), 3, 9), ((31, 38), 10, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1822() {
    let (_, result, _) = set_up_cumulative_state(&[((33, 34), 3, 7), ((33, 34), 3, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1872() {
    let (_, result, _) = set_up_cumulative_state(&[((11, 20), 10, 8), ((13, 20), 8, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1876() {
    let (_, result, _) = set_up_cumulative_state(&[((33, 34), 3, 7), ((33, 34), 3, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_815() {
    let (_, result, _) = set_up_cumulative_state(
        &[((26, 32), 8, 3), ((24, 32), 10, 10), ((31, 32), 3, 6)],
        17,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_861() {
    let (_, result, _) = set_up_cumulative_state(&[((41, 45), 7, 9), ((41, 45), 7, 10)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1962() {
    let (_, result, _) = set_up_cumulative_state(&[((24, 28), 8, 8), ((22, 28), 10, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1981() {
    let (_, result, _) = set_up_cumulative_state(&[((22, 24), 3, 10), ((17, 24), 8, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1970() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 26), 8, 7), ((26, 26), 3, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2044() {
    let (_, result, _) =
        set_up_cumulative_state(&[((22, 27), 9, 3), ((23, 27), 8, 8), ((21, 27), 10, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2130() {
    let (_, result, _) = set_up_cumulative_state(&[((25, 31), 8, 8), ((23, 31), 10, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2139() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 28), 8, 8), ((25, 28), 4, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2145() {
    let (_, result, _) = set_up_cumulative_state(&[((10, 13), 4, 10), ((6, 13), 8, 7)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_951() {
    let (_, result, _) = set_up_cumulative_state(&[((43, 48), 7, 10), ((43, 48), 7, 9)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2085() {
    let (_, result, _) = set_up_cumulative_state(&[((20, 23), 4, 7), ((22, 23), 2, 8)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1458() {
    let (_, result, _) = set_up_cumulative_state(
        &[((32, 35), 4, 7), ((26, 35), 10, 10), ((33, 35), 3, 4)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1466() {
    let (_, result, _) =
        set_up_cumulative_state(&[((12, 20), 9, 9), ((17, 20), 4, 6), ((12, 20), 9, 5)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1006() {
    let (_, result, _) =
        set_up_cumulative_state(&[((43, 48), 7, 10), ((48, 48), 2, 7), ((46, 48), 4, 4)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2348() {
    let (_, result, _) = set_up_cumulative_state(&[((38, 46), 10, 8), ((44, 46), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2376() {
    let (_, result, _) = set_up_cumulative_state(&[((32, 35), 4, 9), ((33, 35), 3, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2382() {
    let (_, result, _) = set_up_cumulative_state(&[((32, 35), 4, 9), ((33, 35), 3, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2291() {
    let (_, result, _) = set_up_cumulative_state(&[((50, 53), 5, 7), ((53, 53), 2, 8)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1554() {
    let (_, result, _) = set_up_cumulative_state(
        &[((16, 25), 10, 7), ((21, 25), 5, 5), ((16, 25), 10, 10)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2528() {
    let (_, result, _) = set_up_cumulative_state(&[((24, 26), 4, 10), ((25, 26), 3, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2539() {
    let (_, result, _) = set_up_cumulative_state(&[((30, 31), 4, 9), ((31, 31), 3, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2397() {
    let (_, result, _) = set_up_cumulative_state(&[((31, 32), 3, 7), ((31, 32), 3, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1585() {
    let (_, result, _) = set_up_cumulative_state(
        &[((21, 29), 9, 9), ((20, 29), 10, 10), ((25, 29), 5, 5)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1616() {
    let (_, result, _) = set_up_cumulative_state(
        &[((30, 38), 10, 7), ((30, 38), 10, 4), ((30, 38), 10, 10)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2621() {
    let (_, result, _) =
        set_up_cumulative_state(&[((32, 41), 10, 8), ((33, 41), 9, 3), ((35, 41), 7, 6)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2440() {
    let (_, result, _) = set_up_cumulative_state(&[((29, 35), 8, 7), ((35, 35), 2, 8)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2639() {
    let (_, result, _) = set_up_cumulative_state(&[((44, 51), 8, 8), ((46, 51), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1118() {
    let (_, result, _) = set_up_cumulative_state(&[((43, 48), 7, 10), ((43, 48), 7, 9)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2651() {
    let (_, result, _) = set_up_cumulative_state(&[((31, 33), 3, 9), ((24, 33), 10, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1642() {
    let (_, result, _) =
        set_up_cumulative_state(&[((8, 11), 4, 10), ((2, 11), 10, 4), ((7, 11), 5, 9)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2692() {
    let (_, result, _) = set_up_cumulative_state(&[((24, 27), 4, 10), ((24, 27), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2563() {
    let (_, result, _) =
        set_up_cumulative_state(&[((48, 54), 7, 6), ((49, 54), 6, 1), ((50, 54), 5, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1200() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 30), 10, 10), ((26, 30), 5, 8)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1216() {
    let (_, result, _) = set_up_cumulative_state(&[((38, 46), 10, 10), ((41, 46), 7, 10)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1231() {
    let (_, result, _) = set_up_cumulative_state(&[((28, 37), 10, 10), ((31, 37), 7, 9)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1758() {
    let (_, result, _) = set_up_cumulative_state(
        &[((21, 29), 10, 7), ((26, 29), 5, 9), ((21, 29), 10, 4)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1813() {
    let (_, result, _) = set_up_cumulative_state(&[((32, 39), 10, 10), ((35, 39), 7, 10)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3046() {
    let (_, result, _) = set_up_cumulative_state(&[((39, 46), 8, 8), ((41, 46), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3138() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 23), 4, 10), ((21, 23), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2810() {
    let (_, result, _) = set_up_cumulative_state(&[((41, 45), 5, 7), ((38, 45), 8, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2815() {
    let (_, result, _) = set_up_cumulative_state(&[((41, 45), 5, 7), ((38, 45), 8, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3220() {
    let (_, result, _) = set_up_cumulative_state(&[((31, 40), 10, 8), ((39, 40), 2, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1948() {
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((12, 19), 9, 5),
            ((11, 19), 10, 4),
            ((11, 19), 10, 7),
            ((11, 19), 10, 10),
        ],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3256() {
    let (_, result, _) =
        set_up_cumulative_state(&[((32, 40), 10, 8), ((35, 40), 7, 6), ((34, 40), 8, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3257() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 24), 4, 10), ((21, 24), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3265() {
    let (_, result, _) = set_up_cumulative_state(&[((25, 28), 4, 9), ((26, 28), 3, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3275() {
    let (_, result, _) = set_up_cumulative_state(&[((22, 25), 4, 10), ((22, 25), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2043() {
    let (_, result, _) =
        set_up_cumulative_state(&[((15, 23), 9, 9), ((14, 23), 10, 7), ((19, 23), 5, 5)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1485() {
    let (_, result, _) = set_up_cumulative_state(
        &[((12, 18), 9, 4), ((12, 18), 9, 6), ((11, 18), 10, 10)],
        17,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2986() {
    let (_, result, _) =
        set_up_cumulative_state(&[((39, 45), 7, 6), ((39, 45), 7, 1), ((38, 45), 8, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2087() {
    let (_, result, _) = set_up_cumulative_state(
        &[((34, 37), 5, 9), ((29, 37), 10, 4), ((29, 37), 10, 7)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3429() {
    let (_, result, _) = set_up_cumulative_state(&[((34, 41), 10, 8), ((36, 41), 8, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3430() {
    let (_, result, _) = set_up_cumulative_state(&[((29, 35), 7, 6), ((32, 35), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2126() {
    let (_, result, _) =
        set_up_cumulative_state(&[((45, 50), 7, 10), ((48, 50), 4, 7), ((47, 50), 5, 8)], 19);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3477() {
    let (_, result, _) = set_up_cumulative_state(&[((39, 46), 8, 8), ((41, 46), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3481() {
    let (_, result, _) = set_up_cumulative_state(&[((51, 53), 4, 9), ((49, 53), 6, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3490() {
    let (_, result, _) = set_up_cumulative_state(&[((41, 45), 6, 10), ((39, 45), 8, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3516() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 24), 4, 10), ((21, 24), 4, 9)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1607() {
    let (_, result, _) =
        set_up_cumulative_state(&[((39, 46), 8, 3), ((40, 46), 7, 10), ((40, 46), 7, 9)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1610() {
    let (_, result, _) =
        set_up_cumulative_state(&[((25, 33), 9, 6), ((25, 33), 9, 4), ((27, 33), 7, 10)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3544() {
    let (_, result, _) = set_up_cumulative_state(&[((0, 4), 8, 8), ((0, 4), 8, 7)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2186() {
    let (_, result, _) = set_up_cumulative_state(
        &[
            ((17, 25), 10, 7),
            ((23, 25), 4, 6),
            ((22, 25), 5, 5),
            ((23, 25), 4, 7),
        ],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2187() {
    let (_, result, _) = set_up_cumulative_state(
        &[((21, 28), 10, 7), ((21, 28), 10, 10), ((21, 28), 10, 4)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3192() {
    let (_, result, _) =
        set_up_cumulative_state(&[((39, 45), 7, 6), ((39, 45), 7, 1), ((38, 45), 8, 7)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3576() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 22), 2, 8), ((19, 22), 4, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3203() {
    let (_, result, _) =
        set_up_cumulative_state(&[((21, 22), 4, 7), ((20, 22), 5, 6), ((16, 22), 9, 4)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2247() {
    let (_, result, _) = set_up_cumulative_state(
        &[((31, 39), 10, 10), ((33, 39), 8, 3), ((34, 39), 7, 10)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1697() {
    let (_, result, _) =
        set_up_cumulative_state(&[((41, 47), 7, 10), ((44, 47), 4, 4), ((46, 47), 2, 7)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3280() {
    let (_, result, _) =
        set_up_cumulative_state(&[((10, 19), 10, 8), ((11, 19), 9, 4), ((18, 19), 2, 8)], 13);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3725() {
    let (_, result, _) = set_up_cumulative_state(&[((31, 38), 10, 8), ((33, 38), 8, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3726() {
    let (_, result, _) = set_up_cumulative_state(&[((23, 32), 10, 8), ((30, 32), 3, 10)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_3747() {
    let (_, result, _) = set_up_cumulative_state(&[((21, 22), 4, 9), ((17, 22), 8, 8)], 14);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1735() {
    let (_, result, _) = set_up_cumulative_state(&[((43, 46), 7, 10), ((43, 46), 7, 9)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1754() {
    let (_, result, _) = set_up_cumulative_state(
        &[((32, 41), 10, 10), ((32, 41), 10, 1), ((35, 41), 7, 10)],
        17,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2329() {
    let (_, result, _) = set_up_cumulative_state(
        &[((30, 38), 10, 7), ((35, 38), 5, 5), ((30, 38), 10, 10)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_1794() {
    let (_, result, _) =
        set_up_cumulative_state(&[((39, 46), 9, 6), ((40, 46), 8, 3), ((41, 46), 7, 9)], 17);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn cumulative_time_table_conflict_2352() {
    let (_, result, _) = set_up_cumulative_state(
        &[((30, 38), 9, 5), ((29, 38), 10, 7), ((29, 38), 10, 10)],
        19,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}
