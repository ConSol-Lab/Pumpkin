#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]

use crate::propagators::linear_tests::set_up_linear_leq_state;
// rcpsp_simplified.mzn + 01.dzn
#[test]
fn linear_leq_conflict_471385() {
    // Test case with 3 variables
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 5, 0), ((1, 1), 4, 0)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_418622() {
    // Test case with 5 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((0, 1), 1, 0),
            ((1, 1), 5, 0),
            ((1, 1), 4, 0),
            ((1, 1), 9, 0),
            ((0, 1), 3, 0),
        ],
        9,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_42296() {
    // Test case with 5 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((0, 1), 5, 0),
            ((1, 1), 5, 0),
            ((1, 1), 4, 0),
            ((1, 1), 9, 0),
            ((0, 1), 3, 0),
        ],
        9,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_199230() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 10, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_118152() {
    // Test case with 3 variables
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 2, 0), ((1, 1), 1, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_199997() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 7, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_41718() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 7, 0)], 13, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_388285() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 10, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_384533() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((34, 34), -1, 0), ((35, 35), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_48764() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_342818() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 9, 0)], 9, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_201487() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 9, 0), ((1, 1), 7, 0)], 13, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_52086() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 9, 0)], 12, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_374297() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_293039() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 7, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_347780() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 13, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_72322() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((24, 24), -1, 0), ((24, 24), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_121822() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((33, 35), 1, 0), ((37, 37), -1, 0)], -5, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_375850() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((26, 26), -1, 0), ((26, 26), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_26151() {
    // Test case with 3 variables
    let (_, result, _) =
        set_up_linear_leq_state(&[((0, 1), 1, 0), ((1, 1), 3, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// ghoulomb.mzn + 3-5-11.dzn
#[test]
fn linear_leq_conflict_156793() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -34, 0), ((80, 80), 1, 0), ((30, 30), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_173631() {
    // Test case with 15 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
        ],
        1,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_18387() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -38, 0), ((96, 96), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_211353() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -22, 0), ((96, 96), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_115695() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -37, 0), ((96, 96), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_413729() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -22, 0), ((96, 96), 1, 0), ((1, 1), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_179278() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -82, 0), ((96, 96), 1, 0), ((1, 1), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_493909() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -37, 0), ((96, 96), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_361286() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -82, 0), ((96, 96), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_381929() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -60, 0), ((96, 96), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_499248() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -92, 0), ((96, 96), 1, 0), ((1, 1), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_247575() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -59, 0), ((65, 65), 1, 0), ((1, 1), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_201811() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -43, 0), ((65, 65), 1, 0), ((20, 20), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_120782() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -46, 0), ((65, 65), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_190798() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -54, 0), ((65, 65), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_283282() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), -46, 0), ((65, 65), 1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_60373() {
    // Test case with 2 variables
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), -37, 0), ((65, 65), 1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_175340() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -26, 0), ((96, 96), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_373034() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -86, 0), ((96, 96), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_41812() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -54, 0), ((96, 96), 1, 0), ((20, 20), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// tsp.mzn + TSP_N20_2.dzn
#[test]
fn linear_leq_conflict_499642() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((16, 18), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_163268() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((4, 6), -1, 0), ((14, 20), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_186039() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((2, 4), -1, 0), ((15, 20), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_319165() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((5, 7), -1, 0), ((15, 19), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_375155() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((4, 6), -1, 0), ((16, 19), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_31526() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((16, 18), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_329350() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((15, 18), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_60708() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((15, 17), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_98707() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((4, 6), -1, 0), ((13, 15), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_344870() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((7, 9), -1, 0), ((16, 18), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_24180() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((7, 9), -1, 0), ((16, 18), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_457612() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((5, 7), -1, 0), ((14, 19), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_93437() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((15, 17), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_174546() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((5, 7), -1, 0), ((14, 19), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_261011() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((2, 4), -1, 0), ((15, 20), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_246346() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((2, 11), -1, 0), ((13, 20), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_311804() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((5, 7), -1, 0), ((14, 19), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_215807() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((15, 17), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_84610() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((3, 5), -1, 0), ((14, 16), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_100950() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), 19, 0), ((6, 8), -1, 0), ((15, 17), 1, 0)],
        18,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// Carpet cutting: cc_base.mzn + mzn_rnd_test.16.dzn
#[test]
fn linear_leq_conflict_279692() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1242, 1291), 1, 0),
            ((1242, 1291), -1, 0),
            ((1, 1), 2953, 0),
        ],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_441089() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((705, 788), 1, 0), ((705, 705), -1, 0), ((1, 1), 3016, 0)],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_285481() {
    // Test case with 18 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1652, 1763), -1, 0),
            ((1, 1), 138, 0),
            ((181, 181), 1, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((56, 56), 1, 0),
            ((0, 121), 1, 0),
            ((72, 72), 1, 0),
            ((510, 510), 1, 0),
            ((76, 76), 1, 0),
            ((113, 113), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_375461() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_448778() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_63797() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_253737() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_287040() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_95034() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_167013() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((901, 964), 1, 0), ((957, 957), -1, 0), ((1, 1), 2968, 0)],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_131018() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((901, 964), 1, 0), ((957, 957), -1, 0), ((1, 1), 2968, 0)],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_90733() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((901, 964), 1, 0), ((957, 957), -1, 0), ((1, 1), 2968, 0)],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_317865() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1142, 1206), 1, 0),
            ((1029, 1029), -1, 0),
            ((1, 1), 2968, 0),
        ],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_130532() {
    // Test case with 4 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((889, 1089), 1, 0),
            ((1, 1), 50, 0),
            ((957, 1028), -1, 0),
            ((1, 1), 3084, 0),
        ],
        2953,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_398824() {
    // Test case with 4 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((889, 1089), 1, 0),
            ((1, 1), 50, 0),
            ((957, 1028), -1, 0),
            ((1, 1), 3084, 0),
        ],
        2953,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_350082() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((566, 592), 1, 0), ((629, 629), -1, 0), ((1, 1), 3016, 0)],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_493970() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((566, 592), 1, 0), ((629, 629), -1, 0), ((1, 1), 3016, 0)],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_105737() {
    // Test case with 16 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1468, 1538), -1, 0),
            ((1, 1), 138, 0),
            ((181, 181), 1, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((0, 121), 1, 0),
            ((72, 72), 1, 0),
            ((510, 510), 1, 0),
            ((76, 76), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_247403() {
    // Test case with 15 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1468, 1538), -1, 0),
            ((1, 1), 138, 0),
            ((181, 181), 1, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((0, 121), 1, 0),
            ((72, 72), 1, 0),
            ((510, 510), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_361445() {
    // Test case with 15 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1468, 1538), -1, 0),
            ((1, 1), 138, 0),
            ((181, 181), 1, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((0, 121), 1, 0),
            ((72, 72), 1, 0),
            ((510, 510), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

// mknapsack_global.fzn + mknap1-6.dzn
#[test]
fn linear_leq_conflict_466539() {
    // Test case with 16 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1343, 1439), -1, 0),
            ((1, 1), 138, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((56, 56), 1, 0),
            ((0, 121), 1, 0),
            ((72, 72), 1, 0),
            ((510, 510), 1, 0),
            ((76, 76), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_131630() {
    // Test case with 16 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1343, 1439), -1, 0),
            ((1, 1), 138, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((56, 56), 1, 0),
            ((0, 121), 1, 0),
            ((72, 72), 1, 0),
            ((510, 510), 1, 0),
            ((76, 76), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_34033() {
    // Test case with 16 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1343, 1439), -1, 0),
            ((1, 1), 138, 0),
            ((181, 181), 1, 0),
            ((170, 170), 1, 0),
            ((74, 74), 1, 0),
            ((197, 197), 1, 0),
            ((72, 72), 1, 0),
            ((0, 121), 1, 0),
            ((112, 112), 1, 0),
            ((268, 268), 1, 0),
            ((113, 113), 1, 0),
            ((63, 63), 1, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
            ((0, 1), 50, 0),
        ],
        -75,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_320721() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1215, 1342), 1, 0),
            ((1271, 1271), -1, 0),
            ((1, 1), 2968, 0),
        ],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_180441() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_25739() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_104407() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((204, 228), 1, 0), ((204, 204), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_260976() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_40070() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_316112() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_27988() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_40870() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_26370() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_219712() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_197472() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_290056() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_412757() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((26, 26), 1, 0), ((0, 81), -1, 0), ((1, 1), 457, 0)],
        315,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_298881() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[((100, 228), 1, 0), ((168, 168), -1, 0), ((1, 1), 315, 0)],
        228,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_439766() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1236, 1236), 1, 0),
            ((1236, 1236), -1, 0),
            ((1, 1), 2953, 0),
        ],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_394788() {
    // Test case with 3 variables
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1242, 1291), 1, 0),
            ((1242, 1291), -1, 0),
            ((1, 1), 2953, 0),
        ],
        2903,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}
