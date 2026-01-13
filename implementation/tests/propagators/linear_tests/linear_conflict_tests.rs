#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]

use crate::propagators::linear_tests::set_up_linear_leq_state;
#[test]
fn linear_leq_conflict_190449() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 7, 0), ((0, 1), 7, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_100085() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 5, 0), ((0, 1), 7, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_154283() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 5, 0)], 12, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_3841() {
    let (_, result, _) = set_up_linear_leq_state(&[((46, 46), -1, 0), ((47, 47), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_198399() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 4, 0)], 4, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_52309() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 4, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_38330() {
    let (_, result, _) = set_up_linear_leq_state(&[((41, 42), -1, 0), ((42, 43), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_13146() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 5, 0), ((1, 1), 9, 0)], 9, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_91499() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 4, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_104532() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 4, 0)], 5, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_279476() {
    let (_, result, _) = set_up_linear_leq_state(&[((43, 43), -1, 0), ((46, 52), 1, 0)], -3, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_258873() {
    let (_, result, _) = set_up_linear_leq_state(&[((37, 37), -1, 0), ((38, 38), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_430237() {
    let (_, result, _) = set_up_linear_leq_state(&[((37, 38), -1, 0), ((38, 43), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_149905() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 2, 0), ((1, 1), 1, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_361443() {
    let (_, result, _) = set_up_linear_leq_state(&[((34, 34), -1, 0), ((35, 42), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_453757() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_441951() {
    let (_, result, _) = set_up_linear_leq_state(&[((51, 53), -1, 0), ((48, 49), 1, 0)], -7, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_230764() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 5, 0), ((1, 1), 5, 0)], 9, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_192039() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 10, 0), ((1, 1), 5, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_209548() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 5, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_317973() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 8, 0)], 15, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_461072() {
    let (_, result, _) = set_up_linear_leq_state(&[((36, 36), -1, 0), ((37, 37), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_13432() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 4, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_353061() {
    let (_, result, _) = set_up_linear_leq_state(&[((45, 45), -1, 0), ((46, 46), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_314195() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_418827() {
    let (_, result, _) = set_up_linear_leq_state(&[((36, 36), -1, 0), ((37, 37), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_425474() {
    let (_, result, _) = set_up_linear_leq_state(&[((38, 42), -1, 0), ((36, 36), 1, 0)], -10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_298325() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_433301() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_181431() {
    let (_, result, _) = set_up_linear_leq_state(&[((44, 50), -1, 0), ((43, 43), 1, 0)], -10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_271174() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_402104() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_128947() {
    let (_, result, _) = set_up_linear_leq_state(&[((47, 48), -1, 0), ((39, 46), 1, 0)], -10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_33836() {
    let (_, result, _) = set_up_linear_leq_state(&[((40, 41), -1, 0), ((41, 42), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_62139() {
    let (_, result, _) = set_up_linear_leq_state(
        &[
            ((1, 1), 10, 0),
            ((1, 1), 9, 0),
            ((0, 1), 5, 0),
            ((0, 1), 9, 0),
        ],
        15,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_189689() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 10, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_32888() {
    let (_, result, _) = set_up_linear_leq_state(&[((38, 38), -1, 0), ((33, 33), 1, 0)], -10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_346027() {
    let (_, result, _) = set_up_linear_leq_state(&[((37, 37), -1, 0), ((38, 38), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_60764() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_449277() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 9, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_162573() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_421083() {
    let (_, result, _) = set_up_linear_leq_state(&[((46, 46), -1, 0), ((47, 53), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_432991() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_308478() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_359394() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_374650() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_31830() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 10, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_22585() {
    let (_, result, _) = set_up_linear_leq_state(&[((47, 47), -1, 0), ((47, 47), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_382215() {
    let (_, result, _) = set_up_linear_leq_state(&[((53, 54), -1, 0), ((54, 55), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_203008() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 9, 0)], 9, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_338333() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_144964() {
    let (_, result, _) = set_up_linear_leq_state(&[((38, 39), -1, 0), ((37, 37), 1, 0)], -5, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_286856() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_387327() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_36227() {
    let (_, result, _) = set_up_linear_leq_state(&[((39, 44), -1, 0), ((38, 38), 1, 0)], -10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_189695() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_142642() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_479807() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_261758() {
    let (_, result, _) = set_up_linear_leq_state(&[((37, 37), -1, 0), ((37, 37), 1, 0)], -1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_145025() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_382558() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_160032() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 5, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_114713() {
    let (_, result, _) = set_up_linear_leq_state(&[((32, 33), -1, 0), ((31, 31), 1, 0)], -5, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_252397() {
    let (_, result, _) = set_up_linear_leq_state(&[((33, 34), -1, 0), ((32, 32), 1, 0)], -10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_370748() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 6, 0), ((1, 1), 10, 0)], 11, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_492685() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 4, 0), ((0, 1), 5, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_341098() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_181628() {
    let (_, result, _) = set_up_linear_leq_state(&[((53, 53), -1, 0), ((49, 53), 1, 0)], -5, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_435667() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 9, 0)], 9, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_137423() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_202242() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_457889() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 3, 0), ((1, 1), 10, 0)], 10, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_18541() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 16, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_148727() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 8, 0)], 8, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_332986() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), 10, 0), ((1, 1), 9, 0)], 14, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_111321() {
    let (_, result, _) = set_up_linear_leq_state(&[((94, 94), 1, 0), ((84, 84), -1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_2142() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -15, 0), ((61, 61), 1, 0), ((44, 44), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_128151() {
    let (_, result, _) = set_up_linear_leq_state(&[((70, 70), 1, 0), ((60, 60), -1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_417830() {
    let (_, result, _) = set_up_linear_leq_state(&[((97, 97), 1, 0), ((82, 82), -1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_87483() {
    let (_, result, _) = set_up_linear_leq_state(&[((67, 67), 1, 0), ((61, 61), -1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_348467() {
    let (_, result, _) = set_up_linear_leq_state(&[((68, 68), 1, 0), ((61, 61), -1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_416502() {
    let (_, result, _) = set_up_linear_leq_state(&[((71, 71), 1, 0), ((65, 65), -1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_109296() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -32, 0), ((65, 65), 1, 0), ((30, 30), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_79771() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -70, 0), ((80, 80), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_414013() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -63, 0), ((80, 80), 1, 0), ((12, 12), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_218800() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -37, 0), ((65, 65), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_114069() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -26, 0), ((96, 96), 1, 0), ((1, 1), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_104763() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -33, 0), ((96, 96), 1, 0), ((1, 1), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_26757() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -22, 0), ((96, 96), 1, 0), ((44, 44), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_63928() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -40, 0), ((96, 96), 1, 0), ((30, 30), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_307963() {
    let (_, result, _) =
        set_up_linear_leq_state(&[((0, 1), 1, 0), ((1, 1), 1, 0), ((1, 1), 1, 0)], 1, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_30488() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -49, 0), ((80, 80), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_292021() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -78, 0), ((96, 96), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_71653() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -59, 0), ((96, 96), 1, 0), ((7, 7), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_167091() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -66, 0), ((96, 96), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_234869() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -44, 0), ((96, 96), 1, 0), ((3, 3), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_77505() {
    let (_, result, _) = set_up_linear_leq_state(&[((1, 1), -52, 0), ((80, 80), 1, 0)], 0, true);
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_342639() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -59, 0), ((96, 96), 1, 0), ((20, 20), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_112055() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -40, 0), ((96, 96), 1, 0), ((20, 20), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}

#[test]
fn linear_leq_conflict_285511() {
    let (_, result, _) = set_up_linear_leq_state(
        &[((1, 1), -49, 0), ((96, 96), 1, 0), ((20, 20), -1, 0)],
        0,
        true,
    );
    assert!(
        result.is_err(),
        "Expected an error to occur but was {result:?}"
    )
}
