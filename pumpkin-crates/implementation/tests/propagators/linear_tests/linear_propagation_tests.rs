#![cfg(test)]
#![allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]

use crate::propagators::linear_tests::set_up_linear_leq_state;

#[test]
fn linear_leq_propagation_491680() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_158220() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((41, 55), -1, 0), ((54, 56), 1, 0), ((0, 1), -169, 0)],
        -4,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -3)
}

#[test]
fn linear_leq_propagation_455807() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_154517() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((35, 48), -1, 0), ((47, 59), 1, 0), ((0, 1), -167, 0)],
        -2,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_410892() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((66, 66), -1, 0), ((66, 71), 1, 0), ((0, 1), -168, 0)],
        -3,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -3)
}

#[test]
fn linear_leq_propagation_185996() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_284170() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((43, 43), 1, 0), ((37, 42), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 164)
}

#[test]
fn linear_leq_propagation_369170() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((37, 56), -1, 0), ((59, 60), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_26754() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_233454() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((46, 55), -1, 0), ((51, 61), 1, 0), ((0, 1), -170, 0)],
        -5,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_58595() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_336621() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((41, 43), -1, 0), ((45, 58), 1, 0), ((0, 1), -167, 0)],
        -2,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_76222() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((31, 31), -1, 0), ((42, 46), 1, 0), ((0, 1), -172, 0)],
        -7,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -18)
}

#[test]
fn linear_leq_propagation_5007() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_384745() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((40, 55), 1, 0), ((23, 23), -1, 0), ((0, 1), 157, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 148)
}

#[test]
fn linear_leq_propagation_136255() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((63, 69), 1, 0), ((54, 61), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 163)
}

#[test]
fn linear_leq_propagation_376309() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((39, 46), -1, 0), ((43, 61), 1, 0), ((0, 1), -170, 0)],
        -5,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -2)
}

#[test]
fn linear_leq_propagation_63538() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((46, 58), -1, 0), ((33, 56), 1, 0)], -3, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 55)
}

#[test]
fn linear_leq_propagation_251555() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((36, 45), -1, 0), ((48, 58), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_100852() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((42, 56), 1, 0), ((44, 59), -1, 0)], -3, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -45)
}

#[test]
fn linear_leq_propagation_137871() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_289040() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((37, 37), -1, 0), ((47, 57), 1, 0), ((0, 1), -173, 0)],
        -8,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -18)
}

#[test]
fn linear_leq_propagation_62296() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((34, 34), -1, 0), ((46, 50), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -13)
}

#[test]
fn linear_leq_propagation_53261() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_330588() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((48, 58), 1, 0), ((38, 38), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 155)
}

#[test]
fn linear_leq_propagation_154961() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((60, 65), 1, 0), ((53, 58), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 163)
}

#[test]
fn linear_leq_propagation_43389() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((34, 34), -1, 0), ((43, 50), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -10)
}

#[test]
fn linear_leq_propagation_389675() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((32, 57), -1, 0), ((56, 65), 1, 0), ((0, 1), -167, 0)],
        -2,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_23155() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_122331() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((28, 36), -1, 0), ((36, 58), 1, 0), ((0, 1), -169, 0)],
        -4,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_486658() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((24, 36), -1, 0), ((34, 56), 1, 0), ((0, 1), -168, 0)],
        -3,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_196690() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_125967() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((32, 43), 1, 0), ((40, 53), -1, 0)], -10, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -42)
}

#[test]
fn linear_leq_propagation_24032() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_233281() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((35, 43), 1, 0), ((43, 53), -1, 0)], -10, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -45)
}

#[test]
fn linear_leq_propagation_29916() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_269164() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((35, 40), -1, 0), ((38, 56), 1, 0), ((0, 1), -168, 0)],
        -3,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_206369() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((24, 24), -1, 0), ((32, 65), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -9)
}

#[test]
fn linear_leq_propagation_381424() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((31, 45), -1, 0), ((45, 61), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_210145() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_269457() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_259396() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((45, 59), 1, 0), ((46, 61), -1, 0)], -2, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -47)
}

#[test]
fn linear_leq_propagation_480633() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((53, 53), -1, 0), ((53, 67), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_146557() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((38, 38), -1, 0), ((53, 58), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -16)
}

#[test]
fn linear_leq_propagation_222536() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((33, 33), -1, 0), ((36, 43), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_442235() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((51, 52), 1, 0), ((50, 55), -1, 0)], -3, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -54)
}

#[test]
fn linear_leq_propagation_42333() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((51, 62), -1, 0), ((58, 60), 1, 0), ((0, 1), -171, 0)],
        -6,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -2)
}

#[test]
fn linear_leq_propagation_374466() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((48, 55), -1, 0), ((59, 68), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -5)
}

#[test]
fn linear_leq_propagation_432431() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_32295() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((34, 44), -1, 0), ((44, 46), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_447801() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_389980() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((27, 27), -1, 0), ((31, 41), 1, 0), ((0, 1), -175, 0)],
        -10,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -14)
}

#[test]
fn linear_leq_propagation_423187() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_94369() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((41, 54), -1, 0), ((55, 55), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -2)
}

#[test]
fn linear_leq_propagation_354297() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_194257() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_151977() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_254921() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((1, 1), 1, 0), ((0, 1), -1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_419110() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((23, 29), -1, 0), ((32, 64), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_383921() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((37, 58), 1, 0), ((28, 36), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 164)
}

#[test]
fn linear_leq_propagation_391408() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((29, 29), -1, 0), ((31, 58), 1, 0), ((0, 1), -167, 0)],
        -2,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -4)
}

#[test]
fn linear_leq_propagation_167920() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_145063() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((17, 39), 1, 0), ((26, 49), -1, 0)], -10, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -27)
}

#[test]
fn linear_leq_propagation_484669() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((36, 52), -1, 0), ((47, 53), 1, 0), ((0, 1), -171, 0)],
        -6,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -1)
}

#[test]
fn linear_leq_propagation_341968() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_37291() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((42, 45), -1, 0), ((50, 58), 1, 0), ((0, 1), -166, 0)],
        -1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -6)
}

#[test]
fn linear_leq_propagation_179371() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 3, 0),
            ((0, 1), 5, 0),
            ((0, 1), 7, 0),
            ((1, 1), 9, 0),
            ((0, 1), 4, 0),
            ((0, 1), 8, 0),
            ((0, 1), 4, 0),
            ((0, 1), 7, 0),
            ((0, 1), 3, 0),
        ],
        9,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_243210() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((15, 15), -1, 0), ((24, 51), 1, 0), ((0, 1), -169, 0)],
        -4,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -13)
}

#[test]
fn linear_leq_propagation_200558() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((38, 46), 1, 0), ((28, 28), -1, 0), ((0, 1), 164, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 155)
}

#[test]
fn linear_leq_propagation_245516() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((29, 42), 1, 0), ((28, 28), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 164)
}

#[test]
fn linear_leq_propagation_430331() {
    let (solver, _, variables) = set_up_linear_leq_state(&[((0, 1), 1, 0)], 0, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_293784() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_6091() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((28, 33), 1, 0), ((17, 17), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 154)
}

#[test]
fn linear_leq_propagation_93135() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((33, 54), 1, 0), ((14, 14), -1, 0), ((0, 1), 165, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 146)
}

#[test]
fn linear_leq_propagation_3963() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((30, 33), 1, 0), ((14, 14), -1, 0), ((0, 1), 161, 0)],
        165,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 149)
}

#[test]
fn linear_leq_propagation_33904() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
        ],
        1,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_275772() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
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
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_236360() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
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
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_272801() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
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
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_304649() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
            ((0, 1), 1, 0),
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
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_94915() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[((85, 121), 1, 0), ((30, 30), -1, 0), ((54, 91), -1, 0)],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= -55)
}

#[test]
fn linear_leq_propagation_341119() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 16, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((0, 1), 117, 0),
            ((0, 1), 118, 0),
            ((86, 86), -1, 0),
            ((3, 3), 1, 0),
            ((0, 1), 102, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 83)
}

#[test]
fn linear_leq_propagation_488326() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((66, 66), -1, 0),
            ((30, 30), 1, 0),
            ((0, 1), 83, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 36)
}

#[test]
fn linear_leq_propagation_334843() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 28, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((0, 1), 117, 0),
            ((0, 1), 118, 0),
            ((0, 1), 119, 0),
            ((0, 1), 120, 0),
            ((0, 1), 121, 0),
            ((79, 79), -1, 0),
            ((0, 1), 102, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 79)
}

#[test]
fn linear_leq_propagation_456517() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 91, 0),
            ((86, 86), -1, 0),
            ((30, 30), 1, 0),
            ((0, 1), 90, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 56)
}

#[test]
fn linear_leq_propagation_22670() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 92, 0),
            ((0, 1), 93, 0),
            ((0, 1), 94, 0),
            ((0, 1), 95, 0),
            ((0, 1), 96, 0),
            ((0, 1), 97, 0),
            ((0, 1), 98, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((95, 95), -1, 0),
            ((20, 20), 1, 0),
            ((0, 1), 91, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 75)
}

#[test]
fn linear_leq_propagation_483104() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 21, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 97, 0),
            ((0, 1), 98, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((0, 1), 102, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((0, 1), 117, 0),
            ((0, 1), 118, 0),
            ((0, 1), 119, 0),
            ((0, 1), 120, 0),
            ((0, 1), 121, 0),
            ((86, 86), -1, 0),
            ((0, 1), 96, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 86)
}

#[test]
fn linear_leq_propagation_136278() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 21, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((0, 1), 90, 0),
            ((0, 1), 91, 0),
            ((102, 102), -1, 0),
            ((30, 30), 1, 0),
            ((0, 1), 74, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 72)
}

#[test]
fn linear_leq_propagation_444242() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 21, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((107, 107), -1, 0),
            ((60, 60), 1, 0),
            ((0, 1), 52, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 47)
}

#[test]
fn linear_leq_propagation_31368() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 17, 0),
            ((0, 1), 18, 0),
            ((0, 1), 19, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((65, 65), -1, 0),
            ((30, 30), 1, 0),
            ((0, 1), 47, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 35)
}

#[test]
fn linear_leq_propagation_46582() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 17, 0),
            ((0, 1), 18, 0),
            ((0, 1), 19, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((0, 1), 90, 0),
            ((0, 1), 91, 0),
            ((0, 1), 92, 0),
            ((0, 1), 93, 0),
            ((0, 1), 94, 0),
            ((0, 1), 95, 0),
            ((0, 1), 96, 0),
            ((0, 1), 97, 0),
            ((0, 1), 98, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((0, 1), 102, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((0, 1), 117, 0),
            ((0, 1), 118, 0),
            ((0, 1), 119, 0),
            ((74, 74), -1, 0),
            ((0, 1), 75, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 74)
}

#[test]
fn linear_leq_propagation_262729() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 18, 0),
            ((0, 1), 19, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((0, 1), 90, 0),
            ((0, 1), 91, 0),
            ((0, 1), 92, 0),
            ((0, 1), 93, 0),
            ((0, 1), 94, 0),
            ((0, 1), 95, 0),
            ((0, 1), 96, 0),
            ((0, 1), 97, 0),
            ((0, 1), 98, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((0, 1), 102, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((76, 76), -1, 0),
            ((7, 7), 1, 0),
            ((0, 1), 77, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 69)
}

#[test]
fn linear_leq_propagation_24597() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 19, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((0, 1), 117, 0),
            ((0, 1), 118, 0),
            ((0, 1), 119, 0),
            ((76, 76), -1, 0),
            ((0, 1), 111, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 76)
}

#[test]
fn linear_leq_propagation_334294() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((0, 1), 90, 0),
            ((0, 1), 91, 0),
            ((0, 1), 92, 0),
            ((0, 1), 93, 0),
            ((0, 1), 94, 0),
            ((0, 1), 95, 0),
            ((0, 1), 96, 0),
            ((0, 1), 97, 0),
            ((0, 1), 98, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((0, 1), 102, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((0, 1), 117, 0),
            ((64, 64), -1, 0),
            ((1, 1), 1, 0),
            ((0, 1), 79, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 63)
}

#[test]
fn linear_leq_propagation_70361() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((0, 1), 90, 0),
            ((0, 1), 91, 0),
            ((0, 1), 92, 0),
            ((0, 1), 93, 0),
            ((0, 1), 94, 0),
            ((0, 1), 95, 0),
            ((0, 1), 96, 0),
            ((0, 1), 97, 0),
            ((0, 1), 98, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((0, 1), 102, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((0, 1), 105, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((0, 1), 115, 0),
            ((0, 1), 116, 0),
            ((66, 66), -1, 0),
            ((3, 3), 1, 0),
            ((0, 1), 83, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 63)
}

#[test]
fn linear_leq_propagation_427279() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((69, 69), -1, 0),
            ((30, 30), 1, 0),
            ((0, 1), 52, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 39)
}

#[test]
fn linear_leq_propagation_392725() {
    let (solver, _, variables) =
        set_up_linear_leq_state(&[((1, 1), 1, 0), ((0, 1), 1, 0), ((0, 1), 1, 0)], 1, false);
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}

#[test]
fn linear_leq_propagation_40310() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 14, 0),
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 17, 0),
            ((0, 1), 18, 0),
            ((0, 1), 19, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 106, 0),
            ((0, 1), 107, 0),
            ((0, 1), 108, 0),
            ((0, 1), 109, 0),
            ((0, 1), 110, 0),
            ((0, 1), 111, 0),
            ((0, 1), 112, 0),
            ((0, 1), 113, 0),
            ((0, 1), 114, 0),
            ((44, 44), -1, 0),
            ((3, 3), 1, 0),
            ((0, 1), 105, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 41)
}

#[test]
fn linear_leq_propagation_6338() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((0, 1), 8, 0),
            ((0, 1), 10, 0),
            ((0, 1), 11, 0),
            ((0, 1), 12, 0),
            ((0, 1), 13, 0),
            ((0, 1), 14, 0),
            ((0, 1), 15, 0),
            ((0, 1), 16, 0),
            ((0, 1), 17, 0),
            ((0, 1), 18, 0),
            ((0, 1), 19, 0),
            ((0, 1), 20, 0),
            ((0, 1), 21, 0),
            ((0, 1), 22, 0),
            ((0, 1), 23, 0),
            ((0, 1), 24, 0),
            ((0, 1), 25, 0),
            ((0, 1), 26, 0),
            ((0, 1), 27, 0),
            ((0, 1), 28, 0),
            ((0, 1), 29, 0),
            ((0, 1), 30, 0),
            ((0, 1), 31, 0),
            ((0, 1), 32, 0),
            ((0, 1), 33, 0),
            ((0, 1), 34, 0),
            ((0, 1), 35, 0),
            ((0, 1), 36, 0),
            ((0, 1), 37, 0),
            ((0, 1), 38, 0),
            ((0, 1), 39, 0),
            ((0, 1), 40, 0),
            ((0, 1), 41, 0),
            ((0, 1), 42, 0),
            ((0, 1), 43, 0),
            ((0, 1), 44, 0),
            ((0, 1), 45, 0),
            ((0, 1), 46, 0),
            ((0, 1), 47, 0),
            ((0, 1), 48, 0),
            ((0, 1), 49, 0),
            ((0, 1), 50, 0),
            ((0, 1), 51, 0),
            ((0, 1), 52, 0),
            ((0, 1), 53, 0),
            ((0, 1), 54, 0),
            ((0, 1), 55, 0),
            ((0, 1), 56, 0),
            ((0, 1), 57, 0),
            ((0, 1), 58, 0),
            ((0, 1), 59, 0),
            ((0, 1), 60, 0),
            ((0, 1), 61, 0),
            ((0, 1), 62, 0),
            ((0, 1), 63, 0),
            ((0, 1), 64, 0),
            ((0, 1), 65, 0),
            ((0, 1), 66, 0),
            ((0, 1), 67, 0),
            ((0, 1), 68, 0),
            ((0, 1), 69, 0),
            ((0, 1), 70, 0),
            ((0, 1), 71, 0),
            ((0, 1), 72, 0),
            ((0, 1), 73, 0),
            ((0, 1), 74, 0),
            ((0, 1), 75, 0),
            ((0, 1), 76, 0),
            ((0, 1), 77, 0),
            ((0, 1), 78, 0),
            ((0, 1), 79, 0),
            ((0, 1), 80, 0),
            ((0, 1), 81, 0),
            ((0, 1), 82, 0),
            ((0, 1), 83, 0),
            ((0, 1), 84, 0),
            ((0, 1), 85, 0),
            ((0, 1), 86, 0),
            ((0, 1), 87, 0),
            ((0, 1), 88, 0),
            ((0, 1), 89, 0),
            ((0, 1), 90, 0),
            ((0, 1), 91, 0),
            ((0, 1), 92, 0),
            ((0, 1), 93, 0),
            ((0, 1), 94, 0),
            ((0, 1), 95, 0),
            ((0, 1), 96, 0),
            ((0, 1), 97, 0),
            ((0, 1), 99, 0),
            ((0, 1), 100, 0),
            ((0, 1), 101, 0),
            ((0, 1), 102, 0),
            ((0, 1), 103, 0),
            ((0, 1), 104, 0),
            ((25, 121), -1, 0),
            ((24, 119), 1, 0),
            ((0, 1), 98, 0),
        ],
        0,
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 97)
}

#[test]
fn linear_leq_propagation_487499() {
    let (solver, _, variables) = set_up_linear_leq_state(
        &[
            ((1, 1), 1, 0),
            ((0, 1), 1, 0),
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
        false,
    );
    assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
}
