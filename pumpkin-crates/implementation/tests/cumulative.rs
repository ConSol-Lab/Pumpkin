#[allow(
    deprecated,
    reason = "Will be refactored in the future using the state API"
)]
#[cfg(test)]
mod tests {
    use implementation::propagators::cumulative::CumulativeConstructor;
    use pumpkin_core::TestSolver;
    use pumpkin_core::state::Conflict;
    use pumpkin_core::state::PropagatorId;
    use pumpkin_core::variables::DomainId;
    fn set_up_cumulative_state(
        task_info: &[((i32, i32), u32, u32)],
        capacity: u32,
    ) -> (TestSolver, Result<PropagatorId, Conflict>, Vec<DomainId>) {
        let mut solver = TestSolver::default();

        let mut start_times = Vec::default();
        let mut durations = Vec::default();
        let mut resource_usages = Vec::default();

        for ((lb, ub), duration, resource_usage) in task_info {
            start_times.push(solver.new_variable(*lb, *ub));
            durations.push(*duration);
            resource_usages.push(*resource_usage);
        }
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(CumulativeConstructor {
            start_times: start_times.clone().into(),
            durations: durations.into(),
            resource_usages: resource_usages.into(),
            capacity,
            constraint_tag,
        });
        (solver, result, start_times)
    }

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

    #[test]
    fn cumulative_time_table_lower_bound_0() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((49, 51), 7, 10), ((49, 163), 3, 10)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 56)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((141, 146), 7, 6), ((141, 151), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 148)
    }

    #[test]
    fn cumulative_time_table_lower_bound_2() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((0, 6), 8, 6), ((-3, 17), 10, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 8)
    }

    #[test]
    fn cumulative_time_table_lower_bound_12() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((18, 22), 8, 8), ((21, 51), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 26)
    }

    #[test]
    fn cumulative_time_table_lower_bound_13() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 8), 10, 8), ((7, 146), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 18)
    }

    #[test]
    fn cumulative_time_table_lower_bound_34() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((81, 82), 3, 9), ((76, 132), 7, 6)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 84)
    }

    #[test]
    fn cumulative_time_table_lower_bound_18() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((18, 25), 9, 6), ((17, 25), 10, 10), ((18, 99), 8, 3)],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 27)
    }

    #[test]
    fn cumulative_time_table_lower_bound_40() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((55, 59), 5, 7), ((57, 119), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 60)
    }

    #[test]
    fn cumulative_time_table_lower_bound_26() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((25, 32), 10, 10), ((25, 32), 10, 7), ((28, 119), 5, 5)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 35)
    }

    #[test]
    fn cumulative_time_table_lower_bound_31() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((25, 32), 10, 10), ((25, 32), 10, 7), ((25, 121), 8, 3)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 35)
    }

    #[test]
    fn cumulative_time_table_lower_bound_86() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((53, 56), 4, 9), ((55, 60), 2, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 57)
    }

    #[test]
    fn cumulative_time_table_lower_bound_87() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((60, 60), 2, 9), ((59, 127), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 62)
    }

    #[test]
    fn cumulative_time_table_lower_bound_90() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 36), 10, 8), ((29, 127), 8, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 45)
    }

    #[test]
    fn cumulative_time_table_lower_bound_91() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 36), 10, 8), ((35, 129), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 45)
    }

    #[test]
    fn cumulative_time_table_lower_bound_113() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 36), 10, 8), ((34, 119), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 45)
    }

    #[test]
    fn cumulative_time_table_lower_bound_124() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((23, 23), 4, 10), ((20, 126), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 27)
    }

    #[test]
    fn cumulative_time_table_lower_bound_129() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 36), 10, 8), ((35, 126), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 45)
    }

    #[test]
    fn cumulative_time_table_lower_bound_29() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((12, 19), 9, 6), ((12, 19), 9, 4), ((18, 133), 2, 10)],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 21)
    }

    #[test]
    fn cumulative_time_table_lower_bound_147() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((46, 52), 8, 8), ((50, 103), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 54)
    }

    #[test]
    fn cumulative_time_table_lower_bound_151() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((45, 45), 4, 9), ((38, 122), 8, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 49)
    }

    #[test]
    fn cumulative_time_table_lower_bound_103() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((0, 8), 10, 8), ((6, 95), 3, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 10)
    }

    #[test]
    fn cumulative_time_table_lower_bound_64() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((23, 27), 10, 10), ((25, 114), 3, 10)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 33)
    }

    #[test]
    fn cumulative_time_table_lower_bound_73() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((102, 108), 8, 6), ((108, 108), 2, 4), ((106, 112), 3, 10)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 110)
    }

    #[test]
    fn cumulative_time_table_lower_bound_222() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((0, 3), 8, 7), ((-4, 93), 8, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 8)
    }

    #[test]
    fn cumulative_time_table_lower_bound_163() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 8), 10, 8), ((7, 24), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 18)
    }

    #[test]
    fn cumulative_time_table_lower_bound_228() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((71, 74), 7, 6), ((72, 107), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 78)
    }

    #[test]
    fn cumulative_time_table_lower_bound_195() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 8), 10, 8), ((7, 19), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 18)
    }

    #[test]
    fn cumulative_time_table_lower_bound_245() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 36), 10, 8), ((34, 104), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 45)
    }

    #[test]
    fn cumulative_time_table_lower_bound_248() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((82, 86), 7, 6), ((84, 103), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 89)
    }

    #[test]
    fn cumulative_time_table_lower_bound_206() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((70, 71), 8, 7), ((67, 86), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 78)
    }

    #[test]
    fn cumulative_time_table_lower_bound_211() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((22, 23), 4, 7), ((22, 90), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 26)
    }

    #[test]
    fn cumulative_time_table_lower_bound_255() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((33, 36), 10, 8), ((34, 84), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 43)
    }

    #[test]
    fn cumulative_time_table_lower_bound_94() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((25, 32), 10, 10), ((25, 32), 10, 7), ((23, 36), 10, 4)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 35)
    }

    #[test]
    fn cumulative_time_table_lower_bound_260() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((53, 57), 8, 7), ((53, 68), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 61)
    }

    #[test]
    fn cumulative_time_table_lower_bound_292() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((53, 57), 8, 8), ((52, 74), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 61)
    }

    #[test]
    fn cumulative_time_table_lower_bound_271() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((78, 78), 2, 9), ((74, 86), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 80)
    }

    #[test]
    fn cumulative_time_table_lower_bound_273() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((0, 7), 8, 6), ((-2, 52), 10, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 8)
    }

    #[test]
    fn cumulative_time_table_lower_bound_558() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((20, 22), 5, 5), ((21, 22), 4, 7), ((18, 43), 5, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 25)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1022() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((36, 37), 10, 8), ((35, 66), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 46)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1025() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((22, 24), 8, 8), ((23, 53), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 30)
    }

    #[test]
    fn cumulative_time_table_lower_bound_570() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((47, 52), 7, 10), ((49, 52), 5, 8), ((50, 66), 3, 10)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 54)
    }

    #[test]
    fn cumulative_time_table_lower_bound_413() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((25, 34), 10, 10), ((33, 62), 2, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 35)
    }

    #[test]
    fn cumulative_time_table_lower_bound_573() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((24, 33), 10, 7), ((30, 33), 4, 7), ((24, 38), 10, 10)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 34)
    }

    #[test]
    fn cumulative_time_table_lower_bound_417() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((24, 31), 8, 3), ((22, 31), 10, 10), ((23, 39), 9, 6)],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 32)
    }

    #[test]
    fn cumulative_time_table_lower_bound_587() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((12, 19), 9, 5), ((12, 19), 9, 9), ((10, 34), 10, 7)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 21)
    }

    #[test]
    fn cumulative_time_table_lower_bound_429() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((21, 23), 10, 10), ((19, 36), 5, 8)], 17); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 31)
    }

    #[test]
    fn cumulative_time_table_lower_bound_596() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((19, 27), 10, 7), ((25, 27), 4, 7), ((18, 38), 10, 10)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 29)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1068() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((21, 23), 4, 10), ((20, 27), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 25)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1159() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((54, 55), 5, 7), ((53, 55), 6, 1), ((54, 60), 2, 9)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 59)
    }

    #[test]
    fn cumulative_time_table_lower_bound_605() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((2, 10), 10, 4), ((8, 10), 4, 10), ((6, 38), 5, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 12)
    }

    #[test]
    fn cumulative_time_table_lower_bound_606() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((7, 15), 10, 4), ((8, 15), 9, 9), ((11, 38), 5, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 17)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1170() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 15), 10, 8), ((8, 50), 8, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 18)
    }

    #[test]
    fn cumulative_time_table_lower_bound_618() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((46, 49), 7, 10), ((47, 64), 3, 10)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 53)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1093() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((25, 28), 4, 9), ((27, 51), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 29)
    }

    #[test]
    fn cumulative_time_table_lower_bound_626() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((12, 20), 9, 5), ((17, 20), 4, 6), ((12, 29), 9, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 21)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1121() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((30, 38), 10, 8), ((37, 49), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 40)
    }

    #[test]
    fn cumulative_time_table_lower_bound_478() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[
                ((26, 27), 3, 6),
                ((26, 27), 3, 1),
                ((19, 27), 10, 10),
                ((26, 51), 2, 3),
            ],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 29)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1149() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((39, 46), 8, 8), ((41, 53), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 47)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1182() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((41, 46), 8, 8), ((44, 64), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 49)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1183() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((43, 50), 8, 8), ((45, 53), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 51)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1190() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((21, 21), 4, 10), ((19, 26), 3, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 25)
    }

    #[test]
    fn cumulative_time_table_lower_bound_497() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((25, 27), 4, 4), ((19, 27), 10, 10), ((25, 30), 3, 6)],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 29)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1259() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 30), 3, 7), ((28, 48), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 32)
    }

    #[test]
    fn cumulative_time_table_lower_bound_503() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[
                ((21, 30), 10, 10),
                ((28, 30), 3, 6),
                ((28, 30), 3, 1),
                ((21, 41), 10, 1),
            ],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 31)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1277() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 30), 3, 7), ((28, 48), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 32)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1237() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 37), 10, 8), ((32, 37), 7, 6), ((34, 41), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 39)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1248() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((42, 45), 8, 8), ((43, 64), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 50)
    }

    #[test]
    fn cumulative_time_table_lower_bound_522() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((48, 49), 6, 8), ((48, 60), 2, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 54)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1342() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((26, 28), 3, 7), ((20, 28), 9, 4), ((24, 50), 5, 4)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 29)
    }

    #[test]
    fn cumulative_time_table_lower_bound_527() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((21, 29), 9, 6), ((28, 29), 2, 7), ((25, 50), 5, 8)], 17); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 30)
    }

    #[test]
    fn cumulative_time_table_lower_bound_757() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[
                ((11, 20), 10, 4),
                ((12, 20), 9, 5),
                ((11, 20), 10, 7),
                ((11, 31), 10, 10),
            ],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 21)
    }

    #[test]
    fn cumulative_time_table_lower_bound_762() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((47, 50), 4, 7), ((46, 50), 5, 8), ((45, 53), 6, 8)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 51)
    }

    #[test]
    fn cumulative_time_table_lower_bound_766() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((20, 24), 5, 5), ((21, 24), 4, 6), ((16, 34), 9, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 25)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1412() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((42, 46), 8, 7), ((43, 46), 7, 1), ((42, 54), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 50)
    }

    #[test]
    fn cumulative_time_table_lower_bound_771() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((21, 30), 10, 7), ((28, 30), 3, 4), ((21, 36), 10, 10)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 31)
    }

    #[test]
    fn cumulative_time_table_lower_bound_583() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((38, 39), 7, 9), ((33, 46), 7, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 45)
    }

    #[test]
    fn cumulative_time_table_lower_bound_790() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((20, 24), 5, 5), ((21, 24), 4, 6), ((16, 42), 9, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 25)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1360() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 32), 10, 8), ((30, 42), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 39)
    }

    #[test]
    fn cumulative_time_table_lower_bound_604() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((52, 54), 6, 8), ((53, 60), 2, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 58)
    }

    #[test]
    fn cumulative_time_table_lower_bound_805() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((21, 29), 10, 4), ((21, 29), 10, 7), ((21, 40), 9, 9)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 31)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1396() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((42, 47), 7, 6), ((42, 49), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 49)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1510() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((39, 45), 7, 6), ((38, 45), 8, 7), ((41, 54), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 46)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1513() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((22, 24), 4, 7), ((21, 24), 5, 6), ((23, 29), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 26)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1423() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 29), 3, 9), ((27, 64), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 32)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1531() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((11, 15), 10, 8), ((14, 23), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 21)
    }

    #[test]
    fn cumulative_time_table_lower_bound_833() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[
                ((11, 20), 10, 4),
                ((12, 20), 9, 5),
                ((11, 20), 10, 7),
                ((11, 38), 10, 10),
            ],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 21)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1447() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((31, 33), 3, 9), ((31, 33), 3, 3), ((32, 60), 2, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 34)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1448() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 38), 8, 8), ((37, 51), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 43)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1454() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((42, 46), 7, 6), ((44, 64), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 49)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1460() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 36), 10, 8), ((34, 64), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 39)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1463() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((28, 36), 10, 8), ((34, 43), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 38)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1571() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((25, 28), 4, 7), ((26, 30), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 29)
    }

    #[test]
    fn cumulative_time_table_lower_bound_857() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((20, 22), 4, 7), ((14, 22), 10, 7), ((18, 31), 5, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 24)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1577() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 14), 10, 8), ((9, 14), 9, 4), ((10, 50), 5, 4)], 13); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 18)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1480() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((18, 21), 4, 10), ((19, 29), 3, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 22)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1481() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 12), 10, 4), ((9, 12), 9, 3), ((11, 51), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 18)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1493() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((23, 24), 4, 10), ((23, 51), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 27)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1509() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((39, 45), 8, 8), ((40, 53), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 47)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1522() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((48, 51), 4, 9), ((46, 53), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 52)
    }

    #[test]
    fn cumulative_time_table_lower_bound_1528() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((34, 38), 8, 8), ((35, 51), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.lower_bound(*variables.last().unwrap()) >= 42)
    }

    #[test]
    fn cumulative_time_table_upper_bound_6() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((141, 146), 7, 9), ((27, 147), 7, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 139)
    }

    #[test]
    fn cumulative_time_table_upper_bound_7() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((142, 147), 7, 9), ((144, 147), 5, 8), ((21, 148), 2, 3)],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 145)
    }

    #[test]
    fn cumulative_time_table_upper_bound_16() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((27, 27), 4, 9), ((21, 30), 4, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 23)
    }

    #[test]
    fn cumulative_time_table_upper_bound_14() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((131, 131), 10, 10), ((54, 140), 7, 10)], 19); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 124)
    }

    #[test]
    fn cumulative_time_table_upper_bound_61() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((127, 127), 2, 8), ((34, 128), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 125)
    }

    #[test]
    fn cumulative_time_table_upper_bound_149() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((126, 129), 6, 10), ((45, 131), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 127)
    }

    #[test]
    fn cumulative_time_table_upper_bound_175() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((71, 71), 3, 9), ((48, 73), 7, 6)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 64)
    }

    #[test]
    fn cumulative_time_table_upper_bound_176() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((22, 23), 4, 10), ((18, 25), 3, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 20)
    }

    #[test]
    fn cumulative_time_table_upper_bound_178() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((12, 20), 9, 3), ((18, 20), 3, 10), ((18, 20), 2, 2)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 18)
    }

    #[test]
    fn cumulative_time_table_upper_bound_154() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((6, 8), 10, 8), ((0, 15), 8, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
    }

    #[test]
    fn cumulative_time_table_upper_bound_212() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((124, 124), 2, 8), ((35, 125), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 120)
    }

    #[test]
    fn cumulative_time_table_upper_bound_74() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((108, 108), 7, 9), ((78, 114), 2, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 106)
    }

    #[test]
    fn cumulative_time_table_upper_bound_174() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((104, 104), 2, 9), ((49, 105), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 101)
    }

    #[test]
    fn cumulative_time_table_upper_bound_262() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((86, 86), 8, 8), ((46, 93), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 82)
    }

    #[test]
    fn cumulative_time_table_upper_bound_306() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((86, 91), 8, 8), ((33, 93), 10, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 81)
    }

    #[test]
    fn cumulative_time_table_upper_bound_240() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((30, 36), 10, 7), ((30, 36), 10, 4), ((25, 39), 10, 10)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 26)
    }

    #[test]
    fn cumulative_time_table_upper_bound_244() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((93, 94), 2, 8), ((39, 94), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 91)
    }

    #[test]
    fn cumulative_time_table_upper_bound_347() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((86, 86), 8, 8), ((26, 93), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 82)
    }

    #[test]
    fn cumulative_time_table_upper_bound_433() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((72, 72), 8, 8), ((59, 79), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 69)
    }

    #[test]
    fn cumulative_time_table_upper_bound_435() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((82, 82), 3, 9), ((0, 84), 8, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 74)
    }

    #[test]
    fn cumulative_time_table_upper_bound_293() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((5, 9), 10, 8), ((0, 14), 8, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 1)
    }

    #[test]
    fn cumulative_time_table_upper_bound_460() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((84, 85), 7, 6), ((52, 90), 6, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 79)
    }

    #[test]
    fn cumulative_time_table_upper_bound_554() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((23, 23), 4, 10), ((20, 26), 3, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 20)
    }

    #[test]
    fn cumulative_time_table_upper_bound_374() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((22, 23), 4, 7), ((18, 25), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 21)
    }

    #[test]
    fn cumulative_time_table_upper_bound_383() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((78, 78), 2, 9), ((56, 79), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 73)
    }

    #[test]
    fn cumulative_time_table_upper_bound_618() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((61, 61), 6, 10), ((24, 66), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 59)
    }

    #[test]
    fn cumulative_time_table_upper_bound_389() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((8, 9), 10, 8), ((0, 17), 8, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 1)
    }

    #[test]
    fn cumulative_time_table_upper_bound_642() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((12, 20), 9, 3), ((18, 20), 3, 10), ((18, 20), 2, 2)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 18)
    }

    #[test]
    fn cumulative_time_table_upper_bound_720() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((21, 24), 8, 7), ((21, 28), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 22)
    }

    #[test]
    fn cumulative_time_table_upper_bound_820() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((31, 39), 10, 3), ((38, 39), 3, 7), ((21, 40), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 37)
    }

    #[test]
    fn cumulative_time_table_upper_bound_446() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((46, 49), 7, 10), ((41, 52), 7, 9)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 42)
    }

    #[test]
    fn cumulative_time_table_upper_bound_866() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((20, 25), 8, 7), ((21, 27), 4, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 21)
    }

    #[test]
    fn cumulative_time_table_upper_bound_869() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((46, 46), 2, 9), ((46, 46), 2, 4), ((41, 47), 5, 4)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 41)
    }

    #[test]
    fn cumulative_time_table_upper_bound_872() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((43, 48), 7, 1), ((48, 48), 2, 8), ((46, 49), 2, 9)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 46)
    }

    #[test]
    fn cumulative_time_table_upper_bound_457() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((38, 39), 7, 9), ((21, 44), 10, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 29)
    }

    #[test]
    fn cumulative_time_table_upper_bound_882() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 31), 3, 7), ((18, 31), 8, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 23)
    }

    #[test]
    fn cumulative_time_table_upper_bound_797() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((32, 35), 10, 10), ((32, 35), 10, 4), ((21, 41), 9, 9)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 26)
    }

    #[test]
    fn cumulative_time_table_upper_bound_912() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((28, 29), 3, 7), ((20, 30), 4, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 25)
    }

    #[test]
    fn cumulative_time_table_upper_bound_484() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((36, 41), 7, 9), ((38, 41), 5, 8), ((31, 42), 10, 1)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 31)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1110() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((46, 47), 2, 8), ((45, 47), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 45)
    }

    #[test]
    fn cumulative_time_table_upper_bound_961() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((27, 27), 5, 6), ((8, 31), 10, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 17)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1122() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 35), 10, 8), ((26, 44), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 32)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1014() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((13, 13), 10, 8), ((0, 22), 8, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 5)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1188() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((51, 51), 4, 9), ((41, 54), 7, 6)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 44)
    }

    #[test]
    fn cumulative_time_table_upper_bound_884() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((34, 40), 7, 10), ((11, 40), 10, 10)], 19); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 30)
    }

    #[test]
    fn cumulative_time_table_upper_bound_890() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((32, 35), 10, 10), ((32, 35), 10, 4), ((22, 41), 10, 7)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 25)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1277() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 38), 10, 8), ((19, 44), 8, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 30)
    }

    #[test]
    fn cumulative_time_table_upper_bound_924() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((45, 50), 7, 10), ((48, 50), 4, 7), ((44, 51), 2, 8)], 19); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 48)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1187() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((32, 41), 10, 3), ((39, 41), 3, 7), ((31, 41), 5, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 36)
    }

    #[test]
    fn cumulative_time_table_upper_bound_957() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((28, 37), 10, 7), ((28, 37), 10, 10), ((26, 37), 10, 4)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 27)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1200() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((53, 57), 6, 1), ((52, 57), 7, 6), ((50, 58), 5, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 52)
    }

    #[test]
    fn cumulative_time_table_upper_bound_633() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((45, 48), 7, 9), ((41, 51), 7, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 41)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1425() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((24, 25), 8, 8), ((21, 31), 4, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 21)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1448() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((22, 22), 3, 10), ((18, 24), 4, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 18)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1288() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((24, 27), 8, 7), ((21, 31), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 25)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1039() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((45, 50), 7, 10), ((50, 50), 2, 9), ((46, 51), 2, 5)], 19); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 48)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1350() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((7, 14), 10, 8), ((8, 16), 3, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 11)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1401() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 29), 3, 7), ((25, 31), 4, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 25)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1710() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((30, 33), 4, 9), ((21, 33), 4, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 29)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1713() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((36, 37), 10, 8), ((11, 45), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 35)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1451() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((4, 8), 10, 8), ((0, 13), 8, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 0)
    }

    #[test]
    fn cumulative_time_table_upper_bound_775() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((30, 36), 7, 9), ((27, 36), 10, 1), ((31, 36), 5, 8)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 31)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1480() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((38, 43), 8, 7), ((26, 45), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 41)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1878() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((26, 32), 7, 6), ((18, 32), 3, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 29)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1880() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((51, 51), 2, 8), ((26, 52), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 49)
    }

    #[test]
    fn cumulative_time_table_upper_bound_815() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[
                ((32, 41), 10, 10),
                ((34, 41), 8, 3),
                ((38, 41), 4, 4),
                ((29, 41), 2, 2),
            ],
            17,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 39)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1909() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((30, 38), 10, 8), ((24, 39), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 34)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1542() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((38, 45), 8, 7), ((31, 45), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 42)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1965() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((31, 33), 7, 6), ((28, 33), 10, 8), ((18, 37), 2, 2)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 31)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1245() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((36, 42), 7, 10), ((39, 42), 4, 7), ((31, 42), 3, 3)], 19); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 39)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1574() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((41, 46), 8, 7), ((42, 46), 7, 1), ((39, 48), 7, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 39)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1588() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((11, 16), 10, 8), ((12, 16), 9, 4), ((8, 20), 3, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 13)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2081() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((21, 23), 4, 9), ((8, 24), 3, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 20)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1619() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((38, 45), 8, 7), ((8, 45), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 43)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1348() {
        let (solver, _, variables) = set_up_cumulative_state(
            &[((39, 46), 8, 3), ((40, 46), 7, 10), ((11, 46), 10, 7)],
            19,
        ); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 36)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1349() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((39, 46), 8, 3), ((40, 46), 7, 10), ((12, 46), 9, 9)], 19); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 37)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2203() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((50, 53), 6, 10), ((29, 55), 7, 6)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 46)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2206() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 30), 10, 8), ((26, 38), 3, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 27)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1685() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((42, 47), 8, 7), ((43, 47), 7, 6), ((39, 49), 7, 1)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 40)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2212() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((26, 33), 10, 8), ((29, 33), 7, 6), ((12, 35), 9, 3)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 24)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1697() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((41, 46), 8, 7), ((42, 46), 7, 1), ((8, 48), 2, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 44)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1706() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((18, 25), 8, 7), ((21, 25), 5, 6), ((12, 25), 9, 4)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 16)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2269() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((32, 35), 10, 8), ((26, 41), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 31)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2276() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((50, 53), 6, 10), ((39, 55), 4, 9)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 49)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1023() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((39, 46), 8, 3), ((40, 46), 7, 9), ((21, 46), 9, 6)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 37)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1053() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((35, 41), 7, 10), ((34, 41), 8, 3), ((23, 41), 3, 5)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 38)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2339() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((36, 38), 3, 9), ((36, 38), 2, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 36)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2357() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((12, 19), 9, 3), ((18, 19), 3, 10), ((8, 20), 10, 4)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 9)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1796() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((31, 31), 3, 7), ((18, 33), 4, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 27)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1797() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((18, 18), 3, 6), ((8, 20), 10, 8)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 8)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2368() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((32, 33), 7, 6), ((29, 33), 10, 8), ((8, 38), 2, 2)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 31)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1804() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((39, 45), 7, 6), ((38, 45), 8, 7), ((36, 45), 3, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 42)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2400() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 36), 10, 8), ((21, 38), 8, 8)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 28)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1105() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((41, 46), 7, 9), ((38, 47), 7, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 39)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1111() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((29, 32), 7, 9), ((21, 35), 10, 10)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 22)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1115() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((45, 46), 7, 10), ((18, 51), 5, 8)], 17); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 41)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1904() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((52, 55), 7, 6), ((34, 58), 2, 9)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 53)
    }

    #[test]
    fn cumulative_time_table_upper_bound_2533() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((14, 19), 8, 7), ((10, 21), 4, 10)], 14); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 15)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1908() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((43, 45), 3, 7), ((10, 45), 8, 7)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 37)
    }

    #[test]
    fn cumulative_time_table_upper_bound_1924() {
        let (solver, _, variables) =
            set_up_cumulative_state(&[((33, 36), 5, 7), ((28, 36), 10, 3), ((21, 37), 5, 6)], 13); // It could be the case that a conflict is returned

        assert!(solver.upper_bound(*variables.last().unwrap()) <= 31)
    }
}
