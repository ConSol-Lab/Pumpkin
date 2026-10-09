use super::TimeTableChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::DomainView;
use crate::RetentionCheck;
use crate::RetentionChecker;

/// A task with the bounds it has in the state.
#[derive(Clone, Copy, Debug)]
struct BoundedTask {
    /// The earliest start time.
    est: i64,
    /// The latest start time.
    lst: i64,
    processing_time: i64,
    resource_usage: i64,
}

impl BoundedTask {
    /// Whether the task runs throughout `[start, end)` whatever its start time.
    fn is_mandatory_in(&self, start: i64, end: i64) -> bool {
        self.lst <= start && end <= self.est + self.processing_time
    }

    /// Whether the task overlaps `[start, end)` when it starts at `start_time`.
    fn overlaps_when_starting_at(&self, start_time: i64, start: i64, end: i64) -> bool {
        start_time < end && start_time + self.processing_time > start
    }
}

/// A maximal interval `[start, end)` over which the mandatory parts of the tasks use `height` of
/// the resource.
#[derive(Clone, Copy, Debug)]
struct Segment {
    start: i64,
    end: i64,
    height: i64,
}

/// Mirrors one pass of the time-table propagators on the bounds of the start times.
///
/// The time-table is built from the mandatory parts `[lst, est + p)` of the tasks. The propagators
/// have nothing left to propagate when no point of the time-table exceeds the capacity, and no
/// task, started at its earliest or at its latest start time, overlaps a part of the time-table
/// that it is not mandatory in and that would exceed the capacity together with it.
///
/// Holes that the propagators make inside the domains when they are allowed to are not checked.
impl<Var, Atomic> RetentionChecker<Atomic> for TimeTableChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check_retention(&self, state: &dyn DomainView<Atomic>) -> RetentionCheck {
        let capacity = i64::from(self.capacity);
        let tasks = self
            .tasks
            .iter()
            .map(|task| {
                let est: i32 = task
                    .start_time
                    .induced_lower_bound(state)
                    .try_into()
                    .expect("the domains of a retention check are bounded");
                let lst: i32 = task
                    .start_time
                    .induced_upper_bound(state)
                    .try_into()
                    .expect("the domains of a retention check are bounded");
                BoundedTask {
                    est: i64::from(est),
                    lst: i64::from(lst),
                    processing_time: i64::from(task.processing_time),
                    resource_usage: i64::from(task.resource_usage),
                }
            })
            .collect::<Vec<_>>();

        if let Some(task) = tasks.iter().find(|task| task.resource_usage > capacity) {
            log::error!(
                "The task {task:?} uses more than the capacity {capacity}; the constraint should have been reported as a conflict"
            );
            return RetentionCheck::PropagationMissed;
        }

        let time_table = time_table(&tasks);

        if let Some(segment) = time_table.iter().find(|segment| segment.height > capacity) {
            log::error!(
                "The mandatory parts in {segment:?} exceed the capacity {capacity}; the overload should have been reported as a conflict"
            );
            return RetentionCheck::PropagationMissed;
        }

        for task in tasks.iter() {
            for segment in time_table.iter() {
                if segment.height + task.resource_usage <= capacity
                    || task.is_mandatory_in(segment.start, segment.end)
                {
                    continue;
                }

                for (bound, start_time) in [("earliest", task.est), ("latest", task.lst)] {
                    if task.overlaps_when_starting_at(start_time, segment.start, segment.end) {
                        log::error!(
                            "The task {task:?} cannot start at its {bound} start time {start_time}: it would exceed the capacity {capacity} in {segment:?}"
                        );
                        return RetentionCheck::PropagationMissed;
                    }
                }
            }
        }

        RetentionCheck::NothingToPropagate
    }
}

/// The segments of the time-table built from the mandatory parts of the tasks, in increasing order
/// of time.
///
/// The segments are bounded by the starts and ends of the mandatory parts, so every task is either
/// mandatory throughout a segment or nowhere in it.
fn time_table(tasks: &[BoundedTask]) -> Vec<Segment> {
    let mandatory_parts = tasks
        .iter()
        .filter(|task| task.lst < task.est + task.processing_time)
        .collect::<Vec<_>>();

    let mut boundaries = mandatory_parts
        .iter()
        .flat_map(|task| [task.lst, task.est + task.processing_time])
        .collect::<Vec<_>>();
    boundaries.sort_unstable();
    boundaries.dedup();

    boundaries
        .windows(2)
        .map(|window| {
            let (start, end) = (window[0], window[1]);
            let height = mandatory_parts
                .iter()
                .filter(|task| task.is_mandatory_in(start, end))
                .map(|task| task.resource_usage)
                .sum();
            Segment { start, end, height }
        })
        .filter(|segment| segment.height > 0)
        .collect()
}
