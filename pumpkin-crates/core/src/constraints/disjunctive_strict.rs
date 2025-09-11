use crate::constraints::Constraint;
use crate::proof::ConstraintTag;
use crate::propagators::disjunctive_propagator::DisjunctivePropagatorConstructor;
use crate::propagators::disjunctive_task::ArgDisjunctiveTask;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

/// Creates the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) [`Constraint`] (also called the `NoOverlap` Constraint or the `Unary Resource` Constraint).
///
/// This constraint ensures that at no point in time, any of the tasks are overlapping.
///
/// The implementation uses edge-finding reasoning as implemented in \[1\]. It propagates both the
/// lower-bound and the upper-bound of the provided tasks.
///
/// The length of `start_times` and `durations` should be the same; if
/// this is not the case then this method will panic.
///
/// It follows the [MiniZinc specifications](https://docs.minizinc.dev/en/stable/lib-globals-scheduling.html#mzn-ref-globals-scheduling-disjunctive-strict) which means that tasks with duration 0 can only be scheduled when no other tasks are running.
///
/// # Bibliography
/// - \[1\] P. Vilím, ‘Filtering algorithms for the unary resource constraint’, Archives of Control
///   Sciences, vol. 18, no. 2, pp. 159–202, 2008.
pub fn disjunctive_strict<Var: IntegerVariable + 'static>(
    tasks: impl IntoIterator<Item = ArgDisjunctiveTask<Var>>,
    constraint_tag: ConstraintTag,
) -> impl Constraint {
    DisjunctiveConstraint {
        tasks: tasks.into_iter().collect::<Vec<_>>(),
        constraint_tag,
    }
}

/// The representation of a `disjunctive` (`no-overlap`) constraint.
struct DisjunctiveConstraint<Var> {
    tasks: Vec<ArgDisjunctiveTask<Var>>,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable + 'static> Constraint for DisjunctiveConstraint<Var> {
    fn post(self, solver: &mut crate::Solver) -> Result<(), crate::ConstraintOperationError> {
        // We post both the propagator on the lower-bound and the propagator on the upper-bound.
        DisjunctivePropagatorConstructor::new(self.tasks.clone(), self.constraint_tag)
            .post(solver)?;
        DisjunctivePropagatorConstructor::new(
            self.tasks.iter().map(|task| ArgDisjunctiveTask {
                // The propagations on the upper-bound take place by "reversing" the tasks such
                // that instead of going from [EST, LST], the domain goes from [-LCT, -ECT]
                start_time: task.start_time.offset(task.processing_time).scaled(-1),
                processing_time: task.processing_time,
            }),
            self.constraint_tag,
        )
        .post(solver)
    }

    fn implied_by(
        self,
        solver: &mut crate::Solver,
        reification_literal: crate::variables::Literal,
    ) -> Result<(), crate::ConstraintOperationError> {
        // We post both the propagator on the lower-bound and the propagator on the upper-bound.
        DisjunctivePropagatorConstructor::new(self.tasks.clone(), self.constraint_tag)
            .implied_by(solver, reification_literal)?;
        DisjunctivePropagatorConstructor::new(
            self.tasks.iter().map(|task| ArgDisjunctiveTask {
                // The propagations on the upper-bound take place by "reversing" the tasks such
                // that instead of going from [EST, LST], the domain goes from [-LCT, -ECT]
                start_time: task.start_time.offset(task.processing_time).scaled(-1),
                processing_time: task.processing_time,
            }),
            self.constraint_tag,
        )
        .implied_by(solver, reification_literal)
    }
}
