use std::cmp::min;

use super::CheckerThetaLambdaTree;
use super::DisjunctiveEdgeFindingChecker;
use super::IndexedTask;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::ConflictChecker;
use crate::IntExt;
use crate::VariableState;

impl<Var, Atomic> ConflictChecker<Atomic> for DisjunctiveEdgeFindingChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
        _premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        // Recall the following:
        // - For conflict detection, the explanation represents a set omega with the following
        //   property: `p_omega > lct_omega - est_omega`.
        //
        //   We simply need to check whether the interval [est_omega, lct_omega] is overloaded
        // - For propagation, the explanation represents a set omega (and omega') such that the
        //   following holds: `min(est_i, est_omega) + p_omega + p_i > lct_omega -> [s_i >=
        //   ect_omega]`.
        let mut lb_interval = i32::MAX;
        let mut ub_interval = i32::MIN;
        let mut p = 0;
        let mut propagating_task = None;
        let mut theta = Vec::new();

        // We go over all of the tasks
        for task in self.tasks.iter() {
            // Only if they are present in the explanation, do we actually process them
            // - For tasks in omega, both bounds should be present to define the interval
            // - For the propagating task, the lower-bound should be present, and the negation of
            //   the consequent ensures that an upper-bound is present
            if task.start_time.induced_lower_bound(&state) != IntExt::NegativeInf
                && task.start_time.induced_upper_bound(&state) != IntExt::PositiveInf
            {
                // Now we calculate the durations of tasks
                let est_task: i32 = task
                    .start_time
                    .induced_lower_bound(&state)
                    .try_into()
                    .unwrap();
                let lst_task =
                    <IntExt as TryInto<i32>>::try_into(task.start_time.induced_upper_bound(&state))
                        .unwrap();

                let is_propagating_task = if let Some(consequent) = consequent {
                    task.start_time.does_atomic_constrain_self(consequent)
                } else {
                    false
                };
                if !is_propagating_task {
                    theta.push(task.clone());
                    p += task.processing_time;
                    lb_interval = lb_interval.min(est_task);
                    ub_interval = ub_interval.max(lst_task + task.processing_time);
                } else {
                    propagating_task = Some(task.clone());
                }
            }
        }

        if consequent.is_some() {
            let propagating_task = propagating_task
                .expect("If there is a consequent then there should be a propagating task");

            let est_task = propagating_task
                .start_time
                .induced_lower_bound(&state)
                .try_into()
                .unwrap();

            let mut theta_lambda_tree = CheckerThetaLambdaTree::new(
                &theta
                    .iter()
                    .enumerate()
                    .map(|(index, task)| IndexedTask {
                        start_time: task.start_time.clone(),
                        processing_time: task.processing_time,
                        id: index,
                    })
                    .collect::<Vec<_>>(),
            );
            theta_lambda_tree.update(&state);
            for (index, task) in theta.iter().enumerate() {
                theta_lambda_tree.add_to_theta(
                    &IndexedTask {
                        start_time: task.start_time.clone(),
                        processing_time: task.processing_time,
                        id: index,
                    },
                    &state,
                );
            }

            min(est_task, lb_interval) + p + propagating_task.processing_time > ub_interval
                && theta_lambda_tree.ect() > propagating_task.start_time.induced_upper_bound(&state)
        } else {
            // We simply check whether the interval is overloaded
            p > (ub_interval - lb_interval)
        }
    }
}
