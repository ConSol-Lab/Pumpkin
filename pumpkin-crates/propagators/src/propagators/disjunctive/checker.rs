use std::cmp::max;
use std::cmp::min;
use std::marker::PhantomData;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::I32Ext;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::VariableState;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::propagation::LocalId;

use crate::disjunctive::ArgDisjunctiveTask;
use crate::disjunctive::disjunctive_task::DisjunctiveTask;
use crate::disjunctive::theta_lambda_tree::Node;

#[derive(Clone, Debug)]
pub struct DisjunctiveEdgeFindingChecker<Var> {
    pub tasks: Box<[ArgDisjunctiveTask<Var>]>,
}

impl<Var, Atomic> InferenceChecker<Atomic> for DisjunctiveEdgeFindingChecker<Var>
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
            if task.start_time.induced_lower_bound(&state) != I32Ext::NegativeInf
                && task.start_time.induced_upper_bound(&state) != I32Ext::PositiveInf
            {
                // Now we calculate the durations of tasks
                let est_task: i32 = task
                    .start_time
                    .induced_lower_bound(&state)
                    .try_into()
                    .unwrap();
                let lst_task =
                    <I32Ext as TryInto<i32>>::try_into(task.start_time.induced_upper_bound(&state))
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
                    .map(|(index, task)| DisjunctiveTask {
                        start_time: task.start_time.clone(),
                        processing_time: task.processing_time,
                        id: LocalId::from(index as u32),
                    })
                    .collect::<Vec<_>>(),
            );
            theta_lambda_tree.update(&state);
            for (index, task) in theta.iter().enumerate() {
                theta_lambda_tree.add_to_theta(
                    &DisjunctiveTask {
                        start_time: task.start_time.clone(),
                        processing_time: task.processing_time,
                        id: LocalId::from(index as u32),
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

#[cfg(test)]
mod tests {
    use pumpkin_checking::TestAtomic;
    use pumpkin_checking::VariableState;

    use super::*;

    #[test]
    fn test_simple_propagation() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 7,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 5,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 6,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x3",
            comparison: pumpkin_checking::Comparison::GreaterEqual,
            value: 8,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = DisjunctiveEdgeFindingChecker {
            tasks: vec![
                ArgDisjunctiveTask {
                    start_time: "x1",
                    processing_time: 2,
                },
                ArgDisjunctiveTask {
                    start_time: "x2",
                    processing_time: 3,
                },
                ArgDisjunctiveTask {
                    start_time: "x3",
                    processing_time: 5,
                },
            ]
            .into(),
        };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn test_conflict() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 1,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = DisjunctiveEdgeFindingChecker {
            tasks: vec![
                ArgDisjunctiveTask {
                    start_time: "x1",
                    processing_time: 2,
                },
                ArgDisjunctiveTask {
                    start_time: "x2",
                    processing_time: 3,
                },
            ]
            .into(),
        };

        assert!(checker.check(state, &premises, None));
    }

    #[test]
    fn test_simple_propagation_not_accepted() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 7,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 5,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 6,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x3",
            comparison: pumpkin_checking::Comparison::GreaterEqual,
            value: 9,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = DisjunctiveEdgeFindingChecker {
            tasks: vec![
                ArgDisjunctiveTask {
                    start_time: "x1",
                    processing_time: 2,
                },
                ArgDisjunctiveTask {
                    start_time: "x2",
                    processing_time: 3,
                },
                ArgDisjunctiveTask {
                    start_time: "x3",
                    processing_time: 5,
                },
            ]
            .into(),
        };

        assert!(!checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn test_conflict_not_accepted() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 2,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = DisjunctiveEdgeFindingChecker {
            tasks: vec![
                ArgDisjunctiveTask {
                    start_time: "x1",
                    processing_time: 2,
                },
                ArgDisjunctiveTask {
                    start_time: "x2",
                    processing_time: 3,
                },
            ]
            .into(),
        };

        assert!(!checker.check(state, &premises, None));
    }
}

#[derive(Debug, Clone)]
pub(super) struct CheckerThetaLambdaTree<Var, Atomic> {
    pub(super) nodes: Vec<Node>,
    /// Then we keep track of a mapping from the [`LocalId`] to its position in the tree since the
    /// methods take as input tasks with [`LocalId`]s.
    mapping: KeyedVec<LocalId, usize>,
    /// The number of internal nodes in the tree; used to calculate the leaf node index based on
    /// the index in the tree
    number_of_internal_nodes: usize,
    /// The tasks which are stored in the leaves of the tree.
    ///
    /// These tasks are sorted based on non-decreasing start time.
    sorted_tasks: Vec<DisjunctiveTask<Var>>,
    phantom_data: PhantomData<Atomic>,
}

impl<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>> CheckerThetaLambdaTree<Var, Atomic> {
    /// Initialises the theta-lambda tree.
    ///
    /// Note that [`Self::update`] should be called to actually create the tree itself.
    pub(super) fn new(tasks: &[DisjunctiveTask<Var>]) -> Self {
        // Calculate the number of internal nodes which are required to create the binary tree
        let mut number_of_internal_nodes = 1;
        while number_of_internal_nodes < tasks.len() {
            number_of_internal_nodes <<= 1;
        }

        CheckerThetaLambdaTree {
            nodes: Default::default(),
            mapping: KeyedVec::default(),
            number_of_internal_nodes: number_of_internal_nodes - 1,
            sorted_tasks: tasks.to_vec(),
            phantom_data: PhantomData,
        }
    }

    /// Update the theta-lambda tree based on the provided `context`.
    ///
    /// It resets theta and lambda to be the empty set.
    pub(super) fn update(&mut self, context: &VariableState<Atomic>) {
        // First we sort the tasks by lower-bound/earliest start time.
        self.sorted_tasks
            .sort_by_key(|task| task.start_time.induced_lower_bound(context));

        // Then we keep track of a mapping from the [`LocalId`] to its position in the tree and a
        // reverse mapping
        self.mapping.clear();
        for (index, task) in self.sorted_tasks.iter().enumerate() {
            while self.mapping.len() <= task.id.index() {
                let _ = self.mapping.push(usize::MAX);
            }
            self.mapping[task.id] = index;
        }

        // Finally, we reset the entire tree to be empty
        self.nodes.clear();
        for _ in 0..=2 * self.number_of_internal_nodes {
            self.nodes.push(Node::empty())
        }
    }

    /// Returns the earliest completion time of Theta
    pub(super) fn ect(&self) -> i32 {
        assert!(!self.nodes.is_empty());
        self.nodes[0].ect
    }

    /// Add the provided task to Theta
    pub(super) fn add_to_theta(
        &mut self,
        task: &DisjunctiveTask<Var>,
        context: &VariableState<Atomic>,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = task.start_time.induced_lower_bound(context) + task.processing_time;

        self.nodes[position] = Node::new_white_node(
            ect.try_into().expect("Should have bounds"),
            task.processing_time,
        );
        self.upheap(position)
    }

    /// Returns the index of the left child of the provided index
    fn get_left_child_index(index: usize) -> usize {
        2 * index + 1
    }

    /// Returns the index of the right child of the provided index
    fn get_right_child_index(index: usize) -> usize {
        2 * index + 2
    }

    /// Returns the index of the parent of the provided index
    fn get_parent(index: usize) -> usize {
        assert!(index > 0);
        (index - 1) / 2
    }

    /// Calculate the new values for the ancestors of the provided index
    pub(super) fn upheap(&mut self, mut index: usize) {
        while index != 0 {
            let parent = Self::get_parent(index);
            let left_child_of_parent = Self::get_left_child_index(parent);
            let right_child_of_parent = Self::get_right_child_index(parent);
            assert!(left_child_of_parent == index || right_child_of_parent == index);

            // The sum of processing times is the sum of processing times in the left child + the
            // sum of processing times in right child
            self.nodes[parent].sum_of_processing_times = self.nodes[left_child_of_parent]
                .sum_of_processing_times
                + self.nodes[right_child_of_parent].sum_of_processing_times;

            // The ECT is either the ECT of the left child node + the processing times of the right
            // child or it is the ECT of the right child (we do not know whether the processing
            // times of the left child influence the processing times of the right child)
            let ect_left = self.nodes[left_child_of_parent].ect
                + self.nodes[right_child_of_parent].sum_of_processing_times;
            self.nodes[parent].ect = max(self.nodes[right_child_of_parent].ect, ect_left);

            // The sum of processing times (including one element of lambda) is either:
            // 1) The sum of processing times of the right child + the sum of processing times of
            //    the left child including one element of lambda
            // 2) The sum of processing times of the left child + the sum of processing times of the
            //    right child include one element of lambda
            let sum_of_processing_times_left_child_lambda = self.nodes[left_child_of_parent]
                .sum_of_processing_times_bar
                + self.nodes[right_child_of_parent].sum_of_processing_times;
            let sum_of_processing_times_right_child_lambda = self.nodes[left_child_of_parent]
                .sum_of_processing_times
                + self.nodes[right_child_of_parent].sum_of_processing_times_bar;
            self.nodes[parent].sum_of_processing_times_bar = max(
                sum_of_processing_times_left_child_lambda,
                sum_of_processing_times_right_child_lambda,
            );

            // The earliest completion time (including one element of lambda) is either:
            // 1) The earliest completion time including one element of lambda from the right child
            // 2) The earliest completion time of the right child + the sum of processing times
            //    including one element of lambda of the right child
            // 2) The earliest completion time of the left child + the sum of processing times
            //    including one element of lambda of the left child
            let ect_right_child_lambda = self.nodes[left_child_of_parent].ect
                + self.nodes[right_child_of_parent].sum_of_processing_times_bar;
            let ect_left_child_lambda = self.nodes[left_child_of_parent].ect_bar
                + self.nodes[right_child_of_parent].sum_of_processing_times;
            self.nodes[parent].ect_bar = max(
                self.nodes[right_child_of_parent].ect_bar,
                max(ect_right_child_lambda, ect_left_child_lambda),
            );

            index = parent;
        }
    }
}
