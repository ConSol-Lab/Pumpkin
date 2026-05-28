use std::cmp::max;
use std::marker::PhantomData;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;
use pumpkin_core::asserts::pumpkin_assert_simple;
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

/// Performs overload checking on the provided `tasks` and returns true if a conflict could be
/// found.
///
/// Recall the following:
/// We try to find a set omega of jobs with the following property: `p_omega > lct_omega -
/// est_omega`.
fn overload_checking<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    tasks: &[ArgDisjunctiveTask<Var>],
    state: &VariableState<Atomic>,
) -> bool {
    // First, we create our theta-lambda tree
    let mut theta = CheckerThetaLambdaTree::new(
        &tasks
            .iter()
            .enumerate()
            .map(|(index, task)| DisjunctiveTask {
                start_time: task.start_time.clone(),
                processing_time: task.processing_time,
                id: LocalId::from(index as u32),
            })
            .collect::<Vec<_>>(),
    );
    // And update it with the current state.
    theta.update(state);

    // Next, we sort based on non-decreasing latest completion time.
    let mut sorted_tasks = tasks
        .iter()
        .enumerate()
        .filter(|(_, task)| {
            task.start_time.induced_lower_bound(&state) != IntExt::NegativeInf
                && task.start_time.induced_upper_bound(&state) != IntExt::PositiveInf
        })
        .collect::<Vec<_>>();
    sorted_tasks.sort_by_key(|(_, task)| {
        task.start_time.induced_upper_bound(&state) + task.processing_time
    });

    // Then we go over the tasks which are bounded in the state.
    for (index, task) in sorted_tasks {
        pumpkin_assert_simple!(
            task.start_time.induced_lower_bound(&state) != IntExt::NegativeInf
                && task.start_time.induced_upper_bound(&state) != IntExt::PositiveInf
        );
        // And we add it to the theta.
        theta.add_to_theta(
            &DisjunctiveTask {
                start_time: task.start_time.clone(),
                processing_time: task.processing_time,
                id: LocalId::from(index as u32),
            },
            &state,
        );

        // If there is an overload of the interval, then we can report that a conflict has been
        // found.
        if theta.ect() > task.start_time.induced_upper_bound(&state) + task.processing_time {
            return true;
        }
    }

    false
}

impl<Var, Atomic> InferenceChecker<Atomic> for DisjunctiveEdgeFindingChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
    <Atomic as AtomicConstraint>::Identifier: Clone,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
        _premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        // We want to detect conflicts, and we split into two cases:
        // 1. If it is a conflict explanation then overload checking can be applied directly and
        //    should lead to a conflict.
        // 2. If it is a propagation explanation, then for any value in the domain of the propagated
        //    variable, scheduling it at that time-point should lead to a conflict (using overload
        //    checking).
        if let Some(consequent) = consequent {
            // First we retrieve the propagating task.
            let task = self
                .tasks
                .iter()
                .find(|task| task.start_time.does_atomic_constrain_self(consequent))
                .expect("Expected to be able to find atomic");

            let lb: i32 = task
                .start_time
                .induced_lower_bound(&state)
                .try_into()
                .expect("expected non-infinity value");
            let ub: i32 = task
                .start_time
                .induced_upper_bound(&state)
                .try_into()
                .expect("expected non-infinity value");

            // Then we go over every value in its domain.
            for i in lb..=ub {
                // We assign the propagating variable to that value.
                let mut assigned_state = state.clone();
                let _ = assigned_state.apply(&task.start_time.atomic_equal(i));

                // If we do not find a conflict using overload checking, then it is not a valid
                // explanation.
                if !overload_checking(&self.tasks, &assigned_state) {
                    return false;
                }
            }
            true
        } else {
            overload_checking(&self.tasks, &state)
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
