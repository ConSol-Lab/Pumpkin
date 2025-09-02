use std::cmp::max;

use super::disjunctive_task::DisjunctiveTask;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::ReadDomains;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

// A node in the [`ThetaTree`] which keeps track of the ECT and sum of processing times of its
// children
//
// As opposed to the nodes in a Theta tree, the nodes in a ThetaLambdaTree also keep track of the
// ECT and sum of processing times if a single element from the Lambda set can be added
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Node {
    /// The earliest completion time of the set of tasks represented by this node.
    ect: i32,
    /// The sum of the processing times of the set of tasks represented by this node.
    sum_of_processing_times: i32,
    /// The earliest completion time of the set of tasks represented by this node if a single grey
    /// task can be added to the set of tasks.
    ect_bar: i32,
    /// The sum of processing times of the set of tasks represented by this node if a single grey
    /// task can be added to the set of tasks.
    sum_of_processing_times_bar: i32,
}

impl Node {
    // Constructs an empty node
    fn empty() -> Self {
        Self {
            ect: i32::MIN,
            sum_of_processing_times: 0,
            ect_bar: i32::MIN,
            sum_of_processing_times_bar: 0,
        }
    }

    // Construct a new white node with the provided value
    fn new_white_node(ect: i32, sum_of_processing_times: i32) -> Self {
        Self {
            ect,
            sum_of_processing_times,
            ect_bar: ect,
            sum_of_processing_times_bar: sum_of_processing_times,
        }
    }

    // Construct a new gray node with the provided value
    fn new_gray_node(ect: i32, sum_of_processing_times: i32) -> Self {
        Self {
            ect: i32::MIN,
            sum_of_processing_times: 0,
            ect_bar: ect,
            sum_of_processing_times_bar: sum_of_processing_times,
        }
    }
}

/// A structure for efficiently calculating the ECT of a set of tasks while only allowing one
/// element from another (non-overlapping) set.
///
/// The implementation is based on \[1\]. The idea is to have a complete binary tree where the leaf
/// nodes represent the tasks. These leaf nodes are sorted by EST and this allows the values of the
/// inner nodes to be calculated using a recursive formula.
///
/// # Bibliography
/// \[1\] P. Vilím, ‘Filtering algorithms for the unary resource constraint’, Archives of Control
/// Sciences, vol. 18, no. 2, pp. 159–202, 2008.
#[derive(Debug, Clone)]
pub(super) struct ThetaLambdaTree<Var> {
    pub(super) nodes: Vec<Node>,
    /// Then we keep track of a mapping from the [`LocalId`] to its position in the tree since the
    /// methods take as input tasks with [`LocalId`]s.
    mapping: KeyedVec<LocalId, usize>,
    /// Then we keep track of a mapping from the position in the tree to its corresponding
    /// [`LocalId`] since [`ThetaLambdaTree`] requires us to return the responsible [`LocalId`].
    reverse_mapping: KeyedVec<usize, LocalId>,
    /// The number of internal nodes in the tree; used to calculate the leaf node index based on
    /// the index in the tree
    number_of_internal_nodes: usize,
    /// The tasks which are stored in the leaves of the tree.
    ///
    /// These tasks are sorted based on non-decreasing start time.
    sorted_tasks: Vec<DisjunctiveTask<Var>>,
}

impl<Var: IntegerVariable> ThetaLambdaTree<Var> {
    /// Initialises the theta-lambda tree.
    ///
    /// Note that [`Self::update`] should be called to actually create the tree itself.
    pub(super) fn new(tasks: &[DisjunctiveTask<Var>]) -> Self {
        // Calculate the number of internal nodes which are required to create the binary tree
        let mut number_of_internal_nodes = 1;
        while number_of_internal_nodes < tasks.len() {
            number_of_internal_nodes <<= 1;
        }

        ThetaLambdaTree {
            nodes: Default::default(),
            mapping: KeyedVec::default(),
            reverse_mapping: KeyedVec::default(),
            number_of_internal_nodes: number_of_internal_nodes - 1,
            sorted_tasks: tasks.to_vec(),
        }
    }

    /// Update the theta-lambda tree based on the provided `context`.
    ///
    /// It resets theta and lambda to be the empty set.
    pub(super) fn update(&mut self, context: PropagationContext) {
        // First we sort the tasks by lower-bound/earliest start time.
        self.sorted_tasks
            .sort_by_key(|task| context.lower_bound(&task.start_time));

        // Then we keep track of a mapping from the [`LocalId`] to its position in the tree and a
        // reverse mapping
        self.mapping.clear();
        self.reverse_mapping.clear();
        for (index, task) in self.sorted_tasks.iter().enumerate() {
            while self.mapping.len() <= task.id.index() {
                let _ = self.mapping.push(usize::MAX);
            }
            self.mapping[task.id] = index;
            let _ = self.reverse_mapping.push(task.id);
        }

        // Finally, we reset the entire tree to be empty
        self.nodes.clear();
        for _ in 0..=2 * self.number_of_internal_nodes {
            self.nodes.push(Node::empty())
        }
    }

    /// Returns the earliest completion time of Theta
    pub(super) fn ect(&self) -> i32 {
        pumpkin_assert_simple!(!self.nodes.is_empty());
        self.nodes[0].ect
    }

    /// Returns the earliest completion time of Theta while allowing one element of Lambda to be
    /// added
    pub(super) fn ect_bar(&self) -> i32 {
        self.nodes[0].ect_bar
    }

    /// Returns the index in the tree of the `index`th node
    fn get_leaf_node_index(&self, index: usize) -> usize {
        pumpkin_assert_simple!(
            index >= self.number_of_internal_nodes,
            "Provided index was not a leaf node"
        );
        index - self.number_of_internal_nodes
    }

    /// Returns the [`LocalId`] for the task corresponding with the task in Lambda which was
    /// responsible for the value of `ect_bar`
    ///
    /// This can be [`None`] if an overflow occurs and there are no elements in lambda
    pub(super) fn responsible_ect_bar(&self) -> Option<LocalId> {
        self.responsible_index_ect_bar_internal(0)
            .map(|index| self.reverse_mapping[self.get_leaf_node_index(index)])
    }

    fn responsible_index_ect_bar_internal(&self, position: usize) -> Option<usize> {
        // See \[1\] for the implementation
        if self.is_leaf(position) {
            // Assuming that all tasks have non-zero processing time
            (self.nodes[position].sum_of_processing_times_bar > 0
                && self.nodes[position].sum_of_processing_times == 0)
                .then_some(position)
        } else {
            let left_child = Self::get_left_child_index(position);
            let right_child = Self::get_right_child_index(position);

            if self.nodes[right_child] != Node::empty()
                && self.nodes[position].ect_bar == self.nodes[right_child].ect_bar
            {
                self.responsible_index_ect_bar_internal(right_child)
            } else if self.nodes[right_child] != Node::empty()
                && self.nodes[position].ect_bar
                    == self.nodes[left_child].ect
                        + self.nodes[right_child].sum_of_processing_times_bar
            {
                self.responsible_index_p_internal(right_child)
            } else if self.nodes[left_child] != Node::empty()
                && self.nodes[position].ect_bar
                    == self.nodes[left_child].ect_bar
                        + self.nodes[right_child].sum_of_processing_times
            {
                self.responsible_index_ect_bar_internal(left_child)
            } else {
                None
            }
        }
    }

    fn responsible_index_p_internal(&self, position: usize) -> Option<usize> {
        if self.is_leaf(position) {
            // Assuming that all tasks have non-zero processing time
            (self.nodes[position].sum_of_processing_times_bar > 0
                && self.nodes[position].sum_of_processing_times == 0)
                .then_some(position)
        } else {
            let left_child = Self::get_left_child_index(position);
            let right_child = Self::get_right_child_index(position);

            if self.nodes[left_child] != Node::empty()
                && self.nodes[position].sum_of_processing_times_bar
                    == self.nodes[left_child].sum_of_processing_times_bar
                        + self.nodes[right_child].sum_of_processing_times
            {
                self.responsible_index_p_internal(left_child)
            } else if self.nodes[right_child] != Node::empty()
                && self.nodes[position].sum_of_processing_times_bar
                    == self.nodes[left_child].sum_of_processing_times
                        + self.nodes[right_child].sum_of_processing_times_bar
            {
                self.responsible_index_p_internal(right_child)
            } else {
                None
            }
        }
    }

    /// Add the provided task to Lambda
    pub(super) fn add_to_lambda<OtherVar: IntegerVariable>(
        &mut self,
        task: &DisjunctiveTask<OtherVar>,
        context: PropagationContext,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = context.lower_bound(&task.start_time) + task.processing_time;

        self.nodes[position] = Node::new_gray_node(ect, task.processing_time);
        self.upheap(position);
    }

    /// Remove the provided task from Lambda (this method assumes that the element is already not a
    /// part of Theta at this point)
    pub(super) fn remove_from_lambda(&mut self, task: &DisjunctiveTask<Var>) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        pumpkin_assert_simple!(self.nodes[position].sum_of_processing_times == 0);
        self.nodes[position] = Node::empty();
        self.upheap(position)
    }

    /// Add the provided task to Theta
    pub(super) fn add_to_theta(
        &mut self,
        task: &DisjunctiveTask<Var>,
        context: PropagationContext,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = context.lower_bound(&task.start_time) + task.processing_time;

        self.nodes[position] = Node::new_white_node(ect, task.processing_time);
        self.upheap(position)
    }

    /// Remove the provided task from Theta
    pub(super) fn remove_from_theta<OtherVar>(&mut self, task: &DisjunctiveTask<OtherVar>) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        self.nodes[position] = Node::empty();
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
        pumpkin_assert_simple!(index > 0);
        (index - 1) / 2
    }

    /// Returns whether the provided index is a leaf node by checking whether its left child is
    /// outside of the range of the number of nodes
    fn is_leaf(&self, index: usize) -> bool {
        pumpkin_assert_simple!(index < self.nodes.len());
        Self::get_left_child_index(index) >= self.nodes.len()
    }

    /// Calculate the new values for the ancestors of the provided index
    pub(super) fn upheap(&mut self, mut index: usize) {
        while index != 0 {
            let parent = Self::get_parent(index);
            let left_child_of_parent = Self::get_left_child_index(parent);
            let right_child_of_parent = Self::get_right_child_index(parent);
            pumpkin_assert_simple!(left_child_of_parent == index || right_child_of_parent == index);

            self.nodes[parent].sum_of_processing_times = self.nodes[left_child_of_parent]
                .sum_of_processing_times
                + self.nodes[right_child_of_parent].sum_of_processing_times;

            self.nodes[parent].ect = max(
                self.nodes[right_child_of_parent].ect,
                self.nodes[left_child_of_parent].ect
                    + self.nodes[right_child_of_parent].sum_of_processing_times,
            );

            self.nodes[parent].sum_of_processing_times_bar = max(
                self.nodes[left_child_of_parent].sum_of_processing_times_bar
                    + self.nodes[right_child_of_parent].sum_of_processing_times,
                self.nodes[left_child_of_parent].sum_of_processing_times
                    + self.nodes[right_child_of_parent].sum_of_processing_times_bar,
            );
            self.nodes[parent].ect_bar = max(
                self.nodes[right_child_of_parent].ect_bar,
                max(
                    self.nodes[left_child_of_parent].ect
                        + self.nodes[right_child_of_parent].sum_of_processing_times_bar,
                    self.nodes[left_child_of_parent].ect_bar
                        + self.nodes[right_child_of_parent].sum_of_processing_times,
                ),
            );

            index = parent;
        }
    }

    /// Returns the total sum of processing times of the elements in the set theta.
    pub(crate) fn sum_of_processing_times(&self) -> i32 {
        let result = self.nodes[0].sum_of_processing_times;

        pumpkin_assert_moderate!(
            self.get_theta()
                .iter()
                .map(|task| task.processing_time)
                .sum::<i32>()
                == result
        );

        result
    }

    /// Returns the tasks which are currently in the set theta.
    ///
    /// This method returns the tasks in order of earliest start time at the time of creating the
    /// theta-lambda-tree.
    pub(crate) fn get_theta(&self) -> Vec<DisjunctiveTask<Var>> {
        // We go over all the leaf nodes
        (self.number_of_internal_nodes..self.nodes.len())
            .filter(|&position| self.nodes[position].sum_of_processing_times != 0)
            .map(|position| self.sorted_tasks[self.get_leaf_node_index(position)].clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::propagation::LocalId;
    use crate::engine::propagation::PropagationContext;
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::disjunctive::theta_lambda_tree::Node;
    use crate::propagators::disjunctive::theta_lambda_tree::ThetaLambdaTree;
    use crate::propagators::disjunctive_task::DisjunctiveTask;

    #[test]
    fn tree_built_correctly() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 0);
        let b = solver.new_variable(25, 25);
        let c = solver.new_variable(30, 30);
        let d = solver.new_variable(32, 32);
        let tasks = [
            DisjunctiveTask {
                start_time: a,
                processing_time: 5,
                id: LocalId::from(0),
            },
            DisjunctiveTask {
                start_time: b,
                processing_time: 9,
                id: LocalId::from(1),
            },
            DisjunctiveTask {
                start_time: c,
                processing_time: 5,
                id: LocalId::from(2),
            },
            DisjunctiveTask {
                start_time: d,
                processing_time: 10,
                id: LocalId::from(3),
            },
        ];

        let mut tree = ThetaLambdaTree::new(&tasks, PropagationContext::new(&solver.assignments));

        for task in tasks.iter() {
            tree.add_to_theta(task, PropagationContext::new(&solver.assignments));
        }
        tree.remove_from_theta(&tasks[2]);
        tree.add_to_lambda(&tasks[2], PropagationContext::new(&solver.assignments));

        assert_eq!(
            tree.nodes[6],
            Node {
                ect: 42,
                sum_of_processing_times: 10,
                ect_bar: 42,
                sum_of_processing_times_bar: 10
            }
        );
        assert_eq!(
            tree.nodes[5],
            Node {
                ect: i32::MIN,
                sum_of_processing_times: 0,
                ect_bar: 35,
                sum_of_processing_times_bar: 5
            }
        );
        assert_eq!(
            tree.nodes[4],
            Node {
                ect: 34,
                sum_of_processing_times: 9,
                ect_bar: 34,
                sum_of_processing_times_bar: 9
            }
        );
        assert_eq!(
            tree.nodes[3],
            Node {
                ect: 5,
                sum_of_processing_times: 5,
                ect_bar: 5,
                sum_of_processing_times_bar: 5
            }
        );
        assert_eq!(
            tree.nodes[2],
            Node {
                ect: 42,
                sum_of_processing_times: 10,
                ect_bar: 45,
                sum_of_processing_times_bar: 15
            }
        );
        assert_eq!(
            tree.nodes[1],
            Node {
                ect: 34,
                sum_of_processing_times: 14,
                ect_bar: 34,
                sum_of_processing_times_bar: 14
            }
        );
        assert_eq!(
            tree.nodes[0],
            Node {
                ect: 44,
                sum_of_processing_times: 24,
                ect_bar: 49,
                sum_of_processing_times_bar: 29
            }
        );
    }
}
