use std::cmp::max;
use std::marker::PhantomData;

use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::VariableState;
use crate::checkers::DisjunctiveCheckerTask;

/// A node of the theta-lambda tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Node {
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
}

#[derive(Debug, Clone)]
pub(crate) struct CheckerThetaLambdaTree<Var, Atomic> {
    nodes: Vec<Node>,
    /// The position of each task among the leaves, indexed by the task's position in the checker.
    mapping: Vec<usize>,
    /// The number of internal nodes in the tree; used to calculate the leaf node index based on
    /// the index in the tree
    number_of_internal_nodes: usize,
    /// The tasks which are stored in the leaves of the tree.
    ///
    /// These tasks are sorted based on non-decreasing start time, each with its position in the
    /// checker.
    sorted_tasks: Vec<(usize, DisjunctiveCheckerTask<Var>)>,
    phantom_data: PhantomData<Atomic>,
}

impl<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>> CheckerThetaLambdaTree<Var, Atomic> {
    /// Initialises the theta-lambda tree.
    ///
    /// Note that [`Self::update`] should be called to actually create the tree itself.
    pub(crate) fn new(tasks: &[DisjunctiveCheckerTask<Var>]) -> Self {
        // Calculate the number of internal nodes which are required to create the binary tree
        let mut number_of_internal_nodes = 1;
        while number_of_internal_nodes < tasks.len() {
            number_of_internal_nodes <<= 1;
        }

        CheckerThetaLambdaTree {
            nodes: Default::default(),
            mapping: Vec::new(),
            number_of_internal_nodes: number_of_internal_nodes - 1,
            sorted_tasks: tasks.iter().cloned().enumerate().collect(),
            phantom_data: PhantomData,
        }
    }

    /// Update the theta-lambda tree based on the provided `context`.
    ///
    /// It resets theta and lambda to be the empty set.
    pub(crate) fn update(&mut self, context: &VariableState<Atomic>) {
        // First we sort the tasks by lower-bound/earliest start time.
        self.sorted_tasks
            .sort_by_key(|(_, task)| task.start_time.induced_lower_bound(context));

        // Then we keep track of the position of each task among the leaves
        self.mapping = vec![usize::MAX; self.sorted_tasks.len()];
        for (position, (index, _)) in self.sorted_tasks.iter().enumerate() {
            self.mapping[*index] = position;
        }

        // Finally, we reset the entire tree to be empty
        self.nodes.clear();
        for _ in 0..=2 * self.number_of_internal_nodes {
            self.nodes.push(Node::empty())
        }
    }

    /// Returns the earliest completion time of Theta
    pub(crate) fn ect(&self) -> i32 {
        assert!(!self.nodes.is_empty());
        self.nodes[0].ect
    }

    /// Add the provided task to Theta
    pub(crate) fn add_to_theta(
        &mut self,
        index: usize,
        task: &DisjunctiveCheckerTask<Var>,
        context: &VariableState<Atomic>,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[index];
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
    fn upheap(&mut self, mut index: usize) {
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
