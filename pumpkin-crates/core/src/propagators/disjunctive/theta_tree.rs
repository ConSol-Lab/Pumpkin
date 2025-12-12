use std::cmp::max;

use super::disjunctive_task::DisjunctiveTask;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::propagation::LocalId;
use crate::propagation::PropagationContext;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

// A node in the [`ThetaTree`] which keeps track of the ECT and sum of processing times of its
// children
#[derive(Debug, Clone)]
struct Node {
    ect: i32,
    sum_of_processing_times: i32,
}

impl Node {
    // Constructs an empty node
    fn empty() -> Self {
        Self {
            ect: i32::MIN,
            sum_of_processing_times: 0,
        }
    }

    // Construct a new node with the provided value
    fn new(ect: i32, sum_of_processing_times: i32) -> Self {
        Self {
            ect,
            sum_of_processing_times,
        }
    }
}

/// A structure for efficiently calculating the ECT of a set of tasks.
///
/// The implementation is based on \[1\]. The idea is to have a complete binary tree where the leaf
/// nodes represent the tasks. These leaf nodes are sorted by EST and this allows the values of the
/// inner nodes to be calculated using a recursive formula.
///
/// # Bibliography
/// \[1\] P. Vilím, ‘Filtering algorithms for the unary resource constraint’, Archives of Control
/// Sciences, vol. 18, no. 2, pp. 159–202, 2008.
#[allow(dead_code, reason = "Will be part of the public API")]
#[derive(Debug)]
pub(super) struct ThetaTree {
    nodes: Vec<Node>,
    /// Then we keep track of a mapping from the [`LocalId`] to its position in the tree since the
    /// methods take as input tasks with [`LocalId`]s.
    mapping: KeyedVec<LocalId, usize>,
}

#[allow(dead_code, reason = "Will be part of the public API")]
impl ThetaTree {
    pub(super) fn new<Var: IntegerVariable>(
        tasks: &[DisjunctiveTask<Var>],
        context: PropagationContext,
    ) -> Self {
        // First we sort the tasks by lower-bound
        let mut sorted_tasks = tasks.to_vec();
        sorted_tasks.sort_by_key(|task| context.lower_bound(&task.start_time));

        // Then we keep track of a mapping from the [`LocalId`] to its position in the tree
        let mut mapping = KeyedVec::default();
        for (index, task) in sorted_tasks.iter().enumerate() {
            while mapping.len() <= task.id.index() {
                let _ = mapping.push(usize::MAX);
            }
            mapping[task.id] = index
        }

        // Calculate the number of internal nodes which are required to create the binary tree
        let mut number_of_internal_nodes = 1;
        while number_of_internal_nodes < tasks.len() {
            number_of_internal_nodes <<= 1;
        }
        let nodes = vec![Node::empty(); 2 * number_of_internal_nodes - 1];
        ThetaTree { nodes, mapping }
    }

    /// Returns the earliest completion time of Theta
    pub(super) fn ect(&self) -> i32 {
        pumpkin_assert_simple!(!self.nodes.is_empty());
        self.nodes[0].ect
    }

    /// Returns the sum of processing times of Theta
    pub(super) fn sum_of_processing_times(&self) -> i32 {
        pumpkin_assert_simple!(!self.nodes.is_empty());
        self.nodes[0].sum_of_processing_times
    }

    /// Add the provided task to Theta
    pub(super) fn add<Var: IntegerVariable>(
        &mut self,
        task: &DisjunctiveTask<Var>,
        context: PropagationContext,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = context.lower_bound(&task.start_time) + task.processing_time;

        self.nodes[position] = Node::new(ect, task.processing_time);
        self.upheap(position)
    }

    /// Remove the provided task from Theta
    pub(super) fn remove<Var: IntegerVariable>(&mut self, task: &DisjunctiveTask<Var>) {
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

    /// Calculate the new values for the ancestors of the provided index
    pub(super) fn upheap(&mut self, mut index: usize) {
        while index != 0 {
            // First we get the parent of the current index
            let parent = Self::get_parent(index);
            // Then we get the children of this parent
            let left_child_parent = Self::get_left_child_index(parent);
            let right_child_parent = Self::get_right_child_index(parent);
            pumpkin_assert_simple!(
                left_child_parent == index || right_child_parent == index,
                "Either the left or the right child should be equal to the provided index"
            );

            // Then we update the values according to the formula
            self.nodes[parent].sum_of_processing_times = self.nodes[left_child_parent]
                .sum_of_processing_times
                + self.nodes[right_child_parent].sum_of_processing_times;
            self.nodes[parent].ect = max(
                self.nodes[right_child_parent].ect,
                self.nodes[left_child_parent].ect
                    + self.nodes[right_child_parent].sum_of_processing_times,
            );

            index = parent;
        }
    }
}
