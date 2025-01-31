use std::cmp::max;

use super::disjunctive_task::DisjunctiveTask;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::ReadDomains;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Node {
    ect: i32,
    sum_of_processing_times: i32,
    ect_bar: i32,
    sum_of_processing_times_bar: i32,
}

impl Node {
    fn empty() -> Self {
        Self {
            ect: i32::MIN,
            sum_of_processing_times: 0,
            ect_bar: i32::MIN,
            sum_of_processing_times_bar: 0,
        }
    }

    fn new_white_node(ect: i32, sum_of_processing_times: i32) -> Self {
        Self {
            ect,
            sum_of_processing_times,
            ect_bar: ect,
            sum_of_processing_times_bar: sum_of_processing_times,
        }
    }

    fn new_gray_node(ect: i32, sum_of_processing_times: i32) -> Self {
        Self {
            ect: i32::MIN,
            sum_of_processing_times: 0,
            ect_bar: ect,
            sum_of_processing_times_bar: sum_of_processing_times,
        }
    }
}

#[derive(Debug)]
pub(super) struct ThetaLambdaTree {
    pub(super) nodes: Vec<Node>,
    mapping: KeyedVec<LocalId, usize>,
    reverse_mapping: KeyedVec<usize, LocalId>,
    number_of_internal_nodes: usize,
}

impl ThetaLambdaTree {
    pub(super) fn new<Var: IntegerVariable>(
        tasks: &[DisjunctiveTask<Var>],
        context: PropagationContext,
    ) -> Self {
        let mut sorted_tasks = tasks.to_vec();
        sorted_tasks.sort_by_key(|task| context.lower_bound(&task.start_variable));

        let mut mapping = KeyedVec::default();
        let mut reverse_mapping = KeyedVec::default();
        for (index, task) in sorted_tasks.iter().enumerate() {
            while mapping.len() <= task.id.index() {
                let _ = mapping.push(usize::MAX);
            }
            mapping[task.id] = index;
            let _ = reverse_mapping.push(task.id);
        }

        let mut number_of_internal_nodes = 1;
        while number_of_internal_nodes < tasks.len() {
            number_of_internal_nodes <<= 1;
        }

        let nodes = vec![Node::empty(); 2 * number_of_internal_nodes - 1];

        ThetaLambdaTree {
            nodes,
            mapping,
            reverse_mapping,
            number_of_internal_nodes: number_of_internal_nodes - 1,
        }
    }

    pub(super) fn ect(&self) -> i32 {
        pumpkin_assert_simple!(!self.nodes.is_empty());
        self.nodes[0].ect
    }

    pub(super) fn ect_bar(&self) -> i32 {
        self.nodes[0].ect_bar
    }

    fn get_leaf_node_index(&self, index: usize) -> usize {
        index - self.number_of_internal_nodes
    }

    pub(super) fn responsible_ect(&self) -> LocalId {
        let index = self
            .responsible_index_ect_internal(0)
            .expect("Expected to be able to get responsible gray task for ect");
        self.reverse_mapping[self.get_leaf_node_index(index)]
    }

    fn responsible_index_ect_internal(&self, position: usize) -> Option<usize> {
        if self.is_leaf(position) {
            // Assuming that all tasks have non-zero processing time
            (self.nodes[position].sum_of_processing_times_bar > 0).then_some(position)
        } else {
            let left_child = Self::get_left_child_index(position);
            let right_child = Self::get_right_child_index(position);

            if self.nodes[right_child] != Node::empty()
                && self.nodes[position].ect_bar == self.nodes[right_child].ect_bar
            {
                self.responsible_index_ect_internal(right_child)
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
                self.responsible_index_ect_internal(left_child)
            } else {
                None
            }
        }
    }

    fn responsible_index_p_internal(&self, position: usize) -> Option<usize> {
        if self.is_leaf(position) {
            // Assuming that all tasks have non-zero processing time
            (self.nodes[position].sum_of_processing_times_bar > 0).then_some(position)
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

    pub(super) fn add_to_lambda<Var: IntegerVariable>(
        &mut self,
        task: &DisjunctiveTask<Var>,
        context: PropagationContext,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = context.lower_bound(&task.start_variable) + task.processing_time;

        self.nodes[position] = Node::new_gray_node(ect, task.processing_time);
        self.upheap(position);
    }

    pub(super) fn remove_from_lambda<Var: IntegerVariable>(&mut self, task: &DisjunctiveTask<Var>) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        self.nodes[position] = Node::empty();
        self.upheap(position)
    }

    pub(super) fn add_to_theta<Var: IntegerVariable>(
        &mut self,
        task: &DisjunctiveTask<Var>,
        context: PropagationContext,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = context.lower_bound(&task.start_variable) + task.processing_time;

        self.nodes[position] = Node::new_white_node(ect, task.processing_time);
        self.upheap(position)
    }

    pub(super) fn remove_from_theta<Var: IntegerVariable>(&mut self, task: &DisjunctiveTask<Var>) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        self.nodes[position] = Node::empty();
        self.upheap(position)
    }

    fn get_left_child_index(index: usize) -> usize {
        2 * index + 1
    }

    fn get_right_child_index(index: usize) -> usize {
        2 * index + 2
    }

    fn get_parent(index: usize) -> usize {
        pumpkin_assert_simple!(index > 0);
        (index - 1) / 2
    }

    fn is_leaf(&self, index: usize) -> bool {
        pumpkin_assert_simple!(index < self.nodes.len());
        Self::get_left_child_index(index) >= self.nodes.len()
    }

    pub(super) fn upheap(&mut self, mut index: usize) {
        while index != 0 {
            let parent = Self::get_parent(index);
            let left_child_parent = Self::get_left_child_index(parent);
            let right_child_parent = Self::get_right_child_index(parent);
            pumpkin_assert_simple!(left_child_parent == index || right_child_parent == index);

            self.nodes[parent].sum_of_processing_times = self.nodes[left_child_parent]
                .sum_of_processing_times
                + self.nodes[right_child_parent].sum_of_processing_times;
            self.nodes[parent].ect = max(
                self.nodes[right_child_parent].ect,
                self.nodes[left_child_parent].ect
                    + self.nodes[right_child_parent].sum_of_processing_times,
            );

            self.nodes[parent].sum_of_processing_times_bar = max(
                self.nodes[left_child_parent].sum_of_processing_times_bar
                    + self.nodes[right_child_parent].sum_of_processing_times,
                self.nodes[left_child_parent].sum_of_processing_times
                    + self.nodes[right_child_parent].sum_of_processing_times_bar,
            );
            self.nodes[parent].ect_bar = max(
                self.nodes[right_child_parent].ect_bar,
                max(
                    self.nodes[left_child_parent].ect
                        + self.nodes[right_child_parent].sum_of_processing_times_bar,
                    self.nodes[left_child_parent].ect_bar
                        + self.nodes[right_child_parent].sum_of_processing_times,
                ),
            );

            index = parent;
        }
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
                start_variable: a,
                processing_time: 5,
                id: LocalId::from(0),
            },
            DisjunctiveTask {
                start_variable: b,
                processing_time: 9,
                id: LocalId::from(1),
            },
            DisjunctiveTask {
                start_variable: c,
                processing_time: 5,
                id: LocalId::from(2),
            },
            DisjunctiveTask {
                start_variable: d,
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
