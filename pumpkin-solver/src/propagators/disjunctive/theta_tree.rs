use std::cmp::max;
use std::rc::Rc;

use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::ReadDomains;
use crate::propagators::Task;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

#[derive(Debug, Clone)]
struct Node {
    ect: i32,
    sum_of_processing_times: i32,
}

impl Node {
    fn empty() -> Self {
        Self {
            ect: i32::MIN,
            sum_of_processing_times: 0,
        }
    }

    fn new(ect: i32, sum_of_processing_times: i32) -> Self {
        Self {
            ect,
            sum_of_processing_times,
        }
    }
}

#[allow(dead_code, reason = "Will be part of the public API")]
#[derive(Debug)]
pub(super) struct ThetaTree {
    nodes: Vec<Node>,
    mapping: KeyedVec<LocalId, usize>,
}

#[allow(dead_code, reason = "Will be part of the public API")]
impl ThetaTree {
    pub(super) fn new<Var: IntegerVariable>(
        tasks: &[Rc<Task<Var>>],
        context: PropagationContext,
    ) -> Self {
        let mut sorted_tasks = tasks.to_vec();
        sorted_tasks.sort_by_key(|task| context.lower_bound(&task.start_variable));

        let mut mapping = KeyedVec::default();
        for (index, task) in sorted_tasks.iter().enumerate() {
            while mapping.len() <= task.id.index() {
                let _ = mapping.push(usize::MAX);
            }
            mapping[task.id] = index
        }

        let mut number_of_internal_nodes = 1;
        while number_of_internal_nodes < tasks.len() {
            number_of_internal_nodes <<= 1;
        }
        let nodes = vec![Node::empty(); 2 * number_of_internal_nodes - 1];
        ThetaTree { nodes, mapping }
    }

    pub(super) fn ect(&self) -> i32 {
        pumpkin_assert_simple!(!self.nodes.is_empty());
        self.nodes[0].ect
    }

    pub(super) fn sum_of_processing_times(&self) -> i32 {
        pumpkin_assert_simple!(!self.nodes.is_empty());
        self.nodes[0].sum_of_processing_times
    }

    pub(super) fn add<Var: IntegerVariable>(
        &mut self,
        task: &Rc<Task<Var>>,
        context: PropagationContext,
    ) {
        // We need to find the leaf node index; note that there are |nodes| / 2 leaves
        let position = self.nodes.len() / 2 + self.mapping[task.id];
        let ect = context.lower_bound(&task.start_variable) + task.processing_time;

        self.nodes[position] = Node::new(ect, task.processing_time);
        self.upheap(position)
    }

    pub(super) fn remove<Var: IntegerVariable>(&mut self, task: &Rc<Task<Var>>) {
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

            index = parent;
        }
    }
}
