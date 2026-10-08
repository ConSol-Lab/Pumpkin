use pumpkin_core::propagation::LocalId;
use pumpkin_core::state::State;

use crate::disjunctive::theta_lambda_tree::DisjunctiveTask;
use crate::propagators::disjunctive::theta_lambda_tree::Node;
use crate::propagators::disjunctive::theta_lambda_tree::ThetaLambdaTree;

#[test]
fn tree_built_correctly() {
    let mut state = State::default();
    let a = state.new_interval_variable(0, 0, None);
    let b = state.new_interval_variable(25, 25, None);
    let c = state.new_interval_variable(30, 30, None);
    let d = state.new_interval_variable(32, 32, None);
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

    let mut tree = ThetaLambdaTree::new(&tasks);

    tree.update(state.get_domains());
    for task in tasks.iter() {
        tree.add_to_theta(task, state.get_domains());
    }
    tree.remove_from_theta(&tasks[2]);
    tree.add_to_lambda(&tasks[2], state.get_domains());

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
