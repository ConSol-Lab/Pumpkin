use core::slice;
use std::collections::HashMap;
use std::hash::Hash;

use crate::constraints::Constraint;
use crate::ffi;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MddNode {
    layer: usize,
    index: usize,
}

impl MddNode {
    pub fn source() -> MddNode {
        MddNode { layer: 0, index: 0 }
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)] // TODO Remove when start inspecting the graph
pub struct MddEdge {
    from: MddNode,
    to: MddNode,
    value: i32,
}

#[derive(Debug)]
pub struct MddGraph<VariableKey: Eq + Hash + Clone> {
    pub layers: Vec<VariableKey>,
    pub transitions: Vec<MddEdge>,
    pub sink: MddNode,
}

impl<VariableKey: Eq + Hash + Clone> Default for MddGraph<VariableKey> {
    fn default() -> Self {
        Self {
            layers: Default::default(),
            transitions: Default::default(),
            sink: MddNode::source(),
        }
    }
}

pub struct MddBuilder<VariableKey: Eq + Hash + Clone> {
    haddock_handle: *mut ffi::HaddockHandle,
    mdd_handle: *mut ffi::MddHandle,
    variables: HashMap<VariableKey, *const ffi::HaddockVarHandle>,
    pointers: HashMap<*const ffi::HaddockVarHandle, VariableKey>,
}

impl<VariableKey: Eq + Hash + Clone> Drop for MddBuilder<VariableKey> {
    fn drop(&mut self) {
        unsafe {
            ffi::release_mdd(self.mdd_handle);
            ffi::release_haddock(self.haddock_handle);
        }
    }
}

#[derive(Debug)]
pub struct MddConstructionError;
impl std::fmt::Display for MddConstructionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MDD construction error")
    }
}

impl std::error::Error for MddConstructionError {}

impl<VariableKey: Eq + Hash + Clone> MddBuilder<VariableKey> {
    pub fn new(width: usize) -> Self {
        let haddock_handle = unsafe { ffi::init_haddock() };
        let mdd_handle = unsafe { ffi::init_mdd(haddock_handle, width) };
        Self {
            haddock_handle,
            mdd_handle,
            variables: Default::default(),
            pointers: Default::default(),
        }
    }

    pub fn add_variable(mut self, var_id: VariableKey, lb: i32, ub: i32) -> Self {
        let var_handle = unsafe { ffi::add_variable(self.haddock_handle, lb, ub) };
        if !self.variables.contains_key(&var_id) {
            self.variables.insert(var_id.clone(), var_handle);
            self.pointers.insert(var_handle, var_id);
        }
        self
    }

    pub fn add_constraint(
        self,
        constraint: Constraint<VariableKey>,
    ) -> Result<Self, MddConstructionError> {
        match constraint {
            Constraint::LinearInequality {
                linear_expr,
                lower_bound,
                upper_bound,
            } => self.add_linear(
                linear_expr,
                lower_bound.unwrap_or(i32::MIN),
                upper_bound.unwrap_or(i32::MAX),
            ),
            Constraint::AllDifferent(vars) => self.add_all_different(vars),
        }
    }

    pub fn build(self) -> Result<MddGraph<VariableKey>, MddConstructionError> {
        let mut graph = MddGraph::<VariableKey>::default();
        let ffi_graph = unsafe { ffi::post_mdd(self.haddock_handle, self.mdd_handle) };
        let ffi_vars =
            unsafe { slice::from_raw_parts(ffi_graph.variables, ffi_graph.n_variables as usize) };
        for ffi_var in ffi_vars {
            if let Some(key) = self.find_pointer(*ffi_var) {
                graph.layers.push(key.clone());
            } else {
                return Err(MddConstructionError);
            }
        }
        let ffi_transitions =
            unsafe { slice::from_raw_parts(ffi_graph.edges, ffi_graph.n_edges as usize) };
        for ffi_trans in ffi_transitions {
            graph.transitions.push(MddEdge {
                from: MddNode {
                    layer: ffi_trans.from.layer as usize,
                    index: ffi_trans.from.node_index as usize,
                },
                to: MddNode {
                    layer: ffi_trans.to.layer as usize,
                    index: ffi_trans.to.node_index as usize,
                },
                value: ffi_trans.value,
            });
        }
        graph.sink = MddNode {
            layer: ffi_graph.sink.layer as usize,
            index: ffi_graph.sink.node_index as usize,
        };
        Ok(graph)
    }

    fn add_linear(
        self,
        expr: Vec<(VariableKey, i32)>,
        lb: i32,
        ub: i32,
    ) -> Result<Self, MddConstructionError> {
        let mut vars = vec![];
        let mut weights = vec![];
        for (key, weight) in expr {
            if let Some(ptr) = self.find_variable(&key) {
                vars.push(ptr);
                weights.push(weight);
            } else {
                return Err(MddConstructionError);
            }
        }
        unsafe {
            ffi::impose_linear(
                self.haddock_handle,
                self.mdd_handle,
                vars.as_ptr(),
                weights.as_ptr(),
                vars.len(),
                lb,
                ub,
            );
        }
        Ok(self)
    }

    fn add_all_different(self, expr: Vec<VariableKey>) -> Result<Self, MddConstructionError> {
        let mut vars = vec![];
        for key in expr {
            if let Some(ptr) = self.find_variable(&key) {
                vars.push(ptr);
            } else {
                return Err(MddConstructionError);
            }
        }
        unsafe {
            ffi::impose_alldiff(
                self.haddock_handle,
                self.mdd_handle,
                vars.as_ptr(),
                vars.len(),
            );
        }
        Ok(self)
    }

    fn find_variable(&self, var_id: &VariableKey) -> Option<*const ffi::HaddockVarHandle> {
        self.variables.get(var_id).copied()
    }

    fn find_pointer(&self, ptr: *const ffi::HaddockVarHandle) -> Option<&VariableKey> {
        self.pointers.get(&ptr)
    }
}

#[cfg(test)]
mod tests {
    // TODO I am yet to figure out a good way to test the conditions of the form
    // TODO "a non-solution is not a path;" unlike the opposite condition,
    // TODO they *should*---but not necessarily *will*!---fail.
    // TODO Ideally, this should be some sort of a soft failure or warning,
    // TODO but I do not know how to do this neatly within `cargo test`.
    use super::*;

    /// Returns `true` if the input solution corresponds to a path in the given MDD
    fn trace<VariableKey: Eq + Hash + Clone>(
        mdd: &MddGraph<VariableKey>,
        sol: &HashMap<VariableKey, i32>,
    ) -> bool {
        let mut node = MddNode::source();
        while node.layer < mdd.layers.len() {
            let var = &mdd.layers[node.layer];
            let val = match sol.get(var) {
                Some(&val) => val,
                None => {
                    return false;
                }
            };
            match mdd
                .transitions
                .iter()
                .find(|edge| edge.from == node && edge.value == val)
            {
                Some(edge) => {
                    node = edge.to;
                }
                None => {
                    return false;
                }
            };
        }
        node == mdd.sink
    }

    #[test]
    fn add_variables_usize() {
        let builder = MddBuilder::<usize>::new(128)
            .add_variable(0, 0, 5)
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5);
        assert_eq!(builder.variables.len(), 4);
        assert_eq!(builder.pointers.len(), 4);
        let mut keys = builder.variables.keys().cloned().collect::<Vec<_>>();
        keys.sort();
        assert_eq!(keys, vec![0, 1, 2, 3]);
    }

    #[test]
    fn add_variables_str() {
        let builder = MddBuilder::<String>::new(128)
            .add_variable("D".to_string(), -1, 1)
            .add_variable("C".to_string(), -2, 2)
            .add_variable("B".to_string(), -3, 3)
            .add_variable("A".to_string(), -4, 4);
        assert_eq!(builder.variables.len(), 4);
        assert_eq!(builder.pointers.len(), 4);
        let mut keys = builder.variables.keys().cloned().collect::<Vec<_>>();
        keys.sort();
        assert_eq!(keys, vec!["A", "B", "C", "D"]);
    }

    #[test]
    fn small_linear() {
        let mdd = MddBuilder::<usize>::new(128)
            // Decision variables: 0 <= x1, ..., x5 <= 5
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5)
            .add_variable(4, 0, 5)
            .add_variable(5, 0, 5)
            // Relaxed constraint: 18 <= x1 + 2*x2 + ... + 5*x5 <= 19,
            .add_constraint(Constraint::<usize>::double_inequality(
                vec![(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)],
                18,
                19,
            ))
            .expect("Failed to impose the constraint")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (5, 4, 0, 0, 1): 5 + 2 * 4 + 5 * 1 = 18
        let sol = HashMap::from_iter(vec![(1, 5), (2, 4), (3, 0), (4, 0), (5, 1)]);
        assert!(trace(&mdd, &sol), "(5, 4, 0, 0, 1) is not in the MDD");
        // - (0, 2, 0, 0, 3): 2 * 2 + 5 * 3 = 19
        let sol = HashMap::from_iter(vec![(1, 0), (2, 2), (3, 0), (4, 0), (5, 3)]);
        assert!(trace(&mdd, &sol), "(0, 2, 0, 0, 3) is not in the MDD");
        // - (1, 0, 3, 2, 0): 1 + 3 * 3 + 4 * 2 = 18
        let sol = HashMap::from_iter(vec![(1, 1), (2, 0), (3, 3), (4, 2), (5, 0)]);
        assert!(trace(&mdd, &sol), "(1, 0, 3, 2, 0) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (0, 0, 0, 0, 0): 0 < 18
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 5)]);
        assert!(!trace(&mdd, &sol), "(0, 0, 0, 0, 0) is in the MDD");
        // - (0, 0, 3, 4, 0): 3 * 3 + 4 * 4 = 25 > 19
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 3), (4, 4), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(0, 0, 3, 4, 0) is in the MDD");
    }

    #[test]
    fn small_lt() {
        let mdd = MddBuilder::<usize>::new(128)
            // Decision variables: 0 <= x1, ..., x5 <= 5
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5)
            .add_variable(4, 0, 5)
            .add_variable(5, 0, 5)
            // Relaxed constraint: x1 + 2*x2 + ... + 5*x5 <= 19
            .add_constraint(Constraint::<usize>::less_than_or_equals(
                vec![(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)],
                19,
            ))
            .expect("Failed to impose the constraint")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (0, 0, 0, 0, 0): 0
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]);
        assert!(trace(&mdd, &sol), "(0, 0, 0, 0, 0) is not in the MDD");
        // - (0, 0, 0, 0, 2): 2 * 5 = 10
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 2)]);
        assert!(trace(&mdd, &sol), "(0, 0, 0, 0, 0) is not in the MDD");
        // - (5, 4, 0, 0, 1): 5 + 2 * 4 + 5 * 1 = 18
        let sol = HashMap::from_iter(vec![(1, 5), (2, 4), (3, 0), (4, 0), (5, 1)]);
        assert!(trace(&mdd, &sol), "(5, 4, 0, 0, 1) is not in the MDD");
        // - (0, 2, 0, 0, 3): 2 * 2 + 5 * 3 = 19
        let sol = HashMap::from_iter(vec![(1, 0), (2, 2), (3, 0), (4, 0), (5, 3)]);
        assert!(trace(&mdd, &sol), "(0, 2, 0, 0, 3) is not in the MDD");
        // - (1, 0, 3, 2, 0): 1 + 3 * 3 + 4 * 2 = 18
        let sol = HashMap::from_iter(vec![(1, 1), (2, 0), (3, 3), (4, 2), (5, 0)]);
        assert!(trace(&mdd, &sol), "(1, 0, 3, 2, 0) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (0, 0, 0, 0, 5): 5 * 5 = 25 > 19
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 5)]);
        assert!(!trace(&mdd, &sol), "(0, 0, 0, 0, 5) is in the MDD");
        // - (0, 0, 3, 4, 0): 3 * 3 + 4 * 4 = 25 > 19
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 3), (4, 4), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(0, 0, 3, 4, 0) is in the MDD");
    }

    #[test]
    fn small_lt_tight() {
        let mdd = MddBuilder::<usize>::new(128)
            // Decision variables: 0 <= x1, ..., x5 <= 5
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5)
            .add_variable(4, 0, 5)
            .add_variable(5, 0, 5)
            // Relaxed constraint: x1 + 2*x2 + ... + 5*x5 <= 4
            .add_constraint(Constraint::<usize>::less_than_or_equals(
                vec![(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)],
                4,
            ))
            .expect("Failed to impose the constraint")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (0, 0, 0, 0, 0): 0
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]);
        assert!(trace(&mdd, &sol), "(0, 0, 0, 0, 0) is not in the MDD");
        // - (0, 0, 0, 1, 0): 4 * 1 = 4
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 1), (5, 0)]);
        assert!(trace(&mdd, &sol), "(0, 0, 0, 1, 0) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (0, 0, 0, 0, 1): 5 * 1 = 5 > 4
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 1)]);
        assert!(!trace(&mdd, &sol), "(0, 0, 0, 0, 1) is in the MDD");
        // - (5, 0, 0, 0, 0): 1 * 5 = 5 > 4
        let sol = HashMap::from_iter(vec![(1, 5), (2, 0), (3, 0), (4, 0), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(5, 0, 0, 0, 0) is in the MDD");
    }

    #[test]
    fn small_gt() {
        let mdd = MddBuilder::<usize>::new(128)
            // Decision variables: 0 <= x1, ..., x5 <= 5
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5)
            .add_variable(4, 0, 5)
            .add_variable(5, 0, 5)
            // Relaxed constraint: x1 + 2*x2 + ... + 5*x5 >= 18
            .add_constraint(Constraint::<usize>::greater_than_or_equals(
                vec![(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)],
                19,
            ))
            .expect("Failed to impose the constraint")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (0, 0, 0, 0, 5): 5 * 5 = 25
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 5)]);
        assert!(trace(&mdd, &sol), "(0, 0, 0, 0, 5) is not in the MDD");
        // - (0, 0, 0, 4, 2): 4 * 4 + 5 * 2 = 26
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 4), (5, 2)]);
        assert!(trace(&mdd, &sol), "(0, 0, 0, 4, 2) is not in the MDD");
        // - (5, 4, 0, 0, 2): 5 + 2 * 4 + 5 * 2 = 23
        let sol = HashMap::from_iter(vec![(1, 5), (2, 4), (3, 0), (4, 0), (5, 2)]);
        assert!(trace(&mdd, &sol), "(5, 4, 0, 0, 1) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (0, 0, 0, 0, 0): 0 < 18
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(0, 0, 0, 0, 0) is in the MDD");
        // - (1, 0, 0, 0, 3): 1 * 1 + 3 * 5 = 16 < 18
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 3)]);
        assert!(!trace(&mdd, &sol), "(1, 0, 0, 0, 3) is in the MDD");
    }

    #[test]
    fn small_eq() {
        let mdd = MddBuilder::<usize>::new(128)
            // Decision variables: 0 <= x1, ..., x5 <= 5
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5)
            .add_variable(4, 0, 5)
            .add_variable(5, 0, 5)
            // Relaxed constraint: x1 + 2*x2 + ... + 5*x5 == 11
            .add_constraint(Constraint::<usize>::equals(
                vec![(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)],
                11,
            ))
            .expect("Failed to impose the constraint")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (1, 0, 0, 0, 2): 1 * 1 + 5 * 2 = 11
        let sol = HashMap::from_iter(vec![(1, 1), (2, 0), (3, 0), (4, 0), (5, 2)]);
        assert!(trace(&mdd, &sol), "(1, 0, 0, 0, 2) is not in the MDD");
        // - (3, 2, 0, 1, 0): 1 * 3 + 2 * 2 + 4 * 1 = 11
        let sol = HashMap::from_iter(vec![(1, 3), (2, 2), (3, 0), (4, 1), (5, 0)]);
        assert!(trace(&mdd, &sol), "(3, 2, 0, 1, 0) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (0, 5, 0, 0, 0): 2 * 5 = 10 <> 11
        let sol = HashMap::from_iter(vec![(1, 0), (2, 5), (3, 0), (4, 0), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(0, 5, 0, 0, 0) is in the MDD");
        // - (1, 0, 0, 0, 3): 1 * 1 + 3 * 5 = 16 <> 10
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 3)]);
        assert!(!trace(&mdd, &sol), "(1, 0, 0, 0, 3) is in the MDD");
    }

    #[test]
    fn two_equalities_sat() {
        let mdd = MddBuilder::<usize>::new(128)
            // Decision variables: 0 <= x1, ..., x5 <= 5
            .add_variable(1, 0, 5)
            .add_variable(2, 0, 5)
            .add_variable(3, 0, 5)
            .add_variable(4, 0, 5)
            .add_variable(5, 0, 5)
            // Relaxed constraint #1: x1 + 2*x2 + ... + 5*x5 == 11
            .add_constraint(Constraint::<usize>::equals(
                vec![(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)],
                11,
            ))
            .expect("Failed to impose constraint #1")
            // Relaxed constraint #2: x1 + x2 == 1
            .add_constraint(Constraint::<usize>::equals(vec![(1, 1), (2, 1)], 1))
            .expect("Failed to impose constraint #2")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (1, 0, 0, 0, 2): 1 * 1 + 5 * 2 = 11
        let sol = HashMap::from_iter(vec![(1, 1), (2, 0), (3, 0), (4, 0), (5, 2)]);
        assert!(trace(&mdd, &sol), "(1, 0, 0, 0, 2) is not in the MDD");
        // - (0, 1, 3, 0, 0): 2 * 1 + 3 * 3 = 11
        let sol = HashMap::from_iter(vec![(1, 0), (2, 1), (3, 3), (4, 0), (5, 0)]);
        assert!(trace(&mdd, &sol), "(0, 4, 1, 0, 0) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (0, 4, 1, 0, 0): 2 * 4 + 3 * 1 = 11, 0 + 4 != 1
        let sol = HashMap::from_iter(vec![(1, 0), (2, 4), (3, 1), (4, 0), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(0, 4, 1, 0, 0) is in the MDD");
        // - (1, 0, 0, 0, 0): 1 * 1 <> 1, 1 + 0 = 1
        let sol = HashMap::from_iter(vec![(1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]);
        assert!(!trace(&mdd, &sol), "(1, 0, 0, 0, 0) is in the MDD");
    }

    // TODO The test below falls into an infininte loop; I am out of ideas why that happens,
    // TODO but the Rust side seems to be clean. Is this a bug in Haddock?
    // #[test]
    // fn two_equalities_unsat() {
    //     let mdd = MddBuilder::<usize>::new(128)
    //         // Decision variables: 0 <= x1, ..., x5 <= 5
    //         .add_variable(1, 0, 5)
    //         .add_variable(2, 0, 5)
    //         .add_variable(3, 0, 5)
    //         .add_variable(4, 0, 5)
    //         .add_variable(5, 0, 5)
    //         // Relaxed constraint #1: x2 + x3 == 9
    //         .add_constraint(Constraint::<usize>::equals(
    //             vec![(2, 1), (3, 1)],
    //             9))
    //         .expect("Failed to impose constraint #1")
    //         // Relaxed constraint #2: 10 * x1 + x2 == 13
    //         .add_constraint(Constraint::<usize>::equals(
    //             vec![(1, 10), (2, 1)],
    //             13))
    //         .expect("Failed to impose constraint #2")
    //         .build()
    //         .expect("Failed to obtain the MDD");
    //     assert!(mdd.transitions.is_empty(), "MDD is not empty");
    // }

    #[test]
    fn all_diff_3x3() {
        let mdd = MddBuilder::<usize>::new(256)
            // Decision variables: 1 <= x1, ..., x3 <= 3
            .add_variable(1, 0, 3)
            .add_variable(2, 0, 3)
            .add_variable(3, 0, 3)
            // Relaxed constraint: alldifferent(x1, x2, x3)
            .add_constraint(Constraint::<usize>::all_different(vec![1, 2, 3]))
            .expect("Failed to impose the constraint")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (1, 2, 3)
        let sol = HashMap::from_iter(vec![(1, 1), (2, 2), (3, 3)]);
        assert!(trace(&mdd, &sol), "(1, 2, 3) is not in the MDD");
        // - (3, 2, 1)
        let sol = HashMap::from_iter(vec![(1, 3), (2, 2), (3, 1)]);
        assert!(trace(&mdd, &sol), "(3, 2, 1) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (1, 3, 1)
        let sol = HashMap::from_iter(vec![(1, 1), (2, 3), (3, 1)]);
        assert!(!trace(&mdd, &sol), "(1, 3, 1) is in the MDD");
        // - (2, 2, 1)
        let sol = HashMap::from_iter(vec![(1, 2), (2, 2), (3, 1)]);
        assert!(!trace(&mdd, &sol), "(2, 2, 1) is in the MDD");
    }

    #[test]
    fn van_hoeve() {
        // Example 6, "An Introduction to Decision Diagrams for Optimization" (van Hoeve, 2024)
        let mdd = MddBuilder::<usize>::new(1024)
            // Decision variables: 0 <= x1, ..., x4 <= 4
            .add_variable(1, 0, 4)
            .add_variable(2, 0, 4)
            .add_variable(3, 0, 4)
            .add_variable(4, 0, 4)
            // Relaxed constraint #1: alldifferent(x1, x2, x3, x4)
            .add_constraint(Constraint::<usize>::all_different(vec![1, 2, 3, 4]))
            .expect("Failed to impose constraint #1")
            // Relaxed constraint #2: x1 + x2 + x3 >= 9
            .add_constraint(Constraint::<usize>::greater_than_or_equals(
                vec![(1, 1), (2, 1), (3, 1)],
                9,
            ))
            .expect("Failed to impose constraint #2")
            .build()
            .expect("Failed to obtain the MDD");
        // Test that the following feasible solutions correspond to DD paths:
        // - (2, 3, 4, 1)
        let sol = HashMap::from_iter(vec![(1, 2), (2, 3), (3, 4), (4, 1)]);
        assert!(trace(&mdd, &sol), "(2, 3, 4, 1) is not in the MDD");
        // - (4, 3, 2, 1)
        let sol = HashMap::from_iter(vec![(1, 4), (2, 3), (3, 2), (4, 1)]);
        assert!(trace(&mdd, &sol), "(4, 3, 2, 1) is not in the MDD");
        // Test that the following infeasible solutions do not correspond to a DD path:
        // - (2, 4, 2, 1)
        let sol = HashMap::from_iter(vec![(1, 2), (2, 4), (3, 2), (4, 1)]);
        assert!(!trace(&mdd, &sol), "(2, 4, 2, 1) is in the MDD");
        // - (3, 3, 2, 1)
        let sol = HashMap::from_iter(vec![(1, 3), (2, 3), (3, 2), (4, 1)]);
        assert!(!trace(&mdd, &sol), "(3, 3, 2, 1) is in the MDD");
        // - (1, 2, 3, 4)
        let sol = HashMap::from_iter(vec![(1, 1), (2, 2), (3, 3), (4, 4)]);
        assert!(!trace(&mdd, &sol), "(1, 2, 3, 4) is in the MDD");
    }
}
