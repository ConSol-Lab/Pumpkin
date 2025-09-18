//use env_logger::init;

use crate::basic_types::PropagationStatusCP;
use crate::predicate;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::variables::IntegerVariable;
use crate::basic_types::PropositionalConjunction;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::declare_inference_label;
use crate::basic_types::PropagatorConflict;


use crate::containers::{HashSet, HashMap};
use std::collections::BTreeMap;

declare_inference_label!(AllDifferent);

/// The [`PropagatorConstructor`] for the [`AllDifferentPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct AllDifferentPropagatorArgs<Var> {
    /// The variables
    pub(crate) vars: Box<[Var]>,
    /// The constraint tag of the constraint this propagator is propagating for.
    pub(crate) constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for AllDifferentPropagatorArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = AllDifferentPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let AllDifferentPropagatorArgs {
            vars,
            constraint_tag,
        } = self;

        for (i, x_i) in vars.iter().enumerate() {
            context.register(x_i.clone(), DomainEvents::ANY_INT, LocalId::from(i as u32));
        }

        let propagator = AllDifferentPropagator {
            vars,
            inference_code: context.create_inference_code(constraint_tag, AllDifferent),
        };

        propagator
    }
}



#[derive(Clone, Debug)]
pub(crate) struct AllDifferentPropagator<Var> {
    vars: Box<[Var]>,
    inference_code: InferenceCode,
}

#[derive(Clone, Debug)]
pub(crate) struct Node {
    id: usize,
    neighbours: Vec<usize>,
    value: i32
}

impl <Var: IntegerVariable + 'static> Propagator 
    for AllDifferentPropagator <Var>
{
    fn name(&self) -> &str {
        "AllDiff"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        //first make a graph (list of nodes)
        let mut initial_graph: Vec<Node> = Vec::new();
        let mut values:Vec<i32>  = Vec::new();
        //map from each value to their id in the graph
        let mut val_map: HashMap<i32, usize> = HashMap::default();

        //node for each variable; will have the same id and value
        //value will be useful for the value nodes
        let mut val_id = self.vars.len();
        self.vars.iter().enumerate().for_each(|(i, x_i)| {
            let lower = context.lower_bound(x_i);
            let upper = context.upper_bound(x_i);
            let mut neighbours: Vec<usize> = Vec::new();
            for j in lower..=upper {
                if(context.contains( x_i, j)){
                    if !values.contains(&j) {
                        val_map.insert(j, val_id);
                        val_id +=1;
                        values.push(j);
                    }
                    match val_map.get(&j) {
                        Some(v) => neighbours.push(*v),
                        None => println!("should never happen"),
                    }
                }
            }
            initial_graph.push(Node{
                id: i,
                neighbours: neighbours,
                value: i as i32, 
            });
        });

        //node with empty neighbours list for each value; id will start from vars.len()
        val_id = self.vars.len();
        for &(v) in &values {
            initial_graph.push(Node{
                id: val_id,
                neighbours: Vec::new(),
                value: v,
            });
            val_id += 1;
        }

        //find a matching in the graph
        let no_vars = self.vars.len();
        let mut is_matched: Vec<bool> = vec![false; no_vars];
        let matches = Self::find_match(&initial_graph, no_vars, &mut is_matched);
        let residual = Self::build_residual_graph(&initial_graph, no_vars, &matches, &mut is_matched);
        //if there are any variables not part of the matching, there is no feasible solution
        if is_matched.contains(&false){
            let index = is_matched.iter().position(|&r| r == false).unwrap();
            //explanation for conflict includes all variables reachable from a variable not part of the matching
            let mut fail_reason: PropositionalConjunction = PropositionalConjunction::new(Vec::new());
            let rev_graph = Self::reverse_graph(&residual);
            let xi = &self.vars[index];
            fail_reason.add(predicate!(xi >= context.lower_bound(xi)));
            fail_reason.add(predicate!(xi <= context.upper_bound(xi)));
            for i in context.lower_bound(xi)..context.upper_bound(xi) {
                if !context.contains( xi, i) {
                    fail_reason.add(predicate![xi != i]);
                }
            }
            fail_reason = Self::add_predicates_from_node(&rev_graph, &index, &mut fail_reason, &mut context, no_vars, &self.vars);
            return Err(PropagatorConflict {
                conjunction: fail_reason,
                inference_code: self.inference_code,
            }.into());
        }
        
        let var_set: HashSet<usize> = (0..no_vars).collect();
        Self::find_sccs_remove_values(&residual, &var_set, context, &self.vars, self.inference_code)
    }
}

impl<Var: IntegerVariable + 'static> AllDifferentPropagator<Var> {
    ///here are some helper functions for the propagator

    ///finds a maximal matching in a given bipartite graph, where the first set has no_vars elements
    ///returns the matching as a list l, l[i] being the index of the matched element for each of the no_vars elements
    fn find_match(graph: & Vec<Node>, no_vars: usize, is_matched: &mut Vec<bool>) -> Vec<i32> {
        let mut flow_graph = graph.clone();
        //add source and sink to create the flow graph from the bipartite one
        let source = Node{
            id: graph.len(),
            neighbours: (0..no_vars).collect(),
            value: -1,
        };
        let sink = Node {
            id: graph.len() + 1,
            neighbours: Vec::new(),
            value: -1,
        };
        for i in no_vars..graph.len(){
            flow_graph[i].neighbours.push(graph.len()+1);
        }
        flow_graph.push(source); flow_graph.push(sink);

        let mut visited: Vec<bool> = vec![false; flow_graph.len()];
        let mut path: Vec<usize> = Vec::new();
        //Ford-Fulkerson with capacity 1 on all edges
        while Self::find_augmenting_path(&flow_graph, graph.len(), graph.len()+1, &mut visited, &mut path){
            for pair in path.windows(2) {
                let from = pair[0];
                let to = pair[1];
                //remove the "to" node from the "from" node's neighbors
                if let Some(pos) = flow_graph[from].neighbours.iter().position(|&x| x == to) {
                    flow_graph[from].neighbours.remove(pos);
                }
                //add the "from" node as a neighbor of the "to" node (if not already there)
                if !flow_graph[to].neighbours.contains(&from) {
                    flow_graph[to].neighbours.push(from);
                }
            }
            visited = vec![false; flow_graph.len()];
            path = Vec::new();
        }
        //get the matching from the flow graph
        let mut matched_values: Vec<i32> = vec![-1; no_vars];
        for i in no_vars..graph.len() {
            if let Some(&val) = flow_graph[i].neighbours.iter().find(|&&x| x < no_vars) {
                matched_values[val] = flow_graph[i].value;
                is_matched[val] = true;
            }
        }        

        matched_values
    }

    ///finds an augmenting path in a flow graph using DFS
    fn find_augmenting_path(graph: &Vec<Node>, current: usize, sink: usize, visited: &mut Vec<bool>, path: &mut Vec<usize>) -> bool {
        if visited[current]{
            return false;
        }
        visited[current] = true;
        path.push(current);
        if current == sink {
            return true;
        }
        for &neighbor in graph[current].neighbours.iter() {
            if Self::find_augmenting_path(graph, neighbor, sink, visited, path) {
                return true;
            }
        }
        path.pop();
        false
    }

    //returns the residual graph from a matching
    fn build_residual_graph(graph: &Vec<Node>, no_vars: usize, matches: &Vec<i32>, is_matched: &mut Vec<bool>) -> Vec<Node> {
        let n = graph.len();
        let sink = n;
    
        let mut nodes: Vec<Node> = Vec::new();
    
        for node in graph.iter() {
            nodes.push(Node {
                id: node.id,
                neighbours: Vec::new(),
                value: node.value,
            });
        }
    
        //sink node
        nodes.push(Node {
            id: sink,
            neighbours: Vec::new(),
            value: 0,
        });
    
        //map value to node id
        //all values should be distinct
        let mut value_to_node: HashMap<i32, usize> = HashMap::default();
        for node in graph.iter().skip(no_vars) {
            value_to_node.insert(node.value, node.id);
        }
    
        //edge from each variable to the value it was matched with
        let mut value_matched_to_var: HashMap<usize, usize> = HashMap::default(); // value_node_id -> variable_id
        for (var_id, &matched_value) in matches.iter().enumerate() {
            if is_matched[var_id] != false {
                if let Some(&value_node_id) = value_to_node.get(&matched_value) {
                    nodes[var_id].neighbours.push(value_node_id);
                    value_matched_to_var.insert(value_node_id, var_id);
                }
            }
        }
    
        //edge from each value to each of the variables it was not matched to
        for (&value, &value_node_id) in &value_to_node {
            for var_id in 0..no_vars {
                let matched_value = matches[var_id];
                let original_neighbours = &graph[var_id].neighbours;
    
                if matched_value != value && original_neighbours.contains(&value_node_id) {
                    nodes[value_node_id].neighbours.push(var_id);
                }
            }
        }
    
        //sink node
        for (&value, &value_node_id) in &value_to_node {
            if value_matched_to_var.contains_key(&value_node_id) {
                //edge from matched value to sink
                nodes[value_node_id].neighbours.push(sink);
            } else {
                //edge from sink to unmatched value
                nodes[sink].neighbours.push(value_node_id);
            }
        }
    
        nodes
    }
    
    //given a graph and ids of variables, finds strongly connected components, then prunes edges that are in different components
    //and not part of the matching
    pub(crate) fn find_sccs_remove_values(graph: &Vec<Node>, var_set: &HashSet<usize>, mut context: PropagationContextMut, self_vars: &Box<[Var]>, inference_code:InferenceCode) -> PropagationStatusCP{
        let mut visited: HashSet<usize> = HashSet::default();
        let mut tstack = Vec::new();
        let mut dfs_num: HashMap<usize, usize> = HashMap::default();
        let mut low_link = HashMap::default();
        let mut index = 0;
        let mut has_scc_split = false;
        let mut to_prune:BTreeMap<usize, Vec<usize>>  = BTreeMap::default();
        let mut what_scc: Vec<usize> = vec![0; graph.len()];
        let mut scc_no = 0;
    
        for &xi in var_set {
            if !visited.contains(&xi) {
                Self::tarjan_remove_values(
                    &graph,
                    xi,
                    &mut visited,
                    &mut tstack,
                    &mut dfs_num,
                    &mut low_link,
                    &mut index,
                    &mut has_scc_split,
                    var_set.len(),
                    &mut to_prune,
                    &mut what_scc,
                    &mut scc_no,
                );
            }
        }

        let mut rev_graph = Self::reverse_graph(&graph);
        //this prunes without taking order into account
        Self::prune_traversal(&to_prune, &mut rev_graph, context, var_set.len(), &self_vars, &what_scc, inference_code)
        //this prunes with order taken into account to make explanations more general; not fully functioning yet
        //Self::prune_ordered(&to_prune, &what_scc, context, var_set.len(), &self_vars, &mut rev_graph, inference_code)
    }

    ///propagates the pruning of variable-value pairs in to_prune, taking into account the order such that explanations are more general
    fn prune_ordered(to_prune: &BTreeMap<usize, Vec<usize>>, what_scc: & Vec<usize>, mut context: PropagationContextMut,
        no_var: usize, self_vars: &Box<[Var]>, graph: &mut Vec<Node>, inference_code: InferenceCode) -> PropagationStatusCP{ //needs to return PropagationStatusCP
        //map with key as the number of the scc and value as a list of the variables in that scc
        let mut scc_map: HashMap<usize, Vec<usize>> = HashMap::default();
        for (node, &scc_id) in what_scc.iter().enumerate().take(no_var) {
            scc_map.entry(scc_id).or_insert_with(Vec::new).push(node);
        }

        //make a graph using scc as the nodes and edges to prune as the edges, to then topologically sort
        let mut scc_prune_graph: HashMap<usize, HashSet<usize>> = HashMap::default();
        let mut indegree: HashMap<usize, usize> = HashMap::default();

        for i in 0..what_scc.len(){
            indegree.entry(what_scc[i]).or_insert(0);
        }

        for (&from_node, neighbors) in to_prune {
            let from_scc = what_scc[from_node];
            for &to_node in neighbors {
                let to_scc = what_scc[to_node];
                if from_scc != to_scc {
                    *indegree.entry(from_scc).or_insert(0) += 1;
                    scc_prune_graph.entry(to_scc).or_default().insert(from_scc);
                }
            }
        }

        let mut queue: Vec<usize> = indegree.iter()
        .filter_map(|(&node, &deg)| if deg == 0 { Some(node) } else { None })
        .collect();
        
        let empty_list = Vec::new();
        while let Some(current_scc) = queue.pop(){
            let nodes_prune = scc_map.get(&current_scc).unwrap_or(&empty_list);
            for xi in nodes_prune{
                //xi is the variable we are pruning for; to_prune[xi] is the list of what edges to prune
                for value in to_prune.get(&xi).unwrap_or(&empty_list){
                    //needed variables for the explanation is the variables in the scc of edge
                    //let needed = scc_map.get(&what_scc[*value]).unwrap_or(&empty_list);

                    let mut explanation: PropositionalConjunction = PropositionalConjunction::new(Vec::new());
                    println!("{:?}", graph);
                    println!("{:?}", xi);
                    explanation = Self::add_predicates_from_node(graph, value, &mut explanation, &mut context, no_var, self_vars);
                    context.post(
                        predicate![
                            self_vars[*xi]
                            != graph[*value].value
                                .try_into()
                                .expect("Expected to be able to fit i64 into i32")
                            ],
                        explanation,
                        inference_code,
                    )?;
                    if let Some(pos) = graph[*xi].neighbours.iter().position(|&x| x == *value) {
                        graph[*xi].neighbours.remove(pos);
                    }
                }
            }

            if let Some(neighbors) = scc_prune_graph.get(&current_scc) {
                for &neighbor_scc in neighbors {
                    if let Some(entry) = indegree.get_mut(&neighbor_scc) {
                        *entry -= 1; // decrement indegree

                        // If indegree becomes 0, we can process this SCC next
                        if *entry == 0 {
                            queue.push(neighbor_scc);
                        }
                    }
                }
            }
        }
        Ok(())

    }

    ///propagates the pruning of variable-value pairs in to_prune
    fn prune_traversal(to_prune: &BTreeMap<usize, Vec<usize>>, graph: &mut Vec<Node>, 
        mut context: PropagationContextMut, no_var: usize, self_vars: &Box<[Var]>, what_scc: & Vec<usize>,
        inference_code: InferenceCode) -> PropagationStatusCP{
        for (xi, vals) in to_prune.iter(){
            for value in vals.iter(){
                let mut explanation: PropositionalConjunction = PropositionalConjunction::new(Vec::new());
                //explanation should contain variables reachable from the node value of the variable-value being pruned
                explanation = Self::add_predicates_from_node(graph, value, &mut explanation, &mut context, no_var, self_vars);
                context.post(
                        predicate![
                            self_vars[*xi]
                            != graph[*value].value
                                .try_into()
                                .expect("Expected to be able to fit i64 into i32")
                            ],
                        explanation,
                        inference_code,
                    )?;
                //remove pruned value from the graph
                if let Some(pos) = graph[*xi].neighbours.iter().position(|&x| x == *value) {
                    graph[*xi].neighbours.remove(pos);
                }

            }
        }
        Ok(())
    }

    //adds to explanation all the predicates for variables that are reachable from the initial_node
    fn add_predicates_from_node(graph: &Vec<Node>, initial_node: &usize, explanation: &mut PropositionalConjunction,
        context: &mut PropagationContextMut, no_var: usize, self_vars: &Box<[Var]>) -> PropositionalConjunction{
        let mut needed: HashSet<usize> = HashSet::default();
        let mut stack = vec![*initial_node];
        //add to the hashset needed of variables all those reached in a traversal
        while let Some(node) = stack.pop() {
            if needed.insert(node) {
                let neighbours = &graph[node].neighbours;
                for &neigh in neighbours.iter() {
                    if !needed.contains(&neigh){
                        stack.push(neigh);
                    }
                }
            }
        }
        //store the predicates from the needed variables in a vector
        let mut new_predicates: Vec<Predicate> = Vec::new();
        for &node in needed.iter(){
            if graph[node].id >= no_var || node == *initial_node {
                continue;
            }
            let variable: &Var = &self_vars[node];
            new_predicates.push(predicate![variable >= context.lower_bound(variable)]);
            new_predicates.push(predicate![variable <= context.upper_bound(variable)]);
            for i in context.lower_bound(variable)..context.upper_bound(variable) {
                if !context.contains( variable, i) {
                    new_predicates.push(predicate![variable != i]);
                }
            }
        }
        let new_explanation = explanation.clone().extend_and_remove_duplicates(new_predicates.into_iter());
        new_explanation
    }

    ///returns a reversed graph of the given one
    fn reverse_graph(graph: &Vec<Node>) -> Vec<Node> {
        let mut nodes: Vec<Node> = Vec::new();
        //println!("{:?}", graph);
        for node in graph.iter() {
            nodes.push(Node {
                id: node.id,
                neighbours: Vec::new(),
                value: node.value,
            });
        }
        for node in graph.iter() {
            for &neigh in node.neighbours.iter() {
                nodes[neigh].neighbours.push(node.id);
            }
        }
        nodes
    }
    
    //actual tarjan algorithms that finds SCC and prunes values
    fn tarjan_remove_values(
        graph: &Vec<Node>,
        curnode: usize,
        visited: &mut HashSet<usize>,
        tstack: &mut Vec<usize>,
        dfs_num: &mut HashMap<usize, usize>,
        low_link: &mut HashMap<usize, usize>,
        index: &mut usize,
        has_scc_split: &mut bool,
        no_vars: usize,
        to_prune: &mut BTreeMap<usize, Vec<usize>>,
        what_scc: &mut Vec<usize>,
        scc_no: &mut usize,
    ) {
        tstack.push(curnode);
        dfs_num.insert(curnode, *index);
        low_link.insert(curnode, *index);
        *index += 1;
        visited.insert(curnode);
    
        for &neighbor in &graph[curnode].neighbours {
            if !visited.contains(&neighbor) {
                Self::tarjan_remove_values(graph, neighbor, visited, tstack, dfs_num, low_link, index, 
                    has_scc_split, no_vars, to_prune, what_scc, scc_no);
                let lowlink_cur = low_link[&curnode];
                let lowlink_neigh = low_link[&neighbor];
                low_link.insert(curnode, lowlink_cur.min(lowlink_neigh));
            } else if tstack.contains(&neighbor) {
                let lowlink_cur = low_link[&curnode];
                let dfsnum_neigh = dfs_num[&neighbor];
                low_link.insert(curnode, lowlink_cur.min(dfsnum_neigh));
                *has_scc_split = true;
            }
        }
    
        //if node is root - entire component found
        if low_link[&curnode] == dfs_num[&curnode] {
            *scc_no +=1;
            let mut scc: HashSet<usize> = HashSet::default();
            while let Some(top) = tstack.pop() {
                scc.insert(top);
                if top == curnode {
                    break;
                }
            }
            //prune edges
            for &xi in &scc {
                what_scc[xi] = *scc_no;
                if xi < graph.len() { 
                    for &e in &graph[xi].neighbours {
                        if !scc.contains(&e) {
                            //xi is the value, e the variable; ensure e is not the sink, but a variable, and that xi is a value
                            if graph[e].id != graph.len()-1 && graph[xi].id >= no_vars {
                                //store the variable-value pairs that need to be pruned to do so later
                                match to_prune.get_mut(&e) {
                                    Some(l) => {
                                        l.push(xi);
                                    },
                                    None => {
                                        to_prune.insert(e, vec![xi]);
                                    },
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_types::Inconsistency;
    use crate::conjunction;
    use crate::engine::reason;
    use crate::engine::test_solver::TestSolver;
    use crate::engine::variables::TransformableVariable;

    #[test]
    fn test_easy() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 3);
        let y = solver.new_variable(1, 1);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [x, y].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        solver.assert_bounds(x, 2, 3);
    }

    #[test]
    fn test_explanation_easy() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 3);
        let y = solver.new_variable(1, 1);
        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [x, y].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //let propagator = solver.new_propagator(AllDifferentPropagator::new([x, y])).expect("non-empty domain");
        //solver.propagate(propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![x != 1]);
        assert_eq!(conjunction!([y >= 1] & [y <= 1]), reason);
    }

    #[test]
    fn test_2scc(){
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 2);
        let b = solver.new_variable(1, 3);
        let c = solver.new_variable(3, 5);
        let d = solver.new_variable(3, 5);
        let e = solver.new_variable(3, 5);
        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [a, b, c, d, e].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //let propagator = solver.new_propagator(AllDifferentPropagator::new([a, b, c, d, e])).expect("non-empty domain");
        //solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(b, 1, 2);
    }

    #[test]
    fn test_2scc_explanation(){
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 2);
        let b = solver.new_variable(1, 3);
        let c = solver.new_variable(3, 5);
        let d = solver.new_variable(3, 5);
        let e = solver.new_variable(3, 5);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [a, b, c, d, e].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //let propagator = solver.new_propagator(AllDifferentPropagator::new([a, b, c, d, e])).expect("non-empty domain");
        //solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(b, 1, 2);
        let reason1 = solver.get_reason_int(predicate![b != 3]);
        let expected1 = PropositionalConjunction::new(vec![predicate!(c >= 3), predicate!(c <= 5), 
            predicate!(d >= 3), predicate!(d <= 5), predicate!(e >= 3), predicate!(e <= 5)].into());
        assert_eq!(expected1, reason1);
    }

    #[test]
    fn testtt(){
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 1);
        let b = solver.new_variable(1, 2);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [a, b].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //solver.new_propagator(AllDifferentPropagator::new([a,b])).expect("non-empty domain");
        solver.assert_bounds(b, 2, 2);
    }

    #[test]
    fn test_3scc(){
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 2);
        let b = solver.new_variable(1, 2);
        let c = solver.new_variable(2, 3);
        let d = solver.new_variable(3, 5);
        let e = solver.new_variable(4, 6);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [a, b, c, d, e].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //let propagator = solver.new_propagator(AllDifferentPropagator::new([a, b, c, d, e])).expect("non-empty domain");
        //solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(c, 3, 3);
        solver.assert_bounds(d, 4, 5);
    }

    #[test]
    fn test_3scc_explanation(){
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 2);
        let b = solver.new_variable(1, 2);
        let c = solver.new_variable(2, 3);
        let d = solver.new_variable(3, 5);
        let e = solver.new_variable(4, 6);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [a, b, c, d, e].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //let propagator = solver.new_propagator(AllDifferentPropagator::new([a, b, c, d, e])).expect("non-empty domain");

        let reason1 = solver.get_reason_int(predicate![c != 2]);
        let expected1 = PropositionalConjunction::new(vec![predicate!(a >= 1), predicate!(a <= 2), predicate!(b >= 1), predicate!(b <= 2)].into());
        assert_eq!(expected1, reason1);

        //this will be a lot smaller after doing explanations in the correct order
        let reason2 = solver.get_reason_int(predicate![d != 3]);
        let expected2 = PropositionalConjunction::new(vec![predicate!(c >= 3), predicate!(c <= 3)].into());
        assert_eq!(expected2, reason2);
    }

    #[test]
    fn test_3scc_2_explanation(){
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 2);
        let b = solver.new_variable(1, 2);
        let c = solver.new_variable(2, 4);
        let d = solver.new_variable(3, 4);
        let e = solver.new_variable(4, 6);
        let f = solver.new_variable(5, 6);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [a, b, c, d, e, f].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");
        //let propagator = solver.new_propagator(AllDifferentPropagator::new([a, b, c, d, e, f])).expect("non-empty domain");

        let reason1 = solver.get_reason_int(predicate![c != 2]);
        let expected1 = PropositionalConjunction::new(vec![predicate!(a >= 1), predicate!(a <= 2), predicate!(b >= 1), predicate!(b <= 2)].into());
        assert_eq!(expected1, reason1);

        //this will be a lot smaller after doing explanations in the correct order
        let reason2 = solver.get_reason_int(predicate![e != 4]);
        let expected2 = PropositionalConjunction::new(vec![predicate!(c >= 3), predicate!(c <= 4), predicate!(d >= 3), predicate!(d <= 4)].into());
        assert_eq!(expected2, reason2);
    }

    #[test]
    fn testy(){
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 3);
        let y = solver.new_variable(1, 3);
        let z = solver.new_variable(3, 4);
        solver.remove(x, 2);
        solver.remove(y, 2);

        let constraint_tag = solver.new_constraint_tag();
        let propagator = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [x, y, z].into(),
                constraint_tag,
            })
            .expect("non-empty domain");

        solver.propagate(propagator).expect("non-empty domain");

        //let propagator = solver.new_propagator(AllDifferentPropagator::new([x, y, z])).expect("non-empty domain");
        solver.assert_bounds(z, 4, 4);

        let reason = solver.get_reason_int(predicate![z != 3]);
        println!("{:?}", reason);
    }

    #[test]
    fn test_no_match() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 2);
        let y = solver.new_variable(1, 2);
        let z = solver.new_variable(1, 2);

        let constraint_tag = solver.new_constraint_tag();

        let err = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [x, y, z].into(),
                constraint_tag,
            })
            .expect_err("string");

        let expected: PropositionalConjunction = conjunction!([x >= 1] & [x <= 2] & [y >= 1] & [y <= 2] & [z >= 1] & [z <= 2]).into();
        //let err = solver.new_propagator(AllDifferentPropagator::new([x, y, z])).expect_err("string");
        //let expected: Inconsistency = conjunction!([x >= 1] & [x <= 1] & [y >= 1] & [y <= 2] & [z >= 1] & [z <= 2]).into();
        println!("{:?}", expected);
        match err {
            Inconsistency::EmptyDomain => panic!("expected an explicit conflict"),
            Inconsistency::Conflict(conflict) => assert_eq!(expected, conflict.conjunction),
        }
        //assert_eq!(expected, err);
    }

    #[test]
    fn test_conflict() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 2);
        let y = solver.new_variable(1, 2);
        let z = solver.new_variable(1, 2);
        let a = solver.new_variable(3, 4);
        let b = solver.new_variable(3, 4);

        let constraint_tag = solver.new_constraint_tag();

        let err = solver
            .new_propagator(AllDifferentPropagatorArgs {
                vars: [x, y, z, a, b].into(),
                constraint_tag,
            })
            .expect_err("string");

        let expected: PropositionalConjunction = conjunction!([x >= 1] & [x <= 2] & [y >= 1] & [y <= 2] & [z >= 1] & [z <= 2]).into();
        //let err = solver.new_propagator(AllDifferentPropagator::new([x, y, z])).expect_err("string");
        //let expected: Inconsistency = conjunction!([x >= 1] & [x <= 1] & [y >= 1] & [y <= 2] & [z >= 1] & [z <= 2]).into();
        println!("{:?}", expected);
        match err {
            Inconsistency::EmptyDomain => panic!("expected an explicit conflict"),
            Inconsistency::Conflict(conflict) => assert_eq!(expected, conflict.conjunction),
        }
        //assert_eq!(expected, err);
    }

    #[test]
    fn test_costas_array_propagation() {
        let mut solver = TestSolver::default();

        // Costas array variables: 4 variables, domain 1..=4
        let x0 = solver.new_variable(1, 4);
        let x1 = solver.new_variable(1, 4);
        let x2 = solver.new_variable(1, 4);
        let x3 = solver.new_variable(1, 4);

        // Difference variables (from FlatZinc X_INTRODUCED_6_, ..., _16)
        // Domain: {-3, -2, -1, 1, 2, 3}
        let diff_domain = (-3..=3).filter(|&x| x != 0).collect::<Vec<_>>();
        let d1 = solver.new_variable(-3, 3);
        solver.remove(d1, 0); // x1 - x0
        let d2 = solver.new_variable(-3, 3);
        solver.remove(d2, 0); // x2 - x1
        let d3 = solver.new_variable(-3, 3);
        solver.remove(d3, 0); // x1 - x0
        let d4 = solver.new_variable(-3, 3);
        solver.remove(d4, 0); // x2 - x1
        let d5 = solver.new_variable(-3, 3);
        solver.remove(d5, 0); // x1 - x0
        let d6 = solver.new_variable(-3, 3);
        solver.remove(d6, 0); // x2 - x1

        // All-different constraints
        //solver.new_propagator(AllDifferentPropagator::new([x0, x1, x2, x3])).unwrap();
        //solver.new_propagator(AllDifferentPropagator::new([d1, d2, d3])).unwrap();
        //solver.new_propagator(AllDifferentPropagator::new([d4, d5])).unwrap();
        //solver.new_propagator(AllDifferentPropagator::new([d6])).unwrap();

        // Apply any initial propagation
        //solver.propagate_all().unwrap();

        // Debug output
        // println!("x0: {:?}", solver.domain(x0));
        // println!("x1: {:?}", solver.domain(x1));
        // println!("x2: {:?}", solver.domain(x2));
        // println!("x3: {:?}", solver.domain(x3));
        // println!("d1: {:?}", solver.domain(d1));
        // println!("d2: {:?}", solver.domain(d2));
        // println!("d3: {:?}", solver.domain(d3));
        // println!("d4: {:?}", solver.domain(d4));
        // println!("d5: {:?}", solver.domain(d5));
        // println!("d6: {:?}", solver.domain(d6));

        let vars = [x0, x1, x2, x3, d1, d2, d3, d4, d5, d6];

        for var in vars {
        let lb = solver.lower_bound(var);
        let ub = solver.upper_bound(var);
        assert!(
            lb <= ub,
            "Variable {:?} has empty domain: lower_bound={} > upper_bound={}",
            var, lb, ub,
        );
        }

        
    }

}