use flatzinc::predicates;

use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::{LocalId, PropagationContextMut, Propagator, PropagatorInitialisationContext};
use crate::engine::variables::IntegerVariable;
use std::collections::HashSet;
use std::vec;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use log::debug;

/// Propagator for the inverse constraint.
/// 
/// This propagator enforces the relationship between two arrays of integer variables (`lhs` and `rhs`),
/// such that `lhs[i] = j` implies `rhs[j] = i`, and vice versa.
#[derive(Clone, Debug)]
pub(crate) struct InversePropagator<VA, VB> {
    lhs: Vec<VA>,
    rhs: Vec<VB>,
}

impl<VA, VB> InversePropagator<VA, VB> {
    pub(crate) fn new(lhs: Vec<VA>, rhs: Vec<VB>) -> Self {
        Self { lhs, rhs }
    }
}

impl<VA: IntegerVariable + 'static, VB: IntegerVariable + 'static> Propagator
    for InversePropagator<VA, VB>
{
    fn name(&self) -> &str {
        "Inverse"
    }


    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        for (i, var) in self.lhs.iter().enumerate() {
            //debug!("I am adding var with id {i} to lhs");
            context.register(var.clone(), DomainEvents::ANY_INT, LocalId::from(i.try_into().unwrap()));
        }
        for (i, var) in self.rhs.iter().enumerate() {
            //debug!("I am adding var with id {i} to rhs");
            context.register(var.clone(), DomainEvents::ANY_INT, LocalId::from((self.lhs.len() + i).try_into().unwrap()));
        }
        Ok(())
    }


    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let n = self.lhs.len(); // dynamically determine size
        // enforce arc consistency
        for i in 0..n {
            let lhs_values: Vec<_> = context.iterate_domain(&self.lhs[i]).collect();
            for value in lhs_values {
                // remove illegal vals
                // if value < 1 || value > n as i32 {
                //     context.remove(&self.lhs[i], value, conjunction!([self.lhs[i] != value]))?;
                //     continue;
                // }
                // if lhs[i] = j rhs[j] must include i
                let is_in_rhs = context
                    .iterate_domain(&self.rhs[(value-1) as usize])
                    .any(|v| v == (i+1) as i32);
                if !is_in_rhs {
                    context.remove(&self.lhs[i], value, conjunction!([self.rhs[(value-1) as usize] != (i +1) as i32]))?;
                }
            }
            //debug!("I made it through lhs");
            let rhs_values: Vec<_> = context.iterate_domain(&self.rhs[i]).collect();
            for value in rhs_values {
                // remove illegal vals
                // if value < 1 || value > n as i32 {
                //     context.remove(&self.rhs[i], value, conjunction!([self.rhs[i] != value]))?;
                //     continue;
                // }
                // if lhs[i] = j rhs[j] must include i
                let is_in_lhs = context
                    .iterate_domain(&self.lhs[(value-1) as usize])
                    .any(|v| v == (i+1) as i32);
                if !is_in_lhs {
                    context.remove(&self.rhs[i], value, conjunction!([self.lhs[(value-1) as usize] != (i +1) as i32]))?;
                }
            }
            //debug!("I made it through rhs");
        }
        // debug!("In lhs the vars:");
        // for i in 0..n {
        //     debug!("   {i} have domains containing:");
        //     let lhs_values: Vec<_> = context.iterate_domain(&self.lhs[i]).collect();
        //     for value in lhs_values {
        //         let val = value - 1; 
        //         debug!("      {val}");
        //     }
        // }
        // debug!("In rhs the vars:");
        // for i in 0..n {
        //     debug!("   {i} have domains containing:");
        //     let rhs_values: Vec<_> = context.iterate_domain(&self.rhs[i]).collect();
        //     for value in rhs_values {
        //         let val = value - 1; 
        //         debug!("      {val}");
        //     }
        // }
        let mut graph = vec![vec![]; 2 * n];
        for i in 0..n {
            for value in context.iterate_domain(&self.lhs[i]) {
                let j = (value-1) as usize;
                graph[i].push(n + j);
                graph[n + j].push(i);
            }
        }


        // Compute maximum matching
        let matching = maximum_matching(&graph, n);
        if matching.iter().any(|x| x.is_none()) {
            let (d1, c1, a1, d2, c2, a2) = dm_decomposition(&graph, &matching, n);
            //debug!("i did dm decomp");
            let mut predicates_res: Vec<Predicate> = Vec::new();
            // predicates_res.push(predicate!(self.lhs[0] != (n+1) as i32));


            //------------------------------------------------------------------------------//
            //******************************************************************************//
            //******************************NAIVE EXPLANATION*******************************//
            //******************************************************************************//
            //------------------------------------------------------------------------------//
            // debug!("{:?}", &graph);

            // for var in 0..n{
            //     let domain_values: HashSet<i32> = context.iterate_domain(&self.lhs[var]).collect();
            //     let ub = context.upper_bound(&self.lhs[var]);
            //     let lb = context.lower_bound(&self.lhs[var]);
            //     if ub == lb {
            //         debug!("upper and lower are equal");
            //         //predicates_res.push(predicate!(self.lhs[var] != lb));
            //         //continue;
            //     }
            //     predicates_res.push(predicate!(self.lhs[var] >= lb));
            //     predicates_res.push(predicate!(self.lhs[var] <= ub));
            //     for value in lb..(ub+1){
            //         if !domain_values.contains(&(value)) && value >= lb && value <= ub{
            //             predicates_res.push(predicate!(self.lhs[var] != value));
            //         }
            //     }
            // }
            // for var in 0..n{
            //     let domain_values: HashSet<i32> = context.iterate_domain(&self.rhs[var]).collect();
            //     let ub = context.upper_bound(&self.rhs[var]);
            //     let lb = context.lower_bound(&self.rhs[var]);
            //     if ub == lb {
            //         debug!("upper and lower are equal");
            //         //predicates_res.push(predicate!(self.rhs[var] != lb));
            //         //continue;
            //     }
            //     predicates_res.push(predicate!(self.rhs[var] >= lb));
            //     predicates_res.push(predicate!(self.rhs[var] <= ub));
            //     for value in lb..(ub+1){
            //         if !domain_values.contains(&(value)) && value >= lb && value <= ub{
            //             predicates_res.push(predicate!(self.rhs[var] != value));
            //         }
            //     }
            // }















            for var in &d1 {
                let var1 = var.clone();
                if var1 <= n {
                    let domain_values: HashSet<i32> = context.iterate_domain(&self.lhs[var1]).collect();
                    let ub = context.upper_bound(&self.lhs[var1]);
                    let lb = context.lower_bound(&self.lhs[var1]);
                    predicates_res.push(predicate!(self.lhs[var1] >= lb));
                    predicates_res.push(predicate!(self.lhs[var1] <= ub));
                    for value in lb..(ub+1){
                        if !domain_values.contains(&(value)) && value >= lb && value <= ub{
                            predicates_res.push(predicate!(self.lhs[var1] != value));
                        }
                    }
                }
            }
            // for var in &d2 {
            //     debug!("{var}");
            //     let var1 = var-n;
            //     if var1 <= n {
            //         let domain_values: HashSet<i32> = context.iterate_domain(&self.rhs[var1]).collect();
            //         let ub = context.upper_bound(&self.rhs[var1]);
            //         let lb = context.lower_bound(&self.rhs[var1]);
            //         if ub == lb {
            //             debug!("upper and lower are equal");
            //             predicates_res.push(predicate!(self.rhs[var1] != lb));
            //             continue;
            //         }
            //         predicates_res.push(predicate!(self.rhs[var1] >= lb));
            //         predicates_res.push(predicate!(self.rhs[var1] <= ub));
            //         for value in 0..n as i32 {
            //             if !domain_values.contains(&(value+1)) && value+1 >= lb && value+1 <= ub{
            //                 predicates_res.push(predicate!(self.rhs[var1] != value+1));
            //             }
            //         }
            //     }
            // }
            // debug!("i added preds for d1");
            // // Add predicates for c1
            // debug!("c1 contains the following");
            for var in c1 {
                if var <= n {
                    let domain_values: HashSet<i32> = context.iterate_domain(&self.lhs[var]).collect();
                    for value in a2.iter().chain(d2.iter()).cloned() {
                        if value >= n && !domain_values.contains(&((value - n + 1) as i32)) {
                            predicates_res.push(predicate!(self.lhs[var] != (value - n + 1) as i32));
                        }
                    }
                }
            }

            // for var in c2 {
            //     let var1 = var-n;
            //     if var1 <= n {
            //         let domain_values: HashSet<i32> = context.iterate_domain(&self.rhs[var1]).collect();
            //         for value in a1.iter().chain(d1.iter()).cloned() {
            //             if value >= n && !domain_values.contains(&((value + 1) as i32)) {
            //                 //println!("index lhs with: {}",var);
            //                 //println!("does not contain: {}",value-n);
            //                 predicates_res.push(predicate!(self.rhs[var1] != (value + 1) as i32));
            //             }
            //         }
            //     }
            // }
            //debug!("{:?}",predicates_res);
            // Return failure with combined predicates
            return Err(PropositionalConjunction::new(predicates_res).into());
        }
        Ok(())
    }    
}

fn maximum_matching(graph: &[Vec<usize>], n: usize) -> Vec<Option<usize>> {
    let mut pair_u = vec![None; n];
    let mut pair_v = vec![None; n];
    let mut visited = vec![false; n];

    fn dfs(
        u: usize,
        graph: &[Vec<usize>],
        pair_u: &mut [Option<usize>],
        pair_v: &mut [Option<usize>],
        visited: &mut [bool],
        n: usize,
    ) -> bool {
        if visited[u] {
            return false;
        }
        visited[u] = true;

        for &v in &graph[u] {
            let right_index = v - n;
            if pair_v[right_index].is_none() || dfs(pair_v[right_index].unwrap(), graph, pair_u, pair_v, visited, n) {
                pair_u[u] = Some(v);
                pair_v[right_index] = Some(u);
                return true;
            }
        }

        false
    }

    for u in 0..n {
        visited.fill(false);
        dfs(u, graph, &mut pair_u, &mut pair_v, &mut visited, n);
    }
    [pair_u, pair_v].concat()
}

fn alternating_bfs(
    graph: &[Vec<usize>],
    exposed_vertices: &[usize],
    matching: &[Option<usize>],
    n: usize,
) -> (HashSet<usize>, HashSet<usize>) {
    //debug!("{:?}", graph);
    use std::collections::{HashSet, VecDeque};

    let mut d1 = HashSet::new(); // even level set
    let mut a2 = HashSet::new(); // odd level set
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    // initialize the queue with exposed vertices
    //debug!("exposed vertices are:");
    for &v in exposed_vertices {
        //debug!("{v}");
        queue.push_back((v, 0)); // (vertex, level)
        visited.insert(v);
    }

    while let Some((current, level)) = queue.pop_front() {
        
        if level % 2 == 0 {
            d1.insert(current);
        } else {
            a2.insert(current);
        }

        for &neighbor in &graph[current] {
            if visited.contains(&neighbor) {
                continue;
            }

            if level % 2 == 0 && matching[current] != Some(neighbor) {
                // even level: unmatched edges
                queue.push_back((neighbor, level + 1));
                visited.insert(neighbor);
            } else if level % 2 == 1 &&  matching[neighbor] == Some(current) {
                // odd level: matched edges
                queue.push_back((neighbor, level + 1));
                visited.insert(neighbor);
            }
        }
    }

    (d1, a2)
}

fn dm_decomposition(
    graph: &[Vec<usize>],
    matching: &[Option<usize>],
    n: usize,
) -> (HashSet<usize>, HashSet<usize>, HashSet<usize>, HashSet<usize>, HashSet<usize>, HashSet<usize>) {
    let mut exposed_left: Vec<usize> = Vec::new();
    let mut exposed_right: Vec<usize> = Vec::new();

    for i in 0..n {
        if matching[i].is_none() {
            exposed_left.push(i);
        }
    }

    for i in 0..n {
        if !matching.iter().any(|&v| v == Some(n + i)) {
            exposed_right.push(n + i);
        }
    }

    let (d1, a2) = alternating_bfs(graph, &exposed_left, matching, n);
    let (d2, a1) = alternating_bfs(graph, &exposed_right, matching, n);

    let v1: HashSet<_> = (0..n).collect();
    let v2: HashSet<_> = (n..2 * n).collect();

    let c1: HashSet<_> = v1.difference(&d1).cloned().collect::<HashSet<_>>()
                           .difference(&a1).cloned().collect();
    let c2: HashSet<_> = v2.difference(&d2).cloned().collect::<HashSet<_>>()
                           .difference(&a2).cloned().collect();

    (d1, c1, a1, d2, c2, a2)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::propagators::inverse::InversePropagator;
    use crate::engine::variables::DomainId; 
    use crate::engine::test_solver::TestSolver;

    #[test]
    fn test_inverse_propagator_simple() {
        let mut solver = TestSolver::default();

        const N: usize = 3; 
        let lhs: [DomainId; N] = [solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1)];
        let rhs: [DomainId; N] = [solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1)];

        let propagator = InversePropagator::new(lhs.to_vec(), rhs.to_vec());
        let propagator_id = solver.new_propagator(propagator).unwrap();

        solver.propagate_until_fixed_point(propagator_id).unwrap();
        for i in 0..N {
            for j in 0..N {
                let contains_lhs = lhs[i].contains(&solver.assignments, (j+1) as i32);
                let contains_rhs = rhs[j].contains(&solver.assignments, (i+1) as i32);
                assert_eq!(
                    contains_lhs, contains_rhs,
                    "Initial consistency failed: lhs[{i}] = {j} and rhs[{j}] = {i}"
                );
            }
        }
    }


    #[test]
    fn test_inverse_propagator_removal() {
        let mut solver = TestSolver::default();

        const N: usize = 3; 
        let lhs: [DomainId; N] = [solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1)];
        let rhs: [DomainId; N] = [solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1),
                                solver.new_variable(1, (N+1) as i32 - 1)];

        let propagator = InversePropagator::new(lhs.to_vec(), rhs.to_vec());
        let propagator_id = solver.new_propagator(propagator).unwrap();

        lhs[0].remove(&mut solver.assignments, 2, None).unwrap();
        solver.propagate_until_fixed_point(propagator_id).unwrap();

        assert!(
            !rhs[1].contains(&solver.assignments, 1),
            "rhs[2] should not contain 0 after removal from lhs[0]"
        );
    }

    #[test]
    fn test_inverse_propagator_bounds() {
        let mut solver = TestSolver::default();

        const N: usize = 5;
        let lhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];
        let rhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];

        let propagator = InversePropagator::new(lhs.to_vec(), rhs.to_vec());
        let propagator_id = solver.new_propagator(propagator).unwrap();

        lhs[0].set_lower_bound(&mut solver.assignments, 3, None).unwrap();
        solver.propagate_until_fixed_point(propagator_id).unwrap();

        assert_eq!(
            rhs[3].lower_bound(&solver.assignments),
            0,
            "Lower bound of rhs[3] should be 0 after propagation"
        );
        assert_eq!(
            rhs[3].upper_bound(&solver.assignments),
            N as i32 - 1,
            "Upper bound of rhs[3] should be {} after propagation",
            N - 1
        );
    }

    #[test]
    fn test_inverse_propagator_rhs_empty_domain() {
        let mut solver = TestSolver::default();

        const N: usize = 3;
        let lhs: [DomainId; N] = [
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
        ];
        let rhs: [DomainId; N] = [
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
        ];
    
        let propagator = InversePropagator::new(lhs.to_vec(), rhs.to_vec());
        let propagator_id = solver.new_propagator(propagator).unwrap();

        lhs[0].remove(&mut solver.assignments, 1, None).unwrap();
        lhs[1].remove(&mut solver.assignments, 1, None).unwrap();
        lhs[2].remove(&mut solver.assignments, 1, None).unwrap();

        let result = solver.propagate(propagator_id);

        assert!(
            result.is_err(),
            "Propagator should detect inconsistency due to empty domain in rhs[1]"
        );
    }



    //MAXIMUM MATCHING TESTING
    #[test]
    fn test_maximum_matching_full_matching() {
        let graph = vec![
            vec![3, 4],
            vec![3, 5],
            vec![4],
        ];
        let n = 3;

        let matching = maximum_matching(&graph, n);

        assert_eq!(matching.len(), n*2);
        assert_eq!(matching[0], Some(3));
        assert_eq!(matching[1], Some(5));
        assert_eq!(matching[2], Some(4));
        assert_eq!(matching[3], Some(0));
        assert_eq!(matching[5], Some(1));
        assert_eq!(matching[4], Some(2));
    }

    #[test]
    fn test_maximum_matching_partial_matching() {
        let graph = vec![
            vec![3],
            vec![4],
            vec![],
        ];
        let n = 3;

        let matching = maximum_matching(&graph, n);

        assert_eq!(matching.len(), n*2);
        assert_eq!(matching[0], Some(3));
        assert_eq!(matching[1], Some(4));
        assert!(matching[2].is_none());
        assert_eq!(matching[3], Some(0));
        assert_eq!(matching[4], Some(1));
        assert!(matching[5].is_none());
    }

    #[test]
    fn test_maximum_matching_no_edges() {
        let graph = vec![vec![], vec![], vec![]];
        let n = 3;

        let matching = maximum_matching(&graph, n);

        assert_eq!(matching, vec![None, None, None, None, None, None]);
    }

    #[test]
    fn test_maximum_matching_cycle_graph() {
        let graph = vec![
            vec![3],
            vec![4],
            vec![5],
        ];
        let n = 3;

        let matching = maximum_matching(&graph, n);

        assert_eq!(matching.len(), n*2);
        assert_eq!(matching[0], Some(3));
        assert_eq!(matching[1], Some(4));
        assert_eq!(matching[2], Some(5));
        assert_eq!(matching[3], Some(0));
        assert_eq!(matching[4], Some(1));
        assert_eq!(matching[5], Some(2));
    }

    #[test]
    fn test_maximum_matching_multiple_vals() {
        let graph = vec![
            vec![3],
            vec![4],
            vec![4],
        ];
        let n = 3;

        let matching = maximum_matching(&graph, n);

        assert_eq!(matching.len(), n*2);
        assert_eq!(matching[0],Some(3));
        assert_eq!(matching[3],Some(0));
        assert!((matching[1] == Some(4) && matching[4] == Some(1))|| (matching[2] == Some(4) && matching[4] == Some(2)));
        assert!(matching[1].is_none() || matching[2].is_none());
        assert_ne!(matching[1], matching[2])
    }



    //DM DECOMPOSITION TESTING
    fn create_hashset(vec: Vec<usize>) -> HashSet<usize> {
        vec.into_iter().collect()
    }

    #[test]
    fn test_disconnected_graph() {
        let graph = vec![vec![], vec![], vec![], vec![]];
        let matching = vec![None, None, None, None];
        let n = 2;

        let (d1, c1, a1, d2, c2, a2) = dm_decomposition(&graph, &matching, n);

        assert_eq!(d1, create_hashset(vec![0, 1]));
        assert_eq!(c1, create_hashset(vec![]));
        assert_eq!(a1, create_hashset(vec![]));
        assert_eq!(d2, create_hashset(vec![2, 3]));
        assert_eq!(c2, create_hashset(vec![]));
        assert_eq!(a2, create_hashset(vec![]));
    }

    #[test]
    fn test_fully_connected_graph() {
        let graph = vec![vec![2, 3], vec![2, 3], vec![0, 1], vec![0, 1]];
        let matching = vec![Some(2), Some(3), Some(0), Some(1)];
        let n = 2;

        let (d1, c1, a1, d2, c2, a2) = dm_decomposition(&graph, &matching, n);

        assert_eq!(d1, create_hashset(vec![]));
        assert_eq!(c1, create_hashset(vec![0, 1]));
        assert_eq!(a1, create_hashset(vec![]));
        assert_eq!(d2, create_hashset(vec![]));
        assert_eq!(c2, create_hashset(vec![2, 3]));
        assert_eq!(a2, create_hashset(vec![]));
    }

    #[test]
    fn test_partial_matching() {
        let graph = vec![vec![5], vec![5], vec![5, 6], vec![6, 7], vec![6,7,8], vec![0,1,2], vec![2,3,4], vec![3,4], vec![4],vec![4]];
        let n = 5;
        let matching = maximum_matching(&graph, n);
        let (d1, c1, a1, d2, c2, a2) = dm_decomposition(&graph, &matching, n);

        assert_eq!(d1, create_hashset(vec![0,1]));
        assert_eq!(c1, create_hashset(vec![2,3]));
        assert_eq!(a1, create_hashset(vec![4]));
        assert_eq!(d2, create_hashset(vec![8,9]));
        assert_eq!(c2, create_hashset(vec![6,7]));
        assert_eq!(a2, create_hashset(vec![5]));
    }

    
    //TEST FULL PROPOGATION
    #[test]
    fn test_partial_matching_propogation_error() {
        let mut solver = TestSolver::default();

        const N: usize = 5;
        let lhs: [DomainId; N] = [
            solver.new_variable(0, 0),
            solver.new_variable(0, 0),
            solver.new_variable(0, 1),
            solver.new_variable(1, 2),
            solver.new_variable(1, 4),
        ];
        let rhs: [DomainId; N] = [
            solver.new_variable(0, 2),
            solver.new_variable(2, 3),
            solver.new_variable(3, 4),
            solver.new_variable(4, 4),
            solver.new_variable(4, 4),
        ];
    
        let propagator = InversePropagator::new(lhs.to_vec(), rhs.to_vec());
        let propagator_id = solver.new_propagator(propagator).unwrap();

        let result = solver.propagate(propagator_id);
        let mut predicates_res = Vec::<Predicate>::new();
        assert_eq!(result,Err(PropositionalConjunction::new(predicates_res).into()))

    }
}
