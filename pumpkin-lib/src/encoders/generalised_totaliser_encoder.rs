use log::debug;

use super::pseudo_boolean_constraint_encoder::EncodingError;
use super::pseudo_boolean_constraint_encoder::PseudoBooleanConstraintEncoderInterface;
use crate::basic_types::HashMap;
use crate::basic_types::WeightedLiteral;
use crate::engine::variables::Literal;
use crate::pumpkin_assert_moderate;
use crate::pumpkin_assert_simple;
use crate::Solver;

/// Implementation of the generalized totalizer encoding for pseudo-boolean constraints.
///
/// # Bibliography
/// \[1] "Generalized totalizer encoding for pseudo-boolean constraints.", Joshi Saurabh, Ruben
/// Martins, Vasco Manquinho; CP '15
#[derive(Debug)]
pub struct GeneralisedTotaliserEncoder {
    index_last_added_weighted_literal: usize,
    layers: Vec<Layer>,
    num_clauses_added: usize,
}

impl PseudoBooleanConstraintEncoderInterface for GeneralisedTotaliserEncoder {
    fn encode_at_most_k(
        mut weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        solver: &mut Solver,
    ) -> Result<Self, EncodingError> {
        // a good heuristic is to sort the literals by weight with a stable ordering
        //  this reduces the size of the encoding significantly
        weighted_literals.sort_by(|p1, p2| p1.weight.cmp(&p2.weight));

        let mut encoder = GeneralisedTotaliserEncoder {
            index_last_added_weighted_literal: usize::MAX,
            layers: vec![],
            num_clauses_added: 0,
        };
        encoder.encode_at_most_k_standard_case(weighted_literals, k, solver);

        Ok(encoder)
    }

    fn strengthen_at_most_k(
        &mut self,
        new_k: u64,
        solver: &mut Solver,
    ) -> Result<(), EncodingError> {
        pumpkin_assert_simple!(self.index_last_added_weighted_literal > 0);
        pumpkin_assert_simple!(
            !self.layers.is_empty() && self.layers.last().unwrap().nodes.len() == 1
        );

        // the literals in each layer are sorted by weight
        //  this is a by-product of the above implementation
        //  this is used by the loop below
        let weighted_literals = &self.layers.last().unwrap().nodes[0];

        self.index_last_added_weighted_literal = weighted_literals.len();

        for i in (0..self.index_last_added_weighted_literal).rev() {
            // forbid all literals that exceed k
            if weighted_literals[i].weight > new_k {
                self.num_clauses_added += 1;
                self.index_last_added_weighted_literal = i;

                if solver.add_clause([!weighted_literals[i].literal]).is_err() {
                    return Err(EncodingError::CannotStrengthen);
                }
            } else {
                // the first time a literal no longer exceeds k, we can stop
                //  since other literals down the line have smaller weights
                break;
            }
        }
        Ok(())
    }
}

impl GeneralisedTotaliserEncoder {
    // The log2 function of Rust is not yet considered stable
    //  so we do a brute force version here
    fn compute_logarithm_based_2(mut num: usize) -> usize {
        pumpkin_assert_simple!(num.is_power_of_two());

        let mut log_value = 0;
        while num > 0 {
            num /= 2;
            log_value += 1;
        }
        log_value
    }

    fn encode_at_most_k_standard_case(
        &mut self,
        weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        solver: &mut Solver,
    ) {
        // the generalised totaliser encoding can be visualised as a binary tree
        //  the leaf nodes are the input literals
        //  each node above the leafs represents the sum of two child nodes, where each partial sum
        // is represented by a new literal  the final/root layer contains all feasible
        // partial sums of the input variables  Note each layer has half of the number of
        // nodes as the previous layer but the number of partial sums (variables) can potentially
        // grow exponentially

        // current layer is the layer that is being used to construct the next layer
        //  the next layer contains the literals that represent the partial sum of the current layer
        self.layers.push(Layer::new());
        // initially, each node in the layer consists of exactly one literal
        self.layers[0].nodes = weighted_literals
            .iter()
            .map(|wl| vec![*wl])
            .collect::<Vec<Vec<WeightedLiteral>>>();

        // these are to be used in the loop below
        //  will be reused to avoid allocating each iteration
        let mut value_to_literal_map: HashMap<u64, Literal> = HashMap::default();
        let mut partial_sums: Vec<u64> = Vec::new();

        //  in each iteration, the literals of the next_layer are created and appropriate clauses
        // are added to capture the partial sums
        for index_current_layer in 0..GeneralisedTotaliserEncoder::compute_logarithm_based_2(
            weighted_literals.len().next_power_of_two(),
        ) {
            self.layers.push(Layer::new());
            let num_nodes_in_current_layer = self.layers[index_current_layer].nodes.len();

            // neighbouring nodes of the current layer are merged and their sum is represented in
            // their parent node that is stored in the next layer  we merge the first
            // and the second node (merge_index = 0), then the third and the fourth node
            // (merge_index = 1), and so on
            for merge_index in 0..num_nodes_in_current_layer / 2 {
                // these are the indicies of the two nodes that will be merged in this step
                let index_node1 = 2 * merge_index;
                let index_node2 = 2 * merge_index + 1;
                // the result of merge the two nodes will be stored in the next layer node
                let mut next_layer_node: Vec<WeightedLiteral> = Vec::new();

                // create new variables and record their indicies as appropriate
                // two stage process: identify partial sums and then create the literals

                //  first compute the necessary partial sums
                partial_sums.clear();

                // collect weights from node1
                for weighted_literal in &self.layers[index_current_layer].nodes[index_node1] {
                    pumpkin_assert_moderate!(weighted_literal.weight <= k);
                    partial_sums.push(weighted_literal.weight);
                }
                // collect weight from node2
                for weighted_literal in &self.layers[index_current_layer].nodes[index_node2] {
                    pumpkin_assert_moderate!(weighted_literal.weight <= k);
                    partial_sums.push(weighted_literal.weight);
                }
                // collect weights that could happen as a result of adding a weight from node1 and a
                // weight from node 2
                for wl1 in &self.layers[index_current_layer].nodes[index_node1] {
                    for wl2 in &self.layers[index_current_layer].nodes[index_node2] {
                        let combined_weight = wl1.weight + wl2.weight;
                        if combined_weight <= k {
                            partial_sums.push(combined_weight);
                        } else {
                            // can break the inner loop, since other values are only getting larger
                            break;
                        }
                    }
                }
                // at this point the vector 'partial sums' contains all partial sums
                //  but it may contain duplicates too
                //  remove the duplicates
                //  note that this is more efficient than using a HashSet
                partial_sums.sort();
                partial_sums.dedup();

                value_to_literal_map.clear();
                //  then create the variables, one for each partial sum, and register the mapping
                // between the partial sum value and the corresponding literal
                for partial_sum in &partial_sums {
                    let literal = solver.new_literal();
                    let _ = value_to_literal_map.insert(*partial_sum, literal);
                    next_layer_node.push(WeightedLiteral {
                        literal,
                        weight: *partial_sum,
                        bound: None,
                    });
                }

                // now perform the merge to define the new variables / summation

                //  define sums of one literal from node1
                //  node1[weight] -> next_layer_node[weight]
                for weighted_literal in &self.layers[index_current_layer].nodes[index_node1] {
                    solver
                        .add_clause(vec![
                            !weighted_literal.literal,
                            *value_to_literal_map.get(&weighted_literal.weight).unwrap(),
                        ])
                        .expect("Adding encoding clause should not lead to conflict");
                    self.num_clauses_added += 1;
                }
                //  define sums of one literal from node2
                //  node2[weight] -> next_layer_node[weight]
                for weighted_literal in &self.layers[index_current_layer].nodes[index_node2] {
                    solver
                        .add_clause(vec![
                            !weighted_literal.literal,
                            *value_to_literal_map.get(&weighted_literal.weight).unwrap(),
                        ])
                        .expect("Adding encoding clause should not lead to conflict");
                    self.num_clauses_added += 1;
                }
                //  define sums could happen as a result of adding a weight from node1 and a weight
                // from node 2  node1[weight1] + node2[weight2] ->
                // next_layer_node[weight1 + weight2]
                for wl1 in &self.layers[index_current_layer].nodes[index_node1] {
                    for wl2 in &self.layers[index_current_layer].nodes[index_node2] {
                        let combined_weight = wl1.weight + wl2.weight;
                        if combined_weight <= k {
                            solver
                                .add_clause(vec![
                                    !wl1.literal,
                                    !wl2.literal,
                                    *value_to_literal_map.get(&combined_weight).unwrap(),
                                ])
                                .expect("Adding encoding clause should not lead to conflict");
                            self.num_clauses_added += 1;
                        // explicitly forbid the assignment of both literals
                        //  note: could look into improving this part with implications weight[i] ->
                        // weight[i-1], could be a good trade-off
                        //  todo check if these clauses are necessary, and see if the trade-off
                        // makes sense      I think it is necessary
                        } else {
                            solver
                                .add_clause(vec![!wl1.literal, !wl2.literal])
                                .expect("Adding encoding clause should not lead to conflict");
                            self.num_clauses_added += 1;
                        }
                    }
                }
                // add the node to the next layer
                self.layers[index_current_layer + 1]
                    .nodes
                    .push(next_layer_node);
            } // node merging done

            // copy over the odd-numbered node that will not merge this round
            //  recall that the number of nodes in a layer may be an odd number
            //      since we merge nodes in pairs, it follows that one node will be unmerged
            //      so we just copy the unmerged node to the next layer
            if num_nodes_in_current_layer % 2 == 1 {
                // the unmerged node will be the last node in the current layer
                let unmerged_node = self.layers[index_current_layer]
                    .nodes
                    .last()
                    .unwrap()
                    .clone();

                self.layers[index_current_layer + 1]
                    .nodes
                    .push(unmerged_node);
            }
        }

        // the last layer now stores the final sum literals
        //  i.e., if the sum of input literals is at least k, then at least the weighted literal
        // with weight k will be set to true  note that there may be holes, so if a literal
        // with weight m_1 is set to true, it may not be the case that literal with weight m_2 with
        // m_2 < m_1 is also set to true      this is a difference when compared to the
        // standard unweighted totaliser      however this is not an issue, since in the
        // above discussion, at least the weight with weight k will be set to true, so we can use
        // that to constrain

        self.index_last_added_weighted_literal = self.layers.last().unwrap().nodes[0].len();

        debug!(
            "Encoding added {} clauses to the solver.",
            self.num_clauses_added
        );
    }
}

#[derive(Debug)]
struct Layer {
    nodes: Vec<Vec<WeightedLiteral>>,
}

impl Layer {
    fn new() -> Layer {
        Layer { nodes: vec![] }
    }
}
