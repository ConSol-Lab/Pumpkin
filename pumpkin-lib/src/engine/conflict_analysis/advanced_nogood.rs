use crate::basic_types::HashMap;
use crate::basic_types::KeyedVec;
use crate::branching::Brancher;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::predicate;
use crate::pumpkin_assert_simple;
/// The struct represents a nogood that does additional bookkeeping. The nogood is used during
/// conflict analysis as the learned nogood. The core functionality is as follows:
/// * Stores predicates, as expected from a nogood.
/// * Automatically minimises the nogood by removing semantic redundancies. For example, if the
///   nogood is '[x >= k]', then adding '[x != k - 2]' will have no effect, since this the added
///   predicate is redundant. During conflict analysis we expect that we may encounter many
///   redundancies, so this helps derive stronger nogoods.
/// * Allows popping the predicate that has the highest position on the trail. This is used as the
///   next candidate in conflict analysis.
/// * Reports whether the nogood is propagating, i.e., has only one predicate from the current
///   decision level.
pub(crate) struct AdvancedNogood {
    current_decision_level: usize,
    // The role of these predicates is solely in selecting the next predicate from the trail. This
    // vector could be avoided by looking only at the assignments, and avoiding repetitions.
    // We only store predicates from the current decision level in this vector.
    predicates_current_decision_level: Vec<PredicateWithInfo>,
    /// Used to hold info about the nogood.
    /// Does implicitly semantic minimisation.
    internal_assignments: Assignments,
    /// Incoming predicate domain ids are mapped to an internal representation, which is used in
    /// the internal assignments data structure.
    external_to_internal_id: HashMap<DomainId, DomainId>,
    internal_to_external_id: KeyedVec<DomainId, DomainId>,
    // note to self: could optimise by separately tracking predicates from the current decision
    // level and those at lower levels. This is because we only need consider the ones at the
    // current decision level when replacing predicates with their reason.
    is_trivially_falsified: bool,
}

impl AdvancedNogood {
    /// Initialises the internal data structures for a new nogood.
    /// The information about the current decision level is used by other parts.
    pub(crate) fn new(current_decision_level: usize) -> AdvancedNogood {
        let mut advanced_nogood = AdvancedNogood {
            current_decision_level,
            predicates_current_decision_level: vec![],
            internal_assignments: Assignments::default(),
            external_to_internal_id: HashMap::default(),
            internal_to_external_id: KeyedVec::default(),
            is_trivially_falsified: false,
        };
        // Internally the decision level is not properly tracked,
        // but it is still useful to differentiate root level operations
        // from nonroot level. This is done by simply starting with a nonroot value.
        advanced_nogood
            .internal_assignments
            .increase_decision_level();
        advanced_nogood
    }

    fn register_id_internally(&mut self, domain_id: DomainId, assignments: &Assignments) {
        // Only register if the domain is not yet registered.
        // I am disabling the clippy warning temporarily.
        #[allow(clippy::map_entry)]
        if !self.external_to_internal_id.contains_key(&domain_id) {
            // Internally the domain_id is replicated but with an internal id.

            // Collect root level state of the domain_id
            let lower_bound = assignments.get_initial_lower_bound(domain_id);
            let upper_bound = assignments.get_initial_upper_bound(domain_id);
            let holes = assignments.get_initial_holes(domain_id);

            // Replicate the domain with the new internal id
            let internal_domain_id = self.internal_assignments.grow(lower_bound, upper_bound);
            for hole in holes {
                internal_domain_id
                    .remove(&mut self.internal_assignments, hole, None)
                    .expect("Cannot fail with root removals.");
            }

            let _ = self
                .external_to_internal_id
                .insert(domain_id, internal_domain_id);

            if self.internal_to_external_id.len() <= domain_id.id as usize {
                self.internal_to_external_id
                    .resize(domain_id.id as usize + 1, DomainId { id: 0 });
            }
            self.internal_to_external_id[internal_domain_id] = domain_id;
        }
    }

    fn get_internal_id(&self, domain_id: DomainId) -> DomainId {
        *self.external_to_internal_id.get(&domain_id).unwrap()
    }

    fn convert_into_internal_predicate(
        &mut self,
        predicate: Predicate,
        assignments: &Assignments,
    ) -> Predicate {
        self.register_id_internally(predicate.get_domain(), assignments);
        let internal_id = self.get_internal_id(predicate.get_domain());
        // The code below is cumbersome, but this is the idea:
        // The internal predicate is the same as the input predicate,
        // but swapping out the domain id for the internal domain id.
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => predicate![internal_id >= lower_bound],
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => predicate![internal_id <= upper_bound],
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => predicate![internal_id != not_equal_constant],
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => predicate![internal_id == equality_constant],
        }
    }

    fn convert_into_external_predicate(&self, predicate: Predicate) -> Predicate {
        let external_id = self.internal_to_external_id[predicate.get_domain()];
        match predicate {
            Predicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => predicate![external_id >= lower_bound],
            Predicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => predicate![external_id <= upper_bound],
            Predicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => predicate![external_id != not_equal_constant],
            Predicate::Equal {
                domain_id: _,
                equality_constant,
            } => predicate![external_id == equality_constant],
        }
    }

    fn add_predicate(&mut self, predicate: Predicate, assignments: &Assignments) {
        let internal_predicate = self.convert_into_internal_predicate(predicate, assignments);

        match self
            .internal_assignments
            .evaluate_predicate(internal_predicate)
        {
            Some(truth_value) => match truth_value {
                true => {
                    // The predicate already satisfied, so it can be ignored.
                }
                false => {
                    // The predicate is falsified, which cannot happen during conflict analysis.
                    // But it can happen when preprocessing at the root level.
                    pumpkin_assert_simple!(self.current_decision_level == 0);
                    self.is_trivially_falsified = true;
                }
            },
            None => {
                // Add the predicate to the list of predicates in the nogood.
                let decision_level = assignments
                    .get_decision_level_for_predicate(&predicate)
                    .unwrap_or_default();

                let trail_position = assignments
                    .get_trail_position(&predicate)
                    .unwrap_or_default();

                // Recall that having self.current_decision_level means preprocessing at the root
                // level, so in that case we add every predicate to the internal assignments.
                if decision_level == self.current_decision_level && self.current_decision_level != 0
                {
                    let predicate_info = PredicateWithInfo {
                        predicate,
                        trail_position,
                    };
                    if !self
                        .predicates_current_decision_level
                        .contains(&predicate_info)
                    {
                        self.predicates_current_decision_level.push(predicate_info);
                    };
                } else {
                    // The predicate is undecided, so we can post it.
                    self.internal_assignments
                        .post_predicate(internal_predicate, None)
                        .expect("Previous code asserted this will be a success.");
                }
            }
        }
    }

    /// Adds the predicate to the nogood, and attaches the extra information (decision level and
    /// trail position) to the predicate internally. The extra information is used by other
    /// functions. Note that semantic redundancies is automatically applied.
    pub(crate) fn add_predicates(
        &mut self,
        predicates: Vec<Predicate>,
        assignments: &Assignments,
        mut brancher: Option<&mut dyn Brancher>,
    ) {
        for predicate in predicates {
            if self.is_trivially_falsified {
                break;
            }

            // println!("\tp: {}", predicate);
            // if !context
            // .assignments
            // .evaluate_predicate(predicate)
            // .is_some_and(|x| x)
            // {
            // println!("\tNot ok: {:?}", predicate);
            // }

            // The 'brancher.is_none()' is a bit of a hack to be able to use the advanced nogood
            // outside of conflict analysis.
            assert!(
                assignments.evaluate_predicate(predicate).is_some_and(|x| x) || brancher.is_none(),
                "Predicates must be true during conflict analysis."
            );
            self.add_predicate(predicate, assignments);

            // Currently we notify of every predicate. It may be better to only do so if the
            // predicate is not subsumed.
            if let Some(ref mut b) = brancher {
                b.on_appearance_in_conflict_predicate(predicate)
            }

            // println!("\tafter add: {:?}", self.predicates);
        }
        self.simplify_current_decision_level_predicates(assignments);
        // println!("simply {:?}", self.predicates_current_decision_level);
        // println!("\t resulting nogood: {:?}", learned_nogood);
        // println!("\t after min: {:?}", learned_nogood);
    }

    /// Need to simplify the current decision level predicates since there may be equivalent
    /// predicates or an equality is presented as two inequalities, which hamper conflicting
    /// analysis, i.e., the predicate may be propagating but it may not be detected because equality
    /// is now two predicates instead of one.
    fn simplify_current_decision_level_predicates(&mut self, assignments: &Assignments) {
        // Current decision level predicates can be empty for root level preprocessing.
        if self.is_trivially_falsified || self.predicates_current_decision_level.is_empty() {
            return;
        }

        // println!("before simpl: {:?}", self.predicates_current_decision_level);
        let mut simplified_nogood = AdvancedNogood::new(0);
        let mut temp_assignments = self.internal_assignments.clone();
        let _ = temp_assignments.synchronise(0);

        for p in self.predicates_current_decision_level.clone() {
            let internal_predicate = self.convert_into_internal_predicate(p.predicate, assignments);
            simplified_nogood.add_predicate(internal_predicate, &temp_assignments);
        }
        let temp = simplified_nogood.extract_final_learned_nogood(&self.internal_assignments);
        self.predicates_current_decision_level = temp
            .iter()
            .map(|p| {
                let external_predicate = self.convert_into_external_predicate(*p);
                PredicateWithInfo {
                    predicate: external_predicate,
                    trail_position: assignments.get_trail_position(&external_predicate).unwrap(),
                }
            })
            .collect();
        // println!("after simpl: {:?}", self.predicates_current_decision_level);
    }

    /// Removes the predicate that has the highest trail position.
    /// Returns None if the nogood is empty.
    pub(crate) fn pop_highest_trail_predicate(&mut self) -> Option<Predicate> {
        assert!(
            !self.predicates_current_decision_level.is_empty(),
            "Do not expect to see an empty nogood during conflict analysis!"
        );
        assert!(self.current_decision_level > 0);
        // For now we do this using several linear passes.
        // In the future could consider doing this more efficiently,
        // a single linear scan or using a heap-like structure.

        // If there is at least one element left.
        if let Some(next_predicate) = self
            .predicates_current_decision_level
            .iter()
            .max_by_key(|p| p.trail_position)
        {
            // Find the position of the next predicate.
            // This is a bit non-elegant but okay for now.
            let position = self
                .predicates_current_decision_level
                .iter()
                .position(|p| p.trail_position == next_predicate.trail_position)
                .unwrap();
            let removed_predicate = self.predicates_current_decision_level.swap_remove(position);

            // println!("pos: {}", position);
            // println!("predi: {}", removed_predicate.predicate);
            // println!("{:?}", self.predicates_current_decision_level);

            Some(removed_predicate.predicate)
        } else {
            None
        }
    }
    /// The nogood is considered propagating if it contains only one predicate from the current
    /// decision level.
    pub(crate) fn is_nogood_propagating(&self) -> bool {
        assert!(
            !self.predicates_current_decision_level.is_empty(),
            "Sanity check."
        );

        // The nogood is propagating if it has exactly one predicate
        // from the current decision level.
        self.predicates_current_decision_level.len() == 1
    }

    /// Extract the internal nogood and returns it as a learned nogood.
    /// The ordering of predicates within the nogood is arbitrary.
    pub(crate) fn extract_final_learned_nogood(
        mut self,
        assignments: &Assignments,
    ) -> Vec<Predicate> {
        if self.is_trivially_falsified {
            return vec![];
        }

        assert!(
            self.current_decision_level == 0 || self.predicates_current_decision_level.len() == 1
        );

        let mut nogood: Vec<Predicate> = vec![];
        if self.current_decision_level > 0 {
            // nogood.push(self.predicates_current_decision_level[0].predicate);
            let external_predicate = self.predicates_current_decision_level[0].predicate;
            let internal_predicate =
                self.convert_into_internal_predicate(external_predicate, assignments);
            self.internal_assignments
                .post_predicate(internal_predicate, None)
                .expect("Asserting predicate must be successfully posted.");
        }

        let internal_domain_description = self.internal_assignments.get_domain_descriptions();
        let mut external_domain_descriptions = internal_domain_description
            .iter()
            .map(|p| self.convert_into_external_predicate(*p))
            .collect();
        nogood.append(&mut external_domain_descriptions);
        nogood
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) struct PredicateWithInfo {
    predicate: Predicate,
    trail_position: usize,
}
