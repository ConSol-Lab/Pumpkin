use std::cmp;

use crate::containers::HashSet;
use crate::containers::KeyedVec;
use crate::containers::SparseSet;
use crate::create_statistics_struct;
use crate::engine::Assignments;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateType;
use crate::statistics::moving_averages::CumulativeMovingAverage;
use crate::statistics::moving_averages::MovingAverage;
use crate::variables::DomainId;

/// Minimiser that removes redundant [`Predicate`]s by analysing the semantic meaning of
/// the predicates.
///
/// A [`Predicate`] is redundant if it is implied by another predicate in the nogood.
///
/// For example, if there is a nogood `[x >= 5] /\ [x >= 7] /\ [...] -> ⊥`, then the predicate [x
/// >= 5] is redundant.
#[derive(Clone, Debug)]
pub(super) struct SemanticMinimiser {
    original_domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    present_ids: SparseSet<DomainId>,
    helper: Vec<Predicate>,

    statistics: SemanticMinimiserStatistics,
}

create_statistics_struct!(SemanticMinimiserStatistics {
    /// The average number of atomic constraints removed by semantic minimisation during conflict analysis
    average_number_of_removed_atomic_constraints_semantic: CumulativeMovingAverage<u64>,
});

impl Default for SemanticMinimiser {
    fn default() -> Self {
        let mapping = |x: &DomainId| x.id() as usize;
        Self {
            original_domains: Default::default(),
            domains: Default::default(),
            present_ids: SparseSet::new(vec![], mapping),
            helper: Vec::default(),
            statistics: SemanticMinimiserStatistics::default(),
        }
    }
}

impl SemanticMinimiser {
    pub(crate) fn minimise_internal(
        &mut self,
        assignments: &Assignments,
        nogood: &mut Vec<Predicate>,
    ) {
        self.accommodate(assignments);
        self.clean_up();
        self.apply_predicates(nogood);

        let len_before = nogood.len();

        // Compile the nogood based on the internal state.
        // Add domain description to the helper.
        for domain_id in self.present_ids.iter() {
            // If at least one domain is inconsistent, we can stop.
            if self.domains[domain_id].inconsistent {
                *nogood = vec![Predicate::trivially_false()];
                return;
            }
            self.domains[domain_id].add_domain_description_to_vector(
                *domain_id,
                &self.original_domains[domain_id],
                &mut self.helper,
            );
        }
        *nogood = self.helper.clone();

        self.statistics
            .average_number_of_removed_atomic_constraints_semantic
            .add_term((len_before - nogood.len()) as u64);
    }
}

impl SemanticMinimiser {
    fn apply_predicates(&mut self, nogood: &Vec<Predicate>) {
        // Apply the predicates to the domains in a straight-forward way.
        // Later we will take into account the effect of holes on the domain.
        for predicate in nogood {
            self.present_ids.insert(predicate.get_domain());

            let domain_id = predicate.get_domain();
            let value = predicate.get_right_hand_side();

            match predicate.get_predicate_type() {
                PredicateType::LowerBound => {
                    self.domains[domain_id].tighten_lower_bound(value);
                }
                PredicateType::UpperBound => {
                    self.domains[domain_id].tighten_upper_bound(value);
                }
                PredicateType::NotEqual => {
                    self.domains[domain_id].add_hole(value);
                }
                PredicateType::Equal => {
                    self.domains[domain_id].assign(value);
                }
            }
        }
        for domain_id in self.present_ids.iter() {
            self.domains[*domain_id].propagate_holes_on_lower_bound();
            self.domains[*domain_id].propagate_holes_on_upper_bound();
            self.domains[*domain_id].remove_redundant_holes();
            self.domains[*domain_id].update_consistency();
        }
    }

    fn accommodate(&mut self, context: &Assignments) {
        assert!(self.domains.len() == self.original_domains.len());

        while (self.domains.len() as u32) < context.num_domains() {
            let domain_id = DomainId::new(self.domains.len() as u32);
            let lower_bound = context.get_initial_lower_bound(domain_id);
            let upper_bound = context.get_initial_upper_bound(domain_id);
            let holes = context.get_initial_holes(domain_id);
            self.grow(lower_bound, upper_bound, holes);
        }
    }

    fn grow(&mut self, lower_bound: i32, upper_bound: i32, holes: Vec<i32>) {
        let mut initial_domain = SimpleIntegerDomain {
            lower_bound,
            upper_bound,
            holes: HashSet::from_iter(holes.iter().cloned()),
            inconsistent: false,
        };

        initial_domain.propagate_holes_on_lower_bound();
        initial_domain.propagate_holes_on_upper_bound();
        initial_domain.remove_redundant_holes();
        initial_domain.update_consistency();

        let _ = self.original_domains.push(initial_domain.clone());
        let _ = self.domains.push(initial_domain);
    }

    pub(crate) fn clean_up(&mut self) {
        // Remove the domain ids from the present domain ids.
        let vals: Vec<DomainId> = self.present_ids.iter().copied().collect();
        for domain_id in vals {
            self.present_ids.remove(&domain_id);
            self.domains[domain_id] = self.original_domains[domain_id].clone();
        }
        self.helper.clear();
    }
}

#[derive(Clone, Default, Debug)]
struct SimpleIntegerDomain {
    lower_bound: i32,
    upper_bound: i32,
    holes: HashSet<i32>,
    inconsistent: bool,
}

impl SimpleIntegerDomain {
    fn tighten_lower_bound(&mut self, lower_bound: i32) {
        self.lower_bound = cmp::max(self.lower_bound, lower_bound);
    }

    fn tighten_upper_bound(&mut self, upper_bound: i32) {
        self.upper_bound = cmp::min(self.upper_bound, upper_bound);
    }

    fn add_hole(&mut self, hole: i32) {
        // Add the hole if it is within the domain.
        // Note that we do not adjust bounds due to holes being at the border. This is taken care of
        // by other functions (propagate bounds based on holes).
        if self.lower_bound <= hole && hole <= self.upper_bound {
            let _ = self.holes.insert(hole);
        }
    }

    fn assign(&mut self, value: i32) {
        // If the domains are inconsistent, or if the assigned value would make the domain
        // inconsistent, declare inconsistency and stop.
        if self.lower_bound > self.upper_bound
            || self.lower_bound > value
            || self.upper_bound < value
        {
            self.inconsistent = true;
        }
        // Otherwise, it is safe to apply the predicate.
        // Note that we do not take into account holes here.
        else {
            self.lower_bound = value;
            self.upper_bound = value;
        }
    }

    fn propagate_holes_on_lower_bound(&mut self) {
        while self.holes.contains(&self.lower_bound) && self.lower_bound <= self.upper_bound {
            self.lower_bound += 1;
        }
    }

    fn propagate_holes_on_upper_bound(&mut self) {
        while self.holes.contains(&self.upper_bound) && self.lower_bound <= self.upper_bound {
            self.upper_bound -= 1;
        }
    }

    fn update_consistency(&mut self) {
        // The domain may have already gotten in an inconsistent state due to equality predicates.
        // Make sure not to make any changes if already inconsistent.
        if !self.inconsistent {
            self.inconsistent = self.lower_bound > self.upper_bound;
        }
    }

    fn remove_redundant_holes(&mut self) {
        // Do nothing if inconsistent.
        if self.inconsistent {
            return;
        }
        // Only keep holes that fall within the current bounds.
        self.holes
            .retain(|hole| self.lower_bound < *hole && *hole < self.upper_bound);
    }

    fn add_domain_description_to_vector(
        &self,
        domain_id: DomainId,
        original_domain: &SimpleIntegerDomain,
        description: &mut Vec<Predicate>,
    ) {
        // If the domain assigned at a nonroot level, this is just one predicate.
        if self.lower_bound == self.upper_bound
            && self.lower_bound != original_domain.lower_bound
            && self.upper_bound != original_domain.upper_bound
        {
            description.push(predicate![domain_id == self.lower_bound]);
            return;
        }

        // Add bounds but avoid root assignments.
        if self.lower_bound != original_domain.lower_bound {
            description.push(predicate![domain_id >= self.lower_bound]);
        }

        if self.upper_bound != original_domain.upper_bound {
            description.push(predicate![domain_id <= self.upper_bound]);
        }

        // Add nonroot holes.
        for hole in self.holes.iter() {
            // Only record holes that are within the lower and upper bound,
            // that are not root assignments.
            // Since bound values cannot be in the holes,
            // we can use '<' or '>'.
            if self.lower_bound < *hole
                && *hole < self.upper_bound
                && !original_domain.holes.contains(hole)
            {
                description.push(predicate![domain_id != *hole])
            }
        }
    }
}
