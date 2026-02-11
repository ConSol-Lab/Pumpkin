use std::cmp;

use pumpkin_core::conflict_resolving::ConflictAnalysisContext;
use pumpkin_core::containers::HashSet;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::SparseSet;
use pumpkin_core::create_statistics_struct;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::ProblemSolution;
use pumpkin_core::statistics::moving_averages::CumulativeMovingAverage;
use pumpkin_core::statistics::moving_averages::MovingAverage;
use pumpkin_core::variables::DomainId;

use crate::minimisers::NogoodMinimiser;

/// [`NogoodMinimiser`] that removes redundant [`Predicate`]s by analysing the semantic meaning of
/// the predicates.
///
/// A [`Predicate`] is redundant if it is implied by another predicate in the nogood.
///
/// For example, if there is a nogood `[x >= 5] /\ [x >= 7] /\ [...] -> ⊥`, then the predicate [x
/// >= 5] is redundant.
#[derive(Clone, Debug)]
pub struct SemanticMinimiser {
    original_domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    present_ids: SparseSet<DomainId>,
    helper: Vec<Predicate>,

    mode: SemanticMinimisationMode,

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
            mode: SemanticMinimisationMode::EnableEqualityMerging,
            statistics: SemanticMinimiserStatistics::default(),
        }
    }
}

/// Used to determine whether the [`SemanticMinimiser`] merges equality [`Predicate`]s or not.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SemanticMinimisationMode {
    /// Enables equality merging; for example, if the predicates [x >= v] and [x <= v] are present
    /// in the domain description, then it is replaced with [x == v].
    EnableEqualityMerging,
    /// Disables equality merging; for example, if the predicates [x >= v] and [x <= v] are present
    /// in the domain description, then they are both kept in the nogood after minimisation.
    DisableEqualityMerging,
}

impl NogoodMinimiser for SemanticMinimiser {
    fn minimise(&mut self, context: &mut ConflictAnalysisContext, nogood: &mut Vec<Predicate>) {
        self.accommodate(context);
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
                self.mode,
            );
        }
        *nogood = self.helper.clone();

        if self.mode == SemanticMinimisationMode::EnableEqualityMerging {
            self.statistics
                .average_number_of_removed_atomic_constraints_semantic
                .add_term((len_before - nogood.len()) as u64);
        }
    }
}

impl SemanticMinimiser {
    /// Sets the [`SemanticMinimisationMode`] used by the [`SemanticMinimiser`].
    pub fn set_mode(&mut self, mode: SemanticMinimisationMode) {
        self.mode = mode
    }

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

    fn accommodate(&mut self, context: &ConflictAnalysisContext) {
        assert!(self.domains.len() == self.original_domains.len());

        while (self.domains.len() as u32) < context.num_domains() as u32 {
            let domain_id = DomainId::new(self.domains.len() as u32);
            let lower_bound = context.initial_lower_bound(domain_id);
            let upper_bound = context.initial_upper_bound(domain_id);
            let holes = context.initial_holes(domain_id);
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
        mode: SemanticMinimisationMode,
    ) {
        if let SemanticMinimisationMode::EnableEqualityMerging = mode {
            // If the domain assigned at a nonroot level, this is just one predicate.
            if self.lower_bound == self.upper_bound
                && self.lower_bound != original_domain.lower_bound
                && self.upper_bound != original_domain.upper_bound
            {
                description.push(predicate![domain_id == self.lower_bound]);
                return;
            }
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

#[cfg(test)]
#[allow(deprecated, reason = "Should be refactored using the state API")]
mod tests {
    use pumpkin_core::Solver;
    use pumpkin_core::branching::Brancher;
    use pumpkin_core::branching::BrancherEvent;
    use pumpkin_core::branching::SelectionContext;
    use pumpkin_core::conjunction;
    use pumpkin_core::predicate;
    use pumpkin_core::predicates::Predicate;
    use pumpkin_core::predicates::PropositionalConjunction;

    use crate::minimisers::NogoodMinimiser;
    use crate::minimisers::semantic_minimiser::SemanticMinimisationMode;
    use crate::minimisers::semantic_minimiser::SemanticMinimiser;

    #[derive(Debug, Default)]
    struct DummyBrancher;

    impl Brancher for DummyBrancher {
        fn next_decision(&mut self, _context: &mut SelectionContext) -> Option<Predicate> {
            todo!()
        }

        fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
            todo!()
        }
    }

    #[test]
    fn trivial_nogood() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_id = solver.new_bounded_integer(0, 10);
        let mut nogood: Vec<Predicate> =
            vec![predicate!(domain_id >= 0), predicate!(domain_id <= 10)];

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert!(nogood.is_empty());
    }

    #[test]
    fn trivial_conflict_bounds() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_id = solver.new_bounded_integer(0, 10);
        let mut nogood: Vec<Predicate> =
            vec![predicate!(domain_id >= 5), predicate!(domain_id <= 4)];

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 1);
        assert_eq!(nogood[0], Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_holes() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_id = solver.new_bounded_integer(0, 10);
        let mut nogood: Vec<Predicate> = vec![
            predicate!(domain_id != 5),
            predicate!(domain_id >= 5),
            predicate!(domain_id <= 5),
        ];

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 1);
        assert_eq!(nogood[0], Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_assignment() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_id = solver.new_bounded_integer(0, 10);
        let mut nogood: Vec<Predicate> =
            vec![predicate!(domain_id != 5), predicate!(domain_id == 5)];

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 1);
        assert_eq!(nogood[0], Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_bounds_reset() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_id = solver.new_bounded_integer(0, 10);
        let mut nogood: Vec<Predicate> =
            vec![predicate!(domain_id != 5), predicate!(domain_id == 5)];

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);
        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        let mut brancher = DummyBrancher;
        let mut other = Vec::default();
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut other,
        );

        assert!(other.is_empty());
    }

    #[test]
    fn simple_bound1() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);
        let domain_1 = solver.new_bounded_integer(0, 5);

        let mut nogood: Vec<Predicate> =
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4])
                .into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 3);
        assert_eq!(
            PropositionalConjunction::from(nogood),
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_bound2() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);
        let domain_1 = solver.new_bounded_integer(0, 5);

        let mut nogood = conjunction!(
            [domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4] & [domain_0 != 7]
        )
        .into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 4);
        assert_eq!(
            PropositionalConjunction::from(nogood),
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 <= 4] & [domain_0 != 7])
        );
    }

    #[test]
    fn simple_bound3() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);
        let domain_1 = solver.new_bounded_integer(0, 5);

        let mut nogood = conjunction!(
            [domain_0 >= 5]
                & [domain_0 <= 9]
                & [domain_1 >= 0]
                & [domain_1 <= 4]
                & [domain_0 != 7]
                & [domain_0 != 7]
                & [domain_0 != 8]
                & [domain_0 != 6]
        )
        .into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 6);
        assert_eq!(
            PropositionalConjunction::from(nogood),
            conjunction!(
                [domain_0 >= 5]
                    & [domain_0 <= 9]
                    & [domain_1 <= 4]
                    & [domain_0 != 7]
                    & [domain_0 != 6]
                    & [domain_0 != 8]
            )
        );
    }

    #[test]
    fn simple_assign() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);
        let domain_1 = solver.new_bounded_integer(0, 5);

        let mut nogood = conjunction!(
            [domain_0 >= 5]
                & [domain_0 <= 9]
                & [domain_1 >= 0]
                & [domain_1 <= 4]
                & [domain_0 != 7]
                & [domain_0 != 7]
                & [domain_0 != 6]
                & [domain_0 == 5]
                & [domain_0 != 7]
        )
        .into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);
        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 2);
        assert_eq!(
            PropositionalConjunction::from(nogood),
            conjunction!([domain_0 == 5] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_assign_no_equality() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);
        let domain_1 = solver.new_bounded_integer(0, 5);

        let mut nogood = conjunction!(
            [domain_0 >= 5]
                & [domain_0 <= 9]
                & [domain_1 >= 0]
                & [domain_1 <= 4]
                & [domain_0 != 7]
                & [domain_0 != 7]
                & [domain_0 != 6]
                & [domain_0 == 5]
                & [domain_0 != 7]
        )
        .into();

        p.set_mode(SemanticMinimisationMode::DisableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 3);
        assert_eq!(
            PropositionalConjunction::from(nogood),
            conjunction!([domain_0 >= 5] & [domain_0 <= 5] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_lb_override1() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);

        let mut nogood = conjunction!([domain_0 >= 2] & [domain_0 >= 1] & [domain_0 >= 5]).into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 1);
        assert_eq!(nogood[0], predicate!(domain_0 >= 5));
    }

    #[test]
    fn hole_lb_override() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);

        let mut nogood =
            conjunction!([domain_0 != 2] & [domain_0 != 3] & [domain_0 >= 5] & [domain_0 >= 1])
                .into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 1);
        assert_eq!(
            PropositionalConjunction::from(nogood),
            conjunction!([domain_0 >= 5])
        );
    }

    #[test]
    fn hole_push_lb() {
        let mut p = SemanticMinimiser::default();
        let mut solver = Solver::default();
        let domain_0 = solver.new_bounded_integer(0, 10);

        let mut nogood =
            conjunction!([domain_0 != 2] & [domain_0 != 3] & [domain_0 >= 1] & [domain_0 != 1])
                .into();

        p.set_mode(SemanticMinimisationMode::EnableEqualityMerging);

        let mut brancher = DummyBrancher;
        p.minimise(
            &mut solver.conflict_analysis_context(&mut brancher),
            &mut nogood,
        );

        assert_eq!(nogood.len(), 1);
        assert_eq!(nogood[0], predicate![domain_0 >= 4]);
    }
}
