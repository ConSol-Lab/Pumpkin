use std::cmp;

use crate::basic_types::HashSet;
use crate::containers::KeyedVec;
use crate::containers::SparseSet;
use crate::engine::Assignments;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::DomainId;

#[derive(Clone, Debug)]
pub struct SemanticMinimiser {
    original_domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    present_ids: SparseSet<DomainId>,
    helper: Vec<Predicate>,
}

impl Default for SemanticMinimiser {
    fn default() -> Self {
        let mapping = |x: &DomainId| x.id as usize;
        Self {
            original_domains: Default::default(),
            domains: Default::default(),
            present_ids: SparseSet::new(vec![], mapping),
            helper: Vec::default(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Mode {
    EnableEqualityMerging,
    DisableEqualityMerging,
}

impl SemanticMinimiser {
    pub fn minimise(
        &mut self,
        nogood: &Vec<Predicate>,
        assignments: &Assignments,
        mode: Mode,
    ) -> Vec<Predicate> {
        self.accommodate(assignments);
        self.clean_up();
        self.apply_predicates(nogood);

        // Compile the nogood based on the internal state.
        // Add domain description to the helper.
        for domain_id in self.present_ids.iter() {
            // If at least one domain is inconsistent, we can stop.
            if self.domains[domain_id].inconsistent {
                return vec![Predicate::trivially_false()];
            }
            self.domains[domain_id].add_domain_description_to_vector(
                *domain_id,
                &self.original_domains[domain_id],
                &mut self.helper,
                mode,
            );
        }
        self.helper.clone()
    }

    fn apply_predicates(&mut self, nogood: &Vec<Predicate>) {
        // Apply the predicates to the domains in a straight-forward way.
        // Later we will take into account the effect of holes on the domain.
        for predicate in nogood {
            self.present_ids.insert(predicate.get_domain());

            match *predicate {
                Predicate::LowerBound {
                    domain_id,
                    lower_bound,
                } => {
                    self.domains[domain_id].tighten_lower_bound(lower_bound);
                }
                Predicate::UpperBound {
                    domain_id,
                    upper_bound,
                } => {
                    self.domains[domain_id].tighten_upper_bound(upper_bound);
                }
                Predicate::NotEqual {
                    domain_id,
                    not_equal_constant,
                } => {
                    self.domains[domain_id].add_hole(not_equal_constant);
                }
                Predicate::Equal {
                    domain_id,
                    equality_constant,
                } => {
                    self.domains[domain_id].assign(equality_constant);
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

    fn accommodate(&mut self, assignments: &Assignments) {
        assert!(self.domains.len() == self.original_domains.len());

        while (self.domains.len() as u32) < assignments.num_domains() {
            let domain_id = DomainId {
                id: self.domains.len() as u32,
            };
            let lower_bound = assignments.get_initial_lower_bound(domain_id);
            let upper_bound = assignments.get_initial_upper_bound(domain_id);
            let holes = assignments.get_initial_holes(domain_id);
            self.grow(lower_bound, upper_bound, holes);
        }
    }

    fn grow(&mut self, lower_bound: i32, upper_bound: i32, holes: Vec<i32>) {
        let initial_domain = SimpleIntegerDomain {
            lower_bound,
            upper_bound,
            holes: HashSet::from_iter(holes.iter().cloned()),
            inconsistent: false,
        };
        let _ = self.original_domains.push(initial_domain.clone());
        let _ = self.domains.push(initial_domain);
    }

    pub fn clean_up(&mut self) {
        // Remove the domain ids from the present domain ids.
        // todo: introduce a clear method for sparse set.
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
        mode: Mode,
    ) {
        if let Mode::EnableEqualityMerging = mode {
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
mod tests {
    use crate::conjunction;
    use crate::engine::conflict_analysis::semantic_minimiser::Mode;
    use crate::engine::conflict_analysis::SemanticMinimiser;
    use crate::engine::Assignments;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::predicates::PropositionalConjunction;

    #[test]
    fn trivial_nogood() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_id = assignments.grow(0, 10);
        let nogood: Vec<Predicate> = vec![predicate!(domain_id >= 0), predicate!(domain_id <= 10)];

        let p = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert!(p.is_empty());
    }

    #[test]
    fn trivial_conflict_bounds() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_id = assignments.grow(0, 10);
        let nogood: Vec<Predicate> = vec![predicate!(domain_id >= 5), predicate!(domain_id <= 4)];

        let p = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(p.len(), 1);
        assert_eq!(p[0], Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_holes() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_id = assignments.grow(0, 10);
        let nogood: Vec<Predicate> = vec![
            predicate!(domain_id != 5),
            predicate!(domain_id >= 5),
            predicate!(domain_id <= 5),
        ];

        let p = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(p.len(), 1);
        assert_eq!(p[0], Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_assignment() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_id = assignments.grow(0, 10);
        let nogood: Vec<Predicate> = vec![predicate!(domain_id != 5), predicate!(domain_id == 5)];

        let p = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(p.len(), 1);
        assert_eq!(p[0], Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_bounds_reset() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_id = assignments.grow(0, 10);
        let nogood: Vec<Predicate> = vec![predicate!(domain_id != 5), predicate!(domain_id == 5)];

        let _ = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        let p = p.minimise(&vec![], &assignments, Mode::EnableEqualityMerging);

        assert!(p.is_empty());
    }

    #[test]
    fn simple_bound1() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);
        let domain_1 = assignments.grow(0, 5);

        let nogood: Vec<Predicate> =
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4])
                .into();

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 3);
        assert_eq!(
            PropositionalConjunction::from(predicates),
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_bound2() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);
        let domain_1 = assignments.grow(0, 5);

        let nogood = conjunction!(
            [domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4] & [domain_0 != 7]
        )
        .into();

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 4);
        assert_eq!(
            PropositionalConjunction::from(predicates),
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 <= 4] & [domain_0 != 7])
        );
    }

    #[test]
    fn simple_bound3() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);
        let domain_1 = assignments.grow(0, 5);

        let nogood = conjunction!(
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

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 6);
        assert_eq!(
            PropositionalConjunction::from(predicates),
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
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);
        let domain_1 = assignments.grow(0, 5);

        let nogood = conjunction!(
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

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 2);
        assert_eq!(
            PropositionalConjunction::from(predicates),
            conjunction!([domain_0 == 5] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_assign_no_equality() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);
        let domain_1 = assignments.grow(0, 5);

        let nogood = conjunction!(
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

        let predicates = p.minimise(&nogood, &assignments, Mode::DisableEqualityMerging);

        assert_eq!(predicates.len(), 3);
        assert_eq!(
            PropositionalConjunction::from(predicates),
            conjunction!([domain_0 >= 5] & [domain_0 <= 5] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_lb_override1() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);

        let nogood = conjunction!([domain_0 >= 2] & [domain_0 >= 1] & [domain_0 >= 5]).into();

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 1);
        assert_eq!(predicates[0], predicate!(domain_0 >= 5));
    }

    #[test]
    fn hole_lb_override() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);

        let nogood =
            conjunction!([domain_0 != 2] & [domain_0 != 3] & [domain_0 >= 5] & [domain_0 >= 1])
                .into();

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 1);
        assert_eq!(
            PropositionalConjunction::from(predicates),
            conjunction!([domain_0 >= 5])
        );
    }

    #[test]
    fn hole_push_lb() {
        let mut p = SemanticMinimiser::default();
        let mut assignments = Assignments::default();
        let domain_0 = assignments.grow(0, 10);

        let nogood =
            conjunction!([domain_0 != 2] & [domain_0 != 3] & [domain_0 >= 1] & [domain_0 != 1])
                .into();

        let predicates = p.minimise(&nogood, &assignments, Mode::EnableEqualityMerging);

        assert_eq!(predicates.len(), 1);
        assert_eq!(predicates[0], predicate![domain_0 >= 4]);
    }
}
