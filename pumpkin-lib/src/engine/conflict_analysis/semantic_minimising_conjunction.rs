use crate::basic_types::HashSet;
use crate::basic_types::KeyedVec;
use crate::predicate;
use crate::predicates::Predicate;
use crate::propagators::SparseSet;
use crate::pumpkin_assert_advanced;
use crate::pumpkin_assert_moderate;
use crate::variables::DomainId;

#[derive(Clone, Default)]
struct SimpleIntegerDomain {
    domain_id: u32,
    lower_bound: i32,
    upper_bound: i32,
    holes: HashSet<i32>,
    original_lower_bound: i32,
    original_upper_bound: i32,
    original_holes: HashSet<i32>,
    inconsistent: bool,
}

impl SimpleIntegerDomain {
    fn reset(&mut self) {
        self.lower_bound = self.original_lower_bound;
        self.upper_bound = self.original_upper_bound;
        self.holes = self.original_holes.clone();
        self.inconsistent = false;
    }

    fn is_nonroot_lower_bound(&self) -> bool {
        self.lower_bound != self.original_lower_bound
    }

    fn is_nonroot_upper_bound(&self) -> bool {
        self.upper_bound != self.original_upper_bound
    }

    fn describe_domain(&self) -> Vec<Predicate> {
        if self.inconsistent {
            return vec![Predicate::trivially_false()];
        }

        let domain_id = DomainId { id: self.domain_id };
        let mut predicates: Vec<Predicate> = vec![];
        // If the domain assigned at a nonroot level, this is just one predicate.
        if self.lower_bound == self.upper_bound
            && self.is_nonroot_lower_bound()
            && self.is_nonroot_upper_bound()
        {
            predicates.push(predicate![domain_id == self.lower_bound]);
            return predicates;
        }

        // Add nonroot holes.
        for hole in self.holes.iter() {
            // Only record holes that are within the lower and upper bound,
            // that are not root assignments.
            // Since bound values cannot be in the holes,
            // we can use '<' or '>'.
            if self.lower_bound < *hole
                && *hole < self.upper_bound
                && !self.original_holes.contains(hole)
            {
                predicates.push(predicate![domain_id != *hole])
            }
        }

        // Add bounds but avoid root assignments.
        if self.is_nonroot_lower_bound() {
            predicates.push(predicate![domain_id >= self.lower_bound]);
        }

        if self.is_nonroot_upper_bound() {
            predicates.push(predicate![domain_id <= self.upper_bound]);
        }
        predicates
    }

    fn num_predicates(&self) -> i32 {
        if !self.inconsistent {
            // Domain is assigned, and it was not a singleton domain to start with.
            if self.lower_bound == self.upper_bound && self.is_nonroot_lower_bound() {
                1
            }
            // Domain is a singleton, and is not inconsistent.
            else if self.original_lower_bound == self.original_upper_bound {
                0
            } else {
                self.is_nonroot_lower_bound() as i32
                    + self.is_nonroot_upper_bound() as i32
                    + self.holes.len() as i32
            }
        } else {
            1
        }
    }

    // Returns the difference in number of predicates needed to describe the domain.
    // A positive value means more predicates are needed, a negative value means less.
    // A none value means the domain became inconsistent.
    fn set_lower_bound(&mut self, new_bound: i32) -> Option<i32> {
        if self.inconsistent {
            return None;
        }

        // Ignore predicate if it is subsumed.
        if new_bound <= self.lower_bound {
            return Some(0);
        }

        // The bound must be tightening because of previous checks.
        pumpkin_assert_moderate!(self.lower_bound < new_bound);

        let num_predicate_before = self.num_predicates();

        self.lower_bound = new_bound;

        // Now propagate the effect of holes on the lower bound.
        while self.holes.contains(&self.lower_bound) && self.lower_bound <= self.upper_bound {
            self.lower_bound += 1;
        }

        if self.lower_bound > self.upper_bound {
            self.inconsistent = true;
            return None;
        }

        // Only keep holes that fall within the current bounds.
        self.holes
            .retain(|hole| self.lower_bound < *hole && *hole < self.upper_bound);

        Some(self.num_predicates() - num_predicate_before)
    }

    fn set_upper_bound(&mut self, new_bound: i32) -> Option<i32> {
        if self.inconsistent {
            return None;
        }

        // Ignore predicate if it is subsumed.
        if new_bound >= self.upper_bound {
            return Some(0);
        }

        // The bound must be tightening because of previous checks.
        pumpkin_assert_moderate!(self.upper_bound > new_bound);

        let num_predicate_before = self.num_predicates();

        self.upper_bound = new_bound;

        // Now propagate the effect of holes on the upper bound.
        while self.holes.contains(&self.upper_bound) && self.lower_bound <= self.upper_bound {
            self.upper_bound -= 1;
        }

        if self.lower_bound > self.upper_bound {
            self.inconsistent = true;
            return None;
        }

        // Only keep holes that fall within the current bounds.
        self.holes
            .retain(|hole| self.lower_bound < *hole && *hole < self.upper_bound);

        Some(self.num_predicates() - num_predicate_before)
    }

    fn remove(&mut self, value: i32) -> Option<i32> {
        if self.inconsistent {
            return None;
        }

        let added_hole = self.holes.insert(value);
        if !added_hole {
            return Some(0);
        } else {
            // The minus one account for the hole that was just added.
            let num_predicates_before = self.num_predicates() - 1;
            let mut tighter_bound = false;

            // Now propagate the effect of holes on the lower bound.
            while self.holes.contains(&self.lower_bound) && self.lower_bound <= self.upper_bound {
                self.lower_bound += 1;
                tighter_bound = true;
            }

            if self.lower_bound > self.upper_bound {
                self.inconsistent = true;
                return None;
            }

            // Now propagate the effect of holes on the upper bound.
            while self.holes.contains(&self.upper_bound) && self.lower_bound <= self.upper_bound {
                self.upper_bound -= 1;
                tighter_bound = true;
            }

            if self.lower_bound > self.upper_bound {
                self.inconsistent = true;
                return None;
            }

            // todo: could remove elements as we increase the bounds.
            if tighter_bound {
                self.holes
                    .retain(|hole| self.lower_bound < *hole && *hole < self.upper_bound);
            }

            // Everyone ok.
            Some(self.num_predicates() - num_predicates_before)
        }
    }

    fn assign(&mut self, value: i32) -> Option<i32> {
        if self.inconsistent {
            None
        }
        // Already assigned, nothing to do.
        else if self.lower_bound == self.upper_bound {
            Some(0)
        }
        // If the assignment cannot be done due to bounds, declare inconsistency.
        else if value < self.lower_bound || self.upper_bound < value {
            self.inconsistent = true;
            None
        }
        // If the assignment cannot be done due to holes, declare inconsistency.
        else if self.holes.contains(&value) {
            self.inconsistent = true;
            None
        }
        // Everything ok, can assign.
        else {
            let num_predicates_before = self.num_predicates();
            self.lower_bound = value;
            self.upper_bound = value;
            self.holes.clear();
            Some(1 - num_predicates_before)
        }
    }
}

pub struct SemanticMinimisingConjunction {
    /// The sparse set indicates which domain ids are stored within the data structure. For those
    /// present, and only for those, the 'domains' field contains information about the domain.
    present_ids: SparseSet<DomainId>,
    domains: KeyedVec<DomainId, SimpleIntegerDomain>,
    /// Denotes the number of predicates needed to represent the domains.
    num_predicates: i32,
    inconsistent_domains: bool,
}

impl Default for SemanticMinimisingConjunction {
    fn default() -> Self {
        let mapping = |x: &DomainId| x.id as usize;
        Self {
            present_ids: SparseSet::new(vec![], mapping),
            domains: Default::default(),
            num_predicates: 0,
            inconsistent_domains: false,
        }
    }
}

impl SemanticMinimisingConjunction {
    pub fn grow(&mut self, original_lower_bound: i32, original_upper_bound: i32, holes: Vec<i32>) {
        self.domains.push(SimpleIntegerDomain {
            domain_id: self.domains.len() as u32,
            lower_bound: original_lower_bound,
            upper_bound: original_upper_bound,
            holes: HashSet::from_iter(holes.iter().cloned()),
            original_lower_bound,
            original_upper_bound,
            original_holes: HashSet::from_iter(holes.iter().cloned()),
            inconsistent: false,
        });
    }

    pub fn push(&mut self, predicate: Predicate) {
        if self.inconsistent_domains {
            return;
        }

        let difference = match predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.domains[domain_id].set_lower_bound(lower_bound),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.domains[domain_id].set_upper_bound(upper_bound),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.domains[domain_id].remove(not_equal_constant),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => self.domains[domain_id].assign(equality_constant),
        };

        if let Some(difference) = difference {
            if difference > 0 && !self.present_ids.contains(&predicate.get_domain()) {
                self.present_ids.insert(predicate.get_domain());
            }

            self.num_predicates += difference;
        } else {
            self.num_predicates = 1;
            self.inconsistent_domains = true;
        }
    }

    pub fn reset(&mut self) {
        // Remove the domain ids from the present domain ids.
        // todo: introduce a clear method for sparse set.
        let vals: Vec<DomainId> = self.present_ids.iter().copied().collect();
        for domain_id in vals {
            self.present_ids.remove(&domain_id);
            self.domains[domain_id].reset();
        }
        self.num_predicates = 0;
        self.inconsistent_domains = false;
    }

    pub fn get_predicates(&self) -> Vec<Predicate> {
        if self.inconsistent_domains {
            vec![Predicate::trivially_false()]
        } else {
            let mut predicates: Vec<Predicate> = vec![];
            for domain_id in self.present_ids.iter() {
                predicates.append(&mut self.domains[*domain_id].describe_domain());
            }
            // pumpkin_assert_moderate!(self.num_predicates as usize == predicates.len());
            predicates
        }
    }

    fn compute_num_predicates_from_scratch(&self) -> usize {
        if !self.inconsistent_domains {
            let mut num_predicates = 0;
            for i in 0..self.domains.len() {
                let domain_id = DomainId::new(i as u32);
                num_predicates += self.domains[domain_id].num_predicates();
            }

            num_predicates as usize
        } else {
            1
        }
    }

    pub fn num_predicates(&self) -> usize {
        pumpkin_assert_advanced!(
            self.num_predicates as usize == self.compute_num_predicates_from_scratch()
        );

        if !self.inconsistent_domains {
            self.num_predicates as usize
        } else {
            1
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::conjunction;
    use crate::engine::conflict_analysis::SemanticMinimisingConjunction;
    use crate::predicate;
    use crate::predicates::Predicate;
    use crate::predicates::PropositionalConjunction;
    use crate::variables::DomainId;

    #[test]
    fn no_changes() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);
        assert!(p.num_predicates() == 0);
        assert!(p.get_predicates().is_empty());
    }

    #[test]
    fn trivial_conflict_bounds() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 >= 5));
        p.push(predicate!(domain_0 <= 4));

        assert!(p.num_predicates() == 1);
        assert!(p.get_predicates()[0] == Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_holes() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 != 5));
        p.push(predicate!(domain_0 >= 5));
        p.push(predicate!(domain_0 <= 5));

        assert!(p.num_predicates() == 1);
        assert!(p.get_predicates()[0] == Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_assignment() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 != 5));
        p.push(predicate!(domain_0 == 5));

        assert!(p.num_predicates() == 1);
        assert!(p.get_predicates()[0] == Predicate::trivially_false());
    }

    #[test]
    fn trivial_conflict_bounds_reset() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 >= 5));
        p.push(predicate!(domain_0 <= 4));

        p.reset();

        assert!(p.num_predicates() == 0);
        assert!(p.get_predicates().is_empty());
    }

    #[test]
    fn simple_bound1() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);
        p.grow(0, 5, vec![]);

        let domain_0 = DomainId::new(0);
        let domain_1 = DomainId::new(1);

        let conjunction =
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4]);
        for predicate in conjunction.iter() {
            p.push(*predicate);
        }

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 3);
        assert_eq!(predicates.iter().count(), 3);
        assert_eq!(
            predicates,
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 <= 4])
        );
    }

    #[test]
    fn simple_bound2() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);
        p.grow(0, 5, vec![]);

        let domain_0 = DomainId::new(0);
        let domain_1 = DomainId::new(1);

        let conjunction =
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4]);
        for predicate in conjunction.iter() {
            p.push(*predicate);
        }

        p.push(predicate!(domain_0 != 7));

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 4);
        assert_eq!(predicates.iter().count(), 4);
        assert_eq!(
            predicates,
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 <= 4] & [domain_0 != 7])
        );
    }

    #[test]
    fn simple_bound3() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);
        p.grow(0, 5, vec![]);

        let domain_0 = DomainId::new(0);
        let domain_1 = DomainId::new(1);

        let conjunction =
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4]);

        for predicate in conjunction.iter() {
            p.push(*predicate);
        }

        assert_eq!(p.num_predicates(), 3);

        p.push(predicate!(domain_0 != 7));

        assert_eq!(p.num_predicates(), 4);
        p.push(predicate!(domain_0 != 7));
        p.push(predicate!(domain_0 != 8));

        assert_eq!(p.num_predicates(), 5);
        p.push(predicate!(domain_0 != 6));

        assert_eq!(p.num_predicates(), 6);

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 6);
        assert_eq!(predicates.iter().count(), 6);
        assert_eq!(
            predicates,
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
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);
        p.grow(0, 5, vec![]);

        let domain_0 = DomainId::new(0);
        let domain_1 = DomainId::new(1);

        let conjunction =
            conjunction!([domain_0 >= 5] & [domain_0 <= 9] & [domain_1 >= 0] & [domain_1 <= 4]);
        for predicate in conjunction.iter() {
            p.push(*predicate);
        }

        p.push(predicate!(domain_0 != 7));
        p.push(predicate!(domain_0 != 7));
        p.push(predicate!(domain_0 != 8));
        p.push(predicate!(domain_0 != 6));

        p.push(predicate!(domain_0 == 5));

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 2);
        assert_eq!(predicates.iter().count(), 2);
        assert_eq!(predicates, conjunction!([domain_0 == 5] & [domain_1 <= 4]));
    }

    #[test]
    fn simple_lb_override1() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 >= 2));
        p.push(predicate!(domain_0 >= 1));
        p.push(predicate!(domain_0 >= 5));

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 1);
        assert_eq!(predicates.iter().count(), 1);
        assert_eq!(predicates, conjunction!([domain_0 >= 5]));
    }

    #[test]
    fn hole_lb_override() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 != 2));
        p.push(predicate!(domain_0 != 3));
        p.push(predicate!(domain_0 >= 1));
        p.push(predicate!(domain_0 >= 5));

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 1);
        assert_eq!(predicates.iter().count(), 1);
        assert_eq!(predicates, conjunction!([domain_0 >= 5]));
    }

    #[test]
    fn hole_push_lb() {
        let mut p = SemanticMinimisingConjunction::default();
        p.grow(0, 10, vec![]);

        let domain_0 = DomainId::new(0);

        p.push(predicate!(domain_0 != 2));
        p.push(predicate!(domain_0 != 3));
        p.push(predicate!(domain_0 >= 1));
        p.push(predicate!(domain_0 != 1));

        let predicates: PropositionalConjunction = p.get_predicates().into();

        assert_eq!(p.num_predicates(), 1);
        assert_eq!(predicates.iter().count(), 1);
        assert_eq!(predicates, conjunction!([domain_0 >= 4]));
    }
}
