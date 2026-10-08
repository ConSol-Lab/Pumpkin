use pumpkin_checking::DomainView;
use pumpkin_checking::IntExt;

use crate::engine::Assignments;
use crate::predicates::Predicate;
use crate::variables::DomainId;

/// The domains of the solver, read in place, for example to check a solution.
impl DomainView<Predicate> for Assignments {
    fn lower_bound(&self, domain: &DomainId) -> IntExt {
        IntExt::Int(self.get_lower_bound(*domain))
    }

    fn upper_bound(&self, domain: &DomainId) -> IntExt {
        IntExt::Int(self.get_upper_bound(*domain))
    }

    fn contains(&self, domain: &DomainId, value: i32) -> bool {
        self.is_value_in_domain(*domain, value)
    }

    fn holes<'a>(&'a self, domain: &DomainId) -> Box<dyn Iterator<Item = i32> + 'a> {
        // The solver also keeps the values removed outside the current bounds.
        let lower_bound = self.get_lower_bound(*domain);
        let upper_bound = self.get_upper_bound(*domain);
        Box::new(
            self.get_holes(*domain)
                .filter(move |&value| lower_bound < value && value < upper_bound),
        )
    }

    fn is_true(&self, predicate: &Predicate) -> bool {
        self.is_predicate_satisfied(*predicate)
    }

    fn iter_domain<'a>(&'a self, domain: &DomainId) -> Option<Box<dyn Iterator<Item = i32> + 'a>>
    where
        DomainId: 'a,
    {
        Some(Box::new(self.get_domain_iterator(*domain)))
    }
}
