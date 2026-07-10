use crate::branching::Brancher;
use crate::branching::BrancherEvent;
use crate::branching::SelectionContext;
#[cfg(doc)]
use crate::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::branching::value_selection::InDomainMin;
use crate::branching::value_selection::ValueSelector;
#[cfg(doc)]
use crate::branching::variable_selection::InputOrder;
use crate::engine::variables::DomainGeneratorIterator;
use crate::predicates::Predicate;
use crate::pumpkin_assert_advanced;
#[cfg(doc)]
use crate::variables::DomainId;

/// A [`Brancher`] which lazily tries to fix the remaining variables.
///
/// It is similar to an [`IndependentVariableValueBrancher`] with [`InputOrder`] for variable
/// selection and [`InDomainMin`] for value selection, with the key difference that it does not
/// require a set of variables as input, but reasons over all defined [`DomainId`]s.
#[derive(Debug, Clone, Copy)]
pub struct FixingBrancher {
    /// The index of the last variable which was encountered to be fixed. The search for an unfixed
    /// variable will start from this point.
    last_fixed: u32,
}

impl Default for FixingBrancher {
    fn default() -> Self {
        Self { last_fixed: 1 }
    }
}

impl Brancher for FixingBrancher {
    fn next_decision(&mut self, context: &mut SelectionContext) -> Option<Predicate> {
        // We look for an unfixed variable to decide upon.
        //
        // Note that we start looking from the index of the last fixed variable that was
        // encountered. We then loop around to ensure that we consider all of the variables.
        if let Some(domain) = DomainGeneratorIterator::new(self.last_fixed, context.num_domains())
            .chain(DomainGeneratorIterator::new(1, self.last_fixed))
            .find(|domain_id| {
                let is_fixed = context.is_integer_fixed(*domain_id);

                if is_fixed {
                    self.last_fixed = domain_id.id();
                }

                !is_fixed
            })
        {
            return Some(InDomainMin.select_value(context, domain));
        }

        pumpkin_assert_advanced!(context.are_all_variables_assigned());

        None
    }

    fn subscribe_to_events(&self) -> Vec<BrancherEvent> {
        vec![]
    }
}
