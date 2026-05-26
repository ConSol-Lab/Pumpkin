use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::checkers::support::Support;
use pumpkin_core::checkers::support::SupportGenerator;
use pumpkin_core::checkers::support::SupportsValue;
use pumpkin_core::checkers::support::UnsupportedValue;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::variables::IntegerVariable;

#[derive(Clone, Debug)]
pub struct MaximumChecker<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar, Rhs, Atomic> InferenceChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        let lowest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_lower_bound(&state))
            .max()
            .unwrap_or(IntExt::NegativeInf);
        let highest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_upper_bound(&state))
            .max()
            .unwrap_or(IntExt::PositiveInf);

        // If the intersection between the domain of `rhs` and `[lowest_maximum,
        // highest_maximum]` is empty, there is a conflict.

        lowest_maximum > self.rhs.induced_upper_bound(&state)
            || highest_maximum < self.rhs.induced_lower_bound(&state)
    }
}

impl<ElementVar, Rhs> SupportGenerator for MaximumChecker<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + SupportsValue,
    Rhs: IntegerVariable + SupportsValue,
{
    type Value = i32;

    fn support(
        &mut self,
        support: &mut Support<Self::Value>,
        local_id: LocalId,
        value: UnsupportedValue,
        domains: &Domains<'_>,
    ) {
        if local_id.unpack() as usize == self.array.len() {
            // We are trying to find support for the rhs.
            //
            // Regardless of whether we are trying to generate support for a lower-bound or an
            // upper-bound, we try to assign the variables that can be assigned to the value to the
            // value of the RHS and assign the rest to their lower-bound.
            let value = self.rhs.unpack(value);
            self.rhs.assign(value, support);

            let lb = domains.lower_bound(&self.rhs);
            let ub = domains.upper_bound(&self.rhs);

            pumpkin_assert_simple!(value == lb || value == ub);

            self.array.iter().for_each(|element| {
                if domains.contains(element, value) {
                    element.assign(value, support);
                } else {
                    element.assign(domains.lower_bound(element), support);
                }
            })
        } else {
            // We are trying to find support for an element in the array.
            //
            // This is somewhat trickier then generating support for the rhs.
            //
            // We need to find a maximum value such that:
            // 1. It is in the domain of `self.rhs`.
            // 2. It is in the domain of at least 1 variable in `self.array`.
            // 3. Every variable can take a value which is <= that value.
            // 4. It is at least as large as the unsupported value.
            //
            // If we have found this value, then we assign the domain in `self.array` and `self.rhs`
            // to that value, and the rest to their lower-bound.
            let var = &self.array[local_id.unpack() as usize];
            let value = var.unpack(value);
            var.assign(value, support);

            let lb = domains.lower_bound(var);
            let ub = domains.upper_bound(var);

            pumpkin_assert_simple!(value == lb || value == ub);

            // We go over all possible maximum values to create a solution.
            for possible_value in value..=domains.upper_bound(&self.rhs) {
                if !domains.contains(&self.rhs, possible_value) {
                    // The rhs cannot be assigned to this maximum value so we can continue.
                    continue;
                }

                // Next, we find an element in `self.array` which has the `possible_value` in its
                // domain.
                let Some((has_value_in_domain_index, has_value_in_domain)) = self
                    .array
                    .iter()
                    .enumerate()
                    .find(|(_, element)| domains.contains(*element, possible_value))
                else {
                    // If we cannot find such an element then we move onto the next possible value.
                    continue;
                };

                // If there is an element in `self.array` which cannot be assigned to
                // `possible_value`, then we continue looking for a value.
                if self.array.iter().enumerate().any(|(index, element)| {
                    index != local_id.unpack() as usize
                        && domains.lower_bound(element) > possible_value
                }) {
                    continue;
                }

                // Finally, we do the assignment.
                has_value_in_domain.assign(possible_value, support);
                self.rhs.assign(possible_value, support);
                self.array
                    .iter()
                    .enumerate()
                    .filter(|(index, _)| {
                        *index != has_value_in_domain_index && *index != local_id.unpack() as usize
                    })
                    .for_each(|(_, element)| element.assign(domains.lower_bound(element), support));

                return;
            }
            panic!("Was not able to generate support for Maximum");
        }
    }

    fn is_solution(&self, support: &Support<Self::Value>) -> bool {
        self.array
            .iter()
            .map(|element| element.support_value(support))
            .max()
            .expect("Expected at least one element in the maximum constraint")
            == self.rhs.support_value(support)
    }
}
