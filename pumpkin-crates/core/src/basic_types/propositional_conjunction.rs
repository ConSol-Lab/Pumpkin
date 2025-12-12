use std::ops::Deref;
use std::ops::Index;
use std::ops::IndexMut;

use itertools::Itertools;

use crate::engine::predicates::predicate::Predicate;

/// A struct which represents a conjunction of [`Predicate`]s (e.g. it can represent `[x >= 5] /\ [y
/// <= 10]`).
#[derive(Clone, Default, Eq)]
pub struct PropositionalConjunction {
    predicates_in_conjunction: Vec<Predicate>,
}

impl Deref for PropositionalConjunction {
    type Target = [Predicate];

    fn deref(&self) -> &Self::Target {
        &self.predicates_in_conjunction
    }
}

impl PropositionalConjunction {
    pub fn new(predicates_in_conjunction: Vec<Predicate>) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction,
        }
    }

    pub fn clear(&mut self) {
        self.predicates_in_conjunction.clear();
    }

    pub fn as_slice(&self) -> &[Predicate] {
        self.predicates_in_conjunction.as_slice()
    }

    pub fn push(&mut self, predicate: Predicate) {
        self.predicates_in_conjunction.push(predicate);
    }

    pub fn swap(&mut self, a: usize, b: usize) {
        self.predicates_in_conjunction.swap(a, b)
    }

    pub fn pop(&mut self) -> Option<Predicate> {
        self.predicates_in_conjunction.pop()
    }

    pub fn extend_and_remove_duplicates(
        mut self,
        additional_elements: impl Iterator<Item = Predicate>,
    ) -> PropositionalConjunction {
        self.predicates_in_conjunction = self
            .predicates_in_conjunction
            .into_iter()
            .chain(additional_elements)
            .unique()
            .collect();
        self
    }
}

impl Extend<Predicate> for PropositionalConjunction {
    fn extend<T: IntoIterator<Item = Predicate>>(&mut self, iter: T) {
        self.predicates_in_conjunction.extend(iter);
    }
}

impl IntoIterator for PropositionalConjunction {
    type Item = Predicate;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.predicates_in_conjunction.into_iter()
    }
}

impl Index<usize> for PropositionalConjunction {
    type Output = Predicate;

    fn index(&self, index: usize) -> &Self::Output {
        &self.predicates_in_conjunction[index]
    }
}

impl IndexMut<usize> for PropositionalConjunction {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.predicates_in_conjunction[index]
    }
}

impl FromIterator<Predicate> for PropositionalConjunction {
    fn from_iter<T: IntoIterator<Item = Predicate>>(iter: T) -> Self {
        let vec = iter.into_iter().collect();
        PropositionalConjunction {
            predicates_in_conjunction: vec,
        }
    }
}

impl<T> From<T> for PropositionalConjunction
where
    T: AsRef<[Predicate]>,
{
    fn from(slice: T) -> Self {
        PropositionalConjunction::new(slice.as_ref().to_vec())
    }
}

impl From<PropositionalConjunction> for Vec<Predicate> {
    fn from(conjunction: PropositionalConjunction) -> Vec<Predicate> {
        conjunction.iter().copied().collect()
    }
}

impl From<Predicate> for PropositionalConjunction {
    fn from(predicate: Predicate) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction: vec![predicate],
        }
    }
}

impl std::fmt::Display for PropositionalConjunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.predicates_in_conjunction.is_empty() {
            write!(f, "{{empty}}")
        } else {
            write!(
                f,
                "{}",
                self.predicates_in_conjunction
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join("; ")
            )
        }
    }
}

impl std::fmt::Debug for PropositionalConjunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl PartialEq for PropositionalConjunction {
    fn eq(&self, other: &Self) -> bool {
        if self.predicates_in_conjunction.len() != other.predicates_in_conjunction.len() {
            return false;
        }

        self.predicates_in_conjunction
            .iter()
            .all(|predicate| other.predicates_in_conjunction.contains(predicate))
    }
}

/// A macro which allows for the creation of a [`PropositionalConjunction`].
///
/// # Example
/// ```rust
/// # use pumpkin_core::predicates::PropositionalConjunction;
/// # use pumpkin_core::Solver;
/// # use pumpkin_core::conjunction;
/// # use pumpkin_core::predicate;
/// let mut solver = Solver::default();
/// let x = solver.new_bounded_integer(0, 10);
/// let y = solver.new_bounded_integer(5, 15);
///
/// let conjunction = conjunction!([x >= 5] & [y <= 10]);
/// assert_eq!(
///     conjunction,
///     PropositionalConjunction::new(vec![predicate!(x >= 5), predicate!(y <= 10)].into())
/// );
/// ```
#[macro_export]
macro_rules! conjunction {
    (@to_conjunction $($body:tt)*) => {
        $crate::predicates::PropositionalConjunction::from($($body)*)
    };

    (@munch {$($body:tt)*} -> & [$($pred:tt)+] $($rest:tt)*) => {
        conjunction!(@munch {$crate::predicate![$($pred)+], $($body)*} -> $($rest)*)
    };

    (@munch {$($body:tt)*} -> ) => {
        conjunction!(@to_conjunction vec![$($body)*])
    };

    (@munch {$($body:tt)*} -> $($rest:tt)+) => {
        compile_error!("Incorrect usage of the macro")
    };

    ($($input:tt)+) => {
        conjunction!(@munch {} -> & $($input)*)
    };

    () => {
        conjunction!(@to_conjunction vec![])
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::variables::DomainId;
    use crate::predicate;

    #[test]
    fn order_is_ignored_for_equality() {
        let x = DomainId::new(0);
        let y = DomainId::new(1);

        let conj1 = conjunction!([x >= 5] & [y <= 7]);
        let conj2 = conjunction!([y <= 7] & [x >= 5]);

        assert_eq!(conj1, conj2);
    }

    #[test]
    fn conjunction_macro_test() {
        assert_eq!(conjunction!(), PropositionalConjunction::default());

        let x = DomainId::new(0);
        let y = DomainId::new(1);
        let conjunction =
            PropositionalConjunction::from(vec![predicate![x >= 5], predicate![y == 1]]);
        assert_eq!(conjunction!([x >= 5] & [y == 1]), conjunction);
    }

    #[test]
    fn nested_path_is_forwarded_to_predicate() {
        struct Wrapper {
            x: DomainId,
        }
        let w = Wrapper {
            x: DomainId::new(0),
        };

        let conjunction = PropositionalConjunction::from(vec![predicate![w.x == 1]]);

        assert_eq!(conjunction!([w.x == 1]), conjunction);
    }
}
