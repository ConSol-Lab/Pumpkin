use super::Predicate;

#[derive(Clone, Default)]
pub struct PropositionalConjunction {
    predicates_in_conjunction: Vec<Predicate>,
}

impl PropositionalConjunction {
    pub fn and(&mut self, predicate: Predicate) {
        self.predicates_in_conjunction.push(predicate);
    }

    pub fn num_predicates(&self) -> u32 {
        self.predicates_in_conjunction.len() as u32
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Predicate> {
        self.predicates_in_conjunction.iter()
    }
}

impl From<Vec<Predicate>> for PropositionalConjunction {
    fn from(predicates_in_conjunction: Vec<Predicate>) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction,
        }
    }
}

impl From<Predicate> for PropositionalConjunction {
    fn from(predicate: Predicate) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction: Vec::from([predicate]),
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
        write!(f, "{}", self)
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

#[macro_export]
macro_rules! conjunction {
    (@to_conjunction $($body:tt)*) => {
        $crate::basic_types::PropositionalConjunction::from($($body)*)
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
    use crate::{basic_types::DomainId, predicate};

    use super::*;

    #[test]
    fn order_is_ignored_for_equality() {
        let x = DomainId { id: 0 };
        let y = DomainId { id: 1 };

        let conj1 = conjunction!([x >= 5] & [y <= 7]);
        let conj2 = conjunction!([y <= 7] & [x >= 5]);

        assert_eq!(conj1, conj2);
    }

    #[test]
    fn conjunction_macro_test() {
        assert_eq!(conjunction!(), PropositionalConjunction::default());

        let x = DomainId { id: 0 };
        let y = DomainId { id: 1 };
        let conjunction = PropositionalConjunction {
            predicates_in_conjunction: vec![predicate![x >= 5], predicate![y == 1]],
        };
        assert_eq!(conjunction!([x >= 5] & [y == 1]), conjunction);
    }

    #[test]
    fn nested_path_is_forwarded_to_predicate() {
        struct Wrapper {
            x: DomainId,
        }
        let w = Wrapper {
            x: DomainId { id: 0 },
        };

        let conjunction = PropositionalConjunction {
            predicates_in_conjunction: vec![predicate![w.x == 1]],
        };

        assert_eq!(conjunction!([w.x == 1]), conjunction);
    }
}
