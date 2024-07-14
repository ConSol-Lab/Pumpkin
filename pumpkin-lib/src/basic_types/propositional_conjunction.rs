use crate::engine::predicates::integer_predicate::IntegerPredicate;

#[derive(Clone, Default, Eq)]
pub struct PropositionalConjunction {
    predicates_in_conjunction: Box<[IntegerPredicate]>,
}

impl PropositionalConjunction {
    pub fn new(predicates_in_conjunction: Box<[IntegerPredicate]>) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction,
        }
    }

    pub fn num_predicates(&self) -> u32 {
        self.predicates_in_conjunction.len() as u32
    }

    pub fn iter(&self) -> std::slice::Iter<'_, IntegerPredicate> {
        self.predicates_in_conjunction.iter()
    }
}

impl FromIterator<IntegerPredicate> for PropositionalConjunction {
    fn from_iter<T: IntoIterator<Item = IntegerPredicate>>(iter: T) -> Self {
        let vec = iter.into_iter().collect();
        PropositionalConjunction {
            predicates_in_conjunction: vec,
        }
    }
}

impl From<Vec<IntegerPredicate>> for PropositionalConjunction {
    fn from(vec: Vec<IntegerPredicate>) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction: vec.into_boxed_slice(),
        }
    }
}

impl From<PropositionalConjunction> for Vec<IntegerPredicate> {
    fn from(conjunction: PropositionalConjunction) -> Vec<IntegerPredicate> {
        conjunction.iter().copied().collect()
    }
}

impl From<IntegerPredicate> for PropositionalConjunction {
    fn from(predicate: IntegerPredicate) -> Self {
        PropositionalConjunction {
            predicates_in_conjunction: Box::new([predicate]),
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
    use super::*;
    use crate::engine::variables::DomainId;
    use crate::predicate;

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
            x: DomainId { id: 0 },
        };

        let conjunction = PropositionalConjunction::from(vec![predicate![w.x == 1]]);

        assert_eq!(conjunction!([w.x == 1]), conjunction);
    }
}
