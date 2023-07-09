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

/*impl IntoIterator for PropositionalConjunction {
    type Item = Predicate;
    type IntoIter = <Vec<Predicate> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.predicates_in_conjunction.into_iter()
    }
}*/

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

/*impl std::ops::Index<u32> for PropositionalConjunction {
    type Output = Predicate;
    fn index(&self, index: u32) -> &Predicate {
        self.predicates_in_conjunction.index(index as usize)
    }
}

impl std::ops::IndexMut<u32> for PropositionalConjunction {
    fn index_mut(&mut self, index: u32) -> &mut Predicate {
        self.predicates_in_conjunction.index_mut(index as usize)
    }
}*/
