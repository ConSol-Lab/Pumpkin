use crate::predicates::Predicate;

#[derive(Clone, Debug)]
pub struct LearnedNogood {
    pub(crate) predicates: Vec<Predicate>,
    pub(crate) backjump_level: usize,
}
