use crate::predicates::Predicate;

#[derive(Clone, Debug)]
pub(crate) struct LearnedNogood {
    pub(crate) predicates: Vec<Predicate>,
    pub(crate) backjump_level: usize,
}
