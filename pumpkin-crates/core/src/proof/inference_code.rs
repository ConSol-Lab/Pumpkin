use std::num::NonZero;

#[cfg(doc)]
use crate::Solver;
use crate::containers::HashMap;
use crate::containers::StorageKey;

/// An identifier for constraints, which is used to relate constraints from the model to steps in
/// the proof. Under the hood, a tag is just a [`NonZero<u32>`]. The underlying integer can be
/// obtained through the [`Into`] implementation.
///
/// Constraint tags only be created through [`Solver::new_constraint_tag()`]. This is a conscious
/// decision, as learned constraints will also need to be tagged, which means the solver has to be
/// responsible for maintaining their uniqueness.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ConstraintTag(NonZero<u32>);

impl From<ConstraintTag> for NonZero<u32> {
    fn from(value: ConstraintTag) -> Self {
        value.0
    }
}

impl ConstraintTag {
    /// Create a new tag directly.
    ///
    /// *Note*: Be careful when doing this. Regular construction should only be done through the
    /// state. It is important that constraint tags remain unique.
    pub(crate) fn from_non_zero(non_zero: NonZero<u32>) -> ConstraintTag {
        ConstraintTag(non_zero)
    }
}

impl StorageKey for ConstraintTag {
    fn index(&self) -> usize {
        self.0.get() as usize - 1
    }

    fn create_from_index(index: usize) -> Self {
        Self::from_non_zero(
            NonZero::new(index as u32 + 1).expect("the '+ 1' ensures the value is non-zero"),
        )
    }
}

/// An inference code is a combination of a constraint tag with an inference rule. Propagators
/// associate an inference code with every propagation to identify why that propagation happened
/// in terms of the constraint and inference that identified it.
///
/// The rule is identified by a [`RuleId`], which is only meaningful in the solver that assigned
/// it; the name of the rule is obtained from that solver.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InferenceCode(ConstraintTag, RuleId);

impl InferenceCode {
    /// Create a new inference code from a [`ConstraintTag`] and a [`RuleId`].
    pub(crate) fn new(tag: ConstraintTag, rule: RuleId) -> Self {
        InferenceCode(tag, rule)
    }

    /// Create an inference code with the rule [`UNKNOWN_RULE`], which every solver knows.
    ///
    /// This should be avoided as much as possible. This is likely only useful for writing unit
    /// tests.
    pub fn unknown_rule(tag: ConstraintTag) -> Self {
        InferenceCode(tag, RuleId::UNKNOWN)
    }

    /// Get the constraint tag.
    pub fn tag(&self) -> ConstraintTag {
        self.0
    }

    /// Get the identifier of the inference rule.
    pub fn rule(&self) -> RuleId {
        self.1
    }
}

/// The name of the rule of [`InferenceCode::unknown_rule`].
pub const UNKNOWN_RULE: &str = "unknown";

/// Identifies an inference rule in the solver that assigned it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RuleId(u32);

impl RuleId {
    const UNKNOWN: RuleId = RuleId(0);
}

/// The names of the inference rules used by a solver, each with the [`RuleId`] that the solver
/// assigned to it. A name is assigned an identifier when it is first used.
#[derive(Clone, Debug)]
pub(crate) struct InferenceRules {
    names: Vec<Box<str>>,
    ids: HashMap<Box<str>, RuleId>,
}

impl Default for InferenceRules {
    fn default() -> Self {
        let mut rules = InferenceRules {
            names: vec![],
            ids: HashMap::default(),
        };
        let unknown = rules.id(UNKNOWN_RULE);
        debug_assert_eq!(unknown, RuleId::UNKNOWN);
        rules
    }
}

impl InferenceRules {
    /// The identifier of the rule with the given name.
    pub(crate) fn id(&mut self, name: &str) -> RuleId {
        if let Some(&id) = self.ids.get(name) {
            return id;
        }

        let id = RuleId(u32::try_from(self.names.len()).expect("fewer than 2^32 rules"));
        self.names.push(name.into());
        let _ = self.ids.insert(name.into(), id);
        id
    }

    /// The name of the rule with the given identifier.
    pub(crate) fn name(&self, id: RuleId) -> &str {
        &self.names[id.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_name_built_at_runtime_gets_the_identifier_of_the_same_name() {
        let mut rules = InferenceRules::default();

        let first = rules.id(&format!("HalfReified({})", "linear"));
        let second = rules.id("HalfReified(linear)");

        assert_eq!(first, second);
        assert_eq!(rules.name(first), "HalfReified(linear)");
        assert_ne!(first, RuleId::UNKNOWN);
    }
}
