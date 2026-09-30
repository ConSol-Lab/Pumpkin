use std::num::NonZero;
use std::sync::Arc;

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
    names: Vec<&'static str>,
    ids: HashMap<&'static str, RuleId>,
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
    pub(crate) fn id(&mut self, name: &'static str) -> RuleId {
        *self.ids.entry(name).or_insert_with(|| {
            let id = RuleId(u32::try_from(self.names.len()).expect("fewer than 2^32 rules"));
            self.names.push(name);
            id
        })
    }

    /// The name of the rule with the given identifier.
    pub(crate) fn name(&self, id: RuleId) -> &'static str {
        self.names[id.0 as usize]
    }
}

#[doc(hidden)]
pub fn convert_label_name(ident_str: &str) -> Arc<str> {
    use convert_case::Casing;

    ident_str.to_case(convert_case::Case::Snake).into()
}

/// Conveniently creates [`InferenceLabel`] for use in a propagator.
///
/// In case it is desirable, the exact string that is printed in the DRCP proof can be
/// provided as a second parameter. Otherwise, the type name is converted to snake
///
/// # Example
/// ```ignore
/// declare_inference_label!(SomeInference);
/// declare_inference_label!(OtherInference, "label");
///
/// // Now we can use `SomeInference` and `OtherInference` when creating an inference
/// // code as it implements `InferenceLabel`.
/// ```
/// case.
#[macro_export]
macro_rules! declare_inference_label {
    ($v:vis $name:ident) => {
        declare_inference_label!($v $name, $crate::proof::convert_label_name(stringify!($name)));
    };

    ($v:vis $name:ident, $label:expr) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        $v struct $name;

        declare_inference_label!(@impl_trait $name, std::sync::Arc::from($label));
    };

    (@impl_trait $name:ident, $label:expr) => {
        impl $crate::proof::InferenceLabel for $name {
            fn to_str(&self) -> std::sync::Arc<str> {
                static LABEL: std::sync::OnceLock<std::sync::Arc<str>> = std::sync::OnceLock::new();

                let label = LABEL.get_or_init(|| $label);

                std::sync::Arc::clone(label)
            }
        }
    };
}

/// A label of the inference mechanism that identifies a particular inference. It is combined with a
/// [`ConstraintTag`] to create an [`InferenceCode`].
///
/// There may be different inference algorithms for the same contraint that are incomparable in
/// terms of propagation strength. To discriminate between these algorithms, the inference label is
/// used.
///
/// Conceptually, the inference label is a string. To aid with auto-complete, we introduce
/// this as a strongly-typed concept. For most cases, creating an inference label is done with the
/// [`declare_inference_label`] macro.
pub trait InferenceLabel {
    /// Returns the string-representation of the inference label.
    ///
    /// Typically different instances of the same propagator will use the same inference label.
    /// Users are encouraged to share the string allocation, which is why the return value is
    /// `Arc<str>`.
    fn to_str(&self) -> Arc<str>;
}

impl InferenceLabel for Arc<str> {
    fn to_str(&self) -> Arc<str> {
        Arc::clone(self)
    }
}

declare_inference_label!(pub Unknown);
