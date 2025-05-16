use std::num::NonZero;
use std::sync::Arc;

use crate::containers::StorageKey;
#[cfg(doc)]
use crate::Solver;

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
    /// constraint satisfaction solver. It is important that constraint tags remain unique.
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

/// An inference code is a combination of a constraint tag with an inference label. Propagators
/// associate an inference code with every propagation to identify why that propagation happened
/// in terms of the constraint and inference that identified it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InferenceCode(NonZero<u32>);

impl StorageKey for InferenceCode {
    fn index(&self) -> usize {
        self.0.get() as usize - 1
    }

    fn create_from_index(index: usize) -> Self {
        Self(NonZero::new(index as u32 + 1).expect("the '+ 1' ensures the value is non-zero"))
    }
}

/// Conveniently creates [`InferenceLabel`] for use in a propagator.
///
/// # Example
/// ```ignore
/// declare_inference_label!(SomeInference);
///
/// // Now we can use `SomeInference` when creating an inference code as it implements
/// // `InferenceLabel`.
/// ```
#[macro_export]
macro_rules! declare_inference_label {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        struct $name;

        declare_inference_label!(@impl_trait $name);
    };

    (@impl_trait $name:ident) => {
        impl $crate::proof::InferenceLabel for $name {
            fn to_str(&self) -> std::sync::Arc<str> {
                static LABEL: std::sync::OnceLock<std::sync::Arc<str>> = std::sync::OnceLock::new();

                let label = LABEL.get_or_init(|| {
                    let ident_str = stringify!($name);
                    let label = <&str as convert_case::Casing<&str>>::to_case(
                        &ident_str,
                        convert_case::Case::Snake,
                    );

                    std::sync::Arc::from(label)
                });

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
