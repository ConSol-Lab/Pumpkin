//! Contains containers which are used by the solver.
mod key_generator;
mod key_value_heap;
mod keyed_vec;
mod sparse_set;

use fnv::FnvBuildHasher;
pub use key_generator::*;
pub use key_value_heap::*;
pub use keyed_vec::*;
pub(crate) use sparse_set::*;

/// [`std::collections::HashMap`] that defaults to a deterministic hasher.
#[allow(clippy::disallowed_types, reason = "this is how we define our HashMap")]
pub type HashMap<K, V, Hasher = FnvBuildHasher> = std::collections::HashMap<K, V, Hasher>;
/// [`std::collections::HashSet`] that defaults to a deterministic hasher.
#[allow(clippy::disallowed_types, reason = "this is how we define our HashSet")]
pub type HashSet<K, Hasher = FnvBuildHasher> = std::collections::HashSet<K, Hasher>;
