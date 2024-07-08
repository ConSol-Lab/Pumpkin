use fnv::FnvBuildHasher;

pub(crate) type HashMap<K, V, Hasher = FnvBuildHasher> = std::collections::HashMap<K, V, Hasher>;
pub(crate) type HashSet<K, Hasher = FnvBuildHasher> = std::collections::HashSet<K, Hasher>;
