use fnv::FnvBuildHasher;

pub type HashMap<K, V, Hasher = FnvBuildHasher> = std::collections::HashMap<K, V, Hasher>;
pub type HashSet<K, Hasher = FnvBuildHasher> = std::collections::HashSet<K, Hasher>;
