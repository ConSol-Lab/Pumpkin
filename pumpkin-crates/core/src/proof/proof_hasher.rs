//! A deterministic hasher for the maps of the proof writer, which are keyed on small integers:
//! predicates, constraint tags and domains.
//!
//! The solver's maps hash with FNV, whose high bits vary little over such keys. The hash table
//! compares the stored key whenever those bits match, so a large map of the proof writer, such as
//! the reference of every atomic constraint written, spent much of its lookup time comparing
//! keys. This hasher mixes every bit of the key into the whole hash.

use std::hash::BuildHasherDefault;
use std::hash::Hasher;

/// A hasher that folds the words it is given and mixes them with the finaliser of SplitMix64.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct ProofHasher {
    state: u64,
}

impl ProofHasher {
    fn add(&mut self, word: u64) {
        self.state = (self.state.rotate_left(29) ^ word).wrapping_mul(0x9e37_79b9_7f4a_7c15);
    }
}

impl Hasher for ProofHasher {
    fn write(&mut self, bytes: &[u8]) {
        for chunk in bytes.chunks(8) {
            let mut word = [0; 8];
            word[..chunk.len()].copy_from_slice(chunk);
            self.add(u64::from_le_bytes(word));
        }
    }

    fn write_u8(&mut self, value: u8) {
        self.add(u64::from(value));
    }

    fn write_u16(&mut self, value: u16) {
        self.add(u64::from(value));
    }

    fn write_u32(&mut self, value: u32) {
        self.add(u64::from(value));
    }

    fn write_u64(&mut self, value: u64) {
        self.add(value);
    }

    fn write_i32(&mut self, value: i32) {
        self.add(u64::from(value as u32));
    }

    fn write_usize(&mut self, value: usize) {
        self.add(value as u64);
    }

    fn finish(&self) -> u64 {
        let mut z = self.state;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }
}

/// The builder of [`ProofHasher`]s.
pub(crate) type ProofBuildHasher = BuildHasherDefault<ProofHasher>;

/// A map of the proof writer.
pub(crate) type ProofHashMap<K, V> = crate::containers::HashMap<K, V, ProofBuildHasher>;

/// A set of the proof writer.
pub(crate) type ProofHashSet<K> = crate::containers::HashSet<K, ProofBuildHasher>;

#[cfg(test)]
mod tests {
    use std::hash::BuildHasher;

    use super::*;

    #[test]
    fn consecutive_keys_differ_in_the_high_bits() {
        let builder = ProofBuildHasher::default();
        let tops: std::collections::BTreeSet<u64> = (0_u32..1024)
            .map(|key| builder.hash_one(key) >> 57)
            .collect();
        assert!(tops.len() > 100, "only {} distinct top bits", tops.len());
    }
}
