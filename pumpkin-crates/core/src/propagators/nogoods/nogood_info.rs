use bitfield_struct::bitfield;

/// A struct which represents a nogood (i.e. a list of [`Predicate`]s which cannot all be true at
/// the same time).
///
/// It additionally contains certain fields related to how the clause was created/activity.
///
/// The LBD and the flags are packed into a single [`NogoodFlags`] to keep this struct at 8 bytes.
#[derive(Clone, Debug, Default)]
pub(crate) struct NogoodInfo {
    /// The LBD and the flags of the nogood.
    flags: NogoodFlags,
    /// The activity score of the nogood.
    pub(crate) activity: f32,
}

/// The LBD and the flags of a nogood, packed into a single `u32`.
#[bitfield(u32)]
struct NogoodFlags {
    /// The LBD score of the nogood; this is an indication of how "good" the nogood is.
    ///
    /// Saturates at [`MAX_LBD`].
    #[bits(29)]
    lbd: u32,
    /// Indicates whether the nogood is a learned nogood or not.
    #[bits(1)]
    is_learned: bool,
    /// Whether the nogood has been marked as deleted; this means that it can be replaced by
    /// another nogood in the future.
    #[bits(1)]
    is_deleted: bool,
    /// Whether to not allow the nogood to have their activity bumped.
    ///
    /// A single nogood can be bumped at most twice when learning a new nogood. It can appear at
    /// most once during conflict analysis, and at most once during recursive minimisation.
    /// Setting this to true prevents a nogood from being bumped twice if it is used in both
    /// conflict analysis and recursive minisation.
    ///
    /// TODO: not clear whether this is a problem or whether it makes sense.
    #[bits(1)]
    block_bumps: bool,
}

/// The largest LBD which can be stored in a [`NogoodInfo`]; larger values are saturated.
const MAX_LBD: u32 = (1 << 29) - 1;

impl NogoodInfo {
    pub(crate) fn new_learned_nogood_info(lbd: u32) -> Self {
        NogoodInfo {
            flags: NogoodFlags::new()
                .with_lbd(lbd.min(MAX_LBD))
                .with_is_learned(true),
            activity: 0.0,
        }
    }

    pub(crate) fn new_permanent_nogood_info() -> Self {
        NogoodInfo {
            flags: NogoodFlags::new(),
            activity: 0.0,
        }
    }

    /// The LBD score of the nogood; this is an indication of how "good" the nogood is.
    pub(crate) fn lbd(&self) -> u32 {
        self.flags.lbd()
    }

    /// Sets the LBD score of the nogood, saturating at [`MAX_LBD`].
    pub(crate) fn set_lbd(&mut self, lbd: u32) {
        self.flags.set_lbd(lbd.min(MAX_LBD))
    }

    /// Indicates whether the nogood is a learned nogood or not.
    pub(crate) fn is_learned(&self) -> bool {
        self.flags.is_learned()
    }

    /// Whether the nogood has been marked as deleted.
    pub(crate) fn is_deleted(&self) -> bool {
        self.flags.is_deleted()
    }

    /// Marks the nogood as deleted.
    pub(crate) fn mark_deleted(&mut self) {
        self.flags.set_is_deleted(true)
    }

    /// Whether the activity of the nogood is currently not allowed to be bumped.
    pub(crate) fn block_bumps(&self) -> bool {
        self.flags.block_bumps()
    }

    /// Sets whether the activity of the nogood is allowed to be bumped.
    pub(crate) fn set_block_bumps(&mut self, block_bumps: bool) {
        self.flags.set_block_bumps(block_bumps)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nogood_info_is_eight_bytes() {
        assert_eq!(size_of::<NogoodInfo>(), 8);
    }

    #[test]
    fn lbd_saturates() {
        let mut info = NogoodInfo::new_learned_nogood_info(u32::MAX);
        assert_eq!(info.lbd(), MAX_LBD);
        assert!(info.is_learned());

        info.set_lbd(5);
        assert_eq!(info.lbd(), 5);
        assert!(info.is_learned());
        assert!(!info.is_deleted());
        assert!(!info.block_bumps());
    }
}
