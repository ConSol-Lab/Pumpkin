use super::variables::Literal;
#[cfg(doc)]
use crate::results::unsatisfiable::UnsatisfiableUnderAssumptions;

/// A core in terms of the assumption literals; see
/// [`UnsatisfiableUnderAssumptions::extract_core`] for more information on the definition
/// and what a core exactly is.
#[derive(Debug, Clone)]
pub enum Core {
    /// An empty core; i.e. the problem is unsatisfiable.
    Empty,
    /// A root level core, the stored literal
    /// ([`Core::RootLevel::root_level_assumption_literal`]) is the provided assumption which
    /// is conflicting at the root level.
    RootLevel {
        root_level_assumption_literal: Literal,
    },
    /// A standard core (i.e. not an empty core nor a root-level core); the stored literals
    /// [`Core::Standard::negated_assumption_literals`] are the negations of the provided
    /// assumptions which are part of the core.
    Standard {
        negated_assumption_literals: Vec<Literal>,
    },
}

impl Core {
    pub fn len(&self) -> usize {
        match self {
            Core::Empty => 0,
            Core::RootLevel {
                root_level_assumption_literal: _,
            } => 1,
            Core::Standard {
                negated_assumption_literals,
            } => negated_assumption_literals.len(),
        }
    }

    /// Returns whether the core is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns whether the core is unit.
    pub fn is_unit(&self) -> bool {
        match self {
            Core::Empty => false,
            Core::RootLevel {
                root_level_assumption_literal: _,
            } => true,
            Core::Standard {
                negated_assumption_literals,
            } => negated_assumption_literals.len() == 1,
        }
    }

    /// Returns whether a core is a core at the root level.
    pub fn is_a_root_level_core(&self) -> bool {
        matches!(
            self,
            Core::RootLevel {
                root_level_assumption_literal: _
            }
        )
    }

    /// Returns the negation of the assumption [`Literal`]s which are part of the core.
    ///
    /// # Example
    /// If we have the instance `all-different(x, y, z)` where `x, y, z ∈ {0, 1, 2}` and the
    /// assumptions `[[x = 1], [y <= 1], [y != 0]]` then this function would return `[[x != 1], [y >
    /// 1], [y = 0]]`.
    pub fn get_negated_assumption_literals(&self) -> Vec<Literal> {
        match self {
            Core::Empty => vec![],
            Core::RootLevel {
                root_level_assumption_literal,
            } => vec![!(*root_level_assumption_literal)],
            Core::Standard {
                negated_assumption_literals,
            } => negated_assumption_literals.to_vec(),
        }
    }

    /// Returns the core in terms of the assumption [`Literal`]s.
    ///
    /// # Example
    /// If we have the instance `all-different(x, y, z)` where `x, y, z ∈ {0, 1, 2}` and the
    /// assumptions `[[x = 1], [y <= 1], [y != 0]]` then this function would return `[[x = 1], [y <=
    /// 1], [y != 0]]`. Note that it does not always return *all* of the assumption literals as the
    /// core.
    pub fn get_core(&self) -> Vec<Literal> {
        match self {
            Core::Empty => vec![],
            Core::RootLevel {
                root_level_assumption_literal,
            } => vec![*root_level_assumption_literal],
            Core::Standard {
                negated_assumption_literals,
            } => negated_assumption_literals
                .iter()
                .map(|negated_assumption_literal| !(*negated_assumption_literal))
                .collect(),
        }
    }
}
