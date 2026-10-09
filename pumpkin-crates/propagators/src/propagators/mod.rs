//! Contains propagator implementations that are used in Pumpkin.
//!
//! See the [`propagation`] for info on propagators.
#[cfg(doc)]
use pumpkin_core::propagation;

pub mod arithmetic;
pub mod cumulative;
pub mod disjunctive;
pub mod element;

#[cfg(test)]
mod solution_check_tests;

#[cfg(test)]
mod tests {
    use pumpkin_core::containers::HashSet;
    use pumpkin_core::propagation::ConflictRule;
    use pumpkin_core::propagators::hypercube_linear::HypercubeLinearRule;
    use pumpkin_core::propagators::nogoods::UnitNogoodRule;
    use pumpkin_core::variables::DomainId;

    use super::arithmetic::*;
    use super::cumulative::time_table::TimeTableRule;
    use super::disjunctive::DisjunctiveEdgeFindingRule;
    use super::element::ElementRule;

    #[test]
    fn rule_names_are_distinct() {
        // A new built-in rule adds its name here. The extended nogood rule shares the name of the
        // unit nogood rule on purpose, and the half reified rule is named after its inner rule.
        let names = [
            AbsoluteValueRule::<DomainId, DomainId>::name(),
            BinaryEqualsRule::<DomainId, DomainId>::name(),
            BinaryNotEqualsRule::<DomainId, DomainId>::name(),
            DivisionRule::<DomainId, DomainId, DomainId>::name(),
            IntegerMultiplicationRule::<DomainId, DomainId, DomainId>::name(),
            LinearLessOrEqualRule::<DomainId>::name(),
            LinearNotEqualRule::<DomainId>::name(),
            MaximumRule::<DomainId, DomainId>::name(),
            TimeTableRule::<DomainId>::name(),
            DisjunctiveEdgeFindingRule::<DomainId>::name(),
            ElementRule::<DomainId, DomainId, DomainId>::name(),
            HypercubeLinearRule::name(),
            UnitNogoodRule::name(),
            "initial_domain".into(),
        ];

        let distinct = names.iter().collect::<HashSet<_>>();
        assert_eq!(distinct.len(), names.len(), "{names:?}");
    }
}
