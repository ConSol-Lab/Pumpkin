use pumpkin_core::variables::DomainId;

use crate::branching::autonomous_search::AutonomousSearch;
use crate::branching::independent_variable_value_brancher::IndependentVariableValueBrancher;
use crate::value_selection::RandomSplitter;
use crate::variable_selection::RandomSelector;

/// A brancher which makes use of VSIDS \[1\] and solution-based phase saving (both adapted for CP).
///
/// If VSIDS does not contain any (unfixed) predicates then it will default to the
/// [`IndependentVariableValueBrancher`].
///
/// Create an instance using [`DefaultBrancher::default_over_all_variables`].
///
/// # Bibliography
/// \[1\] M. W. Moskewicz, C. F. Madigan, Y. Zhao, L. Zhang, and S. Malik, ‘Chaff: Engineering an
/// efficient SAT solver’, in Proceedings of the 38th annual Design Automation Conference, 2001.
///
/// \[2\] E. Demirović, G. Chu, and P. J. Stuckey, ‘Solution-based phase saving for CP: A
/// value-selection heuristic to simulate local search behavior in complete solvers’, in the
/// proceedings of the Principles and Practice of Constraint Programming (CP 2018).
pub type DefaultBrancher =
    AutonomousSearch<IndependentVariableValueBrancher<DomainId, RandomSelector, RandomSplitter>>;
