use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

#[derive(Debug, fzn_rs::FlatZincConstraint)]
pub(crate) enum FlatZincConstraints {
    #[name("int_lin_le")]
    LinearLeq {
        weights: fzn_rs::ArrayExpr<i32>,
        variables: fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>,
        bound: i32,
    },
    #[name("int_lin_eq")]
    LinearEq {
        weights: fzn_rs::ArrayExpr<i32>,
        variables: fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>,
        bound: i32,
    },
    #[name("pumpkin_cumulative")]
    Cumulative {
        start_times: fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>,
        durations: fzn_rs::ArrayExpr<i32>,
        resource_usages: fzn_rs::ArrayExpr<i32>,
        capacity: i32,
    },
    #[name("pumpkin_all_different")]
    AllDifferent(fzn_rs::ArrayExpr<fzn_rs::VariableExpr<i32>>),
}

pub(crate) type FlatZincModel = fzn_rs::TypedInstance<i32, FlatZincConstraints>;

/// Create a domain for a flatzinc variable.
pub(crate) fn create_domain_for_variable(
    state: &mut State,
    name: &str,
    variable: &fzn_rs::ast::Variable<()>,
) -> DomainId {
    match &variable.domain.node {
        fzn_rs::ast::Domain::UnboundedInt => todo!("unbounded integers are not supported yet"),

        fzn_rs::ast::Domain::Bool => state.new_interval_variable(0, 1, Some(name.into())),

        fzn_rs::ast::Domain::Int(domain) => {
            assert!(
                domain.is_continuous(),
                "sparse domains are not yet supported"
            );

            state.new_interval_variable(
                *domain.lower_bound() as i32,
                *domain.upper_bound() as i32,
                Some(name.into()),
            )
        }
    }
}
