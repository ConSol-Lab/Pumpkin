use log::warn;
use pumpkin_solver::constraints::Constraint;
use pumpkin_solver::constraints::{self};
use pumpkin_solver::options::DecisionDiagramOptions;
use pumpkin_solver::variables::DomainId;

pub(crate) fn run(
    context: &mut super::context::CompilationContext<'_>,
    options: DecisionDiagramOptions,
) -> Result<bool, ()> {
    let constraint_groups = match options.intersection_strategy {
        pumpkin_solver::options::DecisionDiagramIntersection::None => context
            .dd_constraints
            .iter()
            .map(|c| vec![c.clone()])
            .collect::<Vec<_>>(),
        pumpkin_solver::options::DecisionDiagramIntersection::All => {
            vec![context.dd_constraints.clone()]
        }
    };
    let mut sat = true;
    for group in constraint_groups {
        match process_group(group, context, options) {
            Ok(mdd_graph) => {
                sat &= constraints::mdd(mdd_graph)
                    .post(context.solver, None)
                    .is_ok();
            }
            Err(_) => {
                warn!("Failed to compile an MDD from a constraint group");
            }
        }
    }
    Ok(sat)
}

fn process_group(
    constraints: Vec<mdd_compile::constraints::Constraint<DomainId>>,
    context: &mut super::context::CompilationContext<'_>,
    options: DecisionDiagramOptions,
) -> Result<mdd_compile::mdd::MddGraph<DomainId>, mdd_compile::mdd::MddConstructionError> {
    let var_ids = constraints
        .iter()
        .flat_map(|cons| cons.variables())
        .collect::<std::collections::HashSet<_>>();
    let mut mdd_builder = mdd_compile::mdd::MddBuilder::<DomainId>::new(options.max_width);
    for &var_id in var_ids {
        let lb = context.solver.lower_bound(&var_id);
        let ub = context.solver.upper_bound(&var_id);
        mdd_builder = mdd_builder.add_variable(var_id, lb, ub);
    }
    for constraint in constraints {
        mdd_builder = mdd_builder.add_constraint(constraint)?;
    }
    mdd_builder.build()
}
