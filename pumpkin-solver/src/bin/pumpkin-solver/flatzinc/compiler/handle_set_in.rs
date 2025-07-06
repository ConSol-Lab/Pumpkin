//! Scan through all constraint definition and determine whether a `set_in` constraint is present;
//! if this is the case then update the domain of the variable directly.
use super::context::CompilationContext;
use crate::flatzinc::error::FlatZincError;

pub(crate) fn run(context: &mut CompilationContext) -> Result<(), FlatZincError> {
    for (_, constraint_item) in &context.constraints {
        let flatzinc::ConstraintItem {
            id,
            exprs,
            annos: _,
        } = constraint_item;
        if id != "set_in" {
            continue;
        }

        let set = context.resolve_set_constant(&exprs[1])?;

        let id = context.identifiers.get_interned(match &exprs[0] {
            flatzinc::Expr::VarParIdentifier(id) => id,
            _ => return Err(FlatZincError::UnexpectedExpr),
        });

        let mut domain = context.integer_equivalences.get_mut_domain(&id);

        // We take the intersection between the two domains
        let new_domain = domain.merge(&set.into());
        *domain = new_domain;
    }
    Ok(())
}
