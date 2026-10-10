//! Scan through all constraint definition and determine whether a `set_in` constraint is present;
//! if this is the case then restrict the domain of the variable, through its view of the
//! representative of its class, to the set.
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

        // The class's domain is that of its representative, of which the variable may be a
        // view; the set is pulled back through the view.
        context.equivalences.restrict(&id, &set.into());
    }
    Ok(())
}
