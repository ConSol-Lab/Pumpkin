//! A model variable declared without bounds, `var int: x`, gets the solver's default bounds,
//! which the checker of a croc proof does not know. A proof whose derivations rely on them does
//! not check, so the compiler warns about each such variable.

use log::warn;

use super::context::CompilationContext;
use super::context::Domain;
use crate::flatzinc::MAX_INTEGER_VALUE;
use crate::flatzinc::MIN_INTEGER_VALUE;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;

pub(crate) fn run(ast: &FlatZincAst, context: &mut CompilationContext) {
    let default = Domain::IntervalDomain {
        lb: MIN_INTEGER_VALUE,
        ub: MAX_INTEGER_VALUE,
    };
    for single_var_decl in &ast.single_variables {
        let SingleVarDecl::IntInRange { id, .. } = single_var_decl else {
            continue;
        };
        let id = context.identifiers.get_interned(id);
        if context.equivalences.is_defined(&id) && context.equivalences.domain(&id) == default {
            warn!(
                "'{id}' has no declared bounds; a croc proof that relies on the solver's default \
                 bounds for it does not check"
            );
        }
    }
}
