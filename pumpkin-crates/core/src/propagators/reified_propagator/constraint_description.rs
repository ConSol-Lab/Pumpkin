use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::propagation::ConstraintDescription;
use crate::propagation::LocalId;
use crate::variables::Literal;

/// The description of a constraint that only has to hold when the reification literal is true.
#[derive(Clone, Debug)]
pub struct HalfReifiedDescription<Description> {
    /// The description of the constraint that is reified.
    pub inner: Description,
    pub reification_literal: Literal,
}

impl<Description: ConstraintDescription> ConstraintDescription
    for HalfReifiedDescription<Description>
{
    fn scope(&self) -> Scope {
        // Whether the inner constraint has to hold depends on the reification literal, so the
        // literal is part of the scope, under a local id after those of the inner constraint.
        let mut scope = self.inner.scope();
        let literal_id = scope
            .domains()
            .map(|(local_id, _)| local_id.successor())
            .max()
            .unwrap_or(LocalId::from(0));
        self.reification_literal
            .add_to_scope(&mut scope, literal_id);
        scope
    }
}
