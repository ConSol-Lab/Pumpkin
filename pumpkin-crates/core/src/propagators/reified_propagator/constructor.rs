#[cfg(feature = "check-consistency")]
use crate::checkers::BoxedRetentionChecker;
use crate::propagation::DomainEvents;
#[cfg(feature = "check-consistency")]
use crate::propagation::LocalId;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
#[cfg(feature = "check-consistency")]
use crate::propagators::ReifiedConsistencyChecker;
use crate::propagators::ReifiedPropagator;
use crate::variables::Literal;

/// A [`PropagatorConstructor`] for the reified propagator.
#[derive(Clone, Debug)]
pub struct ReifiedPropagatorArgs<WrappedArgs> {
    pub propagator: WrappedArgs,
    pub reification_literal: Literal,
}

impl<WrappedArgs, WrappedPropagator> PropagatorConstructor for ReifiedPropagatorArgs<WrappedArgs>
where
    WrappedArgs: PropagatorConstructor<PropagatorImpl = WrappedPropagator>,
    WrappedPropagator: Propagator + Clone,
{
    type PropagatorImpl = ReifiedPropagator<WrappedPropagator>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let ReifiedPropagatorArgs {
            propagator,
            reification_literal,
        } = self;

        let propagator = propagator.create(context.reborrow());

        let reification_literal_id = context.get_next_local_id();

        context.register(
            reification_literal,
            DomainEvents::BOUNDS,
            reification_literal_id,
        );

        #[cfg(feature = "check-propagations")]
        wrap_inference_checkers(&mut context, reification_literal);

        #[cfg(feature = "check-consistency")]
        wrap_consistency_checkers(&mut context, reification_literal, reification_literal_id);

        let name = format!("Reified({})", propagator.name());

        ReifiedPropagator {
            propagator,
            reification_literal,
            reification_literal_id,
            name,
            reason_buffer: vec![],
        }
    }
}

/// Wrap inference checkers: the literal is already known, no local id needed.
#[cfg(feature = "check-propagations")]
fn wrap_inference_checkers(
    context: &mut PropagatorConstructorContext<'_>,
    reification_literal: Literal,
) {
    use crate::propagators::ReifiedChecker;

    for (_, checker) in context.pending_inference_checkers.iter_mut() {
        replace_with::replace_with_or_abort(checker, |inner_checker| {
            use pumpkin_checking::BoxedChecker;

            Box::new(ReifiedChecker {
                inner: BoxedChecker::from(inner_checker),
                reification_literal,
            })
        });
    }
}

/// Wrap consistency checkers: add the reification literal to each scope with the now-known
/// local id, then wrap the checker.
#[cfg(feature = "check-consistency")]
fn wrap_consistency_checkers(
    context: &mut PropagatorConstructorContext<'_>,
    reification_literal: Literal,
    reification_literal_id: LocalId,
) {
    use crate::checkers::ScopeItem;

    for (scope, checker) in context.pending_consistency_checkers.iter_mut() {
        reification_literal.add_to_scope(scope, reification_literal_id);

        replace_with::replace_with_or_abort(checker, |inner_checker| {
            BoxedRetentionChecker::from(ReifiedConsistencyChecker {
                inner: inner_checker,
                reification_literal,
                reification_literal_id,
            })
        });
    }
}
