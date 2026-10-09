use super::ReifiedChecker;
use super::ReifiedPropagator;
use crate::propagation::DomainEvents;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::PropagatorSpec;
use crate::propagation::RuntimeCheckers;
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

    fn create(
        self,
        mut context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let ReifiedPropagatorArgs {
            propagator,
            reification_literal,
        } = self;

        let PropagatorSpec {
            mut registration,
            propagator,
            checkers,
        } = propagator.create(context.reborrow());

        // The local ID for the reification literal will be one larger than the largest ID
        // registered by the wrapped propagator.
        let reification_literal_id = registration
            .iter()
            .map(|(_, _, lid)| lid)
            .max()
            .expect("cannot reify propagators that do not register all variables immediately")
            .successor();

        registration.add(
            &self.reification_literal,
            DomainEvents::BOUNDS,
            reification_literal_id,
        );

        let mut wrapped_checkers = RuntimeCheckers::empty();
        for (inference_code, checker) in checkers.into_iter() {
            let _ = wrapped_checkers.add_conflict_checker(
                inference_code.tag(),
                inference_code.label(),
                ReifiedChecker {
                    inner: checker,
                    reification_literal,
                },
            );
        }

        let name = format!("Reified({})", propagator.name());

        let propagator = ReifiedPropagator {
            propagator,
            reification_literal,
            reification_literal_id,
            name,
            reason_buffer: vec![],
        };

        PropagatorSpec {
            registration,
            checkers: wrapped_checkers,
            propagator,
        }
    }
}
