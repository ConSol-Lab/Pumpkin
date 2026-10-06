use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::ConflictRule;
use crate::propagation::ConstructedPropagator;
use crate::propagation::DomainEvents;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagators::HalfReified;
use crate::propagators::HalfReifiedDescription;
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
    type Rule = HalfReified<WrappedArgs::Rule>;

    fn constraint_description(
        &self,
    ) -> HalfReifiedDescription<<WrappedArgs::Rule as ConflictRule>::Description> {
        HalfReifiedDescription {
            inner: self.propagator.constraint_description(),
            reification_literal: self.reification_literal,
        }
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.propagator.constraint_tag()
    }

    fn create(
        self,
        context: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> ConstructedPropagator<Self::PropagatorImpl> {
        let ReifiedPropagatorArgs {
            propagator,
            reification_literal,
        } = self;

        // The wrapped propagator makes the inferences of the half reified rule.
        let ConstructedPropagator {
            mut events_to_register,
            propagator,
        } = propagator.create(context, inference_code);

        // The local ID for the reification literal will be one larger than the largest ID
        // registered by the wrapped propagator.
        let reification_literal_id = events_to_register
            .iter()
            .map(|(_, _, lid)| lid)
            .max()
            .expect("cannot reify propagators that do not register all variables immediately")
            .successor();

        events_to_register.add(
            &reification_literal,
            DomainEvents::BOUNDS,
            reification_literal_id,
        );

        let name = format!("Reified({})", propagator.name());

        let propagator = ReifiedPropagator {
            propagator,
            reification_literal,
            reification_literal_id,
            name,
            reason_buffer: vec![],
        };

        ConstructedPropagator {
            events_to_register,
            propagator,
        }
    }
}
