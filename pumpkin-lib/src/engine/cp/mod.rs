mod assignments_integer;
pub(crate) mod domain_events;
mod event_sink;
pub(crate) mod opaque_domain_event;
pub(crate) mod propagation;
mod propagator_queue;
pub(crate) mod reason;
pub(crate) mod test_helper;
mod variable_literal_mappings;
mod watch_list_cp;
mod watch_list_propositional;

pub(crate) use assignments_integer::AssignmentsInteger;
pub(crate) use assignments_integer::EmptyDomain;
pub(crate) use propagator_queue::PropagatorQueue;
pub(crate) use variable_literal_mappings::VariableLiteralMappings;
pub(crate) use watch_list_cp::IntDomainEvent;
pub(crate) use watch_list_cp::WatchListCP;
pub(crate) use watch_list_cp::Watchers;
pub(crate) use watch_list_propositional::*;

#[cfg(test)]
mod tests {
    use assignments_integer::AssignmentsInteger;

    use crate::conjunction;
    use crate::engine::cp::assignments_integer;
    use crate::engine::propagation::PropagationContextMut;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::propagation::PropagatorVariable;
    use crate::engine::reason::ReasonStore;
    use crate::engine::variables::Literal;
    use crate::engine::variables::PropositionalVariable;
    use crate::engine::AssignmentsPropositional;

    #[test]
    fn test_no_update_reason_store_if_no_update_lower_bound() {
        let mut assignments_integer = AssignmentsInteger::default();
        let domain = assignments_integer.grow(5, 10);

        let mut reason_store = ReasonStore::default();
        let mut assignments_propositional = AssignmentsPropositional::default();

        assert_eq!(reason_store.len(), 0);
        {
            let mut context = PropagationContextMut::new(
                &mut assignments_integer,
                &mut reason_store,
                &mut assignments_propositional,
                PropagatorId(0),
            );

            let result =
                context.set_lower_bound(&PropagatorVariable::new(domain), 2, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }

    #[test]
    fn test_no_update_reason_store_if_no_update_upper_bound() {
        let mut assignments_integer = AssignmentsInteger::default();
        let domain = assignments_integer.grow(5, 10);

        let mut reason_store = ReasonStore::default();
        let mut assignments_propositional = AssignmentsPropositional::default();

        assert_eq!(reason_store.len(), 0);
        {
            let mut context = PropagationContextMut::new(
                &mut assignments_integer,
                &mut reason_store,
                &mut assignments_propositional,
                PropagatorId(0),
            );

            let result =
                context.set_upper_bound(&PropagatorVariable::new(domain), 15, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }

    #[test]
    fn test_no_update_reason_store_if_no_update_remove() {
        let mut assignments_integer = AssignmentsInteger::default();
        let domain = assignments_integer.grow(5, 10);

        let mut reason_store = ReasonStore::default();
        let mut assignments_propositional = AssignmentsPropositional::default();

        assert_eq!(reason_store.len(), 0);
        {
            let mut context = PropagationContextMut::new(
                &mut assignments_integer,
                &mut reason_store,
                &mut assignments_propositional,
                PropagatorId(0),
            );

            let result = context.remove(&PropagatorVariable::new(domain), 15, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }

    #[test]
    fn test_no_update_reason_store_if_fixed_literal() {
        let mut assignments_integer = AssignmentsInteger::default();
        let mut reason_store = ReasonStore::default();
        let mut assignments_propositional = AssignmentsPropositional::default();
        assignments_propositional.grow();
        let literal = Literal::new(PropositionalVariable::new(0), true);
        assignments_propositional.enqueue_decision_literal(literal);

        assert!(assignments_propositional.is_literal_assigned_true(literal));
        assert_eq!(reason_store.len(), 0);
        {
            let mut context = PropagationContextMut::new(
                &mut assignments_integer,
                &mut reason_store,
                &mut assignments_propositional,
                PropagatorId(0),
            );

            let result =
                context.assign_literal(&PropagatorVariable::new(literal), false, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }
}
