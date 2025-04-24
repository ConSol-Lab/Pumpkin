mod assignments;
pub(crate) mod domain_events;
mod event_sink;
pub(crate) mod opaque_domain_event;
pub(crate) mod propagation;
mod propagator_queue;
pub(crate) mod reason;
pub(crate) mod test_solver;
mod trailed;
mod watch_list_cp;

pub(crate) use assignments::Assignments;
pub(crate) use assignments::EmptyDomain;
pub(crate) use event_sink::*;
pub(crate) use propagator_queue::PropagatorQueue;
pub(crate) use trailed::*;
pub(crate) use watch_list_cp::IntDomainEvent;
pub(crate) use watch_list_cp::WatchListCP;
pub(crate) use watch_list_cp::Watchers;

#[cfg(test)]
mod tests {
    use assignments::Assignments;

    use crate::conjunction;
    use crate::engine::conflict_analysis::SemanticMinimiser;
    use crate::engine::cp::assignments;
    use crate::engine::propagation::PropagationContextMut;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::reason::ReasonStore;
    use crate::engine::TrailedValues;

    #[test]
    fn test_no_update_reason_store_if_no_update_lower_bound() {
        let mut assignments = Assignments::default();
        let mut trailed_values = TrailedValues::default();
        let domain = assignments.grow(5, 10);

        let mut reason_store = ReasonStore::default();
        assert_eq!(reason_store.len(), 0);
        {
            let mut semantic_miniser = SemanticMinimiser::default();
            let mut context = PropagationContextMut::new(
                &mut trailed_values,
                &mut assignments,
                &mut reason_store,
                &mut semantic_miniser,
                PropagatorId(0),
            );

            let result = context.set_lower_bound(&domain, 2, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }

    #[test]
    fn test_no_update_reason_store_if_no_update_upper_bound() {
        let mut assignments = Assignments::default();
        let mut trailed_values = TrailedValues::default();
        let domain = assignments.grow(5, 10);

        let mut reason_store = ReasonStore::default();

        assert_eq!(reason_store.len(), 0);
        {
            let mut semantic_miniser = SemanticMinimiser::default();
            let mut context = PropagationContextMut::new(
                &mut trailed_values,
                &mut assignments,
                &mut reason_store,
                &mut semantic_miniser,
                PropagatorId(0),
            );

            let result = context.set_upper_bound(&domain, 15, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }

    #[test]
    fn test_no_update_reason_store_if_no_update_remove() {
        let mut assignments = Assignments::default();
        let mut trailed_values = TrailedValues::default();
        let domain = assignments.grow(5, 10);

        let mut reason_store = ReasonStore::default();

        assert_eq!(reason_store.len(), 0);
        {
            let mut semantic_miniser = SemanticMinimiser::default();
            let mut context = PropagationContextMut::new(
                &mut trailed_values,
                &mut assignments,
                &mut reason_store,
                &mut semantic_miniser,
                PropagatorId(0),
            );

            let result = context.remove(&domain, 15, conjunction!());
            assert!(result.is_ok());
        }
        assert_eq!(reason_store.len(), 0);
    }
}
