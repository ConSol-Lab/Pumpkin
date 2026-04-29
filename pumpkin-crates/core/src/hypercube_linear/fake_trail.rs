use crate::containers::HashMap;
use crate::containers::KeyedVec;
use crate::hypercube_linear::explanation::HypercubeLinearExplanation;
use crate::hypercube_linear::trail_view::TrailView;
use crate::predicates::Predicate;
use crate::variables::DomainId;

struct FakeAssignment {
    predicate: Predicate,
    /// 1-based; position 0 = "before all assignments" = initial state.
    trail_position: usize,
    checkpoint: usize,
}

pub(super) struct FakeTrail {
    current_checkpoint: usize,
    assignments: Vec<FakeAssignment>,
    reasons: HashMap<Predicate, HypercubeLinearExplanation>,
    initial_bounds: KeyedVec<DomainId, (i32, i32)>,
}

pub(super) struct FakeTrailBuilder {
    current_checkpoint: usize,
    current_builder_checkpoint: usize,
    next_trail_position: usize,
    assignments: Vec<FakeAssignment>,
    reasons: HashMap<Predicate, HypercubeLinearExplanation>,
    initial_bounds: KeyedVec<DomainId, (i32, i32)>,
}

impl FakeTrailBuilder {
    pub(super) fn domain(&mut self, lower: i32, upper: i32) -> DomainId {
        self.initial_bounds.push((lower, upper))
    }

    pub(super) fn at_checkpoint(mut self, checkpoint: usize) -> Self {
        self.current_builder_checkpoint = checkpoint;
        self
    }

    pub(super) fn assign(
        mut self,
        predicate: Predicate,
        reason: HypercubeLinearExplanation,
    ) -> Self {
        self.next_trail_position += 1;
        let tp = self.next_trail_position;
        self.assignments.push(FakeAssignment {
            predicate,
            trail_position: tp,
            checkpoint: self.current_builder_checkpoint,
        });
        let _ = self.reasons.insert(predicate, reason);
        self
    }

    pub(super) fn build(self) -> FakeTrail {
        FakeTrail {
            current_checkpoint: self.current_checkpoint,
            assignments: self.assignments,
            reasons: self.reasons,
            initial_bounds: self.initial_bounds,
        }
    }
}

impl FakeTrail {
    pub(super) fn builder(current_checkpoint: usize) -> FakeTrailBuilder {
        FakeTrailBuilder {
            current_checkpoint,
            current_builder_checkpoint: 0,
            next_trail_position: 0,
            assignments: vec![],
            reasons: HashMap::default(),
            initial_bounds: KeyedVec::default(),
        }
    }
}

impl TrailView for FakeTrail {
    fn trail_position(&self, predicate: Predicate) -> Option<usize> {
        self.assignments
            .iter()
            .find(|a| a.predicate == predicate)
            .map(|a| a.trail_position)
    }

    fn checkpoint_for_predicate(&self, predicate: Predicate) -> Option<usize> {
        let domain = predicate.get_domain();
        let rhs = predicate.get_right_hand_side();

        let (initial_lb, initial_ub) = self
            .initial_bounds
            .get(domain)
            .copied()
            .unwrap_or((i32::MIN, i32::MAX));

        // Check if initial bounds already satisfy the predicate.
        if predicate_satisfied_by_bounds(predicate, initial_lb, initial_ub) {
            return Some(0);
        }

        // Scan assignments in trail order, tracking running lb/ub for the domain.
        let mut lb = initial_lb;
        let mut ub = initial_ub;
        let mut domain_assignments: Vec<_> = self
            .assignments
            .iter()
            .filter(|a| a.predicate.get_domain() == domain)
            .collect();
        domain_assignments.sort_by_key(|a| a.trail_position);

        for assignment in domain_assignments {
            let v = assignment.predicate.get_right_hand_side();
            if assignment.predicate.is_lower_bound_predicate() {
                lb = lb.max(v);
            } else if assignment.predicate.is_upper_bound_predicate() {
                ub = ub.min(v);
            }
            let satisfied = if predicate.is_lower_bound_predicate() {
                lb >= rhs
            } else if predicate.is_upper_bound_predicate() {
                ub <= rhs
            } else {
                false
            };
            if satisfied {
                return Some(assignment.checkpoint);
            }
        }

        None
    }

    fn current_checkpoint(&self) -> usize {
        self.current_checkpoint
    }

    fn trail_position_at_checkpoint(&self, checkpoint: usize) -> usize {
        // Return the last trail position of an assignment at this checkpoint,
        // or 0 (initial state) if no assignments exist at this checkpoint.
        self.assignments
            .iter()
            .filter(|a| a.checkpoint == checkpoint)
            .map(|a| a.trail_position)
            .max()
            .unwrap_or(0)
    }

    fn predicate_at_trail_position(&self, trail_position: usize) -> Predicate {
        self.assignments
            .iter()
            .find(|a| a.trail_position == trail_position)
            .map(|a| a.predicate)
            .expect("no assignment at trail position")
    }

    fn truth_value_at(&self, predicate: Predicate, trail_position: usize) -> Option<bool> {
        let domain = predicate.get_domain();
        let rhs = predicate.get_right_hand_side();
        let lb = self.lower_bound_at_trail_position(domain, trail_position);
        let ub = self.upper_bound_at_trail_position(domain, trail_position);

        if predicate.is_lower_bound_predicate() {
            if lb >= rhs {
                Some(true)
            } else if ub < rhs {
                Some(false)
            } else {
                None
            }
        } else if predicate.is_upper_bound_predicate() {
            if ub <= rhs {
                Some(true)
            } else if lb > rhs {
                Some(false)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn lower_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32 {
        let initial = self
            .initial_bounds
            .get(domain)
            .map(|(lb, _)| *lb)
            .unwrap_or(i32::MIN);

        self.assignments
            .iter()
            .filter(|a| {
                a.trail_position <= trail_position
                    && a.predicate.get_domain() == domain
                    && a.predicate.is_lower_bound_predicate()
            })
            .map(|a| a.predicate.get_right_hand_side())
            .fold(initial, i32::max)
    }

    fn upper_bound_at_trail_position(&self, domain: DomainId, trail_position: usize) -> i32 {
        let initial = self
            .initial_bounds
            .get(domain)
            .map(|(_, ub)| *ub)
            .unwrap_or(i32::MAX);

        self.assignments
            .iter()
            .filter(|a| {
                a.trail_position <= trail_position
                    && a.predicate.get_domain() == domain
                    && a.predicate.is_upper_bound_predicate()
            })
            .map(|a| a.predicate.get_right_hand_side())
            .fold(initial, i32::min)
    }

    fn reason_for(&mut self, predicate: Predicate) -> HypercubeLinearExplanation {
        self.reasons
            .get(&predicate)
            .cloned()
            .unwrap_or_else(|| panic!("no reason stored for predicate {predicate}"))
    }

    fn current_trail_position(&self) -> usize {
        self.assignments
            .iter()
            .map(|a| a.trail_position)
            .max()
            .unwrap_or(0)
    }
}

fn predicate_satisfied_by_bounds(predicate: Predicate, initial_lb: i32, initial_ub: i32) -> bool {
    use crate::predicates::PredicateType::*;

    let rhs = predicate.get_right_hand_side();

    match predicate.get_predicate_type() {
        LowerBound => rhs <= initial_lb,
        NotEqual => rhs < initial_lb || rhs > initial_ub,
        Equal => rhs == initial_lb && initial_lb == initial_ub,
        UpperBound => rhs >= initial_ub,
    }
}
