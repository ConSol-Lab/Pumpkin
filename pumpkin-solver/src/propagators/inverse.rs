use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::{LocalId, PropagationContextMut, Propagator, PropagatorInitialisationContext};
use crate::engine::variables::IntegerVariable;

/// Propagator for the inverse constraint.
/// 
/// This propagator enforces the relationship between two arrays of integer variables (`lhs` and `rhs`),
/// such that `lhs[i] = j` implies `rhs[j] = i`, and vice versa.
#[derive(Clone, Debug)]
pub(crate) struct InversePropagator<VA, VB, const N: usize> {
    lhs: [VA; N],
    rhs: [VB; N],
}

impl<VA, VB, const N: usize> InversePropagator<VA, VB, N> {
    pub(crate) fn new(lhs: [VA; N], rhs: [VB; N]) -> Self {
        Self { lhs, rhs }
    }
}

impl<VA: IntegerVariable + 'static, VB: IntegerVariable + 'static, const N: usize> Propagator
    for InversePropagator<VA, VB, N>
{
    fn name(&self) -> &str {
        "Inverse"
    }


    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), crate::predicates::PropositionalConjunction> {
        for (i, var) in self.lhs.iter().enumerate() {
            context.register(var.clone(), DomainEvents::ANY_INT, LocalId::from(i.try_into().unwrap()));
        }
        for (i, var) in self.rhs.iter().enumerate() {
            context.register(var.clone(), DomainEvents::ANY_INT, LocalId::from((N + i).try_into().unwrap()));
        }
        Ok(())
    }


    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        // enforce arc consistency
        for i in 0..N {
            let lhs_values: Vec<_> = context.iterate_domain(&self.lhs[i]).collect();
            for value in lhs_values {
                // remove illegal vals
                if value < 0 || value >= N as i32 {
                    context.remove(&self.lhs[i], value, conjunction!([self.lhs[i] != value]))?;
                    continue;
                }
    
                // if lhs[i] = j rhs[j] must include i
                let is_in_rhs = context
                    .iterate_domain(&self.rhs[value as usize])
                    .any(|v| v == i as i32);
                if !is_in_rhs {
                    context.remove(&self.lhs[i], value, conjunction!([self.rhs[value as usize] != i as i32]))?;
                }
            }
    
            let rhs_values: Vec<_> = context.iterate_domain(&self.rhs[i]).collect();
            for value in rhs_values {
                //remove illegal vals
                if value < 0 || value >= N as i32 {
                    context.remove(&self.rhs[i], value, conjunction!([self.rhs[i] != value]))?;
                    continue;
                }
    
                // if rhs[i] = j lhs[j] must include i
                let is_in_lhs = context
                    .iterate_domain(&self.lhs[value as usize])
                    .any(|v| v == i as i32);
                if !is_in_lhs {
                    context.remove(&self.rhs[i], value, conjunction!([self.lhs[value as usize] != i as i32]))?;
                }
            }
        }
    


        Ok(())
    }    
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::propagators::inverse::InversePropagator;
    use crate::engine::variables::DomainId; 
    use crate::engine::test_solver::TestSolver;

    #[test]
    fn test_inverse_propagator_simple() {
        let mut solver = TestSolver::default();

        const N: usize = 3; 
        let lhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];
        let rhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];

        let propagator = InversePropagator::new(lhs, rhs);
        let propagator_id = solver.new_propagator(propagator).unwrap();

        solver.propagate_until_fixed_point(propagator_id).unwrap();
        for i in 0..N {
            for j in 0..N {
                let contains_lhs = lhs[i].contains(&solver.assignments, j as i32);
                let contains_rhs = rhs[j].contains(&solver.assignments, i as i32);
                assert_eq!(
                    contains_lhs, contains_rhs,
                    "Initial consistency failed: lhs[{i}] = {j} and rhs[{j}] = {i}"
                );
            }
        }
    }


    #[test]
    fn test_inverse_propagator_removal() {
        let mut solver = TestSolver::default();

        const N: usize = 3;
        let lhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];
        let rhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];

        let propagator = InversePropagator::new(lhs, rhs);
        let propagator_id = solver.new_propagator(propagator).unwrap();

        lhs[0].remove(&mut solver.assignments, 2, None).unwrap();
        solver.propagate_until_fixed_point(propagator_id).unwrap();

        assert!(
            !rhs[2].contains(&solver.assignments, 0),
            "rhs[2] should not contain 0 after removal from lhs[0]"
        );
    }

    #[test]
    fn test_inverse_propagator_bounds() {
        let mut solver = TestSolver::default();

        const N: usize = 5;
        let lhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];
        let rhs: [DomainId; N] = [solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1),
                                solver.new_variable(0, N as i32 - 1)];

        let propagator = InversePropagator::new(lhs, rhs);
        let propagator_id = solver.new_propagator(propagator).unwrap();

        lhs[0].set_lower_bound(&mut solver.assignments, 3, None).unwrap();
        solver.propagate_until_fixed_point(propagator_id).unwrap();

        assert_eq!(
            rhs[3].lower_bound(&solver.assignments),
            0,
            "Lower bound of rhs[3] should be 0 after propagation"
        );
        assert_eq!(
            rhs[3].upper_bound(&solver.assignments),
            N as i32 - 1,
            "Upper bound of rhs[3] should be {} after propagation",
            N - 1
        );
    }

    #[test]
    fn test_inverse_propagator_rhs_empty_domain() {
        let mut solver = TestSolver::default();

        const N: usize = 3;
        let lhs: [DomainId; N] = [
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
        ];
        let rhs: [DomainId; N] = [
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
            solver.new_variable(0, N as i32 - 1),
        ];
    
        let propagator = InversePropagator::new(lhs, rhs);
        let propagator_id = solver.new_propagator(propagator).unwrap();

        lhs[0].remove(&mut solver.assignments, 1, None).unwrap();
        lhs[1].remove(&mut solver.assignments, 1, None).unwrap();
        lhs[2].remove(&mut solver.assignments, 1, None).unwrap();

        let result = solver.propagate(propagator_id);

        assert!(
            result.is_err(),
            "Propagator should detect inconsistency due to empty domain in rhs[1]"
        );
    }
    
}
