use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::BTreeSet;
use std::rc::Rc;

use fzn_rs::ast::RangeList;
use fzn_rs::ArrayExpr;
use fzn_rs::VariableExpr;
use log::warn;
use pumpkin_solver::containers::HashMap;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::Literal;
use pumpkin_solver::Solver;

use crate::flatzinc::ast::Instance;
use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

pub(crate) struct CompilationContext<'a> {
    /// The solver to compile the FlatZinc into.
    pub(crate) solver: &'a mut Solver,

    /// Identifiers of variables that are outputs.
    pub(crate) outputs: Vec<Output>,

    /// Literal which is always true
    pub(crate) true_literal: Literal,
    /// Literal which is always false
    pub(crate) false_literal: Literal,
    /// A mapping from boolean model variables to solver literals.
    pub(crate) boolean_variable_map: HashMap<Rc<str>, Literal>,
    /// The equivalence classes for literals.
    pub(crate) literal_equivalences: VariableEquivalences,
    // A literal which is always true, can be used when using bool constants in the solver
    // pub(crate) constant_bool_true: BooleanDomainId,
    // A literal which is always false, can be used when using bool constants in the solver
    // pub(crate) constant_bool_false: BooleanDomainId,
    /// A mapping from integer model variables to solver literals.
    pub(crate) integer_variable_map: HashMap<Rc<str>, DomainId>,
    /// The equivalence classes for integer variables. The associated data is the bounds for the
    /// domain of the representative of the equivalence class..
    pub(crate) integer_equivalences: VariableEquivalences,
    /// Only instantiate single domain for every constant variable.
    pub(crate) constant_domain_ids: HashMap<i32, DomainId>,
}

impl CompilationContext<'_> {
    pub(crate) fn new(solver: &mut Solver) -> CompilationContext<'_> {
        let true_literal = solver.get_true_literal();
        let false_literal = solver.get_false_literal();

        CompilationContext {
            solver,

            outputs: Default::default(),

            true_literal,
            false_literal,
            boolean_variable_map: Default::default(),
            literal_equivalences: Default::default(),
            integer_variable_map: Default::default(),
            integer_equivalences: Default::default(),
            constant_domain_ids: Default::default(),
        }
    }

    pub(crate) fn resolve_bool_variable(
        &self,
        variable: &VariableExpr<bool>,
    ) -> Result<Literal, FlatZincError> {
        match variable {
            VariableExpr::Identifier(ident) => {
                let representative = self.literal_equivalences.representative(ident)?;

                self.boolean_variable_map
                    .get(&representative)
                    .copied()
                    .ok_or_else(|| FlatZincError::InvalidIdentifier {
                        identifier: Rc::clone(ident),
                        expected_type: "bool var".into(),
                    })
            }
            VariableExpr::Constant(true) => Ok(self.true_literal),
            VariableExpr::Constant(false) => Ok(self.false_literal),
        }
    }

    pub(crate) fn resolve_bool_variable_array(
        &self,
        instance: &Instance,
        array: &ArrayExpr<VariableExpr<bool>>,
    ) -> Result<Vec<Literal>, FlatZincError> {
        instance
            .resolve_array(array)
            .map_err(FlatZincError::UndefinedArray)?
            .map(|expr_result| {
                let expr = expr_result?;
                self.resolve_bool_variable(&expr)
            })
            .collect()
    }

    pub(crate) fn resolve_integer_variable(
        &mut self,
        variable: &VariableExpr<i32>,
    ) -> Result<DomainId, FlatZincError> {
        match variable {
            VariableExpr::Identifier(ident) => {
                let representative = self.integer_equivalences.representative(ident)?;

                self.integer_variable_map
                    .get(&representative)
                    .copied()
                    .ok_or_else(|| FlatZincError::InvalidIdentifier {
                        identifier: Rc::clone(ident),
                        expected_type: "int var".into(),
                    })
            }
            VariableExpr::Constant(value) => {
                Ok(*self.constant_domain_ids.entry(*value).or_insert_with(|| {
                    self.solver
                        .new_named_bounded_integer(*value, *value, value.to_string())
                }))
            }
        }
    }

    pub(crate) fn resolve_integer_array(
        &self,
        instance: &Instance,
        array: &ArrayExpr<i32>,
    ) -> Result<Vec<i32>, FlatZincError> {
        instance
            .resolve_array(array)
            .map_err(FlatZincError::UndefinedArray)?
            .map(|maybe_int| maybe_int.map_err(FlatZincError::from))
            .collect()
    }

    pub(crate) fn resolve_integer_variable_array(
        &mut self,
        instance: &Instance,
        array: &ArrayExpr<VariableExpr<i32>>,
    ) -> Result<Vec<DomainId>, FlatZincError> {
        instance
            .resolve_array(array)
            .map_err(FlatZincError::UndefinedArray)?
            .map(|expr_result| {
                let expr = expr_result?;
                self.resolve_integer_variable(&expr)
            })
            .collect()
    }
}

#[derive(Debug, Default)]
pub(crate) struct VariableEquivalences {
    /// For each variable, the equivalence class it belongs to.
    classes: HashMap<Rc<str>, Rc<RefCell<EquivalenceClass>>>,
}

#[derive(Debug)]
struct EquivalenceClass {
    /// The variables that are part of the equivalence class. We use a BTreeSet so that we can
    /// consistently get a representative, which will be the first element in the set.
    variables: BTreeSet<Rc<str>>,
    /// The domain to associate with these variables.
    domain: Domain,
}

impl VariableEquivalences {
    /// Merge the equivalence classes of the two variables.
    ///
    /// We distinguish between the following edge cases:
    ///  - The two variables are already in the same equivalence class: this is a no-op.
    ///  - One of the variables, or both, do not belong to an equivalence class. In this case the
    ///    method will panic.
    pub(crate) fn merge(&mut self, variable_1: Rc<str>, variable_2: Rc<str>) {
        let equiv_1 = Rc::clone(&self.classes[&variable_1]);
        let equiv_2 = Rc::clone(&self.classes[&variable_2]);

        let merged_domain = equiv_1.borrow().domain.merge(&equiv_2.borrow().domain);
        let merged_variables: BTreeSet<Rc<str>> = equiv_1
            .borrow()
            .variables
            .iter()
            .cloned()
            .chain(equiv_2.borrow().variables.iter().cloned())
            .collect();

        let new_equivalence_class = Rc::new(RefCell::new(EquivalenceClass {
            variables: merged_variables.clone(),
            domain: merged_domain,
        }));

        // Update the equivalence class for all the variables in the class.
        for variable in merged_variables {
            let _ = self
                .classes
                .insert(variable, Rc::clone(&new_equivalence_class));
        }
    }

    /// Create a new equivalence class with the given representative.
    pub(crate) fn create_equivalence_class(&mut self, representative: Rc<str>, lb: i32, ub: i32) {
        let equivalence_class = Rc::new(RefCell::new(EquivalenceClass {
            variables: [Rc::clone(&representative)].into_iter().collect(),
            domain: Domain::IntervalDomain { lb, ub },
        }));

        let _ = self.classes.insert(representative, equivalence_class);
    }

    /// Create a new equivalence class with the given representative.
    pub(crate) fn create_equivalence_class_sparse(
        &mut self,
        representative: Rc<str>,
        values: Vec<i32>,
    ) {
        let equivalence_class = Rc::new(RefCell::new(EquivalenceClass {
            variables: [Rc::clone(&representative)].into_iter().collect(),
            domain: Domain::SparseDomain { values },
        }));

        let _ = self.classes.insert(representative, equivalence_class);
    }

    /// Get the name of the representative variable of the equivalence class the given variable
    /// belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub(crate) fn representative(&self, variable: &str) -> Result<Rc<str>, FlatZincError> {
        let equiv_class =
            self.classes
                .get(variable)
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: variable.into(),
                    // Since you should never see this error message, we give a dummy value. We
                    // cannot panic, due to the `identify_output_arrays` implementation that will
                    // try to resolve non-existent variable names.
                    expected_type: "?".into(),
                })?;

        let ident = equiv_class
            .borrow()
            .variables
            .first()
            .cloned()
            .expect("all classes have at least one representative");

        Ok(ident)
    }

    /// Get the domain for the given variable, based on the equivalence class it belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub(crate) fn domain(&self, variable: &str) -> Domain {
        self.classes[variable].borrow().domain.clone()
    }

    pub(crate) fn get_mut_domain(&mut self, variable: &str) -> RefMut<'_, Domain> {
        RefMut::map(self.classes[variable].borrow_mut(), |class| {
            &mut class.domain
        })
    }

    pub(crate) fn is_defined(&self, variable: &str) -> bool {
        self.classes.contains_key(variable)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Domain {
    IntervalDomain { lb: i32, ub: i32 },
    SparseDomain { values: Vec<i32> },
}

impl From<&'_ RangeList<i32>> for Domain {
    fn from(value: &'_ RangeList<i32>) -> Self {
        if value.is_continuous() {
            Domain::IntervalDomain {
                lb: *value.lower_bound(),
                ub: *value.upper_bound(),
            }
        } else {
            let values = value.into_iter().collect::<_>();

            Domain::SparseDomain { values }
        }
    }
}

impl Domain {
    pub(crate) fn merge(&self, other: &Domain) -> Domain {
        match (self, other) {
            (
                Domain::IntervalDomain { lb, ub },
                Domain::IntervalDomain {
                    lb: lb_other,
                    ub: ub_other,
                },
            ) => {
                let lb = i32::max(*lb, *lb_other);
                let ub = i32::min(*ub, *ub_other);

                Domain::from_lower_bound_and_upper_bound(lb, ub)
            }
            (Domain::SparseDomain { values }, Domain::IntervalDomain { lb, ub })
            | (Domain::IntervalDomain { lb, ub }, Domain::SparseDomain { values }) => {
                warn!("Merging `SparseDomain` with `IntervalDomain`, this could lead to memory issues depending on the implementation");
                // We take all of the values in the sparse set which lie within the interval; note
                // that we do not check whether the resulting values represent an interval
                Domain::SparseDomain {
                    values: values
                        .iter()
                        .copied()
                        .filter(|value| *value >= *lb && *value <= *ub)
                        .collect::<Vec<_>>(),
                }
            }
            (
                Domain::SparseDomain { values },
                Domain::SparseDomain {
                    values: values_other,
                },
            ) => {
                // We simply take the intersection of the two
                let intersection = values
                    .iter()
                    .copied()
                    .filter(|value| values_other.contains(value))
                    .collect::<Vec<_>>();
                Domain::SparseDomain {
                    values: intersection,
                }
            }
        }
    }

    pub(crate) fn is_constant(&self) -> bool {
        match self {
            Domain::IntervalDomain { lb, ub } => lb == ub,
            Domain::SparseDomain { values } => values.len() == 1,
        }
    }

    pub(crate) fn from_lower_bound_and_upper_bound(lb: i32, ub: i32) -> Self {
        Domain::IntervalDomain { lb, ub }
    }

    pub(crate) fn into_boolean(self, solver: &mut Solver, name: String) -> Literal {
        match self {
            Domain::IntervalDomain { lb, ub } => {
                if lb == ub && lb == 1 {
                    solver.get_true_literal()
                } else if lb == ub && lb == 0 {
                    solver.get_false_literal()
                } else {
                    solver.new_named_literal(name)
                }
            }
            Domain::SparseDomain { values } => {
                if values.len() == 1 && values[0] == 1 {
                    solver.get_true_literal()
                } else if values.len() == 1 && values[0] == 0 {
                    solver.get_false_literal()
                } else {
                    solver.new_named_literal(name)
                }
            }
        }
    }

    pub(crate) fn into_variable(self, solver: &mut Solver, name: String) -> DomainId {
        match self {
            Domain::IntervalDomain { lb, ub } => solver.new_named_bounded_integer(lb, ub, name),
            Domain::SparseDomain { values } => solver.new_named_sparse_integer(values, name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merging_doesnt_break_other_vars() {
        let mut equivs = VariableEquivalences::default();
        let a = Rc::from("a");
        let b = Rc::from("b");
        let c = Rc::from("c");
        equivs.create_equivalence_class(Rc::clone(&a), 0, 1);
        assert!(equivs.is_defined(&a));
        assert_eq!(equivs.representative(&a).unwrap(), a);
        assert_eq!(
            equivs.domain(&a),
            Domain::from_lower_bound_and_upper_bound(0, 1)
        );

        equivs.create_equivalence_class(Rc::clone(&b), 1, 3);
        assert!(equivs.is_defined(&b));
        assert_eq!(equivs.representative(&b).unwrap(), b);
        assert_eq!(
            equivs.domain(&b),
            Domain::from_lower_bound_and_upper_bound(1, 3)
        );

        equivs.create_equivalence_class(Rc::clone(&c), 5, 10);
        assert!(equivs.is_defined(&c));
        assert_eq!(equivs.representative(&c).unwrap(), c);
        assert_eq!(
            equivs.domain(&c),
            Domain::from_lower_bound_and_upper_bound(5, 10)
        );

        equivs.merge(Rc::clone(&a), Rc::clone(&b));
        assert!(equivs.is_defined(&a));
        assert_eq!(equivs.representative(&a).unwrap(), a);
        assert_eq!(
            equivs.domain(&a),
            Domain::from_lower_bound_and_upper_bound(1, 1)
        );

        assert!(equivs.is_defined(&b));
        assert_eq!(equivs.representative(&b).unwrap(), a);
        assert_eq!(
            equivs.domain(&b),
            Domain::from_lower_bound_and_upper_bound(1, 1)
        );

        assert!(equivs.is_defined(&c));
        assert_eq!(equivs.representative(&c).unwrap(), c);
        assert_eq!(
            equivs.domain(&c),
            Domain::from_lower_bound_and_upper_bound(5, 10)
        );
    }

    #[test]
    fn merge_two_intervals() {
        let interval_1 = Domain::IntervalDomain { lb: -5, ub: 10 };
        let interval_2 = Domain::IntervalDomain { lb: 0, ub: 20 };

        let interval_1 = interval_1.merge(&interval_2);
        assert!(match interval_1 {
            Domain::IntervalDomain { lb, ub } => lb == 0 && ub == 10,
            Domain::SparseDomain { values: _ } => false,
        });
    }

    #[test]
    fn merge_sparse_domains() {
        let sparse_domain_1 = Domain::SparseDomain {
            values: vec![0, 1, 2],
        };

        let sparse_domain_2 = Domain::SparseDomain {
            values: vec![2, 3, 4],
        };
        let sparse_domain_1 = sparse_domain_1.merge(&sparse_domain_2);
        assert!(match sparse_domain_1 {
            Domain::IntervalDomain { lb: _, ub: _ } => false,
            Domain::SparseDomain { values } => values == vec![2],
        })
    }

    #[test]
    fn merge_sparse_with_interval() {
        let sparse_domain_1 = Domain::SparseDomain {
            values: vec![2, 3, 4],
        };
        let interval = Domain::IntervalDomain { lb: 3, ub: 20 };

        let sparse_domain_1 = sparse_domain_1.merge(&interval);
        assert!(match sparse_domain_1 {
            Domain::IntervalDomain { lb: _, ub: _ } => false,
            Domain::SparseDomain { values } => {
                values == vec![3, 4]
            }
        })
    }

    #[test]
    fn merge_interval_with_sparse() {
        let interval = Domain::IntervalDomain { lb: 3, ub: 20 };
        let sparse_domain_1 = Domain::SparseDomain {
            values: vec![2, 3, 4],
        };

        let interval = interval.merge(&sparse_domain_1);
        assert!(match interval {
            Domain::IntervalDomain { lb: _, ub: _ } => false,
            Domain::SparseDomain { values } => {
                values == vec![3, 4]
            }
        })
    }
}
