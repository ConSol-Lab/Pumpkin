use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use log::warn;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::Literal;
use pumpkin_lib::Solver;

use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

pub(crate) struct CompilationContext<'a> {
    /// The solver to compile the FlatZinc into.
    pub(crate) solver: &'a mut Solver,

    /// All identifiers occuring in the model. The identifiers are interned, to support cheap
    /// cloning.
    pub(crate) identifiers: Identifiers,

    /// Identifiers of variables that are outputs.
    pub(crate) outputs: Vec<Output>,

    /// Literal which is always true
    pub(crate) true_literal: Literal,
    /// Literal which is always false
    pub(crate) false_literal: Literal,
    /// All boolean parameters.
    pub(crate) boolean_parameters: HashMap<Rc<str>, bool>,
    /// All boolean array parameters.
    pub(crate) boolean_array_parameters: HashMap<Rc<str>, Rc<[bool]>>,
    /// A mapping from boolean model variables to solver literals.
    pub(crate) boolean_variable_map: HashMap<Rc<str>, Literal>,
    /// A mapping from boolean variable array identifiers to slices of literals.
    pub(crate) boolean_variable_arrays: HashMap<Rc<str>, Rc<[Literal]>>,
    /// The equivalence classes for literals.
    pub(crate) literal_equivalences: VariableEquivalences,
    // A literal which is always true, can be used when using bool constants in the solver
    // pub(crate) constant_bool_true: BooleanDomainId,
    // A literal which is always false, can be used when using bool constants in the solver
    // pub(crate) constant_bool_false: BooleanDomainId,
    /// All integer parameters.
    pub(crate) integer_parameters: HashMap<Rc<str>, i32>,
    /// All integer array parameters.
    pub(crate) integer_array_parameters: HashMap<Rc<str>, Rc<[i32]>>,
    /// A mapping from integer model variables to solver literals.
    pub(crate) integer_variable_map: HashMap<Rc<str>, DomainId>,
    /// The equivalence classes for integer variables. The associated data is the bounds for the
    /// domain of the representative of the equivalence class..
    pub(crate) integer_equivalences: VariableEquivalences,
    /// Only instantiate single domain for every constant variable.
    pub(crate) constant_domain_ids: HashMap<i32, DomainId>,
    /// A mapping from integer variable array identifiers to slices of domain ids.
    pub(crate) integer_variable_arrays: HashMap<Rc<str>, Rc<[DomainId]>>,

    /// All set parameters.
    pub(crate) set_constants: HashMap<Rc<str>, Set>,
}

/// A set parameter.
#[derive(Clone, Debug)]
pub(crate) enum Set {
    /// A set defined by the interval `lower_bound..=upper_bound`.
    Interval { lower_bound: i32, upper_bound: i32 },
    /// A set defined by some values.
    Sparse { values: Box<[i32]> },
}

impl CompilationContext<'_> {
    pub(crate) fn new(solver: &mut Solver) -> CompilationContext<'_> {
        let true_literal = solver.get_true_literal();
        let false_literal = solver.get_false_literal();

        CompilationContext {
            solver,
            identifiers: Default::default(),

            outputs: Default::default(),

            true_literal,
            false_literal,
            boolean_parameters: Default::default(),
            boolean_array_parameters: Default::default(),
            boolean_variable_map: Default::default(),
            boolean_variable_arrays: Default::default(),
            literal_equivalences: Default::default(),
            integer_parameters: Default::default(),
            integer_array_parameters: Default::default(),
            integer_variable_map: Default::default(),
            integer_equivalences: Default::default(),
            constant_domain_ids: Default::default(),
            integer_variable_arrays: Default::default(),

            set_constants: Default::default(),
        }
    }

    pub(crate) fn is_identifier_parameter(&mut self, identifier: &str) -> bool {
        self.integer_parameters.contains_key(identifier)
    }

    // pub fn resolve_bool_constant(&self, identifier: &str) -> Option<bool> {
    //     self.boolean_parameters.get(identifier).copied()
    // }

    // pub fn resolve_int_constant(&self, identifier: &str) -> Option<i32> {
    //     self.integer_parameters.get(identifier).copied()
    // }

    pub(crate) fn resolve_bool_variable(
        &mut self,
        expr: &flatzinc::Expr,
    ) -> Result<Literal, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => self.resolve_bool_variable_from_identifier(id),
            flatzinc::Expr::Bool(value) => {
                if *value {
                    Ok(self.solver.get_true_literal())
                } else {
                    Ok(self.solver.get_false_literal())
                }
            }
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn resolve_bool_variable_from_identifier(
        &self,
        identifier: &str,
    ) -> Result<Literal, FlatZincError> {
        if let Some(literal) = self
            .boolean_variable_map
            .get(&self.literal_equivalences.representative(identifier))
        {
            Ok(*literal)
        } else {
            self.boolean_parameters
                .get(&self.literal_equivalences.representative(identifier))
                .map(|value| {
                    if *value {
                        self.solver.get_true_literal()
                    } else {
                        self.solver.get_false_literal()
                    }
                })
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: identifier.into(),
                    expected_type: "bool variable".into(),
                })
        }
    }

    pub(crate) fn resolve_bool_variable_array(
        &self,
        expr: &flatzinc::Expr,
    ) -> Result<Rc<[Literal]>, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => {
                if let Some(literal) = self.boolean_variable_arrays.get(id.as_str()) {
                    Ok(Rc::clone(literal))
                } else {
                    self.boolean_array_parameters
                        .get(id.as_str())
                        .map(|array| {
                            array
                                .iter()
                                .map(|value| {
                                    if *value {
                                        self.solver.get_true_literal()
                                    } else {
                                        self.solver.get_false_literal()
                                    }
                                })
                                .collect()
                        })
                        .ok_or_else(|| FlatZincError::InvalidIdentifier {
                            identifier: id.as_str().into(),
                            expected_type: "boolean variable array".into(),
                        })
                }
            }
            flatzinc::Expr::ArrayOfBool(array) => array
                .iter()
                .map(|elem| match elem {
                    flatzinc::BoolExpr::VarParIdentifier(id) => {
                        self.resolve_bool_variable_from_identifier(id)
                    }
                    flatzinc::BoolExpr::Bool(true) => Ok(self.solver.get_true_literal()),
                    flatzinc::BoolExpr::Bool(false) => Ok(self.solver.get_false_literal()),
                })
                .collect(),
            flatzinc::Expr::ArrayOfInt(array) => array
                .iter()
                .map(|elem| match elem {
                    flatzinc::IntExpr::VarParIdentifier(id) => {
                        self.resolve_bool_variable_from_identifier(id)
                    }
                    _ => panic!("Bool search should not be over integer variable"),
                })
                .collect(),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub(crate) fn resolve_array_integer_constants(
        &self,
        expr: &flatzinc::Expr,
    ) -> Result<Rc<[i32]>, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => self
                .integer_array_parameters
                .get(id.as_str())
                .cloned()
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: id.as_str().into(),
                    expected_type: "constant integer array".into(),
                }),
            flatzinc::Expr::ArrayOfInt(exprs) => exprs
                .iter()
                .map(|e| self.resolve_int_expr_to_const(e))
                .collect::<Result<Rc<[i32]>, _>>(),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub(crate) fn resolve_integer_constant_from_id(
        &mut self,
        identifier: &str,
    ) -> Result<DomainId, FlatZincError> {
        let value = self.resolve_int_expr_to_const(&flatzinc::IntExpr::VarParIdentifier(
            identifier.to_owned(),
        ))?;
        Ok(*self.constant_domain_ids.entry(value).or_insert_with(|| {
            self.solver
                .new_named_bounded_integer(value, value, identifier.to_owned())
        }))
    }

    pub(crate) fn resolve_integer_constant_from_expr(
        &self,
        expr: &flatzinc::Expr,
    ) -> Result<i32, FlatZincError> {
        fn try_into_int_expr(expr: flatzinc::Expr) -> Option<flatzinc::IntExpr> {
            match expr {
                flatzinc::Expr::VarParIdentifier(id) => {
                    Some(flatzinc::IntExpr::VarParIdentifier(id))
                }
                flatzinc::Expr::Int(value) => Some(flatzinc::IntExpr::Int(value)),
                _ => None,
            }
        }
        try_into_int_expr(expr.clone())
            .ok_or(FlatZincError::UnexpectedExpr)
            .and_then(|e| self.resolve_int_expr_to_const(&e))
    }

    pub(crate) fn resolve_int_expr_to_const(
        &self,
        expr: &flatzinc::IntExpr,
    ) -> Result<i32, FlatZincError> {
        match expr {
            flatzinc::IntExpr::Int(value) => i32::try_from(*value).map_err(Into::into),
            flatzinc::IntExpr::VarParIdentifier(id) => self
                .integer_parameters
                .get(id.as_str())
                .copied()
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: id.as_str().into(),
                    expected_type: "constant integer".into(),
                }),
        }
    }

    pub(crate) fn resolve_int_expr(
        &mut self,
        expr: &flatzinc::IntExpr,
    ) -> Result<DomainId, FlatZincError> {
        match expr {
            flatzinc::IntExpr::Int(value) => Ok(*self
                .constant_domain_ids
                .entry(*value as i32)
                .or_insert_with(|| {
                    self.solver.new_named_bounded_integer(
                        *value as i32,
                        *value as i32,
                        value.to_string(),
                    )
                })),
            flatzinc::IntExpr::VarParIdentifier(id) => {
                self.resolve_integer_variable_from_identifier(id)
            }
        }
    }

    pub(crate) fn resolve_integer_variable(
        &mut self,
        expr: &flatzinc::Expr,
    ) -> Result<DomainId, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => {
                self.resolve_integer_variable_from_identifier(id)
            }
            flatzinc::Expr::Int(val) => Ok(*self
                .constant_domain_ids
                .entry(*val as i32)
                .or_insert_with(|| {
                    self.solver
                        .new_named_bounded_integer(*val as i32, *val as i32, val.to_string())
                })),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub(crate) fn resolve_integer_variable_from_identifier(
        &mut self,
        identifier: &str,
    ) -> Result<DomainId, FlatZincError> {
        if let Some(domain_id) = self
            .integer_variable_map
            .get(&self.integer_equivalences.representative(identifier))
        {
            Ok(*domain_id)
        } else {
            self.integer_parameters
                .get(&self.integer_equivalences.representative(identifier))
                .map(|value| {
                    *self.constant_domain_ids.entry(*value).or_insert_with(|| {
                        self.solver
                            .new_named_bounded_integer(*value, *value, value.to_string())
                    })
                })
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: identifier.into(),
                    expected_type: "integer variable".into(),
                })
        }
    }

    pub(crate) fn resolve_integer_variable_array(
        &mut self,
        expr: &flatzinc::Expr,
    ) -> Result<Rc<[DomainId]>, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => {
                if let Some(domain_id) = self.integer_variable_arrays.get(id.as_str()) {
                    Ok(Rc::clone(domain_id))
                } else {
                    self.integer_array_parameters
                        .get(id.as_str())
                        .map(|array| {
                            array
                                .iter()
                                .map(|value| {
                                    *self.constant_domain_ids.entry(*value).or_insert_with(|| {
                                        self.solver.new_named_bounded_integer(
                                            *value,
                                            *value,
                                            value.to_string(),
                                        )
                                    })
                                })
                                .collect()
                        })
                        .ok_or_else(|| FlatZincError::InvalidIdentifier {
                            identifier: id.as_str().into(),
                            expected_type: "integer variable array".into(),
                        })
                }
            }
            flatzinc::Expr::ArrayOfInt(array) => array
                .iter()
                .map(|elem| self.resolve_int_expr(elem))
                .collect::<Result<Rc<[DomainId]>, _>>(),

            // The AST is not correct here. Since the type of an in-place array containing only
            // identifiers cannot be determined, and the parser attempts to parse ArrayOfBool
            // first, we may also get this variant even when parsing integer arrays.
            flatzinc::Expr::ArrayOfBool(array) => array
                .iter()
                .map(|elem| {
                    if let flatzinc::BoolExpr::VarParIdentifier(id) = elem {
                        self.resolve_integer_variable_from_identifier(id)
                    } else {
                        Err(FlatZincError::UnexpectedExpr)
                    }
                })
                .collect(),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub(crate) fn resolve_set_constant(&self, expr: &flatzinc::Expr) -> Result<Set, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => {
                self.set_constants.get(id.as_str()).cloned().ok_or(
                    FlatZincError::InvalidIdentifier {
                        identifier: id.clone().into(),
                        expected_type: "set of int".into(),
                    },
                )
            }

            flatzinc::Expr::Set(set_literal) => match set_literal {
                flatzinc::SetLiteralExpr::IntInRange(lower_bound_expr, upper_bound_expr) => {
                    let lower_bound = self.resolve_int_expr_to_const(lower_bound_expr)?;
                    let upper_bound = self.resolve_int_expr_to_const(upper_bound_expr)?;

                    Ok(Set::Interval {
                        lower_bound,
                        upper_bound,
                    })
                }
                flatzinc::SetLiteralExpr::SetInts(exprs) => {
                    let values = exprs
                        .iter()
                        .map(|expr| self.resolve_int_expr_to_const(expr))
                        .collect::<Result<_, _>>()?;

                    Ok(Set::Sparse { values })
                }

                flatzinc::SetLiteralExpr::BoundedFloat(_, _)
                | flatzinc::SetLiteralExpr::SetFloats(_) => panic!("float values are unsupported"),
            },

            flatzinc::Expr::Bool(_)
            | flatzinc::Expr::Int(_)
            | flatzinc::Expr::Float(_)
            | flatzinc::Expr::ArrayOfBool(_)
            | flatzinc::Expr::ArrayOfInt(_)
            | flatzinc::Expr::ArrayOfFloat(_)
            | flatzinc::Expr::ArrayOfSet(_) => Err(FlatZincError::UnexpectedExpr),
        }
    }
}

#[derive(Default, Debug)]
pub(crate) struct Identifiers {
    interned_identifiers: HashSet<Rc<str>>,
}

impl Identifiers {
    pub(crate) fn get_interned(&mut self, identifier: &str) -> Rc<str> {
        if let Some(interned) = self.interned_identifiers.get(identifier) {
            Rc::clone(interned)
        } else {
            let interned: Rc<str> = identifier.into();
            let _ = self.interned_identifiers.insert(Rc::clone(&interned));

            interned
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct VariableEquivalences {
    /// The equivalence classes.
    classes: Vec<BTreeSet<Rc<str>>>,
    /// The domain information associated with each equivalence class.
    domains: Vec<Domain>,

    /// For each variable, the index into `classes` is the equivalence class it belongs to.
    belongs_to: HashMap<Rc<str>, usize>,
}

impl VariableEquivalences {
    /// Merge the equivalence classes of the two variables.
    ///
    /// We distinguish between the following edge cases:
    ///  - The two variables are already in the same equivalence class: this is a no-op.
    ///  - One of the variables, or both, do not belong to an equivalence class. In this case the
    ///    method will panic.
    pub(crate) fn merge(&mut self, variable_1: Rc<str>, variable_2: Rc<str>) {
        let equiv_1_idx = self.belongs_to.get(&variable_1).copied().unwrap();
        let equiv_2_idx = self.belongs_to.get(&variable_2).copied().unwrap();

        // The two variables are in the same equivalence class already.
        if equiv_1_idx == equiv_2_idx {
            return;
        }

        let equiv_1 = self.classes.swap_remove(equiv_1_idx);
        let domain_1 = self.domains.swap_remove(equiv_1_idx);

        if equiv_1_idx != self.classes.len() {
            // rewire the last class that was moved by calls to `swap_remove`
            self.classes[equiv_1_idx].iter().for_each(|class| {
                let _ = self.belongs_to.insert(Rc::clone(class), equiv_1_idx);
            });
        }

        self.classes[equiv_2_idx].extend(equiv_1);
        self.domains[equiv_2_idx].merge(domain_1);
        let _ = self.belongs_to.insert(variable_1, equiv_2_idx);
    }

    /// Create a new equivalence class with the given representative.
    pub(crate) fn create_equivalence_class(&mut self, representative: Rc<str>, lb: i32, ub: i32) {
        let _ = self
            .belongs_to
            .insert(Rc::clone(&representative), self.classes.len());
        self.classes.push([representative].into());
        self.domains
            .push(Domain::from_lower_bound_and_upper_bound(lb, ub));
    }

    /// Get the name of the representative variable of the equivalence class the given variable
    /// belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub(crate) fn representative(&self, variable: &str) -> Rc<str> {
        let equiv_idx = self.belongs_to[variable];

        self.classes[equiv_idx].first().cloned().unwrap()
    }

    /// Get the domain for the given variable, based on the equivalence class it belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub(crate) fn domain(&self, variable: &str) -> Domain {
        let equiv_idx = self.belongs_to[variable];

        self.domains[equiv_idx].clone()
    }

    pub(crate) fn get_mut_domain(&mut self, variable: &str) -> &mut Domain {
        let equiv_idx = self.belongs_to[variable];

        &mut self.domains[equiv_idx]
    }

    pub(crate) fn is_defined(&self, variable: &str) -> bool {
        self.belongs_to.contains_key(variable)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Domain {
    IntervalDomain { lb: i32, ub: i32 },
    SparseDomain { values: Vec<i32> },
}

impl From<Set> for Domain {
    fn from(value: Set) -> Self {
        match value {
            Set::Interval {
                lower_bound,
                upper_bound,
            } => Domain::IntervalDomain {
                lb: lower_bound,
                ub: upper_bound,
            },
            Set::Sparse { values } => Domain::SparseDomain {
                values: values.to_vec(),
            },
        }
    }
}

impl Domain {
    pub(crate) fn merge(&mut self, other: Domain) {
        let domain = match (self.clone(), other) {
            (
                Domain::IntervalDomain { lb, ub },
                Domain::IntervalDomain {
                    lb: lb_other,
                    ub: ub_other,
                },
            ) => {
                let lb = i32::max(lb, lb_other);
                let ub = i32::min(ub, ub_other);

                Domain::from_lower_bound_and_upper_bound(lb, ub)
            }
            (Domain::SparseDomain { values }, Domain::IntervalDomain { lb, ub })
            | (Domain::IntervalDomain { lb, ub }, Domain::SparseDomain { values }) => {
                warn!("Merging `SparseDomain` with `IntervalDomain`, this could lead to memory issues depending on the implementation");
                // We take all of the values in the sparse set which lie within the interval; note
                // that we do not check whether the resulting values represent an interval
                Domain::SparseDomain {
                    values: values
                        .into_iter()
                        .filter(|value| *value >= lb && *value <= ub)
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
                    .into_iter()
                    .filter(|value| values_other.contains(value))
                    .collect::<Vec<_>>();
                Domain::SparseDomain {
                    values: intersection,
                }
            }
        };
        *self = domain
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
        assert_eq!(equivs.representative(&a), a);
        assert_eq!(
            equivs.domain(&a),
            Domain::from_lower_bound_and_upper_bound(0, 1)
        );

        equivs.create_equivalence_class(Rc::clone(&b), 1, 3);
        assert!(equivs.is_defined(&b));
        assert_eq!(equivs.representative(&b), b);
        assert_eq!(
            equivs.domain(&b),
            Domain::from_lower_bound_and_upper_bound(1, 3)
        );

        equivs.create_equivalence_class(Rc::clone(&c), 5, 10);
        assert!(equivs.is_defined(&c));
        assert_eq!(equivs.representative(&c), c);
        assert_eq!(
            equivs.domain(&c),
            Domain::from_lower_bound_and_upper_bound(5, 10)
        );

        equivs.merge(Rc::clone(&a), Rc::clone(&b));
        assert!(equivs.is_defined(&a));
        assert_eq!(equivs.representative(&a), a);
        assert_eq!(
            equivs.domain(&a),
            Domain::from_lower_bound_and_upper_bound(1, 1)
        );

        assert!(equivs.is_defined(&b));
        assert_eq!(equivs.representative(&b), a);
        assert_eq!(
            equivs.domain(&b),
            Domain::from_lower_bound_and_upper_bound(1, 1)
        );

        assert!(equivs.is_defined(&c));
        assert_eq!(equivs.representative(&c), c);
        assert_eq!(
            equivs.domain(&c),
            Domain::from_lower_bound_and_upper_bound(5, 10)
        );
    }

    #[test]
    fn merge_two_intervals() {
        let mut interval_1 = Domain::IntervalDomain { lb: -5, ub: 10 };
        let interval_2 = Domain::IntervalDomain { lb: 0, ub: 20 };

        interval_1.merge(interval_2);
        assert!(match interval_1 {
            Domain::IntervalDomain { lb, ub } => lb == 0 && ub == 10,
            Domain::SparseDomain { values: _ } => false,
        });
    }

    #[test]
    fn merge_sparse_domains() {
        let mut sparse_domain_1 = Domain::SparseDomain {
            values: vec![0, 1, 2],
        };

        let sparse_domain_2 = Domain::SparseDomain {
            values: vec![2, 3, 4],
        };
        sparse_domain_1.merge(sparse_domain_2);
        assert!(match sparse_domain_1 {
            Domain::IntervalDomain { lb: _, ub: _ } => false,
            Domain::SparseDomain { values } => values == vec![2],
        })
    }

    #[test]
    fn merge_sparse_with_interval() {
        let mut sparse_domain_1 = Domain::SparseDomain {
            values: vec![2, 3, 4],
        };
        let interval = Domain::IntervalDomain { lb: 3, ub: 20 };

        sparse_domain_1.merge(interval);
        assert!(match sparse_domain_1 {
            Domain::IntervalDomain { lb: _, ub: _ } => false,
            Domain::SparseDomain { values } => {
                values == vec![3, 4]
            }
        })
    }

    #[test]
    fn merge_interval_with_sparse() {
        let mut interval = Domain::IntervalDomain { lb: 3, ub: 20 };
        let sparse_domain_1 = Domain::SparseDomain {
            values: vec![2, 3, 4],
        };

        interval.merge(sparse_domain_1);
        assert!(match interval {
            Domain::IntervalDomain { lb: _, ub: _ } => false,
            Domain::SparseDomain { values } => {
                values == vec![3, 4]
            }
        })
    }
}
