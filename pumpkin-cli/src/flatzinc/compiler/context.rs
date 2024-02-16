use std::collections::BTreeSet;
use std::rc::Rc;

use pumpkin_lib::basic_types::DomainId;
use pumpkin_lib::basic_types::HashMap;
use pumpkin_lib::basic_types::HashSet;
use pumpkin_lib::basic_types::Literal;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

use crate::flatzinc::instance::Output;
use crate::flatzinc::FlatZincError;

pub struct CompilationContext<'a> {
    /// The solver to compile the FlatZinc into.
    pub solver: &'a mut ConstraintSatisfactionSolver,

    /// All identifiers occuring in the model. The identifiers are interned, to support cheap
    /// cloning.
    pub identifiers: Identifiers,

    /// Identifiers of variables that are outputs.
    pub outputs: Vec<Output>,

    /// All boolean parameters.
    pub boolean_parameters: HashMap<Rc<str>, bool>,
    /// All boolean array parameters.
    pub boolean_array_parameters: HashMap<Rc<str>, Rc<[bool]>>,
    /// A mapping from boolean model variables to solver literals.
    pub boolean_variable_map: HashMap<Rc<str>, Literal>,
    /// A mapping from boolean variable array identifiers to slices of literals.
    pub boolean_variable_arrays: HashMap<Rc<str>, Rc<[Literal]>>,
    /// The equivalence classes for literals.
    pub literal_equivalences: VariableEquivalences,
    /// A literal which is always true, can be used when using bool constants in the solver
    pub constant_bool_true: Literal,
    /// A literal which is always false, can be used when using bool constants in the solver
    pub constant_bool_false: Literal,

    /// All integer parameters.
    pub integer_parameters: HashMap<Rc<str>, i32>,
    /// All integer array parameters.
    pub integer_array_parameters: HashMap<Rc<str>, Rc<[i32]>>,
    /// A mapping from integer model variables to solver literals.
    pub integer_variable_map: HashMap<Rc<str>, DomainId>,
    /// The equivalence classes for integer variables. The associated data is the bounds for the
    /// domain of the representative of the equivalence class..
    pub integer_equivalences: VariableEquivalences,
    /// Only instantiate single domain for every constant variable.
    pub constant_domain_ids: HashMap<i32, DomainId>,
    /// A mapping from integer variable array identifiers to slices of domain ids.
    pub integer_variable_arrays: HashMap<Rc<str>, Rc<[DomainId]>>,
}

impl CompilationContext<'_> {
    pub fn new(solver: &mut ConstraintSatisfactionSolver) -> CompilationContext<'_> {
        let true_literal = solver.get_propositional_assignments().true_literal;
        let false_literal = solver.get_propositional_assignments().false_literal;

        CompilationContext {
            solver,
            identifiers: Default::default(),

            outputs: Default::default(),

            boolean_parameters: Default::default(),
            boolean_array_parameters: Default::default(),
            boolean_variable_map: Default::default(),
            boolean_variable_arrays: Default::default(),
            literal_equivalences: Default::default(),
            constant_bool_true: true_literal,
            constant_bool_false: false_literal,

            integer_parameters: Default::default(),
            integer_array_parameters: Default::default(),
            integer_variable_map: Default::default(),
            integer_equivalences: Default::default(),
            constant_domain_ids: Default::default(),
            integer_variable_arrays: Default::default(),
        }
    }

    // pub fn resolve_bool_constant(&self, identifier: &str) -> Option<bool> {
    //     self.boolean_parameters.get(identifier).copied()
    // }

    // pub fn resolve_int_constant(&self, identifier: &str) -> Option<i32> {
    //     self.integer_parameters.get(identifier).copied()
    // }

    pub fn resolve_bool_variable(
        &mut self,
        expr: &flatzinc::Expr,
    ) -> Result<Literal, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => self.resolve_bool_variable_from_identifier(id),
            flatzinc::Expr::Bool(value) => {
                if *value {
                    Ok(self.constant_bool_true)
                } else {
                    Ok(self.constant_bool_false)
                }
            }
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub fn resolve_bool_variable_from_identifier(
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
                        self.constant_bool_true
                    } else {
                        self.constant_bool_false
                    }
                })
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: identifier.into(),
                    expected_type: "bool variable".into(),
                })
        }
    }

    pub fn resolve_bool_variable_array(
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
                                        self.constant_bool_true
                                    } else {
                                        self.constant_bool_false
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
                .map(|elem| {
                    if let flatzinc::BoolExpr::VarParIdentifier(id) = elem {
                        self.resolve_bool_variable_from_identifier(id)
                    } else {
                        Err(FlatZincError::UnexpectedExpr)
                    }
                })
                .collect(),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub fn resolve_array_integer_constants(
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
                .map(|e| self.resolve_int_expr(e))
                .collect::<Result<Rc<[i32]>, _>>(),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub fn resolve_integer_constant_from_expr(
        &self,
        expr: &flatzinc::Expr,
    ) -> Result<i32, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => self
                .integer_parameters
                .get(id.as_str())
                .copied()
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: id.as_str().into(),
                    expected_type: "constant integer".into(),
                }),
            flatzinc::Expr::Int(value) => i32::try_from(*value).map_err(Into::into),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub fn resolve_int_expr(&self, expr: &flatzinc::IntExpr) -> Result<i32, FlatZincError> {
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

    pub fn resolve_integer_variable(
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
                        .create_new_integer_variable(*val as i32, *val as i32)
                })),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub fn resolve_integer_variable_from_identifier(
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
                    *self
                        .constant_domain_ids
                        .entry(*value)
                        .or_insert_with(|| self.solver.create_new_integer_variable(*value, *value))
                })
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: identifier.into(),
                    expected_type: "integer variable".into(),
                })
        }
    }

    pub fn resolve_integer_variable_array(
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
                                        self.solver.create_new_integer_variable(*value, *value)
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
}

#[derive(Default, Debug)]
pub struct Identifiers {
    interned_identifiers: HashSet<Rc<str>>,
}

impl Identifiers {
    pub fn get_interned(&mut self, identifier: &str) -> Rc<str> {
        if let Some(interned) = self.interned_identifiers.get(identifier) {
            interned.clone()
        } else {
            let interned: Rc<str> = identifier.into();
            self.interned_identifiers.insert(interned.clone());

            interned
        }
    }
}

#[derive(Debug, Default)]
pub struct VariableEquivalences {
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
    ///  - One of the variables, or both, have do not belong to an equivalence class. In this case
    ///  the method will panic.
    pub fn merge(&mut self, variable_1: Rc<str>, variable_2: Rc<str>) {
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
                self.belongs_to.insert(Rc::clone(class), equiv_1_idx);
            });
        }

        self.classes[equiv_2_idx].extend(equiv_1);
        self.domains[equiv_2_idx].merge(domain_1);
        self.belongs_to.insert(variable_1, equiv_2_idx);
    }

    /// Create a new equivalence class with the given representative.
    pub fn create_equivalence_class(&mut self, representative: Rc<str>, lb: i32, ub: i32) {
        self.belongs_to
            .insert(representative.clone(), self.classes.len());
        self.classes.push([representative].into());
        self.domains.push(Domain::from(lb, ub));
    }

    /// Get the name of the representative variable of the equivalence class the given variable
    /// belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub fn representative(&self, variable: &str) -> Rc<str> {
        let equiv_idx = self.belongs_to[variable];

        self.classes[equiv_idx].first().cloned().unwrap()
    }

    /// Get the domain for the given variable, based on the equivalence class it belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub fn domain(&self, variable: &str) -> Domain {
        let equiv_idx = self.belongs_to[variable];

        self.domains[equiv_idx]
    }

    pub fn is_defined(&self, variable: &str) -> bool {
        self.belongs_to.contains_key(variable)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Domain {
    pub lb: i32,
    pub ub: i32,
}

impl Domain {
    pub fn merge(&mut self, other: Domain) {
        let lb = i32::max(self.lb, other.lb);
        let ub = i32::min(self.ub, other.ub);

        *self = Domain::from(lb, ub);
    }

    pub fn is_constant(&self) -> bool {
        self.lb == self.ub
    }

    pub fn from(lb: i32, ub: i32) -> Self {
        Domain { lb, ub }
    }

    pub fn into_literal(self, solver: &mut ConstraintSatisfactionSolver) -> Literal {
        match self {
            Domain { lb, ub } if lb == ub && lb == 1 => {
                solver.get_propositional_assignments().true_literal
            }
            Domain { lb, ub } if lb == ub && lb == 0 => {
                solver.get_propositional_assignments().false_literal
            }
            Domain { .. } => Literal::new(solver.create_new_propositional_variable(), true),
        }
    }

    pub fn into_variable(self, solver: &mut ConstraintSatisfactionSolver) -> DomainId {
        solver.create_new_integer_variable(self.lb, self.ub)
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
        assert_eq!(equivs.domain(&a), Domain::from(0, 1));

        equivs.create_equivalence_class(Rc::clone(&b), 1, 3);
        assert!(equivs.is_defined(&b));
        assert_eq!(equivs.representative(&b), b);
        assert_eq!(equivs.domain(&b), Domain::from(1, 3));

        equivs.create_equivalence_class(Rc::clone(&c), 5, 10);
        assert!(equivs.is_defined(&c));
        assert_eq!(equivs.representative(&c), c);
        assert_eq!(equivs.domain(&c), Domain::from(5, 10));

        equivs.merge(Rc::clone(&a), Rc::clone(&b));
        assert!(equivs.is_defined(&a));
        assert_eq!(equivs.representative(&a), a);
        assert_eq!(equivs.domain(&a), Domain::from(1, 1));

        assert!(equivs.is_defined(&b));
        assert_eq!(equivs.representative(&b), a);
        assert_eq!(equivs.domain(&b), Domain::from(1, 1));

        assert!(equivs.is_defined(&c));
        assert_eq!(equivs.representative(&c), c);
        assert_eq!(equivs.domain(&c), Domain::from(5, 10));
    }
}
