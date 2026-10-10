use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::BTreeMap;
use std::rc::Rc;

use log::warn;
use pumpkin_solver::Solver;
use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::containers::HashSet;
use pumpkin_solver::core::proof::ConstraintTag;
use pumpkin_solver::core::variables::AffineView;
use pumpkin_solver::core::variables::DomainId;
use pumpkin_solver::core::variables::Literal;
use pumpkin_solver::core::variables::TransformableVariable;

use crate::flatzinc::FlatZincError;
use crate::flatzinc::instance::Output;

pub(crate) struct CompilationContext<'a> {
    /// The solver to compile the FlatZinc into.
    pub(crate) solver: &'a mut Solver,

    /// All identifiers occuring in the model. The identifiers are interned, to support cheap
    /// cloning.
    pub(crate) identifiers: Identifiers,

    /// Identifiers of variables that are outputs.
    pub(crate) outputs: Vec<Output>,

    /// The equivalence classes for variables.
    ///
    /// This combines boolean and integer variables.
    pub(crate) equivalences: VariableEquivalences,

    /// A mapping from model variables to solver domains.
    ///
    /// Both boolean and integer variables are represented here. If a variable is a boolean,
    /// then it should be converted to a [`Literal`] as late as possible (e.g. when the constraint
    /// is posted or an array is created).
    pub(crate) variable_map: HashMap<Rc<str>, DomainId>,

    /// Literal which is always true
    pub(crate) true_literal: Literal,
    /// Literal which is always false
    pub(crate) false_literal: Literal,
    /// All boolean parameters.
    pub(crate) boolean_parameters: HashMap<Rc<str>, bool>,
    /// All boolean array parameters.
    pub(crate) boolean_array_parameters: HashMap<Rc<str>, Rc<[bool]>>,
    /// A mapping from boolean variable array identifiers to slices of literals.
    pub(crate) boolean_variable_arrays: HashMap<Rc<str>, Rc<[Literal]>>,
    /// All integer parameters.
    pub(crate) integer_parameters: HashMap<Rc<str>, i32>,
    /// All integer array parameters.
    pub(crate) integer_array_parameters: HashMap<Rc<str>, Rc<[i32]>>,
    /// The equivalence classes for integer variables. The associated data is the bounds for the
    /// Only instantiate single domain for every constant variable.
    pub(crate) constant_domain_ids: HashMap<i32, DomainId>,
    /// A mapping from integer variable array identifiers to slices of domain ids.
    pub(crate) integer_variable_arrays: HashMap<Rc<str>, Rc<[AffineView<DomainId>]>>,

    /// All set parameters.
    pub(crate) set_constants: HashMap<Rc<str>, Set>,

    /// All the constraints with their constraint tags.
    pub(crate) constraints: Vec<(ConstraintTag, flatzinc::ConstraintItem)>,

    /// What the view pass did and how long it took.
    pub(crate) view_statistics: ViewStatistics,
    /// Booleans that a reified comparison of a variable with a constant defines and that occur
    /// only as literals: each becomes the literal of the comparison's predicate, and the
    /// comparison is not posted.
    pub(crate) literal_predicates: Vec<LiteralPredicate>,
    /// The variables the proof replaces by their value; each class's domain is that value.
    pub(crate) declared_constants: Vec<Rc<str>>,
    /// Linear inequalities that read over one variable once views and constants are applied:
    /// each is a bound on that variable, posted at the root instead of as a propagator.
    pub(crate) unit_bounds: Vec<UnitBound>,
    /// The proof format: in the DRCP format, terms are posted as the model writes them and a
    /// half-reified bound stays a propagator, as the DRCP checker reads them.
    pub(crate) proof_format: crate::flatzinc::ProofFormat,
    /// The preprocessing done, as the preamble of a scaffold states it.
    pub(crate) preamble: crate::flatzinc::preamble::Preamble,
}

/// `coefficient * variable <= bound`, which the constraint `tag` states.
#[derive(Clone, Debug)]
pub(crate) struct UnitBound {
    pub(crate) variable: Rc<str>,
    pub(crate) coefficient: i64,
    pub(crate) bound: i64,
    pub(crate) tag: ConstraintTag,
}

/// Counts of the views made, by kind, and the time the pass took.
#[derive(Clone, Copy, Debug, Default)]
pub struct ViewStatistics {
    pub seconds: f64,
    /// Variables that became equal to another.
    pub aliases: usize,
    /// Booleans that became the negation of another.
    pub negations: usize,
    /// Variables that became another view of another variable.
    pub other_views: usize,
    pub parameters: usize,
    pub definitions: usize,
    /// Model constraints that became views and are not posted.
    pub skipped: usize,
    /// Rewrite steps in the log.
    pub rewrites: usize,
    /// Declared replacements in the log.
    pub declarations: usize,
    /// The booleans that became the solver's literal for the comparison they reify.
    pub literal_predicates: usize,
    /// Two-variable equalities still posted as propagators: over two variables already in one
    /// class, declined by the view pass for another reason (a boolean that would not stay a
    /// boolean, no integer solution, an overflow), longer equalities that views reduce to two
    /// terms, and reified or half-reified two-term equalities.
    pub two_term_equalities_in_one_class: usize,
    pub two_term_equalities_declined: usize,
    pub equalities_reduced_to_two_terms: usize,
    pub reified_two_term_equalities: usize,
    /// Affine relations of two variables stated by other constraints, which the view pass does
    /// not read: `int_plus` and `int_times` with a constant operand, `bool_lin_eq` over two
    /// booleans, and a reified equality whose indicator is fixed true.
    pub affine_relations_not_read: usize,
    /// Variables the proof replaces by their value: fixed by their declared domain, or by an
    /// equality over one variable once views apply.
    pub constants: usize,
    /// Equalities that became views only when read again after other views were made.
    pub views_on_rereading: usize,
    /// Equalities that views and constants reduce to `0 = 0`, which are not posted.
    pub redundant_equalities: usize,
    /// Linear inequalities posted as a bound at the root.
    pub unit_bounds: usize,
    /// Half-reified comparisons of a variable with a constant posted as a clause.
    pub half_reified_clauses: usize,
}

/// A boolean that stands for `variable comparator value`, defined by the constraint `tag`.
#[derive(Clone, Debug)]
pub(crate) struct LiteralPredicate {
    pub(crate) boolean: Rc<str>,
    pub(crate) variable: Rc<str>,
    pub(crate) comparator: &'static str,
    pub(crate) value: i32,
    pub(crate) tag: ConstraintTag,
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
            equivalences: Default::default(),

            true_literal,
            false_literal,
            boolean_parameters: Default::default(),
            boolean_array_parameters: Default::default(),
            boolean_variable_arrays: Default::default(),
            integer_parameters: Default::default(),
            integer_array_parameters: Default::default(),
            variable_map: Default::default(),
            constant_domain_ids: Default::default(),
            integer_variable_arrays: Default::default(),

            set_constants: Default::default(),

            constraints: Default::default(),

            view_statistics: Default::default(),
            literal_predicates: vec![],
            declared_constants: vec![],
            unit_bounds: vec![],
            proof_format: crate::flatzinc::ProofFormat::Croc,
            preamble: Default::default(),
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

    pub(crate) fn resolve_bool_variable_from_identifier(
        &self,
        identifier: &str,
    ) -> Result<Literal, FlatZincError> {
        let (representative, view) = self.equivalences.view(identifier);
        if let Some(domain_id) = self.variable_map.get(&representative) {
            match (view.scale, view.offset) {
                (1, 0) => Ok(Literal::new(*domain_id)),
                (-1, 1) => Ok(!Literal::new(*domain_id)),
                _ => Err(FlatZincError::InvalidIdentifier {
                    identifier: identifier.into(),
                    expected_type: "boolean view".into(),
                }),
            }
        } else {
            self.boolean_parameters
                .get(&self.equivalences.representative(identifier))
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

    pub(crate) fn resolve_bool_constants(
        &self,
        expr: &flatzinc::Expr,
    ) -> Result<Rc<[bool]>, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => self
                .boolean_array_parameters
                .get(id.as_str())
                .cloned()
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: id.as_str().into(),
                    expected_type: "constant boolean array".into(),
                }),
            flatzinc::Expr::ArrayOfBool(exprs) => exprs
                .iter()
                .map(|e| self.resolve_bool_expr_to_const(e))
                .collect::<Result<Rc<[bool]>, _>>(),
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

    pub(crate) fn resolve_integer_constant_from_identifier(
        &mut self,
        identifier: &str,
    ) -> Result<DomainId, FlatZincError> {
        let value = self.resolve_int_expr_to_const(&flatzinc::IntExpr::VarParIdentifier(
            identifier.to_owned(),
        ))?;
        Ok(*self
            .constant_domain_ids
            .entry(value)
            .or_insert_with(|| self.solver.new_bounded_integer(value, value)))
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

    pub(crate) fn resolve_bool_expr_to_const(
        &self,
        expr: &flatzinc::BoolExpr,
    ) -> Result<bool, FlatZincError> {
        match expr {
            flatzinc::BoolExpr::Bool(value) => Ok(*value),
            flatzinc::BoolExpr::VarParIdentifier(id) => self
                .boolean_parameters
                .get(id.as_str())
                .copied()
                .ok_or_else(|| FlatZincError::InvalidIdentifier {
                    identifier: id.as_str().into(),
                    expected_type: "constant boolean".into(),
                }),
        }
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
    ) -> Result<AffineView<DomainId>, FlatZincError> {
        match expr {
            flatzinc::IntExpr::Int(value) => Ok(self
                .constant_domain_ids
                .entry(*value as i32)
                .or_insert_with(|| {
                    self.solver
                        .new_bounded_integer(*value as i32, *value as i32)
                })
                .scaled(1)),
            flatzinc::IntExpr::VarParIdentifier(id) => {
                self.resolve_integer_variable_from_identifier(id)
            }
        }
    }

    pub(crate) fn resolve_integer_variable(
        &mut self,
        expr: &flatzinc::Expr,
    ) -> Result<AffineView<DomainId>, FlatZincError> {
        match expr {
            flatzinc::Expr::VarParIdentifier(id) => {
                self.resolve_integer_variable_from_identifier(id)
            }
            flatzinc::Expr::Int(val) => Ok(self
                .constant_domain_ids
                .entry(*val as i32)
                .or_insert_with(|| self.solver.new_bounded_integer(*val as i32, *val as i32))
                .scaled(1)),
            _ => Err(FlatZincError::UnexpectedExpr),
        }
    }

    pub(crate) fn resolve_integer_variable_from_identifier(
        &mut self,
        identifier: &str,
    ) -> Result<AffineView<DomainId>, FlatZincError> {
        if !self.equivalences.classes.contains_key(identifier) {
            return Err(FlatZincError::InvalidIdentifier {
                identifier: identifier.into(),
                expected_type: "integer".into(),
            });
        }
        let (representative, view) = self.equivalences.view(identifier);
        if let Some(domain_id) = self.variable_map.get(&representative) {
            Ok(domain_id.scaled(view.scale).offset(view.offset))
        } else {
            self.integer_parameters
                .get(&representative)
                .map(|value| {
                    self.constant_domain_ids
                        .entry(*value)
                        .or_insert_with(|| self.solver.new_bounded_integer(*value, *value))
                        .scaled(1)
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
    ) -> Result<Rc<[AffineView<DomainId>]>, FlatZincError> {
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
                                    self.constant_domain_ids
                                        .entry(*value)
                                        .or_insert_with(|| {
                                            self.solver.new_bounded_integer(*value, *value)
                                        })
                                        .scaled(1)
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
                .collect::<Result<Rc<[AffineView<DomainId>]>, _>>(),

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

/// `member = scale * representative + offset`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Affine {
    pub(crate) scale: i32,
    pub(crate) offset: i32,
}

impl Affine {
    pub(crate) const IDENTITY: Affine = Affine {
        scale: 1,
        offset: 0,
    };

    /// `self` after `inner`: if `y = self(x)` and `x = inner(t)` then `y = (self ∘ inner)(t)`.
    pub(crate) fn compose(self, inner: Affine) -> Affine {
        Affine {
            scale: self.scale * inner.scale,
            offset: self.scale * inner.offset + self.offset,
        }
    }
}

pub(crate) fn div_floor(numerator: i32, divisor: i32) -> i32 {
    let quotient = numerator / divisor;
    if numerator % divisor != 0 && ((numerator < 0) != (divisor < 0)) {
        quotient - 1
    } else {
        quotient
    }
}

pub(crate) fn div_ceil(numerator: i32, divisor: i32) -> i32 {
    -div_floor(-numerator, divisor)
}

#[derive(Debug, Default)]
pub(crate) struct VariableEquivalences {
    /// For each variable, the equivalence class it belongs to.
    pub(super) classes: HashMap<Rc<str>, Rc<RefCell<EquivalenceClass>>>,
    /// The variables declared boolean: a view of one must keep it boolean.
    pub(crate) booleans: HashSet<Rc<str>>,
    /// The parameters introduced for equalities whose coefficients divide neither way, in the
    /// order of introduction; each is the representative of its class.
    pub(crate) parameters: Vec<Rc<str>>,
}

#[derive(Debug)]
pub(super) struct EquivalenceClass {
    representative: Rc<str>,
    /// Every member as a view of the representative; the representative is the identity.
    members: BTreeMap<Rc<str>, Affine>,
    /// The domain of the representative.
    domain: Domain,
}

impl VariableEquivalences {
    fn create(&mut self, representative: Rc<str>, domain: Domain) {
        let class = Rc::new(RefCell::new(EquivalenceClass {
            representative: Rc::clone(&representative),
            members: [(Rc::clone(&representative), Affine::IDENTITY)]
                .into_iter()
                .collect(),
            domain,
        }));
        let _ = self.classes.insert(representative, class);
    }

    /// Create a new equivalence class with the given representative.
    pub(crate) fn create_equivalence_class(&mut self, representative: Rc<str>, lb: i32, ub: i32) {
        self.create(representative, Domain::IntervalDomain { lb, ub });
    }

    /// Create a new equivalence class with the given representative.
    pub(crate) fn create_equivalence_class_sparse(
        &mut self,
        representative: Rc<str>,
        values: Vec<i32>,
    ) {
        self.create(representative, Domain::SparseDomain { values });
    }

    /// A fresh parameter, unconstrained until the classes it stands for are absorbed.
    pub(crate) fn create_parameter_class(&mut self, name: Rc<str>) {
        self.create(
            Rc::clone(&name),
            Domain::IntervalDomain {
                lb: i32::MIN / 4,
                ub: i32::MAX / 4,
            },
        );
        self.parameters.push(name);
    }

    /// Get the name of the representative variable of the equivalence class the given variable
    /// belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub(crate) fn representative(&self, variable: &str) -> Rc<str> {
        Rc::clone(&self.classes[variable].borrow().representative)
    }

    /// The representative and the view of it the variable is.
    pub(crate) fn view(&self, variable: &str) -> (Rc<str>, Affine) {
        let class = self.classes[variable].borrow();
        (Rc::clone(&class.representative), class.members[variable])
    }

    /// The number of variables in the variable's class.
    pub(crate) fn class_size(&self, variable: &str) -> usize {
        self.classes[variable].borrow().members.len()
    }

    /// Every member of the variable's class with its view of the representative.
    pub(crate) fn members(&self, variable: &str) -> Vec<(Rc<str>, Affine)> {
        self.classes[variable]
            .borrow()
            .members
            .iter()
            .map(|(name, view)| (Rc::clone(name), *view))
            .collect()
    }

    /// Make the class of `absorbed` part of the class of `target`, its representative being
    /// `view` of the target's representative. The target's domain is narrowed to what the
    /// absorbed domain allows through the view.
    pub(crate) fn absorb(&mut self, target: &str, absorbed: &str, view: Affine) {
        let target_class = Rc::clone(&self.classes[target]);
        let absorbed_class = Rc::clone(&self.classes[absorbed]);
        assert!(!Rc::ptr_eq(&target_class, &absorbed_class));

        let moved: Vec<(Rc<str>, Affine)> = absorbed_class
            .borrow()
            .members
            .iter()
            .map(|(name, member_view)| (Rc::clone(name), member_view.compose(view)))
            .collect();
        let absorbed_domain = absorbed_class.borrow().domain.clone();

        {
            let mut class = target_class.borrow_mut();
            for (name, member_view) in &moved {
                let _ = class.members.insert(Rc::clone(name), *member_view);
            }
            class.domain = class.domain.merge(&preimage(&absorbed_domain, view));
        }
        for (name, _) in moved {
            let _ = self.classes.insert(name, Rc::clone(&target_class));
        }
    }

    /// Get the domain for the given variable, based on the equivalence class it belongs to.
    /// If the variable doesn't belong to an equivalence class, this method panics.
    pub(crate) fn domain(&self, variable: &str) -> Domain {
        self.classes[variable].borrow().domain.clone()
    }

    #[allow(
        clippy::needless_pass_by_ref_mut,
        reason = "The domains are behind a `RefCell`, so `&self` would suffice; taking `&mut self` \
                  ensures at compile-time that two mutable borrows cannot be handed out at the \
                  same time, which would panic at runtime"
    )]
    pub(crate) fn get_mut_domain(&mut self, variable: &str) -> RefMut<'_, Domain> {
        RefMut::map(self.classes[variable].borrow_mut(), |class| {
            &mut class.domain
        })
    }

    /// Restrict the variable to the values of `domain`: its class's representative keeps the
    /// values at which the variable's view of it lies in `domain`.
    pub(crate) fn restrict(&mut self, variable: &str, domain: &Domain) {
        let view = self.classes[variable].borrow().members[variable];
        let mut class = self.classes[variable].borrow_mut();
        class.domain = class.domain.merge(&preimage(domain, view));
    }

    pub(crate) fn is_defined(&self, variable: &str) -> bool {
        self.classes.contains_key(variable)
    }
}

/// The values of the representative at which `view` of it lies in `domain`: for an interval, its
/// bounds pulled back and rounded inward; for a set of values, those that are an integer view of
/// some value of the representative.
fn preimage(domain: &Domain, view: Affine) -> Domain {
    match domain {
        Domain::IntervalDomain { lb, ub } => {
            if view.scale > 0 {
                Domain::IntervalDomain {
                    lb: div_ceil(lb - view.offset, view.scale),
                    ub: div_floor(ub - view.offset, view.scale),
                }
            } else {
                Domain::IntervalDomain {
                    lb: div_ceil(ub - view.offset, view.scale),
                    ub: div_floor(lb - view.offset, view.scale),
                }
            }
        }
        Domain::SparseDomain { values } => {
            let mut pulled: Vec<i32> = values
                .iter()
                .filter(|&&value| (value - view.offset) % view.scale == 0)
                .map(|&value| (value - view.offset) / view.scale)
                .collect();
            pulled.sort_unstable();
            pulled.dedup();
            Domain::SparseDomain { values: pulled }
        }
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
                warn!(
                    "Merging `SparseDomain` with `IntervalDomain`, this could lead to memory issues depending on the implementation"
                );
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

    pub(crate) fn from_lower_bound_and_upper_bound(lb: i32, ub: i32) -> Self {
        Domain::IntervalDomain { lb, ub }
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

        equivs.absorb(&a, &b, Affine::IDENTITY);
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
    fn absorbing_a_view_narrows_the_representative() {
        let mut equivs = VariableEquivalences::default();
        let x: Rc<str> = "x".into();
        let y: Rc<str> = "y".into();
        equivs.create_equivalence_class(Rc::clone(&x), 0, 10);
        equivs.create_equivalence_class(Rc::clone(&y), 3, 9);

        // y = 2 * x + 1, so y in 3..9 leaves x in 1..4.
        equivs.absorb(
            &x,
            &y,
            Affine {
                scale: 2,
                offset: 1,
            },
        );
        assert_eq!(equivs.representative(&y), x);
        assert_eq!(
            equivs.view(&y).1,
            Affine {
                scale: 2,
                offset: 1
            }
        );
        assert_eq!(
            equivs.domain(&x),
            Domain::from_lower_bound_and_upper_bound(1, 4)
        );

        // y = -x + 5 leaves x in -4..2 from y in 3..9.
        let mut equivs = VariableEquivalences::default();
        equivs.create_equivalence_class(Rc::clone(&x), -10, 10);
        equivs.create_equivalence_class(Rc::clone(&y), 3, 9);
        equivs.absorb(
            &x,
            &y,
            Affine {
                scale: -1,
                offset: 5,
            },
        );
        assert_eq!(
            equivs.domain(&x),
            Domain::from_lower_bound_and_upper_bound(-4, 2)
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
