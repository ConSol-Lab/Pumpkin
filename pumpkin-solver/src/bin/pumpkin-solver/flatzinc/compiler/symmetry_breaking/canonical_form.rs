//! Canonical form of the model for symmetry detection, also used by dominance detection.
//!
//! Every constraint is reduced to a *tag* (its identifier plus all constant arguments) and a
//! multiset of *slots*: one `(SlotKey, VarIndex)` per variable occurrence. Two variable
//! occurrences may be exchanged without changing the constraint's meaning exactly when their
//! slot keys are equal. This is the soundness-critical part of symmetry detection: a key that
//! wrongly marks two positions as interchangeable would let a non-symmetry pass verification.
//! The default for any argument position is therefore [`SlotKey::Positional`], which never
//! identifies two occurrences; only argument positions whose order is known to be irrelevant
//! are given a shared key.
//!
//! For example, `int_lin_le([1, 1], [x, y], 4)` has the tag `(int_lin_le, 4)` and the slots
//! `(coefficient 1, x)` and `(coefficient 1, y)`. The keys are equal, so exchanging `x` and `y`
//! leaves the constraint unchanged. In `int_lin_le([2, 1], [x, y], 4)` the keys are
//! `coefficient 2` and `coefficient 1`, and in `int_lt(x, y)` they are `position 0` and
//! `position 1`; in both, `x` and `y` cannot be exchanged.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

use flatzinc::ArrayOfBoolExpr;
use flatzinc::ArrayOfIntExpr;
use flatzinc::BoolExpr;
use flatzinc::Expr;
use flatzinc::Goal;
use flatzinc::IntExpr;
use flatzinc::SetLiteralExpr;
use pumpkin_solver::core::containers::HashMap;
use pumpkin_solver::core::variables::DomainId;

use super::super::context::CompilationContext;
use super::super::context::Domain;
use super::super::context::Set;
use crate::flatzinc::FlatZincError;
use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::ast::VarArrayDecl;

/// Dense index of a solver variable in [`ModelIndex::variables`].
pub(crate) type VarIndex = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum VarKind {
    Bool,
    Int,
}

/// A solver variable as seen by symmetry detection: its kind and initial domain.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Variable {
    pub(crate) kind: VarKind,
    pub(crate) domain: Domain,
}

impl Variable {
    pub(crate) fn hash_into(&self, hasher: &mut impl Hasher) {
        self.kind.hash(hasher);
        match &self.domain {
            Domain::IntervalDomain { lb, ub } => {
                0_u8.hash(hasher);
                lb.hash(hasher);
                ub.hash(hasher);
            }
            Domain::SparseDomain { values } => {
                1_u8.hash(hasher);
                values.hash(hasher);
            }
        }
    }
}

/// Identifies which variable occurrences within one constraint may be exchanged: two
/// occurrences are interchangeable exactly when their keys are equal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum SlotKey {
    /// An exact position. Never equal to another occurrence's key.
    Positional { arg: u8, index: u32 },
    /// Any element of an argument whose element order is irrelevant.
    Unordered { arg: u8 },
    /// A term of a linear constraint, keyed by its coefficient.
    Coefficient { arg: u8, coefficient: i64 },
    /// One of a group of mutually exchangeable scalar arguments.
    Commutative { group: u8 },
    /// An element of an array whose companions in parallel constant arrays have the given
    /// values, e.g. a task of `cumulative` keyed by its (duration, requirement).
    Keyed { arg: u8, key: [i64; 2] },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum Constant {
    Bool(bool),
    Int(i64),
    Range(i64, i64),
    Set(Vec<i64>),
    /// Anything else (floats, set expressions); only ever compared for equality.
    Text(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CanonicalConstraint {
    pub(crate) id: Rc<str>,
    /// Constant arguments, sorted.
    pub(crate) constants: Vec<(SlotKey, Constant)>,
    /// Variable occurrences, sorted.
    pub(crate) slots: Vec<(SlotKey, VarIndex)>,
}

impl CanonicalConstraint {
    pub(crate) fn new(
        id: Rc<str>,
        mut constants: Vec<(SlotKey, Constant)>,
        mut slots: Vec<(SlotKey, VarIndex)>,
    ) -> Self {
        constants.sort();
        slots.sort();
        CanonicalConstraint {
            id,
            constants,
            slots,
        }
    }

    /// Hash of everything except the variables.
    pub(crate) fn tag_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.id.hash(&mut hasher);
        self.constants.hash(&mut hasher);
        hasher.finish()
    }

    /// The image of this constraint under a variable permutation.
    pub(crate) fn map(&self, permutation: &[VarIndex]) -> CanonicalConstraint {
        let slots = self
            .slots
            .iter()
            .map(|&(key, var)| (key, permutation[var]))
            .collect();
        CanonicalConstraint::new(Rc::clone(&self.id), self.constants.clone(), slots)
    }
}

/// The variable/constraint incidence structure over canonical constraints.
pub(crate) struct Graph {
    pub(crate) constraints: Vec<CanonicalConstraint>,
    pub(crate) tag_hashes: Vec<u64>,
    /// For each variable, the `(constraint index, slot key)` of every occurrence.
    pub(crate) incident: Vec<Vec<(usize, SlotKey)>>,
}

impl Graph {
    pub(crate) fn new(num_variables: usize, constraints: Vec<CanonicalConstraint>) -> Self {
        let tag_hashes = constraints.iter().map(|c| c.tag_hash()).collect();
        let mut incident = vec![Vec::new(); num_variables];
        for (index, constraint) in constraints.iter().enumerate() {
            for &(key, var) in &constraint.slots {
                incident[var].push((index, key));
            }
        }
        Graph {
            constraints,
            tag_hashes,
            incident,
        }
    }

    pub(crate) fn num_variables(&self) -> usize {
        self.incident.len()
    }
}

// ---- How each constraint's arguments are treated ---------------------------------------------

#[derive(Clone, Copy)]
enum ArgShape {
    Positional,
    Unordered,
    Commutative(u8),
    /// The coefficient array of a linear constraint; consumed together with the next argument.
    Coefficients,
    /// The variable array of a linear constraint; each term is keyed by its coefficient.
    LinearTerms,
    /// An array whose element `i` is keyed by element `i` of the constant arrays at `key_args`.
    ParallelKeyed {
        first_key: usize,
        second_key: Option<usize>,
    },
    /// A constant array that only serves as a key for a [`ArgShape::ParallelKeyed`] argument.
    AbsorbedKey,
}

/// Argument treatment per FlatZinc constraint identifier. Anything not listed is fully
/// positional, which loses symmetries but is always sound.
fn argument_shapes(id: &str) -> &'static [ArgShape] {
    use ArgShape::*;
    const CLAUSE: &[ArgShape] = &[Unordered, Unordered];
    const ARRAY_BOOL: &[ArgShape] = &[Unordered, Positional];
    const BINARY_COMMUTATIVE: &[ArgShape] = &[Commutative(0), Commutative(0)];
    const TERNARY_COMMUTATIVE: &[ArgShape] = &[Commutative(0), Commutative(0), Positional];
    const LINEAR: &[ArgShape] = &[Coefficients, LinearTerms, Positional];
    const LINEAR_REIFIED: &[ArgShape] = &[Coefficients, LinearTerms, Positional, Positional];
    const ALL_DIFFERENT: &[ArgShape] = &[Unordered];
    const CUMULATIVE: &[ArgShape] = &[
        ParallelKeyed {
            first_key: 1,
            second_key: Some(2),
        },
        AbsorbedKey,
        AbsorbedKey,
        Positional,
    ];
    const DISJUNCTIVE: &[ArgShape] = &[
        ParallelKeyed {
            first_key: 1,
            second_key: None,
        },
        AbsorbedKey,
    ];

    match id {
        "bool_clause" => CLAUSE,
        "array_bool_or" | "array_bool_and" | "array_bool_xor" => ARRAY_BOOL,
        "bool_eq" | "int_eq" | "int_ne" => BINARY_COMMUTATIVE,
        "bool_and" | "bool_or" | "bool_xor" | "bool_eq_reif" | "int_eq_reif" | "int_ne_reif"
        | "int_eq_imp" | "int_ne_imp" | "int_max" | "int_min" | "int_plus" | "int_times" => {
            TERNARY_COMMUTATIVE
        }
        "int_lin_eq" | "int_lin_le" | "int_lin_ne" | "bool_lin_eq" | "bool_lin_le" => LINEAR,
        "int_lin_eq_reif" | "int_lin_le_reif" | "int_lin_ne_reif" | "int_lin_eq_imp"
        | "int_lin_le_imp" | "int_lin_ne_imp" => LINEAR_REIFIED,
        "fzn_all_different_int" | "pumpkin_all_different" | "all_different_int" => ALL_DIFFERENT,
        "pumpkin_cumulative" => CUMULATIVE,
        "pumpkin_disjunctive_strict" => DISJUNCTIVE,
        _ => &[],
    }
}

// ---- Resolving identifiers ---------------------------------------------------------------------

#[derive(Clone, Debug)]
enum Element {
    Var(VarIndex),
    Const(Constant),
}

#[derive(Clone, Debug)]
enum Resolved {
    Scalar(Element),
    Array(Vec<Element>),
}

/// Resolution of FlatZinc identifiers to dense variable indices and constants.
pub(crate) struct ModelIndex {
    pub(crate) variables: Vec<Variable>,
    /// The solver variable behind each [`VarIndex`].
    pub(crate) domain_ids: Vec<DomainId>,
    pub(crate) objective: Option<VarIndex>,
    by_name: HashMap<Rc<str>, Resolved>,
    by_domain_id: HashMap<DomainId, VarIndex>,
}

impl ModelIndex {
    pub(crate) fn build(
        ast: &FlatZincAst,
        context: &mut CompilationContext,
    ) -> Result<ModelIndex, FlatZincError> {
        let mut index = ModelIndex {
            variables: Vec::new(),
            domain_ids: Vec::new(),
            objective: None,
            by_name: HashMap::default(),
            by_domain_id: HashMap::default(),
        };

        for (name, value) in &context.integer_parameters {
            index.insert(
                name,
                Resolved::Scalar(Element::Const(Constant::Int(*value as i64))),
            );
        }
        for (name, value) in &context.boolean_parameters {
            index.insert(
                name,
                Resolved::Scalar(Element::Const(Constant::Bool(*value))),
            );
        }
        for (name, values) in &context.integer_array_parameters {
            let elements = values
                .iter()
                .map(|v| Element::Const(Constant::Int(*v as i64)))
                .collect();
            index.insert(name, Resolved::Array(elements));
        }
        for (name, values) in &context.boolean_array_parameters {
            let elements = values
                .iter()
                .map(|v| Element::Const(Constant::Bool(*v)))
                .collect();
            index.insert(name, Resolved::Array(elements));
        }
        for (name, set) in &context.set_constants {
            index.insert(name, Resolved::Scalar(Element::Const(set_constant(set))));
        }

        for declaration in &ast.single_variables {
            let (name, kind) = match declaration {
                SingleVarDecl::Bool { id, .. } => (id, VarKind::Bool),
                SingleVarDecl::IntInRange { id, .. } | SingleVarDecl::IntInSet { id, .. } => {
                    (id, VarKind::Int)
                }
            };
            let name = context.identifiers.get_interned(name);
            let representative = context.equivalences.representative(&name);
            let resolved = if let Some(&domain_id) = context.variable_map.get(&representative) {
                let var = index.variable_for(domain_id, kind, || {
                    context.equivalences.domain(&representative)
                });
                Resolved::Scalar(Element::Var(var))
            } else if let Some(value) = context.integer_parameters.get(&representative) {
                Resolved::Scalar(Element::Const(Constant::Int(*value as i64)))
            } else if let Some(value) = context.boolean_parameters.get(&representative) {
                Resolved::Scalar(Element::Const(Constant::Bool(*value)))
            } else {
                continue;
            };
            index.insert(&name, resolved);
        }

        for declaration in &ast.variable_arrays {
            let (name, resolved) = match declaration {
                VarArrayDecl::Bool { id, array_expr, .. } => {
                    let elements = match array_expr {
                        Some(ArrayOfBoolExpr::Array(items)) => items
                            .iter()
                            .map(|item| index.bool_element(item))
                            .collect::<Result<Vec<_>, _>>()?,
                        Some(ArrayOfBoolExpr::VarParIdentifier(other)) => {
                            index.array_elements(other)?
                        }
                        None => continue,
                    };
                    (id, Resolved::Array(elements))
                }
                VarArrayDecl::Int { id, array_expr, .. } => {
                    let elements = match array_expr {
                        Some(ArrayOfIntExpr::Array(items)) => items
                            .iter()
                            .map(|item| index.int_element(item))
                            .collect::<Result<Vec<_>, _>>()?,
                        Some(ArrayOfIntExpr::VarParIdentifier(other)) => {
                            index.array_elements(other)?
                        }
                        None => continue,
                    };
                    (id, Resolved::Array(elements))
                }
            };
            let name = context.identifiers.get_interned(name);
            index.insert(&name, resolved);
        }

        index.objective = match &ast.solve_item.goal {
            Goal::OptimizeBool(_, BoolExpr::VarParIdentifier(name))
            | Goal::OptimizeInt(_, IntExpr::VarParIdentifier(name)) => {
                match index.by_name.get(name.as_str()) {
                    Some(Resolved::Scalar(Element::Var(var))) => Some(*var),
                    _ => None,
                }
            }
            _ => None,
        };

        Ok(index)
    }

    fn insert(&mut self, name: &str, resolved: Resolved) {
        let _ = self.by_name.insert(Rc::from(name), resolved);
    }

    fn variable_for(
        &mut self,
        domain_id: DomainId,
        kind: VarKind,
        domain: impl FnOnce() -> Domain,
    ) -> VarIndex {
        if let Some(&var) = self.by_domain_id.get(&domain_id) {
            return var;
        }
        let var = self.variables.len();
        self.variables.push(Variable {
            kind,
            domain: domain(),
        });
        self.domain_ids.push(domain_id);
        let _ = self.by_domain_id.insert(domain_id, var);
        var
    }

    fn scalar(&self, name: &str) -> Result<Element, FlatZincError> {
        match self.by_name.get(name) {
            Some(Resolved::Scalar(element)) => Ok(element.clone()),
            _ => Err(FlatZincError::InvalidIdentifier {
                identifier: name.into(),
                expected_type: "variable or constant".into(),
            }),
        }
    }

    fn array_elements(&self, name: &str) -> Result<Vec<Element>, FlatZincError> {
        match self.by_name.get(name) {
            Some(Resolved::Array(elements)) => Ok(elements.clone()),
            _ => Err(FlatZincError::InvalidIdentifier {
                identifier: name.into(),
                expected_type: "array".into(),
            }),
        }
    }

    fn bool_element(&self, expr: &BoolExpr) -> Result<Element, FlatZincError> {
        match expr {
            BoolExpr::Bool(value) => Ok(Element::Const(Constant::Bool(*value))),
            BoolExpr::VarParIdentifier(name) => self.scalar(name),
        }
    }

    fn int_element(&self, expr: &IntExpr) -> Result<Element, FlatZincError> {
        match expr {
            IntExpr::Int(value) => Ok(Element::Const(Constant::Int(*value as i64))),
            IntExpr::VarParIdentifier(name) => self.scalar(name),
        }
    }

    fn resolve(&self, expr: &Expr) -> Result<Resolved, FlatZincError> {
        Ok(match expr {
            Expr::VarParIdentifier(name) => {
                self.by_name.get(name.as_str()).cloned().ok_or_else(|| {
                    FlatZincError::InvalidIdentifier {
                        identifier: name.as_str().into(),
                        expected_type: "variable, constant or array".into(),
                    }
                })?
            }
            Expr::Bool(value) => Resolved::Scalar(Element::Const(Constant::Bool(*value))),
            Expr::Int(value) => Resolved::Scalar(Element::Const(Constant::Int(*value as i64))),
            Expr::Float(value) => {
                Resolved::Scalar(Element::Const(Constant::Text(value.to_bits().to_string())))
            }
            Expr::Set(literal) => Resolved::Scalar(Element::Const(set_literal(literal))),
            Expr::ArrayOfBool(items) => Resolved::Array(
                items
                    .iter()
                    .map(|item| self.bool_element(item))
                    .collect::<Result<_, _>>()?,
            ),
            Expr::ArrayOfInt(items) => Resolved::Array(
                items
                    .iter()
                    .map(|item| self.int_element(item))
                    .collect::<Result<_, _>>()?,
            ),
            Expr::ArrayOfFloat(items) => Resolved::Array(
                items
                    .iter()
                    .map(|item| Element::Const(Constant::Text(format!("{item:?}"))))
                    .collect(),
            ),
            Expr::ArrayOfSet(items) => Resolved::Array(
                items
                    .iter()
                    .map(|item| Element::Const(Constant::Text(format!("{item:?}"))))
                    .collect(),
            ),
        })
    }

    /// Build the canonical form of every constraint in the model.
    pub(crate) fn canonicalise(&self, ast: &FlatZincAst) -> Result<Graph, FlatZincError> {
        let constraints = ast
            .constraint_decls
            .iter()
            .map(|item| self.canonicalise_constraint(item))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Graph::new(self.variables.len(), constraints))
    }

    fn canonicalise_constraint(
        &self,
        item: &flatzinc::ConstraintItem,
    ) -> Result<CanonicalConstraint, FlatZincError> {
        let shapes = argument_shapes(&item.id);
        let mut constants = Vec::new();
        let mut slots = Vec::new();
        let mut coefficients: Option<Vec<i64>> = None;

        let resolved: Vec<Resolved> = item
            .exprs
            .iter()
            .map(|expr| self.resolve(expr))
            .collect::<Result<_, _>>()?;

        // The keys of a `ParallelKeyed` argument come from other arguments, so they are
        // extracted first. If a key array is not a plain integer array, the whole constraint
        // falls back to positional treatment, which is always sound.
        let keys_for = |first_key: usize, second_key: Option<usize>| -> Option<Vec<[i64; 2]>> {
            let mut columns: Vec<Vec<i64>> = Vec::new();
            for key_arg in std::iter::once(first_key).chain(second_key) {
                let Resolved::Array(elements) = resolved.get(key_arg)? else {
                    return None;
                };
                let column = elements
                    .iter()
                    .map(|element| match element {
                        Element::Const(Constant::Int(value)) => Some(*value),
                        _ => None,
                    })
                    .collect::<Option<Vec<_>>>()?;
                columns.push(column);
            }
            let length = columns.first()?.len();
            if columns.iter().any(|column| column.len() != length) {
                return None;
            }
            Some(
                (0..length)
                    .map(|i| [columns[0][i], columns.get(1).map_or(0, |c| c[i])])
                    .collect(),
            )
        };
        let keyed_ok = shapes.iter().all(|shape| match shape {
            ArgShape::ParallelKeyed {
                first_key,
                second_key,
            } => keys_for(*first_key, *second_key).is_some(),
            _ => true,
        });
        let shapes: &[ArgShape] = if keyed_ok { shapes } else { &[] };

        let mut place = |key: SlotKey, element: Element| match element {
            Element::Var(var) => slots.push((key, var)),
            Element::Const(constant) => constants.push((key, constant)),
        };

        for (position, resolved) in resolved.iter().cloned().enumerate() {
            let arg = u8::try_from(position).map_err(|_| FlatZincError::UnexpectedExpr)?;
            let shape = shapes
                .get(position)
                .copied()
                .unwrap_or(ArgShape::Positional);

            match (shape, resolved) {
                (ArgShape::AbsorbedKey, _) => {}
                (
                    ArgShape::ParallelKeyed {
                        first_key,
                        second_key,
                    },
                    Resolved::Array(elements),
                ) => {
                    let keys = keys_for(first_key, second_key).expect("checked above");
                    if keys.len() != elements.len() {
                        return Err(FlatZincError::UnexpectedExpr);
                    }
                    for (key, element) in keys.into_iter().zip(elements) {
                        place(SlotKey::Keyed { arg, key }, element);
                    }
                }
                (ArgShape::Coefficients, Resolved::Array(elements)) => {
                    coefficients = Some(
                        elements
                            .into_iter()
                            .map(|element| match element {
                                Element::Const(Constant::Int(value)) => Ok(value),
                                _ => Err(FlatZincError::UnexpectedExpr),
                            })
                            .collect::<Result<_, _>>()?,
                    );
                }
                (ArgShape::LinearTerms, Resolved::Array(elements)) => {
                    let coefficients = coefficients.take().ok_or(FlatZincError::UnexpectedExpr)?;
                    if coefficients.len() != elements.len() {
                        return Err(FlatZincError::UnexpectedExpr);
                    }
                    for (coefficient, element) in coefficients.into_iter().zip(elements) {
                        place(SlotKey::Coefficient { arg, coefficient }, element);
                    }
                }
                (ArgShape::Unordered, Resolved::Array(elements)) => {
                    for element in elements {
                        place(SlotKey::Unordered { arg }, element);
                    }
                }
                (ArgShape::Commutative(group), Resolved::Scalar(element)) => {
                    place(SlotKey::Commutative { group }, element);
                }
                (_, Resolved::Scalar(element)) => {
                    place(SlotKey::Positional { arg, index: 0 }, element);
                }
                (_, Resolved::Array(elements)) => {
                    for (index, element) in elements.into_iter().enumerate() {
                        let index =
                            u32::try_from(index).map_err(|_| FlatZincError::UnexpectedExpr)?;
                        place(SlotKey::Positional { arg, index }, element);
                    }
                }
            }
        }

        Ok(CanonicalConstraint::new(
            Rc::from(item.id.as_str()),
            constants,
            slots,
        ))
    }
}

fn set_constant(set: &Set) -> Constant {
    match set {
        Set::Interval {
            lower_bound,
            upper_bound,
        } => Constant::Range(*lower_bound as i64, *upper_bound as i64),
        Set::Sparse { values } => {
            let mut values: Vec<i64> = values.iter().map(|v| *v as i64).collect();
            values.sort();
            Constant::Set(values)
        }
    }
}

fn set_literal(literal: &SetLiteralExpr) -> Constant {
    match literal {
        SetLiteralExpr::IntInRange(IntExpr::Int(lb), IntExpr::Int(ub)) => {
            Constant::Range(*lb as i64, *ub as i64)
        }
        SetLiteralExpr::SetInts(items)
            if items.iter().all(|item| matches!(item, IntExpr::Int(_))) =>
        {
            let mut values: Vec<i64> = items
                .iter()
                .map(|item| match item {
                    IntExpr::Int(value) => *value as i64,
                    IntExpr::VarParIdentifier(_) => unreachable!(),
                })
                .collect();
            values.sort();
            Constant::Set(values)
        }
        other => Constant::Text(format!("{other:?}")),
    }
}
