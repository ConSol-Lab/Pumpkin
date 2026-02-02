use std::collections::BTreeMap;
use std::num::NonZero;
use std::path::Path;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;
use fzn_rs::VariableExpr;
use fzn_rs::ast::Domain;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::Comparison;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;
use pumpkin_core::containers::HashSet;

#[derive(Clone, Debug, derive_more::From)]
pub(crate) enum Constraint {
    #[allow(unused, reason = "Could be used in the future")]
    Nogood(Nogood),
    LinearLeq(Linear),
    #[from(skip)]
    LinearEq(Linear),
    Cumulative(Cumulative),
    AllDifferent(AllDifferent),
    Circuit(Circuit),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Atomic {
    True,
    False,
    IntAtomic(IntAtomic<Rc<str>, i32>),
}

impl From<IntAtomic<Rc<str>, i32>> for Atomic {
    fn from(value: IntAtomic<Rc<str>, i32>) -> Self {
        Atomic::IntAtomic(value)
    }
}

impl From<bool> for Atomic {
    fn from(value: bool) -> Self {
        if value { Atomic::True } else { Atomic::False }
    }
}

impl AtomicConstraint for Atomic {
    type Identifier = Rc<str>;

    fn identifier(&self) -> Self::Identifier {
        match self {
            Atomic::True => Rc::from("true"),
            Atomic::False => Rc::from("false"),
            Atomic::IntAtomic(int_atomic) => Rc::clone(&int_atomic.name),
        }
    }

    fn comparison(&self) -> Comparison {
        let Atomic::IntAtomic(int_atomic) = self else {
            return Comparison::Equal;
        };

        match int_atomic.comparison {
            drcp_format::IntComparison::GreaterEqual => Comparison::GreaterEqual,
            drcp_format::IntComparison::LessEqual => Comparison::LessEqual,
            drcp_format::IntComparison::Equal => Comparison::Equal,
            drcp_format::IntComparison::NotEqual => Comparison::NotEqual,
        }
    }

    fn value(&self) -> i32 {
        match self {
            Atomic::True => 1,
            Atomic::False => 0,
            Atomic::IntAtomic(int_atomic) => int_atomic.value,
        }
    }

    fn negate(&self) -> Self {
        match self {
            Atomic::True => Atomic::False,
            Atomic::False => Atomic::True,
            Atomic::IntAtomic(int_atomic) => {
                let owned = int_atomic.clone();
                Atomic::IntAtomic(!owned)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Nogood(HashSet<Atomic>);

impl<A> FromIterator<A> for Nogood
where
    A: Into<Atomic>,
{
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        Nogood(iter.into_iter().map(Into::into).collect())
    }
}

impl<T, A> From<T> for Nogood
where
    T: IntoIterator<Item = A>,
    A: Into<Atomic>,
{
    fn from(value: T) -> Self {
        Nogood(value.into_iter().map(Into::into).collect())
    }
}

impl Nogood {
    #[allow(unused, reason = "Could be used in the future")]
    pub(crate) fn iter(&self) -> impl Iterator<Item = &Atomic> + '_ {
        self.0.iter()
    }
}

/// A checker variable that can be used with [`pumpkin_checking::VariableState`].
#[derive(Clone, Debug)]
pub(crate) struct Variable(pub(crate) VariableExpr<i32>);

impl From<VariableExpr<i32>> for Variable {
    fn from(value: VariableExpr<i32>) -> Self {
        Variable(value)
    }
}

impl CheckerVariable<Atomic> for Variable {
    fn does_atomic_constrain_self(&self, atomic: &Atomic) -> bool {
        let Variable(VariableExpr::Identifier(ident)) = self else {
            return false;
        };

        let Atomic::IntAtomic(atomic) = atomic else {
            return false;
        };

        &atomic.name == ident
    }

    fn atomic_less_than(&self, value: i32) -> Atomic {
        match self.0 {
            VariableExpr::Identifier(ref name) => Atomic::from(IntAtomic {
                name: Rc::clone(name),
                comparison: drcp_format::IntComparison::LessEqual,
                value,
            }),
            VariableExpr::Constant(constant) => (constant <= value).into(),
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Atomic {
        match self.0 {
            VariableExpr::Identifier(ref name) => Atomic::from(IntAtomic {
                name: Rc::clone(name),
                comparison: drcp_format::IntComparison::GreaterEqual,
                value,
            }),
            VariableExpr::Constant(constant) => (constant >= value).into(),
        }
    }

    fn atomic_equal(&self, value: i32) -> Atomic {
        match self.0 {
            VariableExpr::Identifier(ref name) => Atomic::from(IntAtomic {
                name: Rc::clone(name),
                comparison: drcp_format::IntComparison::Equal,
                value,
            }),
            VariableExpr::Constant(constant) => (constant == value).into(),
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Atomic {
        match self.0 {
            VariableExpr::Identifier(ref name) => Atomic::from(IntAtomic {
                name: Rc::clone(name),
                comparison: drcp_format::IntComparison::NotEqual,
                value,
            }),
            VariableExpr::Constant(constant) => (constant != value).into(),
        }
    }

    fn induced_lower_bound(&self, variable_state: &VariableState<Atomic>) -> IntExt {
        match self.0 {
            VariableExpr::Identifier(ref ident) => variable_state.lower_bound(ident),
            VariableExpr::Constant(value) => value.into(),
        }
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<Atomic>) -> IntExt {
        match self.0 {
            VariableExpr::Identifier(ref ident) => variable_state.upper_bound(ident),
            VariableExpr::Constant(value) => value.into(),
        }
    }

    fn induced_fixed_value(&self, variable_state: &VariableState<Atomic>) -> Option<i32> {
        match self.0 {
            VariableExpr::Identifier(ref ident) => variable_state.fixed_value(ident),
            VariableExpr::Constant(value) => value.into(),
        }
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        match self.0 {
            #[allow(
                trivial_casts,
                reason = "without it the compiler does not coerce to Box<dyn ...>"
            )]
            VariableExpr::Identifier(ref ident) => {
                Box::new(variable_state.holes(ident)) as Box<dyn Iterator<Item = i32>>
            }
            VariableExpr::Constant(_) => Box::new(std::iter::empty()),
        }
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        match self.0 {
            #[allow(
                trivial_casts,
                reason = "without it the compiler does not coerce to Box<dyn ...>"
            )]
            VariableExpr::Identifier(ref ident) => variable_state
                .iter_domain(ident)
                .map(|iter| Box::new(iter) as Box<dyn Iterator<Item = i32>>),
            VariableExpr::Constant(value) => Some(Box::new(std::iter::once(value))),
        }
    }

    fn induced_domain_contains(&self, variable_state: &VariableState<Atomic>, value: i32) -> bool {
        match self.0 {
            VariableExpr::Identifier(ref ident) => variable_state.contains(ident, value),
            VariableExpr::Constant(constant_value) => constant_value == value,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Linear {
    pub terms: Vec<Term>,
    pub bound: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct Term {
    pub weight: NonZero<i32>,
    pub variable: Variable,
}

impl Term {
    /// Apply the inverse transformation of this view on a value, to go from the value in the domain
    /// of `self` to a value in the domain of `self.inner`.
    fn invert(&self, value: i32, rounding: Rounding) -> i32 {
        match rounding {
            Rounding::Up => div_ceil(value, self.weight.get()),
            Rounding::Down => div_floor(value, self.weight.get()),
        }
    }
}

enum Rounding {
    Up,
    Down,
}

impl CheckerVariable<Atomic> for Term {
    fn does_atomic_constrain_self(&self, atomic: &Atomic) -> bool {
        self.variable.does_atomic_constrain_self(atomic)
    }

    fn atomic_less_than(&self, value: i32) -> Atomic {
        if self.weight.is_negative() {
            let inverted_value = self.invert(value, Rounding::Up);
            self.variable.atomic_greater_than(inverted_value)
        } else {
            let inverted_value = self.invert(value, Rounding::Down);
            self.variable.atomic_less_than(inverted_value)
        }
    }

    fn atomic_greater_than(&self, value: i32) -> Atomic {
        if self.weight.is_negative() {
            let inverted_value = self.invert(value, Rounding::Down);
            self.variable.atomic_less_than(inverted_value)
        } else {
            let inverted_value = self.invert(value, Rounding::Up);
            self.variable.atomic_greater_than(inverted_value)
        }
    }

    fn atomic_equal(&self, value: i32) -> Atomic {
        if value % self.weight.get() == 0 {
            let inverted_value = self.invert(value, Rounding::Up);
            self.variable.atomic_equal(inverted_value)
        } else {
            Atomic::False
        }
    }

    fn atomic_not_equal(&self, value: i32) -> Atomic {
        if value % self.weight.get() == 0 {
            let inverted_value = self.invert(value, Rounding::Up);
            self.variable.atomic_not_equal(inverted_value)
        } else {
            Atomic::True
        }
    }

    fn induced_lower_bound(&self, variable_state: &VariableState<Atomic>) -> IntExt {
        if self.weight.is_positive() {
            self.variable.induced_lower_bound(variable_state) * self.weight.get()
        } else {
            self.variable.induced_upper_bound(variable_state) * self.weight.get()
        }
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<Atomic>) -> IntExt {
        if self.weight.is_positive() {
            self.variable.induced_upper_bound(variable_state) * self.weight.get()
        } else {
            self.variable.induced_lower_bound(variable_state) * self.weight.get()
        }
    }

    fn induced_fixed_value(&self, variable_state: &VariableState<Atomic>) -> Option<i32> {
        self.variable
            .induced_fixed_value(variable_state)
            .map(|value| value * self.weight.get())
    }

    fn induced_holes<'this, 'state>(
        &'this self,
        _variable_state: &'state VariableState<Atomic>,
    ) -> impl Iterator<Item = i32> + 'state
    where
        'this: 'state,
    {
        todo!("how to compute holes in a scaled domain?");

        #[allow(
            unreachable_code,
            reason = "otherwise the function does not return an impl Iterator"
        )]
        std::iter::empty()
    }

    fn iter_induced_domain<'this, 'state>(
        &'this self,
        variable_state: &'state VariableState<Atomic>,
    ) -> Option<impl Iterator<Item = i32> + 'state>
    where
        'this: 'state,
    {
        self.variable
            .iter_induced_domain(variable_state)
            .map(|iter| iter.map(|value| value * self.weight.get()))
    }

    fn induced_domain_contains(&self, variable_state: &VariableState<Atomic>, value: i32) -> bool {
        if value % self.weight.get() == 0 {
            let inverted = self.invert(value, Rounding::Up);
            self.variable
                .induced_domain_contains(variable_state, inverted)
        } else {
            false
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Task {
    pub(crate) start_time: Variable,
    pub(crate) duration: i32,
    pub(crate) resource_usage: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct Cumulative {
    pub(crate) tasks: Vec<Task>,
    pub(crate) capacity: i32,
}

#[derive(Clone, Debug)]
pub(crate) struct AllDifferent {
    pub(crate) variables: Vec<Variable>,
}

#[derive(Clone, Debug)]
pub(crate) struct Circuit {
    pub(crate) successors: Vec<Variable>,
}

#[allow(unused, reason = "Could be used in the future")]
#[derive(Clone, Debug)]
pub(crate) enum Objective {
    Maximize(Variable),
    Minimize(Variable),
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Model {
    variables: BTreeMap<Rc<str>, Domain>,
    constraints: BTreeMap<ConstraintId, Constraint>,
    pub(crate) objective: Option<Objective>,
}

impl Model {
    /// Add a new variable to the model.
    pub(crate) fn add_variable(&mut self, name: Rc<str>, domain: Domain) {
        let _ = self.variables.insert(name, domain);
    }

    /// Add a new constraint to the model.
    ///
    /// If a constraint with the given ID already exists, this returns false. Otherwise, the
    /// function returns true.
    pub(crate) fn add_constraint(
        &mut self,
        constraint_id: ConstraintId,
        constraint: impl Into<Constraint>,
    ) -> bool {
        self.constraints
            .insert(constraint_id, constraint.into())
            .is_none()
    }

    /// Iterate over the constraints in the map, ordered by [`ConstraintId`].
    #[allow(unused, reason = "Could be used in the future")]
    pub(crate) fn iter_constraints(
        &self,
    ) -> std::collections::btree_map::Iter<'_, ConstraintId, Constraint> {
        self.constraints.iter()
    }

    /// Get the constraint with the given ID if it exists.
    pub(crate) fn get_constraint(&self, constraint_id: ConstraintId) -> Option<&Constraint> {
        self.constraints.get(&constraint_id)
    }

    /// Test whether the atomic is true in the initial domains of the variables.
    ///
    /// Returns false if the atomic is over a variable that is not in the model.
    #[allow(unused, reason = "Could be used in the future")]
    pub(crate) fn is_trivially_true(&self, atomic: &Atomic) -> bool {
        let Some(domain) = self.variables.get(&atomic.identifier()) else {
            return false;
        };

        match domain {
            Domain::UnboundedInt => false,
            Domain::Int(dom) => match atomic.comparison() {
                Comparison::GreaterEqual => *dom.lower_bound() >= atomic.value() as i64,
                Comparison::LessEqual => *dom.upper_bound() <= atomic.value() as i64,
                Comparison::Equal => {
                    *dom.lower_bound() >= atomic.value() as i64
                        && *dom.upper_bound() <= atomic.value() as i64
                }
                Comparison::NotEqual => {
                    if *dom.lower_bound() >= atomic.value() as i64 {
                        return true;
                    }

                    if *dom.upper_bound() <= atomic.value() as i64 {
                        return true;
                    }

                    if dom.is_continuous() {
                        return false;
                    }

                    dom.into_iter().all(|value| value != atomic.value() as i64)
                }
            },
            Domain::Bool => todo!("boolean variables are not yet supported"),
        }
    }
}

pub(crate) fn div_ceil(lhs: i32, other: i32) -> i32 {
    // TODO: The source is taken from the standard library nightly implementation of this
    // function and div_floor. Once they are stabilized, these definitions can be removed.
    // Tracking issue: https://github.com/rust-lang/rust/issues/88581
    let d = lhs / other;
    let r = lhs % other;
    if (r > 0 && other > 0) || (r < 0 && other < 0) {
        d + 1
    } else {
        d
    }
}

pub(crate) fn div_floor(lhs: i32, other: i32) -> i32 {
    // TODO: See todo in `div_ceil`.
    let d = lhs / other;
    let r = lhs % other;
    if (r > 0 && other < 0) || (r < 0 && other > 0) {
        d - 1
    } else {
        d
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Fact {
    pub premises: Vec<Atomic>,
    pub consequent: Option<Atomic>,
}

impl Fact {
    /// Create a fact `premises -> false`.
    #[allow(unused, reason = "Could be used in the future")]
    pub(crate) fn nogood(premises: Vec<Atomic>) -> Self {
        Fact {
            premises,
            consequent: None,
        }
    }
}

type FlatZincModel = fzn_rs::TypedInstance<i32, FlatZincConstraints>;

/// The constraints supported by the checker.
#[derive(Debug, fzn_rs::FlatZincConstraint)]
enum FlatZincConstraints {
    #[name("int_lin_le")]
    LinearLeq {
        weights: fzn_rs::ArrayExpr<i32>,
        variables: fzn_rs::ArrayExpr<VariableExpr<i32>>,
        bound: i32,
    },
    #[name("int_lin_eq")]
    LinearEq {
        weights: fzn_rs::ArrayExpr<i32>,
        variables: fzn_rs::ArrayExpr<VariableExpr<i32>>,
        bound: i32,
    },
    #[name("pumpkin_cumulative")]
    Cumulative {
        start_times: fzn_rs::ArrayExpr<VariableExpr<i32>>,
        durations: fzn_rs::ArrayExpr<i32>,
        resource_usages: fzn_rs::ArrayExpr<i32>,
        capacity: i32,
    },
    #[name("pumpkin_all_different")]
    AllDifferent(fzn_rs::ArrayExpr<VariableExpr<i32>>),
}

/// Parse a FlatZinc file to a checker [`Model`].
#[allow(clippy::field_reassign_with_default, reason = "Could be refactored")]
pub(crate) fn parse_model(path: impl AsRef<Path>) -> anyhow::Result<Model> {
    let model_source = std::fs::read_to_string(path)?;

    // TODO: For now the error handling shortcuts here. Ideally the `FznError` type returns
    // something that can be converted to an owned type, but for now we have to work around the
    // error holding a reference to the source.
    let fzn_ast = fzn_rs::fzn::parse(&model_source).map_err(|err| anyhow::anyhow!("{err}"))?;

    let fzn_model = FlatZincModel::from_ast(fzn_ast)?;

    let mut model = Model::default();
    model.objective = match &fzn_model.solve.method.node {
        fzn_rs::Method::Satisfy => None,
        fzn_rs::Method::Optimize {
            direction: fzn_rs::ast::OptimizationDirection::Minimize,
            objective,
        } => Some(Objective::Minimize(objective.clone().into())),
        fzn_rs::Method::Optimize {
            direction: fzn_rs::ast::OptimizationDirection::Maximize,
            objective,
        } => Some(Objective::Maximize(objective.clone().into())),
    };

    for (name, variable) in fzn_model.variables.iter() {
        model.add_variable(Rc::clone(name), variable.domain.node.clone());
    }

    for (idx, annotated_constraint) in fzn_model.constraints.iter().enumerate() {
        let constraint_id = NonZero::new(idx as u32 + 1).expect(
            "we always add one, and idx is at least zero, constraint_id is always non-zero",
        );

        let constraint = match &annotated_constraint.constraint.node {
            FlatZincConstraints::LinearLeq {
                weights,
                variables,
                bound,
            } => {
                let weights = fzn_model.resolve_array(weights)?;
                let variables = fzn_model.resolve_array(variables)?;

                let mut terms = vec![];

                for (weight, variable) in weights.zip(variables) {
                    let weight = weight?;
                    let variable = variable?;

                    terms.push(Term {
                        weight: weight
                            .try_into()
                            .expect("flatzinc does not have 0-weight terms"),
                        variable: variable.into(),
                    });
                }

                Constraint::LinearLeq(Linear {
                    terms,
                    bound: *bound,
                })
            }

            FlatZincConstraints::LinearEq {
                weights,
                variables,
                bound,
            } => {
                let weights = fzn_model.resolve_array(weights)?;
                let variables = fzn_model.resolve_array(variables)?;

                let mut terms = vec![];

                for (weight, variable) in weights.zip(variables) {
                    let weight = weight?;
                    let variable = variable?;

                    terms.push(Term {
                        weight: weight
                            .try_into()
                            .expect("flatzinc does not have 0-weight terms"),
                        variable: variable.into(),
                    });
                }

                Constraint::LinearEq(Linear {
                    terms,
                    bound: *bound,
                })
            }

            FlatZincConstraints::Cumulative {
                start_times,
                durations,
                resource_usages,
                capacity,
            } => {
                let start_times = fzn_model.resolve_array(start_times)?;
                let durations = fzn_model.resolve_array(durations)?;
                let resource_usages = fzn_model.resolve_array(resource_usages)?;

                let tasks = start_times
                    .zip(durations)
                    .zip(resource_usages)
                    .map(
                        |((maybe_start_time, maybe_duration), maybe_resource_usage)| {
                            let start_time = maybe_start_time?;
                            let duration = maybe_duration?;
                            let resource_usage = maybe_resource_usage?;

                            Ok(Task {
                                start_time: start_time.into(),
                                duration,
                                resource_usage,
                            })
                        },
                    )
                    .collect::<Result<Vec<_>, fzn_rs::InstanceError>>()?;

                Constraint::Cumulative(Cumulative {
                    tasks,
                    capacity: *capacity,
                })
            }

            FlatZincConstraints::AllDifferent(variables) => {
                let variables = fzn_model
                    .resolve_array(variables)?
                    .map(|maybe_variable| maybe_variable.map(Variable::from))
                    .collect::<Result<Vec<_>, _>>()?;

                Constraint::AllDifferent(AllDifferent { variables })
            }
        };

        let _ = model.add_constraint(constraint_id, constraint);
    }

    Ok(model)
}
