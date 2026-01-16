//! Defines what models the checker can check proofs for.
//!
//! The main component of the model are the constraints that the checker supports.

use std::collections::BTreeMap;
use std::num::NonZero;
use std::ops::Deref;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;
use fzn_rs::VariableExpr;
use fzn_rs::ast::Domain;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::Comparison;
use pumpkin_checking::I32Ext;
use pumpkin_checking::VariableState;

use crate::math::div_ceil;
use crate::math::div_floor;

#[derive(Clone, Debug)]
pub enum Constraint {
    Nogood(Nogood),
    LinearLeq(Linear),
    LinearEq(Linear),
    Cumulative(Cumulative),
    AllDifferent(AllDifferent),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Atomic {
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

#[derive(Clone, Debug)]
pub struct Nogood(Vec<Atomic>);

impl<T, A> From<T> for Nogood
where
    T: IntoIterator<Item = A>,
    A: Into<Atomic>,
{
    fn from(value: T) -> Self {
        Nogood(value.into_iter().map(Into::into).collect())
    }
}

impl Deref for Nogood {
    type Target = [Atomic];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A checker variable that can be used with [`pumpkin_checking::VariableState`].
#[derive(Clone, Debug)]
pub struct Variable(VariableExpr<i32>);

impl From<VariableExpr<i32>> for Variable {
    fn from(value: VariableExpr<i32>) -> Self {
        Variable(value)
    }
}

impl CheckerVariable<Atomic> for Variable {
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

    fn induced_lower_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext {
        match self.0 {
            VariableExpr::Identifier(ref ident) => variable_state.lower_bound(ident),
            VariableExpr::Constant(value) => value.into(),
        }
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext {
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
}

#[derive(Clone, Debug)]
pub struct Linear {
    pub terms: Vec<Term>,
    pub bound: i32,
}

#[derive(Clone, Debug)]
pub struct Term {
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

    fn induced_lower_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext {
        if self.weight.is_positive() {
            self.variable.induced_lower_bound(variable_state) * self.weight.get()
        } else {
            self.variable.induced_upper_bound(variable_state) * self.weight.get()
        }
    }

    fn induced_upper_bound(&self, variable_state: &VariableState<Atomic>) -> I32Ext {
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
}

#[derive(Clone, Debug)]
pub struct Task {
    pub start_time: Variable,
    pub duration: i32,
    pub resource_usage: i32,
}

#[derive(Clone, Debug)]
pub struct Cumulative {
    pub tasks: Vec<Task>,
    pub capacity: i32,
}

#[derive(Clone, Debug)]
pub struct AllDifferent {
    pub variables: Vec<Variable>,
}

#[derive(Clone, Debug)]
pub enum Objective {
    Maximize(Variable),
    Minimize(Variable),
}

#[derive(Clone, Debug, Default)]
pub struct Model {
    variables: BTreeMap<Rc<str>, Domain>,
    constraints: BTreeMap<ConstraintId, Constraint>,
    pub objective: Option<Objective>,
}

impl Model {
    /// Add a new variable to the model.
    pub fn add_variable(&mut self, name: Rc<str>, domain: Domain) {
        let _ = self.variables.insert(name, domain);
    }

    /// Add a new constraint to the model.
    ///
    /// If a constraint with the given ID already exists, this returns false. Otherwise, the
    /// function returns true.
    pub fn add_constraint(&mut self, constraint_id: ConstraintId, constraint: Constraint) -> bool {
        self.constraints.insert(constraint_id, constraint).is_none()
    }

    /// Iterate over the constraints in the map, ordered by [`ConstraintId`].
    pub fn iter_constraints(
        &self,
    ) -> std::collections::btree_map::Iter<'_, ConstraintId, Constraint> {
        self.constraints.iter()
    }

    /// Get the constraint with the given ID if it exists.
    pub fn get_constraint(&self, constraint_id: ConstraintId) -> Option<&Constraint> {
        self.constraints.get(&constraint_id)
    }

    /// Test whether the atomic is true in the initial domains of the variables.
    ///
    /// Returns false if the atomic is over a variable that is not in the model.
    pub fn is_trivially_true(&self, atomic: &Atomic) -> bool {
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
