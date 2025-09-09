use std::collections::BTreeMap;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;
use fzn_rs::ast::Domain;
use fzn_rs::VariableExpr;

#[derive(Clone, Debug)]
pub enum Constraint {
    Nogood(Nogood),
    LinearLeq(Linear),
    LinearEq(Linear),
}

pub type Atomic = IntAtomic<Rc<str>, i32>;

#[derive(Clone, Debug)]
pub struct Nogood(Vec<Atomic>);

impl<T> From<T> for Nogood
where
    T: IntoIterator<Item = Atomic>,
{
    fn from(value: T) -> Self {
        Nogood(value.into_iter().collect())
    }
}

impl AsRef<[Atomic]> for Nogood {
    fn as_ref(&self) -> &[Atomic] {
        self.0.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct Linear {
    pub terms: Vec<(i32, VariableExpr<i32>)>,
    pub bound: i32,
}

#[derive(Clone, Debug, Default)]
pub struct Model {
    variables: BTreeMap<Rc<str>, Domain>,
    constraints: BTreeMap<ConstraintId, Constraint>,
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
    pub fn is_trivially_true(&self, atomic: Atomic) -> bool {
        let Some(domain) = self.variables.get(&atomic.name) else {
            return false;
        };

        match domain {
            Domain::UnboundedInt => false,
            Domain::Int(dom) => match atomic.comparison {
                drcp_format::IntComparison::GreaterEqual => {
                    *dom.lower_bound() >= atomic.value as i64
                }
                drcp_format::IntComparison::LessEqual => *dom.upper_bound() <= atomic.value as i64,
                drcp_format::IntComparison::Equal => {
                    *dom.lower_bound() >= atomic.value as i64
                        && *dom.upper_bound() <= atomic.value as i64
                }
                drcp_format::IntComparison::NotEqual => todo!(),
            },
            Domain::Bool => todo!("boolean variables are not yet supported"),
        }
    }
}
