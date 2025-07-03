//! The AST representing a FlatZinc instance. This AST is compatible with both the JSON format and
//! the original FZN format, and is a modified version of the `FlatZinc` type from
//! [`flatzinc-serde`](https://docs.rs/flatzinc-serde).
use std::collections::BTreeMap;
use std::rc::Rc;

/// Represents a FlatZinc instance.
///
/// In the `.fzn` format, identifiers can point to both constants and variables (either single or
/// arrays). In this AST, the constants are immediately resolved and are not kept in their original
/// form. Therefore, any [`Literal::Identifier`] points to a variable.
///
/// All identifiers are [`Rc`]s to allow parsers to re-use the allocation of the variable name.
#[derive(Clone, Debug)]
pub struct Ast {
    /// A mapping from identifiers to variables.
    pub variables: BTreeMap<Rc<str>, Variable<Annotation>>,
    /// The arrays in this instance.
    pub arrays: BTreeMap<Rc<str>, Array>,
    /// A list of constraints.
    pub constraints: Vec<Constraint>,
    /// The goal of the model.
    pub solve: SolveObjective,
}

/// A decision variable.
#[derive(Clone, Debug)]
pub struct Variable<Ann> {
    /// The domain of the variable.
    pub domain: Domain,
    /// The value that the variable is equal to.
    pub value: Option<Literal>,
    /// The annotations on this variable.
    pub annotations: Vec<Ann>,
}

/// A named array of literals.
#[derive(Clone, Debug)]
pub struct Array {
    /// The elements of the array.
    pub contents: Vec<Literal>,
    /// The annotations associated with this array.
    pub annotations: Vec<Annotation>,
}

/// The domain of a [`Variable`].
#[derive(Clone, Debug)]
pub enum Domain {
    /// The set of all integers.
    UnboundedInt,
    /// A finite set of integer values.
    Int(RangeList<i64>),
}

/// Holds a non-empty set of values.
#[derive(Clone, Debug)]
pub struct RangeList<E> {
    /// A sorted list of intervals.
    ///
    /// Invariant: Consecutive intervals are merged.
    intervals: Vec<(E, E)>,
}

impl<E: PartialOrd> RangeList<E> {
    /// The smallest element in the set.
    pub fn lower_bound(&self) -> &E {
        &self.intervals[0].0
    }

    /// The largest element in the set.
    pub fn upper_bound(&self) -> &E {
        let last_idx = self.intervals.len() - 1;

        &self.intervals[last_idx].1
    }

    /// Returns `true` if the set is a continious range from [`Self::lower_bound`] to
    /// [`Self::upper_bound`].
    pub fn is_continuous(&self) -> bool {
        self.intervals.len() == 1
    }
}

/// A literal in the instance.
#[derive(Clone, Debug)]
pub enum Literal {
    Int(i64),
    Identifier(Rc<str>),
    Bool(bool),
    IntSet(RangeList<i64>),
}

#[derive(Clone, Debug)]
pub struct SolveObjective {
    pub method: Method,
    pub annotations: Vec<Annotation>,
}

#[derive(Clone, Debug)]
pub enum Method {
    Satisfy,
    Optimize {
        direction: OptimizationDirection,
        objective: Rc<str>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum OptimizationDirection {
    Minimize,
    Maximize,
}

/// A constraint definition.
#[derive(Clone, Debug)]
pub struct Constraint {
    /// The name of the constraint.
    pub name: Rc<str>,
    /// The list of arguments.
    pub arguments: Vec<Argument>,
    /// Any annotations on the constraint.
    pub annotations: Vec<Annotation>,
}

/// An argument for a [`Constraint`].
#[derive(Clone, Debug)]
pub enum Argument {
    Array(Vec<Literal>),
    Literal(Literal),
}

#[derive(Clone, Debug)]
pub enum Annotation {
    Atom(Rc<str>),
    Call(AnnotationCall),
}

#[derive(Clone, Debug)]
pub struct AnnotationCall {
    /// The name of the annotation.
    pub name: Rc<str>,
    /// Any arguments for the annotation.
    pub arguments: Vec<AnnotationArgument>,
}

/// An individual argument for an [`Annotation`].
#[derive(Clone, Debug)]
pub enum AnnotationArgument {
    Array(Vec<AnnotationLiteral>),
    Literal(AnnotationLiteral),
}

#[derive(Clone, Debug)]
pub enum AnnotationLiteral {
    BaseLiteral(Literal),
    Annotation(Annotation),
}
