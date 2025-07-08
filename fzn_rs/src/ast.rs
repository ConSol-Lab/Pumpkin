//! The AST representing a FlatZinc instance, compatible with both the JSON format and
//! the original FZN format.
//!
//! It is a modified version of the `FlatZinc` type from [`flatzinc-serde`](https://docs.rs/flatzinc-serde).
use std::collections::BTreeMap;
use std::fmt::Display;
use std::ops::RangeInclusive;
use std::rc::Rc;

/// Describes a range `[start, end)` in the source.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    /// The index in the source that starts the span.
    pub start: usize,
    /// The index in the source that ends the span.
    ///
    /// Note the end is exclusive.
    pub end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

#[cfg(feature = "fzn")]
impl chumsky::span::Span for Span {
    type Context = ();

    type Offset = usize;

    fn new(_: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

#[cfg(feature = "fzn")]
impl From<chumsky::span::SimpleSpan> for Span {
    fn from(value: chumsky::span::SimpleSpan) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}

#[cfg(feature = "fzn")]
impl From<Span> for chumsky::span::SimpleSpan {
    fn from(value: Span) -> Self {
        chumsky::span::SimpleSpan::from(value.start..value.end)
    }
}

/// A node in the [`Ast`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node<T> {
    /// The span in the source of this node.
    pub span: Span,
    /// The parsed node.
    pub node: T,
}

/// Represents a FlatZinc instance.
///
/// In the `.fzn` format, identifiers can point to both constants and variables (either single or
/// arrays). In this AST, the constants are immediately resolved and are not kept in their original
/// form. Therefore, any [`Literal::Identifier`] points to a variable.
///
/// All identifiers are [`Rc`]s to allow parsers to re-use the allocation of the variable name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ast {
    /// A mapping from identifiers to variables.
    pub variables: BTreeMap<Rc<str>, Node<Variable<Annotation>>>,
    /// The arrays in this instance.
    pub arrays: BTreeMap<Rc<str>, Node<Array>>,
    /// A list of constraints.
    pub constraints: Vec<Node<Constraint>>,
    /// The goal of the model.
    pub solve: SolveObjective<Annotation>,
}

/// A decision variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable<Ann> {
    /// The domain of the variable.
    pub domain: Node<Domain>,
    /// The value that the variable is equal to.
    pub value: Option<Node<Literal>>,
    /// The annotations on this variable.
    pub annotations: Vec<Node<Ann>>,
}

/// A named array of literals.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Array {
    /// The elements of the array.
    pub contents: Vec<Node<Literal>>,
    /// The annotations associated with this array.
    pub annotations: Vec<Node<Annotation>>,
}

/// The domain of a [`Variable`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Domain {
    /// The set of all integers.
    UnboundedInt,
    /// A finite set of integer values.
    Int(RangeList<i64>),
    /// A boolean domain.
    Bool,
}

/// Holds a non-empty set of values.
#[derive(Clone, Debug, PartialEq, Eq)]
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

impl<E: Copy + Ord> From<RangeInclusive<E>> for RangeList<E> {
    fn from(value: RangeInclusive<E>) -> Self {
        RangeList {
            intervals: vec![(*value.start(), *value.end())],
        }
    }
}

impl<E: Copy + Ord> FromIterator<E> for RangeList<E> {
    fn from_iter<T: IntoIterator<Item = E>>(iter: T) -> Self {
        let mut intervals: Vec<_> = iter.into_iter().map(|e| (e, e)).collect();
        intervals.sort();

        RangeList { intervals }
    }
}

/// A literal in the instance.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    Identifier(Rc<str>),
    Bool(bool),
    IntSet(RangeList<i64>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SolveObjective<Ann> {
    pub method: Node<Method>,
    pub annotations: Vec<Node<Ann>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Method {
    Satisfy,
    Optimize {
        direction: OptimizationDirection,
        objective: Rc<str>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OptimizationDirection {
    Minimize,
    Maximize,
}

/// A constraint definition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Constraint {
    /// The name of the constraint.
    pub name: Node<Rc<str>>,
    /// The list of arguments.
    pub arguments: Vec<Node<Argument>>,
    /// Any annotations on the constraint.
    pub annotations: Vec<Node<Annotation>>,
}

/// An argument for a [`Constraint`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Argument {
    Array(Vec<Node<Literal>>),
    Literal(Node<Literal>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Annotation {
    Atom(Rc<str>),
    Call(AnnotationCall),
}

impl Annotation {
    pub fn name(&self) -> &str {
        match self {
            Annotation::Atom(name) => name,
            Annotation::Call(call) => &call.name,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AnnotationCall {
    /// The name of the annotation.
    pub name: Rc<str>,
    /// Any arguments for the annotation.
    pub arguments: Vec<Node<AnnotationArgument>>,
}

/// An individual argument for an [`Annotation`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AnnotationArgument {
    Array(Vec<Node<AnnotationLiteral>>),
    Literal(Node<AnnotationLiteral>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AnnotationLiteral {
    BaseLiteral(Literal),
    /// In the FZN grammar, this is an `Annotation` instead of an `AnnotationCall`. We divirge from
    /// the grammar to avoid the case where the same input can parse to either a
    /// `Annotation::Atom(ident)` or an `Literal::Identifier`.
    Annotation(AnnotationCall),
}
