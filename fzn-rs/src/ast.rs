//! The AST representing a FlatZinc instance, compatible with both the JSON format and
//! the original FZN format.
//!
//! It is a modified version of the `FlatZinc` type from [`flatzinc-serde`](https://docs.rs/flatzinc-serde).
use std::collections::BTreeMap;
use std::fmt::Display;
use std::iter::FusedIterator;
use std::ops::RangeInclusive;
use std::rc::Rc;

/// Represents a FlatZinc instance.
///
/// In the `.fzn` format, identifiers can point to both constants and variables (either single or
/// arrays). In this AST, the constants are immediately resolved and are not kept in their original
/// form. Therefore, any [`Literal::Identifier`] points to a variable or an array.
///
/// All identifiers are [`Rc`]s to allow parsers to re-use the allocation of the variable name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ast {
    /// A mapping from identifiers to variables.
    pub variables: BTreeMap<Rc<str>, Node<Variable<Annotation>>>,
    /// The arrays in this instance.
    pub arrays: BTreeMap<Rc<str>, Node<Array<Annotation>>>,
    /// A list of constraints.
    pub constraints: Vec<Node<Constraint>>,
    /// The goal of the model.
    pub solve: SolveItem<Annotation>,
}

/// A decision variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable<Ann> {
    /// The domain of the variable.
    pub domain: Node<Domain>,
    /// Optionally, the value that the variable is equal to.
    pub value: Option<Node<Literal>>,
    /// The annotations on this variable.
    pub annotations: Vec<Node<Ann>>,
}

/// A named array of literals.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Array<Ann> {
    /// The domain of the elements of the array.
    pub domain: Node<Domain>,
    /// The elements of the array.
    pub contents: Vec<Node<Literal>>,
    /// The annotations associated with this array.
    pub annotations: Vec<Node<Ann>>,
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

macro_rules! impl_iter_fn {
    ($int_type:ty) => {
        impl<'a> IntoIterator for &'a RangeList<$int_type> {
            type Item = $int_type;

            type IntoIter = RangeListIter<'a, $int_type>;

            fn into_iter(self) -> Self::IntoIter {
                RangeListIter {
                    current_interval: self.intervals.first().copied().unwrap_or((1, 0)),
                    tail: &self.intervals[1..],
                }
            }
        }
    };
}

impl_iter_fn!(i32);
impl_iter_fn!(i64);

impl<E: Copy + Ord> From<RangeInclusive<E>> for RangeList<E> {
    fn from(value: RangeInclusive<E>) -> Self {
        RangeList {
            intervals: vec![(*value.start(), *value.end())],
        }
    }
}

macro_rules! range_list_from_iter {
    ($int_type:ty) => {
        impl FromIterator<$int_type> for RangeList<$int_type> {
            fn from_iter<T: IntoIterator<Item = $int_type>>(iter: T) -> Self {
                let mut intervals: Vec<_> = iter.into_iter().map(|e| (e, e)).collect();
                intervals.sort();
                intervals.dedup();

                let mut idx = 0;

                while idx < intervals.len() - 1 {
                    let current = intervals[idx];
                    let next = intervals[idx + 1];

                    if current.1 >= next.0 - 1 {
                        intervals[idx] = (current.0, next.1);
                        let _ = intervals.remove(idx + 1);
                    } else {
                        idx += 1;
                    }
                }

                RangeList { intervals }
            }
        }
    };
}

range_list_from_iter!(i32);
range_list_from_iter!(i64);

/// An [`Iterator`] over a [`RangeList`].
#[derive(Debug)]
pub struct RangeListIter<'a, E> {
    current_interval: (E, E),
    tail: &'a [(E, E)],
}

macro_rules! impl_range_list_iter {
    ($int_type:ty) => {
        impl Iterator for RangeListIter<'_, $int_type> {
            type Item = $int_type;

            fn next(&mut self) -> Option<Self::Item> {
                let (current_lb, current_ub) = self.current_interval;

                if current_lb > current_ub {
                    let (next_interval, new_tail) = self.tail.split_first()?;
                    self.current_interval = *next_interval;
                    self.tail = new_tail;
                }

                let current_lb = self.current_interval.0;
                self.current_interval.0 += 1;

                Some(current_lb)
            }
        }

        impl FusedIterator for RangeListIter<'_, $int_type> {}
    };
}

impl_range_list_iter!(i32);
impl_range_list_iter!(i64);

/// The foundational element from which expressions are built. Literals are the values/identifiers
/// in the model.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    Identifier(Rc<str>),
    Bool(bool),
    IntSet(RangeList<i64>),
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Literal::Int(value)
    }
}

/// The solve item.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SolveItem<Ann> {
    pub method: Node<Method>,
    pub annotations: Vec<Node<Ann>>,
}

/// Whether to satisfy or optimise the model.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Method {
    Satisfy,
    Optimize {
        direction: OptimizationDirection,
        objective: Literal,
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

/// An annotation on any item in the model.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Annotation {
    /// An annotation without arguments.
    Atom(Rc<str>),
    /// An annotation with arguments.
    Call(AnnotationCall),
}

impl Annotation {
    /// Get the name of the annotation.
    pub fn name(&self) -> &str {
        match self {
            Annotation::Atom(name) => name,
            Annotation::Call(call) => &call.name,
        }
    }
}

/// An annotation with arguments.
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

/// An annotation literal is either a regular [`Literal`] or it is another annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AnnotationLiteral {
    BaseLiteral(Literal),
    /// In the FZN grammar, this is an `Annotation` instead of an `AnnotationCall`. We diverge from
    /// the grammar to avoid the case where the same input can parse to either a
    /// `Annotation::Atom(ident)` or an `Literal::Identifier`.
    Annotation(AnnotationCall),
}

/// Describes a range `[start, end)` in the model file that contains a [`Node`].
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

#[cfg(feature = "fzn-parser")]
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

#[cfg(feature = "fzn-parser")]
impl From<chumsky::span::SimpleSpan> for Span {
    fn from(value: chumsky::span::SimpleSpan) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}

#[cfg(feature = "fzn-parser")]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rangelist_from_iter_identifies_continuous_ranges() {
        let set = RangeList::from_iter([1, 2, 3, 4]);

        assert!(set.is_continuous());
    }

    #[test]
    fn rangelist_from_iter_identifiers_non_continuous_ranges() {
        let set = RangeList::from_iter([1, 3, 4, 6]);

        assert!(!set.is_continuous());
    }

    #[test]
    fn rangelist_iter_produces_elements_in_set() {
        let set: RangeList<i32> = RangeList::from_iter([1, 3, 5]);

        let mut iter = set.into_iter();
        assert_eq!(Some(1), iter.next());
        assert_eq!(Some(3), iter.next());
        assert_eq!(Some(5), iter.next());
        assert_eq!(None, iter.next());
    }
}
