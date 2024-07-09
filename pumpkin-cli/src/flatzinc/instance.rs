use std::fmt::Display;
use std::fmt::Write;
use std::rc::Rc;

use pumpkin_lib::branching::branchers::dynamic_brancher::DynamicBrancher;
use pumpkin_lib::engine::variables::BooleanDomainId;
use pumpkin_lib::engine::variables::DomainId;

/// The objective function of a FlatZinc model,
/// consisting of the direction (e.g. maximization or minimization) and the integer variable which
/// is being optimised
#[derive(Debug, Clone, Copy)]
pub(crate) enum FlatzincObjective {
    Maximize(DomainId),
    Minimize(DomainId),
}

impl FlatzincObjective {
    /// Returns the [DomainId] of the objective function
    pub(crate) fn get_domain(&self) -> &DomainId {
        match self {
            FlatzincObjective::Maximize(domain) => domain,
            FlatzincObjective::Minimize(domain) => domain,
        }
    }
}

#[derive(Default)]
pub(crate) struct FlatZincInstance {
    pub(super) outputs: Vec<Output>,
    pub(super) objective_function: Option<FlatzincObjective>,
    pub(super) search: Option<DynamicBrancher>,
}

impl FlatZincInstance {
    #[cfg(test)]
    pub(crate) fn outputs(&self) -> impl Iterator<Item = &Output> + '_ {
        self.outputs.iter()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Output {
    Bool(VariableOutput<BooleanDomainId>),
    Int(VariableOutput<DomainId>),
    ArrayOfBool(ArrayOutput<BooleanDomainId>),
    ArrayOfInt(ArrayOutput<DomainId>),
}

impl Output {
    pub(crate) fn bool(id: Rc<str>, boolean: BooleanDomainId) -> Output {
        Output::Bool(VariableOutput {
            id,
            variable: boolean,
        })
    }

    pub(crate) fn array_of_bool(
        id: Rc<str>,
        shape: Box<[(i32, i32)]>,
        contents: Rc<[BooleanDomainId]>,
    ) -> Output {
        Output::ArrayOfBool(ArrayOutput {
            id,
            shape,
            contents,
        })
    }

    pub(crate) fn int(id: Rc<str>, domain_id: DomainId) -> Output {
        Output::Int(VariableOutput {
            id,
            variable: domain_id,
        })
    }

    pub(crate) fn array_of_int(
        id: Rc<str>,
        shape: Box<[(i32, i32)]>,
        contents: Rc<[DomainId]>,
    ) -> Output {
        Output::ArrayOfInt(ArrayOutput {
            id,
            shape,
            contents,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct VariableOutput<T> {
    id: Rc<str>,
    variable: T,
}

impl<T> VariableOutput<T> {
    pub(crate) fn print_value<V: Display>(&self, value: impl FnOnce(&T) -> V) {
        println!("{} = {};", self.id, value(&self.variable));
    }

    pub(crate) fn get_variable(&self) -> &T {
        &self.variable
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ArrayOutput<T> {
    id: Rc<str>,
    /// The shape of the array is a sequence of index sets. The number of elements in this sequence
    /// corresponds to the dimensionality of the array, and the element in the sequence at index i
    /// denotes the index set used in dimension i.
    /// Example: [(1, 5), (2, 4)] describes a 2d array, where the first dimension in indexed with
    /// an element of 1..5, and the second dimension is indexed with an element from 2..4.
    shape: Box<[(i32, i32)]>,
    contents: Rc<[T]>,
}

impl<T> ArrayOutput<T> {
    pub(crate) fn print_value<V: Display>(&self, value: impl Fn(&T) -> V) {
        let mut array_buf = String::new();

        for element in self.contents.iter() {
            let value = value(element);
            write!(array_buf, "{value}, ").unwrap();
        }

        let mut shape_buf = String::new();
        for (min, max) in self.shape.iter() {
            write!(shape_buf, "{min}..{max}, ").unwrap();
        }

        if !array_buf.is_empty() {
            // Remove trailing comma and space.
            array_buf.truncate(array_buf.len() - 2);
        }

        let num_dimensions = self.shape.len();
        println!(
            "{} = array{num_dimensions}d({shape_buf}[{array_buf}]);",
            self.id
        );
    }

    pub(crate) fn get_contents(&self) -> impl Iterator<Item = &T> {
        self.contents.iter()
    }
}
