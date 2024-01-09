use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::{Deref, RangeInclusive},
    rc::Rc,
};

use pumpkin_lib::basic_types::{DomainId, Literal};

#[derive(Default)]
pub struct FlatZincInstance {
    integer_array_parameters: HashMap<Rc<str>, Box<[i32]>>,
    integer_parameters: HashMap<Rc<str>, i32>,

    integer_variables: HashMap<Rc<str>, RangeInclusive<i32>>,
    array_of_integer_variables: HashMap<Rc<str>, Box<[Rc<str>]>>,

    bool_variables: HashSet<Rc<str>>,

    output_variables: Vec<OutputVariable>,

    constraints: Vec<flatzinc::ConstraintItem>,
}

#[derive(Clone)]
pub enum OutputVariable {
    Variable(Rc<str>),
    VariableArray(Rc<str>),
}

impl FlatZincInstance {
    pub fn builder() -> FlatZincInstanceBuilder {
        FlatZincInstanceBuilder {
            instance: Default::default(),
        }
    }

    pub fn iter_integer_variables(
        &self,
    ) -> impl Iterator<Item = (Rc<str>, RangeInclusive<i32>)> + '_ {
        self.integer_variables
            .iter()
            .map(|(id, bounds)| (Rc::clone(id), bounds.clone()))
    }

    pub fn iter_bool_variables(&self) -> impl Iterator<Item = Rc<str>> + '_ {
        self.bool_variables.iter().cloned()
    }

    pub fn iter_constraints(&self) -> impl Iterator<Item = &flatzinc::ConstraintItem> + '_ {
        self.constraints.iter()
    }

    pub fn resolve_array_integer_constants(&self, id: &str) -> Option<Box<[i32]>> {
        self.integer_array_parameters.get(id).cloned()
    }

    pub fn resolve_integer_constant(&self, id: &str) -> Option<i32> {
        self.integer_parameters.get(id).copied()
    }

    pub fn resolve_variable_array(&self, id: &str) -> Option<&[Rc<str>]> {
        self.array_of_integer_variables
            .get(id)
            .map(|array| array.deref())
    }

    pub fn iter_output_variables(&self) -> impl Iterator<Item = OutputVariable> + '_ {
        self.output_variables.iter().cloned()
    }
}

pub struct FlatZincInstanceBuilder {
    instance: FlatZincInstance,
}

impl FlatZincInstanceBuilder {
    pub fn add_integer_array_parameter(&mut self, id: Rc<str>, values: Box<[i32]>) {
        self.instance.integer_array_parameters.insert(id, values);
    }

    pub fn add_integer_variable(
        &mut self,
        id: Rc<str>,
        lb: i32,
        ub: i32,
        is_output_variable: bool,
    ) {
        self.instance
            .integer_variables
            .insert(Rc::clone(&id), lb..=ub);

        if is_output_variable {
            self.instance
                .output_variables
                .push(OutputVariable::Variable(id));
        }
    }

    pub fn add_bool_variable(&mut self, id: Rc<str>, is_output_variable: bool) {
        self.instance.bool_variables.insert(Rc::clone(&id));

        if is_output_variable {
            self.instance
                .output_variables
                .push(OutputVariable::Variable(id));
        }
    }

    pub fn build(self) -> FlatZincInstance {
        self.instance
    }

    pub fn add_integer_variable_array(
        &mut self,
        id: Rc<str>,
        array: Box<[Rc<str>]>,
        is_output_variable: bool,
    ) {
        self.instance
            .array_of_integer_variables
            .insert(Rc::clone(&id), array);

        if is_output_variable {
            self.instance
                .output_variables
                .push(OutputVariable::VariableArray(id));
        }
    }

    pub fn add_constraint_item(&mut self, constraint_decl: flatzinc::ConstraintItem) {
        self.instance.constraints.push(constraint_decl);
    }
}

#[derive(Default)]
pub struct VariableMap {
    integer_variables: BTreeMap<Rc<str>, DomainId>,
    bool_variables: BTreeMap<Rc<str>, Literal>,
}

pub enum Variable {
    Integer(DomainId),
    Bool(Literal),
}

impl VariableMap {
    pub fn register_integer_variable(&mut self, id: Rc<str>, domain: DomainId) {
        self.integer_variables.insert(id, domain);
    }

    pub fn register_bool_variable(&mut self, id: Rc<str>, literal: Literal) {
        self.bool_variables.insert(id, literal);
    }

    pub fn resolve(&self, id: &str) -> Option<Variable> {
        self.integer_variables
            .get(id)
            .copied()
            .map(Variable::Integer)
            .or_else(|| self.bool_variables.get(id).copied().map(Variable::Bool))
    }
}
