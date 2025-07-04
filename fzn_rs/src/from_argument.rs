use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast;
use crate::InstanceError;
use crate::IntVariable;

pub trait FromLiteral: Sized {
    fn from_literal(
        node: &ast::Node<ast::Literal>,
        arrays: &BTreeMap<Rc<str>, ast::Array>,
    ) -> Result<Self, InstanceError>;
}

impl FromLiteral for i64 {
    fn from_literal(
        node: &ast::Node<ast::Literal>,
        _: &BTreeMap<Rc<str>, ast::Array>,
    ) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Int(value) => Ok(*value),
            ast::Literal::Identifier(_) => todo!(),
            ast::Literal::Bool(_) => todo!(),
            ast::Literal::IntSet(_) => todo!(),
        }
    }
}

impl FromLiteral for IntVariable {
    fn from_literal(
        node: &ast::Node<ast::Literal>,
        _: &BTreeMap<Rc<str>, ast::Array>,
    ) -> Result<Self, InstanceError> {
        match &node.node {
            ast::Literal::Identifier(identifier) => {
                Ok(IntVariable::Identifier(Rc::clone(identifier)))
            }
            ast::Literal::Int(constant) => Ok(IntVariable::Constant(*constant)),
            ast::Literal::Bool(_) => todo!(),
            ast::Literal::IntSet(_) => todo!(),
        }
    }
}

pub trait FromArgument: Sized {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Array>,
    ) -> Result<Self, InstanceError>;
}

impl<T: FromLiteral> FromArgument for T {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Array>,
    ) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Argument::Literal(literal) => T::from_literal(literal, arrays),
            ast::Argument::Array(literals) => todo!(),
        }
    }
}

impl<T: FromLiteral> FromArgument for Vec<T> {
    fn from_argument(
        argument: &ast::Node<ast::Argument>,
        arrays: &BTreeMap<Rc<str>, ast::Array>,
    ) -> Result<Self, InstanceError> {
        match &argument.node {
            ast::Argument::Array(literals) => literals
                .iter()
                .map(|literal| T::from_literal(literal, arrays))
                .collect::<Result<_, _>>(),

            ast::Argument::Literal(literal) => todo!(),
        }
    }
}
