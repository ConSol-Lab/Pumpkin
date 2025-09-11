use crate::ast;

/// A constraint that has annotations attached to it.
#[derive(Clone, Debug)]
pub struct AnnotatedConstraint<TConstraint, Annotation> {
    pub constraint: ast::Node<TConstraint>,
    pub annotations: Vec<ast::Node<Annotation>>,
}
