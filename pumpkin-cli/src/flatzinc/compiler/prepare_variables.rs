//! Scan through all the variable definitions to prepare the equivalence classes for each of them.

use flatzinc::BoolExpr;
use flatzinc::IntExpr;

use crate::flatzinc::ast::FlatZincAst;
use crate::flatzinc::ast::SingleVarDecl;
use crate::flatzinc::compiler::context::CompilationContext;
use crate::flatzinc::FlatZincError;

pub fn run(ast: &FlatZincAst, context: &mut CompilationContext) -> Result<(), FlatZincError> {
    for single_var_decl in &ast.single_variables {
        match single_var_decl {
            SingleVarDecl::Bool { id, expr, annos: _ } => {
                let id = context.identifiers.get_interned(id);

                let (lb, ub) = match expr {
                    None => (0, 1),

                    Some(BoolExpr::Bool(true)) => (1, 1),
                    Some(BoolExpr::Bool(false)) => (0, 0),

                    Some(BoolExpr::VarParIdentifier(identifier)) => {
                        let other_id = context.identifiers.get_interned(identifier);

                        match context.boolean_parameters.get(&other_id) {
                            Some(true) => (1, 1),
                            Some(false) => (0, 0),
                            None => (0, 1),
                        }
                    }
                };

                context
                    .literal_equivalences
                    .create_equivalence_class(id, lb, ub);
            }

            SingleVarDecl::IntInRange {
                id,
                lb,
                ub,
                expr,
                annos: _,
            } => {
                let id = context.identifiers.get_interned(id);

                let lb = i32::try_from(*lb)?;
                let ub = i32::try_from(*ub)?;

                let (lb, ub) = match expr {
                    None => (lb, ub),

                    Some(IntExpr::Int(value)) => {
                        let value = i32::try_from(*value)?;
                        (value, value)
                    }

                    Some(IntExpr::VarParIdentifier(identifier)) => {
                        let other_id = context.identifiers.get_interned(identifier);

                        match context.integer_parameters.get(&other_id) {
                            Some(int) => (*int, *int),
                            None => (lb, ub),
                        }
                    }
                };

                context
                    .integer_equivalences
                    .create_equivalence_class(id, lb, ub);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use pumpkin_lib::engine::ConstraintSatisfactionSolver;

    use super::*;
    use crate::flatzinc::ast::SingleVarDecl;
    use crate::flatzinc::compiler::context::Domain;

    #[test]
    fn bool_variable_creates_equivalence_class() {
        let ast = create_dummy_ast([SingleVarDecl::Bool {
            id: "SomeVar".into(),
            expr: None,
            annos: vec![],
        }]);

        let mut solver = ConstraintSatisfactionSolver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        let domain = context.literal_equivalences.domain("SomeVar");
        assert_eq!(Domain::from(0, 1), domain);
    }

    #[test]
    fn bool_variable_equal_to_constant_as_singleton_domain() {
        let ast = create_dummy_ast([
            SingleVarDecl::Bool {
                id: "SomeVar".into(),
                expr: Some(BoolExpr::Bool(false)),
                annos: vec![],
            },
            SingleVarDecl::Bool {
                id: "OtherVar".into(),
                expr: Some(BoolExpr::Bool(true)),
                annos: vec![],
            },
        ]);

        let mut solver = ConstraintSatisfactionSolver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from(0, 0),
            context.literal_equivalences.domain("SomeVar")
        );
        assert_eq!(
            Domain::from(1, 1),
            context.literal_equivalences.domain("OtherVar")
        );
    }

    #[test]
    fn bool_expr_resolves_parameter() {
        let ast = create_dummy_ast([SingleVarDecl::Bool {
            id: "SomeVar".into(),
            expr: Some(BoolExpr::VarParIdentifier("FalsePar".into())),
            annos: vec![],
        }]);

        let mut solver = ConstraintSatisfactionSolver::default();
        let mut context = CompilationContext::new(&mut solver);
        context.boolean_parameters.insert("FalsePar".into(), false);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from(0, 0),
            context.literal_equivalences.domain("SomeVar")
        );
    }

    #[test]
    fn bool_expr_ignores_reference_to_non_existent_identifier() {
        let ast = create_dummy_ast([SingleVarDecl::Bool {
            id: "SomeVar".into(),
            expr: Some(BoolExpr::VarParIdentifier("OtherVar".into())),
            annos: vec![],
        }]);

        let mut solver = ConstraintSatisfactionSolver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from(0, 1),
            context.literal_equivalences.domain("SomeVar")
        );
    }

    #[test]
    fn int_expr_constant_is_parsed() {
        let ast = create_dummy_ast([SingleVarDecl::IntInRange {
            id: "SomeVar".into(),
            lb: 1,
            ub: 5,
            expr: Some(IntExpr::Int(3)),
            annos: vec![],
        }]);

        let mut solver = ConstraintSatisfactionSolver::default();
        let mut context = CompilationContext::new(&mut solver);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from(3, 3),
            context.integer_equivalences.domain("SomeVar")
        );
    }

    #[test]
    fn int_expr_named_constant_is_resolved() {
        let ast = create_dummy_ast([SingleVarDecl::IntInRange {
            id: "SomeVar".into(),
            lb: 1,
            ub: 5,
            expr: Some(IntExpr::VarParIdentifier("IntPar".into())),
            annos: vec![],
        }]);

        let mut solver = ConstraintSatisfactionSolver::default();
        let mut context = CompilationContext::new(&mut solver);
        context.integer_parameters.insert("IntPar".into(), 3);

        run(&ast, &mut context).expect("no errors");

        assert_eq!(
            Domain::from(3, 3),
            context.integer_equivalences.domain("SomeVar")
        );
    }

    fn create_dummy_ast(decls: impl IntoIterator<Item = SingleVarDecl>) -> FlatZincAst {
        FlatZincAst {
            parameter_decls: vec![],
            single_variables: decls.into_iter().collect(),
            variable_arrays: vec![],
            constraint_decls: vec![],
            solve_item: flatzinc::SolveItem {
                goal: flatzinc::Goal::Satisfy,
                annotations: vec![],
            },
        }
    }
}
