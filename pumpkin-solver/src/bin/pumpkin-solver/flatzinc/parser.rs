use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::str::FromStr;

use super::ast::FlatZincAst;
use super::ast::FlatZincAstBuilder;
use super::ast::SingleVarDecl;
use super::ast::VarArrayDecl;
use super::FlatZincError;

pub(crate) fn parse(source: impl Read) -> Result<FlatZincAst, FlatZincError> {
    let reader = BufReader::new(source);

    let mut ast_builder = FlatZincAst::builder();

    for line in reader.lines() {
        let line = line?;

        match flatzinc::statements::Stmt::from_str(&line) {
            Ok(stmt) => match stmt {
                // Ignore.
                flatzinc::Stmt::Comment(_) | flatzinc::Stmt::Predicate(_) => {}

                flatzinc::Stmt::Parameter(decl) => ast_builder.add_parameter_decl(decl),
                flatzinc::Stmt::Variable(decl) => parse_var_decl(&mut ast_builder, decl)?,
                flatzinc::Stmt::Constraint(constraint) => ast_builder.add_constraint(constraint),
                flatzinc::Stmt::SolveItem(solve_item) => ast_builder.set_solve_item(solve_item),
            },
            Err(msg) => {
                return Err(FlatZincError::SyntaxError(msg.into()));
            }
        }
    }

    ast_builder.build()
}

fn parse_var_decl(
    ast: &mut FlatZincAstBuilder,
    decl: flatzinc::VarDeclItem,
) -> Result<(), FlatZincError> {
    match decl {
        flatzinc::VarDeclItem::Bool { id, expr, annos } => {
            ast.add_variable_decl(SingleVarDecl::Bool { id, expr, annos });
            Ok(())
        }

        flatzinc::VarDeclItem::IntInRange {
            id,
            lb,
            ub,
            expr,
            annos,
        } => {
            ast.add_variable_decl(SingleVarDecl::IntInRange {
                id,
                lb,
                ub,
                expr,
                annos,
            });
            Ok(())
        }

        flatzinc::VarDeclItem::IntInSet {
            id,
            set,
            expr: _,
            annos,
        } => {
            ast.add_variable_decl(SingleVarDecl::IntInSet { id, set, annos });
            Ok(())
        }

        flatzinc::VarDeclItem::ArrayOfBool {
            ix: _,
            id,
            annos,
            array_expr,
        } => {
            ast.add_variable_array(VarArrayDecl::Bool {
                id,
                annos,
                array_expr,
            });

            Ok(())
        }

        flatzinc::VarDeclItem::ArrayOfInt {
            ix: _,
            id,
            annos,
            array_expr,
        } => {
            ast.add_variable_array(VarArrayDecl::Int {
                id,
                annos,
                array_expr,
            });
            Ok(())
        }
        flatzinc::VarDeclItem::ArrayOfIntInRange {
            ix: _,
            id,
            annos,
            array_expr,
            ..
        } => {
            ast.add_variable_array(VarArrayDecl::Int {
                id,
                annos,
                array_expr,
            });
            Ok(())
        }

        flatzinc::VarDeclItem::ArrayOfIntInSet {
            ix: _,
            id,
            annos,
            array_expr,
            set: _,
        } => {
            ast.add_variable_array(VarArrayDecl::Int {
                id,
                annos,
                array_expr,
            });
            Ok(())
        }

        flatzinc::VarDeclItem::Int { .. } => {
            Err(FlatZincError::UnsupportedVariable("unbounded int".into()))
        }

        flatzinc::VarDeclItem::Float { .. }
        | flatzinc::VarDeclItem::BoundedFloat { .. }
        | flatzinc::VarDeclItem::ArrayOfFloat { .. }
        | flatzinc::VarDeclItem::ArrayOfBoundedFloat { .. } => {
            Err(FlatZincError::UnsupportedVariable("float".into()))
        }

        flatzinc::VarDeclItem::SetOfInt { .. }
        | flatzinc::VarDeclItem::SubSetOfIntSet { .. }
        | flatzinc::VarDeclItem::SubSetOfIntRange { .. }
        | flatzinc::VarDeclItem::ArrayOfSet { .. }
        | flatzinc::VarDeclItem::ArrayOfSubSetOfIntRange { .. }
        | flatzinc::VarDeclItem::ArrayOfSubSetOfIntSet { .. } => {
            Err(FlatZincError::UnsupportedVariable("set".into()))
        }
    }
}
