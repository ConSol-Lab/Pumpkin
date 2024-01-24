use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;

use flatzinc::convert_error;
use flatzinc::VerboseError;

use super::ast::FlatZincAst;
use super::ast::FlatZincAstBuilder;
use super::ast::SingleVarDecl;
use super::ast::VarArrayDecl;
use super::FlatZincError;

pub fn parse(source: impl Read) -> Result<FlatZincAst, FlatZincError> {
    let reader = BufReader::new(source);

    let mut ast_builder = FlatZincAst::builder();

    for line in reader.lines() {
        let line = line?;

        match flatzinc::statement::<VerboseError<&str>>(&line) {
            Ok((_, stmt)) => match stmt {
                // Ignore.
                flatzinc::Stmt::Comment(_) | flatzinc::Stmt::Predicate(_) => {}

                flatzinc::Stmt::Parameter(decl) => ast_builder.add_parameter_decl(decl),
                flatzinc::Stmt::Variable(decl) => parse_var_decl(&mut ast_builder, decl)?,
                flatzinc::Stmt::Constraint(constraint) => ast_builder.add_constraint(constraint),
                flatzinc::Stmt::SolveItem(solve_item) => ast_builder.set_solve_item(solve_item),
            },
            Err(flatzinc::Err::Error(e)) | Err(flatzinc::Err::Failure(e)) => {
                let error_msg = convert_error(line.as_str(), e);
                return Err(FlatZincError::SyntaxError(error_msg.into()));
            }
            Err(_) => {
                return Err(FlatZincError::SyntaxError("Input incomplete".into()));
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

        flatzinc::VarDeclItem::ArrayOfBool {
            ix,
            id,
            annos,
            array_expr,
        } => {
            ast.add_variable_array(VarArrayDecl::Bool {
                ix,
                id,
                annos,
                array_expr,
            });

            Ok(())
        }

        flatzinc::VarDeclItem::ArrayOfInt {
            ix,
            id,
            annos,
            array_expr,
        } => {
            ast.add_variable_array(VarArrayDecl::Int {
                ix,
                id,
                annos,
                array_expr,
            });
            Ok(())
        }
        flatzinc::VarDeclItem::ArrayOfIntInRange {
            ix,
            id,
            annos,
            array_expr,
            lb,
            ub,
        } => {
            ast.add_variable_array(VarArrayDecl::IntInRange {
                ix,
                id,
                annos,
                array_expr,
                lb,
                ub,
            });
            Ok(())
        }

        flatzinc::VarDeclItem::Int { .. } => {
            Err(FlatZincError::UnsupportedVariable("unbounded int".into()))
        }

        flatzinc::VarDeclItem::IntInSet { .. } | flatzinc::VarDeclItem::ArrayOfIntInSet { .. } => {
            Err(FlatZincError::UnsupportedVariable("sparse int".into()))
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
