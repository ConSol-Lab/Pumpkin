use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::str::FromStr;

use super::ast::FlatZincAst;
use super::ast::FlatZincAstBuilder;
use super::ast::SingleVarDecl;
use super::ast::VarArrayDecl;
use super::FlatZincError;

/// The minimum value which an integer variable can take on.
///
/// It is divided by 2 to avoid underflows.
const MIN_VALUE: i32 = i32::MIN / 2;
/// The maximum value which an integer variable can take on.
///
/// It is divided by 2 to avoid overflows.
const MAX_VALUE: i32 = i32::MAX / 2;

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

        flatzinc::VarDeclItem::Int { id, expr, annos } => {
            // For unbounded integers, we take the minimum and maximum possible values
            ast.add_variable_decl(SingleVarDecl::IntInRange {
                id,
                lb: MIN_VALUE as i128,
                ub: MAX_VALUE as i128,
                expr,
                annos,
            });
            Ok(())
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
