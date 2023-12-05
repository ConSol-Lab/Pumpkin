use std::{
    fmt::Write,
    io::{BufRead, BufReader, Read},
};

use flatzinc::{convert_error, VerboseError};
use log::warn;

use super::{
    instance::{FlatZincInstance, FlatZincInstanceBuilder},
    FlatZincError,
};

pub fn parse(source: impl Read) -> Result<FlatZincInstance, FlatZincError> {
    let reader = BufReader::new(source);
    let mut buf = String::new();

    let mut instance_builder = FlatZincInstance::builder();

    for line in reader.lines() {
        let line = line?;
        write!(buf, "{}", line).unwrap();

        match flatzinc::statement::<VerboseError<&str>>(&line) {
            Ok((_, stmt)) => parse_statement(stmt, &mut instance_builder)?,
            Err(flatzinc::Err::Error(e)) | Err(flatzinc::Err::Failure(e)) => {
                let error_msg = convert_error(buf.as_str(), e);
                return Err(FlatZincError::SyntaxError(error_msg.into()));
            }
            Err(_) => {
                return Err(FlatZincError::SyntaxError("Input incomplete".into()));
            }
        }
    }

    Ok(instance_builder.build())
}

fn parse_statement(
    stmt: flatzinc::Stmt,
    instance_builder: &mut FlatZincInstanceBuilder,
) -> Result<(), FlatZincError> {
    match stmt {
        flatzinc::Stmt::Predicate(_) => todo!("implement predicate parsing"),
        flatzinc::Stmt::Comment(_) => Ok(()),
        flatzinc::Stmt::Parameter(par_decl) => parse_par_decl(par_decl, instance_builder),
        flatzinc::Stmt::Variable(var_decl) => parse_var_decl(var_decl, instance_builder),
        flatzinc::Stmt::Constraint(constraint_decl) => {
            instance_builder.add_constraint_item(constraint_decl);
            Ok(())
        }
        flatzinc::Stmt::SolveItem(flatzinc::SolveItem {
            goal: flatzinc::Goal::Satisfy,
            ..
        }) => Ok(()),
        unknown @ flatzinc::Stmt::SolveItem(flatzinc::SolveItem {
            goal:
                flatzinc::Goal::OptimizeInt(_, _)
                | flatzinc::Goal::OptimizeBool(_, _)
                | flatzinc::Goal::OptimizeFloat(_, _)
                | flatzinc::Goal::OptimizeSet(_, _),
            ..
        }) => todo!("implement parse_statement for {unknown:#?}"),
    }
}

fn parse_var_decl(
    var_decl: flatzinc::VarDeclItem,
    instance_builder: &mut FlatZincInstanceBuilder,
) -> Result<(), FlatZincError> {
    match var_decl {
        flatzinc::VarDeclItem::IntInRange {
            id,
            lb,
            ub,
            expr,
            annos,
        } => {
            if expr.is_some() {
                warn!("ignoring var decl expression");
            }

            let is_output_variable = annos.iter().any(|ann| ann.id == "output_var");

            let lb = lb.try_into()?;
            let ub = ub.try_into()?;

            instance_builder.add_integer_variable(id.into(), lb, ub, is_output_variable);
        }

        flatzinc::VarDeclItem::ArrayOfInt {
            id,
            annos,
            array_expr,
            ..
        } => {
            let Some(array_expr) = array_expr else {
                // The FlatZinc grammar specifies all variables arrays must come with an
                // expression.
                unreachable!("array of variable without expression")
            };

            let flatzinc::ArrayOfIntExpr::Array(array) = array_expr else {
                // The FlatZinc grammar specifies variable arrays cannot shadow other declarations.
                unreachable!("array of variable that shadows another declaration")
            };

            let array = array
                .into_iter()
                .map(|int_expr| match int_expr {
                    flatzinc::IntExpr::Int(_) => {
                        todo!("constant in variable array declaration")
                    }
                    flatzinc::IntExpr::VarParIdentifier(id) => id.into(),
                })
                .collect::<Box<_>>();

            let possible_output_annotation = annos.iter().find(|ann| ann.id == "output_array");
            let is_output_variable = if let Some(ann) = possible_output_annotation {
                assert_eq!(1, ann.expressions.len());
                let expr = &ann.expressions[0];

                let flatzinc::AnnExpr::Expr(flatzinc::Expr::ArrayOfSet(sets)) = expr else {
                    todo!("unsupported output_array annotation {ann:#?}")
                };

                // Note: To support proper formatting of the output we will need this in the
                // future. For now, we ignore and will not format exactly to specification for
                // all models.
                assert_eq!(1, sets.len(), "only 1d arrays are currently supported");
                let _num_elements = parse_index_set(&sets[0]);

                true
            } else {
                false
            };

            instance_builder.add_integer_variable_array(id.into(), array, is_output_variable);
        }

        unknown => todo!("implement parse_var_decl for {unknown:#?}"),
    }

    Ok(())
}

fn parse_index_set(expr: &flatzinc::SetExpr) -> usize {
    let flatzinc::SetExpr::Set(flatzinc::SetLiteralExpr::IntInRange(
        flatzinc::IntExpr::Int(1),
        flatzinc::IntExpr::Int(num_elements),
    )) = expr
    else {
        unreachable!("index sets must be in the form 1..<int-literal>");
    };

    (*num_elements)
        .try_into()
        .expect("the bound is not a valid usize")
}

fn parse_par_decl(
    par_decl: flatzinc::ParDeclItem,
    instance_builder: &mut FlatZincInstanceBuilder,
) -> Result<(), FlatZincError> {
    match par_decl {
        flatzinc::ParDeclItem::Int { .. } => {
            todo!("implement parse_par_decl for integer parameters")
        }
        flatzinc::ParDeclItem::ArrayOfInt { id, v, .. } => {
            let values = v
                .into_iter()
                .map(|num| num.try_into())
                .collect::<Result<Box<[i32]>, _>>()?;

            instance_builder.add_integer_array_parameter(id.into(), values);
        }
        flatzinc::ParDeclItem::Bool { .. } => {
            todo!("implement parse_par_decl for boolean parameters")
        }
        flatzinc::ParDeclItem::ArrayOfBool { .. } => {
            todo!("implement parse_par_decl for array of boolean parameters")
        }
        flatzinc::ParDeclItem::Float { .. } => {
            todo!("implement parse_par_decl for float parameters")
        }
        flatzinc::ParDeclItem::ArrayOfFloat { .. } => {
            todo!("implement parse_par_decl for array of float parameters")
        }
        flatzinc::ParDeclItem::SetOfInt { .. } => {
            todo!("implement parse_par_decl for set of int parameters")
        }
        flatzinc::ParDeclItem::ArrayOfSet { .. } => {
            todo!("implement parse_par_decl for array of array of set parameters")
        }
    }

    Ok(())
}
