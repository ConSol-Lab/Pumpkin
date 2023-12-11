//! A model for the balanced incomplete block design problem. For a formal definition of the
//! problem, see:
//! - https://en.wikipedia.org/wiki/Block_design#Pairwise_balanced_uniform_designs_(2-designs_or_BIBDs)
//! - https://mathworld.wolfram.com/BlockDesign.html
//!
//! Informally, the `BIBD(v, b, r, k, l)` problem looks for a binary `v * b` matrix such that all rows sum to
//! `r`, all columns sum to `k`, and the dot product between any two distinct rows is at most `l` (any
//! two pairs of rows have at most `l` overlapping 1s in their columns).
//!
//! The parameters are not independent, but satisfy the following conditions:
//! - `bk = vr`
//! - `l(v - 1) = r(k - 1)`
//! Hence, the problem is defined in terms of v, k, and l.

use pumpkin_lib::{
    basic_types::{variables::IntVar, CSPSolverExecutionFlag},
    engine::ConstraintSatisfactionSolver,
    propagators::{IntTimes, IntTimesArgs, LinearLeq, LinearLeqArgs},
};

#[allow(clippy::upper_case_acronyms)]
struct BIBD {
    /// The number of rows in the matrix.
    rows: u32,
    /// The number of columns in the matrix.
    columns: u32,
    /// The sum each row should equal.
    row_sum: u32,
    /// The sum each column should equal.
    column_sum: u32,
    /// The maximum dot product between any distinct pair of rows.
    max_dot_product: u32,
}

impl BIBD {
    fn from_args() -> Option<BIBD> {
        let args = std::env::args()
            .skip(1)
            .map(|arg| arg.parse::<u32>())
            .collect::<Result<Vec<u32>, _>>()
            .ok()?;

        if args.len() != 3 {
            return None;
        }

        let v = args[0];
        let k = args[1];
        let l = args[2];

        let r = l * (v - 1) / (k - 1);
        let b = v * r / k;

        Some(Self {
            rows: v,
            columns: b,
            row_sum: r,
            column_sum: k,
            max_dot_product: l,
        })
    }
}

fn main() {
    env_logger::init();

    let Some(bibd) = BIBD::from_args() else {
        eprintln!("Usage: {} <v> <k> <l>", std::env::args().next().unwrap());
        return;
    };

    println!(
        "bibd: (v = {}, b = {}, r = {}, k = {}, l = {})",
        bibd.rows, bibd.columns, bibd.row_sum, bibd.column_sum, bibd.max_dot_product
    );

    let mut solver = ConstraintSatisfactionSolver::default();

    // Create 0-1 integer variables that make up the matrix.
    let matrix = (0..bibd.rows)
        .map(|_| {
            (0..bibd.columns)
                .map(|_| solver.create_new_integer_variable(0, 1))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    // Enforce the row sum.
    for row in matrix.iter() {
        linear_equal(&mut solver, row, bibd.row_sum as i32);
    }

    // Enforce the column sum.
    for row in transpose(&matrix) {
        linear_equal(&mut solver, &row, bibd.column_sum as i32);
    }

    // Enforce the dot product constraint.
    // pairwise_product[r1][r2][col] = matrix[r1][col] * matrix[r2][col]
    let pairwise_product = (0..bibd.rows)
        .map(|_| {
            (0..bibd.rows)
                .map(|_| {
                    (0..bibd.columns)
                        .map(|_| solver.create_new_integer_variable(0, 1))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    for r1 in 0..bibd.rows as usize {
        for r2 in r1 + 1..bibd.rows as usize {
            for col in 0..bibd.columns as usize {
                solver.add_propagator::<IntTimes<_, _, _>>(IntTimesArgs {
                    a: matrix[r1][col],
                    b: matrix[r2][col],
                    c: pairwise_product[r1][r2][col],
                });
            }
            linear_less_than_equal(
                &mut solver,
                &pairwise_product[r1][r2],
                bibd.max_dot_product as i32,
            );
        }
    }

    match solver.solve(i64::MAX) {
        CSPSolverExecutionFlag::Feasible => {
            let row_separator = format!("{}+", "+---".repeat(bibd.columns as usize));

            for row in matrix.iter() {
                let line = row
                    .iter()
                    .map(|&var| {
                        if solver.get_integer_assignments().get_assigned_value(var) == 1 {
                            "| * ".to_string()
                        } else {
                            "|   ".to_string()
                        }
                    })
                    .collect::<String>();

                println!("{row_separator}\n{line}|");
            }

            println!("{row_separator}");
        }

        CSPSolverExecutionFlag::Infeasible => println!("UNSATISFIABLE"),
        CSPSolverExecutionFlag::Timeout => println!("UNKNOWN"),
    }
}

fn linear_less_than_equal<Var: IntVar + 'static>(
    solver: &mut ConstraintSatisfactionSolver,
    vars: &[Var],
    c: i32,
) {
    solver.add_propagator::<LinearLeq<_>>(LinearLeqArgs::create(vars.iter().cloned().collect(), c));
}

fn transpose<T: Clone, Inner: AsRef<[T]>>(matrix: &[Inner]) -> Vec<Vec<T>> {
    let rows = matrix.len();
    let cols = matrix[0].as_ref().len();

    (0..cols)
        .map(|col| {
            (0..rows)
                .map(|row| matrix[row].as_ref()[col].clone())
                .collect()
        })
        .collect()
}

fn linear_equal<Var: IntVar + 'static>(
    solver: &mut ConstraintSatisfactionSolver,
    row: &[Var],
    rhs: i32,
) {
    linear_less_than_equal(solver, row, rhs);

    let negated = row.iter().map(|var| var.scaled(-1)).collect::<Vec<_>>();
    linear_less_than_equal(solver, &negated, -rhs);
}
