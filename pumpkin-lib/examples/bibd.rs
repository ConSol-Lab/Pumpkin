//! A model for the BIBD problem. See the following:
//! http://mathworld.wolfram.com/BlockDesign.html
//! http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob028/spec.html
//!
//! The BIBD(v, b, r, k, l) problem is the following:
//! Find a binary matrix of `v` rows and `b` columns, such that each row sums to `r`, each column
//! sums to `k`, and the dot product between any pair of distinct rows is `l`.

use pumpkin_lib::{
    basic_types::DomainId,
    engine::ConstraintSatisfactionSolver,
    propagators::{IntTimes, IntTimesArgs},
};

struct BIBD {
    v: u32,
    b: u32,
    r: u32,
    k: u32,
    l: u32,
}

impl BIBD {
    fn from_args() -> Option<BIBD> {
        let args = std::env::args()
            .skip(1)
            .map(|arg| arg.parse::<u32>())
            .collect::<Result<Vec<u32>, _>>()
            .ok()?;

        if args.len() != 5 {
            return None;
        }

        Some(Self {
            v: args[0],
            b: args[1],
            r: args[2],
            k: args[3],
            l: args[4],
        })
    }
}

fn main() {
    let Some(bibd) = BIBD::from_args() else {
        eprintln!("Usage: {} <v> <b> <r> <k> <l>", std::env::args().nth(0).unwrap());
        return;
    };

    let mut solver = ConstraintSatisfactionSolver::default();
    let matrix = (0..bibd.v)
        .map(|_| {
            (0..bibd.b)
                .map(|_| solver.create_new_integer_variable(0, 1))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let pairwise_product = (0..bibd.v)
        .map(|_| {
            (0..bibd.b)
                .map(|_| {
                    (0..bibd.b)
                        .map(|_| solver.create_new_integer_variable(0, 1))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    for row in matrix.iter() {
        linear_eq(&mut solver, row, bibd.r);
    }

    for row in transpose(&matrix) {
        linear_eq(&mut solver, &row, bibd.k);
    }

    for r1 in 0..bibd.v as usize {
        for r2 in r1 + 1..bibd.v as usize {
            for col in 0..bibd.b as usize {
                solver.add_propagator::<IntTimes<_, _, _>>(IntTimesArgs {
                    a: matrix[r1][col],
                    b: matrix[r2][col],
                    c: pairwise_product[r1][r2][col],
                });
            }
            // int_linear(m[i][j], IRT_EQ, l);
        }
    }
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

fn linear_eq(_solver: &mut ConstraintSatisfactionSolver, _row: &[DomainId], _rhs: u32) {
    todo!()
}
