use std::collections::BTreeMap;

use pumpkin_core::Solver;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;

/// Create the [table](https://sofdem.github.io/gccat/gccat/Cin_relation.html#uid22830) [`NegatableConstraint`].
///
/// A table constraint constrains a tuple of variables to have pre-defined values. For example:
/// ```ignore
/// (x1, x2, x3) in {(1, 3, 5), (3, 1, 4)}
/// ```
/// This has two solutions: either the first tuple of values is assigned to the variables, or the
/// second. The set of value tuples is the 'table'.
///
/// In the XCSP3 specification, this is the "positive table"
/// (<https://www.xcsp.org/specifications/constraints/generic/extension/>).
pub fn table<Var: IntegerVariable + 'static>(
    xs: impl IntoIterator<Item = Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    Table {
        xs: xs.into_iter().collect(),
        table,
        constraint_tag,
    }
}

/// Create the negative [table](https://sofdem.github.io/gccat/gccat/Cin_relation.html#uid22830) [`NegatableConstraint`].
///
/// A negative table is essentially a set of conflicts over the given variables. For example:
/// ```ignore
/// (x1, x2, x3) not in {(1, 3, 5), (3, 1, 4)}
/// ```
/// This prevents any solution where the variables have both the first and the second tuple as
/// values.
///
/// In the XCSP3 specification, this is the "negative table"
/// (<https://www.xcsp.org/specifications/constraints/generic/extension/>).
pub fn negative_table<Var: IntegerVariable + 'static>(
    xs: impl IntoIterator<Item = Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
) -> impl NegatableConstraint {
    NegativeTable {
        xs: xs.into_iter().collect(),
        table,
        constraint_tag,
    }
}

struct Table<Var> {
    xs: Vec<Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable> Table<Var> {
    fn encode(self, solver: &mut Solver, reification_literal: Option<Literal>) {
        // 1. Create a variable `y_i` that selects the row from the table which is chosen.
        let ys: Vec<_> = (0..self.table.len())
            .map(|_| solver.new_literal())
            .collect();

        // 2. Setup the implications between values and `ys`.
        for (col, x_col) in self.xs.iter().enumerate() {
            // A map from domain values to the `ys` variables that support this value.
            let mut values = BTreeMap::new();

            // For every value in this column, aggregate the `ys` that support it.
            for (row, &y_row) in ys.iter().enumerate() {
                let value = self.table[row][col];

                let supports = values.entry(value).or_insert(vec![]);
                supports.push(y_row);
            }

            // For every value in this column, add the clause
            //   `condition <-> (\/ supports)`
            for (value, supports) in values {
                let condition = predicate![x_col == value];

                // For every `support in supports`: `support -> condition`
                for support in supports.iter() {
                    let mut clause = vec![support.get_false_predicate(), condition];

                    // Account for possible reification.
                    // l -> clause
                    clause.extend(reification_literal.iter().map(|l| l.get_false_predicate()));

                    solver.add_clause(clause, self.constraint_tag);
                }

                // `condition -> (\/ supports)`
                let mut clause = vec![!condition];
                clause.extend(supports.iter().map(|l| l.get_true_predicate()));
                // Account for possible reification.
                clause.extend(reification_literal.iter().map(|l| l.get_false_predicate()));

                solver.add_clause(clause, self.constraint_tag);
            }
        }

        // 4. Enforce at least one `y` to be true.
        let poster = solver.add_constraint(crate::constraints::clause(ys, self.constraint_tag));
        if let Some(literal) = reification_literal {
            poster.implied_by(literal);
        } else {
            poster.post();
        }
    }
}

impl<Var: IntegerVariable> Constraint for Table<Var> {
    fn post(self, solver: &mut Solver) {
        self.encode(solver, None)
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        self.encode(solver, Some(reification_literal))
    }
}

impl<Var: IntegerVariable + 'static> NegatableConstraint for Table<Var> {
    type NegatedConstraint = NegativeTable<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        let xs = self.xs.clone();
        let table = self.table.clone();
        let constraint_tag = self.constraint_tag;

        NegativeTable {
            xs,
            table,
            constraint_tag,
        }
    }
}

struct NegativeTable<Var> {
    xs: Vec<Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable> Constraint for NegativeTable<Var> {
    fn post(self, solver: &mut Solver) {
        for row in self.table {
            let clause: Vec<_> = self
                .xs
                .iter()
                .zip(row)
                .map(|(x, value)| predicate![x != value])
                .collect();

            solver.add_clause(clause, self.constraint_tag);
        }
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        for row in self.table {
            let clause: Vec<_> = self
                .xs
                .iter()
                .zip(row)
                .map(|(x, value)| predicate![x != value])
                .chain(std::iter::once(reification_literal.get_false_predicate()))
                .collect();

            solver.add_clause(clause, self.constraint_tag);
        }
    }
}

impl<Var: IntegerVariable + 'static> NegatableConstraint for NegativeTable<Var> {
    type NegatedConstraint = Table<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        let xs = self.xs.clone();
        let table = self.table.clone();
        let constraint_tag = self.constraint_tag;

        Table {
            xs,
            table,
            constraint_tag,
        }
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::results::CSPSolverExecutionFlag;

    use super::*;

    #[test]
    fn eliminating_all_supporting_rows_prunes_the_value() {
        let mut solver = Solver::default();

        let constraint_tag = solver.new_constraint_tag();
        let x1 = solver.new_named_bounded_integer(1, 2, "x1");
        let x2 = solver.new_named_sparse_integer([10, 20, 30], "x2");

        // Rows 2 and 3 are the only support for `x2 == 30`, and are exactly the rows that support
        // `x1 == 2`. Fixing `x1 == 1` should eliminate both of them, which should in turn prune
        // `30` from the domain of `x2`.
        let rows = vec![vec![1, 10], vec![1, 20], vec![2, 30], vec![2, 30]];
        table(vec![x1, x2], rows, constraint_tag).post(&mut solver);

        let result = solver.propagate_to_fixpoint();
        assert_eq!(result, CSPSolverExecutionFlag::Feasible);

        solver.add_clause([predicate![x1 == 1]], constraint_tag);

        let result = solver.propagate_to_fixpoint();
        assert_eq!(result, CSPSolverExecutionFlag::Feasible);

        assert_eq!(solver.upper_bound(&x2), 20);
    }
}
