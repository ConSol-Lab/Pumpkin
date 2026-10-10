use std::collections::BTreeMap;
use std::num::NonZero;

use pumpkin_core::Solver;
use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::HeadField;
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

/// The name of the selector of the row in the decomposition of the table constraint.
pub fn table_selector_name(constraint_tag: ConstraintTag, row: usize) -> String {
    format!("_t{}_{row}", NonZero::<u32>::from(constraint_tag))
}

struct Table<Var> {
    xs: Vec<Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
}

impl<Var: IntegerVariable> Table<Var> {
    /// The clauses of the decomposition, in the order fzn-croc-check numbers them
    /// (`decompose_table` in `theories/proof_system/concrete/decompositions/table.v`): for each
    /// column, and each value of that column in ascending order, `y_i -> x_col == value` for each
    /// row `i` with that value, in row order, then `x_col == value -> \/ y_i` over those rows;
    /// then the clause that some `y_i` holds; with a reification literal `l`, each of these with
    /// `!l` added, followed, when `fully`, by the clause `\/_col x_col != t_i[col] \/ l` for each
    /// row `i`.
    fn clauses(
        &self,
        ys: &[Literal],
        reification_literal: Option<Literal>,
        fully: bool,
    ) -> Vec<Vec<Predicate>> {
        let mut clauses = vec![];

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

                    clauses.push(clause);
                }

                // `condition -> (\/ supports)`
                let mut clause = vec![!condition];
                clause.extend(supports.iter().map(|l| l.get_true_predicate()));
                // Account for possible reification.
                clause.extend(reification_literal.iter().map(|l| l.get_false_predicate()));

                clauses.push(clause);
            }
        }

        // Enforce at least one `y` to be true.
        let mut clause: Vec<Predicate> = ys.iter().map(|y| y.get_true_predicate()).collect();
        clause.extend(reification_literal.iter().map(|l| l.get_false_predicate()));
        clauses.push(clause);

        // With the reification literal false, no row is taken.
        if fully && let Some(literal) = reification_literal {
            for row in &self.table {
                let mut clause: Vec<Predicate> = self
                    .xs
                    .iter()
                    .zip(row)
                    .map(|(x, &value)| predicate![x != value])
                    .collect();
                clause.push(literal.get_true_predicate());
                clauses.push(clause);
            }
        }

        clauses
    }

    /// Post the decomposition: a fresh literal `y_i` that selects row `i`, named by
    /// [`table_selector_name`] under proof logging, and the clauses of [`Table::clauses`]. Under
    /// proof logging in the block format the decomposition is stated first, as
    /// `k <first> table c:<table> <selectors...>`, and each clause, and then the bounds
    /// of the selectors, take the identifiers from `<first>` on, in order.
    fn encode(self, solver: &mut Solver, reification_literal: Option<Literal>, fully: bool) {
        let logging = solver.is_logging_proof();
        let first = if logging {
            let tag = solver.new_constraint_tag();
            solver
                .take_proof_identifier(tag)
                .map(|identifier| (tag, identifier))
        } else {
            None
        };

        // 1. Create a variable `y_i` that selects the row from the table which is chosen. Under
        // proof logging it is named, so that a nogood over it can be written.
        let ys: Vec<_> = (0..self.table.len())
            .map(|row| {
                if logging {
                    solver.new_named_literal(table_selector_name(self.constraint_tag, row))
                } else {
                    solver.new_literal()
                }
            })
            .collect();

        let clauses = self.clauses(&ys, reification_literal, fully);

        let Some((first_tag, identifier)) = first else {
            for clause in clauses {
                solver.add_clause(clause, self.constraint_tag);
            }
            return;
        };

        let mut line = format!(
            "k {identifier} table c:{}",
            NonZero::<u32>::from(self.constraint_tag)
        );
        for row in 0..ys.len() {
            line.push(' ');
            line.push_str(&table_selector_name(self.constraint_tag, row));
        }
        let _ = solver.log_proof_head_line(&[HeadField::Text(line)]);

        // 2. Each clause is a part of the decomposition of its own, cited under the rule
        // `clause`, and the bounds of the selectors are the last part. Every part takes its
        // identifier before any clause is added, since adding one can derive a nogood, which
        // takes the next identifier.
        let mut tags = vec![first_tag];
        for _ in 1..clauses.len() {
            let tag = solver.new_constraint_tag();
            let _ = solver.take_proof_identifier(tag);
            tags.push(tag);
        }
        let bounds_tag = solver.new_constraint_tag();
        let _ = solver.take_proof_identifier(bounds_tag);
        for y in &ys {
            solver.name_selector_bounds(*y.get_integer_variable().inner(), bounds_tag);
        }

        for (clause, tag) in clauses.into_iter().zip(tags) {
            solver.name_rule_of_constraint(tag, "clause");
            solver.add_clause(clause, tag);
        }
    }
}

impl<Var: IntegerVariable> Constraint for Table<Var> {
    fn post(self, solver: &mut Solver) {
        self.encode(solver, None, false)
    }

    fn implied_by(self, solver: &mut Solver, reification_literal: Literal) {
        self.encode(solver, Some(reification_literal), false)
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

    /// Both directions in one decomposition: the clauses of the table implied by the literal,
    /// and, for each row, the clause that the variables differ from it or the literal holds,
    /// which are the clauses of the negative table implied by the negation of the literal.
    fn reify(self, solver: &mut Solver, reification_literal: Literal)
    where
        Self: Sized,
    {
        self.encode(solver, Some(reification_literal), true)
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
