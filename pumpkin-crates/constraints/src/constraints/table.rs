use std::collections::BTreeMap;

use pumpkin_core::constraints::Constraint;
use pumpkin_core::constraints::NegatableConstraint;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagators::nogoods::NogoodPropagator;
use pumpkin_core::state::PropagatorHandle;
use pumpkin_core::state::State;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;

use crate::clause;

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
    propagator_handle: PropagatorHandle<NogoodPropagator>,
) -> impl NegatableConstraint {
    Table {
        xs: xs.into_iter().collect(),
        table,
        constraint_tag,
        propagator_handle,
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
    propagator_handle: PropagatorHandle<NogoodPropagator>,
) -> impl NegatableConstraint {
    NegativeTable {
        xs: xs.into_iter().collect(),
        table,
        constraint_tag,
        propagator_handle,
    }
}

struct Table<Var> {
    xs: Vec<Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
}

impl<Var: IntegerVariable> Table<Var> {
    fn encode(self, state: &mut State, reification_literal: Option<Literal>) {
        // 1. Create a variable `y_i` that selects the row from the table which is chosen.
        let ys: Vec<_> = (0..self.table.len())
            .map(|_| state.new_literal(None))
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
                    let mut predicates = vec![support.get_false_predicate(), condition];

                    // Account for possible reification.
                    // l -> clause
                    predicates.extend(reification_literal.iter().map(|l| l.get_false_predicate()));

                    clause(predicates, self.constraint_tag, self.propagator_handle).post(state);
                }

                // `condition -> (\/ supports)`
                let mut clause = vec![!condition];
                clause.extend(supports.iter().map(|l| l.get_true_predicate()));
                // Account for possible reification.
                clause.extend(reification_literal.iter().map(|l| l.get_false_predicate()));
            }
        }

        // 4. Enforce at least one `y` to be true.
        let constraint = clause(
            ys.into_iter().map(|y| y.get_true_predicate()),
            self.constraint_tag,
            self.propagator_handle,
        );

        if let Some(literal) = reification_literal {
            constraint.implied_by(state, literal);
        } else {
            constraint.post(state);
        }
    }
}

impl<Var: IntegerVariable> Constraint for Table<Var> {
    fn post(self, state: &mut State) {
        self.encode(state, None)
    }

    fn implied_by(self, state: &mut State, reification_literal: Literal) {
        self.encode(state, Some(reification_literal))
    }
}

impl<Var: IntegerVariable + 'static> NegatableConstraint for Table<Var> {
    type NegatedConstraint = NegativeTable<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        let xs = self.xs.clone();
        let table = self.table.clone();
        let constraint_tag = self.constraint_tag;
        let propagator_handle = self.propagator_handle;

        NegativeTable {
            xs,
            table,
            constraint_tag,
            propagator_handle,
        }
    }
}

struct NegativeTable<Var> {
    xs: Vec<Var>,
    table: Vec<Vec<i32>>,
    constraint_tag: ConstraintTag,
    propagator_handle: PropagatorHandle<NogoodPropagator>,
}

impl<Var: IntegerVariable> Constraint for NegativeTable<Var> {
    fn post(self, state: &mut State) {
        for row in self.table {
            let literals: Vec<_> = self
                .xs
                .iter()
                .zip(row)
                .map(|(x, value)| predicate![x != value])
                .collect();

            clause(literals, self.constraint_tag, self.propagator_handle).post(state);
        }
    }

    fn implied_by(self, state: &mut State, reification_literal: Literal) {
        for row in self.table {
            let literals: Vec<_> = self
                .xs
                .iter()
                .zip(row)
                .map(|(x, value)| predicate![x != value])
                .chain(std::iter::once(reification_literal.get_false_predicate()))
                .collect();

            clause(literals, self.constraint_tag, self.propagator_handle).post(state);
        }
    }
}

impl<Var: IntegerVariable + 'static> NegatableConstraint for NegativeTable<Var> {
    type NegatedConstraint = Table<Var>;

    fn negation(&self) -> Self::NegatedConstraint {
        let xs = self.xs.clone();
        let table = self.table.clone();
        let constraint_tag = self.constraint_tag;
        let propagator_handle = self.propagator_handle;

        Table {
            xs,
            table,
            constraint_tag,
            propagator_handle,
        }
    }
}
