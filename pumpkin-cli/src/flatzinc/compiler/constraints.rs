use pumpkin_lib::constraints::ConstraintsExt;
use pumpkin_lib::engine::variables::AffineView;
use pumpkin_lib::engine::variables::DomainId;
use pumpkin_lib::engine::variables::Literal;
use pumpkin_lib::engine::variables::TransformableVariable;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

pub(crate) fn int_lin_le_reif(
    solver: &mut ConstraintSatisfactionSolver,
    terms: Box<[AffineView<DomainId>]>,
    rhs: i32,
    reif: Literal,
) -> bool {
    solver.int_lin_le_reif(terms.clone(), rhs, reif)
        && solver.int_lin_le_reif(
            terms.iter().map(|term| term.scaled(-1)).collect::<Vec<_>>(),
            -rhs - 1,
            !reif,
        )
}

pub(crate) fn int_lin_eq_reif(
    solver: &mut ConstraintSatisfactionSolver,
    terms: Box<[AffineView<DomainId>]>,
    rhs: i32,
    reif: Literal,
) -> bool {
    solver.int_lin_eq_reif(terms.clone(), rhs, reif) && solver.int_lin_ne_reif(terms, rhs, !reif)
}

pub(crate) fn int_le_reif(
    solver: &mut ConstraintSatisfactionSolver,
    a: DomainId,
    b: DomainId,
    reif: Literal,
) -> bool {
    int_lin_le_reif(solver, vec![a.scaled(1), b.scaled(-1)].into(), 0, reif)
}

pub(crate) fn int_lt_reif(
    solver: &mut ConstraintSatisfactionSolver,
    a: DomainId,
    b: DomainId,
    reif: Literal,
) -> bool {
    int_lin_le_reif(solver, vec![a.scaled(1), b.scaled(-1)].into(), -1, reif)
}

pub(crate) fn int_eq_reif(
    solver: &mut ConstraintSatisfactionSolver,
    a: DomainId,
    b: DomainId,
    reif: Literal,
) -> bool {
    int_lin_eq_reif(solver, vec![a.scaled(1), b.scaled(-1)].into(), 0, reif)
}

pub(crate) fn array_bool_or(
    solver: &mut ConstraintSatisfactionSolver,
    clause: impl Into<Vec<Literal>>,
    reif: Literal,
) -> bool {
    let mut clause = clause.into();

    // \/clause -> r
    let all_implications = clause
        .iter()
        .all(|&literal| solver.add_permanent_clause(vec![!literal, reif]).is_ok());

    // r -> \/clause
    clause.insert(0, !reif);

    all_implications && solver.add_permanent_clause(clause).is_ok()
}

pub(crate) fn int_ne_reif(
    solver: &mut ConstraintSatisfactionSolver,
    a: DomainId,
    b: DomainId,
    reif: Literal,
) -> bool {
    solver.int_ne_reif(a, b, reif) && solver.int_eq_reif(a, b, !reif)
}

pub(crate) fn int_lin_ne_reif(
    solver: &mut ConstraintSatisfactionSolver,
    terms: Box<[AffineView<DomainId>]>,
    rhs: i32,
    reif: Literal,
) -> bool {
    solver.int_lin_ne_reif(terms.clone(), rhs, reif) && solver.int_lin_eq_reif(terms, rhs, !reif)
}

pub(crate) fn bool_lin_le(
    solver: &mut ConstraintSatisfactionSolver,
    weights: &[i32],
    bools: &[Literal],
    rhs: i32,
) -> bool {
    let domains = bools
        .iter()
        .enumerate()
        .map(|(index, bool)| {
            let corresponding_domain_id = solver.create_new_integer_variable(0, 1);
            // bool -> [domain = 1]
            let _ = solver.add_permanent_clause(vec![
                !*bool,
                solver.get_lower_bound_literal(corresponding_domain_id, 1),
            ]);
            // !bool -> [domain = 0]
            let _ = solver.add_permanent_clause(vec![
                *bool,
                solver.get_upper_bound_literal(corresponding_domain_id, 0),
            ]);
            corresponding_domain_id.scaled(weights[index])
        })
        .collect::<Vec<_>>();
    solver.int_lin_le(domains, rhs)
}

pub(crate) fn bool_lin_eq(
    solver: &mut ConstraintSatisfactionSolver,
    weights: &[i32],
    bools: &[Literal],
    rhs: DomainId,
) -> bool {
    let domains = bools
        .iter()
        .enumerate()
        .map(|(index, bool)| {
            let corresponding_domain_id = solver.create_new_integer_variable(0, 1);
            // bool -> [domain = 1]
            let _ = solver.add_permanent_clause(vec![
                !*bool,
                solver.get_lower_bound_literal(corresponding_domain_id, 1),
            ]);
            // !bool -> [domain = 0]
            let _ = solver.add_permanent_clause(vec![
                *bool,
                solver.get_upper_bound_literal(corresponding_domain_id, 0),
            ]);
            corresponding_domain_id.scaled(weights[index])
        })
        .chain(std::iter::once(rhs.scaled(-1)))
        .collect::<Vec<_>>();
    solver.int_lin_eq(domains, 0)
}
