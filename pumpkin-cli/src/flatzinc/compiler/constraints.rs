use pumpkin_lib::engine::variables::AffineView;
use pumpkin_lib::engine::variables::Literal;
use pumpkin_lib::engine::variables::DomainId;
use pumpkin_lib::engine::variables::TransformableVariable;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

#[allow(clippy::boxed_local)]
pub(crate) fn int_lin_le_reif(
    _solver: &mut ConstraintSatisfactionSolver,
    _terms: Box<[AffineView<DomainId>]>,
    _rhs: i32,
    _reif: Literal,
) -> bool {
    todo!();
    // solver.int_lin_le_reif(terms.clone(), rhs, reif)
    // && solver.int_lin_le_reif(
    // terms.iter().map(|term| term.scaled(-1)).collect::<Vec<_>>(),
    // -rhs - 1,
    // !reif,
    // )
}

#[allow(clippy::boxed_local)]
pub(crate) fn int_lin_eq_reif(
    _solver: &mut ConstraintSatisfactionSolver,
    _terms: Box<[AffineView<DomainId>]>,
    _rhs: i32,
    _reif: Literal,
) -> bool {
    todo!();
    // negating a boolean domain is not yet supported!
    // solver.int_lin_eq_reif(terms.clone(), rhs, reif) && solver.int_lin_ne_reif(terms, rhs, !reif)
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
    _solver: &mut ConstraintSatisfactionSolver,
    _clause: impl Into<Vec<Literal>>,
    _reif: Literal,
) -> bool {
    // let mut clause = clause.into();

    todo!();

    // \/clause -> r
    // let all_implications = clause
    // .iter()
    // .all(|&literal| solver.add_clause([!literal, reif]).is_ok());
    //
    // r -> \/clause
    // clause.insert(0, !reif);
    //
    // all_implications && solver.add_clause(clause).is_ok()
}

pub(crate) fn int_ne_reif(
    _solver: &mut ConstraintSatisfactionSolver,
    _a: DomainId,
    _b: DomainId,
    _reif: Literal,
) -> bool {
    todo!();
    // solver.int_ne_reif(a, b, reif) && solver.int_eq_reif(a, b, !reif)
}

#[allow(clippy::boxed_local)]
pub(crate) fn int_lin_ne_reif(
    _solver: &mut ConstraintSatisfactionSolver,
    _terms: Box<[AffineView<DomainId>]>,
    _rhs: i32,
    _reif: Literal,
) -> bool {
    todo!();
    // solver.int_lin_ne_reif(terms.clone(), rhs, reif) && solver.int_lin_eq_reif(terms, rhs, !reif)
}

pub(crate) fn bool_lin_le(
    _solver: &mut ConstraintSatisfactionSolver,
    _weights: &[i32],
    _bools: &[Literal],
    _rhs: i32,
) -> bool {
    todo!();
    // let domains = bools
    // .iter()
    // .enumerate()
    // .map(|(index, bool)| {
    // let corresponding_domain_id = solver.create_new_integer_variable(0, 1, None);
    // bool -> [domain = 1]
    // let _ = solver.add_clause([!*bool, predicate![corresponding_domain_id >= 1]]);
    // !bool -> [domain = 0]
    // let _ = solver.add_clause([*bool, predicate![corresponding_domain_id <= 0]]);
    // corresponding_domain_id.scaled(weights[index])
    // })
    // .collect::<Vec<_>>();
    // solver.int_lin_le(domains, rhs)
}

pub(crate) fn bool_lin_eq(
    _solver: &mut ConstraintSatisfactionSolver,
    _weights: &[i32],
    _bools: &[Literal],
    _rhs: DomainId,
) -> bool {
    todo!();
    // let domains = bools
    // .iter()
    // .enumerate()
    // .map(|(index, bool)| {
    // let corresponding_domain_id = solver.create_new_integer_variable(0, 1, None);
    // bool -> [domain = 1]
    // let _ = solver.add_clause([!*bool, predicate![corresponding_domain_id >= 1]]);
    // !bool -> [domain = 0]
    // let _ = solver.add_clause([
    // bool,
    // solver.get_literal(predicate![corresponding_domain_id <= 0]),
    // ]);
    // corresponding_domain_id.scaled(weights[index])
    // })
    // .chain(std::iter::once(rhs.scaled(-1)))
    // .collect::<Vec<_>>();
    // solver.int_lin_eq(domains, 0)
}
