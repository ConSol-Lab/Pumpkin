use pumpkin_lib::basic_types::variables::AffineView;
use pumpkin_lib::basic_types::variables::IntVar;
use pumpkin_lib::basic_types::DomainId;
use pumpkin_lib::basic_types::Literal;
use pumpkin_lib::basic_types::WeightedLiteral;
use pumpkin_lib::constraints::ConstraintsExt;
use pumpkin_lib::encoders::PseudoBooleanConstraintEncoder;
use pumpkin_lib::encoders::PseudoBooleanEncoding;
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
    let negated = terms.iter().map(|var| var.scaled(-1)).collect::<Box<[_]>>();
    int_lin_le_reif(solver, terms, rhs, reif) && int_lin_le_reif(solver, negated, -rhs, !reif)
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
    let terms = weights
        .iter()
        .copied()
        .zip(bools.iter().copied())
        .map(|(weight, literal)| {
            let weight =
                u64::try_from(weight).expect("bool_lin_le with negative weights is not supported");
            WeightedLiteral {
                literal,
                weight,
                bound: None,
            }
        })
        .collect();

    let rhs = u64::try_from(rhs)
        .expect("negative rhs is not supported, since all weights should also be positive");

    let mut encoder = PseudoBooleanConstraintEncoder::new(terms, PseudoBooleanEncoding::GTE);
    encoder.constrain_at_most_k(rhs, solver).is_ok()
}

pub(crate) fn bool_lin_eq(
    solver: &mut ConstraintSatisfactionSolver,
    weights: &[i32],
    bools: &[Literal],
    rhs: i32,
) -> bool {
    if !bool_lin_le(solver, weights, bools, rhs) {
        return false;
    }

    let inverted = bools.iter().map(|&literal| !literal).collect::<Box<_>>();
    bool_lin_le(
        solver,
        weights,
        &inverted,
        weights.iter().sum::<i32>() - rhs,
    )
}
