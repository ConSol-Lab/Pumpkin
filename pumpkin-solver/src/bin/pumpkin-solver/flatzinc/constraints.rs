use fzn_rs::ast::RangeList;
use fzn_rs::ArrayExpr;
use fzn_rs::VariableExpr;

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) enum Constraints {
    SetIn(VariableExpr<i32>, RangeList<i32>),

    #[args]
    SetInReif(SetInReifArgs),

    #[args]
    ArrayIntMinimum(ArrayExtremum),
    #[args]
    ArrayIntMaximum(ArrayExtremum),

    #[args]
    IntMax(TernaryIntArgs),
    #[args]
    IntMin(TernaryIntArgs),

    #[args]
    ArrayIntElement(IntElementArgs),
    #[args]
    ArrayVarIntElement(IntElementArgs),

    #[args]
    IntEqImp(ReifiedBinary),
    #[args]
    IntGeImp(ReifiedBinary),
    #[args]
    IntGtImp(ReifiedBinary),
    #[args]
    IntLeImp(ReifiedBinary),
    #[args]
    IntLtImp(ReifiedBinary),
    #[args]
    IntNeImp(ReifiedBinary),

    #[args]
    IntLinLe(Linear),
    #[args]
    IntLinEq(Linear),
    #[args]
    IntLinNe(Linear),

    #[args]
    IntLinLeReif(ReifiedLinear),
    #[args]
    IntLinEqReif(ReifiedLinear),
    #[args]
    IntLinNeReif(ReifiedLinear),

    #[args]
    IntLinLeImp(ReifiedLinear),
    #[args]
    IntLinEqImp(ReifiedLinear),
    #[args]
    IntLinNeImp(ReifiedLinear),

    #[args]
    IntEq(Binary),
    #[args]
    IntNe(Binary),
    #[args]
    IntLe(Binary),
    #[args]
    IntLt(Binary),
    #[args]
    IntAbs(Binary),

    #[args]
    IntEqReif(ReifiedBinary),
    #[args]
    IntNeReif(ReifiedBinary),
    #[args]
    IntLeReif(ReifiedBinary),
    #[args]
    IntLtReif(ReifiedBinary),

    #[args]
    IntTimes(TernaryIntArgs),
    #[args]
    IntPlus(TernaryIntArgs),
    #[args]
    IntDiv(TernaryIntArgs),

    #[name("pumpkin_all_different")]
    AllDifferent(ArrayExpr<VariableExpr<i32>>),

    #[args]
    #[name("pumpkin_table_int")]
    Table(TableInt),

    #[args]
    #[name("pumpkin_table_int_reif")]
    TableReif(TableIntReif),

    #[args]
    ArrayBoolAnd(ArrayBoolArgs),

    #[args]
    ArrayBoolOr(ArrayBoolArgs),

    #[args]
    BoolClause(BoolClauseArgs),

    #[args]
    BoolEq(BinaryBool),

    #[args]
    BoolNot(BinaryBool),

    #[args]
    #[name("pumpkin_bool_xor")]
    BoolXor(BinaryBool),

    #[args]
    #[name("pumpkin_bool_xor_reif")]
    BoolXorReif(BinaryBoolReif),

    #[args]
    #[name("bool2int")]
    BoolToInt(BoolToIntArgs),

    #[args]
    BoolLinEq(BoolLinEqArgs),

    #[args]
    BoolLinLe(BoolLinLeArgs),

    #[args]
    BoolAnd(BinaryBoolReif),
    #[args]
    BoolEqReif(BinaryBoolReif),

    #[args]
    ArrayBoolElement(BoolElementArgs),
    #[args]
    ArrayVarBoolElement(BoolElementArgs),

    #[args]
    #[name("pumpkin_cumulative")]
    Cumulative(CumulativeArgs),
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct CumulativeArgs {
    pub(crate) start_times: ArrayExpr<VariableExpr<i32>>,
    pub(crate) durations: ArrayExpr<i32>,
    pub(crate) resource_requirements: ArrayExpr<i32>,
    pub(crate) resource_capacity: i32,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct SetInReifArgs {
    pub(crate) variable: VariableExpr<i32>,
    pub(crate) set: RangeList<i32>,
    pub(crate) reification: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolClauseArgs {
    pub(crate) clause_1: ArrayExpr<VariableExpr<bool>>,
    pub(crate) clause_2: ArrayExpr<VariableExpr<bool>>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolLinEqArgs {
    pub(crate) weights: ArrayExpr<i32>,
    pub(crate) variables: ArrayExpr<VariableExpr<bool>>,
    pub(crate) sum: VariableExpr<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolLinLeArgs {
    pub(crate) weights: ArrayExpr<i32>,
    pub(crate) variables: ArrayExpr<VariableExpr<bool>>,
    pub(crate) bound: i32,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolToIntArgs {
    pub(crate) boolean: VariableExpr<bool>,
    pub(crate) integer: VariableExpr<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ArrayBoolArgs {
    pub(crate) booleans: ArrayExpr<VariableExpr<bool>>,
    pub(crate) reification: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BinaryBool {
    pub(crate) a: VariableExpr<bool>,
    pub(crate) b: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BinaryBoolReif {
    pub(crate) a: VariableExpr<bool>,
    pub(crate) b: VariableExpr<bool>,
    pub(crate) reification: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct TableInt {
    pub(crate) variables: ArrayExpr<VariableExpr<i32>>,
    pub(crate) table: ArrayExpr<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct TableIntReif {
    pub(crate) variables: ArrayExpr<VariableExpr<i32>>,
    pub(crate) table: ArrayExpr<i32>,
    pub(crate) reification: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ArrayExtremum {
    pub(crate) extremum: VariableExpr<i32>,
    pub(crate) array: ArrayExpr<VariableExpr<i32>>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct TernaryIntArgs {
    pub(crate) a: VariableExpr<i32>,
    pub(crate) b: VariableExpr<i32>,
    pub(crate) c: VariableExpr<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct IntElementArgs {
    pub(crate) index: VariableExpr<i32>,
    pub(crate) array: ArrayExpr<VariableExpr<i32>>,
    pub(crate) rhs: VariableExpr<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolElementArgs {
    pub(crate) index: VariableExpr<i32>,
    pub(crate) array: ArrayExpr<VariableExpr<bool>>,
    pub(crate) rhs: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct Binary(pub(crate) VariableExpr<i32>, pub(crate) VariableExpr<i32>);

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ReifiedBinary {
    pub(crate) a: VariableExpr<i32>,
    pub(crate) b: VariableExpr<i32>,
    pub(crate) reification: VariableExpr<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct Linear {
    pub(crate) weights: ArrayExpr<i32>,
    pub(crate) variables: ArrayExpr<VariableExpr<i32>>,
    pub(crate) rhs: i32,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ReifiedLinear {
    pub(crate) weights: ArrayExpr<i32>,
    pub(crate) variables: ArrayExpr<VariableExpr<i32>>,
    pub(crate) rhs: i32,
    pub(crate) reification: VariableExpr<bool>,
}
