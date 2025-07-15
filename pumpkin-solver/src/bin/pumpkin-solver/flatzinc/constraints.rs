use fzn_rs::{ast::RangeList, VariableArgument};

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) enum Constraints {
    SetIn(VariableArgument<i32>, RangeList<i32>),

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
    AllDifferent(Vec<VariableArgument<i32>>),

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
    pub(crate) start_times: Vec<VariableArgument<i32>>,
    pub(crate) durations: Vec<i32>,
    pub(crate) resource_requirements: Vec<i32>,
    pub(crate) resource_capacity: i32,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct SetInReifArgs {
    pub(crate) variable: VariableArgument<i32>,
    pub(crate) set: RangeList<i32>,
    pub(crate) reification: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolClauseArgs {
    pub(crate) clause_1: Vec<VariableArgument<bool>>,
    pub(crate) clause_2: Vec<VariableArgument<bool>>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolLinEqArgs {
    pub(crate) weights: Vec<i32>,
    pub(crate) variables: Vec<VariableArgument<bool>>,
    pub(crate) sum: VariableArgument<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolLinLeArgs {
    pub(crate) weights: Vec<i32>,
    pub(crate) variables: Vec<VariableArgument<bool>>,
    pub(crate) bound: i32,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolToIntArgs {
    pub(crate) integer: VariableArgument<i32>,
    pub(crate) boolean: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ArrayBoolArgs {
    pub(crate) booleans: Vec<VariableArgument<bool>>,
    pub(crate) reification: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BinaryBool {
    pub(crate) a: VariableArgument<bool>,
    pub(crate) b: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BinaryBoolReif {
    pub(crate) a: VariableArgument<bool>,
    pub(crate) b: VariableArgument<bool>,
    pub(crate) reification: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct TableInt {
    pub(crate) variables: Vec<VariableArgument<i32>>,
    pub(crate) table: Vec<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct TableIntReif {
    pub(crate) variables: Vec<VariableArgument<i32>>,
    pub(crate) table: Vec<i32>,
    pub(crate) reification: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ArrayExtremum {
    pub(crate) array: Vec<VariableArgument<i32>>,
    pub(crate) extremum: VariableArgument<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct TernaryIntArgs {
    pub(crate) a: VariableArgument<i32>,
    pub(crate) b: VariableArgument<i32>,
    pub(crate) c: VariableArgument<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct IntElementArgs {
    pub(crate) index: VariableArgument<i32>,
    pub(crate) array: Vec<VariableArgument<i32>>,
    pub(crate) rhs: VariableArgument<i32>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct BoolElementArgs {
    pub(crate) index: VariableArgument<i32>,
    pub(crate) array: Vec<VariableArgument<bool>>,
    pub(crate) rhs: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct Binary(
    pub(crate) VariableArgument<i32>,
    pub(crate) VariableArgument<i32>,
);

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ReifiedBinary {
    pub(crate) a: VariableArgument<i32>,
    pub(crate) b: VariableArgument<i32>,
    pub(crate) reification: VariableArgument<bool>,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct Linear {
    pub(crate) weights: Vec<i32>,
    pub(crate) variables: Vec<VariableArgument<i32>>,
    pub(crate) rhs: i32,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) struct ReifiedLinear {
    pub(crate) weights: Vec<i32>,
    pub(crate) variables: Vec<VariableArgument<i32>>,
    pub(crate) rhs: i32,
    pub(crate) reification: VariableArgument<bool>,
}
