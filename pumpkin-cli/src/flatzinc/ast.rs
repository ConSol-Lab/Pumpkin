use super::FlatZincError;

pub(crate) struct FlatZincAst {
    pub(crate) parameter_decls: Vec<flatzinc::ParDeclItem>,
    pub(crate) single_variables: Vec<SingleVarDecl>,
    pub(crate) variable_arrays: Vec<VarArrayDecl>,
    pub(crate) constraint_decls: Vec<flatzinc::ConstraintItem>,
    pub(crate) solve_item: flatzinc::SolveItem,
}

impl FlatZincAst {
    pub(crate) fn builder() -> FlatZincAstBuilder {
        FlatZincAstBuilder {
            parameter_decls: vec![],
            single_variables: vec![],
            variable_arrays: vec![],
            constraint_decls: vec![],
            solve_item: None,
        }
    }
}

pub(crate) struct FlatZincAstBuilder {
    parameter_decls: Vec<flatzinc::ParDeclItem>,
    single_variables: Vec<SingleVarDecl>,
    variable_arrays: Vec<VarArrayDecl>,
    constraint_decls: Vec<flatzinc::ConstraintItem>,
    solve_item: Option<flatzinc::SolveItem>,
}

impl FlatZincAstBuilder {
    pub(crate) fn add_parameter_decl(&mut self, parameter_decl: flatzinc::ParDeclItem) {
        self.parameter_decls.push(parameter_decl);
    }

    pub(crate) fn add_variable_decl(&mut self, variable_decl: SingleVarDecl) {
        self.single_variables.push(variable_decl);
    }

    pub(crate) fn add_variable_array(&mut self, array_decl: VarArrayDecl) {
        self.variable_arrays.push(array_decl);
    }

    pub(crate) fn add_constraint(&mut self, constraint: flatzinc::ConstraintItem) {
        self.constraint_decls.push(constraint);
    }

    pub(crate) fn set_solve_item(&mut self, solve_item: flatzinc::SolveItem) {
        let _ = self.solve_item.insert(solve_item);
    }

    pub(crate) fn build(self) -> Result<FlatZincAst, FlatZincError> {
        let FlatZincAstBuilder {
            parameter_decls,
            single_variables,
            variable_arrays,
            constraint_decls,
            solve_item,
        } = self;

        Ok(FlatZincAst {
            parameter_decls,
            single_variables,
            variable_arrays,
            constraint_decls,
            solve_item: solve_item.ok_or(FlatZincError::MissingSolveItem)?,
        })
    }
}

pub(crate) enum SingleVarDecl {
    Bool {
        id: String,
        expr: Option<flatzinc::BoolExpr>,
        annos: flatzinc::expressions::Annotations,
    },

    IntInRange {
        id: String,
        lb: i128,
        ub: i128,
        expr: Option<flatzinc::IntExpr>,
        annos: flatzinc::expressions::Annotations,
    },

    IntInSet {
        id: String,
        set: Vec<i128>,
        #[allow(dead_code)]
        expr: Option<flatzinc::IntExpr>,
        annos: flatzinc::expressions::Annotations,
    },
}

pub(crate) enum VarArrayDecl {
    Bool {
        #[allow(dead_code)]
        ix: flatzinc::IndexSet,
        id: String,
        annos: Vec<flatzinc::Annotation>,
        array_expr: Option<flatzinc::ArrayOfBoolExpr>,
    },
    Int {
        #[allow(dead_code)]
        ix: flatzinc::IndexSet,
        id: String,
        annos: Vec<flatzinc::Annotation>,
        array_expr: Option<flatzinc::ArrayOfIntExpr>,
    },
}
