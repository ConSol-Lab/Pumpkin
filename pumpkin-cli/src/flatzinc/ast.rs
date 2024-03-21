use flatzinc::AnnExpr;
use flatzinc::Annotation;
use log::warn;
use pumpkin_lib::branching::AntiFirstFail;
use pumpkin_lib::branching::DynamicValueSelector;
use pumpkin_lib::branching::DynamicVariableSelector;
use pumpkin_lib::branching::FirstFail;
use pumpkin_lib::branching::InDomainInterval;
use pumpkin_lib::branching::InDomainMax;
use pumpkin_lib::branching::InDomainMedian;
use pumpkin_lib::branching::InDomainMiddle;
use pumpkin_lib::branching::InDomainMin;
use pumpkin_lib::branching::InDomainRandom;
use pumpkin_lib::branching::InDomainSplit;
use pumpkin_lib::branching::InDomainSplitRandom;
use pumpkin_lib::branching::InputOrder;
use pumpkin_lib::branching::Largest;
use pumpkin_lib::branching::MaxRegret;
use pumpkin_lib::branching::OutDomainMax;
use pumpkin_lib::branching::OutDomainMedian;
use pumpkin_lib::branching::OutDomainMin;
use pumpkin_lib::branching::OutDomainRandom;
use pumpkin_lib::branching::ReverseInDomainSplit;
use pumpkin_lib::branching::Smallest;
use pumpkin_lib::engine::variables::DomainId;
use pumpkin_lib::engine::variables::PropositionalVariable;
use pumpkin_lib::pumpkin_assert_eq_simple;
use pumpkin_lib::pumpkin_assert_simple;

use super::error::FlatZincError;
pub(crate) enum VariableSelectionStrategy {
    AntiFirstFail,
    DomWDeg,
    FirstFail,
    Impact,
    InputOrder,
    Largest,
    MaxRegret,
    MostConstrained,
    Occurrence,
    Smallest,
}

impl VariableSelectionStrategy {
    pub(crate) fn create_from_propositional_variables(
        &self,
        propositional_variables: &[PropositionalVariable],
    ) -> DynamicVariableSelector<PropositionalVariable> {
        DynamicVariableSelector::new(match self {
            VariableSelectionStrategy::AntiFirstFail => {
                warn!("AntiFirstFail does not make sense for propositional variables, defaulting to input order...");
                Box::new(InputOrder::new(propositional_variables))
            }
            VariableSelectionStrategy::DomWDeg => todo!("DomWDeg is not yet implemented"),
            VariableSelectionStrategy::FirstFail => {
                warn!("FirstFail does not make sense for propositional variables, defaulting to input order...");
                Box::new(InputOrder::new(propositional_variables))
            }
            VariableSelectionStrategy::Impact => todo!("Impact is not yet implemented"),
            VariableSelectionStrategy::InputOrder => {
                Box::new(InputOrder::new(propositional_variables))
            }
            VariableSelectionStrategy::Largest => {
                warn!("Largest does not make sense for propositional variables, defaulting to input order...");
                Box::new(InputOrder::new(propositional_variables))
            }
            VariableSelectionStrategy::MaxRegret => {
                warn!("MaxRegret does not make sense for propositional variables, defaulting to input order...");
                Box::new(InputOrder::new(propositional_variables))
            }
            VariableSelectionStrategy::MostConstrained => {
                todo!("MostConstrained is not yet implemented")
            }
            VariableSelectionStrategy::Occurrence => todo!("Occurrence is not yet implemented"),
            VariableSelectionStrategy::Smallest => {
                warn!("Smallest does not make sense for propositional variables, defaulting to input order...");
                Box::new(InputOrder::new(propositional_variables))
            }
        })
    }

    pub(crate) fn create_from_domains(
        &self,
        variables: &[DomainId],
    ) -> DynamicVariableSelector<DomainId> {
        DynamicVariableSelector::new(match self {
            VariableSelectionStrategy::AntiFirstFail => Box::new(AntiFirstFail::new(variables)),
            VariableSelectionStrategy::DomWDeg => todo!("DomWDeg is not yet implemented"),
            VariableSelectionStrategy::FirstFail => Box::new(FirstFail::new(variables)),
            VariableSelectionStrategy::Impact => todo!("Impact is not yet implemented"),
            VariableSelectionStrategy::InputOrder => Box::new(InputOrder::new(variables)),
            VariableSelectionStrategy::Largest => Box::new(Largest::new(variables)),
            VariableSelectionStrategy::MaxRegret => Box::new(MaxRegret::new(variables)),
            VariableSelectionStrategy::MostConstrained => {
                todo!("MostConstrained is not yet implemented")
            }
            VariableSelectionStrategy::Occurrence => todo!("Occurrence is not yet implemented"),
            VariableSelectionStrategy::Smallest => Box::new(Smallest::new(variables)),
        })
    }
}

pub(crate) enum ValueSelectionStrategy {
    InDomain,
    InDomainInterval,
    InDomainMax,
    InDomainMedian,
    InDomainMiddle,
    InDomainMin,
    InDomainRandom,
    InDomainReverseSplit,
    InDomainSplit,
    InDomainSplitRandom,
    OutDomainMax,
    OutDomainMedian,
    OutDomainMin,
    OutDomainRandom,
}

impl ValueSelectionStrategy {
    pub(crate) fn create_for_propositional_variables(
        &self,
    ) -> DynamicValueSelector<PropositionalVariable> {
        DynamicValueSelector::new(match self {
            ValueSelectionStrategy::InDomain
            | ValueSelectionStrategy::InDomainInterval
            | ValueSelectionStrategy::InDomainMin
            | ValueSelectionStrategy::InDomainSplit
            | ValueSelectionStrategy::OutDomainMax => Box::new(InDomainMin),
            ValueSelectionStrategy::InDomainMax
            | ValueSelectionStrategy::InDomainReverseSplit
            | ValueSelectionStrategy::OutDomainMin => Box::new(InDomainMax),
            ValueSelectionStrategy::InDomainMedian => {
                warn!("InDomainMedian does not make sense for propositional variables, defaulting to InDomainMin...");
                Box::new(InDomainMin)
            }
            ValueSelectionStrategy::InDomainMiddle => {
                warn!("InDomainMiddle does not make sense for propositional variables, defaulting to InDomainMin...");
                Box::new(InDomainMin)
            }
            ValueSelectionStrategy::InDomainRandom
            | ValueSelectionStrategy::InDomainSplitRandom
            | ValueSelectionStrategy::OutDomainRandom => Box::new(InDomainRandom),
            ValueSelectionStrategy::OutDomainMedian => {
                warn!("OutDomainMedian does not make sense for propositional variables, defaulting to InDomainMin...");
                Box::new(InDomainMin)
            }
        })
    }

    pub(crate) fn create_for_domains(&self) -> DynamicValueSelector<DomainId> {
        DynamicValueSelector::new(match self {
            ValueSelectionStrategy::InDomain => Box::new(InDomainMin),
            ValueSelectionStrategy::InDomainInterval => Box::new(InDomainInterval),
            ValueSelectionStrategy::InDomainMax => Box::new(InDomainMax),
            ValueSelectionStrategy::InDomainMedian => Box::new(InDomainMedian),
            ValueSelectionStrategy::InDomainMiddle => Box::new(InDomainMiddle),
            ValueSelectionStrategy::InDomainMin => Box::new(InDomainMin),
            ValueSelectionStrategy::InDomainRandom => Box::new(InDomainRandom),
            ValueSelectionStrategy::InDomainReverseSplit => Box::new(ReverseInDomainSplit),
            ValueSelectionStrategy::InDomainSplit => Box::new(InDomainSplit),
            ValueSelectionStrategy::InDomainSplitRandom => Box::new(InDomainSplitRandom),
            ValueSelectionStrategy::OutDomainMax => Box::new(OutDomainMax),
            ValueSelectionStrategy::OutDomainMedian => Box::new(OutDomainMedian),
            ValueSelectionStrategy::OutDomainMin => Box::new(OutDomainMin),
            ValueSelectionStrategy::OutDomainRandom => Box::new(OutDomainRandom),
        })
    }
}

pub(crate) enum Search {
    Bool(SearchStrategy),
    Int(SearchStrategy),
    Seq(Vec<Search>),
    Unspecified,
}

pub(crate) struct SearchStrategy {
    pub(crate) variables: AnnExpr,
    pub(crate) variable_selection_strategy: VariableSelectionStrategy,
    pub(crate) value_selection_strategy: ValueSelectionStrategy,
}

pub(crate) struct FlatZincAst {
    pub(crate) parameter_decls: Vec<flatzinc::ParDeclItem>,
    pub(crate) single_variables: Vec<SingleVarDecl>,
    pub(crate) variable_arrays: Vec<VarArrayDecl>,
    pub(crate) constraint_decls: Vec<flatzinc::ConstraintItem>,
    pub(crate) solve_item: flatzinc::SolveItem,
    pub(crate) search: Search,
}

impl FlatZincAst {
    pub(crate) fn builder() -> FlatZincAstBuilder {
        FlatZincAstBuilder {
            parameter_decls: vec![],
            single_variables: vec![],
            variable_arrays: vec![],
            constraint_decls: vec![],
            solve_item: None,
            search: None,
        }
    }
}

pub(crate) struct FlatZincAstBuilder {
    parameter_decls: Vec<flatzinc::ParDeclItem>,
    single_variables: Vec<SingleVarDecl>,
    variable_arrays: Vec<VarArrayDecl>,
    constraint_decls: Vec<flatzinc::ConstraintItem>,
    solve_item: Option<flatzinc::SolveItem>,

    search: Option<Search>,
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
        if let Some(annotation) = solve_item.annotations.first() {
            self.search = Some(FlatZincAstBuilder::find_search(annotation));
        } else {
            self.search = Some(Search::Unspecified)
        }
        let _ = self.solve_item.insert(solve_item);
    }

    fn find_search(annotation: &Annotation) -> Search {
        match &annotation.id[..] {
            "bool_search" => Search::Bool(FlatZincAstBuilder::find_direct_search(annotation)),
            "float_search" => panic!("Search over floats is currently not supported"),
            "int_search" => Search::Int(FlatZincAstBuilder::find_direct_search(annotation)),
            "seq_search" => {
                pumpkin_assert_eq_simple!(
                    annotation.expressions.len(),
                    1,
                    "Expected a single expression for sequential search"
                );
                Search::Seq(match &annotation.expressions[0] {
                    AnnExpr::Annotations(annotations) => annotations
                        .iter()
                        .map(FlatZincAstBuilder::find_search)
                        .collect::<Vec<_>>(),
                    other => {
                        panic!("Expected a list of annotations for `seq_search` but was {other:?}")
                    }
                })
            }
            "set_search" => panic!("Search over sets is currently not supported"),
            other => panic!("Did not recognise search strategy {other}"),
        }
    }

    fn find_direct_search(annotation: &Annotation) -> SearchStrategy {
        // First element is the optimization variable
        // Second element is the variable selection strategy
        // Third element is the value selection strategy
        // (Optional) Fourth element is the exploration strategy (e.g. complete search)
        pumpkin_assert_simple!(
            annotation.expressions.len() >= 3,
            "Expected the search annotation to have 3 or 4 elements but it has {} elements",
            annotation.expressions.len()
        );

        SearchStrategy {
            variables: annotation.expressions[0].clone(),
            variable_selection_strategy: FlatZincAstBuilder::find_variable_selection_strategy(
                &annotation.expressions[1],
            ),
            value_selection_strategy: FlatZincAstBuilder::find_value_selection_strategy(
                &annotation.expressions[2],
            ),
        }
    }

    fn find_variable_selection_strategy(input: &AnnExpr) -> VariableSelectionStrategy {
        match input {
            flatzinc::AnnExpr::Expr(inner) => match inner {
                flatzinc::Expr::VarParIdentifier(identifier) => match &identifier[..] {
                    "anti_first_fail" => VariableSelectionStrategy::AntiFirstFail,
                    "dom_w_deg" => VariableSelectionStrategy::DomWDeg,
                    "first_fail" => VariableSelectionStrategy::FirstFail,
                    "impact" => VariableSelectionStrategy::Impact,
                    "input_order" => VariableSelectionStrategy::InputOrder,
                    "largest" => VariableSelectionStrategy::Largest,
                    "max_regret" => VariableSelectionStrategy::MaxRegret,
                    "most_constrained" => VariableSelectionStrategy::MostConstrained,
                    "occurrence" => VariableSelectionStrategy::Occurrence,
                    "smallest" => VariableSelectionStrategy::Smallest,
                    other => panic!("Did not recognise variable selection strategy {other}"),
                },
                other => panic!("Expected VarParIdentifier but got {other:?}"),
            },
            other => panic!("Expected an expression but got {other:?}"),
        }
    }

    fn find_value_selection_strategy(input: &AnnExpr) -> ValueSelectionStrategy {
        match input {
            flatzinc::AnnExpr::Expr(inner) => match inner {
                flatzinc::Expr::VarParIdentifier(identifier) => match &identifier[..] {
                    "indomain" => ValueSelectionStrategy::InDomain,
                    "indomain_interval" => ValueSelectionStrategy::InDomainInterval,
                    "indomain_max" => ValueSelectionStrategy::InDomainMax,
                    "indomain_median" => ValueSelectionStrategy::InDomainMedian,
                    "indomain_middle" => ValueSelectionStrategy::InDomainMiddle,
                    "indomain_min" => ValueSelectionStrategy::InDomainMin,
                    "indomain_random" => ValueSelectionStrategy::InDomainRandom,
                    "indomain_reverse_split" => ValueSelectionStrategy::InDomainReverseSplit,
                    "indomain_split" => ValueSelectionStrategy::InDomainSplit,
                    "indomain_split_random" => ValueSelectionStrategy::InDomainSplitRandom,
                    "outdomain_max" => ValueSelectionStrategy::OutDomainMax,
                    "outdomain_median" => ValueSelectionStrategy::OutDomainMedian,
                    "outdomain_min" => ValueSelectionStrategy::OutDomainMin,
                    "outdomain_random" => ValueSelectionStrategy::OutDomainRandom,
                    other => panic!("Did not recognise value selection strategy {other}"),
                },
                other => panic!("Expected VarParIdentifier but got {other:?}"),
            },
            other => panic!("Expected an expression but got {other:?}"),
        }
    }

    pub(crate) fn build(self) -> Result<FlatZincAst, FlatZincError> {
        let FlatZincAstBuilder {
            parameter_decls,
            single_variables,
            variable_arrays,
            constraint_decls,
            solve_item,
            search,
        } = self;

        Ok(FlatZincAst {
            parameter_decls,
            single_variables,
            variable_arrays,
            constraint_decls,
            solve_item: solve_item.ok_or(FlatZincError::MissingSolveItem)?,
            search: search.ok_or(FlatZincError::MissingSolveItem)?,
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
        annos: Vec<Annotation>,
        array_expr: Option<flatzinc::ArrayOfBoolExpr>,
    },
    Int {
        #[allow(dead_code)]
        ix: flatzinc::IndexSet,
        id: String,
        annos: Vec<Annotation>,
        array_expr: Option<flatzinc::ArrayOfIntExpr>,
    },
}
