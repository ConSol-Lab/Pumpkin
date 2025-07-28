use fzn_rs::ast::RangeList;
use fzn_rs::ArrayExpr;
use fzn_rs::FromAnnotationArgument;
use fzn_rs::VariableExpr;
use log::warn;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_solver::branching::value_selection::DynamicValueSelector;
use pumpkin_solver::branching::value_selection::InDomainInterval;
use pumpkin_solver::branching::value_selection::InDomainMax;
use pumpkin_solver::branching::value_selection::InDomainMedian;
use pumpkin_solver::branching::value_selection::InDomainMiddle;
use pumpkin_solver::branching::value_selection::InDomainMin;
use pumpkin_solver::branching::value_selection::InDomainRandom;
use pumpkin_solver::branching::value_selection::InDomainSplit;
use pumpkin_solver::branching::value_selection::InDomainSplitRandom;
use pumpkin_solver::branching::value_selection::OutDomainMax;
use pumpkin_solver::branching::value_selection::OutDomainMedian;
use pumpkin_solver::branching::value_selection::OutDomainMin;
use pumpkin_solver::branching::value_selection::OutDomainRandom;
use pumpkin_solver::branching::value_selection::ReverseInDomainSplit;
use pumpkin_solver::branching::variable_selection::AntiFirstFail;
use pumpkin_solver::branching::variable_selection::DynamicVariableSelector;
use pumpkin_solver::branching::variable_selection::FirstFail;
use pumpkin_solver::branching::variable_selection::InputOrder;
use pumpkin_solver::branching::variable_selection::Largest;
use pumpkin_solver::branching::variable_selection::MaxRegret;
use pumpkin_solver::branching::variable_selection::Smallest;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::Literal;

#[derive(fzn_rs::FlatZincAnnotation)]
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
    pub(crate) fn create_from_literals(
        &self,
        propositional_variables: &[Literal],
    ) -> DynamicVariableSelector<Literal> {
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

#[derive(fzn_rs::FlatZincAnnotation)]
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
    pub(crate) fn create_for_literals(&self) -> DynamicValueSelector<Literal> {
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

/// The exploration strategies for search annotations.
///
/// See
/// https://docs.minizinc.dev/en/stable/lib-stdlib-annotations.html#exploration-strategy-annotations.
#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum Exploration {
    Complete,
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum SearchAnnotation {
    #[args]
    BoolSearch(BoolSearchArgs),
    #[args]
    IntSearch(IntSearchArgs),
    Seq(#[annotation] Vec<SearchAnnotation>),
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) struct IntSearchArgs {
    pub(crate) variables: ArrayExpr<VariableExpr<i32>>,
    #[annotation]
    pub(crate) variable_selection_strategy: VariableSelectionStrategy,
    #[annotation]
    pub(crate) value_selection_strategy: ValueSelectionStrategy,
    #[allow(
        dead_code,
        reason = "the int_search annotation has this argument, so it needs to be present here"
    )]
    #[annotation]
    pub(crate) exploration: Exploration,
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) struct BoolSearchArgs {
    pub(crate) variables: ArrayExpr<VariableExpr<bool>>,
    #[annotation]
    pub(crate) variable_selection_strategy: VariableSelectionStrategy,
    #[annotation]
    pub(crate) value_selection_strategy: ValueSelectionStrategy,
    #[allow(
        dead_code,
        reason = "the int_search annotation has this argument, so it needs to be present here"
    )]
    #[annotation]
    pub(crate) exploration: Exploration,
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum VariableAnnotations {
    OutputVar,
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum ArrayAnnotations {
    OutputArray(ArrayExpr<RangeList<i32>>),
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum ConstraintAnnotations {
    ConstraintTag(TagAnnotation),
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TagAnnotation(ConstraintTag);

impl From<ConstraintTag> for TagAnnotation {
    fn from(value: ConstraintTag) -> Self {
        TagAnnotation(value)
    }
}

impl From<TagAnnotation> for ConstraintTag {
    fn from(value: TagAnnotation) -> Self {
        value.0
    }
}

impl FromAnnotationArgument for TagAnnotation {
    fn from_argument(
        _: &fzn_rs::ast::Node<fzn_rs::ast::AnnotationArgument>,
    ) -> Result<Self, fzn_rs::InstanceError> {
        unreachable!("This never gets parsed from source")
    }
}

pub(crate) type Instance = fzn_rs::TypedInstance<
    i32,
    super::constraints::Constraints,
    VariableAnnotations,
    ArrayAnnotations,
    ConstraintAnnotations,
    SearchAnnotation,
>;
