use fzn_rs::ast::RangeList;
use fzn_rs::FromAnnotationArgument;
use fzn_rs::VariableArgument;
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

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum Search {
    #[args]
    BoolSearch(SearchStrategy),
    #[args]
    IntSearch(SearchStrategy),
    Seq(#[annotation] Vec<Search>),
    Unspecified,
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) struct SearchStrategy {
    pub(crate) variables: Vec<VariableArgument<i64>>,
    #[annotation]
    pub(crate) variable_selection_strategy: VariableSelectionStrategy,
    #[annotation]
    pub(crate) value_selection_strategy: ValueSelectionStrategy,
}

#[derive(fzn_rs::FlatZincConstraint)]
pub(crate) enum Constraints {
    IntEq(VariableArgument<i64>, VariableArgument<i64>),
    SetIn(VariableArgument<i64>, RangeList<i64>),
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum VariableAnnotations {
    OutputVar,
}

#[derive(fzn_rs::FlatZincAnnotation)]
pub(crate) enum ConstraintAnnotations {
    ConstraintTag(TagAnnotation),
}

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

pub(crate) type Instance =
    fzn_rs::TypedInstance<Constraints, VariableAnnotations, ConstraintAnnotations, Search>;
