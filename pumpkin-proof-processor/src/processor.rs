//! The proof processing facilities to turn a proof scaffold into a full DRCP proof.

use std::io::BufRead;
use std::io::Write;
use std::num::NonZero;
use std::rc::Rc;
use std::sync::Arc;

use drcp_format::Conclusion;
use drcp_format::ConstraintId;
use drcp_format::Deduction;
use drcp_format::Inference;
use drcp_format::IntAtomic;
use drcp_format::IntComparison;
use drcp_format::Step;
use drcp_format::reader::ProofReader;
use drcp_format::writer::ProofWriter;
use log::debug;
use log::info;
use log::trace;
use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::HashSet;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Priority;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::CurrentNogood;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::state::PropagatorHandle;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

use crate::deduction_propagator::DeductionPropagationMode;
use crate::deduction_propagator::DeductionPropagator;
use crate::deduction_propagator::DeductionPropagatorConstructor;
use crate::deduction_propagator::MARKED_PROPAGATION_PRIORITY;
use crate::deduction_propagator::UNMARKED_CONFLICT_PRIORITY;
use crate::deduction_propagator::UNMARKED_UNIT_PROPAGATION_PRIORITY;
use crate::explanation_graph::EmptyDomainConflict;
use crate::explanation_graph::ExplanationGraph;
use crate::predicate_heap::PredicateHeap;
use crate::variables::Variables;

#[derive(Debug)]
pub(crate) struct ProofProcessor {
    state: State,
    variables: Variables,

    /// Contains the proof that will be written to the output. Key note: this is in reverse order
    /// due to the backward trimming.
    output_proof: Vec<ProofStage>,

    /// A queue of predicates that should still be explained.
    ///
    /// This is a re-usable heap, but its contents is specific to the processing of an
    /// individual conflict.
    to_process_heap: PredicateHeap,

    /// Predicates over these variables are eliminated from the inferences of every proof stage
    /// (see [`ProofProcessor::eliminate_auxiliary_variables`]).
    auxiliary_variables: HashSet<DomainId>,
    /// The predicate-level form of the inferences of the proof stage that is being explained.
    explanation_graph: ExplanationGraph,
    /// The IDs of the linking nogoods, by their sorted predicates.
    linking_nogood_ids: HashMap<Vec<Predicate>, ConstraintId>,
    /// The proof stages that derive the linking nogoods, by their IDs.
    linking_stages: HashMap<ConstraintId, ProofStage>,
}

/// An inference as it is written to the output proof.
type OutputInference = Inference<String, i32, Arc<str>>;

/// A single proof stage for the proof.
#[derive(Debug)]
struct ProofStage {
    /// The ID of the constraint to introduce.
    constraint_id: ConstraintId,
    /// The inferences for the proof stage.
    inferences: Vec<Inference<String, i32, Arc<str>>>,
    /// The atomic constraints to assume `true` such that a conflict is derived throught the
    /// inferences.
    premises: Vec<IntAtomic<String, i32>>,
    /// The linking nogoods that the inferences cite; each is written before the first proof
    /// stage that cites it.
    linking_nogoods: Vec<ConstraintId>,
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum ProofProcessError {
    #[error("failed to read proof: {0}")]
    Read(#[from] drcp_format::reader::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("proof did not contain a conclusion step")]
    MissingConclusion,

    #[error("undefined variable '{0}'")]
    UndefinedVariable(String),

    #[error("the deduction {0} does not lead to a contradiction")]
    DeductionDoesNotConflict(ConstraintId),

    #[error("all deductions together do not propagate to conflict")]
    DeductionsDoNotConflict,

    #[error("conclusion is not supported by any deduction")]
    InvalidConclusion,

    #[error("the deduction {0} contains the auxiliary variable '{1}'")]
    AuxiliaryVariableInDeduction(ConstraintId, String),
}

/// A deduction that was posted to the solver.
#[derive(Clone, Debug)]
struct PostedDeduction {
    predicates: Rc<[Predicate]>,
    conflict_detection_handle: PropagatorHandle<DeductionPropagator>,
    unit_prop_handle: PropagatorHandle<DeductionPropagator>,
    marked: bool,
}

impl PostedDeduction {
    /// Deactivates the propagators associated with this `PostedDeduction`.
    fn deactivate_propagators(&self, state: &mut State) {
        state
            .get_propagator_mut(self.conflict_detection_handle)
            .expect("All handles are valid")
            .deactivate();
        state
            .get_propagator_mut(self.unit_prop_handle)
            .expect("All handles are valid")
            .deactivate();
    }

    /// Updates the priority of both propagators associated with this `PostedDeduction`.
    fn update_propagators_priority(&self, state: &mut State, new_priority: Priority) {
        state
            .get_propagator_mut(self.conflict_detection_handle)
            .expect("All handles are valid")
            .set_priority(new_priority);
        state
            .get_propagator_mut(self.unit_prop_handle)
            .expect("All handles are valid")
            .set_priority(new_priority);
    }
}

type DeductionStack = KeyedVec<ConstraintTag, Option<PostedDeduction>>;

impl ProofProcessor {
    /// Creates a new [`ProofProcessor`] from a [`State`] and [`Variables`].
    ///
    /// The state is assumed to be set up with propagators. For every variable in the model, there
    /// should be an entry in `variables` mapping the variable name to a domain in `state`.
    pub(crate) fn new(state: State, variables: Variables) -> Self {
        ProofProcessor {
            state,
            variables,
            output_proof: vec![],
            to_process_heap: PredicateHeap::default(),
            auxiliary_variables: HashSet::default(),
            explanation_graph: ExplanationGraph::default(),
            linking_nogood_ids: HashMap::default(),
            linking_stages: HashMap::default(),
        }
    }

    /// Sets the variables to eliminate from the inferences of every proof stage. The deductions in
    /// the scaffold must not contain them.
    pub(crate) fn set_auxiliary_variables(&mut self, auxiliary_variables: HashSet<DomainId>) {
        self.auxiliary_variables = auxiliary_variables;
    }

    /// Process the given proof and write the result to the proof writer.
    ///
    /// To process a proof, only the deductions from `proof_reader` are considered. Deductions that
    /// are redundant for proving the conclusion (determined by propagation) are removed. For the
    /// deductions that remain, inferences are introduced to complete the proof.
    ///
    /// Note that if the input proof contains inferences, they are ignored.
    pub(crate) fn process<R: BufRead, W: Write>(
        mut self,
        proof_reader: ProofReader<R, i32>,
        mut proof_writer: ProofWriter<W, i32>,
    ) -> Result<(), ProofProcessError> {
        // First, we will add all the deductions to the state.
        //
        // We get the conclusion of the proof, as well as the stack of deductions. The marking
        // based on the conclusion is done in the stack, so we need to look for the last marked
        // constraint and process from there.
        let (conclusion, mut nogood_stack) = self.initialise(proof_reader)?;

        info!("Processing a proof with {} stages", nogood_stack.len());
        let mut num_inferences = 0;

        // Next, we will start the backward trimming procedure.
        //
        // Initialization will have marked the deductions that are used to derive the
        // conclusion of the proof. So we immediately enter the backward trimming loop.
        // Going through the scaffold backwards, we remove every deduction. For each
        // deduction, if it is marked, we test whether we can derive a conflict through
        // propagation. If so, a new proof-stage is created with the appropriate
        // inferences.

        info!("Trimming deductions and determining inferences");
        while let Some((tag, maybe_deduction)) = nogood_stack.pop() {
            // Extract the nogood handle and whether the deduction is marked.
            let Some(posted_deduction) = maybe_deduction else {
                continue;
            };

            let new_checkpoint = self.state.get_checkpoint() - 1;
            let _ = self.state.restore_to(new_checkpoint);

            posted_deduction.deactivate_propagators(&mut self.state);

            debug!("Processing deduction {}", NonZero::from(tag));
            trace!("  nogood: {:?}", posted_deduction.predicates);

            // If the deduction was never used, then we don't have to consider it
            // further.
            if !posted_deduction.marked {
                trace!("Deduction {} is not marked", NonZero::from(tag));
                continue;
            }

            // Now we add the predicates in the nogood as assumptions to kick-start the
            // propagation.
            let possible_result = self
                .post_assumptions(&posted_deduction.predicates)
                .and_then(|_| self.state.propagate_to_fixed_point());

            let Err(conflict) = possible_result else {
                return Err(ProofProcessError::DeductionDoesNotConflict(tag.into()));
            };

            let inferences = self.explain_current_conflict(&mut nogood_stack, conflict);
            let premises =
                convert_predicates_to_proof_atomic(&self.variables, &posted_deduction.predicates);
            let stage = self.create_proof_stage(tag.into(), premises, inferences);
            num_inferences += stage.inferences.len();
            self.output_proof.push(stage);
        }

        info!(
            "Writing proof with {} stages and {} inferences",
            self.output_proof.len(),
            num_inferences,
        );

        if !self.auxiliary_variables.is_empty() {
            info!(
                "Eliminated auxiliary variables with {} linking nogoods",
                self.linking_stages.len()
            );
        }

        // Finally, we write the output proof to the file. Since the proof stages are in
        // reverse order, we iterate from the end to the beginning.
        for stage in std::mem::take(&mut self.output_proof).into_iter().rev() {
            for linking_nogood_id in stage.linking_nogoods.iter() {
                // A linking nogood that was written before is no longer in the map.
                if let Some(linking_stage) = self.linking_stages.remove(linking_nogood_id) {
                    write_proof_stage(&mut proof_writer, linking_stage)?;
                }
            }

            write_proof_stage(&mut proof_writer, stage)?;
        }

        let conclusion = match conclusion {
            Conclusion::Unsat => Conclusion::Unsat,
            Conclusion::DualBound(IntAtomic {
                name,
                comparison,
                value,
            }) => Conclusion::DualBound(IntAtomic {
                name: name.as_ref().to_owned(),
                comparison,
                value,
            }),
        };

        proof_writer.log_conclusion(conclusion)?;

        info!("Proof processed successfully");

        Ok(())
    }

    /// Initialise the state with the deductions from the proof.
    ///
    /// Any inferences that are encountered are ignored, since we will introduce our own
    /// inferences.
    fn initialise<R: BufRead>(
        &mut self,
        mut proof_reader: ProofReader<R, i32>,
    ) -> Result<(Conclusion<Rc<str>, i32>, DeductionStack), ProofProcessError> {
        info!("Setting up solver with deductions");

        let mut nogood_stack = KeyedVec::default();

        if let Err(conflict) = self.state.propagate_to_fixed_point() {
            // If the state is in a conflict after propagating only the model
            // constraints, the proof consists of only one proof stage.

            let empty_nogood_tag = self.state.new_constraint_tag();
            nogood_stack.accomodate(empty_nogood_tag, None);

            let inferences = self.explain_current_conflict(&mut nogood_stack, conflict);

            // Log the empty clause to the proof.
            let stage = self.create_proof_stage(empty_nogood_tag.into(), vec![], inferences);
            self.output_proof.push(stage);

            return Ok((Conclusion::Unsat, nogood_stack));
        }

        loop {
            // Try to read the next step from the proof.
            let next_step = proof_reader.next_step()?;

            // If we reached the end of the proof, it is incomplete.
            let Some(step) = next_step else {
                return Err(ProofProcessError::MissingConclusion);
            };

            // Extract the deduction from the step, or otherwise handle the step type.
            let deduction = match step {
                Step::Deduction(deduction) => deduction,

                // A dual bound conclusion is encountered. This is of the form `true -> bound`.
                // This means we have to find a nogood `!bound -> false` and mark it.
                Step::Conclusion(Conclusion::DualBound(bound)) => {
                    return self.prepare_trimming_for_dual_bound_conclusion(bound, nogood_stack);
                }

                Step::Conclusion(Conclusion::Unsat) => {
                    return Err(ProofProcessError::DeductionsDoNotConflict);
                }

                // We ignore inferences when doing proof processing. We will introduce our own
                // inferences regardless of what is in the input.
                Step::Inference(_) => continue,
            };

            // Get the constraint tag for this new constraint.
            //
            // To make comparing the scaffold with the full proof easier, we want to
            // match up the constraint IDs of the deductions. Hence, we generate
            // constraint IDs until the one we get is the same as observed in the
            // scaffold.
            let mut constraint_tag = self.state.new_constraint_tag();
            while NonZero::from(constraint_tag) < deduction.constraint_id {
                constraint_tag = self.state.new_constraint_tag();
            }
            assert_eq!(ConstraintId::from(constraint_tag), deduction.constraint_id);

            // Convert the deduction to the solver's representation.
            let nogood = PropositionalConjunction::from(
                deduction
                    .premises
                    .iter()
                    .map(|premise| convert_proof_atomic_to_predicate(&self.variables, premise))
                    .collect::<Result<Vec<_>, ProofProcessError>>()?,
            );

            if let Some(auxiliary_predicate) = nogood
                .iter()
                .find(|predicate| self.is_auxiliary(**predicate))
            {
                return Err(ProofProcessError::AuxiliaryVariableInDeduction(
                    deduction.constraint_id,
                    convert_predicate_to_proof_atomic(&self.variables, *auxiliary_predicate).name,
                ));
            }

            debug!("Adding deduction {}", deduction.constraint_id);
            trace!("    {nogood:?}");

            self.state.new_checkpoint();
            let nogood_rc = nogood.to_vec().into();
            let conflict_handle = self.state.add_propagator(DeductionPropagatorConstructor {
                nogood: Rc::clone(&nogood_rc),
                constraint_tag,
                priority: UNMARKED_CONFLICT_PRIORITY,
                propagation_mode: DeductionPropagationMode::OnlyConflictDetection,
            });

            let prop_handle = self.state.add_propagator(DeductionPropagatorConstructor {
                nogood: Rc::clone(&nogood_rc),
                constraint_tag,
                priority: UNMARKED_UNIT_PROPAGATION_PRIORITY,
                propagation_mode: DeductionPropagationMode::OnlyUnitPropagation,
            });
            nogood_stack.accomodate(constraint_tag, None);
            nogood_stack[constraint_tag] = Some(PostedDeduction {
                predicates: nogood_rc,
                conflict_detection_handle: conflict_handle,
                unit_prop_handle: prop_handle,
                marked: false,
            });

            if let Err(conflict) = self.state.propagate_to_fixed_point() {
                trace!("Conflict identified");

                // If we reach inconsistency through propagation alone, then it must mean that the
                // proof is a proof of unsatisfiability and not a dual bound proof.
                return self.repair_solver(conflict, constraint_tag, nogood_stack);
            }
        }
    }

    fn prepare_trimming_for_dual_bound_conclusion(
        &mut self,
        bound: IntAtomic<Rc<str>, i32>,
        mut nogood_stack: DeductionStack,
    ) -> Result<(Conclusion<Rc<str>, i32>, DeductionStack), ProofProcessError> {
        let concluded_predicate = convert_proof_atomic_to_predicate(&self.variables, &bound)?;
        info!("Found dual bound conclusion");
        trace!("bound = {concluded_predicate:?}");

        // If the claimed bound is not true given the current assignment, then the
        // conclusion does not follow by propagation.
        if self.state.truth_value(concluded_predicate) != Some(true) {
            return Err(ProofProcessError::InvalidConclusion);
        }

        // If the dual bound is the initial bound on the objective variable, write the
        // correct proof in that situation and short-circuit.
        if self.state.is_implied_by_initial_domain(concluded_predicate) {
            self.to_process_heap.push(concluded_predicate, &self.state);

            let inferences = self.explain_predicates(&mut nogood_stack, vec![]);
            let deduction_id = self.state.new_constraint_tag();

            let premises = vec![convert_predicate_to_proof_atomic(
                &self.variables,
                !concluded_predicate,
            )];
            let stage = self.create_proof_stage(deduction_id.into(), premises, inferences);
            self.output_proof.push(stage);

            return Ok((Conclusion::DualBound(bound), nogood_stack));
        }

        let mut reason_buffer = vec![];
        let inference_code = self.state.get_propagation_reason(
            concluded_predicate,
            &mut reason_buffer,
            CurrentNogood::empty(),
        );

        // We do not use the function to mark the constraint in the nogood stack. It
        // could happen that the conclusion is a root bound, but the proof does not
        // contain a nogood asserting the root bound (an inference is not enough, we
        // explicitly want a deduction that makes the conclusion true).
        trace!("Marking reason for dual bound");
        trace!("  reason = {reason_buffer:?}",);

        // We expect the conclusion to be syntactally implied by one of the deductions.
        let used_constraint_tag = inference_code.expect("must be due to a propagation").tag();
        trace!("  constraint_tag = {}", NonZero::from(used_constraint_tag));

        // Now we explain _why_ the concluded predicate is true. In the output proof, we must insert
        // a deduction that syntactally implies the conclusion. I.e., if the conclusion is [x >= 5],
        // then we _must_ have a nogood `[x <= 4] -> false`.
        //
        // To achieve this, we look at the reason that currently the conclusion is true, and then
        // insert the required deduction supported by the inferences as they exist in the processor
        // at the moment.
        self.to_process_heap.push(concluded_predicate, &self.state);
        let inferences = self.explain_predicates(&mut nogood_stack, vec![]);
        let deduction_id = self.state.new_constraint_tag();

        let premises = vec![convert_predicate_to_proof_atomic(
            &self.variables,
            !concluded_predicate,
        )];
        let stage = self.create_proof_stage(deduction_id.into(), premises, inferences);
        self.output_proof.push(stage);

        Ok((Conclusion::DualBound(bound), nogood_stack))
    }

    /// Takes a solver that is in an inconsistent state because we have a proof of
    /// unsatisfiability, and removes the last added constraint. We mark all the deductions in the
    /// deduction stack that contributed to the conflict, and log the empty nogood step.
    fn repair_solver(
        &mut self,
        conflict: Conflict,
        tag: ConstraintTag,
        mut nogood_stack: DeductionStack,
    ) -> Result<(Conclusion<Rc<str>, i32>, DeductionStack), ProofProcessError> {
        // Add the nogood that triggered the conflict to the nogood stack. Also mark it,
        // because obviously it is used.
        let posted_deduction = nogood_stack[tag]
            .as_mut()
            .expect("the deduction that triggered the conflict must be on the nogood stack");
        posted_deduction.marked = true;
        posted_deduction.update_propagators_priority(&mut self.state, MARKED_PROPAGATION_PRIORITY);

        let inferences = self.explain_current_conflict(&mut nogood_stack, conflict);

        // Log the empty clause to the proof.
        let empty_nogood_id = self.state.new_constraint_tag().into();
        let stage = self.create_proof_stage(empty_nogood_id, vec![], inferences);
        self.output_proof.push(stage);

        Ok((Conclusion::Unsat, nogood_stack))
    }

    /// Explain the current conflict and write to the output proof.
    fn explain_current_conflict(
        &mut self,
        nogood_stack: &mut DeductionStack,
        conflict: Conflict,
    ) -> Vec<Inference<String, i32, Arc<str>>> {
        assert!(self.to_process_heap.is_empty());

        // The inferences in the current conflict ordered by closeness to the conflict.
        // Every element is an option, where `None` serves as a tombstone value.
        //
        // We need tombstones because initial bounds can be used multiple times within the
        // same conflict. This means when we encounter an initial bound, it may already be
        // present in the inference sequence. If that is the case, we move it to the back.
        let mut inferences: Vec<Option<Inference<String, i32, Arc<str>>>> = vec![];

        let predicates_to_explain = match conflict {
            Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code,
            }) => {
                let generated_by = inference_code.tag();
                let label = inference_code.label();

                let constraint_id = self.state.new_constraint_tag().into();
                self.explanation_graph
                    .add_inference(constraint_id, &conjunction, None);
                inferences.push(Some(Inference {
                    constraint_id,
                    premises: convert_predicates_to_proof_atomic(&self.variables, &conjunction),
                    consequent: None,
                    generated_by: Some(generated_by.into()),
                    label: Some(label),
                }));

                self.mark_stack_entry(nogood_stack, inference_code);

                conjunction
            }
            Conflict::EmptyDomain(empty_domain_confict) => {
                let mut predicates_to_explain = vec![];
                let maybe_trigger_inference = empty_domain_confict.get_reason(
                    &mut self.state,
                    &mut predicates_to_explain,
                    CurrentNogood::empty(),
                );

                let mut trigger_inference = None;
                if let Some(inference_code) = maybe_trigger_inference {
                    let generated_by = inference_code.tag();
                    let label = inference_code.label();

                    let constraint_id = self.state.new_constraint_tag().into();
                    self.explanation_graph.add_inference(
                        constraint_id,
                        &predicates_to_explain,
                        Some(empty_domain_confict.trigger_predicate),
                    );
                    trigger_inference = Some(constraint_id);
                    inferences.push(Some(Inference {
                        constraint_id,
                        premises: convert_predicates_to_proof_atomic(
                            &self.variables,
                            &predicates_to_explain,
                        ),
                        consequent: Some(convert_predicate_to_proof_atomic(
                            &self.variables,
                            empty_domain_confict.trigger_predicate,
                        )),
                        generated_by: Some(generated_by.into()),
                        label: Some(label),
                    }));

                    self.mark_stack_entry(nogood_stack, inference_code);
                }

                self.explanation_graph
                    .set_empty_domain_conflict(EmptyDomainConflict {
                        trigger_predicate: empty_domain_confict.trigger_predicate,
                        trigger_inference,
                        reason: predicates_to_explain.clone(),
                    });

                predicates_to_explain.push(!empty_domain_confict.trigger_predicate);

                predicates_to_explain.into()
            }
        };

        // Then we add the conflict to the queue of predicates that need to be explained
        // through inferences.
        for predicate in predicates_to_explain {
            self.to_process_heap.push(predicate, &self.state);
        }

        self.explain_predicates(nogood_stack, inferences)
    }

    fn explain_predicates(
        &mut self,
        nogood_stack: &mut DeductionStack,
        mut inferences: Vec<Option<Inference<String, i32, Arc<str>>>>,
    ) -> Vec<Inference<String, i32, Arc<str>>> {
        let mut initial_bound_indices = HashMap::new();
        let mut reason_buffer = vec![];

        let mut visited_predicates = HashSet::new();
        // For every predicate in the queue, we will introduce appropriate inferences into
        // the proof.
        while let Some(predicate) = self.to_process_heap.pop() {
            if !visited_predicates.insert(predicate) {
                continue;
            }
            // The predicate is either propagated or an initial bound. If it is an
            // initial bound, we dispatch that here.
            if self.state.is_implied_by_initial_domain(predicate) {
                // The initial bound inference is either logged already, or still needs
                // to be logged.
                let inference = match initial_bound_indices.get(&predicate).copied() {
                    // If it is logged previously, we take that inference and replace
                    // that index with a `None`, to append the inference at the current
                    // end.
                    Some(index) => std::mem::take(inferences.get_mut(index).unwrap()),

                    // It is not logged previously, so we log an initial domain
                    // inference.
                    None => {
                        let constraint_id = self.state.new_constraint_tag().into();
                        self.explanation_graph
                            .add_inference(constraint_id, &[], Some(predicate));
                        self.explanation_graph
                            .add_derivation(predicate, Some(constraint_id), &[]);
                        Some(Inference {
                            constraint_id,
                            premises: vec![],
                            consequent: Some(convert_predicate_to_proof_atomic(
                                &self.variables,
                                predicate,
                            )),
                            generated_by: None,
                            label: Some(Arc::from("initial_domain")),
                        })
                    }
                };

                // The inference is now at the end of the sequence, and we store the
                // index to this particular initial bound.
                inferences.push(inference);
                let _ = initial_bound_indices.insert(predicate, inferences.len() - 1);

                continue;
            }

            // In this case, the predicate was propagated by a constraint. We compute the reason
            // and log the appropriate inference step.
            let inference_code = self.state.get_propagation_reason(
                predicate,
                &mut reason_buffer,
                CurrentNogood::empty(),
            );

            // The inference code can be `None` if the reason is constructed to explain an implied
            // predicate. Those inferences do not need to be in the proof, so we only create an
            // inference for propagations by constraints.
            if let Some(inference_code) = inference_code {
                self.mark_stack_entry(nogood_stack, inference_code.clone());

                let constraint_id = self.state.new_constraint_tag().into();
                self.explanation_graph.add_inference(
                    constraint_id,
                    &reason_buffer,
                    Some(predicate),
                );
                self.explanation_graph.add_derivation(
                    predicate,
                    Some(constraint_id),
                    &reason_buffer,
                );

                let label = inference_code.label();
                inferences.push(Some(Inference {
                    constraint_id,
                    premises: convert_predicates_to_proof_atomic(&self.variables, &reason_buffer),
                    consequent: Some(convert_predicate_to_proof_atomic(
                        &self.variables,
                        predicate,
                    )),
                    generated_by: Some(inference_code.tag().into()),
                    label: Some(label),
                }));
            } else {
                self.explanation_graph
                    .add_derivation(predicate, None, &reason_buffer);
            }

            for predicate in reason_buffer.drain(..) {
                self.to_process_heap.push(predicate, &self.state);
            }
        }

        // Reverse the inference sequence, since analysis goes from the conflict to the
        // leafs rather than from the assumptions to the conflict. Also, filter out the
        // tombstone values.
        inferences.into_iter().rev().flatten().collect()
    }

    fn post_assumptions(&mut self, predicates: &[Predicate]) -> Result<(), Conflict> {
        for predicate in predicates.iter() {
            let _ = self.state.post(*predicate)?;
        }

        Ok(())
    }
    /// Given a [`DeductionStack`], mark the constraint indicated by the inference code as used.
    fn mark_stack_entry(&mut self, stack: &mut DeductionStack, inference_code: InferenceCode) {
        let used_constraint_tag = inference_code.tag();

        trace!("Marking constraint {}", NonZero::from(used_constraint_tag));

        let stack_entry = &mut stack[used_constraint_tag];
        if let Some(posted_deduction) = stack_entry {
            posted_deduction.marked = true;
            posted_deduction
                .update_propagators_priority(&mut self.state, MARKED_PROPAGATION_PRIORITY);
        }
    }

    /// Creates the proof stage for the given inferences, eliminating the auxiliary variables from
    /// them if there are any.
    fn create_proof_stage(
        &mut self,
        constraint_id: ConstraintId,
        premises: Vec<IntAtomic<String, i32>>,
        inferences: Vec<OutputInference>,
    ) -> ProofStage {
        let (inferences, linking_nogoods) = if self.auxiliary_variables.is_empty() {
            (inferences, vec![])
        } else {
            self.eliminate_auxiliary_variables(inferences)
        };
        self.explanation_graph.clear();

        ProofStage {
            constraint_id,
            inferences,
            premises,
            linking_nogoods,
        }
    }

    /// Removes the auxiliary variables from the inferences of a proof stage, given in proof order.
    ///
    /// Inferences with a consequent over an auxiliary variable are removed. An inference with
    /// premises over auxiliary variables is replaced by an inference over the premises from which
    /// those were derived, which cites a linking nogood. The linking nogood is derived by the
    /// replaced inferences, so the auxiliary variables only occur in its derivation.
    ///
    /// Returns the new inferences and the IDs of the linking nogoods they cite.
    fn eliminate_auxiliary_variables(
        &mut self,
        inferences: Vec<OutputInference>,
    ) -> (Vec<OutputInference>, Vec<ConstraintId>) {
        let empty_domain_conflict = self.explanation_graph.empty_domain_conflict().cloned();
        let is_auxiliary_trigger = empty_domain_conflict.as_ref().is_some_and(|conflict| {
            self.auxiliary_variables
                .contains(&conflict.trigger_predicate.get_domain())
        });

        let mut new_inferences = vec![];
        let mut linking_nogoods = vec![];

        for inference in inferences.iter() {
            let (premises, consequent) = self
                .explanation_graph
                .get_inference(inference.constraint_id)
                .cloned()
                .expect("every inference is in the explanation graph");

            // The conflict is an empty domain of an auxiliary variable, so the inference that
            // emptied it is replaced by one which derives the conflict from its premises.
            let is_trigger_inference = is_auxiliary_trigger
                && empty_domain_conflict
                    .as_ref()
                    .and_then(|conflict| conflict.trigger_inference)
                    == Some(inference.constraint_id);

            if is_trigger_inference {
                let conflict = empty_domain_conflict.as_ref().unwrap();
                let mut conflict_premises = conflict.reason.clone();
                conflict_premises.push(!conflict.trigger_predicate);

                let replacement = self.replace_by_linking_nogood(
                    &inferences,
                    Some(inference.constraint_id),
                    &conflict_premises,
                    None,
                );
                linking_nogoods.push(replacement.generated_by.unwrap());
                new_inferences.push(replacement);
                continue;
            }

            if consequent.is_some_and(|consequent| self.is_auxiliary(consequent)) {
                continue;
            }

            if premises.iter().any(|&premise| self.is_auxiliary(premise)) {
                let replacement = self.replace_by_linking_nogood(
                    &inferences,
                    Some(inference.constraint_id),
                    &premises,
                    consequent,
                );
                linking_nogoods.push(replacement.generated_by.unwrap());
                new_inferences.push(replacement);
                continue;
            }

            new_inferences.push(inference.clone());
        }

        // The domain of an auxiliary variable became empty without an inference, so no inference
        // above derives the conflict.
        if let Some(conflict) = empty_domain_conflict
            .filter(|conflict| is_auxiliary_trigger && conflict.trigger_inference.is_none())
        {
            let mut conflict_premises = conflict.reason.clone();
            conflict_premises.push(!conflict.trigger_predicate);

            let replacement =
                self.replace_by_linking_nogood(&inferences, None, &conflict_premises, None);
            linking_nogoods.push(replacement.generated_by.unwrap());
            new_inferences.push(replacement);
        }

        (new_inferences, linking_nogoods)
    }

    /// Creates an inference from `premises` to `consequent` in which every premise over an
    /// auxiliary variable is replaced by the premises from which it was derived. The inference
    /// cites a linking nogood, which is created if it does not exist yet.
    ///
    /// The `replaced_inference` is the inference among `inferences` that derives `consequent` from
    /// `premises`, if there is one.
    fn replace_by_linking_nogood(
        &mut self,
        inferences: &[OutputInference],
        replaced_inference: Option<ConstraintId>,
        premises: &[Predicate],
        consequent: Option<Predicate>,
    ) -> OutputInference {
        let (resolved_premises, used_inferences) = self
            .explanation_graph
            .resolve_auxiliary_premises(premises, |predicate| self.is_auxiliary(predicate));

        let mut linking_nogood = resolved_premises.clone();
        if let Some(consequent) = consequent {
            linking_nogood.push(!consequent);
        }
        linking_nogood.sort();
        linking_nogood.dedup();

        let linking_nogood_id = match self.linking_nogood_ids.get(&linking_nogood) {
            Some(&linking_nogood_id) => linking_nogood_id,
            None => {
                // Every inference is only valid within the proof stage that precedes it, so the
                // derivation gets copies with new IDs.
                let derivation = inferences
                    .iter()
                    .filter(|inference| {
                        used_inferences.contains(&inference.constraint_id)
                            || replaced_inference == Some(inference.constraint_id)
                    })
                    .map(|inference| Inference {
                        constraint_id: self.state.new_constraint_tag().into(),
                        ..inference.clone()
                    })
                    .collect::<Vec<_>>();

                let linking_nogood_id: ConstraintId = self.state.new_constraint_tag().into();
                let _ = self.linking_stages.insert(
                    linking_nogood_id,
                    ProofStage {
                        constraint_id: linking_nogood_id,
                        inferences: derivation,
                        premises: convert_predicates_to_proof_atomic(
                            &self.variables,
                            &linking_nogood,
                        ),
                        linking_nogoods: vec![],
                    },
                );
                let _ = self
                    .linking_nogood_ids
                    .insert(linking_nogood, linking_nogood_id);

                linking_nogood_id
            }
        };

        Inference {
            constraint_id: self.state.new_constraint_tag().into(),
            premises: convert_predicates_to_proof_atomic(&self.variables, &resolved_premises),
            consequent: consequent
                .map(|consequent| convert_predicate_to_proof_atomic(&self.variables, consequent)),
            generated_by: Some(linking_nogood_id),
            label: Some(Arc::from("nogood")),
        }
    }

    fn is_auxiliary(&self, predicate: Predicate) -> bool {
        self.auxiliary_variables.contains(&predicate.get_domain())
    }
}

/// Writes the inferences of the proof stage followed by its deduction.
fn write_proof_stage<W: Write>(
    proof_writer: &mut ProofWriter<W, i32>,
    stage: ProofStage,
) -> Result<(), ProofProcessError> {
    let mut sequence = Vec::with_capacity(stage.inferences.len());

    for inference in stage.inferences.into_iter() {
        sequence.push(inference.constraint_id);
        proof_writer.log_inference(inference)?;
    }

    proof_writer.log_deduction(Deduction {
        constraint_id: stage.constraint_id,
        premises: stage.premises,
        sequence,
    })?;

    Ok(())
}

/// Converts a slice of predicates to atomic constraints for the proof log.
fn convert_predicates_to_proof_atomic(
    variables: &Variables,
    predicates: &[Predicate],
) -> Vec<IntAtomic<String, i32>> {
    predicates
        .iter()
        .map(|predicate| convert_predicate_to_proof_atomic(variables, *predicate))
        .collect()
}

/// Converts an atomic from the proofs representation to a solver [`Predicate`].
fn convert_predicate_to_proof_atomic(
    variables: &Variables,
    predicate: Predicate,
) -> IntAtomic<String, i32> {
    let name = variables
        .get_name_for_domain(predicate.get_domain())
        .unwrap()
        .as_ref()
        .to_owned();

    let comparison = match predicate.get_predicate_type() {
        PredicateType::LowerBound => IntComparison::GreaterEqual,
        PredicateType::UpperBound => IntComparison::LessEqual,
        PredicateType::NotEqual => IntComparison::NotEqual,
        PredicateType::Equal => IntComparison::Equal,
    };

    let value = predicate.get_right_hand_side();

    IntAtomic {
        name,
        comparison,
        value,
    }
}

/// Converts an atomic from the proofs representation to a solver [`Predicate`].
fn convert_proof_atomic_to_predicate(
    variables: &Variables,
    atomic: &IntAtomic<Rc<str>, i32>,
) -> Result<Predicate, ProofProcessError> {
    let domain_id = variables
        .get_domain_by_name(&atomic.name)
        .ok_or_else(|| ProofProcessError::UndefinedVariable(atomic.name.as_ref().to_owned()))?;

    let predicate = match atomic.comparison {
        IntComparison::GreaterEqual => {
            predicate![domain_id >= atomic.value]
        }
        IntComparison::LessEqual => {
            predicate![domain_id <= atomic.value]
        }
        IntComparison::Equal => predicate![domain_id == atomic.value],
        IntComparison::NotEqual => {
            predicate![domain_id != atomic.value]
        }
    };

    Ok(predicate)
}

#[cfg(test)]
mod tests {
    use drcp_format::IntComparison::*;
    use drcp_format::reader::ReadAtomic;
    use drcp_format::reader::ReadStep;
    use pumpkin_checking::InferenceChecker;
    use pumpkin_checking::VariableState;
    use pumpkin_core::declare_inference_label;
    use pumpkin_core::propagation::EventsToRegister;
    use pumpkin_core::propagation::PropagationContext;
    use pumpkin_core::propagation::Propagator;
    use pumpkin_core::propagation::PropagatorConstructor;
    use pumpkin_core::propagation::PropagatorConstructorContext;
    use pumpkin_core::propagation::PropagatorSpec;
    use pumpkin_core::propagation::ReadDomains;
    use pumpkin_core::propagation::RuntimeCheckers;
    use pumpkin_core::state::PropagationStatusCP;
    use pumpkin_propagators::arithmetic::BinaryEqualsPropagatorArgs;

    use super::*;

    #[test]
    fn dual_bound_is_initial_bound() {
        let mut state = State::default();
        let mut variables = Variables::default();
        let x1 = state.new_interval_variable(10, 20, Some("x1".into()));
        variables.add_variable("x1".into(), x1);

        let scaffold = r#"
            a 1 [x1 <= 9]
            n 1 1 0
            c -1
        "#;

        let expected = vec![
            inference(
                2,
                [],
                Some(atomic("x1", GreaterEqual, 10)),
                None,
                Some("initial_domain"),
            ),
            deduction(3, [atomic("x1", LessEqual, 9)], [2]),
            Step::Conclusion(Conclusion::DualBound(atomic("x1", GreaterEqual, 10))),
        ];

        test_processing(state, variables, scaffold, expected);
    }

    #[test]
    fn dual_bound_is_root_propagation() {
        let mut state = State::default();
        let mut variables = Variables::default();
        let x1 = state.new_interval_variable(0, 20, Some("x1".into()));
        variables.add_variable("x1".into(), x1);
        let x2 = state.new_interval_variable(10, 10, Some("x2".into()));
        variables.add_variable("x2".into(), x2);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(BinaryEqualsPropagatorArgs {
            a: x1,
            b: x2,
            constraint_tag,
        });

        let scaffold = r#"
            a 1 [x1 <= 9]
            n 2 1 0
            c -1
        "#;

        let expected = vec![
            inference(
                4,
                [],
                Some(atomic("x2", GreaterEqual, 10)),
                None,
                Some("initial_domain"),
            ),
            inference(
                3,
                [atomic("x2", GreaterEqual, 10)],
                Some(atomic("x1", GreaterEqual, 10)),
                Some(1),
                Some("binary_equals"),
            ),
            deduction(5, [atomic("x1", LessEqual, 9)], [4, 3]),
            Step::Conclusion(Conclusion::DualBound(atomic("x1", GreaterEqual, 10))),
        ];

        test_processing(state, variables, scaffold, expected);
    }

    #[test]
    fn dual_bound_conclusion_via_deduction_with_extra_premise_keeps_matching_singleton() {
        let mut state = State::default();
        let mut variables = Variables::default();

        let a = state.new_interval_variable(0, 0, Some("a".into()));
        variables.add_variable("a".into(), a);
        let obj = state.new_interval_variable(0, 20, Some("obj".into()));
        variables.add_variable("obj".into(), obj);

        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(AlwaysConflictConstructor {
            watched: predicate![obj >= 6],
            other: predicate![a <= 0],
            constraint_tag,
        });

        let scaffold = r#"
            a 1 [a <= 0]
            a 2 [obj >= 6]
            n 2 1 2 0
            n 3 2 0
            c -2
        "#;

        let expected = vec![
            inference(
                8,
                [],
                Some(atomic("a", LessEqual, 0)),
                None,
                Some("initial_domain"),
            ),
            inference(
                7,
                [atomic("obj", GreaterEqual, 6), atomic("a", LessEqual, 0)],
                None,
                Some(1),
                Some("always_conflict"),
            ),
            deduction(
                2,
                [atomic("a", LessEqual, 0), atomic("obj", GreaterEqual, 6)],
                [8, 7],
            ),
            inference(
                5,
                [],
                Some(atomic("a", LessEqual, 0)),
                None,
                Some("initial_domain"),
            ),
            inference(
                4,
                [atomic("a", LessEqual, 0)],
                Some(atomic("obj", LessEqual, 5)),
                Some(2),
                Some("nogood"),
            ),
            deduction(6, [atomic("obj", GreaterEqual, 6)], [5, 4]),
            Step::Conclusion(Conclusion::DualBound(atomic("obj", LessEqual, 5))),
        ];

        test_processing(state, variables, scaffold, expected);
    }

    fn inference(
        constraint_id: u32,
        premises: impl Into<Vec<ReadAtomic<i32>>>,
        consequent: Option<ReadAtomic<i32>>,
        generated_by: Option<u32>,
        label: Option<&str>,
    ) -> ReadStep<i32> {
        Step::Inference(Inference {
            constraint_id: NonZero::new(constraint_id).expect("constraint_id is non-zero"),
            premises: premises.into(),
            consequent,
            generated_by: generated_by
                .map(|id| NonZero::new(id).expect("constraint_id is non-zero")),
            label: label.map(Rc::from),
        })
    }

    fn deduction(
        constraint_id: u32,
        premises: impl Into<Vec<ReadAtomic<i32>>>,
        sequence: impl IntoIterator<Item = u32>,
    ) -> ReadStep<i32> {
        Step::Deduction(Deduction {
            constraint_id: NonZero::new(constraint_id).expect("constraint_id is non-zero"),
            premises: premises.into(),
            sequence: sequence
                .into_iter()
                .map(|id| NonZero::new(id).expect("constraint_id is non-zero"))
                .collect(),
        })
    }

    fn atomic(name: &str, comparison: IntComparison, value: i32) -> ReadAtomic<i32> {
        IntAtomic {
            name: Rc::from(name),
            comparison,
            value,
        }
    }

    fn test_processing(
        state: State,
        variables: Variables,
        scaffold: &str,
        expected: Vec<ReadStep<i32>>,
    ) {
        let mut processed = Vec::new();

        let processor = ProofProcessor::new(state, variables);
        processor
            .process(
                ProofReader::new(scaffold.as_bytes()),
                ProofWriter::new(&mut processed),
            )
            .expect("successful process");

        let mut processed_reader = ProofReader::<_, i32>::new(processed.as_slice());
        let processed_proof = std::iter::from_fn(|| {
            processed_reader
                .next_step()
                .expect("processor returns valid proof")
        })
        .collect::<Vec<_>>();

        assert_eq!(processed_proof, expected);
    }

    /// A [`PropagatorConstructor`] for [`AlwaysConflictPropagator`].
    struct AlwaysConflictConstructor {
        watched: Predicate,
        other: Predicate,
        constraint_tag: ConstraintTag,
    }

    impl PropagatorConstructor for AlwaysConflictConstructor {
        type PropagatorImpl = AlwaysConflictPropagator;

        fn create(
            self,
            mut context: PropagatorConstructorContext,
        ) -> PropagatorSpec<Self::PropagatorImpl> {
            declare_inference_label!(AlwaysConflict);

            let AlwaysConflictConstructor {
                watched,
                other,
                constraint_tag,
            } = self;

            // This propagator must not react to `other` becoming true. It only
            // needs to fire once `watched` becomes true.
            let _ = context.register_predicate(watched);

            let mut checkers = RuntimeCheckers::builder();
            let inference_code = checkers.add_inference_checker(
                constraint_tag,
                AlwaysConflict,
                AlwaysConflictChecker { watched, other },
            );

            PropagatorSpec {
                registration: EventsToRegister::empty(),
                checkers: checkers.build(),
                propagator: AlwaysConflictPropagator {
                    watched,
                    other,
                    inference_code,
                },
            }
        }
    }

    #[derive(Clone, Debug)]
    struct AlwaysConflictChecker {
        watched: Predicate,
        other: Predicate,
    }

    impl InferenceChecker<Predicate> for AlwaysConflictChecker {
        fn check(
            &self,
            state: VariableState<Predicate>,
            _premises: &[Predicate],
            _consequent: Option<&Predicate>,
        ) -> bool {
            state.is_true(&self.watched) && state.is_true(&self.other)
        }
    }

    /// A propagator that reports a conflict as soon as both `watched` and `other` are true.
    ///
    /// Since only `watched` is registered for notifications (see
    /// [`AlwaysConflictConstructor::create`]), this propagator stays dormant until `watched` is
    /// explicitly posited, regardless of when `other` becomes true.
    #[derive(Clone)]
    struct AlwaysConflictPropagator {
        watched: Predicate,
        other: Predicate,
        inference_code: InferenceCode,
    }

    impl Propagator for AlwaysConflictPropagator {
        fn name(&self) -> &str {
            "AlwaysConflict"
        }

        fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
            let is_watched_satisfied = context.evaluate_predicate(self.watched) == Some(true);
            let is_other_satisfied = context.evaluate_predicate(self.other) == Some(true);

            if is_watched_satisfied && is_other_satisfied {
                return Err(Conflict::Propagator(PropagatorConflict {
                    conjunction: [self.watched, self.other].into_iter().collect(),
                    inference_code: self.inference_code.clone(),
                }));
            }

            Ok(())
        }
    }
}
