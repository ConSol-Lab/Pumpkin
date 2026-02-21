//! The proof processing facilities to turn a proof scaffold into a full DRCP proof.
//!
//! In future this should be moved to a separate crate, however, currently this depends on features
//! that are not (and should not be) in the public API of the `pumpkin-core` crate.

use std::collections::HashMap;
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
use pumpkin_core::asserts::pumpkin_assert_moderate;
use pumpkin_core::containers::KeyValueHeap;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateIdGenerator;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::PredicateId;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::CurrentNogood;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::state::PropagatorHandle;
use pumpkin_core::state::State;

use crate::deduction_propagator::DeductionPropagator;
use crate::deduction_propagator::DeductionPropagatorArgs;
use crate::variables::Variables;

#[derive(Debug)]
pub(crate) struct ProofProcessor {
    state: State,
    variables: Variables,

    /// Contains the proof that will be written to the output. Key note: this is in reverse order
    /// due to the backward trimming.
    output_proof: Vec<ProofStage>,

    /// A mapping from predicates to predicate IDs.
    predicate_ids: PredicateIdGenerator,
    /// Heap containing the predicates which still need to be processed; sorted non-increasing
    /// based on trail-index where implied predicates are processed first.
    to_process_heap: KeyValueHeap<PredicateId, u32>,
}

#[derive(Debug)]
struct ProofStage {
    inferences: Vec<Inference<String, i32, Arc<str>>>,
    constraint_id: ConstraintId,
    premises: Vec<IntAtomic<String, i32>>,
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
}

/// A deduction that was posted to the solver.
#[derive(Clone, Debug)]
struct PostedDeduction {
    predicates: PropositionalConjunction,
    handle: PropagatorHandle<DeductionPropagator>,
    marked: bool,
}

type DeductionStack = KeyedVec<ConstraintTag, Option<PostedDeduction>>;

impl ProofProcessor {
    pub(crate) fn new(state: State, variables: Variables) -> Self {
        ProofProcessor {
            state,
            variables,
            output_proof: vec![],
            predicate_ids: PredicateIdGenerator::default(),
            to_process_heap: KeyValueHeap::default(),
        }
    }

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
            self.state
                .get_propagator_mut(posted_deduction.handle)
                .expect("all handles are valid")
                .deactivate();

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
            self.output_proof.push(ProofStage {
                inferences,
                constraint_id: tag.into(),
                premises: convert_predicates_to_proof_atomic(
                    &self.variables,
                    &posted_deduction.predicates,
                ),
            });
        }

        info!("Writing final proof to file");

        // Finally, we write the output proof to the file. Since the proof stages are in
        // reverse order, we iterate from the end to the beginning.
        for stage in std::mem::take(&mut self.output_proof).into_iter().rev() {
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
            self.output_proof.push(ProofStage {
                inferences,
                constraint_id: empty_nogood_tag.into(),
                premises: vec![],
            });

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

            debug!("Adding deduction {}", deduction.constraint_id);
            trace!("    {nogood:?}");

            self.state.new_checkpoint();
            let handle = self.state.add_propagator(DeductionPropagatorArgs {
                nogood: nogood.iter().copied().collect(),
                constraint_tag,
            });

            nogood_stack.accomodate(constraint_tag, None);
            nogood_stack[constraint_tag] = Some(PostedDeduction {
                predicates: nogood,
                handle,
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
        let predicate = convert_proof_atomic_to_predicate(&self.variables, &bound)?;
        info!("Found dual bound conclusion");
        trace!("bound = {predicate:?}");

        // If the claimed bound is not true given the current assignment, then the
        // conclusion does not follow by propagation.
        if self.state.truth_value(predicate) != Some(true) {
            return Err(ProofProcessError::InvalidConclusion);
        }

        // If the dual bound is the initial bound on the objective variable, write the
        // correct proof in that situation and short-circuit.
        if self.state.is_trivially_assigned(predicate) {
            self.add_predicate_to_explain(predicate);
            let inferences = self.explain_predicates(&mut nogood_stack, vec![]);
            let deduction_id = self.state.new_constraint_tag();

            self.output_proof.push(ProofStage {
                inferences,
                constraint_id: deduction_id.into(),
                premises: vec![convert_predicate_to_proof_atomic(
                    &self.variables,
                    !predicate,
                )],
            });

            return Ok((Conclusion::DualBound(bound), nogood_stack));
        }

        let mut reason_buffer = vec![];
        let inference_code = self.state.get_propagation_reason(
            predicate,
            &mut reason_buffer,
            CurrentNogood::empty(),
        );

        // We do not use the function to mark the constraint in the nogood stack. It
        // could happen that the conclusion is a root bound, but the proof does not
        // contain a nogood asserting the root bound (an inference is not enough, we
        // explicitly want a deduction that makes the conclusion true).
        trace!("Marking reason for dual bound");
        trace!("  reason = {reason_buffer:?}",);

        // TODO: if the predicate is implied, then `inference_code` will be `None`. In that case,
        // we need to make sure to mark all the inferences that imply predicate, and not just the
        // first one.
        //
        // For example, let's say we have the following proof:
        // ```
        // ...
        // n <id1> [obj <= 5]
        // ...
        // n <id2> [obj != 5]
        // c [obj >= 7]
        // ```
        // In this case, we have to mark both <id1> and <id2>.
        let used_constraint_tag = inference_code.expect("must be due to a propagation").tag();
        trace!("  constraint_tag = {}", NonZero::from(used_constraint_tag));

        let Some(stack_entry) = nogood_stack
            .get_mut(used_constraint_tag)
            .map(|opt| opt.as_mut())
        else {
            return Err(ProofProcessError::InvalidConclusion);
        };

        if let Some(posted_deduction) = stack_entry {
            posted_deduction.marked = true;
        } else {
            // In this case we have to explain by 'root propagation' and no deductions
            // were used. The predicate is propagated by a propagator.
            self.add_predicate_to_explain(predicate);
            let inferences = self.explain_predicates(&mut nogood_stack, vec![]);
            let deduction_id = self.state.new_constraint_tag();

            self.output_proof.push(ProofStage {
                inferences,
                constraint_id: deduction_id.into(),
                premises: vec![convert_predicate_to_proof_atomic(
                    &self.variables,
                    !predicate,
                )],
            });
        }

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

        let inferences = self.explain_current_conflict(&mut nogood_stack, conflict);

        // Log the empty clause to the proof.
        self.output_proof.push(ProofStage {
            inferences,
            constraint_id: self.state.new_constraint_tag().into(),
            premises: vec![],
        });

        Ok((Conclusion::Unsat, nogood_stack))
    }

    /// Explain the current conflict and write to the output proof.
    fn explain_current_conflict(
        &mut self,
        nogood_stack: &mut DeductionStack,
        conflict: Conflict,
    ) -> Vec<Inference<String, i32, Arc<str>>> {
        assert_eq!(self.to_process_heap.num_nonremoved_elements(), 0);

        // The implementation of key-value heap is buggy, but this `clear` seems to reset
        // the internals properly.
        self.to_process_heap.clear();

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

                inferences.push(Some(Inference {
                    constraint_id: self.state.new_constraint_tag().into(),
                    premises: convert_predicates_to_proof_atomic(&self.variables, &conjunction),
                    consequent: None,
                    generated_by: Some(generated_by.into()),
                    label: Some(label),
                }));

                mark_stack_entry(nogood_stack, inference_code);

                conjunction
            }
            Conflict::EmptyDomain(empty_domain_confict) => {
                let mut predicates_to_explain = vec![];
                empty_domain_confict.get_reason(
                    &mut self.state,
                    &mut predicates_to_explain,
                    CurrentNogood::empty(),
                );

                if let Some(inference_code) = empty_domain_confict.trigger_inference_code {
                    let generated_by = inference_code.tag();
                    let label = inference_code.label();

                    inferences.push(Some(Inference {
                        constraint_id: self.state.new_constraint_tag().into(),
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

                    mark_stack_entry(nogood_stack, inference_code);
                }

                predicates_to_explain.push(!empty_domain_confict.trigger_predicate);

                predicates_to_explain.into()
            }
        };

        // Then we add the conflict to the queue of predicates that need to be explained
        // through inferences.
        for predicate in predicates_to_explain {
            self.add_predicate_to_explain(predicate);
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

        // For every predicate in the queue, we will introduce appropriate inferences into
        // the proof.
        while let Some(predicate_id) = self.to_process_heap.pop_max() {
            let predicate = self.predicate_ids.get_predicate(predicate_id);

            // The predicate is either propagated or an initial bound. If it is an
            // initial bound, we dispatch that here.
            if self.state.is_trivially_assigned(predicate) {
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
                mark_stack_entry(nogood_stack, inference_code.clone());

                let label = inference_code.label();
                inferences.push(Some(Inference {
                    constraint_id: self.state.new_constraint_tag().into(),
                    premises: convert_predicates_to_proof_atomic(&self.variables, &reason_buffer),
                    consequent: Some(convert_predicate_to_proof_atomic(
                        &self.variables,
                        predicate,
                    )),
                    generated_by: Some(inference_code.tag().into()),
                    label: Some(label),
                }));
            }

            for predicate in reason_buffer.drain(..) {
                self.add_predicate_to_explain(predicate);
            }
        }

        // Reverse the inference sequence, since analysis goes from the conflict to the
        // leafs rather than from the assumptions to the conflict. Also, filter out the
        // tombstone values.
        inferences.into_iter().rev().flatten().collect()
    }

    /// Add a predicate that still has to be explained.
    fn add_predicate_to_explain(&mut self, predicate: Predicate) {
        assert_eq!(
            self.state.truth_value(predicate),
            Some(true),
            "predicate {predicate:?} does not evaluate to true",
        );

        // The first time we encounter the predicate, we initialise its value in the
        // heap.
        //
        // Note that if the predicate is already in the heap, no action needs to be
        // taken. It can happen that a predicate is returned
        // multiple times as a reason for other predicates.

        // Here we manually adjust the size of the heap to accommodate new elements.
        let predicate_id = self.predicate_ids.get_id(predicate);
        while self.to_process_heap.len() <= predicate_id.index() {
            let next_id = PredicateId::create_from_index(self.to_process_heap.len());
            self.to_process_heap.grow(next_id, 0);
            self.to_process_heap.delete_key(next_id);
        }

        // Then we check whether the predicate was not already present in the heap, if
        // this is not the case then we insert it
        if !self.to_process_heap.is_key_present(predicate_id)
            && *self.to_process_heap.get_value(predicate_id) == 0
        {
            let trail_position = self.state.trail_position(predicate).unwrap();

            // The goal is to traverse predicate in reverse order of the trail.
            //
            // However some predicates may share the trail position. For example, if a
            // predicate that was posted to trail resulted in
            // some other predicates being true, then all
            // these predicates would have the same trail position.
            //
            // When considering the predicates in reverse order of the trail, the
            // implicitly set predicates are posted after the
            // explicitly set one, but they all have the same
            // trail position.
            //
            // To remedy this, we make a tie-breaking scheme to prioritise implied
            // predicates over explicit predicates. This is done
            // by assigning explicitly set predicates the
            // value `2 * trail_position`, whereas implied predicates get `2 *
            // trail_position + 1`.
            let heap_value = if self.state.is_on_trail(predicate) {
                trail_position * 2
            } else {
                trail_position * 2 + 1
            };

            // We restore the key and since we know that the value is 0, we can safely
            // increment with `heap_value`
            self.to_process_heap.restore_key(predicate_id);
            self.to_process_heap
                .increment(predicate_id, heap_value as u32);

            pumpkin_assert_moderate!(
                *self.to_process_heap.get_value(predicate_id) == heap_value.try_into().unwrap(),
                "The value in the heap should be the same as was added"
            )
        }
    }

    fn post_assumptions(&mut self, predicates: &[Predicate]) -> Result<(), Conflict> {
        for predicate in predicates.iter() {
            let _ = self.state.post(*predicate)?;
        }

        Ok(())
    }
}

/// Given a [`DeductionStack`], mark the constraint indicated by the inference code as used.
fn mark_stack_entry(stack: &mut DeductionStack, inference_code: InferenceCode) {
    let used_constraint_tag = inference_code.tag();

    trace!("Marking constraint {}", NonZero::from(used_constraint_tag));

    let stack_entry = &mut stack[used_constraint_tag];
    if let Some(posted_deduction) = stack_entry {
        posted_deduction.marked = true;
    }
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
}
