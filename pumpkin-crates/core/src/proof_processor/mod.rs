//! The proof processing facilities to turn a proof scaffold into a full DRCP proof.
//!
//! In future this should be moved to a separate crate, however, currently this depends on features
//! that are not (and should not be) in the public API of the `pumpkin-core` crate.

use std::collections::HashMap;
use std::fmt::Display;
use std::io::BufRead;
use std::io::Write;
use std::num::NonZero;
use std::rc::Rc;
use std::sync::Arc;

use drcp_format::reader::ProofReader;
use drcp_format::writer::ProofWriter;
use drcp_format::Conclusion;
use drcp_format::ConstraintId;
use drcp_format::Deduction;
use drcp_format::Inference;
use drcp_format::IntAtomic;
use drcp_format::IntComparison;
use drcp_format::Step;
use log::debug;
use log::info;
use log::trace;

use crate::basic_types::PredicateId;
use crate::basic_types::PredicateIdGenerator;
use crate::basic_types::PropagatorConflict;
use crate::basic_types::StoredConflictInfo;
use crate::containers::KeyValueHeap;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::conflict_analysis::ConflictAnalysisContext;
use crate::engine::propagation::CurrentNogood;
use crate::engine::ConstraintSatisfactionSolver;
use crate::engine::VariableNames;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::proof::ProofLog;
use crate::propagators::nogoods::NogoodHandle;
use crate::pumpkin_assert_moderate;
use crate::Solver;

#[derive(Debug)]
pub struct ProofProcessor {
    solver: ConstraintSatisfactionSolver,
    /// Contains the proof that will be written to the output. Key note: this is in reverse order
    /// due to the backward trimming.
    output_proof: Vec<ProofStage>,

    /// A mapping from predicates to predicate IDs.
    predicate_ids: PredicateIdGenerator,
    /// Heap containing the predicates which still need to be processed; sorted non-increasing
    /// based on trail-index where implied predicates are processed first.
    to_process_heap: KeyValueHeap<PredicateId, u32>,
    /// A buffer to put reasons into.
    reason_buffer: Vec<Predicate>,
}

#[derive(Debug)]
struct ProofStage {
    inferences: Vec<Inference<String, i32, Arc<str>>>,
    constraint_id: ConstraintId,
    premises: Vec<IntAtomic<String, i32>>,
}

#[derive(Debug, thiserror::Error)]
pub enum ProofProcessError {
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

    #[error("deduction {0} contains inconsistent premises")]
    InconsistentDeduction(ConstraintId),
}

impl From<Solver> for ProofProcessor {
    fn from(value: Solver) -> Self {
        ProofProcessor {
            solver: value.satisfaction_solver,
            output_proof: vec![],
            predicate_ids: PredicateIdGenerator::default(),
            to_process_heap: KeyValueHeap::default(),
            reason_buffer: vec![],
        }
    }
}

/// A deduction that was posted to the solver.
#[derive(Clone, Debug)]
struct PostedDeduction {
    handle: NogoodHandle,
    marked: bool,
    deduction: Deduction<Rc<str>, i32>,
}

type DeductionStack = KeyedVec<ConstraintTag, Option<PostedDeduction>>;

impl ProofProcessor {
    pub fn process<R: BufRead, W: Write>(
        mut self,
        proof_reader: ProofReader<R, i32>,
        mut proof_writer: ProofWriter<W, i32>,
    ) -> Result<(), ProofProcessError> {
        // First, we will add all the deductions to the satisfaction solver.
        //
        // We get the conclusion of the proof, as well as the stack of deductions. The marking
        // based on the conclusion is done in the stack, so we need to look for the last marked
        // constraint and process from there.
        let (conclusion, mut nogood_stack) = self.initialise_solver(proof_reader)?;

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

            debug!("Processing deduction {}", NonZero::from(tag));

            // Remove the nogood from the solver as it is no-longer needed.
            let predicates = self.solver.remove_nogood(posted_deduction.handle);

            // If the deduction was never used, then we don't have to consider it
            // further.
            if !posted_deduction.marked {
                trace!("Deduction {} is not marked", NonZero::from(tag));
                continue;
            }

            // Now we add the predicates in the nogood as assumptions to kick-start the
            // propagation.
            for predicate in predicates.iter() {
                let assumption_result = self.solver.post_predicate(*predicate);

                if assumption_result.is_err() {
                    self.declare_assumption_conflict(*predicate)?;
                    break;
                }
            }

            if !self.solver.state.is_inconsistent() {
                self.solver.propagate();
            }

            if !self.solver.state.is_inconsistent() {
                return Err(ProofProcessError::DeductionDoesNotConflict(tag.into()));
            }

            let inferences = self.explain_current_conflict(&mut nogood_stack);
            self.output_proof.push(ProofStage {
                inferences,
                constraint_id: tag.into(),
                premises: predicates
                    .iter()
                    .map(|premise| convert_predicate_to_proof_atomic(&self.solver, *premise))
                    .collect(),
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

    /// Initialise the solver with the nogoods from the proof.
    ///
    /// Any inferences that are encountered are ignored, since we will introduce our own
    /// inferences.
    fn initialise_solver<R: BufRead>(
        &mut self,
        mut proof_reader: ProofReader<R, i32>,
    ) -> Result<(Conclusion<Rc<str>, i32>, DeductionStack), ProofProcessError> {
        info!("Setting up solver with deductions");

        let mut nogood_stack = KeyedVec::new();

        if self.solver.state.is_inconsistent() {
            let empty_nogood_tag = self.solver.new_constraint_tag();
            nogood_stack.accomodate(empty_nogood_tag, None);

            // If the solver is alrady in an inconsistent state, then we explain that conflict,
            // write the empty nogood, and return.
            let inferences = self.explain_current_conflict(&mut nogood_stack);

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
                    let predicate = convert_proof_atomic_to_predicate(&self.solver, &bound)?;
                    info!("Found dual bound conclusion");
                    debug!("bound = {}", predicate.display(&self.solver.variable_names));

                    // If the claimed bound is not true given the current assignment, then the
                    // conclusion does not follow by propagation.
                    if self.solver.assignments.evaluate_predicate(predicate) != Some(true) {
                        return Err(ProofProcessError::InvalidConclusion);
                    }

                    let mut reason_buffer = std::mem::take(&mut self.reason_buffer);
                    let inference_code = self.get_reason(predicate, &mut reason_buffer);

                    // We do not use the function to mark the constraint in the nogood stack. It
                    // could happen that the conclusion is a root bound, but the proof does not
                    // contain a nogood asserting the root bound (an inference is not enough, we
                    // explicitly want a deduction that makes the conclusion true).
                    trace!("Marking reason for dual bound");

                    let used_constraint_tag =
                        self.solver.get_constraint_tag_for(inference_code.unwrap());
                    trace!("Marking constraint {}", NonZero::from(used_constraint_tag));
                    let Some(stack_entry) = nogood_stack
                        .get_mut(&used_constraint_tag)
                        .map(|opt| opt.as_mut())
                    else {
                        return Err(ProofProcessError::InvalidConclusion);
                    };

                    if let Some(posted_deduction) = stack_entry {
                        posted_deduction.marked = true;
                    }

                    self.reason_buffer = reason_buffer;
                    return Ok((Conclusion::DualBound(bound), nogood_stack));
                }

                Step::Conclusion(Conclusion::Unsat) => {
                    return Err(ProofProcessError::DeductionsDoNotConflict);
                }

                // We ignore inferences when doing proof processing. We will introduce our own
                // inferences regardless of what is in the input.
                Step::Inference(_) => continue,
            };

            // Get the constraint tag for this new constraint. It should match up exactly with the
            // constraint tag that is indicated in the proof.
            let constraint_tag = self.solver.new_constraint_tag();
            assert_eq!(ConstraintId::from(constraint_tag), deduction.constraint_id);

            // Convert the deduction to the solver's representation.
            let nogood = Nogood(
                deduction
                    .premises
                    .iter()
                    .map(|premise| convert_proof_atomic_to_predicate(&self.solver, premise))
                    .collect::<Result<Vec<_>, ProofProcessError>>()?,
            );

            debug!("Adding deduction {}", deduction.constraint_id);
            debug!("    {}", nogood.display(&self.solver.variable_names));

            match self
                .solver
                .add_removable_nogood(nogood.0.clone(), constraint_tag)
            {
                // TODO: For now we do not do anything with the fact that the nogood may have been
                // processed. If there are redundent predicates, they will also show up in the
                // output proof.
                Ok(handle) => {
                    nogood_stack.accomodate(constraint_tag, None);
                    nogood_stack[constraint_tag] = Some(PostedDeduction {
                        handle,
                        marked: false,
                        deduction,
                    });
                }

                // If we reach inconsistency through propagation alone, then it must mean that the
                // proof is a proof of unsatisfiability and not a dual bound proof.
                Err(handle) => return self.repair_solver(handle, deduction, nogood_stack),
            }
        }
    }

    /// Takes a solver that is in an inconsistent state because we have a proof of
    /// unsatisfiability, and removes the last added constraint. We mark all the deductions in the
    /// deduction stack that contributed to the conflict, and log the empty nogood step.
    fn repair_solver(
        &mut self,
        unsat_triggering_nogood: NogoodHandle,
        deduction: Deduction<Rc<str>, i32>,
        mut nogood_stack: DeductionStack,
    ) -> Result<(Conclusion<Rc<str>, i32>, DeductionStack), ProofProcessError> {
        let tag = ConstraintTag::from_non_zero(deduction.constraint_id);

        // Add the nogood that triggered the conflict to the nogood stack. Also mark it,
        // because obviously it is used.
        nogood_stack.accomodate(tag, None);
        nogood_stack[tag] = Some(PostedDeduction {
            handle: unsat_triggering_nogood,
            marked: true,
            deduction,
        });

        let inferences = self.explain_current_conflict(&mut nogood_stack);

        // Log the empty clause to the proof.
        self.output_proof.push(ProofStage {
            inferences,
            constraint_id: self.solver.new_constraint_tag().into(),
            premises: vec![],
        });

        Ok((Conclusion::Unsat, nogood_stack))
    }

    /// Explain the current conflict and write to the output proof.
    fn explain_current_conflict(
        &mut self,
        nogood_stack: &mut DeductionStack,
    ) -> Vec<Inference<String, i32, Arc<str>>> {
        assert!(self.to_process_heap.is_empty());

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
        let mut initial_bound_indices = HashMap::new();

        let conflict_info = self.solver.state.get_conflict_info();

        let (predicates_to_explain, conflict_inference_code) = match conflict_info {
            StoredConflictInfo::Propagator(PropagatorConflict {
                conjunction,
                inference_code,
            }) => {
                let generated_by = self.solver.get_constraint_tag_for(inference_code);
                let label = self.solver.get_inference_label_for(inference_code);
                inferences.push(Some(Inference {
                    constraint_id: self.solver.new_constraint_tag().into(),
                    premises: convert_predicates_to_proof_atomic(&self.solver, &conjunction),
                    consequent: None,
                    generated_by: Some(generated_by.into()),
                    label: Some(label),
                }));

                (conjunction, inference_code)
            }
            StoredConflictInfo::EmptyDomain {
                conflict_nogood,
                inference_code,
                last_inference: (last_inference_explanation, last_propagated_predicate),
            } => {
                let inference_code =
                    inference_code.expect("is only none in the case of inconsistent assumptions");

                let generated_by = self.solver.get_constraint_tag_for(inference_code);
                let label = self.solver.get_inference_label_for(inference_code);
                inferences.push(Some(Inference {
                    constraint_id: self.solver.new_constraint_tag().into(),
                    premises: convert_predicates_to_proof_atomic(
                        &self.solver,
                        &last_inference_explanation,
                    ),
                    consequent: Some(convert_predicate_to_proof_atomic(
                        &self.solver,
                        last_propagated_predicate,
                    )),
                    generated_by: Some(generated_by.into()),
                    label: Some(label),
                }));

                (conflict_nogood, inference_code)
            }
            StoredConflictInfo::RootLevelConflict(_) => {
                panic!("This should not happen.")
            }
        };

        // We mark and write the very last propagation that led to the conflict.
        mark_stack_entry(&self.solver, nogood_stack, conflict_inference_code);

        // Then we add the conflict to the queue of predicates that need to be explained
        // through inferences.
        for predicate in predicates_to_explain {
            self.add_predicate_to_explain(predicate);
        }

        // We take ownership of the reason buffer here, as it is easier with the borrowing
        // rules. At the end of the function we give ownership back to `self`.
        let mut reason_buffer = std::mem::take(&mut self.reason_buffer);

        // For every predicate in the queue, we will introduce appropriate inferences into
        // the proof.
        while let Some(predicate_id) = self.to_process_heap.pop_max() {
            let predicate = self.predicate_ids.get_predicate(predicate_id);

            // The predicate is either propagated or an initial bound. If it is an
            // initial bound, we dispatch that here.
            if self.solver.assignments.is_initial_bound(predicate) {
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
                        let constraint_id = self.solver.new_constraint_tag().into();
                        Some(Inference {
                            constraint_id,
                            premises: vec![],
                            consequent: Some(convert_predicate_to_proof_atomic(
                                &self.solver,
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
            let inference_code = self.get_reason(predicate, &mut reason_buffer);

            // The inference code can be `None` if the reason is constructed to explain an implied
            // predicate. Those inferences do not need to be in the proof, so we only create an
            // inference for propagations by constraints.
            if let Some(inference_code) = inference_code {
                mark_stack_entry(&self.solver, nogood_stack, inference_code);

                let label = self.solver.get_inference_label_for(inference_code);
                inferences.push(Some(Inference {
                    constraint_id: self.solver.new_constraint_tag().into(),
                    premises: convert_predicates_to_proof_atomic(&self.solver, &reason_buffer),
                    consequent: Some(convert_predicate_to_proof_atomic(&self.solver, predicate)),
                    generated_by: Some(self.solver.get_constraint_tag_for(inference_code).into()),
                    label: Some(label),
                }));
            }

            for predicate in reason_buffer.drain(..) {
                self.add_predicate_to_explain(predicate);
            }
        }

        // Hand back ownership of the reason buffer to `self`.
        self.reason_buffer = reason_buffer;

        // Reverse the inference sequence, since analysis goes from the conflict to the
        // leafs rather than from the assumptions to the conflict. Also, filter out the
        // tombstone values.
        inferences.into_iter().rev().flatten().collect()
    }

    /// Add a predicate that still has to be explained.
    fn add_predicate_to_explain(&mut self, predicate: Predicate) {
        assert_eq!(
            self.solver.assignments.evaluate_predicate(predicate),
            Some(true),
            "predicate {} ({predicate:?}) does not evaluate to true",
            predicate.display(&self.solver.variable_names),
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
            let next_id = PredicateId {
                id: self.to_process_heap.len() as u32,
            };
            self.to_process_heap.grow(next_id, 0);
            self.to_process_heap.delete_key(next_id);
        }

        // Then we check whether the predicate was not already present in the heap, if
        // this is not the case then we insert it
        if !self.to_process_heap.is_key_present(predicate_id)
            && *self.to_process_heap.get_value(predicate_id) == 0
        {
            let trail_position = self
                .solver
                .assignments
                .get_trail_position(&predicate)
                .unwrap();

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
            let heap_value = if self.solver.assignments.trail[trail_position].predicate == predicate
            {
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

    /// Setup the solver such that it is ready for conflict analysis. The given predicate should
    /// conflict with the current state, which means the negation was already true.
    fn declare_assumption_conflict(
        &mut self,
        predicate: Predicate,
    ) -> Result<(), ProofProcessError> {
        let opposite = !predicate;
        assert_eq!(
            self.solver.assignments.evaluate_predicate(opposite),
            Some(true)
        );

        let mut reason_buffer = std::mem::take(&mut self.reason_buffer);
        let inference_code = self.get_reason(predicate, &mut reason_buffer);

        // The inference code can be `None` if the reason is constructed to explain an implied
        // predicate. Those inferences do not need to be in the proof, so we only create an
        // inference for propagations by constraints.
        if let Some(inference_code) = inference_code {
            let mut conflict_nogood = reason_buffer.clone();
            conflict_nogood.push(predicate);

            self.solver
                .state
                .declare_conflict(StoredConflictInfo::EmptyDomain {
                    inference_code: Some(inference_code),
                    conflict_nogood: conflict_nogood.into(),
                    last_inference: (self.reason_buffer.clone().into(), opposite),
                });
        }

        reason_buffer.clear();
        self.reason_buffer = reason_buffer;

        Ok(())
    }

    /// Get the reason of the given predicate being true.
    fn get_reason(
        &mut self,
        predicate: Predicate,
        buffer: &mut (impl Extend<Predicate> + AsRef<[Predicate]>),
    ) -> Option<InferenceCode> {
        assert!(buffer.as_ref().is_empty());
        ConflictAnalysisContext::get_propagation_reason(
            predicate,
            &self.solver.assignments,
            CurrentNogood::new(&self.to_process_heap, &[], &self.predicate_ids),
            &mut self.solver.reason_store,
            &mut self.solver.propagators,
            &mut ProofLog::default(),
            &self.solver.unit_nogood_inference_codes,
            buffer,
            &mut self.solver.notification_engine,
            &self.solver.variable_names,
        )
    }
}

/// Given a [`DeductionStack`], mark the constraint indicated by the inference code as used.
fn mark_stack_entry(
    solver: &ConstraintSatisfactionSolver,
    stack: &mut DeductionStack,
    inference_code: InferenceCode,
) {
    let used_constraint_tag = solver.get_constraint_tag_for(inference_code);
    trace!("Marking constraint {}", NonZero::from(used_constraint_tag));
    let stack_entry = stack[used_constraint_tag].as_mut();

    if let Some(posted_deduction) = stack_entry {
        posted_deduction.marked = true;
    }
}

/// Converts a slice of predicates to atomic constraints for the proof log.
fn convert_predicates_to_proof_atomic(
    solver: &ConstraintSatisfactionSolver,
    predicates: &[Predicate],
) -> Vec<IntAtomic<String, i32>> {
    predicates
        .iter()
        .map(|predicate| convert_predicate_to_proof_atomic(solver, *predicate))
        .collect()
}

/// Converts an atomic from the proofs representation to a solver [`Predicate`].
fn convert_predicate_to_proof_atomic(
    solver: &ConstraintSatisfactionSolver,
    predicate: Predicate,
) -> IntAtomic<String, i32> {
    let name = solver
        .variable_names
        .get_int_name(predicate.get_domain())
        .unwrap()
        .to_owned();

    let comparison = match predicate.get_predicate_type() {
        crate::predicates::PredicateType::LowerBound => IntComparison::GreaterEqual,
        crate::predicates::PredicateType::UpperBound => IntComparison::LessEqual,
        crate::predicates::PredicateType::NotEqual => IntComparison::NotEqual,
        crate::predicates::PredicateType::Equal => IntComparison::Equal,
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
    solver: &ConstraintSatisfactionSolver,
    atomic: &IntAtomic<Rc<str>, i32>,
) -> Result<Predicate, ProofProcessError> {
    let domain_id = solver
        .variable_names
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

/// A wrapper type of a nogood.
///
/// For now this is used to provide a display capability for easy logging/debugging.
struct Nogood(Vec<Predicate>);

impl Nogood {
    fn display<'this: 'names, 'names>(
        &'this self,
        variable_names: &'names VariableNames,
    ) -> impl Display + 'names {
        NogoodDisplay {
            nogood: self,
            names: variable_names,
        }
    }
}

struct NogoodDisplay<'nogood, 'names> {
    nogood: &'nogood Nogood,
    names: &'names VariableNames,
}

impl Display for NogoodDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, predicate) in self.nogood.0.iter().enumerate() {
            if idx > 0 {
                write!(f, " ")?;
            }

            write!(f, "{}", predicate.display(self.names))?;
        }

        write!(f, " => false")
    }
}
