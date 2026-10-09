//! The proof processing facilities to turn a proof scaffold into a full DRCP proof.

use std::collections::HashMap;
use std::io::BufRead;
use std::io::Write;
use std::num::NonZero;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

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
use pumpkin_core::containers::HashSet;
use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::proof::ProofLog;
use pumpkin_core::propagation::Priority;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::CurrentNogood;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::state::PropagatorHandle;
use pumpkin_core::state::State;

use crate::deduction_database::DatabaseMode;
use crate::deduction_database::DeductionDatabase;
use crate::deduction_database::DeductionDatabaseConstructor;
use crate::deduction_database::NogoodIndex;
use crate::deduction_propagator::DeductionPropagationMode;
use crate::deduction_propagator::DeductionPropagator;
use crate::deduction_propagator::DeductionPropagatorConstructor;
use crate::deduction_propagator::MARKED_PROPAGATION_PRIORITY;
use crate::deduction_propagator::UNMARKED_CONFLICT_PRIORITY;
use crate::deduction_propagator::UNMARKED_UNIT_PROPAGATION_PRIORITY;
use crate::predicate_heap::PredicateHeap;
use crate::variables::Variables;

/// The label for a step that reads a bound off the initial domains.
const INITIAL_DOMAIN_LABEL: &str = "initial_domain";

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

    /// Whether the proof is written in the block format, through the proof log.
    blocks: bool,

    /// How the scaffold's deductions are held in the solver.
    deduction_store: DeductionStoreKind,

    /// The databases, once created. `None` while deductions are posted one propagator each.
    databases: Option<Databases>,

    /// The predicates the databases are registered on, with their negations.
    registered_predicates: HashSet<Predicate>,
    /// The number of deductions added to the databases, which numbers the next one.
    registered_deductions: u32,

    /// Whether the steps of the head that add a constraint no kept inference cites are left out
    /// of a proof in the block format.
    drop_unused_steps: bool,

    /// The facts propagated at the root, by the model's constraints alone, that a derivation
    /// cites, each with the tag of the unit nogood the proof derives for it once, before the
    /// scaffold's nogoods. `None` when the facts are explained wherever they are used, as for a
    /// proof in the DRCP format.
    root_lemmas: Option<HashMap<Predicate, RootLemma>>,

    /// The facts at the root that the derivation being explained rests on, with their trail
    /// positions.
    cited_root_facts: Vec<(usize, Predicate)>,
    /// The number of the derivation being explained, which tells whether it cites a fact
    /// already.
    derivation_number: u64,
}

/// The unit nogood a proof derives for a fact at the root.
#[derive(Clone, Copy, Debug)]
struct RootLemma {
    tag: ConstraintTag,
    trail_position: usize,
    /// The number of the last derivation that cites the fact.
    cited_by: u64,
}

/// How the scaffold's deductions are held in the solver.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DeductionStoreKind {
    /// Two propagators per deduction, one detecting conflicts and one propagating.
    PerDeduction,
    /// Three databases: marked deductions, unmarked conflict detection, unmarked propagation.
    Database,
    /// Two databases: marked deductions, and unmarked deductions doing both at one priority.
    DatabaseMerged,
}

/// The handles of the deduction databases.
#[derive(Clone, Copy, Debug)]
struct Databases {
    marked: PropagatorHandle<DeductionDatabase>,
    unmarked_conflict: PropagatorHandle<DeductionDatabase>,
    /// `None` when unmarked conflict detection and propagation share a database.
    unmarked_unit: Option<PropagatorHandle<DeductionDatabase>>,
}

/// Which two predicates of a nogood to watch, and whether fewer than two were unsatisfied, so
/// the nogood may act before anything else changes.
///
/// Watching unsatisfied predicates first and otherwise the most recently satisfied ones is what
/// keeps a watch valid across backtracking: a predicate satisfied later is undone no later than
/// one satisfied earlier.
fn choose_watches(state: &State, predicates: &[Predicate]) -> ([usize; 2], bool) {
    if predicates.len() < 2 {
        return ([0, 0], true);
    }

    let mut chosen: Vec<usize> = predicates
        .iter()
        .enumerate()
        .filter(|&(_, &predicate)| state.truth_value(predicate) != Some(true))
        .map(|(position, _)| position)
        .take(2)
        .collect();

    let immediate = chosen.len() < 2;

    if immediate {
        let mut satisfied: Vec<(usize, usize)> = predicates
            .iter()
            .enumerate()
            .filter(|(position, _)| !chosen.contains(position))
            .map(|(position, &predicate)| (state.trail_position(predicate).unwrap_or(0), position))
            .collect();
        satisfied.sort_unstable_by_key(|a| std::cmp::Reverse(a.0));
        chosen.extend(satisfied.into_iter().map(|(_, position)| position));
    }

    ([chosen[0], chosen[1]], immediate)
}

/// A single proof stage for the proof.
#[derive(Debug)]
struct ProofStage {
    /// The tag of the constraint to introduce, which the inferences citing it name.
    tag: ConstraintTag,
    /// The derivation, from the conflict back, which the output format numbers and orders.
    steps: Vec<ExplanationStep>,
    /// The facts at the root the derivation rests on, each inferred from its unit nogood
    /// before the steps.
    root_facts: Vec<Predicate>,
    /// The nogood: the predicates to assume `true` such that a conflict is derived through the
    /// inferences.
    premises: Vec<Predicate>,
}

/// One step of the explanation of a conflict, in the order conflict analysis reaches it.
#[derive(Debug)]
enum ExplanationStep {
    /// An inference of a propagator; without a consequent, a conflict.
    Inference {
        inference_code: InferenceCode,
        premises: Vec<Predicate>,
        consequent: Option<Predicate>,
    },
    /// The initial domains imply the predicate.
    InitialDomain(Predicate),
}

/// The conclusion of a scaffold, over the names of its variables.
type ScaffoldConclusion = Conclusion<Rc<str>, i32>;

/// The sink a processed proof is written to, which decides the format.
#[derive(Debug)]
#[allow(clippy::large_enum_variant, reason = "there is one per run")]
pub(crate) enum ProofOutput<W> {
    Drcp(ProofWriter<W, i32>),
    /// The proof log the compiler filled with the steps that state the views and the rules of
    /// the constraints, which writes the proof in the block format.
    Blocks(ProofLog),
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum ProofProcessError {
    #[error("failed to read proof: {0}")]
    Read(#[from] drcp_format::reader::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error(
        "undefined variable '{0}': the scaffold names a variable that is neither in the model nor \
         created by the preprocessing the preamble states"
    )]
    UndefinedVariable(String),

    #[error(
        "cannot derive the nogood {0} of the scaffold, {1}: propagating the model's constraints \
         and the nogoods kept after it does not refute it"
    )]
    DeductionDoesNotConflict(ConstraintId, String),

    #[error("all deductions together do not propagate to conflict")]
    DeductionsDoNotConflict,

    #[error("conclusion is not supported by any deduction")]
    InvalidConclusion,
}

/// A deduction that was posted to the solver.
#[derive(Clone, Debug)]
struct PostedDeduction {
    predicates: Rc<[Predicate]>,
    store: PostedIn,
    marked: bool,
}

/// Where a posted deduction lives.
#[derive(Clone, Debug)]
enum PostedIn {
    Propagators {
        conflict_detection_handle: PropagatorHandle<DeductionPropagator>,
        unit_prop_handle: PropagatorHandle<DeductionPropagator>,
    },
    Database(NogoodIndex),
}

impl PostedDeduction {
    /// Deactivates the propagators associated with this `PostedDeduction`.
    fn deactivate_propagators(&self, state: &mut State) {
        if let PostedIn::Propagators {
            conflict_detection_handle,
            unit_prop_handle,
        } = &self.store
        {
            state
                .get_propagator_mut(*conflict_detection_handle)
                .expect("All handles are valid")
                .deactivate();
            state
                .get_propagator_mut(*unit_prop_handle)
                .expect("All handles are valid")
                .deactivate();
        }
    }

    /// Updates the priority of both propagators associated with this `PostedDeduction`.
    fn update_propagators_priority(&self, state: &mut State, new_priority: Priority) {
        if let PostedIn::Propagators {
            conflict_detection_handle,
            unit_prop_handle,
        } = &self.store
        {
            state
                .get_propagator_mut(*conflict_detection_handle)
                .expect("All handles are valid")
                .set_priority(new_priority);
            state
                .get_propagator_mut(*unit_prop_handle)
                .expect("All handles are valid")
                .set_priority(new_priority);
        }
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
            blocks: false,
            deduction_store: DeductionStoreKind::DatabaseMerged,
            databases: None,
            registered_predicates: HashSet::default(),
            registered_deductions: 0,
            drop_unused_steps: false,
            root_lemmas: None,
            cited_root_facts: vec![],
            derivation_number: 1,
        }
    }

    /// Leave out of a proof in the block format the steps of the head, the symmetry and
    /// dominance breaking steps, that add a constraint no kept inference cites.
    pub(crate) fn set_drop_unused_steps(&mut self, drop_unused_steps: bool) {
        self.drop_unused_steps = drop_unused_steps;
    }

    /// Process the given proof and write the result to the proof writer.
    ///
    /// To process a proof, only the deductions from `proof_reader` are considered. Deductions that
    /// are redundant for proving the conclusion (determined by propagation) are removed. For the
    /// deductions that remain, inferences are introduced to complete the proof.
    ///
    /// Note that if the input proof contains inferences, they are ignored.
    pub(crate) fn set_deduction_store(&mut self, store: DeductionStoreKind) {
        self.deduction_store = store;
    }

    pub(crate) fn process<R: BufRead, W: Write>(
        mut self,
        proof_reader: ProofReader<R, i32>,
        mut output: ProofOutput<W>,
    ) -> Result<(), ProofProcessError> {
        self.blocks = matches!(output, ProofOutput::Blocks(_));
        if self.blocks {
            self.root_lemmas = Some(HashMap::new());
        }
        // First, we will add all the deductions to the state.
        //
        // We get the conclusion of the proof, as well as the stack of deductions. The marking
        // based on the conclusion is done in the stack, so we need to look for the last marked
        // constraint and process from there. A scaffold without a conclusion has every deduction
        // marked, so that each is derived.
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

        // Instrumentation: where the time in the trimming loop actually goes. The deciles are
        // over progress through the scaffold, which tells expensive-propagation apart from a
        // cost that grows with the number of deductions still posted.
        let loop_start = Instant::now();
        let total_stages = nogood_stack.len().max(1);
        let mut time_propagate = Duration::ZERO;
        let mut calls_propagate = 0_u64;
        let mut time_explain = Duration::ZERO;
        let mut deciles = [(Duration::ZERO, 0_u64); 10];

        // Whether the checkpoint of the last derivation's assumptions is still open; it is
        // closed with the deduction's own, in one restore, since a restore costs time for every
        // propagator.
        let mut assumptions_open = false;

        while let Some((tag, maybe_deduction)) = nogood_stack.pop() {
            let remaining = nogood_stack.len();
            // Extract the nogood handle and whether the deduction is marked.
            let Some(posted_deduction) = maybe_deduction else {
                continue;
            };

            let new_checkpoint = self.state.get_checkpoint() - 1 - usize::from(assumptions_open);
            let _ = self.state.restore_to(new_checkpoint);
            assumptions_open = false;

            self.deactivate_deduction(&posted_deduction);

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
            for &predicate in posted_deduction.predicates.iter() {
                trace!(
                    "  assumption {predicate:?} currently {:?}",
                    self.state.truth_value(predicate)
                );
            }

            // The assumptions get a checkpoint of their own, so that nothing they imply is taken
            // for a fact at the root.
            self.state.new_checkpoint();
            assumptions_open = true;

            let propagate_start = Instant::now();
            let possible_result = self
                .post_assumptions(&posted_deduction.predicates)
                .and_then(|_| self.state.propagate_to_fixed_point());
            trace!("  propagation result: {:?}", possible_result.as_ref().err());
            let propagate_elapsed = propagate_start.elapsed();

            time_propagate += propagate_elapsed;
            calls_propagate += 1;

            let bucket = ((total_stages - remaining) * 10 / total_stages).min(9);
            deciles[bucket].0 += propagate_elapsed;
            deciles[bucket].1 += 1;

            let Err(conflict) = possible_result else {
                return Err(ProofProcessError::DeductionDoesNotConflict(
                    tag.into(),
                    describe_nogood(&self.variables, &posted_deduction.predicates),
                ));
            };

            let explain_start = Instant::now();
            let steps = self.explain_conflict(&mut nogood_stack, conflict);
            num_inferences += steps.len() + self.cited_root_facts.len();
            time_explain += explain_start.elapsed();

            let root_facts = self.take_cited_root_facts();

            self.output_proof.push(ProofStage {
                steps,
                root_facts,
                tag,
                premises: posted_deduction.predicates.to_vec(),
            });
        }

        if assumptions_open {
            let _ = self.state.restore_to(self.state.get_checkpoint() - 1);
        }

        let lemma_stages = self.derive_root_lemmas();
        num_inferences += lemma_stages
            .iter()
            .map(|stage| stage.steps.len() + stage.root_facts.len())
            .sum::<usize>();
        info!(
            "Deriving {} facts at the root once, as unit nogoods",
            lemma_stages.len()
        );
        // The stages are written from the last to the first, so these come first.
        self.output_proof.extend(lemma_stages);

        let loop_elapsed = loop_start.elapsed();
        {
            use std::sync::atomic::Ordering as AtomicOrdering;

            use crate::deduction_propagator::PROPAGATE_CALLS;
            use crate::deduction_propagator::PROPAGATE_INACTIVE;
            use crate::deduction_propagator::PROPAGATE_SHORTCUTS;

            let calls = PROPAGATE_CALLS.load(AtomicOrdering::Relaxed);

            use crate::deduction_database::DATABASE_ENQUEUED;
            use crate::deduction_database::DATABASE_NOTIFICATIONS;
            use crate::deduction_database::DATABASE_WATCHERS_VISITED;

            info!(
                "Deduction databases: {} notifications, {} enqueued, {} watchers visited",
                DATABASE_NOTIFICATIONS.load(AtomicOrdering::Relaxed),
                DATABASE_ENQUEUED.load(AtomicOrdering::Relaxed),
                DATABASE_WATCHERS_VISITED.load(AtomicOrdering::Relaxed),
            );
            info!(
                "Deduction propagators ran {calls} times ({} shortcut, {} inactive)",
                PROPAGATE_SHORTCUTS.load(AtomicOrdering::Relaxed),
                PROPAGATE_INACTIVE.load(AtomicOrdering::Relaxed),
            );
        }
        info!(
            "Trimming took {:.1}s: {:.1}s propagating over {calls_propagate} deductions, \
             {:.1}s explaining, {:.1}s elsewhere",
            loop_elapsed.as_secs_f64(),
            time_propagate.as_secs_f64(),
            time_explain.as_secs_f64(),
            (loop_elapsed - time_propagate - time_explain).as_secs_f64(),
        );

        for (index, (duration, calls)) in deciles.iter().enumerate() {
            if *calls == 0 {
                continue;
            }

            info!(
                "  scaffold {:>3}-{:>3}%: {calls:>7} deductions, {:>8.1}ms each, {:>7.1}s total",
                index * 10,
                (index + 1) * 10,
                duration.as_secs_f64() * 1000.0 / *calls as f64,
                duration.as_secs_f64(),
            );
        }

        info!(
            "Writing proof with {} stages and {} inferences",
            self.output_proof.len(),
            num_inferences,
        );

        if let (true, ProofOutput::Blocks(proof_log)) = (self.drop_unused_steps, &mut output) {
            self.drop_unused_held_steps(proof_log);
        }

        // Finally, we write the output proof to the file. Since the proof stages are in
        // reverse order, we iterate from the end to the beginning.
        let write_start = Instant::now();

        // The bound of a dual bound conclusion, or `None` for unsatisfiability.
        let conclusion: Option<Option<Predicate>> = conclusion
            .map(|conclusion| match conclusion {
                Conclusion::Unsat => Ok(None),
                Conclusion::DualBound(bound) => {
                    convert_proof_atomic_to_predicate(&self.variables, &bound).map(Some)
                }
            })
            .transpose()?;

        match output {
            ProofOutput::Drcp(mut proof_writer) => {
                for stage in std::mem::take(&mut self.output_proof).into_iter().rev() {
                    let inferences = self.drcp_inferences(stage.steps);
                    let mut sequence = Vec::with_capacity(inferences.len());

                    for inference in inferences.into_iter() {
                        sequence.push(inference.constraint_id);
                        proof_writer.log_inference(inference)?;
                    }

                    proof_writer.log_deduction(Deduction {
                        constraint_id: stage.tag.into(),
                        premises: convert_predicates_to_proof_atomic(
                            &self.variables,
                            &stage.premises,
                        ),
                        sequence,
                    })?;
                }

                match conclusion {
                    Some(None) => proof_writer.log_conclusion::<&str>(Conclusion::Unsat)?,
                    Some(Some(bound)) => proof_writer.log_conclusion(Conclusion::DualBound(
                        convert_predicate_to_proof_atomic(&self.variables, bound),
                    ))?,
                    None => {}
                }
            }

            ProofOutput::Blocks(mut proof_log) => {
                for stage in std::mem::take(&mut self.output_proof).into_iter().rev() {
                    for step in stage.steps {
                        match step {
                            ExplanationStep::Inference {
                                inference_code,
                                premises,
                                consequent,
                            } => {
                                let _ = proof_log.log_inference_in_state(
                                    &mut self.state,
                                    inference_code,
                                    premises,
                                    consequent,
                                )?;
                            }
                            ExplanationStep::InitialDomain(predicate) => {
                                let _ = proof_log
                                    .log_domain_inference_in_state(&mut self.state, predicate)?;
                            }
                        }
                    }

                    if let Some(root_lemmas) = self.root_lemmas.as_ref() {
                        proof_log.log_facts_from_unit_nogoods(
                            stage
                                .root_facts
                                .iter()
                                .map(|predicate| (*predicate, root_lemmas[predicate].tag)),
                        );
                    }

                    proof_log.log_nogood_with_tag(&self.state, stage.tag, stage.premises)?;
                }

                // The last nogood of a proof of a bound is the unit nogood of the improvement on
                // it, which the conclusion resolves against the assumed improvement.
                match conclusion {
                    Some(None) => proof_log.conclude_unsat_in_state(&self.state)?,
                    Some(Some(bound)) => proof_log.conclude_optimal_in_state(&self.state, bound)?,
                    None => {}
                }
            }
        }

        info!("Writing took {:.1}s", write_start.elapsed().as_secs_f64());
        info!("Proof processed successfully");

        Ok(())
    }

    /// Leave out the held steps of the head that add a constraint which no kept inference cites,
    /// directly or through a kept step citing it.
    fn drop_unused_held_steps(&self, proof_log: &mut ProofLog) {
        let mut cited: HashSet<ConstraintTag> = self
            .output_proof
            .iter()
            .flat_map(|stage| stage.steps.iter())
            .filter_map(|step| match step {
                ExplanationStep::Inference { inference_code, .. } => Some(inference_code.tag()),
                ExplanationStep::InitialDomain(_) => None,
            })
            .collect();

        let held_steps = proof_log.held_steps();
        let num_held_steps = held_steps.len();
        let mut unused = vec![];
        // A step cites only steps before it, so going backwards sees every citation of a step
        // before the step itself.
        for (step, cites) in held_steps.into_iter().rev() {
            if cited.contains(&step) {
                cited.extend(cites);
            } else {
                unused.push(step);
            }
        }

        info!(
            "Leaving out {} of {num_held_steps} held steps, whose constraint no kept inference cites",
            unused.len()
        );
        proof_log.drop_held_steps(unused);
    }

    /// Initialise the state with the deductions from the proof.
    ///
    /// Any inferences that are encountered are ignored, since we will introduce our own
    /// inferences. Returns `None` for the conclusion when the scaffold has none, with every
    /// deduction marked.
    fn initialise<R: BufRead>(
        &mut self,
        mut proof_reader: ProofReader<R, i32>,
    ) -> Result<(Option<ScaffoldConclusion>, DeductionStack), ProofProcessError> {
        info!("Setting up solver with deductions");

        let mut nogood_stack = KeyedVec::default();

        if let Err(conflict) = self.state.propagate_to_fixed_point() {
            // If the state is in a conflict after propagating only the model
            // constraints, the proof consists of only one proof stage, which explains the facts
            // at the root it uses in place.
            self.root_lemmas = None;

            let empty_nogood_tag = self.state.new_constraint_tag();
            nogood_stack.accomodate(empty_nogood_tag, None);

            let steps = self.explain_conflict(&mut nogood_stack, conflict);

            // Log the empty clause to the proof.
            let root_facts = self.take_cited_root_facts();
            self.output_proof.push(ProofStage {
                steps,
                root_facts,
                tag: empty_nogood_tag,
                premises: vec![],
            });

            return Ok((Some(Conclusion::Unsat), nogood_stack));
        }

        if self.deduction_store != DeductionStoreKind::PerDeduction {
            self.create_databases();
            // Adding a propagator enqueues it, and a checkpoint may only be taken at a fixed
            // point. The databases are empty, so this cannot conflict.
            self.state
                .propagate_to_fixed_point()
                .expect("empty deduction databases cannot conflict");
        }

        loop {
            // Try to read the next step from the proof.
            let next_step = proof_reader.next_step()?;

            // A scaffold that ends without a conclusion, as when the solver stopped before it
            // proved anything, has every deduction derived.
            let Some(step) = next_step else {
                info!("The scaffold has no conclusion; every deduction is derived");
                self.mark_all_deductions(&mut nogood_stack);
                return Ok((None, nogood_stack));
            };

            // Extract the deduction from the step, or otherwise handle the step type.
            let deduction = match step {
                Step::Deduction(deduction) => deduction,

                // A dual bound conclusion is encountered. This is of the form `true -> bound`.
                // This means we have to find a nogood `!bound -> false` and mark it.
                Step::Conclusion(Conclusion::DualBound(bound)) => {
                    let (conclusion, nogood_stack) =
                        self.prepare_trimming_for_dual_bound_conclusion(bound, nogood_stack)?;
                    return Ok((Some(conclusion), nogood_stack));
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
            // To make comparing the scaffold with the full proof easier, we match up the
            // constraint IDs of the deductions where we can: we generate constraint IDs until
            // the one we get is the one observed in the scaffold. Compiling for a proof in the
            // block format can take more tags than the scaffold's compilation did, and then
            // the deduction takes the next tag.
            let mut constraint_tag = self.state.new_constraint_tag();
            while NonZero::from(constraint_tag) < deduction.constraint_id {
                constraint_tag = self.state.new_constraint_tag();
            }

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
            let nogood_rc: Rc<[Predicate]> = nogood.to_vec().into();

            let store = if self.databases.is_some() {
                self.post_to_databases(constraint_tag, Rc::clone(&nogood_rc))
            } else {
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

                PostedIn::Propagators {
                    conflict_detection_handle: conflict_handle,
                    unit_prop_handle: prop_handle,
                }
            };

            nogood_stack.accomodate(constraint_tag, None);
            nogood_stack[constraint_tag] = Some(PostedDeduction {
                predicates: nogood_rc,
                store,
                marked: false,
            });

            if let Err(conflict) = self.state.propagate_to_fixed_point() {
                trace!("Conflict identified");

                // If we reach inconsistency through propagation alone, then it must mean that the
                // proof is a proof of unsatisfiability and not a dual bound proof.
                let (conclusion, nogood_stack) =
                    self.repair_solver(conflict, constraint_tag, nogood_stack)?;
                return Ok((Some(conclusion), nogood_stack));
            }
        }
    }

    /// Mark every deduction, so that each is derived.
    fn mark_all_deductions(&mut self, nogood_stack: &mut DeductionStack) {
        for index in 0..nogood_stack.len() {
            let tag = ConstraintTag::create_from_index(index);
            if let Some(posted_deduction) = nogood_stack[tag].as_mut() {
                self.mark_deduction(posted_deduction);
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

        // Now we explain _why_ the concluded predicate is true. In the output proof, we must
        // insert a deduction that syntactally implies the conclusion. I.e., if the conclusion is
        // [x >= 5], then we _must_ have a nogood `[x <= 4] -> false`.
        //
        // To achieve this, we look at the reason that currently the conclusion is true, which
        // may be the initial domain, and then insert the required deduction supported by the
        // inferences as they exist in the processor at the moment. It could happen that the
        // conclusion is a root bound, but the proof does not contain a nogood asserting the root
        // bound (an inference is not enough, we explicitly want a deduction that makes the
        // conclusion true).
        let steps = self.explain_dual_bound(&mut nogood_stack, concluded_predicate);
        let deduction_tag = self.state.new_constraint_tag();

        let root_facts = self.take_cited_root_facts();

        self.output_proof.push(ProofStage {
            steps,
            root_facts,
            tag: deduction_tag,
            premises: vec![!concluded_predicate],
        });

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
        self.mark_deduction(posted_deduction);

        let steps = self.explain_conflict(&mut nogood_stack, conflict);

        // Log the empty clause to the proof.
        let tag = self.state.new_constraint_tag();
        let root_facts = self.take_cited_root_facts();
        self.output_proof.push(ProofStage {
            steps,
            root_facts,
            tag,
            premises: vec![],
        });

        Ok((Conclusion::Unsat, nogood_stack))
    }

    /// Explain a conflict: the steps from the conflict back to the assumptions and the initial
    /// domains, marking every deduction they cite.
    fn explain_conflict(
        &mut self,
        nogood_stack: &mut DeductionStack,
        conflict: Conflict,
    ) -> Vec<ExplanationStep> {
        self.collect_conflict_explanation(nogood_stack, conflict)
    }

    /// Explain why the concluded predicate of a dual bound holds. The deduction states the
    /// negation of that predicate as its nogood.
    fn explain_dual_bound(
        &mut self,
        nogood_stack: &mut DeductionStack,
        concluded_predicate: Predicate,
    ) -> Vec<ExplanationStep> {
        self.explain_later(concluded_predicate);
        self.collect_predicate_explanations(nogood_stack, vec![])
    }

    /// The steps that explain the conflict, from the conflict back to the assumptions and the
    /// initial domains, marking every deduction they cite.
    fn collect_conflict_explanation(
        &mut self,
        nogood_stack: &mut DeductionStack,
        conflict: Conflict,
    ) -> Vec<ExplanationStep> {
        assert!(self.to_process_heap.is_empty());

        let mut steps = vec![];

        let predicates_to_explain: Vec<Predicate> = match conflict {
            Conflict::Propagator(PropagatorConflict {
                conjunction,
                inference_code,
            }) => {
                self.mark_stack_entry(nogood_stack, inference_code.clone());
                steps.push(ExplanationStep::Inference {
                    inference_code,
                    premises: conjunction.to_vec(),
                    consequent: None,
                });

                conjunction.to_vec()
            }
            Conflict::EmptyDomain(empty_domain_conflict) => {
                let mut predicates_to_explain = vec![];
                let maybe_trigger_inference = empty_domain_conflict.get_reason(
                    &mut self.state,
                    &mut predicates_to_explain,
                    CurrentNogood::empty(),
                );

                // The trigger emptied the domain, so its negation holds. Together with the
                // premises that imply the trigger, that is the conflict. A trigger without an
                // inference holds because the domain implies it.
                if let Some(inference_code) = maybe_trigger_inference {
                    self.mark_stack_entry(nogood_stack, inference_code.clone());
                    steps.push(ExplanationStep::Inference {
                        inference_code,
                        premises: predicates_to_explain.clone(),
                        consequent: Some(empty_domain_conflict.trigger_predicate),
                    });
                }

                predicates_to_explain.push(!empty_domain_conflict.trigger_predicate);

                predicates_to_explain
            }
        };

        // Then we add the conflict to the queue of predicates that need to be explained
        // through inferences.
        for predicate in predicates_to_explain {
            self.explain_later(predicate);
        }

        self.collect_predicate_explanations(nogood_stack, steps)
    }

    /// Explain every predicate in the queue, and the predicates their explanations rest on,
    /// appending a step for each that a propagator or the initial domains imply. A predicate
    /// without a reason is one of the assumptions. One that the current domain implies without
    /// an inference adds no step, and its reason is explained in its place.
    fn collect_predicate_explanations(
        &mut self,
        nogood_stack: &mut DeductionStack,
        mut steps: Vec<ExplanationStep>,
    ) -> Vec<ExplanationStep> {
        let mut reason_buffer = vec![];
        let mut visited_predicates: HashSet<Predicate> = HashSet::default();

        while let Some(predicate) = self.to_process_heap.pop() {
            if !visited_predicates.insert(predicate) {
                continue;
            }

            if self.state.is_implied_by_initial_domain(predicate) {
                steps.push(ExplanationStep::InitialDomain(predicate));
                continue;
            }

            // In this case, the predicate was propagated by a constraint. We compute the reason
            // and log the appropriate inference step.
            let inference_code = self.state.get_propagation_reason(
                predicate,
                &mut reason_buffer,
                CurrentNogood::empty(),
            );

            if let Some(inference_code) = inference_code {
                self.mark_stack_entry(nogood_stack, inference_code.clone());
                steps.push(ExplanationStep::Inference {
                    inference_code,
                    premises: reason_buffer.clone(),
                    consequent: Some(predicate),
                });
            }

            for &predicate in &reason_buffer {
                self.explain_later(predicate);
            }
            reason_buffer.clear();
        }

        steps
    }

    /// Queue the predicate to be explained, unless it is a fact at the root, which the
    /// derivation infers from its unit nogood instead.
    fn explain_later(&mut self, predicate: Predicate) {
        if !self.cite_root_lemma(predicate) {
            self.to_process_heap.push(predicate, &self.state);
        }
    }

    /// The facts at the root the derivation rests on, the latest on the trail first, so that a
    /// bound comes before the weaker bounds it implies; the next derivation cites its own.
    fn take_cited_root_facts(&mut self) -> Vec<Predicate> {
        let mut facts = std::mem::take(&mut self.cited_root_facts);
        facts.sort_unstable_by_key(|a| std::cmp::Reverse(a.0));
        self.derivation_number += 1;
        facts.into_iter().map(|(_, predicate)| predicate).collect()
    }

    /// Whether the predicate is a fact propagated at the root, which the derivation being
    /// explained then infers from the unit nogood the proof derives for it. Such facts are
    /// explained in place when the proof derives no unit nogoods for them.
    fn cite_root_lemma(&mut self, predicate: Predicate) -> bool {
        let Some(root_lemmas) = self.root_lemmas.as_ref() else {
            return false;
        };

        if !root_lemmas.contains_key(&predicate) {
            if self.state.get_checkpoint_for_predicate(predicate) != Some(0)
                || self.state.is_implied_by_initial_domain(predicate)
            {
                return false;
            }

            // A predicate the domain implies without being on the trail, or one posted at the
            // root, has no reason, and is explained in place.
            let mut reason = vec![];
            if self
                .state
                .get_propagation_reason(predicate, &mut reason, CurrentNogood::empty())
                .is_none()
            {
                return false;
            }

            let Some(trail_position) = self.state.trail_position(predicate) else {
                return false;
            };
            let tag = self.state.new_constraint_tag();
            let _ = self.root_lemmas.as_mut().expect("checked above").insert(
                predicate,
                RootLemma {
                    tag,
                    trail_position,
                    cited_by: 0,
                },
            );
        }

        let derivation_number = self.derivation_number;
        let lemma = self
            .root_lemmas
            .as_mut()
            .expect("checked above")
            .get_mut(&predicate)
            .expect("inserted above");
        if lemma.cited_by != derivation_number {
            lemma.cited_by = derivation_number;
            self.cited_root_facts
                .push((lemma.trail_position, predicate));
        }
        true
    }

    /// The stages that derive the unit nogoods of the facts at the root that the proof cites,
    /// each from the propagation of its fact and the unit nogoods of the facts it rests on, in
    /// the reverse of the order they are written in.
    fn derive_root_lemmas(&mut self) -> Vec<ProofStage> {
        if self.root_lemmas.is_none() {
            return vec![];
        }

        // Only the model's constraints propagate at the root, so no deduction is marked.
        let mut no_deductions = DeductionStack::default();
        let mut derived: HashSet<Predicate> = HashSet::default();
        let mut stages = vec![];

        loop {
            let to_derive: Vec<(Predicate, RootLemma)> = self
                .root_lemmas
                .as_ref()
                .expect("checked above")
                .iter()
                .filter(|(predicate, _)| !derived.contains(predicate))
                .map(|(&predicate, &lemma)| (predicate, lemma))
                .collect();
            if to_derive.is_empty() {
                break;
            }

            for (predicate, lemma) in to_derive {
                let _ = derived.insert(predicate);

                let mut reason = vec![];
                let inference_code = self
                    .state
                    .get_propagation_reason(predicate, &mut reason, CurrentNogood::empty())
                    .expect("a fact with a unit nogood is propagated at the root");
                debug_assert!(self.cited_root_facts.is_empty());
                for &premise in reason.iter() {
                    self.explain_later(premise);
                }
                let steps = self.collect_predicate_explanations(
                    &mut no_deductions,
                    vec![ExplanationStep::Inference {
                        inference_code,
                        premises: reason,
                        consequent: Some(predicate),
                    }],
                );

                stages.push((
                    lemma.trail_position,
                    ProofStage {
                        tag: lemma.tag,
                        steps,
                        root_facts: self.take_cited_root_facts(),
                        premises: vec![!predicate],
                    },
                ));
            }
        }

        // A fact comes after the facts it rests on, so the latest comes first here.
        stages.sort_by_key(|a| std::cmp::Reverse(a.0));
        stages.into_iter().map(|(_, stage)| stage).collect()
    }

    /// The DRCP inferences of a stage, in the order they propagate: the steps reversed, each
    /// initial-domain inference once, numbered with fresh constraint tags. Atoms over variables
    /// without a name, the constants of the model, are left out.
    fn drcp_inferences(
        &mut self,
        steps: Vec<ExplanationStep>,
    ) -> Vec<Inference<String, i32, Arc<str>>> {
        let named = |variables: &Variables, predicate: &Predicate| {
            variables
                .get_name_for_domain(predicate.get_domain())
                .is_some()
        };

        // The inferences ordered by closeness to the conflict. Every element is an option, where
        // `None` serves as a tombstone value: when an initial bound is used again, its inference
        // moves to the back.
        let mut inferences: Vec<Option<Inference<String, i32, Arc<str>>>> = vec![];
        let mut initial_bound_indices: HashMap<Predicate, usize> = HashMap::new();

        for step in steps {
            match step {
                ExplanationStep::Inference {
                    inference_code,
                    premises,
                    consequent,
                } => {
                    let premises: Vec<Predicate> = premises
                        .into_iter()
                        .filter(|predicate| named(&self.variables, predicate))
                        .collect();
                    let consequent =
                        consequent.filter(|predicate| named(&self.variables, predicate));
                    inferences.push(Some(Inference {
                        constraint_id: self.state.new_constraint_tag().into(),
                        premises: convert_predicates_to_proof_atomic(&self.variables, &premises),
                        consequent: consequent.map(|predicate| {
                            convert_predicate_to_proof_atomic(&self.variables, predicate)
                        }),
                        generated_by: Some(inference_code.tag().into()),
                        label: Some(inference_code.label()),
                    }));
                }

                ExplanationStep::InitialDomain(predicate) => {
                    if !named(&self.variables, &predicate) {
                        continue;
                    }
                    let inference = match initial_bound_indices.get(&predicate).copied() {
                        Some(index) => std::mem::take(&mut inferences[index]),
                        None => Some(Inference {
                            constraint_id: self.state.new_constraint_tag().into(),
                            premises: vec![],
                            consequent: Some(convert_predicate_to_proof_atomic(
                                &self.variables,
                                predicate,
                            )),
                            generated_by: None,
                            label: Some(Arc::from(INITIAL_DOMAIN_LABEL)),
                        }),
                    };
                    inferences.push(inference);
                    let _ = initial_bound_indices.insert(predicate, inferences.len() - 1);
                }
            }
        }

        // Reverse the inference sequence, since analysis goes from the conflict to the leafs
        // rather than from the assumptions to the conflict, and filter out the tombstones.
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

        if let Some(Some(posted_deduction)) = stack.get_mut(used_constraint_tag) {
            self.mark_deduction(posted_deduction);
        }
    }

    /// Mark a deduction as needed by the proof, which raises its priority so it propagates before
    /// unmarked ones. In the databases that is a move from the unmarked ones to the marked one.
    fn mark_deduction(&mut self, posted: &mut PostedDeduction) {
        if posted.marked {
            return;
        }

        posted.marked = true;

        match &posted.store {
            PostedIn::Propagators { .. } => {
                posted.update_propagators_priority(&mut self.state, MARKED_PROPAGATION_PRIORITY);
            }

            PostedIn::Database(index) => {
                let index = *index;
                let databases = self.databases.expect("a database entry implies databases");

                let inference_code = self
                    .database(databases.unmarked_conflict)
                    .inference_code(index)
                    .expect("an unmarked deduction is in the unmarked conflict database");

                self.database(databases.unmarked_conflict).deactivate(index);
                if let Some(unit) = databases.unmarked_unit {
                    self.database(unit).deactivate(index);
                }

                let (watched, immediate) = choose_watches(&self.state, &posted.predicates);
                self.database(databases.marked).add(
                    index,
                    Rc::clone(&posted.predicates),
                    inference_code,
                    watched,
                    immediate,
                );
            }
        }
    }

    /// Remove a deduction from consideration: the backward pass has decided about it.
    fn deactivate_deduction(&mut self, posted: &PostedDeduction) {
        match &posted.store {
            PostedIn::Propagators { .. } => posted.deactivate_propagators(&mut self.state),

            PostedIn::Database(index) => {
                let databases = self.databases.expect("a database entry implies databases");
                self.database(databases.marked).deactivate(*index);
                self.database(databases.unmarked_conflict)
                    .deactivate(*index);
                if let Some(unit) = databases.unmarked_unit {
                    self.database(unit).deactivate(*index);
                }
            }
        }
    }

    fn database(&mut self, handle: PropagatorHandle<DeductionDatabase>) -> &mut DeductionDatabase {
        self.state
            .get_propagator_mut(handle)
            .expect("database handles are valid for the life of the processor")
    }

    /// Create the databases, empty.
    fn create_databases(&mut self) {
        let marked = self.state.add_propagator(DeductionDatabaseConstructor {
            mode: DatabaseMode::ConflictsAndPropagation,
            priority: MARKED_PROPAGATION_PRIORITY,
        });

        let (unmarked_conflict, unmarked_unit) = match self.deduction_store {
            DeductionStoreKind::DatabaseMerged => (
                self.state.add_propagator(DeductionDatabaseConstructor {
                    mode: DatabaseMode::ConflictsAndPropagation,
                    priority: UNMARKED_CONFLICT_PRIORITY,
                }),
                None,
            ),
            _ => {
                let conflict = self.state.add_propagator(DeductionDatabaseConstructor {
                    mode: DatabaseMode::ConflictsOnly,
                    priority: UNMARKED_CONFLICT_PRIORITY,
                });
                let unit = self.state.add_propagator(DeductionDatabaseConstructor {
                    mode: DatabaseMode::PropagationOnly,
                    priority: UNMARKED_UNIT_PROPAGATION_PRIORITY,
                });
                (conflict, Some(unit))
            }
        };

        self.databases = Some(Databases {
            marked,
            unmarked_conflict,
            unmarked_unit,
        });
    }

    /// Register every database on the predicates of the nogood it has not seen before, and on
    /// their negations.
    fn register_with_databases(&mut self, predicates: &[Predicate]) {
        let databases = self
            .databases
            .expect("databases exist before deductions are posted");
        let handles = [
            Some(databases.marked),
            Some(databases.unmarked_conflict),
            databases.unmarked_unit,
        ];

        for &predicate in predicates {
            if !self.registered_predicates.insert(predicate) {
                continue;
            }

            for handle in handles.into_iter().flatten() {
                let predicate_id = self.state.register_predicate(handle, predicate);
                let _ = self.state.register_predicate(handle, !predicate);
                self.database(handle)
                    .note_predicate(predicate, predicate_id);
            }
        }
    }

    /// Add a deduction to the unmarked databases, waking them if it may act immediately.
    fn post_to_databases(
        &mut self,
        constraint_tag: ConstraintTag,
        predicates: Rc<[Predicate]>,
    ) -> PostedIn {
        self.register_with_databases(&predicates);

        let databases = self
            .databases
            .expect("databases exist before deductions are posted");
        // Dense, in the order deductions are posted, which is the order of their tags.
        let index = NogoodIndex(self.registered_deductions);
        self.registered_deductions += 1;
        let inference_code = crate::deduction_database::nogood_inference_code(
            &mut self.state,
            constraint_tag,
            &predicates,
        );
        let (watched, immediate) = choose_watches(&self.state, &predicates);

        self.database(databases.unmarked_conflict).add(
            index,
            Rc::clone(&predicates),
            inference_code.clone(),
            watched,
            immediate,
        );
        if let Some(unit) = databases.unmarked_unit {
            self.database(unit).add(
                index,
                Rc::clone(&predicates),
                inference_code.clone(),
                watched,
                immediate,
            );
        }

        if immediate {
            // Both unmarked databases examine the nogood in the propagation that follows.
            self.state.wake_propagator(databases.unmarked_conflict);
            if let Some(unit) = databases.unmarked_unit {
                self.state.wake_propagator(unit);
            }
        }

        PostedIn::Database(index)
    }
}

/// The nogood as the conjunction of its atoms, written with the variables' names.
fn describe_nogood(variables: &Variables, predicates: &[Predicate]) -> String {
    let atoms: Vec<String> = convert_predicates_to_proof_atomic(variables, predicates)
        .into_iter()
        .map(|atomic| {
            let comparison = match atomic.comparison {
                IntComparison::GreaterEqual => ">=",
                IntComparison::LessEqual => "<=",
                IntComparison::Equal => "==",
                IntComparison::NotEqual => "!=",
            };
            format!("[{} {comparison} {}]", atomic.name, atomic.value)
        })
        .collect();
    if atoms.is_empty() {
        "the empty nogood".to_owned()
    } else {
        atoms.join(" /\\ ")
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
                3,
                [],
                Some(atomic("x1", GreaterEqual, 10)),
                None,
                Some("initial_domain"),
            ),
            deduction(2, [atomic("x1", LessEqual, 9)], [3]),
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
                5,
                [],
                Some(atomic("x2", GreaterEqual, 10)),
                None,
                Some("initial_domain"),
            ),
            inference(
                4,
                [atomic("x2", GreaterEqual, 10)],
                Some(atomic("x1", GreaterEqual, 10)),
                Some(1),
                Some("binary_equals"),
            ),
            deduction(3, [atomic("x1", LessEqual, 9)], [5, 4]),
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
                6,
                [],
                Some(atomic("a", LessEqual, 0)),
                None,
                Some("initial_domain"),
            ),
            inference(
                5,
                [atomic("obj", GreaterEqual, 6), atomic("a", LessEqual, 0)],
                None,
                Some(1),
                Some("always_conflict"),
            ),
            deduction(
                2,
                [atomic("a", LessEqual, 0), atomic("obj", GreaterEqual, 6)],
                [6, 5],
            ),
            inference(
                8,
                [],
                Some(atomic("a", LessEqual, 0)),
                None,
                Some("initial_domain"),
            ),
            inference(
                7,
                [atomic("a", LessEqual, 0)],
                Some(atomic("obj", LessEqual, 5)),
                Some(2),
                Some("nogood"),
            ),
            deduction(4, [atomic("obj", GreaterEqual, 6)], [8, 7]),
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
                ProofOutput::Drcp(ProofWriter::new(&mut processed)),
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
