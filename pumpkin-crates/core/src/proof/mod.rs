//! Pumpkin supports proof logging for SAT and CP problems. During search, the solver produces a
//! [`ProofLog`], which is a list of deductions made by the solver.
//!
//! Proof logging for CP is supported in the DRCP format. This format explicitly supports usage
//! where the solver logs a proof scaffold which later processed into a full proof after search
//! has completed.
mod dimacs;
mod finalizer;
mod inference_code;
mod proof_atomics;
pub mod view_log;

use std::fs::File;
use std::io::Write;
use std::num::NonZero;
use std::path::Path;
use std::sync::Arc;

use dimacs::DimacsProof;
use drcp_format::Deduction;
use drcp_format::Inference;
use drcp_format::IntAtomic;
use drcp_format::IntComparison;
use drcp_format::writer::ProofWriter;
use drcp_format::writer::blocks::BlockProofWriter;
use drcp_format::writer::blocks::ReferencedInference;
pub(crate) use finalizer::*;
pub use inference_code::*;
use proof_atomics::ProofAtomics;
use pumpkin_checking::InvalidDeduction;
use pumpkin_checking::SupportingInference;
use pumpkin_checking::VariableState;
use pumpkin_checking::verify_deduction;

#[cfg(doc)]
use crate::Solver;
use crate::containers::HashMap;
use crate::containers::HashSet;
use crate::containers::KeyGenerator;
use crate::engine::Assignments;
use crate::engine::predicates::predicate::PredicateType;
use crate::engine::variable_names::VariableNames;
use crate::engine::variables::AffineView;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::DomainId;
use crate::variables::Literal;

/// The line of a croc scaffold that ends its preamble; the nogoods follow it.
pub const SCAFFOLD_NOGOODS_MARKER: &str = "nogoods";

/// A part of a line written by `ProofLog::log_head_line`.
#[derive(Clone, Debug)]
pub enum HeadField {
    Text(String),
    /// The reference of the atom, declared first if it is new.
    Atom(Predicate),
    /// The number of the rule label, declared first if it is new.
    Label(&'static str),
    /// The name the proof gives the variable.
    Name(DomainId),
}

/// A proof log which logs the proof steps necessary to prove unsatisfiability or optimality. We
/// allow the following types of proofs:
/// - A CP proof log - This can be created using [`ProofLog::cp`].
/// - A DIMACS proof log - This can be created using [`ProofLog::dimacs`].
///
/// When a proof log should not be generated, use the implementation of [`Default`].
#[derive(Debug, Default)]
pub struct ProofLog {
    internal_proof: Option<ProofImpl>,
    pub(crate) supporting_inferences: Vec<SupportingInference<Predicate>>,
    /// For each supporting inference, whether it explains a predicate at the root.
    supporting_inferences_at_root: Vec<bool>,
    /// Whether the inferences being logged explain why predicates hold at the root.
    explaining_root: bool,
    /// The inferences logged for the next nogood that explain predicates at the root. They are
    /// placed before the others: an explanation of a root predicate can be logged after an
    /// inference that uses the predicate, when conflict analysis or a minimiser asks for it late,
    /// and it needs no premise that is not itself explained at the root.
    root_explanations: HashSet<ConstraintTag>,
}

impl ProofLog {
    /// Create a CP proof logger.
    pub fn cp(file_path: &Path, log_hints: bool) -> std::io::Result<ProofLog> {
        ProofLog::cp_with(file_path, log_hints, false)
    }

    /// Create a logger of a croc scaffold: a CP proof without hints whose nogoods keep their
    /// atoms over every named variable, fixed or not, since the proof processor and
    /// fzn-croc-check read a variable with a fixed domain as a variable.
    ///
    /// The file starts with the preamble, given by [`ProofLog::log_scaffold_preamble`], and the
    /// line [`SCAFFOLD_NOGOODS_MARKER`]; what is logged before the preamble is given is held
    /// until then.
    pub fn croc_scaffold(file_path: &Path) -> std::io::Result<ProofLog> {
        ProofLog::cp_with(file_path, false, true)
    }

    /// Write the preamble of a croc scaffold, which ends with the line
    /// [`SCAFFOLD_NOGOODS_MARKER`], followed by the nogoods logged so far. Does nothing for other
    /// proofs, or when the preamble is already written.
    pub fn log_scaffold_preamble(&mut self, preamble: &str) -> std::io::Result<()> {
        let Some(ProofImpl::CpProof { writer, .. }) = self.internal_proof.as_mut() else {
            return Ok(());
        };
        writer.get_mut().release(preamble)
    }

    fn cp_with(
        file_path: &Path,
        log_hints: bool,
        croc_scaffold: bool,
    ) -> std::io::Result<ProofLog> {
        let file = File::create(file_path)?;

        let sink = if file_path.extension().is_some_and(|ext| ext == "gz") {
            Sink::GzippedFile(flate2::write::GzEncoder::new(
                file,
                flate2::Compression::fast(),
            ))
        } else {
            Sink::File(file)
        };

        let writer = ProofWriter::new(CpSink {
            sink,
            held: croc_scaffold.then(Vec::new),
        });

        Ok(ProofLog {
            internal_proof: Some(ProofImpl::CpProof {
                writer,
                propagation_order_hint: if log_hints { Some(vec![]) } else { None },
                logged_domain_inferences: HashMap::default(),
                proof_atomics: ProofAtomics::default(),
                keeps_fixed_variables: croc_scaffold,
            }),
            supporting_inferences: vec![],
            supporting_inferences_at_root: vec![],
            explaining_root: false,
            root_explanations: HashSet::default(),
        })
    }

    /// Create a proof logger that writes the full proof in the block format fzn-croc-check
    /// reads, with the steps of its head written first by [`ProofLog::log_head`]: every learned
    /// nogood as a resolution chain from the conflict when `resolution_chains` holds, and
    /// otherwise as a refutation by the inferences that derived it.
    ///
    /// With `binary`, the proof is written in the binary form fzn-croc-check reads.
    pub fn blocks(
        file_path: &Path,
        resolution_chains: bool,
        binary: bool,
    ) -> std::io::Result<ProofLog> {
        let file = File::create(file_path)?;

        let sink = if file_path.extension().is_some_and(|ext| ext == "gz") {
            Sink::GzippedFile(flate2::write::GzEncoder::new(
                file,
                flate2::Compression::fast(),
            ))
        } else {
            Sink::File(file)
        };

        let writer = if binary {
            BlockProofWriter::new_binary(std::io::BufWriter::new(sink))?
        } else {
            BlockProofWriter::new(std::io::BufWriter::new(sink))
        };

        Ok(ProofLog {
            internal_proof: Some(ProofImpl::BlockProof(Box::new(BlockProof {
                writer,
                inferences: HashMap::default(),
                propagation_order: vec![],
                logged_domain_inferences: HashMap::default(),
                proof_atomics: ProofAtomics::default(),
                derived_identifiers: HashMap::default(),
                next_identifier: 1,
                last_nogood: None,
                rule_of_constraint: HashMap::default(),
                rule_by_indicator_side: HashMap::default(),
                positions_of_constraint: HashMap::default(),
                set_in_domains: HashMap::default(),
                selector_bounds: HashMap::default(),
                selector_bound_tags: HashSet::default(),
                objective_view: None,
                reification: HashMap::default(),
                fixing_bounds_when_true: HashSet::default(),
                resolution_chains,
                held_lines: vec![],
                atom_references: HashMap::default(),
                label_references: HashMap::default(),
                seen_premises: HashSet::default(),
            }))),
            supporting_inferences: vec![],
            supporting_inferences_at_root: vec![],
            explaining_root: false,
            root_explanations: HashSet::default(),
        })
    }

    /// Hold the steps that come before the first nogood, such as the preprocessing steps of the
    /// FlatZinc compiler, to be written before it, and set the identifier the first nogood takes.
    pub fn log_head(
        &mut self,
        lines: &[String],
        first_free_identifier: u32,
    ) -> std::io::Result<()> {
        let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() else {
            return Ok(());
        };

        proof.held_lines.extend(lines.iter().cloned());
        proof.next_identifier = first_free_identifier;

        Ok(())
    }

    /// Hold a line after the head, in the block format, with each atom, label and variable
    /// written as the proof refers to it; the held lines are written before the first nogood.
    /// Does nothing for other proofs.
    pub(crate) fn log_head_line(
        &mut self,
        fields: &[HeadField],
        variable_names: &VariableNames,
    ) -> std::io::Result<()> {
        let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() else {
            return Ok(());
        };
        let line = proof.render_head_line(fields, variable_names)?;
        proof.held_lines.push(line);
        Ok(())
    }

    /// Take the identifier the next constraint of a proof in the block format has, as the
    /// identifier of the constraint with the tag: inferences from it cite that identifier.
    /// Returns `None` for other proofs.
    pub fn take_identifier(&mut self, constraint_tag: ConstraintTag) -> Option<u32> {
        let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() else {
            return None;
        };
        let identifier = proof.next_identifier;
        proof.next_identifier += 1;
        let _ = proof.derived_identifiers.insert(
            constraint_tag,
            NonZero::new(identifier).expect("identifiers start at 1"),
        );
        Some(identifier)
    }

    /// Name the rule of fzn-drcp-check that licenses the inferences from a constraint, when it
    /// differs from the label of the propagator: a constraint Pumpkin decomposes, such as an
    /// all-different into disequalities or a minimum into a maximum of negations, is still cited
    /// as itself.
    pub fn name_rule_of_constraint(&mut self, constraint_tag: ConstraintTag, rule: &str) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof
                .rule_of_constraint
                .insert(constraint_tag, Arc::from(rule));
        }
    }

    /// Name the rules that license the inferences from a reified constraint Pumpkin decomposes into
    /// clauses: an inference whose nogood contains the indicator's true predicate is licensed by
    /// the rule of the constraint, one whose nogood contains its negation by the rule of the
    /// constraint's negation.
    pub fn name_rules_by_indicator_side(
        &mut self,
        constraint_tag: ConstraintTag,
        indicator: Predicate,
        rule_when_true: &str,
        rule_when_false: &str,
    ) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof.rule_by_indicator_side.insert(
                constraint_tag,
                (
                    indicator,
                    Arc::from(rule_when_true),
                    Arc::from(rule_when_false),
                ),
            );
        }
    }

    /// Give the objective variable as `scale * representative + offset`, stated by the
    /// constraint `evidence`: a bound the solver proves on the representative is concluded on
    /// the objective.
    pub fn name_objective_view(
        &mut self,
        objective: &str,
        representative: &str,
        scale: i32,
        offset: i32,
        evidence: u32,
    ) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            proof.objective_view = Some(ObjectiveView {
                objective: Arc::from(objective),
                representative: Arc::from(representative),
                scale: i64::from(scale),
                offset: i64::from(offset),
                evidence: NonZero::new(evidence).expect("a model constraint is numbered from one"),
            });
        }
    }

    /// State, in an inference from the reified constraint whose nogood contains its indicator, the
    /// bounds of the declared domains that fix each 0/1 variable the nogood bounds on one side.
    /// The rule of the constraint under its indicator, such as the disequality of
    /// `pumpkin_bool_xor_reif`, reads only the nogood, and needs both terms fixed.
    pub fn state_fixing_bounds_when_true(&mut self, constraint_tag: ConstraintTag) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof.fixing_bounds_when_true.insert(constraint_tag);
        }
    }

    /// State that the checker reads the constraint as reified (`fully`) or half-reified: an
    /// inference from it cites the rule of the inner constraint `R` as `reified(R)` or
    /// `half_reified(R)`.
    pub fn name_reification(&mut self, constraint_tag: ConstraintTag, fully: bool) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof.reification.insert(constraint_tag, fully);
        }
    }

    /// Give the set of a `set_in` constraint over the domain, as ranges of values: a predicate on
    /// the domain that holds at the root and whose negation no value of the set satisfies is
    /// cited from the constraint under the rule `set_in`, since the checker does not read the
    /// set into the initial domains.
    pub fn name_set_in_domain(
        &mut self,
        constraint_tag: ConstraintTag,
        domain: DomainId,
        ranges: Vec<(i32, i32)>,
    ) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            proof
                .set_in_domains
                .entry(domain)
                .or_default()
                .push((constraint_tag, ranges));
        }
    }

    /// Give the constraint of a decomposition that states the bounds of a fresh 0/1 variable,
    /// such as a selector of a table: an inference the domain of the variable implies cites that
    /// constraint under the rule `initial_domain`, since the initial domains do not hold it.
    pub fn name_selector_bounds(&mut self, domain: DomainId, constraint_tag: ConstraintTag) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof.selector_bounds.insert(domain, constraint_tag);
            let _ = proof.selector_bound_tags.insert(constraint_tag);
        }
    }

    /// Give the elements of a constraint over a list of expressions, such as an all-different,
    /// so that an inference from it names the positions it reads: those over the variables it
    /// mentions and the constants with a value it mentions.
    pub fn name_positions_of_constraint(
        &mut self,
        constraint_tag: ConstraintTag,
        elements: Vec<AffineView<DomainId>>,
    ) {
        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof
                .positions_of_constraint
                .insert(constraint_tag, elements);
        }
    }

    /// Create a dimacs proof logger.
    pub fn dimacs(file_path: &Path) -> std::io::Result<ProofLog> {
        let file = File::create(file_path)?;
        Ok(ProofLog {
            internal_proof: Some(ProofImpl::DimacsProof(DimacsProof::new(file))),
            supporting_inferences: vec![],
            supporting_inferences_at_root: vec![],
            explaining_root: false,
            root_explanations: HashSet::default(),
        })
    }

    /// Mark the inferences logged from now on as explanations of predicates at the root, or stop
    /// doing so; returns whether they were marked before.
    pub(crate) fn set_explaining_root(&mut self, explaining_root: bool) -> bool {
        std::mem::replace(&mut self.explaining_root, explaining_root)
    }

    /// Log an inference to the proof.
    pub(crate) fn log_inference(
        &mut self,
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        inference_code: InferenceCode,
        premises: impl IntoIterator<Item = Predicate> + Clone,
        propagated: Option<Predicate>,
        variable_names: &VariableNames,
        assignments: &Assignments,
    ) -> std::io::Result<ConstraintTag> {
        let inference_tag = constraint_tags.next_key();
        if self.explaining_root {
            let _ = self.root_explanations.insert(inference_tag);
        }

        if cfg!(feature = "check-deductions") {
            self.supporting_inferences_at_root
                .push(self.explaining_root);
            self.supporting_inferences.push(SupportingInference {
                premises: premises.clone().into_iter().collect(),
                consequent: propagated,
            });
        }

        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            let _ = proof.inferences.insert(
                inference_tag,
                BufferedInference {
                    premises: premises.into_iter().collect(),
                    consequent: propagated,
                    constraint: Some(inference_code.tag()),
                    label: inference_code.label(),
                },
            );
            proof.propagation_order.push(Some(inference_tag));
            return Ok(inference_tag);
        }

        let Some(ProofImpl::CpProof {
            writer,
            propagation_order_hint: Some(propagation_sequence),
            proof_atomics,
            ..
        }) = self.internal_proof.as_mut()
        else {
            return Ok(inference_tag);
        };

        let inference = Inference {
            constraint_id: inference_tag.into(),
            premises: premises
                .into_iter()
                .filter(|&predicate| !is_likely_a_constant(predicate, variable_names, assignments))
                .map(|premise| proof_atomics.map_predicate_to_proof_atomic(premise, variable_names))
                .collect(),
            consequent: propagated
                .filter(|&predicate| !is_likely_a_constant(predicate, variable_names, assignments))
                .map(|predicate| {
                    proof_atomics.map_predicate_to_proof_atomic(predicate, variable_names)
                }),
            generated_by: Some(inference_code.tag().into()),
            label: Some(inference_code.label()),
        };

        writer.log_inference(inference)?;

        propagation_sequence.push(Some(inference_tag));

        Ok(inference_tag)
    }

    /// Log an inference that claims the given predicate is part of the initial domain.
    pub(crate) fn log_domain_inference(
        &mut self,
        predicate: Predicate,
        variable_names: &VariableNames,
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        assignments: &Assignments,
    ) -> std::io::Result<Option<ConstraintTag>> {
        let tag = self.log_domain_inference_unmarked(
            predicate,
            variable_names,
            constraint_tags,
            assignments,
        )?;
        if self.explaining_root
            && let Some(tag) = tag
        {
            let _ = self.root_explanations.insert(tag);
        }
        Ok(tag)
    }

    fn log_domain_inference_unmarked(
        &mut self,
        predicate: Predicate,
        variable_names: &VariableNames,
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        assignments: &Assignments,
    ) -> std::io::Result<Option<ConstraintTag>> {
        if cfg!(feature = "check-deductions") {
            self.supporting_inferences_at_root
                .push(self.explaining_root);
            self.supporting_inferences.push(SupportingInference {
                premises: vec![],
                consequent: Some(predicate),
            });
        }

        let is_constant = match self.internal_proof {
            // A block proof states what is known about every named variable, fixed or not.
            Some(ProofImpl::BlockProof(_)) => is_unnamed(predicate, variable_names),
            _ => is_likely_a_constant(predicate, variable_names, assignments),
        };
        if is_constant {
            // The predicate is over a constant variable. We assume we do not want to
            // log these if they have no name.

            return Ok(None);
        }

        let inference_tag = constraint_tags.next_key();

        if let Some(ProofImpl::BlockProof(proof)) = self.internal_proof.as_mut() {
            // The bounds an inference rests on are logged after it, so that they apply before
            // it: the propagation order is the reverse of the order of logging. They explain
            // the root when it does.
            let (tag, premises) = proof.log_domain_inference(predicate, inference_tag);
            for premise in premises {
                let premise_tag = self.log_domain_inference_unmarked(
                    premise,
                    variable_names,
                    constraint_tags,
                    assignments,
                )?;
                if self.explaining_root
                    && let Some(premise_tag) = premise_tag
                {
                    let _ = self.root_explanations.insert(premise_tag);
                }
            }
            return Ok(Some(tag));
        }

        let Some(ProofImpl::CpProof {
            writer,
            propagation_order_hint: Some(propagation_sequence),
            logged_domain_inferences,
            proof_atomics,
            ..
        }) = self.internal_proof.as_mut()
        else {
            return Ok(Some(inference_tag));
        };

        if let Some(hint_idx) = logged_domain_inferences.get(&predicate).copied() {
            let tag = propagation_sequence[hint_idx]
                .take()
                .expect("the logged_domain_inferences always points to some index");
            propagation_sequence.push(Some(tag));

            let _ = logged_domain_inferences.insert(predicate, propagation_sequence.len() - 1);

            return Ok(Some(tag));
        }

        let inference = Inference {
            constraint_id: inference_tag.into(),
            premises: vec![],
            consequent: Some(
                proof_atomics.map_predicate_to_proof_atomic(predicate, variable_names),
            ),
            generated_by: None,
            label: Some("initial_domain"),
        };

        writer.log_inference(inference)?;

        propagation_sequence.push(Some(inference_tag));

        let _ = logged_domain_inferences.insert(predicate, propagation_sequence.len() - 1);

        Ok(Some(inference_tag))
    }

    /// Log a deduction (learned nogood) to the proof.
    ///
    /// The inferences and marked propagations are assumed to be recorded in reverse-application
    /// order.
    pub(crate) fn log_deduction(
        &mut self,
        premises: impl IntoIterator<Item = Predicate> + Clone,
        variable_names: &VariableNames,
        constraint_tags: &mut KeyGenerator<ConstraintTag>,
        assignments: &Assignments,
    ) -> std::io::Result<ConstraintTag> {
        let constraint_tag = constraint_tags.next_key();

        if cfg!(feature = "check-deductions") {
            self.verify_deduction_at_runtime(premises.clone());
        }

        match &mut self.internal_proof {
            Some(ProofImpl::CpProof {
                writer,
                propagation_order_hint,
                proof_atomics,
                logged_domain_inferences,
                keeps_fixed_variables,
            }) => {
                // Reset the logged domain inferences.
                logged_domain_inferences.clear();

                let keeps_fixed_variables = *keeps_fixed_variables;
                let deduction = Deduction {
                    constraint_id: constraint_tag.into(),
                    premises: premises
                        .into_iter()
                        .filter(|&predicate| {
                            if keeps_fixed_variables {
                                !is_unnamed(predicate, variable_names)
                            } else {
                                !is_likely_a_constant(predicate, variable_names, assignments)
                            }
                        })
                        .map(|premise| {
                            proof_atomics.map_predicate_to_proof_atomic(premise, variable_names)
                        })
                        .collect(),
                    sequence: roots_first(
                        propagation_order_hint
                            .as_ref()
                            .iter()
                            .flat_map(|vec| vec.iter().rev().copied())
                            .flatten()
                            .collect(),
                        &self.root_explanations,
                    )
                    .into_iter()
                    .map(|tag| tag.into())
                    .collect(),
                };

                writer.log_deduction(deduction)?;

                // Clear the hints for the next nogood.
                if let Some(hints) = propagation_order_hint.as_mut() {
                    hints.clear();
                }
                self.root_explanations.clear();

                Ok(constraint_tag)
            }

            Some(ProofImpl::DimacsProof(writer)) => {
                let clause = premises.into_iter().map(|predicate| !predicate);
                writer.learned_clause(clause, variable_names)?;
                Ok(constraint_tag)
            }

            Some(ProofImpl::BlockProof(proof)) => {
                proof.log_refutation(
                    constraint_tag,
                    premises,
                    &self.root_explanations,
                    variable_names,
                    assignments,
                )?;
                self.root_explanations.clear();
                Ok(constraint_tag)
            }

            None => Ok(constraint_tag),
        }
    }

    pub(crate) fn unsat(self, variable_names: &VariableNames) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof { mut writer, .. }) => {
                writer.log_conclusion::<&str>(drcp_format::Conclusion::Unsat)
            }
            Some(ProofImpl::DimacsProof(mut writer)) => writer
                .learned_clause(std::iter::empty(), variable_names)
                .map(|_| ()),
            Some(ProofImpl::BlockProof(mut proof)) => {
                proof.write_held_lines()?;
                proof
                    .writer
                    .log_conclusion::<&str>(drcp_format::Conclusion::Unsat)
            }
            None => Ok(()),
        }
    }

    pub(crate) fn optimal(
        self,
        objective_bound: Predicate,
        variable_names: &VariableNames,
    ) -> std::io::Result<()> {
        match self.internal_proof {
            Some(ProofImpl::CpProof {
                mut writer,
                mut proof_atomics,
                ..
            }) => {
                let atomic =
                    proof_atomics.map_predicate_to_proof_atomic(objective_bound, variable_names);

                writer.log_conclusion::<&str>(drcp_format::Conclusion::DualBound(atomic))
            }

            Some(ProofImpl::DimacsProof(_)) => {
                panic!("Cannot conclude optimality in DIMACS proof")
            }

            Some(ProofImpl::BlockProof(mut proof)) => {
                proof.conclude_optimal(objective_bound, variable_names)
            }

            None => Ok(()),
        }
    }

    pub fn is_logging_inferences(&self) -> bool {
        matches!(
            self.internal_proof,
            Some(ProofImpl::CpProof {
                propagation_order_hint: Some(_),
                ..
            }) | Some(ProofImpl::BlockProof(_))
        ) || cfg!(feature = "check-deductions")
    }

    pub(crate) fn reify_predicate(&mut self, literal: Literal, predicate: Predicate) {
        match self.internal_proof {
            Some(ProofImpl::CpProof {
                ref mut proof_atomics,
                ..
            }) => proof_atomics.reify_predicate(literal, predicate),
            Some(ProofImpl::BlockProof(ref mut proof)) => {
                proof.proof_atomics.reify_predicate(literal, predicate)
            }
            _ => {}
        }
    }

    pub(crate) fn is_logging_proof(&self) -> bool {
        self.internal_proof.is_some()
    }

    fn verify_deduction_at_runtime(
        &mut self,
        premises: impl IntoIterator<Item = Predicate> + Clone,
    ) {
        let (mut ordered, rest): (Vec<_>, Vec<_>) = self
            .supporting_inferences
            .iter()
            .cloned()
            .zip(self.supporting_inferences_at_root.iter().copied())
            .rev()
            .partition(|(_, at_root)| *at_root);
        ordered.extend(rest);
        match verify_deduction(
            premises.clone(),
            ordered.into_iter().map(|(inference, _)| inference),
        ) {
            Ok(_) => {
                self.supporting_inferences.clear();
                self.supporting_inferences_at_root.clear();
            }
            Err(InvalidDeduction(ignored_inferences)) => {
                eprintln!("Supporting inferences:");
                for inference in self.supporting_inferences.iter() {
                    eprintln!("{:?} -> {:?}", inference.premises, inference.consequent);
                }

                if !ignored_inferences.is_empty() {
                    eprintln!("Ignored inferences:");
                    for ignored_inference in ignored_inferences {
                        eprintln!(
                            "{:?} -> {:?}",
                            ignored_inference.inference.premises,
                            ignored_inference.inference.consequent
                        );
                    }
                }

                panic!(
                    "Failed to verify deduction: {:?} -> false",
                    itertools::join(premises, " & ")
                );
            }
        }
    }
}

/// Returns `true` if the given predicate is over a variable without a name, a constant of the
/// model, or over the domain of the trivially true predicate, which is named but fixed; a proof
/// in the block format leaves both out.
fn is_unnamed(predicate: Predicate, variable_names: &VariableNames) -> bool {
    predicate.get_domain() == Predicate::trivially_true().get_domain()
        || variable_names
            .get_int_name(predicate.get_domain())
            .is_none()
}

/// Returns `true` if the given predicate is likely a constant from the model that was unnamed.
fn is_likely_a_constant(
    predicate: Predicate,
    variable_names: &VariableNames,
    assignments: &Assignments,
) -> bool {
    let domain = predicate.get_domain();

    let is_fixed =
        assignments.get_initial_lower_bound(domain) == assignments.get_initial_upper_bound(domain);

    let is_unnamed = variable_names.get_int_name(domain).is_none();

    is_fixed || is_unnamed
}

/// A wrapper around either a file or a gzipped file.
///
/// Whether or not we will gzip on the fly is a runtime decision, and this wrapper is the [`Write`]
/// implementation that [`ProofWriter`] will write to.
#[derive(Debug)]
enum Sink {
    File(File),
    GzippedFile(flate2::write::GzEncoder<File>),
}

impl Write for Sink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Sink::File(file) => file.write(buf),
            Sink::GzippedFile(gz_encoder) => gz_encoder.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Sink::File(file) => file.flush(),
            Sink::GzippedFile(gz_encoder) => gz_encoder.flush(),
        }
    }
}

/// The sink of a CP proof. A croc scaffold starts with its preamble, which the solver knows only
/// once the model is compiled, so what is logged before it is held until then.
#[derive(Debug)]
struct CpSink {
    sink: Sink,
    /// What was logged before the preamble of a croc scaffold; `None` once the preamble is
    /// written, and for other proofs.
    held: Option<Vec<u8>>,
}

impl CpSink {
    /// Write the preamble and the marker line, then what was held.
    fn release(&mut self, preamble: &str) -> std::io::Result<()> {
        let Some(held) = self.held.take() else {
            return Ok(());
        };
        self.sink.write_all(preamble.as_bytes())?;
        if !preamble.is_empty() && !preamble.ends_with('\n') {
            self.sink.write_all(b"\n")?;
        }
        self.sink.write_all(SCAFFOLD_NOGOODS_MARKER.as_bytes())?;
        self.sink.write_all(b"\n")?;
        self.sink.write_all(&held)
    }
}

impl Write for CpSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self.held.as_mut() {
            Some(held) => {
                held.extend_from_slice(buf);
                Ok(buf.len())
            }
            None => self.sink.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self.held {
            Some(_) => Ok(()),
            None => self.sink.flush(),
        }
    }
}

impl Drop for CpSink {
    /// A scaffold whose preamble was never given states no preprocessing.
    fn drop(&mut self) {
        let _ = self.release("");
    }
}

#[derive(Debug)]
#[allow(
    clippy::large_enum_variant,
    reason = "there will only ever be one per solver"
)]
#[allow(
    variant_size_differences,
    reason = "there will only ever be one per solver"
)]
#[allow(
    clippy::enum_variant_names,
    reason = "the variants name the kinds of proof"
)]
enum ProofImpl {
    CpProof {
        writer: ProofWriter<CpSink, i32>,
        // If propagation hints are enabled, this is a buffer used to record propagations in the
        // order they can be applied to derive the next nogood.
        //
        // Every element is optional, because when we log a domain inference multiple
        // times, we have to move the corresponding constraint tag to the end of the hint.
        // We do this by replacing the existing value with `None` and appending `Some` at
        // the end.
        propagation_order_hint: Option<Vec<Option<ConstraintTag>>>,
        proof_atomics: ProofAtomics,
        /// The domain inferences that are logged for the next deduction. For each
        /// inference we keep the index in the propagation order hint.
        logged_domain_inferences: HashMap<Predicate, usize>,
        /// Whether a nogood keeps its atoms over named variables with a fixed domain, as a croc
        /// scaffold does: its reader holds them as variables. Otherwise they are left out, as
        /// for the DRCP checker.
        keeps_fixed_variables: bool,
    },
    DimacsProof(DimacsProof<File>),
    BlockProof(Box<BlockProof>),
}

/// An inference kept until the nogood it contributes to is logged.
#[derive(Debug)]
struct BufferedInference {
    premises: Vec<Predicate>,
    consequent: Option<Predicate>,
    /// The constraint the inference is from; [`None`] for the initial domains.
    constraint: Option<ConstraintTag>,
    label: Arc<str>,
}

/// The objective variable of the model as `scale * representative + offset`, stated by the
/// constraint `evidence`.
#[derive(Debug)]
struct ObjectiveView {
    objective: Arc<str>,
    representative: Arc<str>,
    scale: i64,
    offset: i64,
    evidence: NonZero<u32>,
}

/// A `set_in` constraint with its set, as ranges of values.
type SetInRanges = (ConstraintTag, Vec<(i32, i32)>);

/// The state of a proof written in the block format.
#[derive(Debug)]
struct BlockProof {
    writer: BlockProofWriter<std::io::BufWriter<Sink>, i32>,
    /// The inferences logged since the last nogood, by the tag they were logged under.
    inferences: HashMap<ConstraintTag, BufferedInference>,
    /// The tags of those inferences in the reverse of the order they apply in; an entry is
    /// emptied when an initial-domain inference is logged again and moves to the end.
    propagation_order: Vec<Option<ConstraintTag>>,
    logged_domain_inferences: HashMap<Predicate, usize>,
    proof_atomics: ProofAtomics,
    /// The identifier of every learned nogood in the proof, which numbers derived constraints
    /// on from the model's and the head's.
    derived_identifiers: HashMap<ConstraintTag, NonZero<u32>>,
    next_identifier: u32,
    /// The identifier and the atomic constraints of the last nogood written.
    last_nogood: Option<(NonZero<u32>, Vec<Predicate>)>,
    /// The rule for the inferences from a model constraint, where it is not the label.
    rule_of_constraint: HashMap<ConstraintTag, Arc<str>>,
    /// For a reified constraint Pumpkin decomposes, the indicator's true predicate and the rule
    /// that licenses an inference whose nogood contains it, or contains its negation.
    rule_by_indicator_side: HashMap<ConstraintTag, (Predicate, Arc<str>, Arc<str>)>,
    /// The elements of a model constraint whose inferences name positions in it.
    positions_of_constraint: HashMap<ConstraintTag, Vec<AffineView<DomainId>>>,
    /// The reified constraints whose inferences on the side of the indicator state the bounds
    /// that fix their 0/1 variables.
    fixing_bounds_when_true: HashSet<ConstraintTag>,
    /// The head and the steps after it, held until the first nogood.
    held_lines: Vec<String>,
    /// The reference of every atom written so far, by the predicate it is written for, so that
    /// an atom written again costs one lookup rather than its name.
    atom_references: HashMap<Predicate, NonZero<i32>>,
    /// The reference of every rule written so far.
    label_references: HashMap<Box<str>, NonZero<u32>>,
    /// The premises of an inference seen so far, reused from one inference to the next.
    seen_premises: HashSet<Predicate>,
    /// Whether a learned nogood is written as a resolution chain rather than a refutation.
    resolution_chains: bool,
    /// The constraints the checker reads as reified (`true`) or half-reified (`false`).
    reification: HashMap<ConstraintTag, bool>,
    /// The objective as a view of the representative the solver bounds, when it is one.
    objective_view: Option<ObjectiveView>,
    /// For a domain that `set_in` constraints restrict, each constraint and its set as ranges.
    set_in_domains: HashMap<DomainId, Vec<SetInRanges>>,
    /// For a fresh 0/1 variable of a decomposition, the constraint that states its bounds.
    selector_bounds: HashMap<DomainId, ConstraintTag>,
    /// The constraints in [`BlockProof::selector_bounds`], cited under `initial_domain`.
    selector_bound_tags: HashSet<ConstraintTag>,
}

impl Drop for BlockProof {
    /// A proof that stops before its first nogood still states the head and the steps.
    fn drop(&mut self) {
        let _ = self.write_held_lines();
    }
}

impl BlockProof {
    /// Buffer the inference that the predicate holds by the initial domains, or by the
    /// constraint that states the domain of the variable, under the tag, or move it to the end
    /// of the propagation order when it is buffered already. Returns its tag and the predicates
    /// it rests on, which the caller logs after it.
    fn log_domain_inference(
        &mut self,
        predicate: Predicate,
        inference_tag: ConstraintTag,
    ) -> (ConstraintTag, Vec<Predicate>) {
        if let Some(index) = self.logged_domain_inferences.get(&predicate).copied() {
            let tag = self.propagation_order[index]
                .take()
                .expect("the logged_domain_inferences always points to some index");
            self.propagation_order.push(Some(tag));
            let _ = self
                .logged_domain_inferences
                .insert(predicate, self.propagation_order.len() - 1);
            let premises = self
                .inferences
                .get(&tag)
                .map(|inference| inference.premises.clone())
                .unwrap_or_default();
            return (tag, premises);
        }

        let set_in = self
            .set_in_domains
            .get(&predicate.get_domain())
            .and_then(|sets| {
                sets.iter()
                    .find(|(_, ranges)| {
                        ranges
                            .iter()
                            .all(|&(lower, upper)| holds_throughout(predicate, lower, upper))
                    })
                    .map(|(tag, _)| *tag)
            });
        // A set that does not imply the predicate on its own may with the declared domain, as
        // the set 1..31 and the declared domain {0, 31, 32} imply [x >= 31]: the set gives the
        // bounds of its hull, and the declared domain the predicate within them.
        let premises = match set_in {
            Some(_) => vec![],
            None => self
                .set_in_domains
                .get(&predicate.get_domain())
                .and_then(|sets| sets.first())
                .and_then(|(_, ranges)| {
                    let lower = ranges.iter().map(|&(lower, _)| lower).min()?;
                    let upper = ranges.iter().map(|&(_, upper)| upper).max()?;
                    let domain = predicate.get_domain();
                    Some(vec![
                        predicate![domain >= lower],
                        predicate![domain <= upper],
                    ])
                })
                .unwrap_or_default(),
        };
        let selector_bounds = self.selector_bounds.get(&predicate.get_domain()).copied();
        let (constraint, label) = match (set_in, selector_bounds) {
            (Some(tag), _) => (Some(tag), Arc::from("set_in")),
            (None, Some(tag)) => (Some(tag), Arc::from("initial_domain")),
            (None, None) => (None, Arc::from("initial_domain")),
        };
        let _ = self.inferences.insert(
            inference_tag,
            BufferedInference {
                premises: premises.clone(),
                consequent: Some(predicate),
                constraint,
                label,
            },
        );
        self.propagation_order.push(Some(inference_tag));
        let _ = self
            .logged_domain_inferences
            .insert(predicate, self.propagation_order.len() - 1);
        (inference_tag, premises)
    }

    /// The line of the fields, with each atom, label and variable written as the proof refers
    /// to it; the atoms and labels it uses are declared first.
    fn render_head_line(
        &mut self,
        fields: &[HeadField],
        variable_names: &VariableNames,
    ) -> std::io::Result<String> {
        let mut line = String::new();
        for field in fields {
            match field {
                HeadField::Text(text) => line.push_str(text),
                HeadField::Atom(predicate) => {
                    let atomic = self
                        .proof_atomics
                        .map_predicate_to_proof_atomic(*predicate, variable_names);
                    let code = self.writer.atomic_reference(atomic)?;
                    line.push_str(&code.to_string());
                }
                HeadField::Label(label) => {
                    let code = self.writer.label_reference(label)?;
                    line.push_str(&code.to_string());
                }
                HeadField::Name(domain) => line.push_str(
                    variable_names
                        .get_int_name(*domain)
                        .expect("a variable in a proof line is named"),
                ),
            }
        }
        Ok(line)
    }

    /// The reference of the atom written for the predicate, declared first if it is new.
    fn atom_reference(
        &mut self,
        predicate: Predicate,
        variable_names: &VariableNames,
    ) -> std::io::Result<NonZero<i32>> {
        if let Some(&reference) = self.atom_references.get(&predicate) {
            return Ok(reference);
        }
        let atomic = self
            .proof_atomics
            .map_predicate_to_proof_atomic(predicate, variable_names);
        let reference = self.writer.atomic_reference(atomic)?;
        let _ = self.atom_references.insert(predicate, reference);
        Ok(reference)
    }

    /// The reference of the rule, declared first if it is new.
    fn label_reference(&mut self, rule: &str) -> std::io::Result<NonZero<u32>> {
        if let Some(&reference) = self.label_references.get(rule) {
            return Ok(reference);
        }
        let reference = self.writer.label_reference(rule)?;
        let _ = self.label_references.insert(Box::from(rule), reference);
        Ok(reference)
    }

    /// Write the held head and steps before the next nogood or the conclusion.
    fn write_held_lines(&mut self) -> std::io::Result<()> {
        for line in std::mem::take(&mut self.held_lines) {
            self.writer.log_line(&line)?;
        }
        Ok(())
    }

    /// Write the nogood as a refutation by the inferences logged for it, in the order they
    /// propagate. The checker applies every inference it is given, so an inference whose
    /// premises do not hold at its turn, which Pumpkin's hints may contain, is left out, and
    /// none after the conflict is written.
    fn log_refutation(
        &mut self,
        constraint_tag: ConstraintTag,
        premises: impl IntoIterator<Item = Predicate>,
        root_explanations: &HashSet<ConstraintTag>,
        variable_names: &VariableNames,
        assignments: &Assignments,
    ) -> std::io::Result<()> {
        self.write_held_lines()?;
        let identifier =
            NonZero::new(self.next_identifier).expect("derived identifiers start at one");
        self.next_identifier += 1;

        let is_constant = |predicate: Predicate| is_unnamed(predicate, variable_names);
        let nogood: Vec<Predicate> = premises
            .into_iter()
            .map(|predicate| self.proof_atomics.canonical(predicate))
            .filter(|&predicate| !is_constant(predicate))
            .collect();

        let mut kept: Vec<KeptInference> = vec![];
        let mut reached_conflict = false;

        let mut seen = std::mem::take(&mut self.seen_premises);
        if let Ok(mut state) =
            VariableState::prepare_for_conflict_check(nogood.iter().copied(), None)
        {
            let order = roots_first(
                self.propagation_order
                    .iter()
                    .rev()
                    .flatten()
                    .copied()
                    .collect(),
                root_explanations,
            );
            for tag in order {
                let Some(inference) = self.inferences.get(&tag) else {
                    continue;
                };
                // An explanation can repeat a premise, which a nogood states once.
                seen.clear();
                let mut inference_premises: Vec<Predicate> = inference
                    .premises
                    .iter()
                    .map(|&predicate| self.proof_atomics.canonical(predicate))
                    .filter(|&predicate| !is_constant(predicate) && seen.insert(predicate))
                    .collect();
                if !inference_premises
                    .iter()
                    .all(|premise| state.is_true(premise))
                {
                    continue;
                }
                let consequent = inference
                    .consequent
                    .map(|predicate| self.proof_atomics.canonical(predicate));
                // The negation of the consequent ends the nogood, and is not repeated before it.
                if let Some(consequent) = consequent {
                    inference_premises.retain(|&premise| premise != !consequent);
                }
                let narrows = !matches!(
                    consequent,
                    Some(consequent) if !is_constant(consequent) && state.is_true(&consequent)
                );
                if narrows {
                    let mut bounds = self.bounds_fixing_terms(
                        inference.constraint,
                        &inference_premises,
                        consequent,
                        assignments,
                    );
                    bounds.extend(self.declared_bounds_relied_on(
                        inference.constraint,
                        &inference_premises,
                        consequent,
                        assignments,
                    ));
                    for bound in bounds {
                        if !state.is_true(&bound) {
                            let _ = state.apply(&bound);
                            kept.push((vec![], Some(bound), None, Arc::from("initial_domain")));
                        }
                        inference_premises.push(bound);
                    }
                }
                match consequent {
                    // An inference whose consequent already holds narrows nothing.
                    Some(consequent) if !is_constant(consequent) && state.is_true(&consequent) => {}
                    Some(consequent) if !is_constant(consequent) => {
                        let consistent = state.apply(&consequent);
                        kept.push((
                            inference_premises,
                            Some(consequent),
                            inference.constraint,
                            Arc::clone(&inference.label),
                        ));
                        if !consistent {
                            reached_conflict = true;
                            break;
                        }
                    }
                    Some(consequent) => {
                        // A bound on a constant either holds already, and changes nothing, or
                        // fails, and then the premises are a conflict on their own.
                        let value = assignments.get_initial_lower_bound(consequent.get_domain());
                        if !holds_for_value(consequent, value) {
                            kept.push((
                                inference_premises,
                                None,
                                inference.constraint,
                                Arc::clone(&inference.label),
                            ));
                            reached_conflict = true;
                            break;
                        }
                    }
                    None => {
                        kept.push((
                            inference_premises,
                            None,
                            inference.constraint,
                            Arc::clone(&inference.label),
                        ));
                        reached_conflict = true;
                        break;
                    }
                }
            }
        } else {
            // The nogood's atoms cannot all hold, so it needs no inference.
            reached_conflict = true;
        }

        self.seen_premises = seen;

        if !reached_conflict {
            eprintln!("warning: the inferences logged for nogood {identifier} reach no conflict");
        }

        let chain = if self.resolution_chains && reached_conflict && !kept.is_empty() {
            let chain = resolution_chain(&nogood, &kept);
            if chain.is_none() {
                eprintln!(
                    "warning: no resolution chain reaches nogood {identifier}; it is written as a \
                     refutation"
                );
            }
            chain
        } else {
            None
        };

        let mut inferences = Vec::with_capacity(kept.len());
        for (inference_premises, consequent, constraint, label) in kept {
            let mut atoms = Vec::with_capacity(inference_premises.len() + 1);
            for &predicate in inference_premises.iter() {
                atoms.push(self.atom_reference(predicate, variable_names)?);
            }
            if let Some(consequent) = consequent {
                atoms.push(self.atom_reference(!consequent, variable_names)?);
            }
            let hint = constraint
                .and_then(|tag| self.positions_of_constraint.get(&tag))
                .map(|elements| {
                    hint_positions(
                        elements,
                        &inference_premises,
                        consequent,
                        variable_names,
                        assignments,
                    )
                })
                .unwrap_or_default();
            let label = constraint
                .and_then(|tag| self.rule_of_constraint.get(&tag).cloned())
                .unwrap_or(label);
            let label = match constraint.and_then(|tag| self.rule_by_indicator_side.get(&tag)) {
                Some((indicator, rule_when_true, rule_when_false)) => {
                    let indicator = self.proof_atomics.canonical(*indicator);
                    let in_nogood = |predicate: Predicate| {
                        inference_premises.contains(&predicate) || consequent == Some(!predicate)
                    };
                    if in_nogood(indicator) {
                        Arc::clone(rule_when_true)
                    } else if in_nogood(!indicator) {
                        Arc::clone(rule_when_false)
                    } else {
                        label
                    }
                }
                None => label,
            };
            // The initial domains are constraint 0, whichever constraint the solver's inference
            // came from, except for the bounds a decomposition states for its fresh variables.
            let constraint = constraint.filter(|tag| {
                label.as_ref() != "initial_domain" || self.selector_bound_tags.contains(tag)
            });
            let rule = rule_name(&label);
            let label = match constraint.and_then(|tag| self.reification.get(&tag)) {
                Some(true) => self.label_reference(&format!("reified({rule})"))?,
                Some(false) => self.label_reference(&format!("half_reified({rule})"))?,
                None => self.label_reference(rule)?,
            };
            let constraint = constraint.map(|tag| {
                self.derived_identifiers
                    .get(&tag)
                    .copied()
                    .unwrap_or_else(|| NonZero::<u32>::from(tag))
            });
            inferences.push(ReferencedInference {
                nogood: atoms,
                constraint,
                label,
                hint,
            });
        }

        match chain {
            Some((steps, derived)) => {
                let mut kept_inferences: Vec<Option<_>> =
                    inferences.into_iter().map(Some).collect();
                let mut chain_inferences = Vec::with_capacity(steps.len());
                for step in steps {
                    match step {
                        ChainStep::Kept(position) => chain_inferences.push(
                            kept_inferences[position]
                                .take()
                                .expect("a chain resolves on each inference once"),
                        ),
                        ChainStep::Implied(premises, atom) => {
                            let mut nogood = Vec::with_capacity(premises.len() + 1);
                            for predicate in premises.into_iter().chain(std::iter::once(!atom)) {
                                nogood.push(self.atom_reference(predicate, variable_names)?);
                            }
                            chain_inferences.push(ReferencedInference {
                                nogood,
                                constraint: None,
                                label: self.label_reference("initial_domain")?,
                                hint: vec![],
                            });
                        }
                    }
                }
                let mut derived_nogood = Vec::with_capacity(derived.len());
                for predicate in derived {
                    derived_nogood.push(self.atom_reference(predicate, variable_names)?);
                }
                self.writer
                    .log_referenced_block(identifier, &derived_nogood, &chain_inferences)?;
            }
            None => {
                let mut written = Vec::with_capacity(nogood.len());
                for &predicate in nogood.iter() {
                    written.push(self.atom_reference(predicate, variable_names)?);
                }
                self.writer
                    .log_referenced_refutation(identifier, &written, &inferences)?;
            }
        }

        let _ = self.derived_identifiers.insert(constraint_tag, identifier);
        self.last_nogood = Some((identifier, nogood));
        self.inferences.clear();
        self.propagation_order.clear();
        self.logged_domain_inferences.clear();

        Ok(())
    }

    /// The bounds the declared domains give that an inference from a constraint registered with
    /// [`ProofLog::state_fixing_bounds_when_true`] needs, when its nogood contains the indicator:
    /// for every other atom `[d >= 1]` or `[d <= 0]` over a 0/1 variable, the bound `[d <= 1]` or
    /// `[d >= 0]` that fixes the variable, unless the inference states it already.
    /// The declared bounds an inference from a `set_in` constraint relies on without stating
    /// them: for a variable whose atoms in the nogood can hold together but not within its
    /// declared bounds, the bound or bounds that make them fail, such as `[x >= 1]` beside
    /// `[x <= 1] [x != 1]` when `x` is declared from 1.
    fn declared_bounds_relied_on(
        &self,
        constraint: Option<ConstraintTag>,
        premises: &[Predicate],
        consequent: Option<Predicate>,
        assignments: &Assignments,
    ) -> Vec<Predicate> {
        let is_set_in = constraint
            .and_then(|tag| self.rule_of_constraint.get(&tag))
            .is_some_and(|rule| rule.as_ref() == "set_in");
        if !is_set_in {
            return vec![];
        }
        let atoms: Vec<Predicate> = premises
            .iter()
            .copied()
            .chain(consequent.map(|predicate| !predicate))
            .collect();
        let mut domains: Vec<DomainId> = atoms.iter().map(|atom| atom.get_domain()).collect();
        domains.sort_unstable();
        domains.dedup();
        let consistent = |atoms: &[Predicate]| {
            VariableState::prepare_for_conflict_check(atoms.iter().copied(), None).is_ok()
        };
        let mut bounds = vec![];
        for domain in domains {
            let on_domain: Vec<Predicate> = atoms
                .iter()
                .copied()
                .filter(|atom| atom.get_domain() == domain)
                .collect();
            if !consistent(&on_domain) {
                continue;
            }
            let lower = predicate![domain >= assignments.get_initial_lower_bound(domain)];
            let upper = predicate![domain <= assignments.get_initial_upper_bound(domain)];
            let with = |extra: &[Predicate]| {
                let mut all = on_domain.clone();
                all.extend_from_slice(extra);
                consistent(&all)
            };
            if with(&[lower, upper]) {
                continue;
            }
            if !with(&[lower]) {
                bounds.push(lower);
            } else if !with(&[upper]) {
                bounds.push(upper);
            } else {
                bounds.extend([lower, upper]);
            }
        }
        bounds.retain(|bound| !premises.contains(bound));
        bounds
    }

    fn bounds_fixing_terms(
        &self,
        constraint: Option<ConstraintTag>,
        premises: &[Predicate],
        consequent: Option<Predicate>,
        assignments: &Assignments,
    ) -> Vec<Predicate> {
        let Some(tag) = constraint.filter(|tag| self.fixing_bounds_when_true.contains(tag)) else {
            return vec![];
        };
        let Some((indicator, _, _)) = self.rule_by_indicator_side.get(&tag) else {
            return vec![];
        };
        let indicator = self.proof_atomics.canonical(*indicator);
        let atoms: Vec<Predicate> = premises
            .iter()
            .copied()
            .chain(consequent.map(|predicate| !predicate))
            .collect();
        if !atoms.contains(&indicator) {
            return vec![];
        }
        atoms
            .iter()
            .filter(|&&atom| atom != indicator)
            .filter_map(|&atom| {
                let domain = atom.get_domain();
                if assignments.get_initial_lower_bound(domain) != 0
                    || assignments.get_initial_upper_bound(domain) != 1
                {
                    return None;
                }
                match (atom.get_predicate_type(), atom.get_right_hand_side()) {
                    (PredicateType::LowerBound, 1) => Some(predicate![domain <= 1]),
                    (PredicateType::UpperBound, 0) => Some(predicate![domain >= 0]),
                    _ => None,
                }
            })
            .filter(|bound| !premises.contains(bound))
            .collect()
    }

    /// Conclude an optimisation with the bound on the objective: the improvement on the bound
    /// is assumed with an `o` line, and a refutation derives the empty nogood from the
    /// assumption and the unit nogood of the improvement, which the solver derived last. Without
    /// that nogood, the initial domains are cited instead. When the solver bounds a
    /// representative of which the objective is a view, the constraint stating the view carries
    /// the assumption over to the representative.
    fn conclude_optimal(
        &mut self,
        objective_bound: Predicate,
        variable_names: &VariableNames,
    ) -> std::io::Result<()> {
        self.write_held_lines()?;
        let bound = self
            .proof_atomics
            .map_predicate_to_proof_atomic(objective_bound, variable_names);
        let is_lower = match bound.comparison {
            IntComparison::GreaterEqual => true,
            IntComparison::LessEqual => false,
            IntComparison::Equal | IntComparison::NotEqual => {
                unreachable!("a bound on the objective is a lower or an upper bound")
            }
        };
        let view = self
            .objective_view
            .as_ref()
            .filter(|view| view.representative.as_ref() == bound.name);
        // The bound on the objective: through a view with a negative scale, a lower bound on the
        // representative is an upper bound on the objective.
        let (is_lower, bound_value) = match view {
            Some(view) => (
                is_lower == (view.scale > 0),
                view.scale * i64::from(bound.value) + view.offset,
            ),
            None => (is_lower, i64::from(bound.value)),
        };
        let conclusion_line = if is_lower {
            format!("c LOWER_BOUND {bound_value}")
        } else {
            format!("c UPPER_BOUND {bound_value}")
        };
        let link = view.map(|view| (Arc::clone(&view.objective), view.evidence));

        let improvement_predicate = self.proof_atomics.canonical(!objective_bound);
        let bound_reference = self.writer.atomic_reference(bound)?;
        let improvement = self.writer.atomic_reference(!bound)?;

        let unit = self
            .last_nogood
            .as_ref()
            .filter(|(_, nogood)| nogood.as_slice() == [improvement_predicate])
            .map(|(identifier, _)| *identifier);
        if unit.is_none() {
            eprintln!(
                "warning: the last nogood is not the unit nogood of the improvement; the proof \
                 cites the initial domains for it"
            );
        }

        let assumed = self.next_identifier;
        let empty = self.next_identifier + 1;
        self.next_identifier += 2;

        // The declarations come before the refutation, which a declaration would end.
        let initial_domain = self.writer.label_reference("initial_domain")?;
        let mut inferences = vec![];
        match link {
            Some((objective, evidence)) => {
                let value = i32::try_from(bound_value).expect("the objective's bound fits");
                let (holds, assumed_atomic) = if is_lower {
                    (
                        IntAtomic::new(&*objective, IntComparison::GreaterEqual, value),
                        IntAtomic::new(&*objective, IntComparison::LessEqual, value - 1),
                    )
                } else {
                    (
                        IntAtomic::new(&*objective, IntComparison::LessEqual, value),
                        IntAtomic::new(&*objective, IntComparison::GreaterEqual, value + 1),
                    )
                };
                let holds = self.writer.atomic_reference(holds)?;
                let assumed_atomic = self.writer.atomic_reference(assumed_atomic)?;
                let linear_bounds = self.writer.label_reference("linear_bounds")?;
                inferences.push(format!("  {holds} c:{assumed} l:{initial_domain}"));
                inferences.push(format!(
                    "  {assumed_atomic} {bound_reference} c:{evidence} l:{linear_bounds}"
                ));
            }
            None => inferences.push(format!(
                "  {} c:{assumed} l:{initial_domain}",
                -improvement.get()
            )),
        }
        match unit {
            Some(unit) => {
                let nogood = self.writer.label_reference("nogood")?;
                inferences.push(format!("  {improvement} c:{unit} l:{nogood}"));
            }
            None => inferences.push(format!("  {improvement} c:0 l:{initial_domain}")),
        }

        self.writer
            .log_line(&format!("o {assumed} {bound_value}"))?;
        self.writer.log_line(&format!("g {empty}"))?;
        for inference in inferences {
            self.writer.log_line(&inference)?;
        }
        self.writer.log_line(&conclusion_line)
    }
}

/// The rule of fzn-croc-check that licenses an inference Pumpkin labels as given. The binary
/// propagators of an equality and a disequality explain what the rules for a linear equality of
/// two terms and a linear disequality check, and an inference the domains imply is checked by
/// the rule of the initial domains.
pub fn rule_name(label: &str) -> &str {
    match label {
        "domain_implication" => "initial_domain",
        "binary_equals" => "linear_equal_binary",
        "binary_not_equals" => "linear_not_equal",
        "linear_not_equals" => "linear_not_equal",
        other => other,
    }
}

/// The positions of the elements an inference reads: every element over a variable the
/// inference mentions, and every constant element whose value an element over such a variable
/// takes at one of the inference's atoms.
fn hint_positions(
    elements: &[AffineView<DomainId>],
    premises: &[Predicate],
    consequent: Option<Predicate>,
    variable_names: &VariableNames,
    assignments: &Assignments,
) -> Vec<u32> {
    let atoms: Vec<Predicate> = premises.iter().copied().chain(consequent).collect();
    let mentioned = |element: &AffineView<DomainId>| {
        atoms
            .iter()
            .any(|atom| atom.get_domain() == *element.inner())
    };
    let values: Vec<i64> = elements
        .iter()
        .flat_map(|element| {
            atoms
                .iter()
                .filter(|atom| atom.get_domain() == *element.inner())
                .map(|atom| {
                    i64::from(element.scale()) * i64::from(atom.get_right_hand_side())
                        + i64::from(element.view_offset())
                })
                .collect::<Vec<_>>()
        })
        .collect();
    elements
        .iter()
        .enumerate()
        .filter(|(_, element)| {
            if variable_names.get_int_name(*element.inner()).is_none()
                || *element.inner() == Predicate::trivially_true().get_domain()
            {
                let value = i64::from(element.scale())
                    * i64::from(assignments.get_initial_lower_bound(*element.inner()))
                    + i64::from(element.view_offset());
                values.contains(&value)
            } else {
                mentioned(element)
            }
        })
        .map(|(position, _)| position as u32)
        .collect()
}

/// The tags in the order they apply, with the explanations of root predicates moved before the
/// rest; each group keeps its order.
fn roots_first(
    order: Vec<ConstraintTag>,
    root_explanations: &HashSet<ConstraintTag>,
) -> Vec<ConstraintTag> {
    let (mut roots, rest): (Vec<_>, Vec<_>) = order
        .into_iter()
        .partition(|tag| root_explanations.contains(tag));
    roots.extend(rest);
    roots
}

/// An inference kept for a nogood: its premises, its consequent, the constraint it is from and
/// its label.
type KeptInference = (
    Vec<Predicate>,
    Option<Predicate>,
    Option<ConstraintTag>,
    Arc<str>,
);

/// One inference of a resolution chain.
enum ChainStep {
    /// The kept inference at this position.
    Kept(usize),
    /// The nogood of the premises and the negation of the atom, which the premises imply.
    Implied(Vec<Predicate>, Predicate),
}

/// Whether the atoms imply the atom.
fn implies(atoms: impl IntoIterator<Item = Predicate>, atom: Predicate) -> bool {
    VariableState::prepare_for_conflict_check(atoms, None).is_ok_and(|state| state.is_true(&atom))
}

/// The resolution chain that derives the nogood from the kept inferences, which refute it in the
/// order they propagate. The chain starts at the conflict, the last inference, and resolves the
/// propagations in the reverse order, each on its consequent, once an atom of the nogood reached
/// so far holds because of it. An atom that the consequent makes hold only together with the
/// atoms that held on its variable before, or that is not the consequent itself, is first
/// replaced by them through the nogood the domain implies. The atoms left at the end that are
/// not in the nogood are replaced in the same way by the nogood's atoms on their variable.
///
/// Returns the steps and the nogood derived, which is part of `nogood`, or `None` when the chain
/// does not reach one.
fn resolution_chain(
    nogood: &[Predicate],
    kept: &[KeptInference],
) -> Option<(Vec<ChainStep>, Vec<Predicate>)> {
    // For each variable, the atoms that hold on it in the order they come to hold: the nogood's
    // first, at position -1, then the consequents.
    let mut sources: HashMap<DomainId, Vec<(isize, Predicate)>> = HashMap::default();
    for &atom in nogood {
        sources
            .entry(atom.get_domain())
            .or_default()
            .push((-1, atom));
    }
    for (position, (_, consequent, _, _)) in kept.iter().enumerate() {
        if let Some(consequent) = consequent {
            sources
                .entry(consequent.get_domain())
                .or_default()
                .push((position as isize, *consequent));
        }
    }
    let held_before = |domain: DomainId, position: isize| {
        sources
            .get(&domain)
            .into_iter()
            .flatten()
            .filter(move |(source, _)| *source < position)
            .map(|(_, atom)| *atom)
    };

    let mut reached: HashMap<DomainId, Vec<Predicate>> = HashMap::default();
    let add = |reached: &mut HashMap<DomainId, Vec<Predicate>>, atom: Predicate| {
        let bucket = reached.entry(atom.get_domain()).or_default();
        if !bucket.contains(&atom) {
            bucket.push(atom);
        }
    };
    let remove = |reached: &mut HashMap<DomainId, Vec<Predicate>>, atom: Predicate| {
        if let Some(bucket) = reached.get_mut(&atom.get_domain()) {
            bucket.retain(|other| *other != atom);
        }
    };

    let last = kept.len() - 1;
    let (premises, consequent, _, _) = &kept[last];
    for &atom in premises.iter().chain(consequent.map(|c| !c).iter()) {
        add(&mut reached, atom);
    }
    let mut steps = vec![ChainStep::Kept(last)];

    for position in (0..last).rev() {
        let Some(consequent) = kept[position].1 else {
            continue;
        };
        let domain = consequent.get_domain();
        let Some(bucket) = reached.get(&domain).cloned() else {
            continue;
        };
        let mut resolves = false;
        let earlier: Vec<Predicate> = held_before(domain, position as isize).collect();
        let before = VariableState::prepare_for_conflict_check(earlier.iter().copied(), None).ok();
        for atom in bucket {
            if before.as_ref().is_some_and(|state| state.is_true(&atom)) {
                continue;
            }
            resolves = true;
            if atom == consequent {
                continue;
            }
            // The consequent and the atoms that held on the variable before, the latest first,
            // as few as imply the atom.
            let mut premises = vec![consequent];
            for &earlier_atom in earlier.iter().rev() {
                if implies(premises.iter().copied(), atom) {
                    break;
                }
                premises.push(earlier_atom);
            }
            remove(&mut reached, atom);
            for &premise in &premises {
                add(&mut reached, premise);
            }
            steps.push(ChainStep::Implied(premises, atom));
        }
        if resolves {
            remove(&mut reached, consequent);
            for &premise in &kept[position].0 {
                add(&mut reached, premise);
            }
            steps.push(ChainStep::Kept(position));
        }
    }

    let left: Vec<Predicate> = reached
        .values()
        .flatten()
        .copied()
        .filter(|atom| !nogood.contains(atom))
        .collect();
    for atom in left {
        let premises: Vec<Predicate> = held_before(atom.get_domain(), 0).collect();
        if !implies(premises.iter().copied(), atom) {
            return None;
        }
        remove(&mut reached, atom);
        for &premise in &premises {
            add(&mut reached, premise);
        }
        steps.push(ChainStep::Implied(premises, atom));
    }

    // A chain that resolves nothing derives the conflict's nogood as it is written.
    let derived: Vec<Predicate> = if steps.len() == 1 {
        premises
            .iter()
            .copied()
            .chain(consequent.map(|c| !c))
            .collect()
    } else {
        nogood
            .iter()
            .copied()
            .filter(|atom| {
                reached
                    .get(&atom.get_domain())
                    .is_some_and(|bucket| bucket.contains(atom))
            })
            .collect()
    };
    Some((steps, derived))
}

/// Whether the predicate holds for every value from `lower` to `upper`.
fn holds_throughout(predicate: Predicate, lower: i32, upper: i32) -> bool {
    let rhs = predicate.get_right_hand_side();
    match predicate.get_predicate_type() {
        PredicateType::LowerBound => lower >= rhs,
        PredicateType::UpperBound => upper <= rhs,
        PredicateType::Equal => lower == rhs && upper == rhs,
        PredicateType::NotEqual => rhs < lower || rhs > upper,
    }
}

/// Whether the predicate holds when its variable takes the value.
fn holds_for_value(predicate: Predicate, value: i32) -> bool {
    let rhs = predicate.get_right_hand_side();
    match predicate.get_predicate_type() {
        PredicateType::LowerBound => value >= rhs,
        PredicateType::UpperBound => value <= rhs,
        PredicateType::Equal => value == rhs,
        PredicateType::NotEqual => value != rhs,
    }
}
