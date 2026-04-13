use std::{
    fs::File,
    io::{BufWriter, Write},
    num::NonZero,
};

use itertools::Itertools;

use crate::{
    predicates::Predicate,
    proof::ConstraintTag,
    state::State,
    variables::{AffineView, DomainId},
};

/// A wrapper around a proof file
#[derive(Debug)]
pub struct Trace {
    /// The proof file.
    proof_file: Option<BufWriter<File>>,

    options: TraceOptions,
}

/// How to trace the hypercube linear resolver.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TraceOptions {
    pub include_intermediate_steps: bool,
}

impl Trace {
    /// Trace to the given file.
    pub fn to_file(file: File, options: TraceOptions) -> Trace {
        Trace {
            proof_file: Some(BufWriter::new(file)),
            options,
        }
    }

    /// Discard what is written to the trace.
    pub fn discard() -> Trace {
        Trace {
            proof_file: None,
            options: TraceOptions::default(),
        }
    }

    pub fn write_variables(&mut self, state: &State) {
        let Some(writer) = self.proof_file.as_mut() else {
            return;
        };

        for (domain_id, name) in state.variable_names.named_domains() {
            if is_likely_a_constant(domain_id, state) {
                continue;
            }

            writeln!(writer, "v {domain_id} {name}").expect("failed to write to trace");
        }
    }

    /// Log an axiom.
    pub fn axiom(
        &mut self,
        hypercube: impl IntoIterator<Item = Predicate>,
        linear_terms: impl IntoIterator<Item = AffineView<DomainId>>,
        linear_rhs: i32,
    ) {
        let Some(writer) = self.proof_file.as_mut() else {
            return;
        };

        writeln!(
            writer,
            "i {h} -> {r} <= {linear_rhs}",
            h = hypercube.into_iter().format(" & "),
            r = linear_terms
                .into_iter()
                .format_with(" ", |elt, f| f(&format_args!(
                    "{} {}",
                    elt.scale, elt.inner
                )))
        )
        .expect("failed to write proof");
    }

    /// Log an intermediate deduction.
    pub fn intermediate_deduction(
        &mut self,
        hypercube: impl IntoIterator<Item = Predicate>,
        linear_terms: impl IntoIterator<Item = AffineView<DomainId>>,
        linear_rhs: i32,
    ) {
        if !self.options.include_intermediate_steps {
            return;
        }

        let Some(writer) = self.proof_file.as_mut() else {
            return;
        };

        writeln!(
            writer,
            "di {h} -> {r} <= {linear_rhs}",
            h = hypercube.into_iter().format(" & "),
            r = linear_terms
                .into_iter()
                .format_with(" ", |elt, f| f(&format_args!(
                    "{} {}",
                    elt.scale, elt.inner
                )))
        )
        .expect("failed to write proof");
    }

    /// Log a deduction.
    pub fn deduction(
        &mut self,
        constraint_tag: ConstraintTag,
        hypercube: impl IntoIterator<Item = Predicate>,
        linear_terms: impl IntoIterator<Item = AffineView<DomainId>>,
        linear_rhs: i32,
    ) {
        let Some(writer) = self.proof_file.as_mut() else {
            return;
        };

        writeln!(
            writer,
            "d {id} {h} -> {r} <= {linear_rhs}",
            id = NonZero::from(constraint_tag),
            h = hypercube.into_iter().format(" & "),
            r = linear_terms
                .into_iter()
                .format_with(" ", |elt, f| f(&format_args!(
                    "{} {}",
                    elt.scale, elt.inner
                )))
        )
        .expect("failed to write proof");
    }
}

fn is_likely_a_constant(domain_id: DomainId, state: &State) -> bool {
    let is_fixed = state.assignments.get_initial_lower_bound(domain_id)
        == state.assignments.get_initial_upper_bound(domain_id);

    if !is_fixed {
        return false;
    }

    let value = state.assignments.get_initial_upper_bound(domain_id);

    match state.variable_names.get_int_name(domain_id) {
        Some(name) => name.parse::<i32>().is_ok_and(|v| v == value),
        None => true,
    }
}
