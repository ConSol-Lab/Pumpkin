use std::{
    fs::File,
    io::{BufWriter, Write},
    num::NonZero,
};

use itertools::Itertools;

use crate::{
    predicates::Predicate,
    proof::ConstraintTag,
    variables::{AffineView, DomainId},
};

/// A wrapper around a proof file
#[derive(Debug)]
pub struct Trace {
    /// The proof file.
    proof_file: Option<BufWriter<File>>,
}

impl Trace {
    /// Trace to the given file.
    pub fn to_file(file: File) -> Trace {
        Trace {
            proof_file: Some(BufWriter::new(file)),
        }
    }

    /// Discard what is written to the trace.
    pub fn discard() -> Trace {
        Trace { proof_file: None }
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
