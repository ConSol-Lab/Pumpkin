use std::collections::HashMap;
use std::fmt::Display;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::io::Write;
use std::num::NonZero;
use std::num::NonZeroU32;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::combinator::map;
use nom::combinator::map_opt;
use nom::combinator::recognize;
use nom::combinator::value;
use nom::multi::many0_count;
use nom::multi::separated_list1;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::separated_pair;
use nom::sequence::tuple;
use nom::IResult;

use crate::reader::DrcpError;
use crate::AtomicConstraint;
use crate::BoolAtomicConstraint;
use crate::Comparison;
use crate::IntAtomicConstraint;

/// Associates literals used in the proof with the [`AtomicConstraint`] they correspond to.
///
/// The definitions can be serialized into a literal definition file with
/// [`LiteralDefinitions::write`]. The serialized form can be parsed using
/// [`LiteralDefinitions::parse`].
#[derive(Clone, Debug, Default)]
pub struct LiteralDefinitions<Identifier> {
    definitions: HashMap<NonZeroU32, Vec<AtomicConstraint<Identifier>>>,
}

impl<Identifier> LiteralDefinitions<Identifier> {
    /// Parse from a source as written to by [`LiteralDefinitions::write`].
    ///
    /// If multiple definition lines are encountered for the same literal, then the last
    /// encountered definition will overwrite the other ones.
    ///
    /// # Example
    /// ```
    /// use std::num::NonZero;
    ///
    /// use drcp_format::AtomicConstraint;
    /// use drcp_format::BoolAtomicConstraint;
    /// use drcp_format::Comparison;
    /// use drcp_format::IntAtomicConstraint;
    ///
    /// let source = r#"
    /// 20 [x1 <= 20] [x1 != 21]
    /// 5 [x2 == true]
    /// 6 [x3 >= 5] [x3 == 5]
    /// "#;
    ///
    /// let definitions = drcp_format::LiteralDefinitions::<String>::parse(source.as_bytes())
    ///     .expect("valid lits file");
    ///
    /// let atomics_for_20 = [
    ///     AtomicConstraint::Int(IntAtomicConstraint {
    ///         name: "x1".to_owned(),
    ///         comparison: Comparison::LessThanEqual,
    ///         value: 20,
    ///     }),
    ///     AtomicConstraint::Int(IntAtomicConstraint {
    ///         name: "x1".to_owned(),
    ///         comparison: Comparison::NotEqual,
    ///         value: 21,
    ///     }),
    /// ];
    /// assert_eq!(
    ///     Some(atomics_for_20.as_slice()),
    ///     definitions.get(NonZero::new(20).unwrap())
    /// );
    ///
    /// let atomics_for_5 = [AtomicConstraint::Bool(BoolAtomicConstraint {
    ///     name: "x2".to_owned(),
    ///     value: true,
    /// })];
    /// assert_eq!(
    ///     Some(atomics_for_5.as_slice()),
    ///     definitions.get(NonZero::new(5).unwrap())
    /// );
    ///
    /// let atomics_for_6 = [
    ///     AtomicConstraint::Int(IntAtomicConstraint {
    ///         name: "x3".to_owned(),
    ///         comparison: Comparison::GreaterThanEqual,
    ///         value: 5,
    ///     }),
    ///     AtomicConstraint::Int(IntAtomicConstraint {
    ///         name: "x3".to_owned(),
    ///         comparison: Comparison::Equal,
    ///         value: 5,
    ///     }),
    /// ];
    /// assert_eq!(
    ///     Some(atomics_for_20.as_slice()),
    ///     definitions.get(NonZero::new(20).unwrap())
    /// );
    /// ```
    pub fn parse(source: impl Read) -> Result<Self, DrcpError>
    where
        Identifier: for<'a> From<&'a str>,
    {
        let mut reader = BufReader::new(source);
        let mut buffer = String::new();

        let mut definitions = HashMap::new();

        'line_loop: loop {
            // Read lines until we find a non-empty line. The contents of `line` will be trimmed.
            while buffer.trim().is_empty() {
                let read_bytes = reader.read_line(&mut buffer)?;

                if read_bytes == 0 {
                    // The end of the file has been reached.
                    break 'line_loop;
                }
            }

            let (_, (id, definition)) = atomic_definition(buffer.trim())?;

            let _ = definitions.insert(id, definition);

            buffer.clear();
        }

        Ok(LiteralDefinitions { definitions })
    }

    /// Add a new definition to the literal definitions.
    pub fn add(&mut self, code: NonZeroU32, atomic: AtomicConstraint<Identifier>) {
        self.definitions.entry(code).or_default().push(atomic);
    }

    /// Get the atomic constraints for a code.
    pub fn get(&self, code: NonZeroU32) -> Option<&[AtomicConstraint<Identifier>]> {
        self.definitions.get(&code).map(|v| v.as_slice())
    }

    /// Write out all the definitions to the given sink.
    pub fn write(&mut self, mut sink: impl Write) -> std::io::Result<()>
    where
        Identifier: Display,
    {
        for (code, atomics) in self.definitions.iter() {
            write!(sink, "{code}")?;

            for atomic in atomics {
                write!(sink, " {atomic}")?;
            }

            writeln!(sink)?;
        }

        Ok(())
    }
}

/// `<unsigned non-zero integer> <list of atomics>`
fn atomic_definition<Identifier>(
    input: &str,
) -> IResult<&str, (NonZero<u32>, Vec<AtomicConstraint<Identifier>>)>
where
    Identifier: for<'a> From<&'a str>,
{
    separated_pair(variable, tag(" "), atomic_list)(input)
}

/// Parses an unsigned non-zero 32-bit integer
fn variable(input: &str) -> IResult<&str, NonZero<u32>> {
    map_opt(nom::character::complete::u32, NonZero::new)(input)
}

/// Parses space-separated list of atomic constraints.
fn atomic_list<Identifier>(input: &str) -> IResult<&str, Vec<AtomicConstraint<Identifier>>>
where
    Identifier: for<'a> From<&'a str>,
{
    separated_list1(space, atomic)(input)
}

/// Parses an atomic constraint, either boolean or integer.
fn atomic<Identifier>(input: &str) -> IResult<&str, AtomicConstraint<Identifier>>
where
    Identifier: for<'a> From<&'a str>,
{
    alt((
        map(int_atomic, AtomicConstraint::Int),
        map(bool_atomic, AtomicConstraint::Bool),
    ))(input)
}

/// `[<var> == <true | false>]`.
fn bool_atomic<Identifier>(input: &str) -> IResult<&str, BoolAtomicConstraint<Identifier>>
where
    Identifier: for<'a> From<&'a str>,
{
    let inner = separated_pair(
        identifier,
        tag(" == "),
        alt((value(true, tag("true")), value(false, tag("false")))),
    );

    delimited(
        tag("["),
        map(inner, |(name, value)| BoolAtomicConstraint { name, value }),
        tag("]"),
    )(input)
}

/// `[<var> <comparator> <value>]`.
fn int_atomic<Identifier>(input: &str) -> IResult<&str, IntAtomicConstraint<Identifier>>
where
    Identifier: for<'a> From<&'a str>,
{
    let inner = tuple((
        identifier,
        delimited(space, comparator, space),
        nom::character::complete::i64,
    ));

    delimited(
        tag("["),
        map(inner, |(name, comparison, value)| IntAtomicConstraint {
            name,
            comparison,
            value,
        }),
        tag("]"),
    )(input)
}

/// Parses an integer comparator.
fn comparator(input: &str) -> IResult<&str, Comparison> {
    alt((
        value(Comparison::Equal, tag("==")),
        value(Comparison::NotEqual, tag("!=")),
        value(Comparison::LessThanEqual, tag("<=")),
        value(Comparison::GreaterThanEqual, tag(">=")),
    ))(input)
}

/// Parses a variable name and turns it into an identifier.
///
/// Variable names be accepted by the following regex: `[A-Za-z_][A-Za-z0-9_]*`
fn identifier<Identifier>(input: &str) -> IResult<&str, Identifier>
where
    Identifier: for<'a> From<&'a str>,
{
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        Identifier::from,
    )(input)
}

/// Parses a space character.
fn space(input: &str) -> IResult<&str, &str> {
    tag(" ")(input)
}
