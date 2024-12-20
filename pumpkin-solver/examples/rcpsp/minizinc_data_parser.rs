//! Generic parsing for DZN files.

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;

use log::warn;
use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::combinator::map;
use nom::multi::many0;
use nom::multi::separated_list0;
use nom::multi::separated_list1;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::IResult;
use nom::Parser;
use thiserror::Error;

#[derive(Clone, Debug)]
pub(crate) struct DataFile {
    values: HashMap<Box<str>, DataValue>,
}

#[allow(variant_size_differences, reason = "will not be stored in bulk")]
#[derive(Debug, Error)]
pub(crate) enum DznError {
    #[error("failed to read source: {0}")]
    Io(#[from] std::io::Error),

    #[error("input is not valid ascii")]
    EncodingError,

    #[error("malformed dzn input {input}: {description}")]
    ParseError {
        input: Box<str>,
        description: Box<str>,
    },
}

impl<'a> From<nom::Err<nom::error::Error<&'a [u8]>>> for DznError {
    fn from(value: nom::Err<nom::error::Error<&'a [u8]>>) -> Self {
        match value {
            nom::Err::Incomplete(_) => unreachable!("not using any nom::streaming parsers"),
            nom::Err::Error(e) | nom::Err::Failure(e) => {
                let input = match std::str::from_utf8(e.input) {
                    Ok(input) => input.into(),
                    Err(_) => return DznError::EncodingError,
                };

                DznError::ParseError {
                    input,
                    description: e.code.description().into(),
                }
            }
        }
    }
}

impl DataFile {
    pub(crate) fn parse_dzn(source: impl Read) -> Result<DataFile, DznError> {
        let mut reader = BufReader::new(source);
        let mut buffer = Vec::new();

        let mut values = HashMap::default();

        loop {
            buffer.clear();
            let _ = reader.read_until(b';', &mut buffer)?;

            match parse_definition(&buffer)? {
                Some((key, value)) => {
                    if values.insert(key.clone(), value).is_some() {
                        warn!("Duplicate key '{key}' found in dzn.");
                    }
                }
                None => break,
            }
        }

        Ok(DataFile { values })
    }

    pub(crate) fn get_int(&self, key: &str) -> Result<i32, MissingValueError> {
        match self.values.get(key) {
            Some(DataValue::Int(int)) => Ok(*int),
            Some(_) => Err(MissingValueError::TypeMismatch {
                key: key.into(),
                expected: "int".into(),
            }),
            None => Err(MissingValueError::MissingKey(key.into())),
        }
    }

    pub(crate) fn get_1d_int_array(&self, key: &str) -> Result<&[i32], MissingValueError> {
        match self.values.get(key) {
            Some(DataValue::ArrayOfInt(ints)) => Ok(ints),
            Some(_) => Err(MissingValueError::TypeMismatch {
                key: key.into(),
                expected: "int".into(),
            }),
            None => Err(MissingValueError::MissingKey(key.into())),
        }
    }

    pub(crate) fn get_2d_int_array(&self, key: &str) -> Result<&[Box<[i32]>], MissingValueError> {
        match self.values.get(key) {
            Some(DataValue::ArrayOfInt2d(ints)) => Ok(ints),
            Some(_) => Err(MissingValueError::TypeMismatch {
                key: key.into(),
                expected: "int".into(),
            }),
            None => Err(MissingValueError::MissingKey(key.into())),
        }
    }

    pub(crate) fn get_1d_set_of_int_array(
        &self,
        key: &str,
    ) -> Result<&[BTreeSet<i32>], MissingValueError> {
        match self.values.get(key) {
            Some(DataValue::ArrayOfSet(sets)) => Ok(sets),
            Some(_) => Err(MissingValueError::TypeMismatch {
                key: key.into(),
                expected: "int".into(),
            }),
            None => Err(MissingValueError::MissingKey(key.into())),
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub(crate) enum MissingValueError {
    #[error("the key '{0}' does not exist in the data file")]
    MissingKey(Box<str>),

    #[error("expected key '{key}' to be type '{expected}'")]
    TypeMismatch { key: Box<str>, expected: Box<str> },
}

fn parse_definition(buffer: &[u8]) -> Result<Option<(Box<str>, DataValue)>, DznError> {
    // DEFINITION := IDENTIFIER '=' VALUE ';'

    let buffer = whitespace(buffer);

    if buffer.is_empty() {
        return Ok(None);
    }

    let (buffer, identifier) = identifier(buffer)?;
    let buffer = whitespace(buffer);
    let (buffer, _) = byte_tag(b'=')(buffer)?;
    let buffer = whitespace(buffer);
    let (buffer, value) = dzn_value(buffer)?;
    let buffer = whitespace(buffer);
    let (buffer, _) = byte_tag(b';')(buffer)?;

    assert!(buffer.is_empty(), "buffer should be terminated by ';'");

    Ok(Some((identifier, value)))
}

fn whitespace(buffer: &[u8]) -> &[u8] {
    let (remainder, _) =
        take_while::<_, &[u8], nom::error::Error<&[u8]>>(|b: u8| b.is_ascii_whitespace())(buffer)
            .expect("should not fail");

    remainder
}

fn whitespace_parser(input: &[u8]) -> IResult<&[u8], ()> {
    Ok((whitespace(input), ()))
}

fn identifier(input: &[u8]) -> IResult<&[u8], Box<str>> {
    let prefix_parser = take_while1(|byte| matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'_'));
    let suffix_parser =
        take_while(|byte| matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));

    let mut parser = Parser::and(prefix_parser, suffix_parser);
    let (input, (prefix, suffix)) = parser.parse(input)?;

    let mut identifier = String::from_utf8(prefix.to_vec()).expect("valid ascii");
    identifier.push_str(std::str::from_utf8(suffix).expect("valid ascii"));

    Ok((input, identifier.into()))
}

fn dzn_value(input: &[u8]) -> IResult<&[u8], DataValue> {
    alt((
        map(dzn_integer, DataValue::Int),
        map(dzn_array_of_int, DataValue::ArrayOfInt),
        map(dzn_array_of_int_2d, DataValue::ArrayOfInt2d),
        map(dzn_array_of_set, DataValue::ArrayOfSet),
    ))(input)
}

fn dzn_integer(input: &[u8]) -> IResult<&[u8], i32> {
    let chars = (b'0'..=b'9').collect::<Box<_>>();

    let (input, is_negative) = match input[0] {
        b'-' => (&input[1..], true),
        _ => (input, false),
    };

    let (input, number) = map(is_a(chars.as_ref()), |slice| {
        std::str::from_utf8(slice)
            .expect("all chars are ascii")
            .parse::<i32>()
            .expect("should be a number, if it is too big for i32 that is too bad")
    })(input)?;

    if is_negative {
        Ok((input, -number))
    } else {
        Ok((input, number))
    }
}

fn dzn_array_of_int(input: &[u8]) -> IResult<&[u8], Box<[i32]>> {
    delimited(
        whitespace_parser.and(byte_tag(b'[')),
        map(
            separated_list0(
                whitespace_wrapped(b','),
                whitespace_parser.and(dzn_integer).map(|(_, x)| x),
            ),
            |vec| vec.into_boxed_slice(),
        ),
        whitespace_parser.and(byte_tag(b']')),
    )(input)
}

// TODO is there a better way to encapsulate the result type?
#[allow(clippy::type_complexity, reason = "Should be refactored")]
fn dzn_array_of_int_2d(input: &[u8]) -> IResult<&[u8], Box<[Box<[i32]>]>> {
    let nested_array = map(
        separated_list1(
            whitespace_wrapped(b','),
            whitespace_parser.and(dzn_integer).map(|(_, x)| x),
        ),
        |list| list.into_boxed_slice(),
    );

    let delimited_nested_array = map(pair(whitespace_wrapped(b'|'), nested_array), |(_, list)| {
        list
    });

    delimited(
        whitespace_parser.and(byte_tag(b'[')),
        map(many0(delimited_nested_array), |vec| vec.into_boxed_slice()),
        pair(
            whitespace_wrapped(b'|'),
            whitespace_parser.and(byte_tag(b']')),
        ),
    )(input)
}

fn dzn_array_of_set(input: &[u8]) -> IResult<&[u8], Box<[BTreeSet<i32>]>> {
    delimited(
        whitespace_parser.and(byte_tag(b'[')),
        map(
            separated_list0(
                whitespace_wrapped(b','),
                whitespace_parser.and(dzn_set_of_int).map(|(_, x)| x),
            ),
            |vec| vec.into_boxed_slice(),
        ),
        whitespace_parser.and(byte_tag(b']')),
    )(input)
}

fn dzn_set_of_int(input: &[u8]) -> IResult<&[u8], BTreeSet<i32>> {
    delimited(
        whitespace_parser.and(byte_tag(b'{')),
        map(
            separated_list0(
                whitespace_wrapped(b','),
                whitespace_parser.and(dzn_integer).map(|(_, x)| x),
            ),
            |vec| vec.into_iter().collect::<BTreeSet<_>>(),
        ),
        whitespace_parser.and(byte_tag(b'}')),
    )(input)
}

fn whitespace_wrapped(separator: u8) -> impl FnMut(&[u8]) -> IResult<&[u8], ()> {
    move |input: &[u8]| {
        let input = whitespace(input);
        let (input, _) = byte_tag(separator)(input)?;
        let input = whitespace(input);

        Ok((input, ()))
    }
}

fn byte_tag(byte: u8) -> impl Fn(&[u8]) -> IResult<&[u8], u8> {
    move |input: &[u8]| map(tag(&[byte]), |slice: &[u8]| slice[0])(input)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum DataValue {
    Int(i32),
    ArrayOfInt(Box<[i32]>),
    ArrayOfInt2d(Box<[Box<[i32]>]>),
    ArrayOfSet(Box<[BTreeSet<i32>]>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_dzn_file_is_valid() {
        let source = "";

        let _ = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
    }

    #[test]
    fn whitespace_is_ignored() {
        let source = r#"

        "#;

        let _ = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
    }

    #[test]
    fn parse_integer_definition() {
        let source = r#"
            some_value = 1;
            other = -20;
            final = 324;
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");

        assert_eq!(Ok(1), data_file.get_int("some_value"));
        assert_eq!(Ok(-20), data_file.get_int("other"));
        assert_eq!(Ok(324), data_file.get_int("final"));
    }

    #[test]
    fn identifiers_conform_to_flatzinc_spec() {
        let valid_identifiers = r#"
            _some_value = 1;
            _1987 = 1;
        "#;

        let _ = DataFile::parse_dzn(valid_identifiers.as_bytes()).expect("valid dzn");
    }

    #[test]
    fn identifiers_cannot_start_with_digit() {
        let invalid_identifiers = r#"
            1987 = 1;
        "#;

        let _ = DataFile::parse_dzn(invalid_identifiers.as_bytes()).expect_err("invalid dzn");
    }

    #[test]
    fn parse_1d_array_of_int() {
        let source = r#"
            some_value = [1, 2, 3, 4];
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
        let array = data_file.get_1d_int_array("some_value");

        let expected: &[i32] = &[1, 2, 3, 4];
        assert_eq!(Ok(expected), array);
    }

    #[test]
    fn parse_1d_array_of_int_with_whitespace() {
        let source = r#"
            some_value = [ 1, 2, 3, 4 ];
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
        let array = data_file.get_1d_int_array("some_value");

        let expected: &[i32] = &[1, 2, 3, 4];
        assert_eq!(Ok(expected), array);
    }

    #[test]
    fn parse_1d_array_of_int_with_whitespace_empty() {
        let source = r#"
            some_value = [ ];
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
        let array = data_file.get_1d_int_array("some_value");

        let expected: &[i32] = &[];
        assert_eq!(Ok(expected), array);
    }

    #[test]
    fn parse_2d_array_of_int() {
        let source = r#"
            some_value = [| 1, 2 | 3, 4 |];
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
        let array = data_file.get_2d_int_array("some_value");

        let expected: &[Box<[i32]>] = &[[1, 2].into(), [3, 4].into()];
        assert_eq!(Ok(expected), array);
    }

    #[test]
    fn parse_1d_array_of_set_of_int() {
        let source = r#"
            some_value = [{1}, {1, 2}, {3, 5}, {}];
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
        let array = data_file.get_1d_set_of_int_array("some_value");

        let expected: &[BTreeSet<i32>] = &[
            BTreeSet::from([1]),
            BTreeSet::from([1, 2]),
            BTreeSet::from([3, 5]),
            BTreeSet::new(),
        ];
        assert_eq!(Ok(expected), array);
    }

    #[test]
    fn parse_1d_array_of_set_of_int_whitespace_empty() {
        let source = r#"
            some_value = [{1}, {1, 2}, {3, 5}, { }];
        "#;

        let data_file = DataFile::parse_dzn(source.as_bytes()).expect("valid dzn");
        let array = data_file.get_1d_set_of_int_array("some_value");

        let expected: &[BTreeSet<i32>] = &[
            BTreeSet::from([1]),
            BTreeSet::from([1, 2]),
            BTreeSet::from([3, 5]),
            BTreeSet::new(),
        ];
        assert_eq!(Ok(expected), array);
    }
}
