//! The FlatZinc instances are handled as text, one statement per line, which is also how the solver
//! reads them. That keeps every example a valid instance that the solver compiles itself.

/// One line of a FlatZinc instance.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Statement<'a> {
    /// A parameter, variable or array declaration, with the name it declares.
    Declaration { name: &'a str },
    /// A constraint, with the name of the constraint.
    Constraint { name: &'a str },
    /// A solve item, a predicate declaration, a comment or an empty line.
    Other,
}

pub(crate) fn classify(line: &str) -> Statement<'_> {
    let line = line.trim();

    if let Some(rest) = line.strip_prefix("constraint") {
        return match identifiers(rest).next() {
            Some((start, end)) => Statement::Constraint {
                name: &rest[start..end],
            },
            None => Statement::Other,
        };
    }

    if line.is_empty()
        || line.starts_with('%')
        || line.starts_with("solve")
        || line.starts_with("predicate")
    {
        return Statement::Other;
    }

    match declared_name_range(line) {
        Some((start, end)) => Statement::Declaration {
            name: &line[start..end],
        },
        None => Statement::Other,
    }
}

/// The range of the declared name in a declaration: the identifier after the colon that ends the
/// type, which is the first colon that is not part of `::`.
pub(crate) fn declared_name_range(line: &str) -> Option<(usize, usize)> {
    let bytes = line.as_bytes();
    let colon = (0..bytes.len()).find(|&index| {
        bytes[index] == b':'
            && bytes.get(index + 1) != Some(&b':')
            && (index == 0 || bytes[index - 1] != b':')
    })?;

    identifiers(&line[colon + 1..])
        .next()
        .map(|(start, end)| (colon + 1 + start, colon + 1 + end))
}

/// The byte ranges of the identifiers in `text`, in order. Numbers are skipped.
pub(crate) fn identifiers(text: &str) -> impl Iterator<Item = (usize, usize)> + '_ {
    let bytes = text.as_bytes();
    let mut index = 0;

    std::iter::from_fn(move || {
        while index < bytes.len() {
            let byte = bytes[index];
            if byte.is_ascii_alphabetic() || byte == b'_' {
                let start = index;
                while index < bytes.len()
                    && (bytes[index].is_ascii_alphanumeric() || bytes[index] == b'_')
                {
                    index += 1;
                }
                return Some((start, index));
            } else if byte.is_ascii_digit() {
                while index < bytes.len()
                    && (bytes[index].is_ascii_alphanumeric() || bytes[index] == b'_')
                {
                    index += 1;
                }
            } else {
                index += 1;
            }
        }
        None
    })
}

/// The byte ranges of the integer literals in `text`, with their values. A minus sign directly in
/// front of a number is part of it, and the bounds of a range such as `1..5` are two literals.
pub(crate) fn integer_literals(text: &str) -> Vec<(usize, usize, i64)> {
    let bytes = text.as_bytes();
    let mut literals = vec![];
    let mut index = 0;

    while index < bytes.len() {
        let byte = bytes[index];
        if byte.is_ascii_alphabetic() || byte == b'_' {
            while index < bytes.len()
                && (bytes[index].is_ascii_alphanumeric() || bytes[index] == b'_')
            {
                index += 1;
            }
        } else if byte.is_ascii_digit() {
            let start = if index > 0 && bytes[index - 1] == b'-' {
                index - 1
            } else {
                index
            };
            while index < bytes.len() && bytes[index].is_ascii_digit() {
                index += 1;
            }
            if let Ok(value) = text[start..index].parse() {
                literals.push((start, index, value));
            }
        } else {
            index += 1;
        }
    }

    literals
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declarations_and_constraints_are_recognised() {
        assert_eq!(
            classify("var -3..3: a :: output_var;"),
            Statement::Declaration { name: "a" }
        );
        assert_eq!(
            classify("array [1..2] of var 1..5: xs :: output_array([1..2]) = [x1,x2];"),
            Statement::Declaration { name: "xs" }
        );
        assert_eq!(
            classify("array [1..3] of int: X_INTRODUCED_0_ = [-1,2,3];"),
            Statement::Declaration {
                name: "X_INTRODUCED_0_"
            }
        );
        assert_eq!(
            classify("constraint int_lin_le([1, -1], [x1, x2], 0);"),
            Statement::Constraint { name: "int_lin_le" }
        );
        assert_eq!(classify("solve satisfy;"), Statement::Other);
    }

    #[test]
    fn integer_literals_include_their_sign() {
        let values = integer_literals("int_lin_le([1, -1], [x1, x2], 0)")
            .into_iter()
            .map(|(_, _, value)| value)
            .collect::<Vec<_>>();
        assert_eq!(values, vec![1, -1, 0]);

        let values = integer_literals("var -3..10: a")
            .into_iter()
            .map(|(_, _, value)| value)
            .collect::<Vec<_>>();
        assert_eq!(values, vec![-3, 10]);
    }
}
