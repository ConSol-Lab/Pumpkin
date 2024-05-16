use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Display;
use std::str::FromStr;

use regex::Regex;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[allow(variant_size_differences)]
enum Value {
    Int(i32),
    Bool(bool),
    IntArray(Vec<i32>),
}

impl FromStr for Value {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<i32>()
            .map(Value::Int)
            .or_else(|_| {
                s.parse::<bool>()
                    .map(Value::Bool)
                    .or_else(|_| create_array_from_string(s))
            })
            .map_err(|e| e.to_string())
    }
}

struct IntArrayError;
impl Display for IntArrayError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Could not parse int array")
    }
}

fn create_array_from_string(s: &str) -> Result<Value, IntArrayError> {
    let captures = Regex::new(r"array1d\([0-9]+\.\.[0-9]+,\s*\[(\d+(?:,\s\d+)*\d*)\]\)")
        .unwrap()
        .captures_iter(s)
        .next();
    if let Some(captures) = captures {
        Ok(Value::IntArray(
            captures
                .get(1)
                .unwrap()
                .as_str()
                .split(", ")
                .map(|integer| integer.parse::<i32>().unwrap())
                .collect::<Vec<_>>(),
        ))
    } else {
        Err(IntArrayError)
    }
}

#[derive(Debug)]
pub struct Solutions<const ORDERED: bool> {
    assignments: Vec<BTreeMap<String, Value>>,
}

impl<const ORDERED: bool> PartialEq for Solutions<ORDERED> {
    fn eq(&self, other: &Self) -> bool {
        if ORDERED {
            // If the solutions are ordered then we do a comparison which also checks the order (by
            // simply comparing the vecs)
            self.assignments == other.assignments
        } else {
            // If the solutions are unordered then we go a comparison on an ordered BTreeSet which
            // means that we disregard the order in which the solver(s) found the solutions
            self.assignments.iter().collect::<BTreeSet<_>>()
                == other.assignments.iter().collect::<BTreeSet<_>>()
        }
    }
}

impl<const ORDERED: bool> FromStr for Solutions<ORDERED> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();

        if !s.ends_with("==========") {
            return Err("solutions should end with '=========='".into());
        }

        let assignments = s
            .split("----------")
            .map(parse_solution)
            .collect::<Result<_, _>>()?;
        Ok(Solutions { assignments })
    }
}

fn parse_solution(solution: &str) -> Result<BTreeMap<String, Value>, String> {
    let solution = solution.trim().trim_end_matches("==========");

    solution
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty() && !line.starts_with('%'))
        .map(parse_solution_line)
        .collect()
}

fn parse_solution_line(line: &str) -> Result<(String, Value), String> {
    let mut components = line.split(" = ");

    let variable = components
        .next()
        .ok_or_else(|| format!("Invalid solution line '{line}'"))?
        .to_owned();

    let value = components
        .next()
        .ok_or_else(|| format!("Invalid solution line '{line}'"))?
        .trim_end_matches(';')
        .parse::<Value>()
        .map_err(|_| format!("Failed to parse value from '{line}'"))?;

    Ok((variable, value))
}
