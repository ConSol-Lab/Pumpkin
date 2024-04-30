use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[allow(variant_size_differences)]
enum Value {
    Int(i32),
    Bool(bool),
}

impl FromStr for Value {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<i32>()
            .map(Value::Int)
            .or_else(|_| s.parse::<bool>().map(Value::Bool))
            .map_err(|e| e.to_string())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Solutions {
    assignments: BTreeSet<BTreeMap<String, Value>>,
}

impl FromStr for Solutions {
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
        .filter(|line| !line.is_empty())
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
