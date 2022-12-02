use std::fs;

use crate::basic_types::PropositionalVariable;

use super::{FileFormat, Literal, Solution};

pub struct Instance {
    pub hard_clauses: Vec<Vec<Literal>>,
    pub soft_clauses: Vec<SoftClause>,
}

impl Instance {
    pub fn new() -> Instance {
        Instance {
            hard_clauses: vec![],
            soft_clauses: vec![],
        }
    }

    pub fn are_hard_clauses_violated(&self, solution: &Solution) -> bool {
        self.hard_clauses
            .iter()
            .any(|clause| Instance::is_clause_violated(clause, solution))
    }

    pub fn compute_soft_clause_violation(&self, solution: &Solution) -> u64 {
        self.soft_clauses
            .iter()
            .filter_map(|soft_clause| {
                if Instance::is_clause_violated(&soft_clause.literals, solution) {
                    Some(soft_clause.weight)
                } else {
                    None
                }
            })
            .sum()
    }

    pub fn is_empty(&self) -> bool {
        self.hard_clauses.is_empty() && self.soft_clauses.is_empty()
    }

    fn is_clause_violated(literals: &[Literal], solution: &Solution) -> bool {
        literals
            .iter()
            .all(|literal| !solution.get_literal_value(*literal))
    }
}

impl Instance {
    pub fn read_file(&mut self, file_location: &str, file_format: FileFormat) {
        assert!(self.is_empty());
        match file_format {
            FileFormat::CnfDimacsPLine => self.read_cnf_p_line(file_location),
            FileFormat::WcnfDimacsPLine => self.read_wcnf_p_line(file_location),
            FileFormat::MaxSAT2022 => todo!(),
        }
    }

    fn read_cnf_p_line(&mut self, file_location: &str) {
        //this is a slow method of reading, especially for large files (GBs) from the MaxSAT competition
        //  but for now it will do

        let file_contents = fs::read_to_string(file_location).unwrap_or_else(|_| {
            panic!("Unable to read file with path: {}", file_location);
        });

        //skip comments
        //  comments are lines that star with 'c'
        let mut lines = file_contents.lines().filter(|line| !line.starts_with('c'));

        //read the header line
        //  the format is 'p cnf [num variables] [num clauses]
        let mut header = lines.next().unwrap().split(' ');
        let mut temp = header.next();
        assert!(temp == Some("p"));
        temp = header.next();
        assert!(temp == Some("cnf"));
        let _num_variables = header.next().unwrap().parse::<u64>().unwrap();
        let num_clauses = header.next().unwrap().parse::<u64>().unwrap();

        let mut num_clauses_read = 0;
        //read clauses one by one
        for line in lines {
            let literals: Vec<Literal> = line
                .split(' ')
                .filter_map(|s| {
                    let variable_index = s.parse::<i64>().unwrap();
                    match variable_index.cmp(&0) {
                        std::cmp::Ordering::Equal => None,
                        std::cmp::Ordering::Greater => Some(Literal::new(
                            PropositionalVariable::new(variable_index.unsigned_abs() as u32),
                            true,
                        )),
                        std::cmp::Ordering::Less => Some(Literal::new(
                            PropositionalVariable::new(variable_index.unsigned_abs() as u32),
                            false,
                        )),
                    }
                })
                .collect();

            self.hard_clauses.push(literals);
            num_clauses_read += 1;
        }
        assert!(
            num_clauses == num_clauses_read,
            "Num of clauses in the file does not match the header."
        );
    }

    fn read_wcnf_p_line(&mut self, file_location: &str) {
        //this is a slow method of reading, especially for large files (GBs) from the MaxSAT competition
        //  but for now it will do

        let file_contents = fs::read_to_string(file_location).unwrap_or_else(|_| {
            panic!("Unable to read file with path: {}", file_location);
        });

        //skip comments
        //  comments are lines that star with 'c'
        let mut lines = file_contents.lines().filter(|line| !line.starts_with('c'));

        //read the header line
        //  the format is 'p wcnf [num variables] [num clauses] [top weight]
        let mut header = lines.next().unwrap().split(' ');
        let mut temp = header.next();
        assert!(temp == Some("p"));
        temp = header.next();
        assert!(temp == Some("wcnf"));
        let _num_variables = header.next().unwrap().parse::<u64>().unwrap();
        let num_clauses = header.next().unwrap().parse::<u64>().unwrap();
        let top_weight = header.next().unwrap().parse::<u64>().unwrap();

        let mut num_clauses_read = 0;
        //read clauses one by one
        for line in lines {
            let mut raw_integers = line
                .split(' ')
                .filter_map(|s| {
                    if s.is_empty() {
                        return None;
                    }

                    let integer = s.parse::<i64>().unwrap();
                    if integer == 0 {
                        None
                    } else {
                        Some(integer)
                    }
                })
                .peekable();

            if raw_integers.peek() == Some(&(top_weight as i64)) {
                //hard clauses start with the top weight value
                raw_integers.next(); //remove the first value from consideration
                let literals: Vec<Literal> = raw_integers
                    .filter_map(|v| match v.cmp(&0) {
                        std::cmp::Ordering::Greater => Some(Literal::new(
                            PropositionalVariable::new(v.unsigned_abs() as u32),
                            true,
                        )),
                        std::cmp::Ordering::Less => Some(Literal::new(
                            PropositionalVariable::new(v.unsigned_abs() as u32),
                            false,
                        )),
                        std::cmp::Ordering::Equal => {
                            panic!("Zero values are unexpected at this point")
                        }
                    })
                    .collect();

                self.hard_clauses.push(literals);
            } else {
                //soft clause

                let weight = raw_integers.next().unwrap() as u64;

                let literals: Vec<Literal> = raw_integers
                    .filter_map(|v| match v.cmp(&0) {
                        std::cmp::Ordering::Greater => Some(Literal::new(
                            PropositionalVariable::new(v.unsigned_abs() as u32),
                            true,
                        )),
                        std::cmp::Ordering::Less => Some(Literal::new(
                            PropositionalVariable::new(v.unsigned_abs() as u32),
                            false,
                        )),
                        std::cmp::Ordering::Equal => {
                            panic!("Zero values are unexpected at this point")
                        }
                    })
                    .collect();

                self.soft_clauses.push(SoftClause { literals, weight });
            }

            num_clauses_read += 1;
        }
        assert!(
            num_clauses == num_clauses_read,
            "Num of clauses in the file does not match the header."
        );
    }
}

pub struct SoftClause {
    pub literals: Vec<Literal>,
    pub weight: u64,
}
