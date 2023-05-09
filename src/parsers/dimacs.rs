use std::fs;

use log::debug;

use crate::{
    basic_types::{Function, Literal, PropositionalVariable},
    engine::{ConstraintSatisfactionSolver, SATEngineDataStructures},
};

pub fn parse_cnf(
    file_location: &str,
    csp_solver: &mut ConstraintSatisfactionSolver,
) -> std::io::Result<()> {
    //this is a slow method of reading, especially for large files (GBs) from the MaxSAT competition
    //  but for now it will do

    let file_contents = fs::read_to_string(file_location)?;

    //skip comments
    //  comments are lines that star with 'c'
    let mut lines = file_contents.lines().filter(|line| !line.starts_with('c'));

    //read the header line
    //  the format is 'p cnf [num variables] [num clauses]
    let mut header = lines.next().unwrap().split_whitespace();
    let mut temp = header.next();
    assert!(temp == Some("p"));
    temp = header.next();
    assert!(temp == Some("cnf"));
    let num_variables = header.next().unwrap().parse::<u64>().unwrap();
    let num_clauses = header.next().unwrap().parse::<u64>().unwrap();

    let variables: Vec<PropositionalVariable> = (0..num_variables)
        .map(|_i| csp_solver.create_new_propositional_variable())
        .collect();

    debug!("Reading file: {}", file_location);
    debug!("Number of variables: {}", num_variables);
    debug!("Number of clauses: {}", num_clauses);

    let mut num_clauses_read = 0;
    //read clauses one by one
    for line in lines {
        let literals: Vec<Literal> = line
            .split_whitespace()
            .filter_map(|s| {
                let variable_index = s.parse::<i64>().unwrap();
                match variable_index.cmp(&0) {
                    std::cmp::Ordering::Equal => None,
                    std::cmp::Ordering::Greater => Some(Literal::new(
                        variables[variable_index.unsigned_abs() as usize - 1], //minus one is important since in the vector the indicies are from zero, whereas the indexing is from 1 in the file
                        true,
                    )),
                    std::cmp::Ordering::Less => Some(Literal::new(
                        variables[variable_index.unsigned_abs() as usize - 1], //minus one is important since in the vector the indicies are from zero, whereas the indexing is from 1 in the file
                        false,
                    )),
                }
            })
            .collect();

        csp_solver.add_permanent_clause(literals);

        num_clauses_read += 1;
    }
    assert!(
        num_clauses == num_clauses_read,
        "Num of clauses in the file does not match the header."
    );
    Ok(())
}

pub fn parse_wcnf(
    file_location: &str,
    csp_solver: &mut ConstraintSatisfactionSolver,
) -> std::io::Result<Function> {
    let mut objective_function = Function::new();

    //this is a slow method of reading, especially for large files (GBs) from the MaxSAT competition
    //  but for now it will do

    let file_contents = fs::read_to_string(file_location)?;

    //skip comments
    //  comments are lines that star with 'c'
    let mut lines = file_contents.lines().filter(|line| !line.starts_with('c'));

    //read the header line
    //  the format is 'p wcnf [num variables] [num clauses] [top weight]
    let mut header = lines.next().unwrap().split_whitespace();
    let mut temp = header.next();
    assert!(temp == Some("p"));
    temp = header.next();
    assert!(temp == Some("wcnf"));
    let num_variables = header.next().unwrap().parse::<u64>().unwrap();
    let num_clauses = header.next().unwrap().parse::<u64>().unwrap();
    let top_weight = header.next().unwrap().parse::<u64>().unwrap();

    let variables: Vec<PropositionalVariable> = (0..num_variables)
        .map(|_i| csp_solver.create_new_propositional_variable())
        .collect();

    debug!("Reading file: {}", file_location);
    debug!("Number of variables: {}", num_variables);
    debug!("Number of clauses: {}", num_clauses);
    debug!("Top weight: {}", top_weight);

    let mut num_clauses_read = 0;
    //read clauses one by one
    for line in lines {
        let mut raw_integers = line
            .split_whitespace()
            .filter_map(|s| {
                //in case there are double spaces "  ", the split will return an empty string
                //  should do a more robust file reading algorithm but for now this will do
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
                        variables[v.unsigned_abs() as usize - 1], //minus one is important since in the vector the indicies are from zero, whereas the indexing is from 1 in the file
                        true,
                    )),
                    std::cmp::Ordering::Less => Some(Literal::new(
                        variables[v.unsigned_abs() as usize - 1], //minus one is important since in the vector the indicies are from zero, whereas the indexing is from 1 in the file
                        false,
                    )),
                    std::cmp::Ordering::Equal => {
                        panic!("Zero values are unexpected at this point")
                    }
                })
                .collect();

            csp_solver.add_permanent_clause(literals);
        } else {
            //soft clause

            let weight = raw_integers.next().unwrap() as u64;

            let mut literals: Vec<Literal> = raw_integers
                .filter_map(|v| match v.cmp(&0) {
                    std::cmp::Ordering::Greater => Some(Literal::new(
                        variables[v.unsigned_abs() as usize - 1], //minus one is important since in the vector the indicies are from zero, whereas the indexing is from 1 in the file
                        true,
                    )),
                    std::cmp::Ordering::Less => Some(Literal::new(
                        variables[v.unsigned_abs() as usize - 1], //minus one is important since in the vector the indicies are from zero, whereas the indexing is from 1 in the file
                        false,
                    )),
                    std::cmp::Ordering::Equal => {
                        panic!("Zero values are unexpected at this point")
                    }
                })
                .collect();

            literals = SATEngineDataStructures::preprocess_clause(
                literals,
                csp_solver.get_propositional_assignments(),
            );

            //the soft clause is violated at the root level
            if literals.is_empty() {
                objective_function.add_constant_term(weight);
            }
            //the soft clause is satisfied at the root level
            //  the clause may be ignored, do nothing
            else if csp_solver
                .get_propositional_assignments()
                .is_literal_assigned_true(literals[0])
            {
            }
            //the soft clause is a unit clause, we can use the literal in the objective directly without needing an additional selector variable
            else if literals.len() == 1 {
                objective_function.add_weighted_literal(!literals[0], weight);
            }
            //general case, a soft clause with more than one literals
            else {
                let soft_literal =
                    Literal::new(csp_solver.create_new_propositional_variable(), true);

                literals.push(soft_literal);
                objective_function.add_weighted_literal(soft_literal, weight);
                csp_solver.add_permanent_clause(literals);
            }
        }

        num_clauses_read += 1;
    }
    assert!(
        num_clauses == num_clauses_read,
        "Num of clauses in the file does not match the header."
    );
    Ok(objective_function)
}
