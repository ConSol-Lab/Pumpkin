use log::debug;
use std::{
    fs::{self},
    time::Instant,
};

use crate::{
    arguments::ArgumentHandler,
    basic_types::{
        CSPSolverExecutionFlag, FileFormat, Function, IntegerVariable, Literal,
        PropositionalVariable, PumpkinExecutionFlag, Stopwatch,
    },
    engine::{ConstraintSatisfactionSolver, SATEngineDataStructures},
    propagators::SimpleLinearInequalityPropagator,
    pumpkin_asserts::pumpkin_assert_simple,
};

use super::LinearSearch;

pub struct Pumpkin {
    csp_solver: ConstraintSatisfactionSolver,
    linear_search: LinearSearch,
    objective_function: Function,
    stopwatch: Stopwatch,
}

impl Pumpkin {
    pub fn new(argument_handler: &ArgumentHandler) -> Pumpkin {
        Pumpkin {
            csp_solver: ConstraintSatisfactionSolver::new(argument_handler),
            linear_search: LinearSearch::new(),
            objective_function: Function::new(),
            stopwatch: Stopwatch::new(argument_handler.get_integer_argument("time-limit")),
        }
    }

    pub fn solve(&mut self) -> PumpkinExecutionFlag {
        pumpkin_assert_simple!(self.csp_solver.get_state().is_ready());

        debug!(
            "Basic initialisation took {} seconds.",
            self.stopwatch.get_elapsed_time()
        );

        //set phasing saving to an optimistic version, where objective literals are being set to zero
        let optimistic_phases: Vec<Literal> = self
            .objective_function
            .get_function_as_weighted_literals_vector(&self.csp_solver)
            .iter()
            .map(|wl| !wl.literal)
            .collect();
        self.csp_solver
            .set_fixed_phases_for_variables(&optimistic_phases);

        //compute initial solution
        let csp_execution_flag = self
            .csp_solver
            .solve(self.stopwatch.get_remaining_time_budget());

        if let CSPSolverExecutionFlag::Infeasible = csp_execution_flag {
            return PumpkinExecutionFlag::Infeasible;
        }

        if let CSPSolverExecutionFlag::Timeout = csp_execution_flag {
            return PumpkinExecutionFlag::Timeout;
        }

        debug!(
            "Initial solution took {} seconds",
            self.stopwatch.get_elapsed_time()
        );

        //simple preprocessing on the objective function

        let output = LinearSearch::solve(
            &mut self.csp_solver,
            &self.objective_function,
            &self.stopwatch,
        );

        if self.csp_solver.get_state().timeout() {
            PumpkinExecutionFlag::Feasible {
                feasible_solution: output.solution,
                objective_value: output.objective_value,
            }
        } else {
            PumpkinExecutionFlag::Optimal {
                optimal_solution: output.solution,
                objective_value: output.objective_value,
            }
        }
    }

    pub fn reset_variable_selection(&mut self, random_seed: i64) {
        self.csp_solver.reset_variable_selection(random_seed);
    }
}

//methods for reading files
//  perhaps in the future these should be moved outside the solver?
impl Pumpkin {
    pub fn read_file(
        &mut self,
        file_location: &str,
        file_format: FileFormat,
    ) -> std::io::Result<()> {
        let time_start = Instant::now();

        match file_format {
            FileFormat::CnfDimacsPLine => self.read_cnf_p_line(file_location),
            FileFormat::WcnfDimacsPLine => self.read_wcnf_p_line(file_location),
            FileFormat::MaxSAT2022 => todo!(),
        }?;

        debug!(
            "Reading file took {} seconds.",
            time_start.elapsed().as_secs()
        );

        Ok(())
    }

    fn read_wcnf_p_line(&mut self, file_location: &str) -> std::io::Result<()> {
        pumpkin_assert_simple!(
            self.objective_function.is_empty(),
            "Expected an empty objective function."
        );

        //this is a slow method of reading, especially for large files (GBs) from the MaxSAT competition
        //  but for now it will do

        let file_contents = fs::read_to_string(file_location)?;

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
        let num_variables = header.next().unwrap().parse::<u64>().unwrap();
        let num_clauses = header.next().unwrap().parse::<u64>().unwrap();
        let top_weight = header.next().unwrap().parse::<u64>().unwrap();

        let variables: Vec<PropositionalVariable> = (0..num_variables)
            .map(|_i| self.csp_solver.create_new_propositional_variable())
            .collect();

        debug!("Reading file: {}", file_location);
        debug!("Number of variables: {}", num_variables);
        debug!("Number of clauses: {}", num_clauses);
        debug!("Top weight: {}", top_weight);

        let mut num_clauses_read = 0;
        //read clauses one by one
        for line in lines {
            let mut raw_integers = line
                .split(' ')
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

                self.csp_solver.add_permanent_clause(literals);
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
                    self.csp_solver.get_propositional_assignments(),
                );

                //the soft clause is violated at the root level
                if literals.is_empty() {
                    self.objective_function.add_constant_term(weight);
                }
                //the soft clause is satisfied at the root level
                //  the clause may be ignored, do nothing
                else if self
                    .csp_solver
                    .get_propositional_assignments()
                    .is_literal_assigned_true(literals[0])
                {
                }
                //the soft clause is a unit clause, we can use the literal in the objective directly without needing an additional selector variable
                else if literals.len() == 1 {
                    self.objective_function
                        .add_weighted_literal(!literals[0], weight);
                }
                //general case, a soft clause with more than one literals
                else {
                    let soft_literal =
                        Literal::new(self.csp_solver.create_new_propositional_variable(), true);

                    literals.push(soft_literal);
                    self.objective_function
                        .add_weighted_literal(soft_literal, weight);
                    self.csp_solver.add_permanent_clause(literals);
                }
            }

            num_clauses_read += 1;
        }
        assert!(
            num_clauses == num_clauses_read,
            "Num of clauses in the file does not match the header."
        );
        Ok(())
    }

    fn read_cnf_p_line(&mut self, file_location: &str) -> std::io::Result<()> {
        //this is a slow method of reading, especially for large files (GBs) from the MaxSAT competition
        //  but for now it will do

        let file_contents = fs::read_to_string(file_location)?;

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
        let num_variables = header.next().unwrap().parse::<u64>().unwrap();
        let num_clauses = header.next().unwrap().parse::<u64>().unwrap();

        let variables: Vec<PropositionalVariable> = (0..num_variables)
            .map(|_i| self.csp_solver.create_new_propositional_variable())
            .collect();

        debug!("Reading file: {}", file_location);
        debug!("Number of variables: {}", num_variables);
        debug!("Number of clauses: {}", num_clauses);

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

            self.csp_solver.add_permanent_clause(literals);

            num_clauses_read += 1;
        }
        assert!(
            num_clauses == num_clauses_read,
            "Num of clauses in the file does not match the header."
        );
        Ok(())
    }

    //this is purely for testing purposes
    fn read_cnf_p_line_into_simple_linear_inequality_propagator(
        &mut self,
        file_location: &str,
    ) -> std::io::Result<()> {
        let file_contents = fs::read_to_string(file_location)?;

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
        let num_variables = header.next().unwrap().parse::<u64>().unwrap();
        let num_clauses = header.next().unwrap().parse::<u64>().unwrap();

        let variables: Vec<IntegerVariable> = (0..num_variables)
            .map(|_i| self.csp_solver.create_new_integer_variable(0, 1))
            .collect();

        debug!("Reading file: {}", file_location);
        debug!("Number of variables: {}", num_variables);
        debug!("Number of clauses: {}", num_clauses);

        let mut num_clauses_read = 0;
        //read clauses one by one
        for line in lines {
            let raw_integers: Vec<i64> = line
                .split(' ')
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
                .collect();

            let integer_variables: Vec<IntegerVariable> = raw_integers
                .iter()
                .map(|raw_int| variables[raw_int.unsigned_abs() as usize - 1])
                .collect();

            let weights: Vec<i64> = raw_integers
                .iter()
                .map(|raw_int| if *raw_int > 0 { 1 } else { -1 })
                .collect();

            let right_hand_side: i64 = 1 - weights
                .iter()
                .filter_map(|w| if *w > 0 { None } else { Some(1) })
                .sum::<i64>();

            let propagator = SimpleLinearInequalityPropagator::new(
                &integer_variables,
                &weights,
                right_hand_side,
            );

            self.csp_solver.add_propagator(Box::new(propagator));

            num_clauses_read += 1;
        }
        assert!(
            num_clauses == num_clauses_read,
            "Num of clauses in the file does not match the header."
        );
        Ok(())
    }
}

impl Pumpkin {
    pub fn create_argument_handler() -> ArgumentHandler {
        let mut argument_handler = ArgumentHandler::new();

        argument_handler.define_new_category("General", "todo");

        argument_handler.define_string_argument(
            "file-location",
            "General",
            "If non-empty, reads the instance given in the file into the solver.",
            "",
            &[],
        );

        argument_handler.define_integer_argument(
            "time-limit",
            "General",
            "Maximum runtime in seconds. In the current implementation should be used for indicative purposes only, todo.",
            i64::MAX,
            0,
            i64::MAX
        );

        argument_handler.define_integer_argument(
            "num-conflicts-per-restart",
            "General",
            "Number of conflicts before each restart. This is a fixed-length restart strategy.",
            4000,
            0,
            1 << 60,
        );

        argument_handler.define_integer_argument
        ("threshold-learned-clauses",
        "General",
        "Threshold indicating the target number of learned clauses to be kept in the solver. This number could be exceeded temporarily but occassionally the solver will delete learned clauses.", 
        4000,
        0,
        1 << 60);

        argument_handler.define_string_argument(
            "learned-clause-sorting-strategy",
            "General",
            "Decides which clauses will be removed when cleaning up learned clauses todo.",
            "lbd",
            &["lbd", "activity"],
        );

        argument_handler.define_integer_argument(
            "random-seed",
            "General",
            "Influences initial order of variables. todo example.",
            -2,
            -2,
            i64::MAX,
        );

        argument_handler.define_bool_argument(
            "verbose",
            "General",
            "Enables complete logging output",
            false,
        );
        argument_handler.define_bool_argument(
            "omit-timestamp",
            "General",
            "Removes the timestamps from the logging lines",
            false,
        );
        argument_handler.define_bool_argument(
            "omit-call-site",
            "General",
            "Removes the call site from the logging lines",
            false,
        );

        argument_handler
    }
}
