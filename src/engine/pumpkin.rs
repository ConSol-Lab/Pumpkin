use log::debug;
use std::{
    fs,
    time::{Duration, Instant},
};

use crate::{
    basic_types::{
        CSPSolverExecutionFlag, FileFormat, Function, IntegerVariable, Literal,
        PumpkinExecutionFlag, Stopwatch,
    },
    engine::ConstraintSatisfactionSolver,
    parsers::dimacs::{parse_cnf, parse_wcnf},
    propagators::SimpleLinearInequalityPropagator,
    pumpkin_asserts::pumpkin_assert_simple,
};

use super::{
    linear_search::UpperBoundEncoding, LinearSearch, SATDataStructuresInternalParameters,
    SatisfactionSolverOptions,
};

pub struct Pumpkin {
    csp_solver: ConstraintSatisfactionSolver,
    linear_search: LinearSearch,
    objective_function: Function,
    stopwatch: Stopwatch,
}

impl Pumpkin {
    pub fn new(
        sat_options: SATDataStructuresInternalParameters,
        solver_options: SatisfactionSolverOptions,
        upper_bound_encoding: UpperBoundEncoding,
        time_limit: Option<Duration>,
    ) -> Pumpkin {
        Pumpkin {
            csp_solver: ConstraintSatisfactionSolver::new(sat_options, solver_options),
            objective_function: Function::new(),
            stopwatch: Stopwatch::new(
                time_limit
                    .map(|duration| duration.as_secs() as i64)
                    .unwrap_or(i64::MAX),
            ),
            linear_search: LinearSearch::new(upper_bound_encoding),
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

        let output = self.linear_search.solve(
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
            FileFormat::CnfDimacsPLine => parse_cnf(file_location, &mut self.csp_solver)?,
            FileFormat::WcnfDimacsPLine => {
                let objective_function = parse_wcnf(file_location, &mut self.csp_solver)?;
                self.objective_function = objective_function;
            }
            FileFormat::MaxSAT2022 => todo!(),
        };

        debug!(
            "Reading file took {} seconds.",
            time_start.elapsed().as_secs()
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
        let mut header = lines.next().unwrap().split_whitespace();
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
