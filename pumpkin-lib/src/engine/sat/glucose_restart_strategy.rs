use std::fmt::Debug;
use std::fmt::Formatter;

use crate::basic_types::moving_average::CumulativeMovingAverage;
use crate::basic_types::moving_average::MovingAverageInterface;
use crate::basic_types::moving_average::WindowedMovingAverage;
use crate::basic_types::sequence_generators::ConstantSequence;
use crate::basic_types::sequence_generators::GeometricSequence;
use crate::basic_types::sequence_generators::LubySequence;
use crate::basic_types::sequence_generators::SequenceGeneratorInterface;
use crate::basic_types::sequence_generators::SequenceGeneratorType;
use crate::engine::debug_helper::DebugDyn;
use crate::engine::SatisfactionSolverOptions;

pub struct GlucoseRestartStrategy {
    // considering making the fields generic?
    restart_sequence_generator: Box<dyn SequenceGeneratorInterface>,
    num_conflicts_until_restart: i64,
    min_num_conflicts_before_first_restart: i64,
    lbd_short_term_moving_average: Box<dyn MovingAverageInterface>,
    lbd_coef: f64,
    lbd_long_term_moving_average: Box<dyn MovingAverageInterface>,
    num_variables_coef: f64,
    num_assigned_variables_moving_average: Box<dyn MovingAverageInterface>,
    num_restarts: i64,
    num_blocked_restarts: i64,
}

impl Debug for GlucoseRestartStrategy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlucoseRestartStrategy")
            .field(
                "restart_sequence_generator",
                &DebugDyn::from("SequenceGeneratorInterface"),
            )
            .field(
                "num_conflicts_until_restart",
                &self.num_conflicts_until_restart,
            )
            .field(
                "min_num_conflicts_before_first_restart",
                &self.min_num_conflicts_before_first_restart,
            )
            .field(
                "lbd_short_term_moving_average",
                &DebugDyn::from("MovingAverageInterface"),
            )
            .field("lbd_coef", &self.lbd_coef)
            .field(
                "lbd_long_term_moving_average",
                &DebugDyn::from("MovingAverageInterface"),
            )
            .field("num_variables_coef", &self.num_variables_coef)
            .field(
                "num_assigned_variables_moving_average",
                &DebugDyn::from("MovingAverageInterface"),
            )
            .field("num_restarts", &self.num_restarts)
            .field("num_blocked_restarts", &self.num_blocked_restarts)
            .finish()
    }
}

// impl Default for GlucoseRestartStrategy {
// default values based on the SAT solver Glucose
// fn default() -> Self {
// Self {
// restart_sequence_generator: Box::new(ConstantSequence::new(50)),
// num_conflicts_until_restart: 50,
// min_num_conflicts_before_first_restart: 0,
// lbd_short_term_moving_average: Box::new(WindowedMovingAverage::new(50)),
// lbd_coef: 1.25,
// lbd_long_term_moving_average: Box::<CumulativeMovingAverage>::default(),
// num_variables_coef: 1.4,
// num_assigned_variables_moving_average: Box::new(WindowedMovingAverage::new(5000)),
// num_restarts: 0,
// num_blocked_restarts: 0,
// }
// }
// }

impl GlucoseRestartStrategy {
    pub fn new(solver_options: &SatisfactionSolverOptions) -> Self {
        let mut restart_sequence_generator: Box<dyn SequenceGeneratorInterface> =
            match solver_options.restart_sequence_generator_type {
                SequenceGeneratorType::Constant => Box::new(ConstantSequence::new(
                    solver_options.restart_base_interval as i64,
                )),
                SequenceGeneratorType::Geometric => Box::new(GeometricSequence::new(
                    solver_options.restart_base_interval as i64,
                    solver_options.restart_geometric_coef.expect(
                        "Using the geometric sequence for restarts, but the parameter restarts-geometric-coef is not defined.",
                    ),
                )),
                SequenceGeneratorType::Luby => Box::new(LubySequence::new(solver_options.restart_base_interval as i64)),
            };

        let num_conflicts_until_restart = restart_sequence_generator.next();

        GlucoseRestartStrategy {
            restart_sequence_generator,
            num_conflicts_until_restart,
            min_num_conflicts_before_first_restart: solver_options
                .restart_min_num_conflicts_before_first_restart
                as i64,
            lbd_short_term_moving_average: Box::new(WindowedMovingAverage::new(
                solver_options.restart_base_interval,
            )),
            lbd_coef: solver_options.restart_lbd_coef,
            lbd_long_term_moving_average: Box::<CumulativeMovingAverage>::default(),
            num_variables_coef: solver_options.restart_num_assigned_coef,
            num_assigned_variables_moving_average: Box::new(WindowedMovingAverage::new(
                solver_options.restart_num_assigned_window,
            )),
            num_restarts: 0,
            num_blocked_restarts: 0,
        }
    }

    // pub fn new(
    // mut restart_sequence_generator: Box<dyn SequenceGeneratorInterface>,
    // min_num_conflicts_before_first_restart: u64,
    // lbd_short_term_moving_average: Box<dyn MovingAverageInterface>,
    // lbd_long_term_moving_average: Box<dyn MovingAverageInterface>,
    // lbd_coef: f64,
    // num_assigned_variables_moving_average: Box<dyn MovingAverageInterface>,
    // num_variables_coef: f64,
    // ) -> Self {
    // let num_conflicts_until_restart = restart_sequence_generator.next();
    //
    // GlucoseRestartStrategy {
    // restart_sequence_generator,
    // num_conflicts_until_restart,
    // min_num_conflicts_before_first_restart: min_num_conflicts_before_first_restart as i64,
    // lbd_short_term_moving_average,
    // lbd_coef,
    // lbd_long_term_moving_average,
    // num_variables_coef,
    // num_assigned_variables_moving_average,
    // num_restarts: 0,
    // num_blocked_restarts: 0,
    // }
    // }

    pub fn should_restart(&mut self) -> bool {
        // do not restart until a certain number of conflicts take place
        //  this is done to collect some early runtime statistics for the retart strategy
        if self.num_restarts < self.min_num_conflicts_before_first_restart {
            return false;
        }
        // do not restart until a minimum number of conflicts took place after the last restart
        if self.num_conflicts_until_restart > 0 {
            return false;
        }
        // now restarts could be considered
        //  only restart if the solver is learning "bad" clauses
        self.lbd_short_term_moving_average.value()
            > self.lbd_long_term_moving_average.value() * self.lbd_coef
    }

    pub fn notify_conflict(&mut self, lbd: u32, num_literals_on_trail: usize) {
        // update moving averages
        self.num_assigned_variables_moving_average
            .add_term(num_literals_on_trail as u64);
        self.lbd_short_term_moving_average.add_term(lbd as u64);
        self.lbd_long_term_moving_average.add_term(lbd as u64);
        // if the solver has more variables assigned now than in the recent past,
        //  block the restart. The idea is that the solver is 'closer' to finding a solution
        if self.num_restarts >= self.min_num_conflicts_before_first_restart
            && self.num_conflicts_until_restart <= 0
            && num_literals_on_trail as f64
                > self.num_assigned_variables_moving_average.value() * self.num_variables_coef
        {
            // block restarts
            self.num_blocked_restarts += 1;
            self.num_conflicts_until_restart = self.restart_sequence_generator.next();
        }
    }

    pub fn notify_restart(&mut self) {
        self.num_restarts += 1;
        self.num_conflicts_until_restart = self.restart_sequence_generator.next();
        self.lbd_short_term_moving_average
            .adapt(self.num_conflicts_until_restart as u64);
    }
}
