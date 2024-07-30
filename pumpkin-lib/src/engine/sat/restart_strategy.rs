use std::fmt::Debug;

use crate::basic_types::moving_averages::CumulativeMovingAverage;
use crate::basic_types::moving_averages::MovingAverage;
use crate::basic_types::moving_averages::WindowedMovingAverage;
use crate::basic_types::sequence_generators::ConstantSequence;
use crate::basic_types::sequence_generators::GeometricSequence;
use crate::basic_types::sequence_generators::LubySequence;
use crate::basic_types::sequence_generators::SequenceGenerator;
use crate::basic_types::sequence_generators::SequenceGeneratorType;

/// The options which are used by the solver to determine when a restart should occur.
///
/// An implementation of a restart strategy based on the specfication of [Section 4 of \[1\]](https://fmv.jku.at/papers/BiereFroehlich-POS15.pdf)
/// (for more information about the Glucose restart strategies see [\[2\]](https://www.cril.univ-artois.fr/articles/xxmain.pdf) and
/// [\[3\]](http://www.pragmaticsofsat.org/2012/slides-glucose.pdf)). The idea is to restart when the
/// the quality of the learned clauses (indicated by the LBD [\[4\]](https://www.ijcai.org/Proceedings/09/Papers/074.pdf)
/// which is the number of different decision levels present in a learned clause, lower is generally
/// better) is poor. It takes this into account by keeping track of the overall average LBD and the
/// short-term average LBD and comparing these with one another.
///
/// The strategy also takes into account that if a solver is "close" to finding a solution that it
/// would be better to not restart at that moment and it can then decide to skip a restart.
///
/// It generalises the aforementioned Glucose restart strategy by using adjustable
/// [`RestartOptions`] which allows the user to also specify Luby restarts
/// (see [\[5\]](https://www.sciencedirect.com/science/article/pii/0020019093900299)) and
/// constant restarts (see [Section 3 of \[1\]](https://fmv.jku.at/papers/BiereFroehlich-POS15.pdf)).
///
/// # Bibliography
/// \[1\] A. Biere and A. Fröhlich, ‘Evaluating CDCL restart schemes’, Proceedings of Pragmatics of
/// SAT, pp. 1–17, 2015.
///
/// \[2\] G. Audemard and L. Simon, ‘Refining restarts strategies for SAT and UNSAT’, in Principles
/// and Practice of Constraint Programming: 18th International Conference, CP 2012, Québec City, QC,
/// Canada, October 8-12, 2012. Proceedings, 2012, pp. 118–126.
///
/// \[3\] G. Audemard and L. Simon, ‘Glucose 2.1: Aggressive, but reactive, clause database
/// management, dynamic restarts (system description)’, in Pragmatics of SAT 2012 (POS’12), 2012.
///
/// \[4\] G. Audemard and L. Simon, ‘Predicting learnt clauses quality in modern SAT solvers’, in
/// Twenty-first international joint conference on artificial intelligence, 2009.
///
/// \[5\] M. Luby, A. Sinclair, and D. Zuckerman, ‘Optimal speedup of Las Vegas algorithms’,
/// Information Processing Letters, vol. 47, no. 4, pp. 173–180, 1993.
#[derive(Debug, Clone, Copy)]
pub struct RestartOptions {
    /// Decides the sequence based on which the restarts are performed.
    /// To be used in combination with [`RestartOptions::base_interval`]
    pub sequence_generator_type: SequenceGeneratorType,
    /// The base interval length is used as a multiplier to the restart sequence.
    /// For example, constant restarts with base interval 100 means a retart is triggered every 100
    /// conflicts.
    pub base_interval: u64,
    /// The minimum number of conflicts to be reached before the first restart is considered
    pub min_num_conflicts_before_first_restart: u64,
    /// Used to determine if a restart should be forced.
    /// The state is "bad" if the current LBD value is much greater than the global LBD average A
    /// greater/lower value for lbd-coef means a less/more frequent restart policy
    pub lbd_coef: f64,
    /// Used to determine if a restart should be blocked.
    /// To be used in combination with
    /// [`RestartOptions::num_assigned_window`].
    /// A restart is blocked if the number of assigned propositional variables is must greater than
    /// the average number of assigned variables in the recent past A greater/lower value for
    /// [`RestartOptions::num_assigned_coef`] means fewer/more blocked restarts
    pub num_assigned_coef: f64,
    /// Used to determine the length of the recent past that should be considered when deciding on
    /// blocking restarts. The solver considers the last
    /// [`RestartOptions::num_assigned_window`] conflicts as the reference point for the
    /// number of assigned variables
    pub num_assigned_window: u64,
    /// The coefficient in the geometric sequence `x_i = x_{i-1} * geometric-coef` where `x_1 =
    /// `[`RestartOptions::base_interval`]. Used only if
    /// [`RestartOptions::sequence_generator_type`] is assigned to
    /// [`SequenceGeneratorType::Geometric`].
    pub geometric_coef: Option<f64>,
}

impl Default for RestartOptions {
    fn default() -> Self {
        // The values which are used are based on [Glucose](https://github.com/audemard/glucose).
        Self {
            sequence_generator_type: SequenceGeneratorType::Constant,
            base_interval: 50,
            min_num_conflicts_before_first_restart: 10000,
            lbd_coef: 1.25,
            num_assigned_coef: 1.4,
            num_assigned_window: 5000,
            geometric_coef: None,
        }
    }
}

#[derive(Debug)]
pub(crate) struct RestartStrategy {
    /// A generator for determining how many conflicts should be found before the next restart is
    /// able to take place (one example of such a generator is [`LubySequence`]).
    sequence_generator: Box<dyn SequenceGenerator>,
    /// The number of conflicts encountered since the last restart took place
    number_of_conflicts_encountered_since_restart: u64,
    /// The minimum number of conflicts until the next restart is able to take place (note that if
    /// [`RestartStrategy::number_of_conflicts_encountered_since_restart`] >
    /// [`RestartStrategy::number_of_conflicts_until_restart`] it does not necessarily mean
    /// that a restart is guaranteed to take place in the next call to
    /// [`RestartStrategy::should_restart`]).
    number_of_conflicts_until_restart: u64,
    /// The minimum number of conflicts until the first restart is able to take place
    minimum_number_of_conflicts_before_first_restart: u64,
    /// The recent average of LBD values, used in [`RestartStrategy::should_restart`].
    lbd_short_term_moving_average: Box<dyn MovingAverage>,
    /// A coefficient which influences the decision whether a restart should take place in
    /// [`RestartStrategy::should_restart`], the higher this value, the fewer restarts are
    /// performed.
    lbd_coefficient: f64,
    /// The long-term average of LBD values, used in [`RestartStrategy::should_restart`].
    lbd_long_term_moving_average: Box<dyn MovingAverage>,
    /// A coefficient influencing whether a restart will be blocked in
    /// [`RestartStrategy::notify_conflict`], the higher the value, the fewer restarts are
    /// performed.
    number_of_variables_coefficient: f64,
    /// The average number of variables which are assigned, used in
    /// [`RestartStrategy::notify_conflict`].
    number_of_assigned_variables_moving_average: Box<dyn MovingAverage>,
    /// The number of restarts which have been performed.
    number_of_restarts: u64,
    /// The number of restarts which have been blocked.
    number_of_blocked_restarts: u64,
}

impl Default for RestartStrategy {
    fn default() -> Self {
        RestartStrategy::new(RestartOptions::default())
    }
}

impl RestartStrategy {
    pub(crate) fn new(options: RestartOptions) -> Self {
        let mut sequence_generator: Box<dyn SequenceGenerator> =
            match options.sequence_generator_type {
                SequenceGeneratorType::Constant => Box::new(ConstantSequence::new(
                    options.base_interval as i64,
                )),
                SequenceGeneratorType::Geometric => Box::new(GeometricSequence::new(
                    options.base_interval as i64,
                    options.geometric_coef.expect(
                        "Using the geometric sequence for restarts, but the parameter restarts-geometric-coef is not defined.",
                    ),
                )),
                SequenceGeneratorType::Luby => Box::new(LubySequence::new(options.base_interval as i64)),
            };

        let number_of_conflicts_until_restart = sequence_generator.next().try_into().expect("Expected restart generator to generate a positive value but it generated a negative one");

        RestartStrategy {
            sequence_generator,
            number_of_conflicts_encountered_since_restart: 0,
            number_of_conflicts_until_restart,
            minimum_number_of_conflicts_before_first_restart: options
                .min_num_conflicts_before_first_restart,
            lbd_short_term_moving_average: Box::new(WindowedMovingAverage::new(
                options.base_interval,
            )),
            lbd_coefficient: options.lbd_coef,
            lbd_long_term_moving_average: Box::<CumulativeMovingAverage>::default(),
            number_of_variables_coefficient: options.num_assigned_coef,
            number_of_assigned_variables_moving_average: Box::new(WindowedMovingAverage::new(
                options.num_assigned_window,
            )),
            number_of_restarts: 0,
            number_of_blocked_restarts: 0,
        }
    }

    /// Determines whether the restart strategy indicates that a restart should take place; the
    /// strategy considers three conditions (in this order):
    /// - If no restarts have taken place yet then a restart can only take place if the number of
    ///   conflicts encountered since the last restart is larger or equal to
    ///   [`RestartOptions::min_num_conflicts_before_first_restart`]
    /// - If the previous condition is met then a restart can only take place if the number of
    ///   conflicts encountered since the last restart is larger or equal to the number of conflicts
    ///   until the next restart as indicated by the restart sequence generator specified in
    ///   [`RestartOptions::sequence_generator_type`]
    /// - If both of the previous conditions hold then it is determined whether the short-term
    ///   average LBD is lower than the long-term average LBD multiplied by
    ///   [`RestartOptions::lbd_coef`], this condition determines whether the solver is learning
    ///   "bad" clauses based on the LBD; if it is learning "sufficiently bad" clauses then a
    ///   restart will be performed.
    pub(crate) fn should_restart(&self) -> bool {
        // Do not restart until a certain number of conflicts take place before the first restart
        // this is done to collect some early runtime statistics for the restart strategy
        if self.should_restart_first_time() {
            return false;
        }

        // Do not restart until a minimum number of conflicts took place after the last restart
        if !self.should_trigger_later_restart() {
            return false;
        }

        // Restarts can now be considered!
        // Only restart if the solver is learning "bad" clauses, this is the case if the long-term
        // average lbd multiplied by the `lbd_coefficient` is lower than the short-term average lbd
        self.lbd_long_term_moving_average.value() * self.lbd_coefficient
            <= self.lbd_short_term_moving_average.value()
    }

    fn should_restart_first_time(&self) -> bool {
        self.number_of_restarts == 0
            && self.number_of_conflicts_encountered_since_restart
                < self.minimum_number_of_conflicts_before_first_restart
    }

    /// Notifies the restart strategy that a conflict has taken place so that it can adjust its
    /// internal values, this method has the additional responsibility of checking whether a restart
    /// should be blocked based on whether the solver is "sufficiently close" to finding a solution.
    pub(crate) fn notify_conflict(&mut self, lbd: u32, number_of_pruned_values: u64) {
        // Update moving averages
        self.number_of_assigned_variables_moving_average
            .add_term(number_of_pruned_values);
        self.lbd_short_term_moving_average.add_term(lbd as u64);
        self.lbd_long_term_moving_average.add_term(lbd as u64);

        // Increase the number of conflicts encountered since the last restart
        self.number_of_conflicts_encountered_since_restart += 1;

        if self.should_block_restart(number_of_pruned_values) {
            // Restart has been blocked
            self.number_of_blocked_restarts += 1;
            self.reset_values()
        }
    }

    fn should_block_restart(&self, number_of_pruned_values: u64) -> bool {
        // If the solver has more variables assigned now than in the recent past, then block the
        // restart. The idea is that the solver is 'closer' to finding a solution and restarting
        // could be harmful to the performance

        if self.should_restart_first_time() {
            // Don't block the first restart.
            return false;
        }

        let close_to_solution = number_of_pruned_values as f64
            > self.number_of_assigned_variables_moving_average.value()
                * self.number_of_variables_coefficient;

        self.should_trigger_later_restart() && close_to_solution
    }

    fn should_trigger_later_restart(&self) -> bool {
        self.number_of_conflicts_until_restart <= self.number_of_conflicts_encountered_since_restart
    }

    /// Notifies the restart strategy that a restart has taken place so that it can adjust its
    /// internal values
    pub(crate) fn notify_restart(&mut self) {
        self.number_of_restarts += 1;
        self.reset_values()
    }

    /// Resets the values related to determining whether a restart takes place; this method should
    /// be called whenever a restart has taken place or should have taken place and was blocked.
    fn reset_values(&mut self) {
        self.number_of_conflicts_until_restart =
            self.sequence_generator.next().try_into().expect("Expected restart generator to generate a positive value but it generated a negative one");
        self.number_of_conflicts_encountered_since_restart = 0;
        self.lbd_short_term_moving_average
            .adapt(self.number_of_conflicts_until_restart);
    }
}
