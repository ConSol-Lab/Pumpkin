use std::rc::Rc;

use super::remove_task_from_profile;
use super::ResourceProfileInterface;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Structures used for storing the data related to resource profiles;
/// A [`PointResourceProfile`] represents a cumulative mandatory
/// resource usage of the [`profile tasks`][ResourceProfile::profile_tasks] at time point
/// [`PointResourceProfile::point`].
#[derive(Clone, Debug)]
pub(crate) struct PointResourceProfile<Var> {
    /// The start time of the [`ResourceProfile`] (inclusive)
    point: i32,
    /// The IDs of the tasks which are part of the profile
    profile_tasks: Vec<Rc<Task<Var>>>,
    /// The amount of cumulative resource usage of all [`profile
    /// tasks`][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    height: i32,
}

impl<Var> ResourceProfileInterface<Var> for PointResourceProfile<Var>
where
    Var: IntegerVariable + 'static,
{
    fn create_default_at_time_point(time_point: i32) -> Self {
        Self {
            point: time_point,
            profile_tasks: vec![],
            height: 0,
        }
    }

    fn get_start(&self) -> i32 {
        self.point
    }

    fn get_end(&self) -> i32 {
        self.point
    }

    fn get_height(&self) -> i32 {
        self.height
    }

    fn get_profile_tasks(&self) -> &Vec<Rc<Task<Var>>> {
        &self.profile_tasks
    }

    fn get_profile_tasks_mut(&mut self) -> &mut Vec<Rc<Task<Var>>> {
        &mut self.profile_tasks
    }

    fn add_to_height(&mut self, addition: i32) {
        self.height += addition
    }

    fn add_profile_task(&mut self, task: Rc<Task<Var>>) {
        self.profile_tasks.push(task)
    }

    fn remove_profile_task(&mut self, task: &Rc<Task<Var>>) {
        remove_task_from_profile(&mut self.profile_tasks, task)
    }
}
