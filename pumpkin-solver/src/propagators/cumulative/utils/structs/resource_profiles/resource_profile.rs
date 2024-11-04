use std::rc::Rc;

use super::remove_task_from_profile;
use super::ResourceProfileInterface;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Structures used for storing the data related to resource profiles;
/// A [`ResourceProfile`] represents a rectangle where the height is the cumulative mandatory
/// resource usage of the [`profile tasks`][ResourceProfile::profile_tasks]
#[derive(Clone, Debug)]
pub(crate) struct ResourceProfile<Var> {
    /// The start time of the [`ResourceProfile`] (inclusive)
    start: i32,
    /// The end time of the [`ResourceProfile`] (inclusive)
    end: i32,
    /// The IDs of the tasks which are part of the profile
    profile_tasks: Vec<Rc<Task<Var>>>,
    /// The amount of cumulative resource usage of all [`profile
    /// tasks`][ResourceProfile::profile_tasks] (i.e. the height of the rectangle)
    height: i32,
}

impl<Var: IntegerVariable + 'static> ResourceProfile<Var> {
    pub(crate) fn new(
        start: i32,
        end: i32,
        profile_tasks: Vec<Rc<Task<Var>>>,
        height: i32,
    ) -> Self {
        Self {
            start,
            end,
            profile_tasks,
            height,
        }
    }
}

impl<Var: IntegerVariable + 'static> ResourceProfileInterface<Var> for ResourceProfile<Var> {
    fn create_profile(start: i32, end: i32, profile_tasks: Vec<Rc<Task<Var>>>, height: i32) -> Self
    where
        Self: Sized,
    {
        Self {
            start,
            end,
            profile_tasks,
            height,
        }
    }

    fn get_start(&self) -> i32 {
        self.start
    }

    fn get_end(&self) -> i32 {
        self.end
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
