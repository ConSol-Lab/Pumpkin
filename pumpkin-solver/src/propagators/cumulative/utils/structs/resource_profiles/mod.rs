pub(crate) mod point_resource_profile;
pub(crate) mod resource_profile;
pub(crate) mod updatable_resource_profile;

use std::rc::Rc;

pub(crate) use resource_profile::*;

use super::Task;
use crate::variables::IntegerVariable;

pub(crate) trait ResourceProfileInterface<Var>: Clone {
    fn create_default_at_time_point(_time_point: i32) -> Self
    where
        Self: Sized,
    {
        unimplemented!()
    }

    fn create_profile(
        _start: i32,
        _end: i32,
        _profile_tasks: Vec<Rc<Task<Var>>>,
        _height: i32,
        _updated: bool,
    ) -> Self
    where
        Self: Sized,
    {
        unimplemented!()
    }

    fn is_updated(&self) -> bool {
        true
    }

    fn mark_processed(&mut self) {}

    fn mark_updated(&mut self) {}

    fn get_start(&self) -> i32;

    fn get_end(&self) -> i32;

    fn get_height(&self) -> i32;

    fn get_profile_tasks(&self) -> &Vec<Rc<Task<Var>>>;

    fn get_profile_tasks_mut(&mut self) -> &mut Vec<Rc<Task<Var>>>;

    fn add_to_height(&mut self, addition: i32);

    fn add_profile_task(&mut self, task: Rc<Task<Var>>);

    fn remove_profile_task(&mut self, task: &Rc<Task<Var>>);
}

pub(crate) fn remove_task_from_profile<Var: IntegerVariable + 'static>(
    profile_tasks: &mut Vec<Rc<Task<Var>>>,
    to_remove: &Rc<Task<Var>>,
) {
    let _ = profile_tasks.remove(
        profile_tasks
            .iter()
            .position(|profile_task| profile_task.id == to_remove.id)
            .expect("Task should be present"),
    );
}
