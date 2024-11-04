pub(crate) mod point_resource_profile;
pub(crate) mod resource_profile;
pub(crate) mod updatable_resource_profile;

use std::rc::Rc;

pub(crate) use resource_profile::*;

use super::Task;

pub(crate) trait ResourceProfileInterface<Var> {
    fn is_updated(&self) -> bool {
        true
    }

    fn mark_updated(&mut self) {}

    fn mark_processed(&mut self) {}

    fn get_start(&self) -> i32;

    fn get_end(&self) -> i32;

    fn get_height(&self) -> i32;

    fn get_profile_tasks(&self) -> &Vec<Rc<Task<Var>>>;

    fn get_profile_tasks_mut(&mut self) -> &mut Vec<Rc<Task<Var>>>;

    fn add_to_height(&mut self, addition: i32);

    fn add_profile_task(&mut self, task: Rc<Task<Var>>);

    fn remove_profile_task(&mut self, task: &Rc<Task<Var>>);
}
