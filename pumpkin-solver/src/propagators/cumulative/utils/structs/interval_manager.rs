use std::cmp::Ordering;
use std::ops::Range;
use std::rc::Rc;

use itertools::Itertools;

use super::Task;
use crate::containers::KeyedVec;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::ReadDomains;
use crate::propagators::has_overlap_with_interval;
use crate::pumpkin_assert_extreme;
use crate::variables::IntegerVariable;

#[derive(Debug, Clone)]
pub(crate) struct IntervalManager<Var: IntegerVariable> {
    task_to_index: KeyedVec<LocalId, usize>,
    sorted_tasks: Vec<Rc<Task<Var>>>,
    bounds: Vec<Range<i32>>,

    max_length: i32,
    length_outdated: bool,
}

impl<Var: IntegerVariable> IntervalManager<Var> {
    pub(crate) fn new() -> Self {
        let task_to_index = KeyedVec::default();
        let bounds = Vec::new();
        let sorted_tasks = Vec::new();

        Self {
            bounds,
            task_to_index,
            sorted_tasks,
            length_outdated: true,
            max_length: 0,
        }
    }

    pub(crate) fn reset_from_scratch<Context: ReadDomains>(&mut self, context: Context) {
        self.sorted_tasks.sort_unstable_by(|a, b| {
            let interval_a = context.lower_bound(&a.start_variable)
                ..context.upper_bound(&a.start_variable) + a.processing_time;
            let interval_b = context.lower_bound(&b.start_variable)
                ..context.upper_bound(&b.start_variable) + b.processing_time;
            interval_a.cmp(interval_b)
        });
        for i in 0..self.sorted_tasks.len() {
            let task = &self.sorted_tasks[i];
            self.task_to_index[task.id] = i;

            let interval = context.lower_bound(&task.start_variable)
                ..context.upper_bound(&task.start_variable) + task.processing_time;
            self.bounds[task.id.unpack() as usize] = interval;
        }
    }

    pub(crate) fn initialise<Context: ReadDomains>(
        &mut self,
        tasks: &[Rc<Task<Var>>],
        context: Context,
    ) {
        self.sorted_tasks = tasks.to_vec();
        for (i, task) in tasks.iter().enumerate() {
            let _ = self.task_to_index.push(i);
            let interval = context.lower_bound(&task.start_variable)
                ..context.upper_bound(&task.start_variable) + task.processing_time;
            self.bounds.push(interval);
        }
        self.sorted_tasks.sort_by(|a, b| {
            let interval_a = context.lower_bound(&a.start_variable)
                ..context.upper_bound(&a.start_variable) + a.processing_time;
            let interval_b = context.lower_bound(&b.start_variable)
                ..context.upper_bound(&b.start_variable) + b.processing_time;
            interval_a.cmp(interval_b)
        });

        for (index, task) in self.sorted_tasks.iter().enumerate() {
            self.task_to_index[task.id] = index;
        }
    }

    fn swap(&mut self, index: usize, other_index: usize) {
        self.task_to_index[self.sorted_tasks[index].id] = other_index;
        self.task_to_index[self.sorted_tasks[other_index].id] = index;

        self.sorted_tasks.swap(index, other_index);
    }

    pub(crate) fn update_task(&mut self, updated_task: &Rc<Task<Var>>, new_bounds: &Range<i32>) {
        let mut task_index = self.task_to_index[updated_task.id];
        while task_index > 0 {
            let comparing_task = &self.sorted_tasks[task_index - 1];
            let stored_bounds = self.bounds[comparing_task.id.unpack() as usize].clone();
            if matches!(new_bounds.clone().cmp(stored_bounds), Ordering::Less) {
                self.swap(task_index, task_index - 1);
            } else {
                break;
            }
            task_index -= 1;
        }

        while task_index < self.sorted_tasks.len() - 1 {
            let comparing_task = &self.sorted_tasks[task_index + 1];
            let stored_bounds = self.bounds[comparing_task.id.unpack() as usize].clone();
            if matches!(new_bounds.clone().cmp(stored_bounds), Ordering::Greater) {
                self.swap(task_index, task_index + 1);
            } else {
                break;
            }
            task_index += 1;
        }

        if !self.length_outdated {
            let previous_bound = &self.bounds[updated_task.id.unpack() as usize];
            self.length_outdated = previous_bound.end - previous_bound.start == self.max_length;
        }
        self.bounds[updated_task.id.unpack() as usize] = new_bounds.clone();
    }

    fn get_max_length(&mut self) -> i32 {
        if self.length_outdated {
            self.length_outdated = false;

            self.max_length = self
                .bounds
                .iter()
                .map(|bound| bound.end - bound.start)
                .max()
                .unwrap_or_default();
        }
        self.max_length
    }

    pub(crate) fn seek<'a>(
        &'a mut self,
        start: i32,
        end: i32,
        cursor: &'a mut usize,
        context: PropagationContext,
        first_iteration: bool,
    ) -> IntervalIterator<'a, Var> {
        pumpkin_assert_extreme!((0..self.sorted_tasks.len()).tuple_windows().all(
            |(first_index, second_index)| {
                let first_task = &self.sorted_tasks[first_index];
                let second_task = &self.sorted_tasks[second_index];
                !matches!(
                    self.bounds[first_task.id.unpack() as usize]
                        .clone()
                        .cmp(self.bounds[second_task.id.unpack() as usize].clone()),
                    Ordering::Greater
                )
            }
        ));

        // If the cursor is uninitialised then we move it
        if first_iteration
            || (*cursor < self.sorted_tasks.len() && {
                let task = &self.sorted_tasks[*cursor];
                self.bounds[task.id.unpack() as usize].start > start
            })
        {
            pumpkin_assert_extreme!((0..self.sorted_tasks.len()).all(|index| {
                let task = &self.sorted_tasks[index];
                let bounds = self.bounds[task.id.unpack() as usize].clone();

                let result = matches!(
                    bounds.clone().cmp(
                        context.lower_bound(&task.start_variable)
                            ..context.upper_bound(&task.start_variable) + task.processing_time,
                    ),
                    Ordering::Equal
                );

                if !result {
                    eprintln!(
                        "{:?} does not match {:?} for task: {:?}",
                        bounds,
                        context.lower_bound(&task.start_variable)
                            ..context.upper_bound(&task.start_variable) + task.processing_time,
                        task
                    )
                }
                result
            }));
            let earliest_start_considered =
                start.checked_sub(self.get_max_length()).unwrap_or_default();
            *cursor = self.lower_bound(earliest_start_considered);
        }

        // Then we keep going starting from the cursor until we find an element such that the start
        // time is equal to or larger than the start time
        while *cursor + 1 < self.sorted_tasks.len()
            && self.bounds[self.sorted_tasks[*cursor + 1].id.unpack() as usize].start
                < start.checked_sub(self.get_max_length()).unwrap_or_default()
        {
            *cursor += 1;
        }

        pumpkin_assert_extreme!(self.sorted_tasks[..*cursor].iter().all(|task| {
            let stored_bounds = &self.bounds[task.id.unpack() as usize];
            let result = !has_overlap_with_interval(stored_bounds.start, stored_bounds.end, start, end);
            if !result {
                let pointed_task = &self.sorted_tasks[*cursor];
                let pointed_bounds = &self.bounds[pointed_task.id.unpack() as usize];
                eprintln!("Found overlap between {stored_bounds:?} for task {task:?} with the interval {:?}\nCursor is pointing to {pointed_task:?} with bounds {pointed_bounds:?}", start..=end)
            }
            result
        }));

        IntervalIterator {
            inner: self,
            cursor: *cursor,
            start,
            stop: end,
        }
    }

    fn lower_bound(&self, start: i32) -> usize {
        // Taken from https://github.com/sstadick/rust-lapper/blob/master/src/lib.rs#L396
        let mut size = self.sorted_tasks.len();
        let mut low = 0;

        while size > 0 {
            let half = size / 2;
            let other_half = size - half;
            let probe = low + half;
            let other_low = low + other_half;
            let v = &self.sorted_tasks[probe];
            size = half;
            low = if self.bounds[v.id.unpack() as usize].start < start {
                other_low
            } else {
                low
            }
        }
        low
    }
}

pub(crate) struct IntervalIterator<'a, Var: IntegerVariable> {
    inner: &'a IntervalManager<Var>,
    cursor: usize,
    start: i32,
    stop: i32,
}

impl<'a, Var: IntegerVariable> Iterator for IntervalIterator<'a, Var> {
    type Item = &'a Rc<Task<Var>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cursor < self.inner.sorted_tasks.len() {
            let task = &self.inner.sorted_tasks[self.cursor];
            self.cursor += 1;
            let interval = &self.inner.bounds[task.id.unpack() as usize];

            let is_overlapping =
                has_overlap_with_interval(interval.start, interval.end, self.start, self.stop);
            if is_overlapping {
                return Some(task);
            } else if interval.start > self.stop {
                break;
            }
        }
        None
    }
}
