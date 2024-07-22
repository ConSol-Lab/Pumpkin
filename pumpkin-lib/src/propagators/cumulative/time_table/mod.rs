//! Contains the time-table propagators which use so-called time-table reasoning
//! for propagating the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint.
//!
//! # Theoretical
//!
//! These time-table propagators reason about something called the **mandatory part** of a [`Task`];
//! informally, the mandatory part of a [`Task`] is the time points in which a [`Task`] *has* to
//! execute given its current bounds. Mathematically (see [`crate::propagators::cumulative`] for
//! notation details), the mandatory part of a [`Task`] *i* is the interval `[LST_i, ECT_i)`
//! (i.e. the time points between the latest start time of a task and its earliest completion time).
//!
//! The so-called **time-table** is a data-structure which are used to track the cumulative
//! mandatory parts at different time-points. Our time-tables consist of [`ResourceProfile`]s which
//! represent the cumulative resource usage ([`height`][ResourceProfile::height]) across an interval
//! (\[[`start`][ResourceProfile::start], [`end`][ResourceProfile::end]\]) of a set of [`Task`]s
//! ([`profile_tasks`][ResourceProfile::profile_tasks]).
//!
//! Propagation oftentimes uses these time-tables to either update the bounds of the [`Task`]s or to
//! remove values from the domain. If a time-table has been built then for any [`Task`] which
//! overflows the resource capacity if it overlaps with a [`ResourceProfile`] (and is not part of
//! it) all start times which cause this task to overlap with any part of the [`ResourceProfile`]
//! can be removed from the domain.
//!
//! The simplest example of this is if we have a resource with capacity 1 and we have the following
//! two [`Task`]s:
//! - Task 1: Start times: [0, 5], Processing time: 4, Resource usage: 1
//! - Task 2: Start times: [3, 3], Processing time: 2, Resource usage: 1
//!
//! In this case the time-table would consist of a single [`ResourceProfile`] with
//! [`start`][ResourceProfile::start] 3 and [`end`][ResourceProfile::end] 4 signifying that Task 2
//! executes in the interval `[3, 4]` for 2 units of time. It can be seen that if Task 1 is
//! scheduled at the earliest possible starting time of 0 that there would be an overflow of the
//! resource, we could thus propagate the lower-bound on the start time of Task 1 to be 5.
//!
//! There are several algorithms which perform time-table reasoning with varying complexities and
//! with varying strengths such as [\[2\]](https://pure.tue.nl/ws/portalfiles/portal/2374269/431902.pdf),
//! [\[3\]](https://www.diva-portal.org/smash/get/diva2:1041645/FULLTEXT01.pdf) and
//! [\[4\]](https://dial.uclouvain.be/pr/boreal/object/boreal%3A171186/datastream/PDF_01/view).
//! For more information about explanations for this type of reasoning see
//! [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
//! for more information about time-table reasoning
//!
//! # Practical
//!
//! Certain common functions are stored in
//! [`crate::propagators::cumulative::time_table::time_table_util`] such as
//! [`should_enqueue`] which determines whether a time-table propagator has seen sufficient changes
//! to warrant being scheduled once more or [`propagate_based_on_timetable`] which goes over all
//! profiles and tasks and determines whether a propagation can take place and performs it if this
//! is the case. It should be noted that these methods assume that the provided [`ResourceProfile`]s
//! are maximal (i.e. there is no [`ResourceProfile`] adjacent to it with the same
//! [`ResourceProfile::profile_tasks`]) and that they are sorted in increasing order of start time;
//! not adhering to these guidelines could result in missed propagations.
//!
//! # Bibliography
//!
//! \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
//! Computer Science and Software Engineering, 2011.
//!
//! \[2\] W. P. M. Nuijten, ‘Time and resource constrained scheduling: a constraint satisfaction
//! approach’, 1994.
//!
//! \[3\] N. Beldiceanu and M. Carlsson, ‘A new multi-resource cumulatives constraint with negative
//! heights’, in International Conference on Principles and Practice of Constraint Programming,
//! 2002, pp. 63–79.
//!
//! \[4\] S. Gay, R. Hartert, and P. Schaus, ‘Simple and scalable time-table filtering for the
//! cumulative constraint’, in Principles and Practice of Constraint Programming: 21st International
//! Conference, CP 2015, Cork, Ireland, August 31--September 4, 2015, Proceedings 21, 2015, pp.
//! 149–157.

mod explanations;
mod propagation_handler;
mod time_table_over_interval;
mod time_table_over_interval_incremental;
mod time_table_per_point;
mod time_table_per_point_incremental;
mod time_table_util;
pub use explanations::CumulativeExplanationType;
pub(crate) use time_table_over_interval::*;
pub(crate) use time_table_over_interval_incremental::*;
pub(crate) use time_table_per_point::*;
pub(crate) use time_table_per_point_incremental::*;

#[cfg(doc)]
use crate::propagators::cumulative::time_table::time_table_util::*;
#[cfg(doc)]
use crate::propagators::Task;
