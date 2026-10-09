//! One propagator for all the deductions of a scaffold.
//!
//! The scaffold's deductions are nogoods. Posted as one propagator each, a nogood is registered
//! with the engine on every predicate it mentions, so a predicate that appears in many nogoods
//! has a watch list thousands long, and each time it becomes true the engine dispatches every
//! propagator on that list, at a cost far above the decision each makes.
//!
//! This propagator holds all the nogoods, the way the solver's own nogood store does:
//! two watched predicates per nogood, a blocker per watcher so a nogood that cannot act is
//! recognised without reading it, and one registration with the engine per distinct predicate
//! however many nogoods mention it. Deductions are added and deactivated without the engine
//! learning about it.
//!
//! The processor keeps up to three of these, one per priority level, because trimming relies on
//! unmarked conflict detection running before unmarked unit propagation and both running after
//! marked deductions. A nogood moves between databases when it is marked.
//!
//! The processor registers each predicate of a nogood with the engine when the first nogood
//! mentioning it is added, so the scaffold is read once, as it arrives, and wakes the database
//! when a freshly added nogood has to act before anything else changes.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering as AtomicOrdering;

use pumpkin_core::containers::KeyedVec;
use pumpkin_core::containers::StorageKey;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::PredicateId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;

declare_inference_label!(Nogood);

/// Register the runtime checker for a deduction and return the inference code its propagations
/// carry.
pub(crate) fn nogood_inference_code(
    state: &mut pumpkin_core::state::State,
    constraint_tag: pumpkin_core::proof::ConstraintTag,
    predicates: &[Predicate],
) -> InferenceCode {
    state.add_inference_checker(
        constraint_tag,
        Nogood,
        pumpkin_core::propagators::nogoods::NogoodChecker {
            nogood: predicates.iter().copied().collect(),
        },
    )
}

/// Instrumentation: how often the database is woken, how often that leads to being enqueued, and
/// how many watchers it visits.
pub(crate) static DATABASE_NOTIFICATIONS: AtomicU64 = AtomicU64::new(0);
pub(crate) static DATABASE_ENQUEUED: AtomicU64 = AtomicU64::new(0);
pub(crate) static DATABASE_WATCHERS_VISITED: AtomicU64 = AtomicU64::new(0);

/// What a database does with the nogoods it holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DatabaseMode {
    /// Detect conflicts and propagate: the treatment of marked deductions.
    ConflictsAndPropagation,
    /// Only report a nogood whose predicates all hold.
    ConflictsOnly,
    /// Only propagate the last unsatisfied predicate of a nogood.
    PropagationOnly,
}

impl DatabaseMode {
    fn detects_conflicts(self) -> bool {
        !matches!(self, DatabaseMode::PropagationOnly)
    }

    fn propagates(self) -> bool {
        !matches!(self, DatabaseMode::ConflictsOnly)
    }
}

/// The position of a nogood. Databases sharing a scaffold share this numbering.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct NogoodIndex(pub(crate) u32);

impl StorageKey for NogoodIndex {
    fn index(&self) -> usize {
        self.0 as usize
    }

    fn create_from_index(index: usize) -> Self {
        NogoodIndex(index as u32)
    }
}

#[derive(Clone, Debug)]
struct Entry {
    predicates: Rc<[Predicate]>,
    inference_code: InferenceCode,
    /// Indices into `predicates` of the two watched predicates; equal for a unit nogood.
    watched: [usize; 2],
    active: bool,
}

/// A reference from a predicate's watch list to a nogood watching it.
#[derive(Clone, Copy, Debug)]
struct Watcher {
    nogood: NogoodIndex,
    /// A predicate of the nogood other than the watched one. While it is false the nogood holds
    /// and can do nothing, which is decided without reading the nogood.
    blocker: Predicate,
}

/// Constructs an empty [`DeductionDatabase`].
#[derive(Debug)]
pub(crate) struct DeductionDatabaseConstructor {
    pub(crate) mode: DatabaseMode,
    pub(crate) priority: Priority,
}

impl PropagatorConstructor for DeductionDatabaseConstructor {
    type PropagatorImpl = DeductionDatabase;

    fn create(
        self,
        _context: PropagatorConstructorContext,
    ) -> PropagatorSpec<Self::PropagatorImpl> {
        let propagator = DeductionDatabase {
            mode: self.mode,
            priority: self.priority,
            entries: KeyedVec::default(),
            watch_lists: KeyedVec::default(),
            unsorted: KeyedVec::default(),
            predicate_ids: HashMap::new(),
            updated: VecDeque::new(),
            pending: vec![],
        };

        PropagatorSpec {
            registration: EventsToRegister::empty(),
            checkers: RuntimeCheckers::builder().build(),
            propagator,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct DeductionDatabase {
    mode: DatabaseMode,
    priority: Priority,
    entries: KeyedVec<NogoodIndex, Option<Entry>>,
    /// The watchers of each predicate, in nogood order whenever a list is read; see
    /// [`DeductionDatabase::insert_watcher`].
    watch_lists: KeyedVec<PredicateId, Vec<Watcher>>,
    /// Whether a watcher was put on the list out of nogood order since it was last sorted.
    unsorted: KeyedVec<PredicateId, bool>,
    predicate_ids: HashMap<Predicate, PredicateId>,
    /// Watched predicates that became true and have not been fully processed. The front entry
    /// may be partly processed: the database stops after one propagation so that propagators of
    /// higher priority run in between, which is the order the per-deduction scheme had.
    updated: VecDeque<PredicateId>,
    /// Nogoods added with at most one unsatisfied predicate, which must be examined the next time
    /// the database runs rather than when a watched predicate changes.
    pending: Vec<NogoodIndex>,
}

impl DeductionDatabase {
    /// Record the identifier the engine notifies the database with for the predicate, which the
    /// processor registers, with its negation, before adding the first nogood that mentions it.
    /// A propagation posts the negation of a nogood predicate; being notified of it is what lets
    /// the database resume when it stopped after one propagation.
    pub(crate) fn note_predicate(&mut self, predicate: Predicate, predicate_id: PredicateId) {
        let _ = self.predicate_ids.insert(predicate, predicate_id);
    }

    /// Put a watcher on a predicate's list, which is put back in nogood order before it is next
    /// read.
    ///
    /// Among the nogoods able to propagate the same predicate, the earliest in the scaffold
    /// propagates and is the one marked, which is what trimming wants: an early deduction is
    /// available to every later conflict.
    /// Inserting in place would move the rest of a list thousands long each time, so the watcher
    /// is appended and the list sorted when read, which for a list sorted but for a few appended
    /// watchers costs little more than reading it.
    fn insert_watcher(&mut self, predicate_id: PredicateId, watcher: Watcher) {
        self.watch_lists.accomodate(predicate_id, vec![]);
        self.unsorted.accomodate(predicate_id, false);
        let list = &mut self.watch_lists[predicate_id];
        if list
            .last()
            .is_some_and(|last| last.nogood.0 > watcher.nogood.0)
        {
            self.unsorted[predicate_id] = true;
        }
        list.push(watcher);
    }

    /// Put the watch list of the predicate in nogood order.
    fn sort_watch_list(&mut self, predicate_id: PredicateId) {
        if self.unsorted.get(predicate_id).copied().unwrap_or(false) {
            self.watch_lists[predicate_id].sort_by_key(|watcher| watcher.nogood.0);
            self.unsorted[predicate_id] = false;
        }
    }

    /// Add a nogood watching the predicates at `watched`.
    ///
    /// The processor chooses the pair since it has the state: two unsatisfied predicates when
    /// there are two, otherwise the most recently satisfied ones, which is what keeps the watches
    /// valid across backtracking. `immediate` says fewer than two were unsatisfied, so the nogood
    /// may already propagate or conflict.
    pub(crate) fn add(
        &mut self,
        index: NogoodIndex,
        predicates: Rc<[Predicate]>,
        inference_code: InferenceCode,
        watched: [usize; 2],
        immediate: bool,
    ) {
        for slot in 0..2 {
            if predicates.is_empty() || (slot == 1 && watched[0] == watched[1]) {
                break;
            }

            let id = self.predicate_ids[&predicates[watched[slot]]];
            self.insert_watcher(
                id,
                Watcher {
                    nogood: index,
                    blocker: predicates[watched[1 - slot]],
                },
            );
        }

        self.entries.accomodate(index, None);
        self.entries[index] = Some(Entry {
            predicates,
            inference_code,
            watched,
            active: true,
        });

        if immediate {
            self.pending.push(index);
        }
    }

    /// The inference code of an active nogood.
    pub(crate) fn inference_code(&self, index: NogoodIndex) -> Option<InferenceCode> {
        match self.entries.get(index) {
            Some(Some(entry)) if entry.active => Some(entry.inference_code.clone()),
            _ => None,
        }
    }

    /// Stop the nogood from doing anything. Its watchers are dropped when next encountered.
    pub(crate) fn deactivate(&mut self, index: NogoodIndex) {
        if let Some(Some(entry)) = self.entries.get_mut(index) {
            entry.active = false;
        }
    }

    /// Examine a nogood in full: conflict if everything holds, propagate if one predicate is
    /// left, in accordance with the mode.
    fn examine(
        &self,
        index: NogoodIndex,
        context: &mut PropagationContext,
    ) -> Result<(), Conflict> {
        let Some(entry) = self.entries[index].as_ref() else {
            return Ok(());
        };

        if !entry.active {
            return Ok(());
        }

        let mut unsatisfied = None;
        let mut num_unsatisfied = 0;

        for &predicate in entry.predicates.iter() {
            if context.evaluate_predicate(predicate) != Some(true) {
                num_unsatisfied += 1;
                unsatisfied = Some(predicate);

                if num_unsatisfied == 2 {
                    return Ok(());
                }
            }
        }

        match unsatisfied {
            None if self.mode.detects_conflicts() => {
                Err(Conflict::Propagator(PropagatorConflict {
                    conjunction: entry.predicates.iter().copied().collect(),
                    inference_code: entry.inference_code.clone(),
                }))
            }

            Some(predicate)
                if self.mode.propagates() && context.evaluate_predicate(predicate).is_none() =>
            {
                let explanation = entry
                    .predicates
                    .iter()
                    .copied()
                    .filter(|&other| other != predicate)
                    .collect::<PropositionalConjunction>();

                // The predicate is unassigned, so this only ever reports an explicit conflict
                // through the engine, never an empty domain.
                context.post(!predicate, (explanation, &entry.inference_code))?;
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Whether the nogood conflicts: every predicate holds. Read-only, for the pass that looks
    /// for conflicts before anything is propagated.
    fn conflicts(&self, index: NogoodIndex, context: &PropagationContext) -> Option<Conflict> {
        let entry = self.entries[index].as_ref()?;

        if !entry.active {
            return None;
        }

        // Both watched predicates are in the nogood, so it conflicts only if they hold, which is
        // decided without reading the rest of it.
        let [first, second] = entry.watched;
        if context.evaluate_predicate(entry.predicates[first]) != Some(true)
            || context.evaluate_predicate(entry.predicates[second]) != Some(true)
        {
            return None;
        }

        entry
            .predicates
            .iter()
            .all(|&predicate| context.evaluate_predicate(predicate) == Some(true))
            .then(|| {
                Conflict::Propagator(PropagatorConflict {
                    conjunction: entry.predicates.iter().copied().collect(),
                    inference_code: entry.inference_code.clone(),
                })
            })
    }

    /// Process one nogood whose watched predicate `satisfied` just became true.
    ///
    /// Returns whether the watcher stays on that predicate's list, and whether a propagation was
    /// posted.
    fn process(
        &mut self,
        index: NogoodIndex,
        satisfied: Predicate,
        context: &mut PropagationContext,
    ) -> Result<(bool, bool), Conflict> {
        let predicate_ids = &self.predicate_ids;
        let Some(entry) = self.entries[index].as_mut() else {
            return Ok((false, false));
        };

        if !entry.active {
            return Ok((false, false));
        }

        let satisfied_slot = entry
            .watched
            .iter()
            .position(|&position| entry.predicates[position] == satisfied)
            .expect("a watcher is only on the list of a predicate the nogood watches");
        let satisfied_position = entry.watched[satisfied_slot];
        let other_position = entry.watched[1 - satisfied_slot];
        let other = entry.predicates[other_position];

        if context.evaluate_predicate(other) == Some(true) {
            // Both watches hold, so the other one's notification is still queued. Two-watched
            // literals would wait for it; a propagator scanning its whole nogood on every
            // notification acted now, and the order in which unit deductions propagate decides
            // which of them the proof keeps. So decide now, from the whole nogood.
            let mut unsatisfied = None;
            let mut second = None;

            for (position, &predicate) in entry.predicates.iter().enumerate() {
                if context.evaluate_predicate(predicate) != Some(true) {
                    if unsatisfied.is_none() {
                        unsatisfied = Some(position);
                    } else {
                        second = Some(position);
                        break;
                    }
                }
            }

            return match (unsatisfied, second) {
                (None, _) => {
                    if self.mode.detects_conflicts() {
                        Err(Conflict::Propagator(PropagatorConflict {
                            conjunction: entry.predicates.iter().copied().collect(),
                            inference_code: entry.inference_code.clone(),
                        }))
                    } else {
                        Ok((true, false))
                    }
                }

                (Some(position), None) => {
                    let predicate = entry.predicates[position];

                    // Watch the unsatisfied predicate in place of this one, so a later change to
                    // it is seen; the watcher on this list goes.
                    entry.watched[satisfied_slot] = position;
                    let new_id = predicate_ids[&predicate];
                    let inference_code = entry.inference_code.clone();
                    let explanation = entry
                        .predicates
                        .iter()
                        .copied()
                        .filter(|&candidate| candidate != predicate)
                        .collect::<PropositionalConjunction>();
                    let watcher = Watcher {
                        nogood: index,
                        blocker: other,
                    };
                    self.insert_watcher(new_id, watcher);

                    if self.mode.propagates() && context.evaluate_predicate(predicate).is_none() {
                        context.post(!predicate, (explanation, &inference_code))?;
                        Ok((false, true))
                    } else {
                        Ok((false, false))
                    }
                }

                (Some(position), Some(_)) => {
                    entry.watched[satisfied_slot] = position;
                    let new_id = predicate_ids[&entry.predicates[position]];
                    let watcher = Watcher {
                        nogood: index,
                        blocker: other,
                    };
                    self.insert_watcher(new_id, watcher);
                    Ok((false, false))
                }
            };
        }

        // Look for an unsatisfied predicate, other than the two watched ones, to move this watch
        // to. Finding one means the nogood cannot act, and the new watch will report when that
        // changes.
        let replacement = entry
            .predicates
            .iter()
            .enumerate()
            .find_map(|(position, &predicate)| {
                (position != other_position
                    && position != satisfied_position
                    && context.evaluate_predicate(predicate) != Some(true))
                .then_some(position)
            });

        if let Some(position) = replacement {
            entry.watched[satisfied_slot] = position;
            let new_id = predicate_ids[&entry.predicates[position]];
            let watcher = Watcher {
                nogood: index,
                blocker: other,
            };
            self.insert_watcher(new_id, watcher);
            return Ok((false, false));
        }

        // No replacement: everything but possibly `other` holds.
        match context.evaluate_predicate(other) {
            Some(true) if self.mode.detects_conflicts() => {
                Err(Conflict::Propagator(PropagatorConflict {
                    conjunction: entry.predicates.iter().copied().collect(),
                    inference_code: entry.inference_code.clone(),
                }))
            }

            None if self.mode.propagates() => {
                let explanation = entry
                    .predicates
                    .iter()
                    .copied()
                    .filter(|&predicate| predicate != other)
                    .collect::<PropositionalConjunction>();

                context.post(!other, (explanation, &entry.inference_code))?;
                Ok((true, true))
            }

            _ => Ok((true, false)),
        }
    }
}

impl Propagator for DeductionDatabase {
    fn name(&self) -> &str {
        "DeductionDatabase"
    }

    fn priority(&self) -> Priority {
        self.priority
    }

    fn synchronise(&mut self, _context: NotificationContext<'_>) {
        // Backtracking empties the propagator queue but not this list. A predicate recorded here
        // need no longer be true, and treating it as true would produce a propagation whose
        // reason does not hold. The pending nogoods stay: examining one re-evaluates it.
        self.updated.clear();
    }

    fn notify_predicate_id_satisfied(
        &mut self,
        _context: NotificationContext,
        predicate_id: PredicateId,
    ) -> EnqueueDecision {
        let _ = DATABASE_NOTIFICATIONS.fetch_add(1, AtomicOrdering::Relaxed);

        let watched = matches!(self.watch_lists.get(predicate_id), Some(list) if !list.is_empty());

        if watched {
            self.updated.push_back(predicate_id);
        }

        // A predicate nothing watches costs only this call, unless work is waiting: a nogood
        // added ready to act, or notifications left over from a call that stopped after
        // propagating. A trigger, and the negation the database itself posted, are exactly
        // such predicates.
        if watched || !self.pending.is_empty() || !self.updated.is_empty() {
            let _ = DATABASE_ENQUEUED.fetch_add(1, AtomicOrdering::Relaxed);
            EnqueueDecision::Enqueue
        } else {
            EnqueueDecision::Skip
        }
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        for index in std::mem::take(&mut self.pending) {
            self.examine(index, &mut context)?;
        }

        // Drop notifications that predate a backtrack: the predicate is no longer true and its
        // watchers have nothing to do.
        self.updated.retain(|&predicate_id| {
            let predicate = context.get_predicate(predicate_id);
            context.evaluate_predicate(predicate) == Some(true)
        });

        // First, conflicts, over everything notified, before any propagation. With one
        // propagator per deduction, all conflict detection ran at a higher priority than any
        // unit propagation, so a conflict was never pre-empted by a propagation that would mark
        // further deductions. A database that only detects conflicts finds the same first
        // conflict reading the lists below, since moving a watch cannot make a nogood conflict.
        if self.mode == DatabaseMode::ConflictsAndPropagation {
            for index in 0..self.updated.len() {
                let predicate_id = self.updated[index];
                self.sort_watch_list(predicate_id);
                for watcher in self.watch_lists[predicate_id].iter() {
                    if context.evaluate_predicate(watcher.blocker) == Some(false) {
                        continue;
                    }

                    if let Some(conflict) = self.conflicts(watcher.nogood, &context) {
                        return Err(conflict);
                    }
                }
            }
        }

        // Then the watch lists proper, stopping after the first propagation so propagators of
        // higher priority run before the next one, as they did with one propagator per deduction.
        while let Some(&predicate_id) = self.updated.front() {
            self.sort_watch_list(predicate_id);

            // The list is taken out while it is read, and the watchers that stay are moved
            // forward over the ones that go, keeping the order. Processing a nogood moves its
            // watch to another predicate; a watcher put on this list meanwhile is added back.
            let mut list = std::mem::take(&mut self.watch_lists[predicate_id]);
            let satisfied = context.get_predicate(predicate_id);
            let mut kept = 0;
            let mut read = 0;
            let mut outcome = Ok(false);

            while read < list.len() {
                let watcher = list[read];
                read += 1;
                let _ = DATABASE_WATCHERS_VISITED.fetch_add(1, AtomicOrdering::Relaxed);

                let active = matches!(&self.entries[watcher.nogood], Some(entry) if entry.active);

                let (stays, posted) = if !active {
                    (false, false)
                } else if context.evaluate_predicate(watcher.blocker) == Some(false) {
                    // The blocker is false, so the nogood holds whatever else happens and the
                    // watch stays where it is. An unassigned blocker excuses nothing: with every
                    // other predicate true the nogood is unit on it.
                    (true, false)
                } else {
                    match self.process(watcher.nogood, satisfied, &mut context) {
                        Ok(result) => result,
                        Err(conflict) => {
                            outcome = Err(conflict);
                            (true, false)
                        }
                    }
                };

                if stays {
                    list[kept] = watcher;
                    kept += 1;
                }

                if posted {
                    outcome = Ok(true);
                }

                if !matches!(outcome, Ok(false)) {
                    break;
                }
            }

            let unread = list.len() - read;
            list.copy_within(read.., kept);
            list.truncate(kept + unread);
            let added = std::mem::replace(&mut self.watch_lists[predicate_id], list);
            for watcher in added {
                self.insert_watcher(predicate_id, watcher);
            }

            match outcome {
                Err(conflict) => return Err(conflict),
                // The rest of this list, and of `updated`, waits for the next call. The
                // propagation just posted notifies this database, which re-enqueues it.
                Ok(true) => return Ok(()),
                Ok(false) => {
                    let _ = self.updated.pop_front();
                }
            }
        }

        Ok(())
    }

    fn propagate_from_scratch(&self, context: PropagationContext) -> PropagationStatusCP {
        // The stateless equivalent the engine's checks call: every active nogood, in full. It can
        // only report conflicts, since it cannot post through a shared reference.
        for entry in self.entries.iter().flatten() {
            if !entry.active || !self.mode.detects_conflicts() {
                continue;
            }

            let all_hold = entry
                .predicates
                .iter()
                .all(|&predicate| context.evaluate_predicate(predicate) == Some(true));

            if all_hold {
                return Err(Conflict::Propagator(PropagatorConflict {
                    conjunction: entry.predicates.iter().copied().collect(),
                    inference_code: entry.inference_code.clone(),
                }));
            }
        }

        Ok(())
    }
}
