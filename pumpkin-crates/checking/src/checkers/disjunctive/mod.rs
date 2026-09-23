mod helpers;
mod inference;
#[cfg(test)]
mod tests;

/// The checker of edge-finding for the disjunctive constraint.
#[derive(Clone, Debug)]
pub struct DisjunctiveEdgeFindingChecker<Var> {
    pub tasks: Box<[DisjunctiveCheckerTask<Var>]>,
}

/// A task of the disjunctive constraint, as the checker sees it.
#[derive(Clone, Debug)]
pub struct DisjunctiveCheckerTask<Var> {
    pub start_time: Var,
    pub processing_time: i32,
}
