#[cfg(all(feature = "check-inferences", feature = "check-inferences-proof"))]
compile_error!(
    "the features check-inferences and check-inferences-proof are two modes of inference \
     checking; enable one of them"
);

use std::sync::Once;

use log::warn;

/// The runtime checks compiled into the solver, each with the feature that enables it.
pub fn active_runtime_checks() -> Vec<&'static str> {
    let mut checks = vec![];

    if cfg!(feature = "check-inferences") {
        checks.push("inference checks (check-inferences)");
    }
    if cfg!(feature = "check-inferences-proof") {
        checks.push(
            "inference checks of the inferences used by conflict analysis (check-inferences-proof)",
        );
    }
    if cfg!(feature = "check-retention-all") {
        checks.push("retention checks of every checker at each fixpoint (check-retention-all)");
    } else if cfg!(feature = "check-retention") {
        checks.push("retention checks (check-retention)");
    }
    if cfg!(feature = "check-solutions") {
        checks.push("solution checks (check-solutions)");
    }
    if cfg!(feature = "check-deductions") {
        checks.push("deduction checks (check-deductions)");
    }

    checks
}

/// Warn, once, that runtime checks are active and which, when solving starts.
pub(crate) fn report_runtime_checks() {
    static REPORTED: Once = Once::new();

    REPORTED.call_once(|| {
        let checks = active_runtime_checks();
        if checks.is_empty() {
            return;
        }

        warn!(
            "RUNTIME CHECKS ARE ACTIVE, WHICH MAY SLOW DOWN THE SOLVER: {}",
            checks.join(", ")
        );
    });
}
