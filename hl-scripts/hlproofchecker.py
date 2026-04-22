#!/usr/bin/env python3
"""
Check a Hypercube Linear (HL) proof file using the Z3 SMT solver, and
optionally verify that each axiom is implied by the FlatZinc model.

The proof file contains these line types:
  v   - variable name mapping: v <solver_id> <fzn_name>
  i   - axiom
  di  - intermediate derived state (written at each loop iteration of the resolver)
  d   - final derived constraint

For each 'di' step a Z3 query is issued using:
  - all 'i' steps accumulated since the last 'd' (or start), plus
  - the most recent 'di' step in the current group (reset by each 'd'), if any.

For each 'd' step a query is issued using the same set of parents, then the
group is reset.

  - unsat  => derivation is correct
  - sat    => a counterexample was found; the derivation may be wrong

Axiom verification (--fzn):
  For each 'i' axiom H -> R, checks that M ∧ H ∧ ¬R is UNSAT using Pumpkin
  with the default conflict resolver.

Usage:
    python3 hlproofchecker.py proof.hl
    python3 hlproofchecker.py proof.hl --fzn model.fzn --solver ./pumpkin-solver
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass

import z3


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


@dataclass
class Predicate:
    var: str
    op: str  # '==', '!=', '<=', '>='
    val: int


@dataclass
class LinearInequality:
    terms: list  # list of (int coeff, str var)
    bound: int
    trivially_false: bool = False


@dataclass
class HLStep:
    kind: str  # 'i', 'di', or 'd'
    step_id: int | None  # integer ID for 'd' steps, None for 'i'/'di' steps
    predicates: list  # list[Predicate]
    linear: LinearInequality
    line_num: int  # 1-based line number in source file


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

_PRED_RE = re.compile(r"\[(\w+)\s*(==|!=|<=|>=)\s*(-?\d+)\]")


def parse_predicate(s: str) -> Predicate:
    m = _PRED_RE.match(s.strip())
    if not m:
        raise ValueError(f"Cannot parse predicate: {s!r}")
    return Predicate(var=m.group(1), op=m.group(2), val=int(m.group(3)))


def parse_linear(s: str) -> LinearInequality:
    """Parse the right-hand side of '->': e.g. ' 1 x1 -1 x3 <= 0' or ' <= -1'."""
    s = s.strip()
    idx = s.rfind("<=")
    if idx < 0:
        raise ValueError(f"No '<=' in linear part: {s!r}")
    lhs = s[:idx].strip()
    bound = int(s[idx + 2 :].strip())

    terms = []
    if lhs:
        tokens = lhs.split()
        if len(tokens) % 2 != 0:
            raise ValueError(f"Odd number of tokens in linear LHS: {lhs!r}")
        for i in range(0, len(tokens), 2):
            coeff = int(tokens[i])
            var = tokens[i + 1]
            terms.append((coeff, var))

    trivially_false = not terms and bound < 0
    return LinearInequality(terms=terms, bound=bound, trivially_false=trivially_false)


def parse_line(line_num: int, raw: str) -> HLStep:
    """Parse one line of the proof file into an HLStep."""
    raw = raw.strip()
    raw = re.sub(r'\s+confl@\d+\s+bt=\d+\s*$', '', raw)
    if not raw:
        raise ValueError("Empty line")

    parts = raw.split("->", 1)
    if len(parts) != 2:
        raise ValueError(f"Line {line_num}: missing '->' in: {raw!r}")

    prefix = parts[0].strip()
    linear = parse_linear(parts[1])

    if prefix == "i" or prefix.startswith("i "):
        kind = "i"
        step_id = None
        pred_str = prefix[1:].strip()
    elif prefix == "di" or prefix.startswith("di "):
        kind = "di"
        step_id = None
        pred_str = prefix[2:].strip()
    elif prefix.startswith("d "):
        kind = "d"
        rest = prefix[2:].strip()
        id_and_preds = rest.split(None, 1)
        step_id = int(id_and_preds[0])
        pred_str = id_and_preds[1].strip() if len(id_and_preds) > 1 else ""
    else:
        raise ValueError(f"Line {line_num}: unknown prefix in: {raw!r}")

    predicates = []
    if pred_str:
        for chunk in re.split(r"\s*&\s*", pred_str):
            chunk = chunk.strip()
            if chunk:
                predicates.append(parse_predicate(chunk))

    return HLStep(
        kind=kind,
        step_id=step_id,
        predicates=predicates,
        linear=linear,
        line_num=line_num,
    )


def _collect_var_map(path: str) -> dict[str, str]:
    """Streaming pass to extract the var_map (solver ID → FlatZinc name)."""
    var_map: dict[str, str] = {}
    with open(path) as f:
        for raw in f:
            parts = raw.split()
            if parts and parts[0] == "v" and len(parts) == 3:
                var_map[parts[1]] = parts[2]
    return var_map


def iter_steps(path: str):
    """Generator that yields HLStep objects, skipping 'v' lines."""
    with open(path) as f:
        for line_num, raw in enumerate(f, 1):
            raw = raw.rstrip("\n")
            if not raw.strip():
                continue
            tokens = raw.split(None, 1)
            if tokens[0] == "v":
                continue
            yield parse_line(line_num, raw)


# ---------------------------------------------------------------------------
# Z3 helpers
# ---------------------------------------------------------------------------


def _z3_var(name: str, cache: dict) -> z3.ArithRef:
    """Return (or create and cache) the Z3 Int variable for `name`."""
    if name not in cache:
        cache[name] = z3.Int(name)
    return cache[name]


def _z3_predicate(pred: Predicate, cache: dict) -> z3.BoolRef:
    v = _z3_var(pred.var, cache)
    if pred.op == "==":
        return v == pred.val
    if pred.op == "!=":
        return v != pred.val
    if pred.op == "<=":
        return v <= pred.val
    if pred.op == ">=":
        return v >= pred.val
    raise ValueError(f"Unknown predicate op: {pred.op!r}")


def _z3_linear_holds(linear: LinearInequality, cache: dict) -> z3.BoolRef:
    if linear.trivially_false:
        return z3.BoolVal(False)
    if not linear.terms and linear.bound >= 0:
        return z3.BoolVal(True)
    lhs = z3.Sum([c * _z3_var(v, cache) for c, v in linear.terms])
    return lhs <= linear.bound


def _z3_axiom(step: HLStep, cache: dict) -> z3.BoolRef:
    """Return the formula H ⇒ L for `step`."""
    l = _z3_linear_holds(step.linear, cache)
    if not step.predicates:
        return l
    h = z3.And([_z3_predicate(p, cache) for p in step.predicates])
    return z3.Implies(h, l)


# ---------------------------------------------------------------------------
# Human-readable step label (for progress / error messages)
# ---------------------------------------------------------------------------


def _hl_str(step: HLStep) -> str:
    h = " & ".join(f"[{p.var} {p.op} {p.val}]" for p in step.predicates)
    l = (
        f"<= {step.linear.bound}"
        if not step.linear.terms
        else " ".join(f"{c} {v}" for c, v in step.linear.terms)
        + f" <= {step.linear.bound}"
    )
    return f"{h} -> {l}"


def _step_label(step: HLStep) -> str:
    if step.kind == "di":
        return f"di (line {step.line_num})"
    return f"d ID={step.step_id} (line {step.line_num})"


# ---------------------------------------------------------------------------
# Z3 proof checking — streaming, single pass
# ---------------------------------------------------------------------------

_PROGRESS_WIDTH = 60  # dots per line before wrapping


def run_z3_checks(proof_path: str) -> bool:
    """Stream through the proof file and check each derivation step with Z3.

    Variables are declared lazily as they are first encountered.
    Only the current proof group is held in memory at any time.

    Returns True if all checks pass, False if any check fails.
    """
    solver = z3.Solver()
    vars_cache: dict[str, z3.ArithRef] = {}

    current_i_group: list[HLStep] = []
    current_di: HLStep | None = None
    check_num = n_fail = 0
    col = 0  # current column in the progress line

    def _progress_ok() -> None:
        nonlocal col
        sys.stderr.write(".")
        col += 1
        if col >= _PROGRESS_WIDTH:
            sys.stderr.write(f" {check_num}\n")
            col = 0
        sys.stderr.flush()

    def _progress_fail(label: str) -> None:
        nonlocal col
        if col > 0:
            sys.stderr.write("\n")
            col = 0
        sys.stderr.write(f"FAIL: {label}\n")
        sys.stderr.flush()

    def _check(parents: list[HLStep], target: HLStep) -> bool:
        solver.push()
        for parent in parents:
            solver.add(_z3_axiom(parent, vars_cache))
        for pred in target.predicates:
            solver.add(_z3_predicate(pred, vars_cache))
        solver.add(z3.Not(_z3_linear_holds(target.linear, vars_cache)))
        result = solver.check()
        solver.pop()
        return result == z3.unsat

    for step in iter_steps(proof_path):
        if step.kind == "i":
            current_i_group.append(step)

        elif step.kind == "di":
            check_num += 1
            parents = current_i_group + ([current_di] if current_di is not None else [])
            ok = _check(parents, step)
            if ok:
                _progress_ok()
            else:
                n_fail += 1
                _progress_fail(_step_label(step))
            current_di = step
            current_i_group = []

        else:  # 'd'
            check_num += 1
            parents = current_i_group + ([current_di] if current_di is not None else [])
            ok = _check(parents, step)
            if ok:
                _progress_ok()
            else:
                n_fail += 1
                _progress_fail(_step_label(step))
            current_di = None
            current_i_group = []

    # Finish the progress line if it has partial content.
    if col > 0:
        sys.stderr.write(f" {check_num}\n")

    n_ok = check_num - n_fail
    sys.stderr.write(
        f"Derivation checks: {n_ok} ok, {n_fail} failed"
        f" ({check_num} total)\n"
    )
    return n_fail == 0


# ---------------------------------------------------------------------------
# FlatZinc axiom verification
# ---------------------------------------------------------------------------


def _fzn_var(xN: str, var_map: dict) -> str:
    """Translate a solver variable ID to its FlatZinc name, falling back to xN."""
    return var_map.get(xN, xN)


def _fzn_predicate(pred: Predicate, var_map: dict) -> str:
    """FlatZinc constraint line asserting a single predicate."""
    fname = _fzn_var(pred.var, var_map)
    if pred.op == "<=":
        return f"constraint int_le({fname}, {pred.val});"
    if pred.op == ">=":
        return f"constraint int_le({pred.val}, {fname});"
    if pred.op == "==":
        return f"constraint int_eq({fname}, {pred.val});"
    if pred.op == "!=":
        return f"constraint int_ne({fname}, {pred.val});"
    raise ValueError(f"Unknown predicate op: {pred.op!r}")


def _fzn_negated_linear(linear: LinearInequality, var_map: dict) -> str | None:
    """FlatZinc constraint for ¬(linear holds).

    Returns None  if the negation is trivially false (nothing to assert;
                  the check is vacuously UNSAT — skip it).
    Returns ""    if the negation is trivially true (no constraint needed;
                  only the hypercube conditions are checked).
    Returns a constraint string otherwise.
    """
    if linear.trivially_false:
        # linear is always false; ¬(always false) = true → no constraint needed
        return ""
    if not linear.terms and linear.bound >= 0:
        # linear is always true; ¬(always true) = false → trivially UNSAT, skip
        return None
    # Negate sum(ci*xi) <= bound  as  sum(-ci*xi) <= -(bound+1)
    neg_coeffs = [-c for c, _ in linear.terms]
    fnames = [_fzn_var(v, var_map) for _, v in linear.terms]
    coeffs_str = "[" + ", ".join(str(c) for c in neg_coeffs) + "]"
    vars_str = "[" + ", ".join(fnames) + "]"
    rhs = -(linear.bound + 1)
    return f"constraint int_lin_le({coeffs_str}, {vars_str}, {rhs});"


def verify_axiom(
    step: HLStep,
    fzn_content: str,
    var_map: dict,
    solver_bin: str,
    timeout: int = 30,
) -> bool:
    """Verify that M ∧ H ∧ ¬R is UNSAT using Pumpkin.

    Returns True  if UNSAT (axiom verified).
    Returns False if SAT or unknown (axiom not verified).
    """
    neg_lin = _fzn_negated_linear(step.linear, var_map)
    if neg_lin is None:
        # Negation is false; M ∧ H ∧ false is trivially UNSAT.
        return True

    extra = [_fzn_predicate(pred, var_map) for pred in step.predicates]
    if neg_lin:
        extra.append(neg_lin)

    # Insert new constraints before the solve statement.
    solve_idx = fzn_content.rfind("\nsolve ")
    if solve_idx == -1:
        augmented = fzn_content + "\n" + "\n".join(extra) + "\n"
    else:
        augmented = (
            fzn_content[: solve_idx + 1]
            + "\n".join(extra)
            + "\n"
            + fzn_content[solve_idx + 1 :]
        )

    with tempfile.NamedTemporaryFile(suffix=".fzn", mode="w", delete=False) as tmp:
        tmp.write(augmented)
        tmp_path = tmp.name

    try:
        result = subprocess.run(
            [solver_bin, tmp_path],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        return "=====UNSATISFIABLE=====" in result.stdout
    except subprocess.TimeoutExpired:
        print(
            f"  TIMEOUT: axiom at line {step.line_num}: {_hl_str(step)}",
            file=sys.stderr,
        )
        return False
    finally:
        os.unlink(tmp_path)


def run_axiom_verification(
    proof_path: str,
    var_map: dict,
    fzn_path: str,
    solver_bin: str,
) -> bool:
    """Verify all 'i' axioms against the FlatZinc model.

    Streams through the proof file; only one step is held in memory at a time.
    Returns True if all axioms are verified, False otherwise.
    """
    with open(fzn_path) as f:
        fzn_content = f.read()

    n_ok = n_fail = n_skip = 0

    for step in iter_steps(proof_path):
        if step.kind != "i":
            continue

        neg_lin = _fzn_negated_linear(step.linear, var_map)
        if neg_lin is None:
            # Trivially UNSAT — the negation is false, no solver call needed.
            n_skip += 1
            continue

        ok = verify_axiom(step, fzn_content, var_map, solver_bin)
        if ok:
            n_ok += 1
            print(f"OK: axiom at line {step.line_num}")
        else:
            n_fail += 1
            print(
                f"FAIL: axiom at line {step.line_num}: {_hl_str(step)}",
                file=sys.stderr,
            )

    print(
        f"Axiom verification: {n_ok} ok, {n_fail} failed, {n_skip} skipped",
        file=sys.stderr,
    )
    return n_fail == 0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main():
    parser = argparse.ArgumentParser(
        description="HL proof checker: Z3 derivation checks and optional axiom verification."
    )
    parser.add_argument("proof", help="Proof file (.hl)")
    parser.add_argument(
        "--fzn",
        metavar="FILE",
        help="FlatZinc model to verify axioms against (enables axiom verification)",
    )
    parser.add_argument(
        "--solver",
        default="pumpkin-solver",
        metavar="BIN",
        help="Pumpkin solver binary used for axiom verification (default: pumpkin-solver)",
    )
    args = parser.parse_args()

    ok = run_z3_checks(args.proof)
    if not ok:
        sys.exit(1)

    if args.fzn:
        var_map = _collect_var_map(args.proof)
        ok = run_axiom_verification(args.proof, var_map, args.fzn, args.solver)
        if not ok:
            sys.exit(2)


if __name__ == "__main__":
    main()
