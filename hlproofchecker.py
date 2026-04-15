#!/usr/bin/env python3
"""
Convert a Hypercube Linear (HL) proof file to SMT-LIB 2.0 (QF_LIA) for verification,
and optionally verify that each axiom is implied by the FlatZinc model.

The proof file contains three step types:
  v   - variable name mapping: v <solver_id> <fzn_name>
  i   - axiom
  di  - intermediate derived state (written at each loop iteration of the resolver)
  d   - final derived constraint

For each 'di' step a (check-sat) query is generated using:
  - parent 1: the most recent 'i' step seen before it
  - parent 2: the most recent 'di' step in the current group (reset by each 'd'), or none

For each 'd' step a query is generated using the immediately preceding 'di' as its parent.

  - unsat  => derivation is correct
  - sat    => a counterexample was found; the derivation may be wrong

Axiom verification (--fzn):
  For each 'i' axiom H -> R, checks that M ∧ H ∧ ¬R is UNSAT using Pumpkin
  with the default conflict resolver.

Usage:
    python3 hlproofchecker.py proof.hl [output.smt2]
    python3 hlproofchecker.py proof.hl | z3 -in
    python3 hlproofchecker.py proof.hl --fzn model.fzn --solver ./pumpkin-solver
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass


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


def _collect_vars_and_var_map(path: str) -> tuple[list[str], dict[str, str]]:
    """Single streaming pass: extract all SMT variable names and the var_map.

    The var_map maps solver variable IDs (xN) to FlatZinc variable names.
    """
    seen: set[str] = set()
    var_map: dict[str, str] = {}

    with open(path) as f:
        for raw in f:
            raw = raw.rstrip("\n")
            if not raw.strip():
                continue
            tokens = raw.split(None, 1)
            if tokens[0] == "v":
                parts = raw.split()
                if len(parts) == 3:
                    var_map[parts[1]] = parts[2]
                continue

            if "->" not in raw:
                continue

            lhs_part, rhs_part = raw.split("->", 1)

            for m in _PRED_RE.finditer(lhs_part):
                seen.add(m.group(1))

            linear_lhs = rhs_part[: rhs_part.rfind("<=")].strip() if "<=" in rhs_part else ""
            if linear_lhs:
                toks = linear_lhs.split()
                for i in range(1, len(toks), 2):
                    seen.add(toks[i])

    def sort_key(v: str):
        if v.startswith("x") and v[1:].isdigit():
            return (0, int(v[1:]))
        return (1, v)

    return sorted(seen, key=sort_key), var_map


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
# SMT-LIB generation helpers
# ---------------------------------------------------------------------------


def smt_int(v: int) -> str:
    """Format an integer as a valid SMT-LIB 2 numeral."""
    if v >= 0:
        return str(v)
    return f"(- {-v})"


def smt_predicate(pred: Predicate) -> str:
    var, val = pred.var, smt_int(pred.val)
    if pred.op == "==":
        return f"(= {var} {val})"
    if pred.op == "!=":
        return f"(not (= {var} {val}))"
    if pred.op == "<=":
        return f"(<= {var} {val})"
    if pred.op == ">=":
        return f"(>= {var} {val})"
    raise ValueError(f"Unknown predicate op: {pred.op!r}")


def smt_hypercube(predicates: list) -> str:
    """Conjunction of predicates, or 'true' for empty hypercube."""
    if not predicates:
        return "true"
    if len(predicates) == 1:
        return smt_predicate(predicates[0])
    parts = " ".join(smt_predicate(p) for p in predicates)
    return f"(and {parts})"


def smt_linear_sum(linear: LinearInequality) -> str:
    """SMT-LIB expression for the LHS sum of the linear inequality."""
    if not linear.terms:
        return "0"
    parts = []
    for coeff, var in linear.terms:
        if coeff == 1:
            parts.append(var)
        elif coeff == -1:
            parts.append(f"(- {var})")
        else:
            parts.append(f"(* {smt_int(coeff)} {var})")
    if len(parts) == 1:
        return parts[0]
    return "(+ " + " ".join(parts) + ")"


def smt_linear_holds(linear: LinearInequality) -> str:
    """SMT-LIB expression that is true iff the linear inequality holds."""
    if linear.trivially_false:
        return "false"
    if not linear.terms and linear.bound >= 0:
        return "true"
    return f"(<= {smt_linear_sum(linear)} {smt_int(linear.bound)})"


def smt_negate_linear(linear: LinearInequality) -> str:
    """SMT-LIB expression for NOT (linear holds), i.e. sum >= bound+1."""
    if linear.trivially_false:
        return "true"  # not false = true
    if not linear.terms and linear.bound >= 0:
        return "false"  # not true = false
    return f"(>= {smt_linear_sum(linear)} {smt_int(linear.bound + 1)})"


def smt_axiom_assert(step: HLStep) -> str:
    """(assert ...) string for an 'i' axiom step."""
    h = smt_hypercube(step.predicates)
    l = smt_linear_holds(step.linear)
    if h == "true":
        return f"(assert {l})"
    if l == "false":
        return f"(assert (not {h}))"
    return f"(assert (=> {h} {l}))"


# ---------------------------------------------------------------------------
# Top-level SMT generation
# ---------------------------------------------------------------------------


def _hl_str(step: HLStep) -> str:
    """Human-readable summary of a step for SMT comments."""
    h = " & ".join(f"[{p.var} {p.op} {p.val}]" for p in step.predicates)
    l = (
        f"<= {step.linear.bound}"
        if not step.linear.terms
        else " ".join(f"{c} {v}" for c, v in step.linear.terms)
        + f" <= {step.linear.bound}"
    )
    return f"{h} -> {l}"


def _emit_check(
    out, check_num: int, label: str, parents: list[HLStep], target: HLStep
) -> None:
    """Write one push/assert*/check-sat/pop block directly to `out`.

    Checks: given all `parents` hold (as HL implications), can we satisfy
    target's hypercube while violating target's linear?  UNSAT = correct.
    """
    out.write(f"; === Check {check_num}: {label} (line {target.line_num}) ===\n")
    out.write("(push 1)\n")

    for parent in parents:
        out.write(f"; Parent (line {parent.line_num}): {_hl_str(parent)}\n")
        out.write(smt_axiom_assert(parent) + "\n")

    h_target = smt_hypercube(target.predicates)
    not_l_target = smt_negate_linear(target.linear)

    out.write(f"; Negate target: {_hl_str(target)}\n")
    if h_target != "true":
        out.write(f"(assert {h_target})\n")
    if not_l_target != "true":
        out.write(f"(assert {not_l_target})\n")

    out.write(
        f'(echo "Check {check_num}: {label} line={target.line_num} -- expected unsat")\n'
    )
    out.write("(check-sat)\n")
    out.write("(pop 1)\n")
    out.write("\n")


def generate_smt_streaming(proof_path: str, out) -> tuple[int, int, dict[str, str]]:
    """Stream through the proof file and write SMT-LIB to `out`.

    Does two passes over the file: one to collect variable names (for
    declarations), one to emit checks.  Neither pass holds more than the
    current proof group in memory.

    Returns (n_di, n_d, var_map).
    """
    all_vars, var_map = _collect_vars_and_var_map(proof_path)

    out.write("; HL Proof Verification\n")
    out.write("; Each (check-sat) should return 'unsat' if the derivation is correct.\n")
    out.write("; A 'sat' result means the step is not entailed by its parents.\n")
    out.write("(set-logic QF_LIA)\n\n")

    for var in all_vars:
        out.write(f"(declare-const {var} Int)\n")
    if all_vars:
        out.write("\n")

    current_i_group: list[HLStep] = []
    current_di: HLStep | None = None
    n_di = n_d = 0
    check_num = 0

    for step in iter_steps(proof_path):
        if step.kind == "i":
            current_i_group.append(step)

        elif step.kind == "di":
            check_num += 1
            n_di += 1
            parents = current_i_group + ([current_di] if current_di is not None else [])
            _emit_check(out, check_num, "di", parents, step)
            current_di = step
            current_i_group = []

        else:  # 'd'
            check_num += 1
            n_d += 1
            parents = current_i_group + ([current_di] if current_di is not None else [])
            _emit_check(out, check_num, f"d ID={step.step_id}", parents, step)
            current_di = None
            current_i_group = []

    return n_di, n_d, var_map


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
        description="HL proof checker: SMT generation and optional axiom verification."
    )
    parser.add_argument("proof", help="Proof file (.hl)")
    parser.add_argument("output", nargs="?", help="SMT output file (default: stdout)")
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

    if args.output:
        with open(args.output, "w") as f:
            n_di, n_d, var_map = generate_smt_streaming(args.proof, f)
        print(
            f"Written {n_di + n_d} checks ({n_di} di + {n_d} d) to {args.output}",
            file=sys.stderr,
        )
    else:
        _, _, var_map = generate_smt_streaming(args.proof, sys.stdout)

    if args.fzn:
        ok = run_axiom_verification(args.proof, var_map, args.fzn, args.solver)
        if not ok:
            sys.exit(2)


if __name__ == "__main__":
    main()
