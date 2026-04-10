#!/usr/bin/env python3
"""
Convert a Hypercube Linear (HL) proof file to SMT-LIB 2.0 (QF_LIA) for verification.

The proof file contains three step types:
  i   - axiom
  di  - intermediate derived state (written at each loop iteration of the resolver)
  d   - final derived constraint

For each 'di' step a (check-sat) query is generated using:
  - parent 1: the most recent 'i' step seen before it
  - parent 2: the most recent 'di' step in the current group (reset by each 'd'), or none

For each 'd' step a query is generated using the immediately preceding 'di' as its parent.

  - unsat  => derivation is correct
  - sat    => a counterexample was found; the derivation may be wrong

Usage:
    python3 hl2smt.py proof.hl [output.smt2]
    python3 hl2smt.py proof.hl | z3 -in
"""

import re
import sys
from dataclasses import dataclass, field


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class Predicate:
    var: str
    op: str   # '==', '!=', '<=', '>='
    val: int


@dataclass
class LinearInequality:
    terms: list          # list of (int coeff, str var)
    bound: int
    trivially_false: bool = False


@dataclass
class HLStep:
    kind: str            # 'i', 'di', or 'd'
    step_id: int | None  # integer ID for 'd' steps, None for 'i'/'di' steps
    predicates: list     # list[Predicate]
    linear: LinearInequality
    line_num: int        # 1-based line number in source file


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

_PRED_RE = re.compile(r'\[(\w+)\s*(==|!=|<=|>=)\s*(-?\d+)\]')


def parse_predicate(s: str) -> Predicate:
    m = _PRED_RE.match(s.strip())
    if not m:
        raise ValueError(f"Cannot parse predicate: {s!r}")
    return Predicate(var=m.group(1), op=m.group(2), val=int(m.group(3)))


def parse_linear(s: str) -> LinearInequality:
    """Parse the right-hand side of '->': e.g. ' 1 x1 -1 x3 <= 0' or ' <= -1'."""
    s = s.strip()
    idx = s.rfind('<=')
    if idx < 0:
        raise ValueError(f"No '<=' in linear part: {s!r}")
    lhs = s[:idx].strip()
    bound = int(s[idx + 2:].strip())

    terms = []
    if lhs:
        tokens = lhs.split()
        if len(tokens) % 2 != 0:
            raise ValueError(f"Odd number of tokens in linear LHS: {lhs!r}")
        for i in range(0, len(tokens), 2):
            coeff = int(tokens[i])
            var = tokens[i + 1]
            terms.append((coeff, var))

    trivially_false = (not terms and bound < 0)
    return LinearInequality(terms=terms, bound=bound, trivially_false=trivially_false)


def parse_line(line_num: int, raw: str) -> HLStep:
    """Parse one line of the proof file into an HLStep."""
    raw = raw.strip()
    if not raw:
        raise ValueError("Empty line")

    parts = raw.split('->', 1)
    if len(parts) != 2:
        raise ValueError(f"Line {line_num}: missing '->' in: {raw!r}")

    prefix = parts[0].strip()
    linear = parse_linear(parts[1])

    if prefix == 'i' or prefix.startswith('i '):
        kind = 'i'
        step_id = None
        pred_str = prefix[1:].strip()
    elif prefix == 'di' or prefix.startswith('di '):
        kind = 'di'
        step_id = None
        pred_str = prefix[2:].strip()
    elif prefix.startswith('d '):
        kind = 'd'
        rest = prefix[2:].strip()
        id_and_preds = rest.split(None, 1)
        step_id = int(id_and_preds[0])
        pred_str = id_and_preds[1].strip() if len(id_and_preds) > 1 else ''
    else:
        raise ValueError(f"Line {line_num}: unknown prefix in: {raw!r}")

    predicates = []
    if pred_str:
        for chunk in re.split(r'\s*&\s*', pred_str):
            chunk = chunk.strip()
            if chunk:
                predicates.append(parse_predicate(chunk))

    return HLStep(kind=kind, step_id=step_id, predicates=predicates,
                  linear=linear, line_num=line_num)


def read_proof(path: str) -> list[HLStep]:
    steps = []
    with open(path) as f:
        for line_num, raw in enumerate(f, 1):
            raw = raw.rstrip('\n')
            if not raw.strip():
                continue
            steps.append(parse_line(line_num, raw))
    return steps


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
    if pred.op == '==':
        return f"(= {var} {val})"
    if pred.op == '!=':
        return f"(not (= {var} {val}))"
    if pred.op == '<=':
        return f"(<= {var} {val})"
    if pred.op == '>=':
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
        return "true"   # not false = true
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


def collect_vars(steps: list[HLStep]) -> list[str]:
    """Return sorted list of all variable names appearing in the proof."""
    seen = set()
    for step in steps:
        for pred in step.predicates:
            seen.add(pred.var)
        for _coeff, var in step.linear.terms:
            seen.add(var)

    def sort_key(v: str):
        # Sort xN variables numerically, anything else lexicographically
        if v.startswith('x') and v[1:].isdigit():
            return (0, int(v[1:]))
        return (1, v)

    return sorted(seen, key=sort_key)


# ---------------------------------------------------------------------------
# Top-level generation
# ---------------------------------------------------------------------------

def _hl_str(step: HLStep) -> str:
    """Human-readable summary of a step for SMT comments."""
    h = " & ".join(f"[{p.var} {p.op} {p.val}]" for p in step.predicates)
    l = (f"<= {step.linear.bound}" if not step.linear.terms
         else " ".join(f"{c} {v}" for c, v in step.linear.terms)
              + f" <= {step.linear.bound}")
    return f"{h} -> {l}"


def _emit_check(out: list, check_num: int, label: str,
                parents: list[HLStep], target: HLStep) -> None:
    """Emit one push/assert*/check-sat/pop block.

    Checks: given all `parents` hold (as HL implications), can we satisfy
    target's hypercube while violating target's linear?  UNSAT = correct.
    """
    out.append(f"; === Check {check_num}: {label} (line {target.line_num}) ===")
    out.append("(push 1)")

    for parent in parents:
        out.append(f"; Parent (line {parent.line_num}): {_hl_str(parent)}")
        out.append(smt_axiom_assert(parent))

    h_target = smt_hypercube(target.predicates)
    not_l_target = smt_negate_linear(target.linear)

    out.append(f"; Negate target: {_hl_str(target)}")
    if h_target != "true":
        out.append(f"(assert {h_target})")
    if not_l_target != "true":
        out.append(f"(assert {not_l_target})")

    out.append(f'(echo "Check {check_num}: {label} line={target.line_num} -- expected unsat")')
    out.append("(check-sat)")
    out.append("(pop 1)")
    out.append("")


def generate_smt(steps: list[HLStep]) -> str:
    out = []

    out.append("; HL Proof Verification")
    out.append("; Each (check-sat) should return 'unsat' if the derivation is correct.")
    out.append("; A 'sat' result means the step is not entailed by its parents.")
    out.append("(set-logic QF_LIA)")
    out.append("")

    all_vars = collect_vars(steps)
    if all_vars:
        for var in all_vars:
            out.append(f"(declare-const {var} Int)")
        out.append("")

    current_i_group: list[HLStep] = []  # 'i' steps since the last 'di' or 'd'
    current_di: HLStep | None = None    # most recent 'di' in this group (reset by 'd')
    check_num = 0

    for step in steps:
        if step.kind == 'i':
            current_i_group.append(step)

        elif step.kind == 'di':
            check_num += 1
            parents = current_i_group + ([current_di] if current_di is not None else [])
            _emit_check(out, check_num, "di", parents, step)
            current_di = step
            current_i_group = []

        else:  # 'd'
            check_num += 1
            parents = current_i_group + ([current_di] if current_di is not None else [])
            _emit_check(out, check_num, f"d ID={step.step_id}", parents, step)
            current_di = None
            current_i_group = []

    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} proof.hl [output.smt2]", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else None

    steps = read_proof(input_path)
    smt = generate_smt(steps)

    if output_path:
        with open(output_path, 'w') as f:
            f.write(smt)
        n_di = sum(1 for s in steps if s.kind == 'di')
        n_d = sum(1 for s in steps if s.kind == 'd')
        print(f"Written {n_di + n_d} checks ({n_di} di + {n_d} d) to {output_path}",
              file=sys.stderr)
    else:
        sys.stdout.write(smt)


if __name__ == '__main__':
    main()
