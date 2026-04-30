---
name: hl-info
description: The mathematics of hypercube linear resolution.
---

# Hypercube Linear Resolution — Implementation Reference

> Summary of *Resolution Meets Cutting Planes: Introducing Hypercube Linear Resolution* (Flippo, Stuckey, Demirović — CPAIOR '26).
> Use as context when implementing the proof system, propagator, or conflict analysis.

## 1. Core Idea

A **hypercube linear constraint** is a unification of a clause and an integer linear inequality:

```
H → Σ wᵢ·xᵢ ≤ r
```

where `H` is a *hypercube* (a consistent conjunction of atomic constraints).

- If `wᵢ = 0` for all `i` and `r ≤ -1` → degenerates to a **clause** (`H → ⊥`).
- If `D ⊨ H` (hypercube entailed by the current domain) → behaves as the plain linear inequality `Σ wᵢxᵢ ≤ r`.
- Generalises **indicator constraints** (single 0-1 bound in `H`).
- Variables may appear in *both* `H` and the linear part — no separation required.

The proof system, **hypercube linear resolution (HLR)**, is **sound and complete** for integer linear reasoning. It can be viewed as an extended cutting-planes system where extended variables are bound constraints.

---

## 2. Preliminaries / Data Model

### Hypercube `H`
- A **consistent** conjunction of atomic constraints (e.g. no `⟨x ≥ l⟩ ∧ ⟨x ≤ u⟩` with `l > u`).
- May implicitly include the variable's full domain bounds without changing meaning.
- `D_H` denotes the domain induced by `H`.

### Linear constraint `Σ wᵢxᵢ ≤ r` slack
```
slack(D, R) = r − Σ lb(D, wᵢxᵢ)
lb(D, wᵢxᵢ) = wᵢ · lb(D, xᵢ)   if wᵢ ≥ 0
            = wᵢ · ub(D, xᵢ)   if wᵢ < 0
```

---

## 3. Hypercube Linear Slack

**Key definition** (Definition 2):
```
hlslack(D, L) = r − Σ max( lb(D, wᵢxᵢ), lb(D_H, wᵢxᵢ) )
```

Takes the *tighter* of the current domain bound or the hypercube bound for each variable contribution. Rationale: once `H` becomes entailed, `D_H`'s bounds will hold, so we can already use them when reasoning about the linear component.

- If `D ⊨ H` then `hlslack(D, H → R) = slack(D, R)`.
- Conflict detection: `L` is conflicting w.r.t. `D` iff `D ⊨ H` AND `slack(D, R) < 0`.

---

## 4. Hypercube Linear Propagator

For `L ≡ H → R` under domain `D`, three propagation rules:

### Rule 1 — Hypercube fully entailed
If `D ⊨ H`: propagate `R` as a standard linear constraint.

### Rule 2 — Negative slack, one hypercube bound unassigned
If `hlslack(D, L) < 0` and exactly one bound `c ∈ H` is unsatisfied, propagate `¬c`:
- `c = ⟨x ≤ d⟩` → propagate `x ≥ d + 1`
- `c = ⟨x ≥ d⟩` → propagate `x ≤ d − 1`

### Rule 3 — Non-negative slack, one hypercube bound unassigned
If `hlslack(D, L) ≥ 0` and exactly one `c ∈ H` over variable `x` (with linear coefficient `w`) is unassigned, you can still propagate a *weaker* bound:
- `c ≡ ⟨x ≥ d⟩`, `w > 0` → `x ≤ ⌊(hlslack(D, L) + w·d) / w⌋`
- `c ≡ ⟨x ≤ d⟩`, `w < 0` → `x ≥ ⌈(hlslack(D, L) + w·d) / w⌉`

**Reason**: in the case `c` is true, `R` enforces a tighter bound; in the case `¬c`, the bound is implied directly. The weaker bound is true in either case.

### Explanations
Every propagated bound `c` carries an explanation `B` — the conjunction of bounds that caused it. Same concept as in LCG solvers.

---

## 5. Inference Rules of HLR

Four rules: `Weaken`, `Divide`, `ResF` (Fourier), `ResH` (propositional).

### 5.1 Weakening (Definition 3)
Move part of a coefficient from the linear term into the hypercube.

```
H → Σ wᵢxᵢ ≤ r
─────────────────────────────────────────────  (lower bound)
H ∧ ⟨xⱼ ≥ d⟩ → Σᵢ≠ⱼ wᵢxᵢ + (wⱼ−1)xⱼ ≤ r − d

H → Σ wᵢxᵢ ≤ r
─────────────────────────────────────────────  (upper bound)
H ∧ ⟨xⱼ ≤ d⟩ → Σᵢ≠ⱼ wᵢxᵢ + (wⱼ+1)xⱼ ≤ r + d
```

**Full elimination of `xⱼ`** = apply weakening `|wⱼ|` times, ending at:
- `wⱼ > 0`: `H ∧ ⟨xⱼ ≥ d⟩ → Σᵢ≠ⱼ wᵢxᵢ ≤ r − wⱼd`
- `wⱼ < 0`: `H ∧ ⟨xⱼ ≤ d⟩ → Σᵢ≠ⱼ wᵢxᵢ ≤ r + wⱼd`

**Slack-preserving property (Proposition 3)**: weakening on the *current* domain bound `⟨x ≥ lb(D,x)⟩` (or `⟨x ≤ ub(D,x)⟩`) does **not change `hlslack(D, L)`**. This is critical for keeping the conflict alive during analysis.

### 5.2 Division
```
H → Σ wᵢxᵢ ≤ r        (d > 0)
────────────────────────────────
H → Σ ⌊wᵢ/d⌋ xᵢ ≤ ⌊r/d⌋
```
Sound (standard rounded division on integer inequalities). Not required for completeness but prevents coefficient overflow.

### 5.3 Fourier Resolution `ResF`
For `L₁ ≡ H₁ → R₁`, `L₂ ≡ H₂ → R₂` where `xⱼ` has positive weight `a` in `R₁` and negative weight `b` in `R₂` (so `a·b < 0`), and `H₁ ∧ H₂` is consistent:

```
H₁ → R₁     H₂ → R₂
──────────────────────────────
H₁ ∧ H₂ → FR(R₁, R₂, xⱼ)
```

where `FR(R₁, R₂, xⱼ) = Σᵢ≠ⱼ (−w'ⱼ·wᵢ + wⱼ·w'ᵢ) xᵢ ≤ −w'ⱼ·r + wⱼ·r'`, eliminating `xⱼ`.

### 5.4 Propositional-style Resolution `ResH`
For two hypercube linears with bounds `c₁ ≡ ⟨x ≥ d₁⟩ ∈ H₁` and `c₂ ≡ ⟨x ≤ d₂⟩ ∈ H₂` such that `d₁ ≤ d₂ + 1` (covers all values of `x`), and `H'₁ ∧ H'₂` consistent (`Hₖ ≡ H'ₖ ∧ cₖ`):

The rule is parameterised as `ResH_F`. The default instantiation requires `L₂` to be reduced to a clause first (linear part `⊥`):
```
H'₁ ∧ c₁ → R₁     H'₂ ∧ c₂ → ⊥
─────────────────────────────────
H'₁ ∧ H'₂ → R₁
```
Soundness relies on: at least one of `c₁`, `c₂` is true for every value of `x`; weakening `L₂` to a clause is always possible.


