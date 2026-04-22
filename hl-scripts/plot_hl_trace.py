#!/usr/bin/env python3
"""
Plot the relationship between backtrack level and number of linear terms
in derived hypercube linear constraints from a Pumpkin HL trace file.

Usage: python plot_hl_trace.py <trace_file> [--output <path>]
"""

import argparse
import re
import sys
from collections import defaultdict

import matplotlib.pyplot as plt
import numpy as np

# d <id> <antecedents> -> <coeff> <var> ... <= <rhs> confl@<N> bt=<N>
D_LINE_RE = re.compile(
    r'^d \d+ .*?-> (.*?) confl@(\d+) bt=(\d+)\s*$'
)


def count_linear_terms(linear_part: str) -> int:
    """Count coefficient-variable pairs before the '<=' in the linear part."""
    # linear_part is everything between '->' and 'confl@...'
    # Format: [coeff var]* <= rhs
    le_idx = linear_part.rfind('<=')
    if le_idx == -1:
        return 0
    tokens = linear_part[:le_idx].split()
    # Each term is two tokens: coefficient and variable name
    return len(tokens) // 2


def parse_trace(path: str):
    """Stream-parse the trace and yield (confl_level, bt_level, num_terms) per 'd' line."""
    with open(path, 'r') as f:
        for line in f:
            if not line.startswith('d '):
                continue
            m = D_LINE_RE.match(line)
            if not m:
                continue
            linear_part = m.group(1)
            confl = int(m.group(2))
            bt = int(m.group(3))
            n_terms = count_linear_terms(linear_part)
            yield confl, bt, n_terms


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('trace', help='Path to the .hl trace file')
    parser.add_argument('--output', '-o', default=None,
                        help='Save plot to this path instead of displaying it')
    args = parser.parse_args()

    bt_levels = []
    n_terms_list = []
    confl_levels = []

    for confl, bt, n_terms in parse_trace(args.trace):
        bt_levels.append(bt)
        n_terms_list.append(n_terms)
        confl_levels.append(confl)

    if not bt_levels:
        print("No 'd' lines found in trace.", file=sys.stderr)
        sys.exit(1)

    bt_levels = np.array(bt_levels)
    n_terms_list = np.array(n_terms_list)
    confl_levels = np.array(confl_levels)

    print(f"Parsed {len(bt_levels)} derived HLs")
    print(f"  bt range:    [{bt_levels.min()}, {bt_levels.max()}]")
    print(f"  terms range: [{n_terms_list.min()}, {n_terms_list.max()}]")

    # --- Aggregate: median and percentiles of #terms per bt level ---
    bt_unique = np.unique(bt_levels)
    medians = []
    p25 = []
    p75 = []
    means = []
    for bt in bt_unique:
        mask = bt_levels == bt
        vals = n_terms_list[mask]
        medians.append(np.median(vals))
        p25.append(np.percentile(vals, 25))
        p75.append(np.percentile(vals, 75))
        means.append(np.mean(vals))

    medians = np.array(medians)
    p25 = np.array(p25)
    p75 = np.array(p75)

    # --- Correlation ---
    corr = np.corrcoef(bt_levels, n_terms_list)[0, 1]
    print(f"  Pearson r(bt, #terms) = {corr:.3f}")

    fig, axes = plt.subplots(2, 1, figsize=(10, 10))
    fig.suptitle('HL Conflict Analysis: Backtrack Level vs. Linear Term Count', fontsize=13)

    # Panel 1: scatter plot (raw data)
    ax = axes[0]
    ax.scatter(bt_levels, n_terms_list, alpha=0.15, s=6, color='steelblue', label='derived HL')
    ax.set_xlabel('Backtrack level (bt)')
    ax.set_ylabel('Number of linear terms')
    ax.set_title(f'Raw data  (n={len(bt_levels)}, Pearson r={corr:.3f})')
    ax.legend(loc='upper left', markerscale=3)

    # Panel 2: aggregated — bar chart of median per bt level, with IQR error bars
    ax = axes[1]
    yerr_low = medians - p25
    yerr_high = p75 - medians
    ax.bar(bt_unique, medians, color='steelblue', alpha=0.7, label='median',
           yerr=[yerr_low, yerr_high], error_kw=dict(ecolor='black', capsize=3, linewidth=0.8))
    ax.set_xlabel('Backtrack level (bt)')
    ax.set_ylabel('Number of linear terms')
    ax.set_title('Median #terms per backtrack level (error bars = IQR)')
    ax.legend()

    plt.tight_layout()

    if args.output:
        plt.savefig(args.output, dpi=150)
        print(f"Plot saved to {args.output}")
    else:
        plt.show()


if __name__ == '__main__':
    main()
