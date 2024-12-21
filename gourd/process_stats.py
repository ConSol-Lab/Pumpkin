import argparse
import json

parser = argparse.ArgumentParser()
parser.add_argument("path")
args = parser.parse_args()
with open(args.path) as log:
    accs = list()
    acc = list()
    for line in log:
        if set(line.strip()) == {"-"}:
            accs.append(acc)
            acc = list()
            continue
        if "%%%mzn-stat: " in line:
            acc.append(line.strip("\n"))
    dump = list()
    if len(acc) > 0:
        accs.append(acc)
    for acc in accs:
        n_calls = None
        n_conflicts = None
        avg_clique = None
        n_decisions = None
        n_solver_conflicts = None
        n_props = None
        solver_time = None
        objective = None
        for line in acc:
            if "objective" in line:
                _, objective = line.split("=")
                objective = int(objective)
            if "nodePackingPropagator" in line:
                _, val = line.split("=")
                if "NCalls" in line:
                    n_calls = int(val)
                if "NConflicts" in line:
                    n_conflicts = int(val)
                if "AverageCliqueSize" in line:
                    avg_clique = float(val)
            if "engine" in line:
                _, val = line.split("=")
                if "NumDecisions" in line:
                    n_decisions = int(val)
                if "NumConflicts" in line:
                    n_solver_conflicts = int(val)
                if "NumPropagations" in line:
                    n_props = int(val)
                if "TimeSpentInSolver" in line:
                    solver_time = int(val)
        if objective is None:
            continue
        dump.append(
            {
                "objective": objective,
                "calls": n_calls,
                "conflicts": n_conflicts,
                "avg_clique": avg_clique,
                "solver_decisions": n_decisions,
                "solver_conflicts": n_solver_conflicts,
                "solver_propagations": n_props,
                "solver_time": solver_time,
            }
        )
    print("'" + json.dumps(dump) + "'")
