import glob
import pathlib
from tqdm import tqdm
import os

def read_file(instance):
    lines = instance.readlines()
    index = 0

    line = lines[0].split()
    # There are 2 sentinel tasks
    n_tasks = int(line[0]) + 2
    n_resources = int(line[1])
    
    index += 1

    precedences = []
    processing_times = []
    resource_usages = [[] for _ in range(n_resources)]
    resource_capacities = []

    # Read the precedences
    for task_index in range(n_tasks):
        line = lines[index].split()
        assert task_index == int(line[0])

        num_successors = int(line[2])

        for successor_index in range(num_successors):
            successor = int(line[3 + successor_index])
            weight = int(line[3 + successor_index + num_successors][1:-1])
            precedences.append((task_index + 1, weight, successor + 1))
        index += 1
    
    # Read the processing times and resource usages
    for task_index in range(n_tasks):
        line = lines[index].split()
        assert task_index == int(line[0])

        processing_times.append(int(line[2]))
        for resource_index in range(n_resources):
            resource_usages[resource_index].append(line[3 + resource_index])

        index += 1
    
    for resource_capacity in lines[index].split():
        resource_capacities.append(int(resource_capacity))

    assert len(processing_times) == n_tasks
    assert len(resource_usages) == n_resources
    for resource_usage in resource_usages:
        assert len(resource_usage) == n_tasks
    assert len(resource_capacities) == n_resources

    return n_tasks, n_resources, precedences, resource_usages, resource_capacities, processing_times

def convert_to_dzn(n_tasks, n_resources, precedences, resource_usages, resource_capacities, processing_times):
    dzn = []
    dzn.append(f"n_res = {n_resources};\n")
    dzn.append(f"rcap = {resource_capacities};\n")
    dzn.append(f"n_tasks = {n_tasks};\n")
    dzn.append(f"dur = {processing_times};\n")
    
    for (index, resource_usage) in enumerate(resource_usages):
        if index == 0:
            dzn.append(f"rr = [| ")
        else:
            dzn.append("\n\t| ")
        dzn.append(", ".join(resource_usage))

        if index == len(resource_usages) - 1:
            dzn.append(" |];\n")
    
    for (index, precedence) in enumerate(precedences):
        if index == 0:
            dzn.append("dcons = [| ")
        else:
            dzn.append("\n\t| ")
        dzn.append(f"{precedence[0]}, {precedence[1]}, {precedence[2]}")

        if index == len(precedences) - 1:
            dzn.append(" |];\n")
    dzn.append(f"n_dc = {len(precedences)};")

    return "".join(dzn)

files = list(glob.glob("**/*.sch"))
files.extend(list(glob.glob("**/*.SCH", recursive = True)))
directories = set()

for file in tqdm(files):
    with open(file, "r") as instance:
        n_tasks, n_resources, precedences, resource_usages, resource_capacities, processing_times = read_file(instance)
        dzn = convert_to_dzn(n_tasks, n_resources, precedences, resource_usages, resource_capacities, processing_times)
    path = pathlib.Path(file)

    converted = "/".join(list(map(lambda x: f"{x}_converted", path.parts[:-1])))
    directories.add(converted)
    if not os.path.exists(converted):
        os.makedirs(converted)
    with open(f"{converted}/{path.stem}.dzn", "w+") as dzn_file:
        dzn_file.write(dzn)
print(directories)
