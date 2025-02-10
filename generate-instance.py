import argparse


def main(group_size, cap, low, high, add_long_task):
    if group_size < 0 or cap < 0 or low < 0 or high < 0:
        print("All parameters must be positive")
        return
    if low + high <= cap:
        print("Low and high usage tasks should exceed the capacity")
        return
    if low + low > cap:
        print("Two low tasks may not exceed the capacity")
        return
    if high + high <= cap:
        print("Two high tasks should exceed the capacity")
        return
    long_task_cap = cap + 1
    final_cap = cap + long_task_cap if add_long_task else cap
    print("n_res = 3;")
    print("rc = [ {} ];".format(", ".join(str(final_cap) for _ in range(3))))
    n_tasks = 3 * group_size + (1 if add_long_task else 0)
    target_makespan = 3 * group_size
    print(f"n_tasks = {n_tasks};")
    durations = [
        target_makespan - 1 if ix == n_tasks - 1 and add_long_task else 1
        for ix in range(n_tasks)
    ]
    print("d = [ {} ];".format(", ".join(str(d) for d in durations)))
    print("rr = [", end="")
    for resource_index in range(3):
        high_group = resource_index
        low_group = (high_group + 1) % 3
        usage = [
            (
                long_task_cap
                if add_long_task and task == n_tasks - 1
                else (
                    high
                    if task // group_size == high_group
                    else low if task // group_size == low_group else 0
                )
            )
            for task in range(n_tasks)
        ]
        print(
            " " * (0 if high_group == 0 else 6) + "|",
            ", ".join(str(task_usage) for task_usage in usage),
            end="|];\n" if high_group == 2 else "\n",
        )
    print("suc = [ {} ];".format(", ".join("{ }" for _ in range(n_tasks))))


if __name__ == "__main__":
    parser = argparse.ArgumentParser("Generates triangular RCPSP instances")
    parser.add_argument("group_size", type=int)
    parser.add_argument("-r", "--resource-capacity", default=100, type=int)
    parser.add_argument("-p", "--low-group-usage", default=40, type=int)
    parser.add_argument("-q", "--high-group-usage", default=70, type=int)
    parser.add_argument("-l", "--add-long-task", action="store_true")
    args = parser.parse_args()
    main(
        args.group_size,
        args.resource_capacity,
        args.low_group_usage,
        args.high_group_usage,
        args.add_long_task,
    )
