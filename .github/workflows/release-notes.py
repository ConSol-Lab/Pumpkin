#!/usr/bin/env python3

import argparse
import pathlib
import sys


_STDIO = pathlib.Path("-")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--tag", required=True)
    parser.add_argument("-o", "--output", type=pathlib.Path, required=True)
    args = parser.parse_args()

    [package, version] = args.tag.rsplit("-", 1)
    version = version.lstrip("v")

    print(f"Parsing {package} release notes for version {version}", file=sys.stderr)

    changelog_path = pathlib.Path(f"{package}/CHANGELOG.md")
    with changelog_path.open() as fh:
        lines = fh.readlines()

    note_lines = []
    for line in lines:
        if line.startswith("## ") and version in line:
            note_lines.append(line)
        elif note_lines and line.startswith("## "):
            break
        elif note_lines:
            note_lines.append(line)

    notes = "".join(note_lines).strip()
    if args.output == _STDIO:
        print(notes)
    else:
        args.output.write_text(notes)


if __name__ == "__main__":
    main()
