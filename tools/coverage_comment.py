#!/usr/bin/env python3
"""Convert bisect_ppx summaries into a Markdown PR comment."""

from __future__ import annotations

import re
import sys
from pathlib import Path


SUMMARY_RE = re.compile(r"^Coverage: (?P<covered>\d+)/(?P<total>\d+) \((?P<percent>[0-9.]+)%\)$")
PER_FILE_RE = re.compile(
    r"^\s*(?P<percent>[0-9.]+)\s+%\s+(?P<covered>\d+)/(?P<total>\d+)\s+(?P<path>.+?)\s*$"
)


def parse_summary_text(summary: str) -> tuple[str, str, str]:
    match = SUMMARY_RE.match(summary.strip())
    if match is None:
        raise ValueError("Unable to parse coverage summary")
    return match["percent"], match["covered"], match["total"]


def parse_per_file_text(per_file: str) -> list[tuple[str, str, str, str]]:
    rows = []
    for line in per_file.splitlines():
        match = PER_FILE_RE.match(line)
        if match is None:
            raise ValueError(f"Unable to parse coverage row: {line!r}")
        if match["path"] == "Project coverage":
            continue
        rows.append((match["path"], match["percent"], match["covered"], match["total"]))
    return rows


def parse_summary(path: Path) -> tuple[str, str, str]:
    try:
        return parse_summary_text(path.read_text(encoding="utf-8"))
    except ValueError as error:
        raise SystemExit(f"{error} from {path}") from error


def parse_per_file(path: Path) -> list[tuple[str, str, str, str]]:
    try:
        return parse_per_file_text(path.read_text(encoding="utf-8"))
    except ValueError as error:
        raise SystemExit(str(error)) from error


def render_comment(summary: str, per_file: str) -> str:
    percent, covered, total = parse_summary_text(summary)
    per_file_rows = parse_per_file_text(per_file)
    lines = [
        "<!-- ant-coverage-report -->",
        "## Coverage",
        "",
        "| Scope | Coverage | Covered | Total |",
        "| --- | ---: | ---: | ---: |",
        f"| Project | {percent}% | {covered} | {total} |",
        "",
        "<details>",
        "<summary>Per-file coverage</summary>",
        "",
        "| File | Coverage | Covered | Total |",
        "| --- | ---: | ---: | ---: |",
    ]
    for file_path, file_percent, file_covered, file_total in per_file_rows:
        lines.append(f"| `{file_path}` | {file_percent}% | {file_covered} | {file_total} |")
    lines.extend(["", "</details>", ""])
    return "\n".join(lines)


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit("usage: coverage_comment.py SUMMARY PER_FILE")

    summary = Path(sys.argv[1]).read_text(encoding="utf-8")
    per_file = Path(sys.argv[2]).read_text(encoding="utf-8")
    print(render_comment(summary, per_file), end="")


if __name__ == "__main__":
    main()
