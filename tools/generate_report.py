#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows a combined
speedup summary (samples, geometric mean, end-to-end, best, lowest) across all entries.
"""

from __future__ import annotations

import argparse
import os
import shutil
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from dominate import document
from dominate import tags as tag
from plot_speedup import SpeedupStats, compare_stats, load_records, pairs_from_result


def _parse_entry(raw: str) -> Tuple[str, Path]:
    if "=" not in raw:
        raise argparse.ArgumentTypeError(f"entry must be LABEL=PATH, got {raw!r}")
    label, path = raw.split("=", 1)
    label = label.strip()
    if not label:
        raise argparse.ArgumentTypeError(f"entry label cannot be empty: {raw!r}")
    clean_path = Path(path.strip())
    if not clean_path:
        raise argparse.ArgumentTypeError(f"entry path cannot be empty: {raw!r}")
    return label, clean_path


def _fmt(value: float) -> str:
    """Format a speedup value with sensible precision for very small numbers."""
    return f"{value:.4g}"


def _render_html(
    title: str,
    entries: Sequence[Tuple[str, str]],
    summary: SpeedupStats | None,
    css_href: str,
) -> str:
    doc = document(title=title)
    doc["lang"] = "en"
    with doc.head:
        tag.meta(charset="utf-8")
        tag.link(rel="stylesheet", href=css_href)
    with doc:
        with tag.main():
            tag.h1(title)
            tag.p("Select a benchmark run to explore the detailed results.")
            if summary:
                with tag.section(cls="stats"):
                    _stat_card("Samples", f"{summary.samples}")
                    _stat_card("Geometric mean", f"{_fmt(summary.geo_mean)}x")
                    _stat_card("End-to-end speedup", f"{_fmt(summary.end_to_end)}x")
                    _stat_card("Best speedup", f"{_fmt(summary.maximum)}x")
                    _stat_card("Lowest speedup", f"{_fmt(summary.minimum)}x")
            with tag.section(cls="grid"):
                for label, rel in entries:
                    with tag.a(href=rel, cls="card"):
                        tag.span(label)
    return doc.render()


def _stat_card(label: str, value: str) -> None:
    with tag.div(cls="stat"):
        tag.span(label, cls="label")
        tag.span(value, cls="value")


def generate_report(
    *,
    title: str,
    output: Path,
    entries: Sequence[Tuple[str, Path]],
    data_paths: Sequence[Path] | None = None,
    css_source: Path | None = None,
) -> SpeedupStats | None:
    output.parent.mkdir(parents=True, exist_ok=True)
    entries_with_rel = _relativize(entries, output)
    css_source = css_source or Path(__file__).with_name("style.css")
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = output.parent / css_source.name
    css_href = os.path.relpath(css_path, output.parent)

    inferred_paths: list[Path] = []
    if data_paths:
        inferred_paths.extend(data_paths)
    else:
        for _, report_path in entries:
            if report_path.exists():
                inferred = _extract_data_path(report_path)
                if inferred and inferred.exists():
                    inferred_paths.append(inferred)

    summary: SpeedupStats | None = None
    if inferred_paths:
        pairs = _collect_pairs(inferred_paths)
        if pairs:
            _, summary = compare_stats(pairs)

    output.write_text(
        _render_html(title, entries_with_rel, summary, css_href),
        encoding="utf-8",
    )
    shutil.copyfile(css_source, css_path)
    return summary


def _relativize(entries: Iterable[Tuple[str, Path]], output: Path) -> List[Tuple[str, str]]:
    base = output.parent
    return [(label, os.path.relpath(path, base)) for label, path in entries]


def _extract_data_path(report_path: Path) -> Path | None:
    """Attempt to recover the data source path from a generated report."""
    try:
        text = report_path.read_text(encoding="utf-8")
    except OSError:
        return None
    marker = "Data source:"
    if marker not in text:
        return None
    start = text.find(marker) + len(marker)
    end = text.find("</", start)
    if end == -1:
        return None
    rel = text[start:end].strip()
    if not rel:
        return None
    return (report_path.parent / rel).resolve()


def _collect_pairs(data_paths: Iterable[Path]) -> list[tuple[float, float]]:
    pairs: list[tuple[float, float]] = []
    for path in data_paths:
        baselines, memos = pairs_from_result(load_records(path))
        pairs.extend(zip(baselines, memos))
    return pairs


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--title", default="Live Benchmark Index", help="page title")
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("output/index.html"),
        help="where to write the index file",
    )
    parser.add_argument(
        "--entry",
        action="append",
        type=_parse_entry,
        required=True,
        help="benchmark entry formatted as LABEL=REPORT_PATH (can be repeated)",
    )
    parser.add_argument(
        "--data",
        action="append",
        type=Path,
        help="optional JSONL stats file to include in the combined summary (can be repeated)",
    )
    args = parser.parse_args()

    summary = generate_report(
        title=args.title,
        output=args.output,
        entries=args.entry,
        data_paths=args.data,
    )
    msg = f"wrote {args.output} with {len(args.entry)} links"
    if summary:
        msg += (
            f" (combined samples: {summary.samples}, "
            f"geo mean: {_fmt(summary.geo_mean)}x, "
            f"end-to-end: {_fmt(summary.end_to_end)}x, "
            f"max: {_fmt(summary.maximum)}x, "
            f"min: {_fmt(summary.minimum)}x)"
        )
    print(msg)


if __name__ == "__main__":
    main()
