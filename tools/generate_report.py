#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows a combined
speedup summary (samples, geometric mean, end-to-end, best, lowest) across all entries.
"""

from __future__ import annotations

import os
import shutil
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from dominate import document
from dominate import tags as tag
from plot_speedup import SpeedupStats, compare_stats, load_records, pairs_from_result
import generate_speedup_index as speedup_module


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


def generate_html(
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


def generate_reports() -> None:
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_simple.json"),
        output_dir=Path("output/live-simple"),
    )
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_left_to_right.json"),
        output_dir=Path("output/live-left-to-right"),
    )
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_demand_driven.json"),
        output_dir=Path("output/live-demand-driven"),
    )
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_from_hazel.json"),
        output_dir=Path("output/hazel"),
    )
    generate_html(
        title="Live Benchmark Index",
        output=Path("output/index.html"),
        entries=[
            ("Simple Benchmark", Path("output/live-simple/index.html")),
            ("Left-to-right Benchmark", Path("output/live-left-to-right/index.html")),
            ("Demand-driven Benchmark", Path("output/live-demand-driven/index.html")),
            ("Hazel Benchmark", Path("output/hazel/index.html")),
        ],
    )


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
