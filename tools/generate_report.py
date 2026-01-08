#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows combined
speedup summaries (geometric + arithmetic means) for key comparisons.
"""

from __future__ import annotations

import os
import shutil
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from dominate import document
from dominate import tags as tag
from common import fmt_speedup, stat_card
from plot_speedup import (
    SpeedupStats,
    compare_stats,
    load_records,
    pairs_from_profiles,
    pairs_from_result,
    pairs_from_steps,
)
import generate_speedup_index as speedup_module


def _render_html(
    title: str,
    entries: Sequence[Tuple[str, str]],
    summary: SpeedupStats | None,
    comparison_summaries: Sequence[Tuple[str, SpeedupStats]] | None,
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
            if comparison_summaries:
                with tag.section(cls="stats"):
                    for label, stats in comparison_summaries:
                        value = (
                            f"Geo {fmt_speedup(stats.geo_mean)}x · "
                            f"Arith {fmt_speedup(stats.arith_mean)}x"
                        )
                        stat_card(label, value)
            elif summary:
                with tag.section(cls="stats"):
                    stat_card("Samples", f"{summary.samples}")
                    stat_card("Geometric mean", f"{fmt_speedup(summary.geo_mean)}x")
                    stat_card("Arithmetic mean", f"{fmt_speedup(summary.arith_mean)}x")
                    stat_card("End-to-end speedup", f"{fmt_speedup(summary.end_to_end)}x")
                    stat_card("Best speedup", f"{fmt_speedup(summary.maximum)}x")
                    stat_card("Lowest speedup", f"{fmt_speedup(summary.minimum)}x")
            with tag.section(cls="grid"):
                for label, rel in entries:
                    with tag.a(href=rel, cls="card"):
                        tag.span(label)
    return doc.render()




def generate_html(
    *,
    title: str,
    output: Path,
    entries: Sequence[Tuple[str, Path]],
    data_paths: Sequence[Path] | None = None,
    css_source: Path,
) -> SpeedupStats | None:
    output.parent.mkdir(parents=True, exist_ok=True)
    entries_with_rel = _relativize(entries, output)
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

    comparisons: list[tuple[str, list[tuple[float, float]]]] = [
        ("Memo vs CEK", []),
        ("CEK vs Plain", []),
        ("Memo vs Plain", []),
        ("Memo vs Plain (steps)", []),
    ]
    if entries:
        for _, report_path in entries:
            if report_path.exists():
                inferred = _extract_data_path(report_path)
                if inferred and inferred.exists():
                    result = load_records(inferred)
                    comparisons[0][1].extend(
                        pairs_from_profiles(
                            result, baseline_key="cek_profile", memo_key="memo_profile"
                        )
                    )
                    comparisons[1][1].extend(
                        pairs_from_profiles(
                            result, baseline_key="plain_profile", memo_key="cek_profile"
                        )
                    )
                    comparisons[2][1].extend(
                        pairs_from_profiles(
                            result, baseline_key="plain_profile", memo_key="memo_profile"
                        )
                    )
                    comparisons[3][1].extend(pairs_from_steps(result))

    comparison_summaries: list[Tuple[str, SpeedupStats]] = []
    for label, pairs in comparisons:
        if pairs:
            _, stats = compare_stats(pairs)
            comparison_summaries.append((label, stats))

    output.write_text(
        _render_html(
            title, entries_with_rel, summary, comparison_summaries, css_href
        ),
        encoding="utf-8",
    )
    shutil.copyfile(css_source, css_path)
    return summary


def generate_reports() -> None:
    css_source = Path(__file__).with_name("style.css")
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_simple.json"),
        output_dir=Path("output/live-simple"),
        css_source=css_source,
    )
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_left_to_right.json"),
        output_dir=Path("output/live-left-to-right"),
        css_source=css_source,
    )
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_demand_driven.json"),
        output_dir=Path("output/live-demand-driven"),
        css_source=css_source,
    )
    speedup_module.generate_speedup_report(
        input_path=Path("eval_steps_from_hazel.json"),
        output_dir=Path("output/hazel"),
        css_source=css_source,
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
        css_source=css_source,
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
    candidates = [report_path.parent / rel]
    candidates.extend(parent / rel for parent in report_path.parents)
    for candidate in candidates:
        if candidate.exists():
            return candidate.resolve()
    return None


def _collect_pairs(data_paths: Iterable[Path]) -> list[tuple[float, float]]:
    pairs: list[tuple[float, float]] = []
    for path in data_paths:
        baselines, memos = pairs_from_result(load_records(path))
        pairs.extend(zip(baselines, memos))
    return pairs
