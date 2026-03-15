#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows combined
speedup summaries (geometric + arithmetic means) for key comparisons.
"""

from __future__ import annotations

import os
import shutil
import sys
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from dominate import document
from dominate import tags as tag
from common import fmt_speedup, stat_card
from plot_speedup import (
    SpeedupStats,
    compare_stats,
    load_records,
    profile_totals_from_result,
    plot_speedup_cdf,
    plot_scatter,
    pairs_from_profiles,
    pairs_from_steps,
)
import generate_speedup_index as speedup_module

BASE_EXPERIMENTS: list[tuple[str, str]] = [
    ("append", "Append"),
    ("filter", "Filter"),
    ("map", "Map"),
    ("qs", "Quicksort"),
    ("is", "Insertion Sort"),
    ("ms", "Merge Sort"),
    ("pair", "Pair"),
    ("rev", "Reverse"),
]

VARIANTS: list[tuple[str, str, str]] = [
    ("eval_steps_{key}.json", "output/{key}", ""),
    ("eval_steps_th_{key}.json", "output/th_{key}", " (th)"),
    ("eval_steps_at_{key}.json", "output/at_{key}", " (at)"),
]

TABLE_VARIANTS: list[tuple[str, str]] = [
    ("Alice", "eval_steps_{key}.json"),
    ("Bob", "eval_steps_at_{key}.json"),
    ("Charlie", "eval_steps_th_{key}.json"),
]

SPEED_BREAKDOWN_ORDER = [
    "rules lookup",
    "rule composition",
    "rule instantiation",
    "rule insertion",
    "rule application",
    "misc",
]


def _render_html(
    title: str,
    entries: Sequence[Tuple[str, str]],
    summary: SpeedupStats | None,
    comparison_summaries: Sequence[Tuple[str, SpeedupStats]] | None,
    combined_scatter_rel: str | None,
    combined_cdf_rel: str | None,
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
                            f"End-to-end {fmt_speedup(stats.end_to_end)}x"
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
            if combined_scatter_rel:
                with tag.section(cls="plot"):
                    tag.img(src=combined_scatter_rel, alt="Combined scatter plot across all benchmarks")
            if combined_cdf_rel:
                with tag.section(cls="plot"):
                    tag.img(src=combined_cdf_rel, alt="Combined speedup CDF across all benchmarks")
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
    all_pairs: list[tuple[float, float]] = []
    ratios: list[float] = []
    if inferred_paths:
        all_pairs = _collect_pairs(inferred_paths)
        if all_pairs:
            ratios, summary = compare_stats(all_pairs)

    combined_scatter_rel: str | None = None
    combined_cdf_rel: str | None = None
    if all_pairs:
        scatter_name = plot_scatter(all_pairs, output.parent)
        combined_scatter_rel = os.path.relpath(output.parent / scatter_name, output.parent)
    if ratios:
        cdf_name = plot_speedup_cdf(ratios, output.parent)
        combined_cdf_rel = os.path.relpath(output.parent / cdf_name, output.parent)

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
            title,
            entries_with_rel,
            summary,
            comparison_summaries,
            combined_scatter_rel,
            combined_cdf_rel,
            css_href,
        ),
        encoding="utf-8",
    )
    shutil.copyfile(css_source, css_path)
    return summary

def generate_table() -> None:
    speedup_module.generate_table(
        to_compares=[
            ("Random", Path("eval_steps_asymptotic_random.json")),
            ("Low entropy", Path("eval_steps_asymptotic_low_entropy.json")),
            ("Modification", Path("eval_steps_asymptotic_warmed_up.json")),
            ("Repeated", Path("eval_steps_asymptotic_repeated.json")),
        ],
        output_dir=Path("")
    )

def generate_reports() -> None:
    css_source = Path(__file__).with_name("style.css")
    experiments = _hazel_experiments() + _arith_experiments() + _asymptotics_experiments()
    _generate_reports_for_experiments(
        title="Benchmark Index",
        output=Path("output/index.html"),
        experiments=experiments,
        css_source=css_source,
    )


def generate_hazel_reports() -> None:
    css_source = Path(__file__).with_name("style.css")
    _generate_reports_for_experiments(
        title="Hazel Benchmark Index",
        output=Path("output/index.html"),
        experiments=_hazel_experiments(),
        css_source=css_source,
    )


def generate_arith_reports() -> None:
    css_source = Path(__file__).with_name("style.css")
    _generate_reports_for_experiments(
        title="Arith Benchmark Index",
        output=Path("output/arith_index.html"),
        experiments=_arith_experiments(),
        css_source=css_source,
    )

def generate_asymptotics_reports() -> None:
    css_source = Path(__file__).with_name("style.css")
    _generate_reports_for_experiments(
        title="Asymptotics Benchmark Index",
        output=Path("output/asymptotics_index.html"),
        experiments=_asymptotics_experiments(),
        css_source=css_source,
    )


def _hazel_experiments() -> list[tuple[str, Path, Path]]:
    experiments: list[tuple[str, Path, Path]] = []
    for steps_pattern, output_pattern, label_suffix in VARIANTS:
        for key, label in BASE_EXPERIMENTS:
            experiments.append(
                (
                    f"{label} Benchmark{label_suffix}",
                    Path(steps_pattern.format(key=key)),
                    Path(output_pattern.format(key=key)),
                )
            )
    return experiments


def _arith_experiments() -> list[tuple[str, Path, Path]]:
    return [("Arith Benchmark", Path("eval_steps_arith.json"), Path("output/arith"))]

def _asymptotics_experiments() -> list[tuple[str, Path, Path]]:
    result = list()
    for program in ["map", "append", "simple_filter", "reverse", "pair", "insertion_sort", "merge_sort"]: # Reminder: Add quick_sort
        for list_type in ["random", "low_entropy", "mod", "repeated"]:
            result.append((f"{program}-{list_type}", Path(f"eval_steps_asymptotic_{program}_{list_type}.json"), Path(f"output/asymptotics_{program}_{list_type}")))
    return result

def _generate_reports_for_experiments(
    *,
    title: str,
    output: Path,
    experiments: Sequence[tuple[str, Path, Path]],
    css_source: Path,
) -> None:
    generated_entries: list[tuple[str, Path]] = []
    for label, input_path, output_dir in experiments:
        if not input_path.exists():
            print(f"[generate_report] skipping missing input: {input_path}", file=sys.stderr)
            continue
        speedup_module.generate_speedup_report(
            input_path=input_path,
            output_dir=output_dir,
            css_source=css_source,
        )
        generated_entries.append((label, output_dir / "index.html"))
    generate_html(
        title=title,
        output=output,
        entries=generated_entries,
        css_source=css_source,
    )


def _geomean_memo_vs_cek_speedup(input_path: Path) -> str:
    result = load_records(input_path)
    pairs = pairs_from_profiles(result, baseline_key="cek_profile", memo_key="memo_profile")
    _, stats = compare_stats(pairs)
    return f"${fmt_speedup(stats.geo_mean)}\\times$"


def _escape_latex(value: str) -> str:
    escaped = value.replace("\\", r"\textbackslash{}")
    escaped = escaped.replace("&", r"\&")
    escaped = escaped.replace("%", r"\%")
    escaped = escaped.replace("$", r"\$")
    escaped = escaped.replace("#", r"\#")
    escaped = escaped.replace("_", r"\_")
    escaped = escaped.replace("{", r"\{")
    escaped = escaped.replace("}", r"\}")
    return escaped


def _memo_speed_breakdown_lines(data_paths: Sequence[Path]) -> list[str]:
    if not data_paths:
        return ["timeout%"]

    aggregate_slot_ns: dict[str, float] = {}
    total_ns = 0.0
    for path in data_paths:
        result = load_records(path)
        slot_totals, slot_total_ns = profile_totals_from_result(result)
        total_ns += slot_total_ns
        for slot_name, slot_ns in slot_totals.items():
            aggregate_slot_ns[slot_name] = aggregate_slot_ns.get(slot_name, 0.0) + slot_ns

    if total_ns <= 0 or not aggregate_slot_ns:
        return ["timeout%"]

    def bucket_name(slot_name: str) -> str:
        if slot_name == "lookup_step":
            return "rules lookup"
        if slot_name == "compose_step":
            return "rule composition"
        if slot_name == "instantiate":
            return "rule instantiation"
        if slot_name == "insert_step":
            return "rule insertion"
        if slot_name == "step_through":
            return "rule application"
        if slot_name == "exec_cek":
            return "misc"
        return slot_name

    bucket_totals: dict[str, float] = {}
    for slot_name, slot_ns in aggregate_slot_ns.items():
        name = bucket_name(slot_name)
        bucket_totals[name] = bucket_totals.get(name, 0.0) + slot_ns

    ordered_names = [name for name in SPEED_BREAKDOWN_ORDER if name in bucket_totals]
    ordered_names.extend(sorted(name for name in bucket_totals if name not in SPEED_BREAKDOWN_ORDER))

    lines = [
        "\\begin{tabular}{lr}",
        "\\hline",
        "Name & Percent \\\\",
        "\\hline",
    ]
    for name in ordered_names:
        percent = 100.0 * bucket_totals[name] / total_ns
        lines.append(f"{_escape_latex(name)} & {percent:.2f}\\% \\\\")
    lines.extend(
        [
            "\\hline",
            "\\end{tabular}%",
        ]
    )
    return lines


def generate_tex_table(*, output_path: Path = Path("output/hazel.tex")) -> None:
    available_input_paths: list[Path] = []
    rows: list[tuple[str, list[str]]] = []
    for variant_label, steps_pattern in TABLE_VARIANTS:
        values: list[str] = []
        for key, _ in BASE_EXPERIMENTS:
            input_path = Path(steps_pattern.format(key=key))
            if not input_path.exists():
                values.append("timeout")
                continue
            available_input_paths.append(input_path)
            values.append(_geomean_memo_vs_cek_speedup(input_path))
        rows.append((variant_label, values))

    point_count = 0
    total_speedup = "timeout"
    if available_input_paths:
        pairs = _collect_pairs(available_input_paths)
        point_count = len(pairs)
        if pairs:
            _, stats = compare_stats(pairs)
            total_speedup = f"${fmt_speedup(stats.geo_mean)}\\times$"
    breakdown_lines = _memo_speed_breakdown_lines(available_input_paths)

    variant_labels = [variant_label for variant_label, _ in rows]
    header_cells = " & ".join(["Benchmark", *variant_labels])
    col_spec = "l" + ("r" * len(variant_labels))
    lines = [
        "% Auto-generated by tools/generate_report.py",
        "\\newcommand{\\hazelPointCount}{" + str(point_count) + "}",
        "\\newcommand{\\hazelTotalSpeedup}{" + total_speedup + "}",
        "\\newcommand{\\hazelSpeedBreakdown}{%",
        *breakdown_lines,
        "}",
        "\\newcommand{\\hazelSpeedupTable}{%",
        "\\begin{tabular}{" + col_spec + "}",
        "\\hline",
        header_cells + " \\\\",
        "\\hline",
    ]
    for benchmark_idx, (_, benchmark_label) in enumerate(BASE_EXPERIMENTS):
        benchmark_values = [values[benchmark_idx] for _, values in rows]
        lines.append(" & ".join([benchmark_label, *benchmark_values]) + " \\\\")
    lines.extend(
        [
            "\\hline",
            "\\end{tabular}%",
            "}",
            "",
        ]
    )
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text("\n".join(lines), encoding="utf-8")


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
        result = load_records(path)
        pairs.extend(
            pairs_from_profiles(result, baseline_key="cek_profile", memo_key="memo_profile")
        )
    return pairs
