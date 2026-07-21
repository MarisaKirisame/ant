#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows combined
speedup summaries (geometric + arithmetic means) for key comparisons.
"""

from __future__ import annotations

import math
import json
import os
import shutil
import statistics
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
    plot_entropy_scatter,
    plot_speedup_cdf,
    plot_entropy_speedup_lines,
    plot_speedup_vs_size,
    plot_scatter_for_kind,
    pairs_from_profiles,
    pairs_from_steps,
)
import generate_speedup_index as speedup_module

BASE_EXPERIMENTS: list[tuple[str, str]] = [
    ("append", "Append"),
    ("filter", "Filter"),
    ("map", "Map"),
    ("qs", "QuickSort"),
    ("is", "InsertSort"),
    ("ms", "MergeSort"),
    ("pair", "Pair"),
    ("rev", "Reverse"),
]

SCALING_SIZES = (10, 20, 40, 100, 200, 400)
ENTROPY_SCALING_SIZES = (10, 20, 40, 100, 200, 400, 1000, 2000, 4000)
ENTROPY_CATEGORIES = (
    ("random", "Baseline"),
    ("block", "Block"),
    ("mod1", "Change1"),
    ("same", "Constant"),
)
ENTROPY_PROGRAMS = (
    ("map", "Map"),
    ("append", "Append"),
    # ("insertion_sort", "Insertion sort"),
    ("merge_sort", "Merge sort"),
    ("quick_sort", "Quick sort"),
    # ("reverse", "Reverse"),
    ("simple_filter", "Filter"),
    ("pair", "Pair"),
)

HAZEL_COMPARE_EXCLUDED_MODES = frozenset()

VARIANTS: list[tuple[str, str, str]] = [
    ("results/hazel/{key}.json", "{key}", ""),
    ("results/hazel/th_{key}.json", "th_{key}", " (th)"),
    ("results/hazel/at_{key}.json", "at_{key}", " (at)"),
]

HAZEL_COMPARE_VARIANTS: list[tuple[str, str, str]] = [
    ("results/hazel-compare/{key}.json", "{key}", ""),
    ("results/hazel-compare/th_{key}.json", "th_{key}", " (th)"),
    ("results/hazel-compare/at_{key}.json", "at_{key}", " (at)"),
]

TABLE_VARIANTS: list[tuple[str, str]] = [
    ("1", "results/hazel/{key}.json"),
    ("2", "results/hazel/at_{key}.json"),
    ("3", "results/hazel/th_{key}.json"),
]

HAZEL_NO_EVICT_VARIANTS: list[tuple[str, str, str]] = [
    ("results/hazel-no-evict/{key}.json", "{key}", ""),
    ("results/hazel-no-evict/th_{key}.json", "th_{key}", " (th)"),
    ("results/hazel-no-evict/at_{key}.json", "at_{key}", " (at)"),
]

HAZEL_NO_EVICT_TABLE_VARIANTS: list[tuple[str, str]] = [
    ("1", "results/hazel-no-evict/{key}.json"),
    ("2", "results/hazel-no-evict/at_{key}.json"),
    ("3", "results/hazel-no-evict/th_{key}.json"),
]

def _render_html(
    title: str,
    entries: Sequence[Tuple[str, str]],
    downloads: Sequence[Tuple[str, str]] | None,
    description: str | None,
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
            tag.p(description or "Select a benchmark run to explore the detailed results.")
            if downloads:
                with tag.section(cls="grid"):
                    for label, rel in downloads:
                        with tag.a(href=rel, cls="card", download=""):
                            tag.span(label)
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
    downloads: Sequence[Tuple[str, Path]] | None = None,
    data_paths: Sequence[Path] | None = None,
    description: str | None = None,
    css_source: Path,
    report_kind: str = "hazel",
) -> SpeedupStats | None:
    output.parent.mkdir(parents=True, exist_ok=True)
    entries_with_rel = _relativize(entries, output)
    downloads_with_rel = _relativize(downloads, output) if downloads else None
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = output.parent / css_source.name
    css_href = os.path.relpath(css_path, output.parent)

    inferred_paths: list[Path] = []
    if data_paths is not None:
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
        scatter_name = plot_scatter_for_kind(
            all_pairs,
            output.parent,
            report_kind=report_kind,
        )
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
    if data_paths is None and entries:
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
            downloads_with_rel,
            description,
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
            ("Random", Path("results/entropy/map/random/65536.json")),
            ("Low entropy", Path("results/entropy/map/block/65536.json")),
            ("Change1", Path("results/entropy/map/mod1/65536.json")),
            ("Constant", Path("results/entropy/map/same/65536.json")),
        ],
        output_dir=Path("")
    )

def generate_reports() -> None:
    generate_hazel_reports()
    if Path("results/hazel-no-evict").exists():
        generate_hazel_no_evict_reports()
    generate_arith_reports()
    css_source = Path(__file__).with_name("style.css")
    entries = [
        ("Hazel Report", Path("output/hazel/index.html")),
        ("Hazel No-Eviction Ablation", Path("output/hazel-no-evict/index.html")),
        ("Arith Report", Path("output/arith/index.html")),
        ("Scaling Report", Path("output/scaling/index.html")),
    ]
    entries = [(label, path) for label, path in entries if path.exists()]
    generate_html(
        title="Benchmark Reports",
        output=Path("output/index.html"),
        entries=entries,
        css_source=css_source,
    )


def _scaling_points(data_dirs: Sequence[Path], *, sizes: Sequence[int] = SCALING_SIZES) -> list[tuple[int, float, int]]:
    if not data_dirs:
        raise ValueError("data_dirs is empty")
    points: list[tuple[int, float, int]] = []
    for size in sizes:
        pairs: list[tuple[float, float]] = []
        for data_dir in data_dirs:
            input_path = data_dir / f"{size}.json"
            if not input_path.exists():
                raise FileNotFoundError(f"missing scaling result: {input_path}")
            result = load_records(input_path)
            pairs.extend(pairs_from_profiles(result, baseline_key="cek_profile", memo_key="memo_profile"))
        if not pairs:
            raise ValueError(f"scaling results contain no executions for size {size}")
        _, stats = compare_stats(pairs)
        points.append((size, stats.geo_mean, stats.samples))
    return points


def _write_scaling_page(
    *, title: str, data_dirs: Sequence[Path], output_dir: Path, css_source: Path, sizes: Sequence[int] = SCALING_SIZES
) -> None:
    points = _scaling_points(data_dirs, sizes=sizes)
    output_dir.mkdir(parents=True, exist_ok=True)
    plot_name = plot_speedup_vs_size([(size, speedup) for size, speedup, _ in points], output_dir)
    shutil.copyfile(css_source, output_dir / "style.css")
    doc = document(title=title)
    doc["lang"] = "en"
    with doc.head:
        tag.meta(charset="utf-8")
        tag.link(rel="stylesheet", href="style.css")
    with doc:
        with tag.main(cls="panel"):
            tag.h1(title)
            tag.p("Each size is an independent experiment. Speedup is the existing geometric mean of CEK / memoized profile-time ratios.", cls="meta")
            with tag.section(cls="plot"):
                tag.img(src=plot_name, alt="Memo versus CEK speedup by input size")
            with tag.table():
                with tag.thead():
                    with tag.tr():
                        tag.th("Input size")
                        tag.th("Speedup")
                        tag.th("Samples")
                with tag.tbody():
                    for size, speedup, samples in points:
                        with tag.tr():
                            tag.td(str(size))
                            tag.td(f"{fmt_speedup(speedup)}x")
                            tag.td(str(samples))
    (output_dir / "index.html").write_text(doc.render(), encoding="utf-8")


def generate_scaling_reports(*, modes: Sequence[str] | None = None, sizes: Sequence[int] = SCALING_SIZES) -> None:
    css_source = Path(__file__).with_name("style.css")
    base_output = Path("output/scaling")
    selected_modes = list(modes) if modes is not None else [
        output_pattern.format(key=key)
        for _, output_pattern, _ in VARIANTS
        for key, _ in BASE_EXPERIMENTS
    ]
    entries: list[tuple[str, Path]] = []
    arith_output = base_output / "arith"
    _write_scaling_page(
        title="Arithmetic Scaling",
        data_dirs=[Path("results/arith")],
        output_dir=arith_output,
        css_source=css_source,
        sizes=sizes,
    )
    entries.append(("Arithmetic", arith_output / "index.html"))
    hazel_output = base_output / "hazel"
    _write_scaling_page(
        title="Hazel Scaling (All Modes)",
        data_dirs=[Path("results/hazel") / mode for mode in selected_modes],
        output_dir=hazel_output,
        css_source=css_source,
        sizes=sizes,
    )
    entries.append(("Hazel (all modes)", hazel_output / "index.html"))
    entropy_index = base_output / "entropy" / "index.html"
    if entropy_index.exists():
        entries.append(("Input entropy (6 functions)", entropy_index))
    generate_html(
        title="Scaling Experiments",
        output=base_output / "index.html",
        entries=entries,
        css_source=css_source,
    )


def generate_entropy_scaling_report(*, sizes: Sequence[int] = ENTROPY_SCALING_SIZES) -> None:
    output_dir = Path("output/scaling/entropy")
    output_dir.mkdir(parents=True, exist_ok=True)
    for stale_plot in [*output_dir.glob("*speedup-vs-size.png"), *output_dir.glob("*-scatter.png")]:
        stale_plot.unlink()
    plots: list[tuple[str, str, list[tuple[str, list[tuple[int, float]]]]]] = []
    geomean_by_category: dict[str, dict[int, list[float]]] = {
        input_label: {size: [] for size in sizes} for _, input_label in ENTROPY_CATEGORIES
    }
    scatter_by_category: dict[str, list[tuple[float, float]]] = {input_label: [] for _, input_label in ENTROPY_CATEGORIES}
    for program, label in ENTROPY_PROGRAMS:
        series: list[tuple[str, list[tuple[int, float]]]] = []
        for input_kind, input_label in ENTROPY_CATEGORIES:
            points: list[tuple[int, float]] = []
            for size in sizes:
                input_path = Path("results/entropy") / program / input_kind / f"{size}.json"
                if not input_path.exists():
                    raise FileNotFoundError(f"missing entropy result: {input_path}")
                result = load_records(input_path)
                pairs = pairs_from_profiles(result, baseline_key="cek_profile", memo_key="memo_profile")
                if not pairs:
                    raise ValueError(f"entropy result contains no executions: {input_path}")
                _, stats = compare_stats(pairs)
                points.append((size, stats.geo_mean))
                geomean_by_category[input_label][size].append(stats.geo_mean)
                scatter_by_category[input_label].extend(pairs)
            series.append((input_label, points))
        plot_name = plot_entropy_speedup_lines(
            series,
            output_dir,
            output_name=f"{program}-speedup-vs-size.png",
            title=f"{label}: Speedup by Input Pattern",
        )
        plots.append((label, plot_name, series))
    pattern_series = [
        (
            input_label,
            [(size, _geometric_mean(geomean_by_category[input_label][size])) for size in sizes],
        )
        for _, input_label in ENTROPY_CATEGORIES
    ]
    pattern_plot_name = plot_entropy_speedup_lines(
        pattern_series,
        output_dir,
        output_name="speedup-by-pattern.png",
        title="Speedup by Input Pattern",
    )
    scatter_plot_name = plot_entropy_scatter(
        [(input_label, scatter_by_category[input_label]) for _, input_label in ENTROPY_CATEGORIES],
        output_dir,
        output_name="entropy-scatter.png",
        title="Memo vs CEK by Input Pattern",
    )
    tex_output = output_dir / "entropy_result.tex"
    generate_entropy_scaling_tex(plots=plots, sizes=sizes, output_path=tex_output)
    css_source = Path(__file__).with_name("style.css")
    shutil.copyfile(css_source, output_dir / "style.css")
    doc = document(title="Input Entropy Scaling")
    doc["lang"] = "en"
    with doc.head:
        tag.meta(charset="utf-8")
        tag.link(rel="stylesheet", href="style.css")
    with doc:
        with tag.main(cls="panel"):
            tag.h1("Input Entropy Scaling")
            tag.p(
                f"Six predefined list functions. Each plot has Baseline, Block, Change1, and Constant lines over input size; this sweep ends at {sizes[-1]:,}.",
                cls="meta",
            )
            with tag.section(cls="grid"):
                with tag.a(href=tex_output.name, cls="card", download=""):
                    tag.span("Download entropy_result.tex")
            tag.h2("All Functions")
            with tag.section(cls="plot"):
                tag.img(src=scatter_plot_name, alt="Memo versus CEK scatter plot by input pattern")
            tag.h2("Speedup by Input Pattern")
            with tag.section(cls="plot"):
                tag.img(src=pattern_plot_name, alt="Speedup by input pattern")
            for label, plot_name, series in plots:
                tag.h2(label)
                with tag.section(cls="plot"):
                    tag.img(src=plot_name, alt=f"{label} speedup by size and input pattern")
                with tag.table():
                    with tag.thead():
                        with tag.tr():
                            tag.th("Input pattern")
                            for size in sizes:
                                tag.th(str(size))
                    with tag.tbody():
                        for input_label, points in series:
                            with tag.tr():
                                tag.td(input_label)
                                for _, speedup in points:
                                    tag.td(f"{fmt_speedup(speedup)}x")
    (output_dir / "index.html").write_text(doc.render(), encoding="utf-8")


def _entropy_tex_macro_name(program: str) -> str:
    return "".join(part.capitalize() for part in program.split("_")) + "SpeedupTable"


def generate_entropy_scaling_tex(
    *,
    plots: Sequence[tuple[str, str, list[tuple[str, list[tuple[int, float]]]]]],
    sizes: Sequence[int] = ENTROPY_SCALING_SIZES,
    output_path: Path = Path("output/scaling/entropy/entropy_result.tex"),
) -> None:
    if len(plots) != len(ENTROPY_PROGRAMS):
        raise ValueError("entropy LaTeX tables must cover every configured program")
    lines = ["% Auto-generated by tools/generate_report.py"]
    macro_names: list[tuple[str, str]] = []
    for (program, _), (label, _, series) in zip(ENTROPY_PROGRAMS, plots):
        macro_name = _entropy_tex_macro_name(program)
        macro_names.append((label, macro_name))
        col_spec = "l|" + ("r" * len(sizes))
        header = " & ".join(["Input pattern", *(str(size) for size in sizes)]) + r" \\"
        lines.extend(
            [
                f"\\newcommand{{\\entropy{macro_name}}}{{%",
                f"\\begin{{tabular}}{{{col_spec}}}",
                "\\hline",
                header,
                "\\hline",
            ]
        )
        for input_label, points in series:
            speedups = [fmt_speedup(speedup) for _, speedup in points]
            lines.append(" & ".join([_escape_latex(input_label), *speedups]) + r" \\")
        lines.extend(["\\hline", "\\end{tabular}%", "}", ""])

    lines.extend(["\\newcommand{\\entropySpeedupTables}{%"])
    for index, (label, macro_name) in enumerate(macro_names):
        if index:
            lines.append("\\par\\medskip")
        lines.append(f"\\textbf{{{_escape_latex(label)}}}\\par")
        lines.append(f"\\entropy{macro_name}")
    lines.extend(["}", ""])
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text("\n".join(lines), encoding="utf-8")


def generate_hazel_reports(
    *,
    include_hazel_compare: bool = True,
    modes: Sequence[str] | None = None,
    hazel_compare_modes: Sequence[str] | None = None,
) -> None:
    css_source = Path(__file__).with_name("style.css")
    tex_output = Path("output/hazel/hazel_result.tex")
    extra_entries: list[tuple[str, Path]] = []
    if include_hazel_compare:
        hazel_compare_index = generate_hazel_compare_reports(modes=hazel_compare_modes)
        if hazel_compare_index is not None:
            extra_entries.append(("CEK vs Hazel Baseline", hazel_compare_index))
    generate_tex_table(
        output_path=tex_output,
        include_hazel_compare=include_hazel_compare,
        modes=modes,
        hazel_compare_modes=hazel_compare_modes,
        generate_hazel_compare_report=False,
    )

    _generate_reports_for_experiments(
        title="Hazel Benchmark Index",
        output=Path("output/hazel/index.html"),
        experiments=_hazel_experiments(Path("output/hazel"), modes=modes),
        downloads=[("Download hazel_result.tex", tex_output)],
        css_source=css_source,
        report_kind="hazel",
        extra_entries=extra_entries,
        description=(
            "In the CEK vs Hazel Baseline comparison, timeout and oom statuses refer to the external "
            "Hazel baseline evaluator. A timeout means Hazel did not finish within the per-benchmark compare "
            "budget; oom means the Hazel process exhausted memory. The comparison summary only aggregates "
            "Hazel rows whose status is ok."
        ),
    )


def generate_hazel_no_evict_reports(*, modes: Sequence[str] | None = None) -> None:
    css_source = Path(__file__).with_name("style.css")
    tex_output = Path("output/hazel-no-evict/hazel_no_evict_result.tex")
    generate_hazel_eviction_ablation_tex(
        output_path=tex_output,
        modes=modes,
    )
    ablation_index = generate_hazel_eviction_ablation_report(
        output_dir=Path("output/hazel-no-evict/eviction_ablation"),
        output=Path("output/hazel-no-evict/eviction_ablation/index.html"),
        modes=modes,
    )
    generate_html(
        title="Hazel No-Eviction Ablation Index",
        output=Path("output/hazel-no-evict/index.html"),
        entries=[("Eviction Ablation", ablation_index)],
        downloads=[("Download hazel_no_evict_result.tex", tex_output)],
        data_paths=[],
        css_source=css_source,
        report_kind="hazel",
        description=(
            "No-eviction ablation for the Hazel benchmark inputs. The ablation page compares Chordata "
            "memoized evaluation with eviction against the same Chordata run without eviction."
        ),
    )


def generate_arith_reports() -> None:
    css_source = Path(__file__).with_name("style.css")
    tex_output = Path("output/arith/arith_result.tex")
    generate_arith_tex(output_path=tex_output)
    _generate_reports_for_experiments(
        title="Arith Benchmark Index",
        output=Path("output/arith/index.html"),
        experiments=_arith_experiments(Path("output/arith")),
        downloads=[("Download arith_result.tex", tex_output)],
        css_source=css_source,
        report_kind="arith",
    )


def _hazel_experiments(
    base_dir: Path,
    *,
    modes: Sequence[str] | None = None,
    variants: Sequence[tuple[str, str, str]] = VARIANTS,
) -> list[tuple[str, Path, Path]]:
    selected = set(modes) if modes is not None else None
    experiments: list[tuple[str, Path, Path]] = []
    for steps_pattern, output_pattern, label_suffix in variants:
        for key, label in BASE_EXPERIMENTS:
            mode = output_pattern.format(key=key)
            if selected is not None and mode not in selected:
                continue
            experiments.append(
                (
                    f"{label} Benchmark{label_suffix}",
                    Path(steps_pattern.format(key=key)),
                    base_dir / output_pattern.format(key=key),
                )
            )
    return experiments


def _arith_experiments(base_dir: Path) -> list[tuple[str, Path, Path]]:
    return [("Arith Benchmark", Path("results/arith/arith.json"), base_dir / "arith")]


def _exec_time_rows(path: Path) -> list[dict]:
    rows: list[dict] = []
    with path.open(encoding="utf-8") as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                row = json.loads(line)
            except json.JSONDecodeError as exc:
                raise RuntimeError(f"failed to parse {path}:{line_no}") from exc
            if isinstance(row, dict) and row.get("name") == "exec_time":
                rows.append(row)
    if not rows:
        raise RuntimeError(f"no exec_time rows found in {path}")
    return rows


def _exec_time_row_key(row: dict, *, path: Path, row_index: int) -> tuple[object, ...]:
    missing = [
        key
        for key in (
            "trace_exec_index",
            "input_size",
            "trace_source_indices",
            "trace_source_exercise_ids",
        )
        if key not in row
    ]
    if missing:
        missing_keys = ", ".join(missing)
        raise RuntimeError(f"{path} row {row_index} is missing ablation alignment keys: {missing_keys}")
    source_indices = row["trace_source_indices"]
    source_exercise_ids = row["trace_source_exercise_ids"]
    if not isinstance(source_indices, list) or not isinstance(source_exercise_ids, list):
        raise RuntimeError(f"{path} row {row_index} has malformed ablation alignment keys")
    return (
        row["trace_exec_index"],
        row["input_size"],
        tuple(source_indices),
        tuple(source_exercise_ids),
    )


def _profile_total_from_row(row: dict, key: str, *, path: Path) -> float:
    entries = row.get(key)
    if not isinstance(entries, list):
        raise RuntimeError(f"{path} row is missing profile field {key}")
    return _profile_sum(entries)


def _eviction_ablation_pairs(
    evict_path: Path,
    no_evict_path: Path,
) -> tuple[list[tuple[float, float]], list[float]]:
    if not evict_path.exists() or not no_evict_path.exists():
        return ([], [])
    evict_rows = _exec_time_rows(evict_path)
    no_evict_rows = _exec_time_rows(no_evict_path)
    evict_by_key: dict[tuple[object, ...], dict] = {}
    for index, row in enumerate(evict_rows):
        key = _exec_time_row_key(row, path=evict_path, row_index=index)
        if key in evict_by_key:
            raise RuntimeError(f"duplicate ablation alignment key in {evict_path}: {key}")
        evict_by_key[key] = row
    no_evict_by_key: dict[tuple[object, ...], dict] = {}
    for index, row in enumerate(no_evict_rows):
        key = _exec_time_row_key(row, path=no_evict_path, row_index=index)
        if key in no_evict_by_key:
            raise RuntimeError(f"duplicate ablation alignment key in {no_evict_path}: {key}")
        no_evict_by_key[key] = row
    evict_keys = set(evict_by_key)
    no_evict_keys = set(no_evict_by_key)
    if evict_keys != no_evict_keys:
        missing_no_evict = sorted(evict_keys - no_evict_keys, key=repr)
        missing_evict = sorted(no_evict_keys - evict_keys, key=repr)
        raise RuntimeError(
            "eviction ablation inputs do not align: "
            f"{evict_path} vs {no_evict_path}; "
            f"missing no-evict keys={missing_no_evict[:3]}, missing evict keys={missing_evict[:3]}"
        )
    time_pairs: list[tuple[float, float]] = []
    evict_live_words_values: list[int] = []
    no_evict_live_words_values: list[int] = []
    for key in sorted(evict_keys, key=repr):
        evict_row = evict_by_key[key]
        no_evict_row = no_evict_by_key[key]
        evict_time = _profile_total_from_row(evict_row, "memo_profile", path=evict_path)
        no_evict_time = _profile_total_from_row(no_evict_row, "memo_profile", path=no_evict_path)
        if evict_time > 0 and no_evict_time > 0:
            time_pairs.append((no_evict_time, evict_time))
        evict_live_words = evict_row.get("memo_resting_adjusted_live_words")
        no_evict_live_words = no_evict_row.get("memo_resting_adjusted_live_words")
        if (
            isinstance(evict_live_words, int)
            and isinstance(no_evict_live_words, int)
            and evict_live_words > 0
            and no_evict_live_words > 0
        ):
            evict_live_words_values.append(evict_live_words)
            no_evict_live_words_values.append(no_evict_live_words)

    # Memory is a benchmark-level peak, not a per-trace statistic.  Compute
    # each configuration's peak independently so every benchmark contributes
    # exactly one ratio to the cross-benchmark geometric mean.
    memory_ratios = []
    if evict_live_words_values and no_evict_live_words_values:
        memory_ratios.append(
            float(max(no_evict_live_words_values)) / float(max(evict_live_words_values))
        )
    return (time_pairs, memory_ratios)


def _hazel_eviction_ablation_inputs(
    *,
    modes: Sequence[str] | None = None,
) -> list[tuple[str, Path, Path]]:
    selected = set(modes) if modes is not None else None
    inputs: list[tuple[str, Path, Path]] = []
    for (evict_label, evict_pattern), (no_evict_label, no_evict_pattern) in zip(
        TABLE_VARIANTS,
        HAZEL_NO_EVICT_TABLE_VARIANTS,
    ):
        if evict_label != no_evict_label:
            raise ValueError("eviction ablation variants must stay aligned")
        for key, benchmark_label in BASE_EXPERIMENTS:
            mode = _mode_from_steps_pattern(evict_pattern, key)
            if selected is not None and mode not in selected:
                continue
            inputs.append(
                (
                    f"{benchmark_label} / User {evict_label}",
                    Path(evict_pattern.format(key=key)),
                    Path(no_evict_pattern.format(key=key)),
                )
            )
    return inputs


def _collect_eviction_ablation(
    *,
    modes: Sequence[str] | None = None,
) -> tuple[list[tuple[float, float]], list[float]]:
    all_pairs: list[tuple[float, float]] = []
    all_memory_ratios: list[float] = []
    for _, evict_path, no_evict_path in _hazel_eviction_ablation_inputs(modes=modes):
        time_pairs, memory_ratios = _eviction_ablation_pairs(evict_path, no_evict_path)
        all_pairs.extend(time_pairs)
        all_memory_ratios.extend(memory_ratios)
    return (all_pairs, all_memory_ratios)


def _eviction_overhead_stats(time_pairs: Sequence[tuple[float, float]]) -> tuple[list[float], SpeedupStats]:
    """Compute evicting/no-eviction ratios from no-eviction, evicting time pairs."""
    return compare_stats([(evict_time, no_evict_time) for no_evict_time, evict_time in time_pairs])


def generate_hazel_eviction_ablation_report(
    *,
    output_dir: Path,
    output: Path,
    modes: Sequence[str] | None = None,
) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    css_source = Path(__file__).with_name("style.css")
    css_path = output.parent / css_source.name
    shutil.copyfile(css_source, css_path)
    css_href = os.path.relpath(css_path, output.parent)
    for stale_plot in output_dir.glob("[0-9]*.png"):
        stale_plot.unlink()
    (output_dir / "eviction_ablation_scatter.png").unlink(missing_ok=True)

    time_pairs, memory_ratios = _collect_eviction_ablation(modes=modes)
    ratios: list[float] = []
    stats: SpeedupStats | None = None
    scatter_rel: str | None = None
    memory_overhead = None
    if time_pairs:
        ratios, stats = _eviction_overhead_stats(time_pairs)
        scatter_name = plot_scatter_for_kind(
            time_pairs,
            output_dir,
            report_kind="hazel",
            title="Cache Eviction Ablation",
            xlabel="No-eviction Chordata time (ns)",
            ylabel="Evicting Chordata time (ns)",
            output_name="eviction_ablation_scatter.png",
        )
        scatter_rel = os.path.relpath(output_dir / scatter_name, output.parent)
    if memory_ratios:
        memory_overhead = _geometric_mean(memory_ratios)

    summary_path = output_dir / "eviction_ablation_summary.json"
    summary_payload = {
        "samples": stats.samples if stats else 0,
        "geo_mean_evict_over_no_evict_time": stats.geo_mean if stats else None,
        "arith_mean_evict_over_no_evict_time": stats.arith_mean if stats else None,
        "end_to_end_evict_over_no_evict_time": stats.end_to_end if stats else None,
        "geo_mean_no_evict_over_evict_memory": memory_overhead,
    }
    summary_path.write_text(json.dumps(summary_payload, indent=2), encoding="utf-8")

    doc = document(title="Cache Eviction Ablation")
    doc["lang"] = "en"
    with doc.head:
        tag.meta(charset="utf-8")
        tag.link(rel="stylesheet", href=css_href)
    with doc:
        with tag.main():
            tag.h1("Cache Eviction Ablation")
            tag.p(
                "Hazel benchmark Chordata memo runs with cache eviction compared against the same runs with eviction disabled.",
                cls="meta",
            )
            with tag.section(cls="stats"):
                if stats:
                    stat_card("Samples", str(stats.samples))
                    stat_card("Time overhead", f"{fmt_speedup(stats.geo_mean)}x")
                    stat_card("Arithmetic mean", f"{fmt_speedup(stats.arith_mean)}x")
                    stat_card("End-to-end", f"{fmt_speedup(stats.end_to_end)}x")
                else:
                    stat_card("Samples", "0")
                    stat_card("Time overhead", "timeout")
                if memory_overhead is not None:
                    stat_card("Memory overhead", f"{fmt_speedup(memory_overhead)}x")
                else:
                    stat_card("Memory overhead", "timeout")
            if scatter_rel:
                with tag.section(cls="plot"):
                    tag.img(src=scatter_rel, alt="Evicting Chordata versus no-eviction Chordata scatter plot")
            with tag.section(cls="grid"):
                with tag.a(href=os.path.relpath(summary_path, output.parent), cls="card", download=""):
                    tag.span("Download eviction_ablation_summary.json")

    output.write_text(doc.render(), encoding="utf-8")
    return output


def generate_hazel_eviction_ablation_tex(
    *,
    output_path: Path,
    modes: Sequence[str] | None = None,
) -> None:
    selected = set(modes) if modes is not None else None
    available_pairs: list[tuple[float, float]] = []
    available_memory_ratios: list[float] = []
    rows: list[tuple[str, list[str], list[str]]] = []
    for (evict_label, evict_pattern), (no_evict_label, no_evict_pattern) in zip(
        TABLE_VARIANTS,
        HAZEL_NO_EVICT_TABLE_VARIANTS,
    ):
        if evict_label != no_evict_label:
            raise ValueError("eviction ablation variants must stay aligned")
        speedup_values: list[str] = []
        memory_overhead_values: list[str] = []
        for key, _ in BASE_EXPERIMENTS:
            mode = _mode_from_steps_pattern(evict_pattern, key)
            if selected is not None and mode not in selected:
                speedup_values.append("timeout")
                memory_overhead_values.append("timeout")
                continue
            evict_path = Path(evict_pattern.format(key=key))
            no_evict_path = Path(no_evict_pattern.format(key=key))
            time_pairs, memory_ratios = _eviction_ablation_pairs(evict_path, no_evict_path)
            if time_pairs:
                _, stats = _eviction_overhead_stats(time_pairs)
                speedup_values.append(fmt_speedup(stats.geo_mean))
                available_pairs.extend(time_pairs)
            else:
                speedup_values.append("timeout")
            if memory_ratios:
                memory_overhead_values.append(fmt_speedup(_geometric_mean(memory_ratios)))
                available_memory_ratios.extend(memory_ratios)
            else:
                memory_overhead_values.append("timeout")
        rows.append((evict_label, speedup_values, memory_overhead_values))

    point_count = len(available_pairs)
    total_speedup = "timeout"
    total_memory_overhead = "timeout"
    if available_pairs:
        _, stats = _eviction_overhead_stats(available_pairs)
        total_speedup = _tex_ratio(stats.geo_mean, include_times_symbol=True)
    if available_memory_ratios:
        total_memory_overhead = _tex_ratio(
            _geometric_mean(available_memory_ratios),
            include_times_symbol=True,
        )

    variant_labels = [variant_label for variant_label, _, _ in rows]
    escaped_variant_labels = [_escape_latex(label) for label in variant_labels]
    variant_count = len(variant_labels)
    group_header = (
        f" & \\multicolumn{{{variant_count}}}{{c|}}{{time overhead}}"
        f" & \\multicolumn{{{variant_count}}}{{c}}{{memory overhead}} \\\\"
    )
    user_labels = [f"User {label}" for label in escaped_variant_labels]
    user_header = " & ".join(["Benchmark", *user_labels, *user_labels]) + " \\\\"
    col_spec = f"l|{'r' * variant_count}|{'r' * variant_count}"
    lines = [
        "% Auto-generated by tools/generate_report.py",
        "\\newcommand{\\hazelNoEvictPointCount}{" + str(point_count) + "}",
        "\\newcommand{\\hazelNoEvictTotalSpeedup}{" + total_speedup + "}",
        "\\newcommand{\\hazelNoEvictTotalMemoryOverhead}{" + total_memory_overhead + "}",
        "\\newcommand{\\hazelNoEvictBaselineGeoMean}{N/A}",
        "\\newcommand{\\hazelNoEvictSpeedBreakdown}{N/A}",
        "\\newcommand{\\hazelNoEvictSpeedupTable}{%",
        "\\begin{tabular}{" + col_spec + "}",
        "\\hline",
        group_header,
        user_header,
        "\\hline",
    ]
    for benchmark_idx, (_, benchmark_label) in enumerate(BASE_EXPERIMENTS):
        speedup_row: list[str] = []
        memory_overhead_row: list[str] = []
        for _, speedup_values, memory_overhead_values in rows:
            speedup = speedup_values[benchmark_idx]
            memory_overhead = memory_overhead_values[benchmark_idx]
            speedup_row.append("X" if speedup == "timeout" else speedup)
            memory_overhead_row.append("X" if memory_overhead == "timeout" else memory_overhead)
        lines.append(" & ".join([benchmark_label, *speedup_row, *memory_overhead_row]) + " \\\\")
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


def _generate_reports_for_experiments(
    *,
    title: str,
    output: Path,
    experiments: Sequence[tuple[str, Path, Path]],
    downloads: Sequence[tuple[str, Path]] | None = None,
    css_source: Path,
    report_kind: str,
    extra_entries: Sequence[tuple[str, Path]] | None = None,
    description: str | None = None,
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
            report_kind=report_kind,
        )
        generated_entries.append((label, output_dir / "index.html"))
    if extra_entries:
        generated_entries.extend(extra_entries)
    generate_html(
        title=title,
        output=output,
        entries=generated_entries,
        downloads=downloads,
        description=description,
        css_source=css_source,
        report_kind=report_kind,
    )


def _geomean_memo_vs_cek_speedup(input_path: Path) -> str:
    result = load_records(input_path)
    pairs = pairs_from_profiles(result, baseline_key="cek_profile", memo_key="memo_profile")
    _, stats = compare_stats(pairs)
    return fmt_speedup(stats.geo_mean)


def _tex_ratio(value: float, *, include_times_symbol: bool) -> str:
    formatted = fmt_speedup(value)
    if include_times_symbol:
        return f"${formatted}\\times$"
    return formatted


def _memo_vs_cek_max_live_words(input_path: Path) -> tuple[int, int] | None:
    result = load_records(input_path)
    memo_live_words_values: list[int] = []
    cek_live_words_values: list[int] = []
    for record in result.exec_times:
        memo_live_words = record.memo_resting_adjusted_live_words
        cek_live_words = record.cek_resting_adjusted_live_words
        if memo_live_words is None or cek_live_words is None:
            continue
        if memo_live_words <= 0 or cek_live_words <= 0:
            continue
        memo_live_words_values.append(memo_live_words)
        cek_live_words_values.append(cek_live_words)
    if not memo_live_words_values or not cek_live_words_values:
        return None
    return (max(memo_live_words_values), max(cek_live_words_values))


def _max_memo_vs_cek_memory_overhead(input_path: Path) -> str:
    max_live_words = _memo_vs_cek_max_live_words(input_path)
    if not max_live_words:
        return "timeout"
    max_memo_live_words, max_cek_live_words = max_live_words
    return fmt_speedup(float(max_memo_live_words) / float(max_cek_live_words))


def _max_memo_vs_cek_memory_overhead_ratio(input_path: Path) -> float | None:
    max_live_words = _memo_vs_cek_max_live_words(input_path)
    if not max_live_words:
        return None
    max_memo_live_words, max_cek_live_words = max_live_words
    if max_memo_live_words <= 0 or max_cek_live_words <= 0:
        return None
    return float(max_memo_live_words) / float(max_cek_live_words)


def _geometric_mean(values: Sequence[float]) -> float:
    if not values:
        raise ValueError("values must be non-empty")
    if any(value <= 0 for value in values):
        raise ValueError("values must be positive")
    return math.exp(statistics.mean(math.log(value) for value in values))


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
            return "lookup rule"
        if slot_name == "compose_step":
            return "compose rule"
        if slot_name == "instantiate":
            return "intantiation"
        if slot_name == "insert_step":
            return "insert rule"
        if slot_name == "step_through":
            return "apply rule"
        if slot_name == "exec_cek":
            return "misc"
        return slot_name

    bucket_totals: dict[str, float] = {}
    for slot_name, slot_ns in aggregate_slot_ns.items():
        name = bucket_name(slot_name)
        bucket_totals[name] = bucket_totals.get(name, 0.0) + slot_ns

    ordered_names = sorted(
        bucket_totals,
        key=lambda name: (-bucket_totals[name], name),
    )

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


def _mode_from_steps_pattern(steps_pattern: str, key: str) -> str:
    name = Path(steps_pattern.format(key=key)).name
    if name.startswith("th_"):
        return f"th_{key}"
    if name.startswith("at_"):
        return f"at_{key}"
    return key


def generate_tex_table(
    *,
    output_path: Path = Path("output/hazel/hazel_result.tex"),
    include_hazel_compare: bool = True,
    modes: Sequence[str] | None = None,
    hazel_compare_modes: Sequence[str] | None = None,
    table_variants: Sequence[tuple[str, str]] = TABLE_VARIANTS,
    macro_prefix: str = "hazel",
    generate_hazel_compare_report: bool = True,
) -> None:
    if include_hazel_compare:
        if generate_hazel_compare_report:
            generate_hazel_compare_reports(modes=hazel_compare_modes)
        baseline_geomean = _hazel_baseline_geomean_for_tex()
    else:
        baseline_geomean = "timeout"
    selected = set(modes) if modes is not None else None
    available_input_paths: list[Path] = []
    rows: list[tuple[str, list[str], list[str]]] = []
    for variant_label, steps_pattern in table_variants:
        speedup_values: list[str] = []
        memory_overhead_values: list[str] = []
        for key, _ in BASE_EXPERIMENTS:
            mode = _mode_from_steps_pattern(steps_pattern, key)
            if selected is not None and mode not in selected:
                speedup_values.append("timeout")
                memory_overhead_values.append("timeout")
                continue
            input_path = Path(steps_pattern.format(key=key))
            if not input_path.exists():
                speedup_values.append("timeout")
                memory_overhead_values.append("timeout")
                continue
            available_input_paths.append(input_path)
            speedup_values.append(_geomean_memo_vs_cek_speedup(input_path))
            memory_overhead_values.append(_max_memo_vs_cek_memory_overhead(input_path))
        rows.append((variant_label, speedup_values, memory_overhead_values))

    point_count = 0
    total_speedup = "timeout"
    total_memory_overhead = "timeout"
    if available_input_paths:
        pairs = _collect_pairs(available_input_paths)
        point_count = len(pairs)
        if pairs:
            _, stats = compare_stats(pairs)
            total_speedup = _tex_ratio(stats.geo_mean, include_times_symbol=True)
        experiment_memory_overheads: list[float] = []
        for input_path in available_input_paths:
            ratio = _max_memo_vs_cek_memory_overhead_ratio(input_path)
            if ratio is not None:
                experiment_memory_overheads.append(ratio)
        if experiment_memory_overheads:
            total_memory_overhead = _tex_ratio(
                _geometric_mean(experiment_memory_overheads), include_times_symbol=True
            )
    breakdown_lines = _memo_speed_breakdown_lines(available_input_paths)

    variant_labels = [variant_label for variant_label, _, _ in rows]
    escaped_variant_labels = [_escape_latex(label) for label in variant_labels]
    variant_count = len(variant_labels)
    group_header = (
        f" & \\multicolumn{{{variant_count}}}{{c|}}{{time speedup}}"
        f" & \\multicolumn{{{variant_count}}}{{c}}{{memory overhead}} \\\\"
    )
    user_labels = [f"User {label}" for label in escaped_variant_labels]
    user_header = " & ".join(["Benchmark", *user_labels, *user_labels]) + " \\\\"
    col_spec = f"l|{'r' * variant_count}|{'r' * variant_count}"
    lines = [
        "% Auto-generated by tools/generate_report.py",
        f"\\newcommand{{\\{macro_prefix}PointCount}}{{" + str(point_count) + "}",
        f"\\newcommand{{\\{macro_prefix}TotalSpeedup}}{{" + total_speedup + "}",
        f"\\newcommand{{\\{macro_prefix}TotalMemoryOverhead}}{{" + total_memory_overhead + "}",
        f"\\newcommand{{\\{macro_prefix}BaselineGeoMean}}{{" + baseline_geomean + "}",
        f"\\newcommand{{\\{macro_prefix}SpeedBreakdown}}{{%",
        *breakdown_lines,
        "}",
        f"\\newcommand{{\\{macro_prefix}SpeedupTable}}{{%",
        "\\begin{tabular}{" + col_spec + "}",
        "\\hline",
        group_header,
        user_header,
        "\\hline",
    ]
    for benchmark_idx, (_, benchmark_label) in enumerate(BASE_EXPERIMENTS):
        speedup_row: list[str] = []
        memory_overhead_row: list[str] = []
        for _, speedup_values, memory_overhead_values in rows:
            speedup = speedup_values[benchmark_idx]
            memory_overhead = memory_overhead_values[benchmark_idx]
            display_speedup = "X" if speedup == "timeout" else speedup
            display_memory_overhead = "X" if memory_overhead == "timeout" else memory_overhead
            speedup_row.append(display_speedup)
            memory_overhead_row.append(display_memory_overhead)
        benchmark_values = [*speedup_row, *memory_overhead_row]
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


def generate_arith_tex(
    *,
    input_path: Path = Path("results/arith/arith.json"),
    output_path: Path = Path("output/arith/arith_result.tex"),
) -> None:
    speedup = "timeout"
    if input_path.exists():
        result = load_records(input_path)
        pairs = pairs_from_profiles(result, baseline_key="cek_profile", memo_key="memo_profile")
        if pairs:
            _, stats = compare_stats(pairs)
            speedup = f"${fmt_speedup(stats.geo_mean)}\\times$"

    lines = [
        "% Auto-generated by tools/generate_report.py",
        "\\newcommand{\\arithTotalSpeedup}{" + speedup + "}",
        "",
    ]
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


def _profile_sum(entries: object) -> float:
    if not isinstance(entries, list):
        return 0.0
    total = 0.0
    for pair in entries:
        if not isinstance(pair, list) or len(pair) != 2:
            continue
        value = pair[1]
        if isinstance(value, (int, float)):
            total += float(value)
    return total


def _collect_hazel_compare_pairs(
    input_paths: Sequence[Path],
    *,
    hazel_key: str,
) -> list[tuple[float, float]]:
    pairs: list[tuple[float, float]] = []
    for path in input_paths:
        if not path.exists():
            continue
        for line in path.read_text(encoding="utf-8").splitlines():
            if not line.strip():
                continue
            try:
                row = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not isinstance(row, dict):
                continue
            if row.get("name") != "exec_time":
                continue
            if row.get("hazel_status") != "ok":
                continue
            cek = _profile_sum(row.get("cek_profile"))
            hazel_value = row.get(hazel_key)
            if not isinstance(hazel_value, (int, float)):
                continue
            hazel = float(hazel_value)
            if cek > 0 and hazel > 0:
                pairs.append((cek, hazel))
    return pairs


def _hazel_compare_summary(pairs: Sequence[tuple[float, float]]) -> dict[str, float]:
    hazel_over_cek_ratios = [hz / cek for cek, hz in pairs]
    return {
        "samples": float(len(hazel_over_cek_ratios)),
        "geo_mean_hazel_over_cek": math.exp(statistics.mean(math.log(r) for r in hazel_over_cek_ratios)),
        "arith_mean_hazel_over_cek": statistics.mean(hazel_over_cek_ratios),
        "end_to_end_hazel_over_cek": sum(hz for _, hz in pairs) / sum(cek for cek, _ in pairs),
    }


def _summary_json(path: Path) -> dict[str, object] | None:
    if not path.exists():
        return None
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(data, dict):
        return None
    return data


def generate_hazel_compare_reports(
    *,
    output_dir: Path = Path("output/hazel/hazel_compare"),
    output: Path = Path("output/hazel/hazel_compare/index.html"),
    modes: Sequence[str] | None = None,
) -> Path | None:
    selected = set(modes) if modes is not None else None
    input_paths: list[Path] = []
    for steps_pattern, _, _ in HAZEL_COMPARE_VARIANTS:
        for key, _ in BASE_EXPERIMENTS:
            mode = _mode_from_steps_pattern(steps_pattern, key)
            if selected is not None:
                if mode not in selected:
                    continue
            elif mode in HAZEL_COMPARE_EXCLUDED_MODES:
                continue
            input_paths.append(Path(steps_pattern.format(key=key)))

    eval_only_pairs = _collect_hazel_compare_pairs(input_paths, hazel_key="hazel_eval_only_ns")
    eval_only_summary_path = output_dir / "hazel_vs_cek_eval_only_summary.json"
    stale_summary_path = output_dir / "hazel_vs_chordata_eval_only_summary.json"
    if not eval_only_pairs:
        eval_only_summary_path.unlink(missing_ok=True)
        stale_summary_path.unlink(missing_ok=True)
        return None

    output_dir.mkdir(parents=True, exist_ok=True)
    css_source = Path(__file__).with_name("style.css")
    css_path = output.parent / css_source.name
    shutil.copyfile(css_source, css_path)
    css_href = os.path.relpath(css_path, output.parent)
    for stale_plot in output_dir.glob("[0-9]*.png"):
        stale_plot.unlink()
    stale_summary_path.unlink(missing_ok=True)

    eval_only_summary: dict[str, float] | None = None
    eval_only_scatter_rel: str | None = None
    if eval_only_pairs:
        eval_only_summary = _hazel_compare_summary(eval_only_pairs)
        eval_only_summary_path.write_text(json.dumps(eval_only_summary, indent=2), encoding="utf-8")
        hazel_baseline_pairs = [(hazel, cek) for cek, hazel in eval_only_pairs]
        scatter_name = plot_scatter_for_kind(
            hazel_baseline_pairs,
            output_dir,
            report_kind="hazel",
            title="CEK vs Hazel Baseline",
            xlabel="Hazel baseline time (ns)",
            ylabel="CEK time (ns)",
            output_name="hazel_vs_cek_scatter.png",
        )
        eval_only_scatter_rel = os.path.relpath(output_dir / scatter_name, output.parent)

    doc = document(title="CEK vs Hazel Baseline")
    doc["lang"] = "en"
    with doc.head:
        tag.meta(charset="utf-8")
        tag.link(rel="stylesheet", href=css_href)
    with doc:
        with tag.main():
            tag.h1("CEK vs Hazel Baseline")
            tag.p("Summary of Hazel baseline time divided by CEK time from hazel-compare result data.")

            if eval_only_summary:
                tag.h2("Hazel baseline / CEK")
                with tag.section(cls="stats"):
                    stat_card("Samples", str(int(eval_only_summary["samples"])))
                    stat_card(
                        "Geometric mean",
                        f"{fmt_speedup(eval_only_summary['geo_mean_hazel_over_cek'])}x",
                    )
                    stat_card(
                        "Arithmetic mean",
                        f"{fmt_speedup(eval_only_summary['arith_mean_hazel_over_cek'])}x",
                    )
                    stat_card(
                        "End-to-end",
                        f"{fmt_speedup(eval_only_summary['end_to_end_hazel_over_cek'])}x",
                    )
                if eval_only_scatter_rel:
                    with tag.section(cls="plot"):
                        tag.img(
                            src=eval_only_scatter_rel,
                            alt="CEK versus Hazel baseline scatter plot",
                        )

    output.write_text(doc.render(), encoding="utf-8")
    return output


def _hazel_baseline_geomean_for_tex(
    summary_path: Path = Path("output/hazel/hazel_compare/hazel_vs_cek_eval_only_summary.json"),
) -> str:
    summary = _summary_json(summary_path)
    if summary is None:
        return "timeout"
    geo = summary.get("geo_mean_hazel_over_cek")
    if not isinstance(geo, (int, float)):
        return "timeout"
    if float(geo) <= 0:
        return "timeout"
    return _tex_ratio(float(geo), include_times_symbol=True)
