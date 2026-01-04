#!/usr/bin/env python3
"""Generate an index.html summarizing memoization speedups."""

from __future__ import annotations

import argparse
import os
import shutil
from pathlib import Path

from dominate import document
from dominate import tags as tag
from dominate.util import raw
from plot_speedup import (
    SpeedupStats,
    generate_plot,
    load_profile_totals,
    load_records,
    plot_size_vs_sc,
    plot_depth_breakdown,
    plot_depth_breakdown_cdf,
    render_profile_table,
)


def _fmt(value: float) -> str:
    """Format a speedup value with sensible precision for very small numbers."""
    return f"{value:.4g}"


def _image_src(plot_path: Path, output_dir: Path) -> str:
    """Return a relative path for the plot image."""
    return os.path.relpath(plot_path, output_dir)


def _render_html(
    stats: SpeedupStats,
    line_src: str,
    scatter_src: str,
    data_label: str,
    profile_table: str,
    memo_src: str | None,
    memo_cdf_src: str | None,
    size_scatter_src: str | None,
    css_href: str,
) -> str:
    doc = document(title="Memoization Speedup")
    doc["lang"] = "en"
    with doc.head:
        tag.meta(charset="utf-8")
        tag.link(rel="stylesheet", href=css_href)
    with doc:
        with tag.main(cls="panel"):
            tag.h1("Memoization Speedup")
            tag.p(f"Data source: {data_label}", cls="meta")
            with tag.section(cls="stats"):
                _stat_card("Samples", f"{stats.samples}")
                _stat_card("Geometric mean", f"{_fmt(stats.geo_mean)}x")
                _stat_card("End-to-end speedup", f"{_fmt(stats.end_to_end)}x")
                _stat_card("Best speedup", f"{_fmt(stats.maximum)}x")
                _stat_card("Lowest speedup", f"{_fmt(stats.minimum)}x")
            _plot_image(line_src, "Speedup per run plot")
            _plot_image(scatter_src, "Their vs our scatter plot")
            if memo_src:
                _plot_image(memo_src, "Memo stats depth vs node count plot")
            if memo_cdf_src:
                _plot_image(memo_cdf_src, "Memo stats CDF plot")
            if size_scatter_src:
                _plot_image(size_scatter_src, "Memo size vs sc scatter plot")
            with tag.section(cls="profile"):
                raw(profile_table)
    return doc.render()


def _stat_card(label: str, value: str) -> None:
    with tag.div(cls="stat"):
        tag.span(label, cls="label")
        tag.span(value, cls="value")


def _plot_image(src: str, alt: str) -> None:
    with tag.section(cls="plot"):
        tag.img(src=src, alt=alt)


def generate_speedup_report(
    *,
    input_path: Path,
    plot_path: Path,
    output_path: Path,
    scatter_path: Path | None = None,
    css_source: Path | None = None,
) -> SpeedupStats:
    plot_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    css_source = css_source or Path(__file__).with_name("style.css")
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = output_path.parent / css_source.name
    css_href = os.path.relpath(css_path, output_path.parent)
    scatter_path = scatter_path or plot_path.with_name("scatter.png")

    _, stats = generate_plot(input_path, plot_path, scatter_path)
    memo_src = None
    memo_cdf_src = None
    size_scatter_src = None
    records = load_records(input_path)
    if records.depth_breakdown:
        memo_plot = plot_path.with_name("memo_stats.png")
        plot_depth_breakdown(records.depth_breakdown, memo_plot)
        memo_src = _image_src(memo_plot, output_path.parent)
        memo_cdf_plot = plot_path.with_name("memo_stats_cdf.png")
        plot_depth_breakdown_cdf(records.depth_breakdown, memo_cdf_plot)
        memo_cdf_src = _image_src(memo_cdf_plot, output_path.parent)
    if any(records.size_vs_sc):
        size_scatter_plot = plot_path.with_name("size_vs_sc.png")
        plot_size_vs_sc(records.size_vs_sc, size_scatter_plot)
        size_scatter_src = _image_src(size_scatter_plot, output_path.parent)
    profile_totals, profile_total_time = load_profile_totals(input_path)
    profile_table = render_profile_table(profile_totals, profile_total_time)
    line_src = _image_src(plot_path, output_path.parent)
    scatter_src = _image_src(scatter_path, output_path.parent)
    data_label = os.path.relpath(input_path, output_path.parent)
    output_path.write_text(
        _render_html(
            stats,
            line_src,
            scatter_src,
            data_label,
            profile_table,
            memo_src,
            memo_cdf_src,
            size_scatter_src,
            css_href,
        ),
        encoding="utf-8",
    )
    shutil.copyfile(css_source, css_path)
    return stats


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--input",
        type=Path,
        default=Path("eval_steps.json"),
        help="path to the JSONL stats file (default: eval_steps.json)",
    )
    parser.add_argument(
        "--plot",
        type=Path,
        default=Path("speedup.png"),
        help="where to write the plot image (default: speedup.png)",
    )
    parser.add_argument(
        "--scatter",
        type=Path,
        default=None,
        help="where to write the scatter image (default: scatter.png next to plot)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("index.html"),
        help="where to write the HTML report (default: index.html)",
    )
    args = parser.parse_args()

    scatter_path = args.scatter or args.plot.with_name("scatter.png")
    stats = generate_speedup_report(
        input_path=args.input,
        plot_path=args.plot,
        output_path=args.output,
        scatter_path=scatter_path,
    )
    print(
        f"wrote {args.output} (plot: {args.plot}, scatter: {scatter_path}, "
        f"geo mean: {_fmt(stats.geo_mean)}x, "
        f"end-to-end: {_fmt(stats.end_to_end)}x, max: {_fmt(stats.maximum)}x, "
        f"min: {_fmt(stats.minimum)}x)"
    )


if __name__ == "__main__":
    main()
