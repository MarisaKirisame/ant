#!/usr/bin/env python3
"""Generate an index.html summarizing memoization speedups."""

from __future__ import annotations

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


def _render_html(
    stats: SpeedupStats,
    line_plot: str,
    scatter_plot: str,
    data_label: str,
    profile_table: str,
    memo_plot: str | None,
    memo_cdf_plot: str | None,
    size_scatter_plot: str | None,
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
            _plot_image(line_plot, "Speedup per run plot")
            _plot_image(scatter_plot, "Their vs our scatter plot")
            if memo_plot:
                _plot_image(memo_plot, "Memo stats depth vs node count plot")
            if memo_cdf_plot:
                _plot_image(memo_cdf_plot, "Memo stats CDF plot")
            if size_scatter_plot:
                _plot_image(size_scatter_plot, "Memo size vs sc scatter plot")
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
    output_dir: Path,
    css_source: Path | None = None,
) -> SpeedupStats:
    output_path = output_dir / "index.html"
    css_source = css_source or Path(__file__).with_name("style.css")
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = output_dir / css_source.name
    css_href = css_source.name
    _, stats, line_plot, scatter_plot = generate_plot(input_path, output_dir)
    memo_plot = None
    memo_cdf_plot = None
    size_scatter_plot = None
    records = load_records(input_path)
    if records.depth_breakdown:
        memo_plot = plot_depth_breakdown(records.depth_breakdown, output_dir)
        memo_cdf_plot = plot_depth_breakdown_cdf(records.depth_breakdown, output_dir)
    if any(records.size_vs_sc):
        size_scatter_plot = plot_size_vs_sc(records.size_vs_sc, output_dir)
    profile_totals, profile_total_time = load_profile_totals(input_path)
    profile_table = render_profile_table(profile_totals, profile_total_time)
    data_label = str(input_path)
    output_path.write_text(
        _render_html(
            stats,
            line_plot,
            scatter_plot,
            data_label,
            profile_table,
            memo_plot,
            memo_cdf_plot,
            size_scatter_plot,
            css_href,
        ),
        encoding="utf-8",
    )
    shutil.copyfile(css_source, css_path)
    return stats
