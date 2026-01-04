#!/usr/bin/env python3
"""Generate an index.html summarizing memoization speedups."""

from __future__ import annotations

import shutil
from pathlib import Path

from dominate import document
from dominate import tags as tag
from dominate.util import raw
from common import fmt_speedup, stat_card
from plot_speedup import (
    SpeedupStats,
    generate_plot,
    load_records,
    plot_size_vs_sc,
    plot_depth_breakdown,
    plot_depth_breakdown_cdf,
    profile_totals_from_result,
    render_profile_table,
)
from stats import Result


def render_html(
    records: Result,
    output_dir: Path,
    data_label: str,
    css_href: str,
) -> tuple[str, SpeedupStats]:
    _, stats, line_plot, scatter_plot = generate_plot(records, output_dir)
    memo_plot = None
    memo_cdf_plot = None
    size_scatter_plot = None
    if records.depth_breakdown:
        memo_plot = plot_depth_breakdown(records.depth_breakdown, output_dir)
        memo_cdf_plot = plot_depth_breakdown_cdf(records.depth_breakdown, output_dir)
    if any(records.size_vs_sc):
        size_scatter_plot = plot_size_vs_sc(records.size_vs_sc, output_dir)
    profile_totals, profile_total_time = profile_totals_from_result(records)
    profile_table = render_profile_table(profile_totals, profile_total_time)
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
                stat_card("Samples", f"{stats.samples}")
                stat_card("Geometric mean", f"{fmt_speedup(stats.geo_mean)}x")
                stat_card("End-to-end speedup", f"{fmt_speedup(stats.end_to_end)}x")
                stat_card("Best speedup", f"{fmt_speedup(stats.maximum)}x")
                stat_card("Lowest speedup", f"{fmt_speedup(stats.minimum)}x")
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
    return doc.render(), stats


def _plot_image(src: str, alt: str) -> None:
    with tag.section(cls="plot"):
        tag.img(src=src, alt=alt)


def generate_speedup_report(
    *,
    input_path: Path,
    output_dir: Path,
    css_source: Path,
) -> SpeedupStats:
    output_path = output_dir / "index.html"
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = output_dir / css_source.name
    css_href = css_source.name
    data_label = str(input_path)
    records = load_records(input_path)
    html, stats = render_html(records, output_dir, data_label, css_href)
    output_path.write_text(html, encoding="utf-8")
    shutil.copyfile(css_source, css_path)
    print(
        f"wrote {output_path} ("
        f"geo mean: {fmt_speedup(stats.geo_mean)}x, "
        f"end-to-end: {fmt_speedup(stats.end_to_end)}x, "
        f"max: {fmt_speedup(stats.maximum)}x, "
        f"min: {fmt_speedup(stats.minimum)}x)"
    )
    return stats
