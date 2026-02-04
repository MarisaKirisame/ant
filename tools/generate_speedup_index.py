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
    generate_plot_for_pairs,
    load_records,
    pairs_from_profiles,
    pairs_from_steps,
    plot_rule_stat,
    plot_rule_stat_hits,
    plot_rule_stat_insert_time,
    plot_rule_stat_pvar_length_insert_time,
    plot_rule_stat_depth_insert_time,
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
) -> str:
    memo_plot = plot_depth_breakdown(records.depth_breakdown, output_dir)
    memo_cdf_plot = plot_depth_breakdown_cdf(records.depth_breakdown, output_dir)
    size_scatter_plot = plot_rule_stat(records.rule_stat, output_dir)
    hit_scatter_plot = plot_rule_stat_hits(records.rule_stat, output_dir)
    insert_time_scatter_plot = plot_rule_stat_insert_time(records.rule_stat, output_dir)
    pvar_insert_time_scatter_plot = plot_rule_stat_pvar_length_insert_time(records.rule_stat, output_dir)
    depth_insert_time_scatter_plot = plot_rule_stat_depth_insert_time(records.rule_stat, output_dir)
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
            _render_speedup_comparison(
                records,
                output_dir,
                label="Memo vs CEK",
                baseline_key="cek_profile",
                memo_key="memo_profile",
            )
            _render_speedup_comparison(
                records,
                output_dir,
                label="CEK vs Plain",
                baseline_key="plain_profile",
                memo_key="cek_profile",
            )
            _render_speedup_comparison(
                records,
                output_dir,
                label="Memo vs Plain",
                baseline_key="plain_profile",
                memo_key="memo_profile",
            )
            _render_speedup_comparison(
                records,
                output_dir,
                label="Memo vs Plain (steps)",
                pairs=pairs_from_steps(records),
            )
            _plot_image(memo_plot, "Memo stats depth vs node count plot")
            _plot_image(memo_cdf_plot, "Memo stats CDF plot")
            _plot_image(size_scatter_plot, "Memo rule size vs sc scatter plot")
            _plot_image(hit_scatter_plot, "Memo rule size vs hit count scatter plot")
            _plot_image(insert_time_scatter_plot, "Memo rule size vs insert time scatter plot")
            _plot_image(pvar_insert_time_scatter_plot, "Memo rule pvar length vs insert time scatter plot")
            _plot_image(depth_insert_time_scatter_plot, "Memo rule depth vs insert time scatter plot")
            _render_large_rule_stats(records, min_size=40, limit=5)
            with tag.section(cls="profile"):
                raw(profile_table)
    return doc.render()


def _render_speedup_comparison(
    records: Result,
    output_dir: Path,
    *,
    label: str,
    baseline_key: str | None = None,
    memo_key: str | None = None,
    pairs: list[tuple[float, float]] | None = None,
) -> None:
    if pairs is None:
        if baseline_key is None or memo_key is None:
            raise ValueError("baseline_key and memo_key are required when pairs is None")
        pairs = pairs_from_profiles(records, baseline_key=baseline_key, memo_key=memo_key)
    stats, line_plot, scatter_plot = generate_plot_for_pairs(pairs, output_dir)
    with tag.section(cls="comparison"):
        with tag.details():
            tag.summary(label)
            with tag.section(cls="stats"):
                stat_card("Samples", f"{stats.samples}")
                stat_card("Geometric mean", f"{fmt_speedup(stats.geo_mean)}x")
                stat_card("Arithmetic mean", f"{fmt_speedup(stats.arith_mean)}x")
                stat_card("End-to-end speedup", f"{fmt_speedup(stats.end_to_end)}x")
                stat_card("Best speedup", f"{fmt_speedup(stats.maximum)}x")
                stat_card("Lowest speedup", f"{fmt_speedup(stats.minimum)}x")
            _plot_image(line_plot, f"{label} speedup per run plot")
            _plot_image(scatter_plot, f"{label} scatter plot")
    return None


def _plot_image(src: str, alt: str) -> None:
    with tag.section(cls="plot"):
        tag.img(src=src, alt=alt)


def _render_large_rule_stats(records: Result, *, min_size: int, limit: int) -> None:
    rules = [rule for rule in records.rule_stat if rule.size > min_size]
    rules = sorted(rules, key=lambda rule: rule.size, reverse=True)[:limit]
    with tag.section(cls="rule-stats"):
        tag.h2(f"Rules with size > {min_size}")
        if not rules:
            tag.p("No rules matched the size filter.")
            return None
        with tag.table():
            with tag.thead():
                with tag.tr():
                    tag.th("Size")
                    tag.th("SC")
                    tag.th("Hit count")
                    tag.th("Insert time (ns)")
                    tag.th("PVar length")
                    tag.th("Depth")
                    tag.th("Rule")
            with tag.tbody():
                for rule in rules:
                    with tag.tr():
                        tag.td(str(rule.size))
                        tag.td(str(rule.sc))
                        tag.td(str(rule.hit_count))
                        tag.td(str(rule.insert_time))
                        tag.td(str(rule.pvar_length))
                        tag.td(str(rule.depth))
                        tag.td(tag.code(rule.rule))
    return None


def generate_speedup_report(
    *,
    input_path: Path,
    output_dir: Path,
    css_source: Path,
) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "index.html"
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = output_dir / css_source.name
    css_href = css_source.name
    data_label = str(input_path)
    records = load_records(input_path)
    html = render_html(records, output_dir, data_label, css_href)
    output_path.write_text(html, encoding="utf-8")
    shutil.copyfile(css_source, css_path)
    return None
