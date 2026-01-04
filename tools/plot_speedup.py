#!/usr/bin/env python3
"""Plot speedup ratios from eval_steps.json produced by RunLive.

The metric plotted (wall-clock time vs evaluation steps) is controlled by
``REPORT_WALL_CLOCK_TIME`` below. Expected JSONL keys for each exec_time record:
  * "memo_profile": list of [name, time_ns] entries for memoized run
  * "cek_profile": list of [name, time_ns] entries for CEK run
  * "plain_profile": list of [name, time_ns] entries for baseline run
  * "step": memoized step count
  * "without_memo_step": baseline step count

The script computes speedup statistics and writes two plots: a baseline vs
memoized scatter plot on log-log axes, and a speedup-over-run line plot. The
plots are saved to ``speedup.png`` and ``scatter.png`` by default. When a
profile table output path is provided, a HTML table summarizing memoized
profile time share per slot is also written.
"""

from __future__ import annotations

import math
import statistics
from pathlib import Path
from typing import Iterable, Sequence

from dominate import tags as tag
import matplotlib.pyplot as plt
import numpy as np

from common import fresh
from stats import (
    MemoSizeVsSc,
    MemoStatsNode,
    ProfileEntry,
    Result,
    SpeedupStats,
    load_records,
)
# Toggle the metric being reported. When True, plot wall-clock time speedups;
# when False, plot evaluation-step speedups.
REPORT_WALL_CLOCK_TIME = True

if REPORT_WALL_CLOCK_TIME:
    MEMO_KEY = "memo_profile"
    BASELINE_KEY = "plain_profile"
    METRIC_LABEL = "profiled time (ns)"
else:
    MEMO_KEY = "step"
    BASELINE_KEY = "without_memo_step"
    METRIC_LABEL = "evaluation steps"




def compare_stats(
    pairs: Sequence[tuple[float, float]]
) -> tuple[list[float], SpeedupStats]:
    """Compute speedup ratios and summary stats from baseline/memo pairs."""
    if not pairs:
        raise ValueError("pairs is empty")
    ratios = [baseline / memo for baseline, memo in pairs]
    total_baseline = sum(baseline for baseline, _ in pairs)
    total_memo = sum(memo for _, memo in pairs)
    geo_mean = math.exp(statistics.mean(math.log(r) for r in ratios))
    stats = SpeedupStats(
        samples=len(pairs),
        geo_mean=geo_mean,
        end_to_end=total_baseline / total_memo,
        minimum=min(ratios),
        maximum=max(ratios),
    )
    return ratios, stats


def _fresh_plot_name() -> str:
    return f"{fresh()}.png"


def plot_scatter(pairs: Iterable[tuple[float, float]], output_dir: Path) -> str:
    pairs_list = list(pairs)
    baselines = [baseline for baseline, _ in pairs_list]
    memos = [memo for _, memo in pairs_list]
    output_path = output_dir / _fresh_plot_name()
    plt.figure(figsize=(6, 4.5))
    plt.scatter(memos, baselines, alpha=0.75)
    min_time = min(min(baselines), min(memos))
    max_time = max(max(baselines), max(memos))
    log_memos = np.log10(memos)
    log_baselines = np.log10(baselines)
    slope, intercept = np.polyfit(log_memos, log_baselines, 1)
    reg_x = np.array([min_time, max_time])
    reg_y = 10 ** (slope * np.log10(reg_x) + intercept)
    plt.plot(reg_x, reg_y, color="tab:blue", linewidth=1.5, label="Linear fit")
    plt.plot(
        [min_time, max_time],
        [min_time, max_time],
        color="black",
        linestyle="--",
        linewidth=1,
    )
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel(f"Our ({METRIC_LABEL})")
    plt.ylabel(f"Their ({METRIC_LABEL})")
    plt.title(f"Our vs Their ({METRIC_LABEL}, log-log)")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.legend()
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    return output_path.name


def plot_speedup_line(ratios: Sequence[float], output_dir: Path) -> str:
    xs = list(range(1, len(ratios) + 1))
    output_path = output_dir / _fresh_plot_name()
    plt.figure(figsize=(6, 4.5))
    plt.plot(xs, ratios, marker="o", linewidth=1.5)
    plt.yscale("log")
    plt.xlabel("Execution number (nth run)")
    plt.ylabel(f"Speedup ({METRIC_LABEL}, baseline / memoized, log scale)")
    plt.title(f"Speedup per Execution ({METRIC_LABEL}, log scale)")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    return output_path.name


def plot_depth_breakdown(
    depth_breakdown: Sequence[MemoStatsNode], output_dir: Path
) -> str:
    if not depth_breakdown:
        raise ValueError("depth_breakdown is empty")
    output_path = output_dir / _fresh_plot_name()
    plt.figure(figsize=(6, 4.5))
    depths = [node.depth for node in depth_breakdown]
    counts = [node.node_count for node in depth_breakdown]
    plt.plot(depths, counts, marker="o", linewidth=1.5)
    plt.xlabel("Depth")
    plt.ylabel("Node count")
    plt.title("Memo stats by depth")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    return output_path.name


def plot_depth_breakdown_cdf(
    depth_breakdown: Sequence[MemoStatsNode], output_dir: Path
) -> str:
    if not depth_breakdown:
        raise ValueError("depth_breakdown is empty")
    output_path = output_dir / _fresh_plot_name()
    plt.figure(figsize=(6, 4.5))
    nodes_sorted = sorted(depth_breakdown, key=lambda node: node.depth)
    total = sum(node.node_count for node in nodes_sorted)
    if total <= 0:
        raise ValueError("depth_breakdown total count must be positive")
    depths: list[int] = []
    cdf: list[float] = []
    cumulative = 0
    for node in nodes_sorted:
        cumulative += node.node_count
        depths.append(node.depth)
        cdf.append(100.0 * cumulative / total)
    plt.plot(depths, cdf, marker="o", linewidth=1.5)
    plt.xlabel("Depth")
    plt.ylabel("Nodes <= depth (%)")
    plt.title("Memo stats CDF")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    return output_path.name


def plot_size_vs_sc(size_vs_sc: Sequence[MemoSizeVsSc], output_dir: Path) -> str:
    if not size_vs_sc:
        raise ValueError("size_vs_sc is empty")
    output_path = output_dir / _fresh_plot_name()
    plt.figure(figsize=(6, 4.5))
    sizes = [entry.size for entry in size_vs_sc]
    scs = [entry.sc for entry in size_vs_sc]
    plt.scatter(sizes, scs, alpha=0.6)
    plt.xlabel("Pattern size")
    plt.ylabel("Step count (sc)")
    plt.title("Memo pattern size vs step count")
    plt.yscale("log")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
    return output_path.name


def _sum_profile(entries: Sequence[ProfileEntry], *, key_name: str) -> float:
    if not entries:
        raise ValueError(f"{key_name} must be non-empty")
    total = sum(entry.time_ns for entry in entries)
    if total <= 0:
        raise ValueError(f"{key_name} total must be positive")
    return total


def pairs_from_result(result: Result) -> tuple[list[float], list[float]]:
    if REPORT_WALL_CLOCK_TIME:
        memos = memo_profile_totals_from_result(result)
        baselines = plain_profile_totals_from_result(result)
    else:
        memos = steps_from_result(result)
        baselines = without_memo_steps_from_result(result)
    return baselines, memos


def steps_from_result(result: Result) -> list[float]:
    steps = [float(record.step) for record in result.exec_times]
    if any(value <= 0 for value in steps):
        raise ValueError("step must be positive")
    return steps


def without_memo_steps_from_result(result: Result) -> list[float]:
    steps = [float(record.without_memo_step) for record in result.exec_times]
    if any(value <= 0 for value in steps):
        raise ValueError("without_memo_step must be positive")
    return steps


def memo_profile_totals_from_result(result: Result) -> list[float]:
    return [
        _sum_profile(record.memo_profile, key_name=MEMO_KEY)
        for record in result.exec_times
    ]


def plain_profile_totals_from_result(result: Result) -> list[float]:
    return [
        _sum_profile(record.plain_profile, key_name=BASELINE_KEY)
        for record in result.exec_times
    ]


def _profile_totals(result: Result) -> tuple[dict[str, float], float]:
    totals: dict[str, float] = {}
    total_time = 0.0
    for record in result.exec_times:
        for entry in record.memo_profile:
            totals[entry.name] = totals.get(entry.name, 0.0) + entry.time_ns
            total_time += entry.time_ns
    if total_time <= 0:
        raise RuntimeError("memo profile total time must be positive")
    return totals, total_time


def profile_totals_from_result(result: Result) -> tuple[dict[str, float], float]:
    return _profile_totals(result)


def profile_values_from_result(result: Result, profile_key: str) -> list[float]:
    values: list[float] = []
    for record in result.exec_times:
        entries = getattr(record, profile_key)
        values.append(_sum_profile(entries, key_name=profile_key))
    return values


def pairs_from_profiles(
    result: Result, *, baseline_key: str, memo_key: str
) -> list[tuple[float, float]]:
    baselines = profile_values_from_result(result, baseline_key)
    memos = profile_values_from_result(result, memo_key)
    return list(zip(baselines, memos))


def pairs_from_steps(result: Result) -> list[tuple[float, float]]:
    baselines = without_memo_steps_from_result(result)
    memos = steps_from_result(result)
    return list(zip(baselines, memos))


def load_profile_totals(input_path: Path) -> tuple[dict[str, float], float]:
    result = load_records(input_path)
    return profile_totals_from_result(result)


def render_profile_table(totals: dict[str, float], total_time: float) -> str:
    rows = sorted(totals.items(), key=lambda item: item[1], reverse=True)
    tbl = tag.table()
    with tbl:
        with tag.thead():
            with tag.tr():
                tag.th("Slot")
                tag.th("Total time (ns)")
                tag.th("Percent")
        with tag.tbody():
            for name, value in rows:
                percent = 100.0 * value / total_time
                with tag.tr():
                    tag.td(name)
                    tag.td(f"{value:.0f}")
                    tag.td(f"{percent:.2f}%")
    return tbl.render()


def generate_plot(
    result: Result, output_dir: Path
) -> tuple[list[float], SpeedupStats, str, str]:
    """Write plots for result into output_dir and return ratios, stats, and filenames."""
    baselines, memos = pairs_from_result(result)
    pairs = list(zip(baselines, memos))
    ratios, stats, line_plot, scatter_plot = generate_plot_for_pairs(pairs, output_dir)
    return ratios, stats, line_plot, scatter_plot


def generate_plot_for_pairs(
    pairs: Sequence[tuple[float, float]], output_dir: Path
) -> tuple[SpeedupStats, str, str]:
    ratios, stats = compare_stats(pairs)
    line_plot = plot_speedup_line(ratios, output_dir)
    scatter_plot = plot_scatter(pairs, output_dir)
    return stats, line_plot, scatter_plot
