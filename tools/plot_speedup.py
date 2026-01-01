#!/usr/bin/env python3
"""Plot speedup ratios from eval_steps.json produced by RunLive.

The metric plotted (wall-clock time vs evaluation steps) is controlled by
``REPORT_WALL_CLOCK_TIME`` below. Expected JSONL keys:
  - When ``REPORT_WALL_CLOCK_TIME`` is True:
      * "memo_profile": list of [name, time_ns] entries for memoized run
      * "plain_profile": list of [name, time_ns] entries for baseline run
  - When False:
      * "step": memoized step count
      * "without_memo_step": baseline step count

The script computes speedup statistics and writes two plots: a baseline vs
memoized scatter plot on log-log axes, and a speedup-over-run line plot. The
plots are saved to ``speedup.png`` and ``scatter.png`` by default. When a
profile table output path is provided, a HTML table summarizing memoized
profile time share per slot is also written.
"""

from __future__ import annotations

import argparse
import html
import json
import math
import statistics
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional, Sequence

import matplotlib.pyplot as plt
import numpy as np

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


@dataclass(frozen=True)
class SpeedupStats:
    """Summary statistics for a collection of speedup ratios."""

    samples: int
    geo_mean: float
    end_to_end: float
    minimum: float
    maximum: float


@dataclass(frozen=True)
class MemoStatsNode:
    depth: int
    node_count: int


@dataclass(frozen=True)
class MemoSizeVsSc:
    size: int
    sc: int


@dataclass(frozen=True)
class Result:
    pairs: list[tuple[float, float]]
    memo_stats: list[list[MemoStatsNode]]
    size_vs_sc: list[list[MemoSizeVsSc]]


def _sum_profile(entries: object, *, key_name: str) -> float:
    if not isinstance(entries, list):
        raise ValueError(f"{key_name} must be a list")
    total = 0.0
    for idx, entry in enumerate(entries):
        if not (isinstance(entry, list) and len(entry) == 2):
            raise ValueError(f"{key_name}[{idx}] must be a [name, time] pair")
        _, value = entry
        if not isinstance(value, (int, float)):
            raise ValueError(f"{key_name}[{idx}] time must be numeric")
        total += float(value)
    return total


def load_profile_totals(path: Path) -> tuple[dict[str, float], float]:
    totals: dict[str, float] = {}
    total_time = 0.0
    with path.open() as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
                if rec.get("name") != "exec_time":
                    continue
                entries = rec[MEMO_KEY]
                if not isinstance(entries, list):
                    raise ValueError(f"{MEMO_KEY} must be a list")
                for idx, entry in enumerate(entries):
                    if not (isinstance(entry, list) and len(entry) == 2):
                        raise ValueError(f"{MEMO_KEY}[{idx}] must be a [name, time] pair")
                    name, value = entry
                    if not isinstance(name, str):
                        raise ValueError(f"{MEMO_KEY}[{idx}] name must be a string")
                    if not isinstance(value, (int, float)):
                        raise ValueError(f"{MEMO_KEY}[{idx}] time must be numeric")
                    totals[name] = totals.get(name, 0.0) + float(value)
                    total_time += float(value)
            except Exception as exc:  # pylint: disable=broad-except
                raise RuntimeError(f"failed to parse line {line_no}") from exc
    if total_time <= 0:
        raise RuntimeError("memo profile total time must be positive")
    return totals, total_time


def render_profile_table(totals: dict[str, float], total_time: float) -> str:
    rows = sorted(totals.items(), key=lambda item: item[1], reverse=True)
    lines = [
        "<table>",
        "  <thead><tr><th>Slot</th><th>Total time (ns)</th><th>Percent</th></tr></thead>",
        "  <tbody>",
    ]
    for name, value in rows:
        percent = 100.0 * value / total_time
        lines.append(
            f"    <tr><td>{html.escape(name)}</td><td>{value:.0f}</td><td>{percent:.2f}%</td></tr>"
        )
    lines.append("  </tbody>")
    lines.append("</table>")
    return "\n".join(lines)


def write_profile_table(input_path: Path, output_path: Path) -> None:
    totals, total_time = load_profile_totals(input_path)
    table = render_profile_table(totals, total_time)
    output_path.write_text(table, encoding="utf-8")


def load_records(path: Path) -> Result:
    """Return parsed records from a JSONL file."""
    pairs: list[tuple[float, float]] = []
    memo_stats: list[list[MemoStatsNode]] = []
    size_vs_sc: list[list[MemoSizeVsSc]] = []
    with path.open() as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
                name = rec.get("name")
                if name == "exec_time":
                    if REPORT_WALL_CLOCK_TIME:
                        memo_value = _sum_profile(rec[MEMO_KEY], key_name=MEMO_KEY)
                        baseline_value = _sum_profile(rec[BASELINE_KEY], key_name=BASELINE_KEY)
                    else:
                        memo_value = rec[MEMO_KEY]
                        baseline_value = rec[BASELINE_KEY]
                    if memo_value <= 0:
                        raise ValueError(f"{MEMO_KEY} must be positive")
                    if baseline_value <= 0:
                        raise ValueError(f"{BASELINE_KEY} must be positive")
                    pairs.append((baseline_value, memo_value))
                elif name == "memo_stats":
                    stats = rec.get("depth_breakdown")
                    if not isinstance(stats, list):
                        raise ValueError("depth_breakdown must be a list")
                    nodes: list[MemoStatsNode] = []
                    for idx, entry in enumerate(stats):
                        if not isinstance(entry, dict):
                            raise ValueError(f"depth_breakdown[{idx}] must be an object")
                        depth = entry.get("depth")
                        node_count = entry.get("node_count")
                        if not isinstance(depth, int):
                            raise ValueError(f"depth_breakdown[{idx}].depth must be an int")
                        if not isinstance(node_count, int):
                            raise ValueError(f"depth_breakdown[{idx}].node_count must be an int")
                        nodes.append(MemoStatsNode(depth=depth, node_count=node_count))
                    memo_stats.append(nodes)
                    raw_size_vs_sc = rec.get("size_vs_sc", [])
                    if raw_size_vs_sc is None:
                        raw_size_vs_sc = []
                    if not isinstance(raw_size_vs_sc, list):
                        raise ValueError("size_vs_sc must be a list")
                    size_vs_sc_entries: list[MemoSizeVsSc] = []
                    for idx, entry in enumerate(raw_size_vs_sc):
                        if not isinstance(entry, dict):
                            raise ValueError(f"size_vs_sc[{idx}] must be an object")
                        size = entry.get("size")
                        sc = entry.get("sc")
                        if not isinstance(size, int):
                            raise ValueError(f"size_vs_sc[{idx}].size must be an int")
                        if not isinstance(sc, int):
                            raise ValueError(f"size_vs_sc[{idx}].sc must be an int")
                        size_vs_sc_entries.append(MemoSizeVsSc(size=size, sc=sc))
                    size_vs_sc.append(size_vs_sc_entries)
                else:
                    raise ValueError(f"unexpected record name: {name}")
            except Exception as exc:  # pylint: disable=broad-except
                raise RuntimeError(f"failed to parse line {line_no}") from exc
    if not pairs:
        raise RuntimeError("no records found in file")
    return Result(pairs=pairs, memo_stats=memo_stats, size_vs_sc=size_vs_sc)


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


def plot_scatter(pairs: Iterable[tuple[float, float]], output: Path) -> None:
    pairs_list = list(pairs)
    baselines = [baseline for baseline, _ in pairs_list]
    memos = [memo for _, memo in pairs_list]
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
    plt.savefig(output)
    plt.close()


def plot_speedup_line(ratios: Sequence[float], output: Path) -> None:
    xs = list(range(1, len(ratios) + 1))
    plt.figure(figsize=(6, 4.5))
    plt.plot(xs, ratios, marker="o", linewidth=1.5)
    plt.yscale("log")
    plt.xlabel("Execution number (nth run)")
    plt.ylabel(f"Speedup ({METRIC_LABEL}, baseline / memoized, log scale)")
    plt.title(f"Speedup per Execution ({METRIC_LABEL}, log scale)")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


def plot_memo_stats(
    memo_stats: Sequence[Sequence[MemoStatsNode]], output: Path
) -> None:
    if not memo_stats:
        raise ValueError("memo_stats is empty")
    plt.figure(figsize=(6, 4.5))
    for idx, nodes in enumerate(memo_stats, 1):
        if not nodes:
            continue
        depths = [node.depth for node in nodes]
        counts = [node.node_count for node in nodes]
        label = f"run {idx}" if len(memo_stats) > 1 else None
        plt.plot(depths, counts, marker="o", linewidth=1.5, label=label)
    plt.xlabel("Depth")
    plt.ylabel("Node count")
    plt.title("Memo stats by depth")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    if len(memo_stats) > 1:
        plt.legend()
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


def plot_memo_stats_cdf(
    memo_stats: Sequence[Sequence[MemoStatsNode]], output: Path
) -> None:
    if not memo_stats:
        raise ValueError("memo_stats is empty")
    plt.figure(figsize=(6, 4.5))
    for idx, nodes in enumerate(memo_stats, 1):
        if not nodes:
            continue
        nodes_sorted = sorted(nodes, key=lambda node: node.depth)
        total = sum(node.node_count for node in nodes_sorted)
        if total <= 0:
            continue
        depths: list[int] = []
        cdf: list[float] = []
        cumulative = 0
        for node in nodes_sorted:
            cumulative += node.node_count
            depths.append(node.depth)
            cdf.append(100.0 * cumulative / total)
        label = f"run {idx}" if len(memo_stats) > 1 else None
        plt.plot(depths, cdf, marker="o", linewidth=1.5, label=label)
    plt.xlabel("Depth")
    plt.ylabel("Nodes <= depth (%)")
    plt.title("Memo stats CDF")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    if len(memo_stats) > 1:
        plt.legend()
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


def plot_size_vs_sc(size_vs_sc: Sequence[Sequence[MemoSizeVsSc]], output: Path) -> None:
    if not size_vs_sc:
        raise ValueError("size_vs_sc is empty")
    plt.figure(figsize=(6, 4.5))
    has_points = False
    for idx, entries in enumerate(size_vs_sc, 1):
        if not entries:
            continue
        sizes = [entry.size for entry in entries]
        scs = [entry.sc for entry in entries]
        label = f"run {idx}" if len(size_vs_sc) > 1 else None
        plt.scatter(sizes, scs, alpha=0.6, label=label)
        has_points = True
    if not has_points:
        raise ValueError("size_vs_sc has no entries")
    plt.xlabel("Pattern size")
    plt.ylabel("Step count (sc)")
    plt.title("Memo pattern size vs step count")
    plt.yscale("log")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    if len(size_vs_sc) > 1:
        plt.legend()
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


def generate_plot(
    input_path: Path, line_output: Path, scatter_output: Optional[Path] = None
) -> tuple[list[float], SpeedupStats]:
    """Load pairs from input_path, write plots, and return ratios and stats."""
    result = load_records(input_path)
    ratios, stats = compare_stats(result.pairs)
    plot_speedup_line(ratios, line_output)
    if scatter_output is not None:
        plot_scatter(result.pairs, scatter_output)
    return ratios, stats


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--input",
        type=Path,
        default=Path("eval_steps.json"),
        help="path to the JSONL stats file (default: eval_steps.json)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("speedup.png"),
        help="path to save the speedup line plot (default: speedup.png)",
    )
    parser.add_argument(
        "--scatter",
        type=Path,
        default=Path("scatter.png"),
        help="path to save the scatter plot (default: scatter.png)",
    )
    parser.add_argument(
        "--profile-table",
        type=Path,
        default=Path("memo_profile.html"),
        help="path to save the memo profile HTML table (default: memo_profile.html)",
    )
    args = parser.parse_args()

    ratios, stats = generate_plot(args.input, args.output, args.scatter)
    write_profile_table(args.input, args.profile_table)
    print(
        f"plotted {stats.samples} samples to {args.output} and {args.scatter} "
        f"(geo mean: {stats.geo_mean:.2f}x, end-to-end: {stats.end_to_end:.2f}x, "
        f"min: {stats.minimum:.2f}x, max: {stats.maximum:.2f}x)"
    )


if __name__ == "__main__":
    main()
