#!/usr/bin/env python3
"""Plot speedup ratios from eval_steps.json produced by RunLive.

The metric plotted (wall-clock time vs evaluation steps) is controlled by
``REPORT_WALL_CLOCK_TIME`` below. Expected JSONL keys:
  - When ``REPORT_WALL_CLOCK_TIME`` is True:
      * "wall_time_ns": memoized wall-clock time
      * "without_memo_wall_time_ns": baseline wall-clock time
  - When False:
      * "step": memoized step count
      * "without_memo_step": baseline step count

The script computes speedup statistics and plots baseline vs memoized values
as a scatter plot on log-log axes. The resulting plot is saved to
``speedup.png`` in the current working directory.
"""

from __future__ import annotations

import argparse
import json
import math
import statistics
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence

import matplotlib.pyplot as plt

# Toggle the metric being reported. When True, plot wall-clock time speedups;
# when False, plot evaluation-step speedups.
REPORT_WALL_CLOCK_TIME = True

if REPORT_WALL_CLOCK_TIME:
    MEMO_KEY = "wall_time_ns"
    BASELINE_KEY = "without_memo_wall_time_ns"
    METRIC_LABEL = "wall-clock time (ns)"
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


def load_records(path: Path) -> list[tuple[float, float]]:
    """Return (baseline, memoized) metric pairs from a JSONL file."""
    pairs: list[tuple[float, float]] = []
    with path.open() as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
                memo_value = rec[MEMO_KEY]
                baseline_value = rec[BASELINE_KEY]
                if memo_value <= 0:
                    raise ValueError(f"{MEMO_KEY} must be positive")
                if baseline_value <= 0:
                    raise ValueError(f"{BASELINE_KEY} must be positive")
                pairs.append((baseline_value, memo_value))
            except Exception as exc:  # pylint: disable=broad-except
                raise RuntimeError(f"failed to parse line {line_no}") from exc
    if not pairs:
        raise RuntimeError("no records found in file")
    return pairs


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


def plot_speedup(pairs: Iterable[tuple[float, float]], output: Path):
    pairs_list = list(pairs)
    baselines = [baseline for baseline, _ in pairs_list]
    memos = [memo for _, memo in pairs_list]
    plt.figure(figsize=(8, 4.5))
    plt.scatter(baselines, memos, alpha=0.75)
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel(f"Their ({METRIC_LABEL})")
    plt.ylabel(f"Our ({METRIC_LABEL})")
    plt.title(f"Their vs Our ({METRIC_LABEL}, log-log)")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


def generate_plot(input_path: Path, output_path: Path) -> tuple[list[float], SpeedupStats]:
    """Load ratios from input_path, write the plot, and return ratios and stats."""
    pairs = load_records(input_path)
    ratios, stats = compare_stats(pairs)
    plot_speedup(pairs, output_path)
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
        help="path to save the generated plot (default: speedup.png)",
    )
    args = parser.parse_args()

    ratios, stats = generate_plot(args.input, args.output)
    print(
        f"plotted {stats.samples} samples to {args.output} "
        f"(geo mean: {stats.geo_mean:.2f}x, end-to-end: {stats.end_to_end:.2f}x, "
        f"min: {stats.minimum:.2f}x, max: {stats.maximum:.2f}x)"
    )


if __name__ == "__main__":
    main()
