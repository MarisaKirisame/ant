#!/usr/bin/env python3
"""Plot speedup ratios from eval_steps.json produced by RunLive.

Each line in the input file is a JSON object with the keys:
  - "step": number of steps with memoization
  - "without_memo_step": baseline step count without memoization

The script computes the speedup ratio `without_memo_step / step` per entry and
plots it as a single line. The resulting plot is saved to ``speedup.png`` in
the current working directory.
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


@dataclass(frozen=True)
class SpeedupStats:
    """Summary statistics for a collection of speedup ratios."""

    samples: int
    mean: float
    minimum: float
    maximum: float


def load_records(path: Path) -> list[float]:
    """Return the list of speedup ratios from a JSONL file."""
    ratios: list[float] = []
    with path.open() as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
                step = rec["step"]
                without = rec["without_memo_step"]
                if step <= 0:
                    raise ValueError("step must be positive")
                ratios.append(without / step)
            except Exception as exc:  # pylint: disable=broad-except
                raise RuntimeError(f"failed to parse line {line_no}") from exc
    if not ratios:
        raise RuntimeError("no records found in file")
    return ratios


def compute_stats(ratios: Sequence[float]) -> SpeedupStats:
    """Compute summary statistics for a sequence of ratios."""
    if not ratios:
        raise ValueError("ratios is empty")
    geo_mean = math.exp(statistics.mean(math.log(r) for r in ratios))
    return SpeedupStats(
        samples=len(ratios),
        mean=geo_mean,
        minimum=min(ratios),
        maximum=max(ratios),
    )


def plot_speedup(ratios: Iterable[float], output: Path):
    xs = list(range(1, len(ratios) + 1))
    plt.figure(figsize=(8, 4.5))
    plt.plot(xs, ratios, marker="o", linewidth=1.5)
    plt.yscale("log")
    plt.xlabel("Execution number (nth run)")
    plt.ylabel("Speedup (baseline / memoized, log scale)")
    plt.title("Memoization Speedup per Execution (log scale)")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


def generate_plot(input_path: Path, output_path: Path) -> tuple[list[float], SpeedupStats]:
    """Load ratios from input_path, write the plot, and return ratios and stats."""
    ratios = load_records(input_path)
    plot_speedup(ratios, output_path)
    return ratios, compute_stats(ratios)


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
        f"(mean: {stats.mean:.2f}x, min: {stats.minimum:.2f}x, max: {stats.maximum:.2f}x)"
    )


if __name__ == "__main__":
    main()
