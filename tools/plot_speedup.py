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
from pathlib import Path

import matplotlib.pyplot as plt


def load_records(path: Path):
    ratios = []
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


def plot_speedup(ratios, output: Path):
    xs = list(range(1, len(ratios) + 1))
    plt.figure(figsize=(8, 4.5))
    plt.plot(xs, ratios, marker="o", linewidth=1.5)
    plt.xlabel("Sample")
    plt.ylabel("Speedup (baseline / memoized)")
    plt.title("Memoization Speedup per Execution")
    plt.grid(True, linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.savefig(output)
    plt.close()


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

    ratios = load_records(args.input)
    plot_speedup(ratios, args.output)
    mean_ratio = sum(ratios) / len(ratios)
    print(f"plotted {len(ratios)} samples to {args.output} (mean speedup: {mean_ratio:.2f}x)")


if __name__ == "__main__":
    main()
