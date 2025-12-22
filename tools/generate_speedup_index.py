#!/usr/bin/env python3
"""Generate an index.html summarizing memoization speedups."""

from __future__ import annotations

import argparse
import os
from pathlib import Path

import json
from dataclasses import dataclass
from typing import Iterable

from plot_speedup import SpeedupStats, generate_plot


def _fmt(value: float) -> str:
    """Format a speedup value with sensible precision for very small numbers."""
    return f"{value:.4g}"

@dataclass(frozen=True)
class WrapResult:
    """Single wrap test execution result."""

    wrap_depth: int
    code: str
    value: str
    runtime_seconds: float
    step: int
    without_memo_step: int

    @property
    def speedup(self) -> float:
        return self.without_memo_step / self.step if self.step else float("inf")


def _image_src(plot_path: Path, output_dir: Path) -> str:
    """Return a relative path for the plot image."""
    return os.path.relpath(plot_path, output_dir)


def _load_wrap_results(path: Path | None) -> list[WrapResult]:
    """Return parsed wrap test results or an empty list if path is None."""
    if path is None:
        return []
    results: list[WrapResult] = []
    decoder = json.JSONDecoder()
    content = path.read_text()
    idx = 0
    length = len(content)
    while idx < length:
        while idx < length and content[idx].isspace():
            idx += 1
        if idx >= length:
            break
        try:
            raw, next_idx = decoder.raw_decode(content, idx)
        except Exception as exc:  # pylint: disable=broad-except
            snippet = content[idx : min(length, idx + 80)].splitlines()[0]
            raise RuntimeError(f"failed to parse wrap result near: {snippet!r}") from exc
        results.append(
            WrapResult(
                wrap_depth=int(raw["wrap_depth"]),
                code=str(raw["code"]),
                value=str(raw["value"]),
                runtime_seconds=float(raw["runtime_seconds"]),
                step=int(raw["step"]),
                without_memo_step=int(raw["without_memo_step"]),
            )
        )
        idx = next_idx
    return results


def _wrap_results_table(results: Iterable[WrapResult]) -> str:
    rows = []
    for res in results:
        rows.append(
            "<tr>"
            f"<td>{res.wrap_depth}</td>"
            f"<td><code>{res.code}</code></td>"
            f"<td>{res.value}</td>"
            f"<td>{res.step:,}</td>"
            f"<td>{res.without_memo_step:,}</td>"
            f"<td>{res.speedup:.2f}x</td>"
            f"<td>{res.runtime_seconds:.2f}s</td>"
            "</tr>"
        )
    return "\n        ".join(rows)


def _render_wrap_section(results: list[WrapResult], data_label: str) -> str:
    if not results:
        return ""
    rows = _wrap_results_table(results)
    return f"""
    <section class="wrap">
      <div class="wrap-header">
        <div>
          <h2>Wrap Tests</h2>
          <p class="meta">Data source: {data_label}</p>
        </div>
        <p class="memo-note">Shows memoized wrap depth evaluations vs. baseline</p>
      </div>
      <div class="wrap-table">
        <table>
          <thead>
            <tr>
              <th>Depth</th>
              <th>Code</th>
              <th>Value</th>
              <th>Steps (memo)</th>
              <th>Steps (baseline)</th>
              <th>Speedup</th>
              <th>Runtime</th>
            </tr>
          </thead>
          <tbody>
        {rows}
          </tbody>
        </table>
      </div>
    </section>
"""


def _render_html(
    stats: SpeedupStats,
    img_src: str,
    data_label: str,
    wrap_section: str = "",
) -> str:
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Memoization Speedup</title>
  <style>
    :root {{
      --bg: #0f172a;
      --card: #111827;
      --text: #e5e7eb;
      --muted: #94a3b8;
      --accent: #38bdf8;
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      min-height: 100vh;
      font-family: "Segoe UI", Helvetica, sans-serif;
      background: radial-gradient(circle at 20% 20%, #1f2937, #0f172a 55%);
      color: var(--text);
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 32px;
    }}
    .panel {{
      width: min(960px, 100%);
      background: var(--card);
      border: 1px solid #1f2937;
      border-radius: 16px;
      padding: 28px 32px 32px;
      box-shadow: 0 10px 50px rgba(0, 0, 0, 0.35);
    }}
    h1 {{
      margin: 0 0 12px;
      font-size: 26px;
      letter-spacing: 0.2px;
    }}
    .meta {{
      margin: 0 0 24px;
      color: var(--muted);
      font-size: 14px;
    }}
    .stats {{
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 12px;
      margin-bottom: 20px;
    }}
    .stat {{
      padding: 14px 12px;
      border: 1px solid #1f2937;
      border-radius: 10px;
      background: #0b1324;
    }}
    .label {{
      display: block;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.8px;
      color: var(--muted);
      margin-bottom: 6px;
    }}
    .value {{
      font-size: 22px;
      font-weight: 600;
      color: var(--accent);
    }}
    .plot {{
      width: 100%;
      border: 1px solid #1f2937;
      border-radius: 12px;
      background: #0b1324;
      padding: 12px;
    }}
    .plot img {{
      width: 100%;
      display: block;
      border-radius: 8px;
    }}
    .wrap {{
      margin-top: 28px;
      padding-top: 24px;
      border-top: 1px solid #1f2937;
    }}
    .wrap-header {{
      display: flex;
      flex-wrap: wrap;
      justify-content: space-between;
      gap: 12px;
      align-items: center;
      margin-bottom: 12px;
    }}
    .wrap-header h2 {{
      margin: 0;
      font-size: 20px;
    }}
    .memo-note {{
      margin: 0;
      color: var(--muted);
      font-size: 13px;
    }}
    table {{
      width: 100%;
      border-collapse: collapse;
      font-size: 14px;
    }}
    th, td {{
      padding: 8px 10px;
      border-bottom: 1px solid #1f2937;
      text-align: left;
    }}
    th {{
      text-transform: uppercase;
      letter-spacing: 0.6px;
      font-size: 12px;
      color: var(--muted);
    }}
    tbody tr:last-child td {{
      border-bottom: none;
    }}
    code {{
      font-size: 13px;
      color: var(--accent);
    }}
  </style>
</head>
<body>
  <main class="panel">
    <h1>Memoization Speedup</h1>
    <p class="meta">Data source: {data_label}</p>
    <section class="stats">
      <div class="stat">
        <span class="label">Samples</span>
        <span class="value">{stats.samples}</span>
      </div>
      <div class="stat">
        <span class="label">Mean speedup</span>
        <span class="value">{_fmt(stats.mean)}x</span>
      </div>
      <div class="stat">
        <span class="label">Best speedup</span>
        <span class="value">{_fmt(stats.maximum)}x</span>
      </div>
      <div class="stat">
        <span class="label">Lowest speedup</span>
        <span class="value">{_fmt(stats.minimum)}x</span>
      </div>
    </section>
    <section class="plot">
      <img src="{img_src}" alt="Speedup plot">
    </section>
    {wrap_section}
  </main>
</body>
</html>
"""


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
        "--output",
        type=Path,
        default=Path("index.html"),
        help="where to write the HTML report (default: index.html)",
    )
    parser.add_argument(
        "--wrap-results",
        type=Path,
        default=None,
        help="path to wrap_tests_results.json; when provided adds a wrap section",
    )
    args = parser.parse_args()

    args.plot.parent.mkdir(parents=True, exist_ok=True)
    args.output.parent.mkdir(parents=True, exist_ok=True)

    _, stats = generate_plot(args.input, args.plot)
    img_src = _image_src(args.plot, args.output.parent)
    data_label = os.path.relpath(args.input, args.output.parent)
    wrap_results = _load_wrap_results(args.wrap_results)
    wrap_section = ""
    if wrap_results:
        wrap_label = os.path.relpath(args.wrap_results, args.output.parent)
        wrap_section = _render_wrap_section(wrap_results, wrap_label)

    args.output.write_text(_render_html(stats, img_src, data_label, wrap_section), encoding="utf-8")
    print(
        f"wrote {args.output} (plot: {args.plot}, mean: {_fmt(stats.mean)}x, "
        f"max: {_fmt(stats.maximum)}x, min: {_fmt(stats.minimum)}x)"
    )


if __name__ == "__main__":
    main()
