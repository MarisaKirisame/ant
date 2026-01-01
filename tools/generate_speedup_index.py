#!/usr/bin/env python3
"""Generate an index.html summarizing memoization speedups."""

from __future__ import annotations

import argparse
import os
from pathlib import Path

from plot_speedup import (
    SpeedupStats,
    generate_plot,
    load_profile_totals,
    load_records,
    plot_memo_stats,
    plot_memo_stats_cdf,
    plot_size_vs_sc,
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
) -> str:
    memo_section = (
        f"""
    <section class="plot">
      <img src="{memo_src}" alt="Memo stats depth vs node count plot">
    </section>"""
        if memo_src
        else ""
    )
    memo_cdf_section = (
        f"""
    <section class="plot">
      <img src="{memo_cdf_src}" alt="Memo stats CDF plot">
    </section>"""
        if memo_cdf_src
        else ""
    )
    size_scatter_section = (
        f"""
    <section class="plot">
      <img src="{size_scatter_src}" alt="Memo size vs sc scatter plot">
    </section>"""
        if size_scatter_src
        else ""
    )
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
      grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
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
      margin-bottom: 16px;
    }}
    .plot img {{
      width: 100%;
      display: block;
      border-radius: 8px;
    }}
    .profile {{
      margin-top: 8px;
    }}
    table {{
      width: 100%;
      border-collapse: collapse;
      background: #0b1324;
      border: 1px solid #1f2937;
      border-radius: 12px;
      overflow: hidden;
      font-size: 13px;
    }}
    th, td {{
      text-align: left;
      padding: 10px 12px;
      border-bottom: 1px solid #1f2937;
    }}
    thead {{
      background: #0f1a30;
      color: var(--muted);
      text-transform: uppercase;
      letter-spacing: 0.6px;
      font-size: 11px;
    }}
    tbody tr:last-child td {{
      border-bottom: none;
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
        <span class="label">Geometric mean</span>
        <span class="value">{_fmt(stats.geo_mean)}x</span>
      </div>
      <div class="stat">
        <span class="label">End-to-end speedup</span>
        <span class="value">{_fmt(stats.end_to_end)}x</span>
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
      <img src="{line_src}" alt="Speedup per run plot">
    </section>
    <section class="plot">
      <img src="{scatter_src}" alt="Their vs our scatter plot">
    </section>
{memo_section}
{memo_cdf_section}
{size_scatter_section}
    <section class="profile">
      {profile_table}
    </section>
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

    args.plot.parent.mkdir(parents=True, exist_ok=True)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    scatter_path = args.scatter or args.plot.with_name("scatter.png")

    _, stats = generate_plot(args.input, args.plot, scatter_path)
    memo_src = None
    memo_cdf_src = None
    size_scatter_src = None
    records = load_records(args.input)
    if records.memo_stats:
        memo_plot = args.plot.with_name("memo_stats.png")
        plot_memo_stats(records.memo_stats, memo_plot)
        memo_src = _image_src(memo_plot, args.output.parent)
        memo_cdf_plot = args.plot.with_name("memo_stats_cdf.png")
        plot_memo_stats_cdf(records.memo_stats, memo_cdf_plot)
        memo_cdf_src = _image_src(memo_cdf_plot, args.output.parent)
    if any(records.size_vs_sc):
        size_scatter_plot = args.plot.with_name("size_vs_sc.png")
        plot_size_vs_sc(records.size_vs_sc, size_scatter_plot)
        size_scatter_src = _image_src(size_scatter_plot, args.output.parent)
    profile_totals, profile_total_time = load_profile_totals(args.input)
    profile_table = render_profile_table(profile_totals, profile_total_time)
    line_src = _image_src(args.plot, args.output.parent)
    scatter_src = _image_src(scatter_path, args.output.parent)
    data_label = os.path.relpath(args.input, args.output.parent)
    args.output.write_text(
        _render_html(
            stats,
            line_src,
            scatter_src,
            data_label,
            profile_table,
            memo_src,
            memo_cdf_src,
            size_scatter_src,
        ),
        encoding="utf-8",
    )
    print(
        f"wrote {args.output} (plot: {args.plot}, scatter: {scatter_path}, "
        f"geo mean: {_fmt(stats.geo_mean)}x, "
        f"end-to-end: {_fmt(stats.end_to_end)}x, max: {_fmt(stats.maximum)}x, "
        f"min: {_fmt(stats.minimum)}x)"
    )


if __name__ == "__main__":
    main()
