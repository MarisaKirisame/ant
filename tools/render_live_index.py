#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows a combined
speedup summary (samples, geometric mean, best, lowest) across all entries.
"""

from __future__ import annotations

import argparse
import html
import os
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from plot_speedup import SpeedupStats, compute_stats, load_records


def _parse_entry(raw: str) -> Tuple[str, Path]:
    if "=" not in raw:
        raise argparse.ArgumentTypeError(f"entry must be LABEL=PATH, got {raw!r}")
    label, path = raw.split("=", 1)
    label = label.strip()
    if not label:
        raise argparse.ArgumentTypeError(f"entry label cannot be empty: {raw!r}")
    clean_path = Path(path.strip())
    if not clean_path:
        raise argparse.ArgumentTypeError(f"entry path cannot be empty: {raw!r}")
    return label, clean_path


def _fmt(value: float) -> str:
    """Format a speedup value with sensible precision for very small numbers."""
    return f"{value:.4g}"


def _render_html(
    title: str,
    entries: Sequence[Tuple[str, str]],
    summary: SpeedupStats | None,
) -> str:
    links = "\n".join(
        f'      <a class="card" href="{html.escape(rel)}"><span>{html.escape(label)}</span></a>'
        for label, rel in entries
    )
    stats_html = (
        f"""
    <section class="stats">
      <div class="stat">
        <span class="label">Samples</span>
        <span class="value">{summary.samples}</span>
      </div>
      <div class="stat">
        <span class="label">Mean speedup</span>
        <span class="value">{_fmt(summary.mean)}x</span>
      </div>
      <div class="stat">
        <span class="label">Best speedup</span>
        <span class="value">{_fmt(summary.maximum)}x</span>
      </div>
      <div class="stat">
        <span class="label">Lowest speedup</span>
        <span class="value">{_fmt(summary.minimum)}x</span>
      </div>
    </section>"""
        if summary
        else ""
    )
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>{html.escape(title)}</title>
  <style>
    :root {{
      --bg: #0f172a;
      --card: #111827;
      --muted: #94a3b8;
      --accent: #38bdf8;
      --text: #e5e7eb;
    }}
    * {{
      box-sizing: border-box;
    }}
    body {{
      margin: 0;
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      font-family: "Segoe UI", Helvetica, sans-serif;
      background: radial-gradient(circle at 20% 20%, #1f2937, #0f172a 55%);
      color: var(--text);
      padding: 32px;
    }}
    main {{
      width: min(720px, 100%);
      padding: 32px;
      border-radius: 18px;
      border: 1px solid #1f2937;
      background: #111827;
      box-shadow: 0 10px 50px rgba(0, 0, 0, 0.35);
    }}
    h1 {{
      margin: 0 0 16px;
      font-size: 28px;
      letter-spacing: 0.3px;
    }}
    p {{
      margin: 0 0 24px;
      color: var(--muted);
    }}
    .stats {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
      gap: 12px;
      margin: 0 0 20px;
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
    .grid {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 16px;
    }}
    .card {{
      display: flex;
      align-items: center;
      justify-content: center;
      text-align: center;
      min-height: 120px;
      border-radius: 12px;
      border: 1px solid #1f2937;
      text-decoration: none;
      color: var(--accent);
      font-weight: 600;
      background: #0b1324;
      transition: border-color 0.2s ease, transform 0.2s ease;
    }}
    .card:hover {{
      border-color: var(--accent);
      transform: translateY(-2px);
    }}
  </style>
</head>
<body>
  <main>
    <h1>{html.escape(title)}</h1>
    <p>Select a benchmark run to explore the detailed results.</p>
{stats_html}
    <section class="grid">
{links}
    </section>
  </main>
</body>
</html>
"""


def _relativize(entries: Iterable[Tuple[str, Path]], output: Path) -> List[Tuple[str, str]]:
    base = output.parent
    return [(label, os.path.relpath(path, base)) for label, path in entries]


def _extract_data_path(report_path: Path) -> Path | None:
    """Attempt to recover the data source path from a generated report."""
    try:
        text = report_path.read_text(encoding="utf-8")
    except OSError:
        return None
    marker = "Data source:"
    if marker not in text:
        return None
    start = text.find(marker) + len(marker)
    end = text.find("</", start)
    if end == -1:
        return None
    rel = text[start:end].strip()
    if not rel:
        return None
    return (report_path.parent / rel).resolve()


def _collect_ratios(data_paths: Iterable[Path]) -> list[float]:
    ratios: list[float] = []
    for path in data_paths:
        ratios.extend(load_records(path))
    return ratios


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--title", default="Live Benchmark Index", help="page title")
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("output/index.html"),
        help="where to write the index file",
    )
    parser.add_argument(
        "--entry",
        action="append",
        type=_parse_entry,
        required=True,
        help="benchmark entry formatted as LABEL=REPORT_PATH (can be repeated)",
    )
    parser.add_argument(
        "--data",
        action="append",
        type=Path,
        help="optional JSONL stats file to include in the combined summary (can be repeated)",
    )
    args = parser.parse_args()

    args.output.parent.mkdir(parents=True, exist_ok=True)
    entries_with_rel = _relativize(args.entry, args.output)

    data_paths: list[Path] = []
    if args.data:
        data_paths.extend(args.data)
    else:
        for _, report_path in args.entry:
            if report_path.exists():
                inferred = _extract_data_path(report_path)
                if inferred and inferred.exists():
                    data_paths.append(inferred)

    summary: SpeedupStats | None = None
    if data_paths:
        ratios = _collect_ratios(data_paths)
        if ratios:
            summary = compute_stats(ratios)

    args.output.write_text(
        _render_html(args.title, entries_with_rel, summary), encoding="utf-8"
    )
    msg = f"wrote {args.output} with {len(entries_with_rel)} links"
    if summary:
        msg += (
            f" (combined samples: {summary.samples}, "
            f"mean: {_fmt(summary.mean)}x, "
            f"max: {_fmt(summary.maximum)}x, "
            f"min: {_fmt(summary.minimum)}x)"
        )
    print(msg)


if __name__ == "__main__":
    main()
