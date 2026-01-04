#!/usr/bin/env python3
"""Render a small index.html that links to multiple benchmark reports.

If the underlying data files are available, the page also shows a combined
speedup summary (samples, geometric mean, end-to-end, best, lowest) across all entries.
"""

from __future__ import annotations

import argparse
import html
import os
import shutil
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

from plot_speedup import SpeedupStats, compare_stats, load_records, pairs_from_result


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
    css_href: str,
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
        <span class="label">Geometric mean</span>
        <span class="value">{_fmt(summary.geo_mean)}x</span>
      </div>
      <div class="stat">
        <span class="label">End-to-end speedup</span>
        <span class="value">{_fmt(summary.end_to_end)}x</span>
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
  <link rel="stylesheet" href="{html.escape(css_href)}">
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


def _collect_pairs(data_paths: Iterable[Path]) -> list[tuple[float, float]]:
    pairs: list[tuple[float, float]] = []
    for path in data_paths:
        baselines, memos = pairs_from_result(load_records(path))
        pairs.extend(zip(baselines, memos))
    return pairs


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
    css_source = Path(__file__).with_suffix(".css")
    if not css_source.exists():
        raise FileNotFoundError(f"missing stylesheet source: {css_source}")
    css_path = args.output.with_suffix(".css")
    css_href = os.path.relpath(css_path, args.output.parent)

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
        pairs = _collect_pairs(data_paths)
        if pairs:
            _, summary = compare_stats(pairs)

    args.output.write_text(
        _render_html(args.title, entries_with_rel, summary, css_href),
        encoding="utf-8",
    )
    shutil.copyfile(css_source, css_path)
    msg = f"wrote {args.output} with {len(entries_with_rel)} links"
    if summary:
        msg += (
            f" (combined samples: {summary.samples}, "
            f"geo mean: {_fmt(summary.geo_mean)}x, "
            f"end-to-end: {_fmt(summary.end_to_end)}x, "
            f"max: {_fmt(summary.maximum)}x, "
            f"min: {_fmt(summary.minimum)}x)"
        )
    print(msg)


if __name__ == "__main__":
    main()
