#!/usr/bin/env python3
"""Parsing helpers and data structures for speedup stats.

Each exec_time record is expected to include both step counts and profiles.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class SpeedupStats:
    """Summary statistics for a collection of speedup ratios."""

    samples: int
    geo_mean: float
    arith_mean: float
    end_to_end: float
    minimum: float
    maximum: float


@dataclass(frozen=True)
class MemoStatsNode:
    depth: int
    node_count: int


@dataclass(frozen=True)
class MemoRuleStat:
    size: int
    sc: int
    hit_count: int


@dataclass(frozen=True)
class ProfileEntry:
    name: str
    time_ns: float


@dataclass(frozen=True)
class ExecTimeRecord:
    step: int
    without_memo_step: int
    memo_profile: list[ProfileEntry]
    cek_profile: list[ProfileEntry]
    plain_profile: list[ProfileEntry]


@dataclass(frozen=True)
class Result:
    exec_times: list[ExecTimeRecord]
    depth_breakdown: list[MemoStatsNode]
    rule_stat: list[MemoRuleStat]


def _parse_profile(entries: object, *, key_name: str) -> list[ProfileEntry]:
    if not isinstance(entries, list):
        raise ValueError(f"{key_name} must be a list")
    parsed: list[ProfileEntry] = []
    for idx, entry in enumerate(entries):
        if not (isinstance(entry, list) and len(entry) == 2):
            raise ValueError(f"{key_name}[{idx}] must be a [name, time] pair")
        name, value = entry
        if not isinstance(name, str):
            raise ValueError(f"{key_name}[{idx}] name must be a string")
        if not isinstance(value, (int, float)):
            raise ValueError(f"{key_name}[{idx}] time must be numeric")
        parsed.append(ProfileEntry(name=name, time_ns=float(value)))
    return parsed


def load_records(
    path: Path,
) -> Result:
    """Return parsed records from a JSONL file."""
    exec_times: list[ExecTimeRecord] = []
    depth_breakdown: list[MemoStatsNode] = []
    rule_stat: list[MemoRuleStat] = []
    seen_memo_stats = False
    with path.open() as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
                name = rec.get("name")
                if name == "exec_time":
                    step = rec.get("step")
                    without_memo_step = rec.get("without_memo_step")
                    memo_profile = _parse_profile(
                        rec.get("memo_profile"), key_name="memo_profile"
                    )
                    cek_profile = _parse_profile(
                        rec.get("cek_profile"), key_name="cek_profile"
                    )
                    plain_profile = _parse_profile(
                        rec.get("plain_profile"), key_name="plain_profile"
                    )
                    if not isinstance(step, int):
                        raise ValueError("step must be an int")
                    if not isinstance(without_memo_step, int):
                        raise ValueError("without_memo_step must be an int")
                    exec_times.append(
                        ExecTimeRecord(
                            step=step,
                            without_memo_step=without_memo_step,
                            memo_profile=memo_profile,
                            cek_profile=cek_profile,
                            plain_profile=plain_profile,
                        )
                    )
                elif name == "memo_stats":
                    if seen_memo_stats:
                        raise ValueError("memo_stats record appears more than once")
                    seen_memo_stats = True
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
                    depth_breakdown = nodes
                    raw_rule_stat = rec.get("rule_stat", [])
                    if not isinstance(raw_rule_stat, list):
                        raise ValueError("rule_stat must be a list")
                    rule_stat_entries: list[MemoRuleStat] = []
                    for idx, entry in enumerate(raw_rule_stat):
                        if not isinstance(entry, dict):
                            raise ValueError(f"rule_stat[{idx}] must be an object")
                        size = entry.get("size")
                        sc = entry.get("sc")
                        hit_count = entry.get("hit_count")
                        if not isinstance(size, int):
                            raise ValueError(f"rule_stat[{idx}].size must be an int")
                        if not isinstance(sc, int):
                            raise ValueError(f"rule_stat[{idx}].sc must be an int")
                        if not isinstance(hit_count, int):
                            raise ValueError(f"rule_stat[{idx}].hit_count must be an int")
                        rule_stat_entries.append(
                            MemoRuleStat(size=size, sc=sc, hit_count=hit_count)
                        )
                    rule_stat = rule_stat_entries
                else:
                    raise ValueError(f"unexpected record name: {name}")
            except Exception as exc:  # pylint: disable=broad-except
                raise RuntimeError(f"failed to parse line {line_no}") from exc
    if not exec_times:
        raise RuntimeError("no exec_time records found in file")
    return Result(
        exec_times=exec_times, depth_breakdown=depth_breakdown, rule_stat=rule_stat
    )
