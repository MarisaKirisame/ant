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
class MemoNodeStat:
    depth: int
    insert_time: int
    node_state: str


@dataclass(frozen=True)
class MemoHashtableStat:
    depth: int
    size: int


@dataclass(frozen=True)
class MemoNodeCounts:
    stem_nodes: int
    branch_nodes: int
    total_nodes: int


@dataclass(frozen=True)
class MemoRuleStat:
    size: int
    pvar_length: int
    sc: int
    hit_count: int
    insert_time: int
    depth: int
    rule: str


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
    node_stat: list[MemoNodeStat]
    rule_stat: list[MemoRuleStat]
    node_counts: MemoNodeCounts | None
    hashtable_stat: list[MemoHashtableStat]


def _require_dict(value: object, *, ctx: str) -> dict:
    if not isinstance(value, dict):
        raise ValueError(f"{ctx} must be an object")
    return value


def _require_list(value: object, *, ctx: str) -> list:
    if not isinstance(value, list):
        raise ValueError(f"{ctx} must be a list")
    return value


def _require_int(value: object, *, ctx: str) -> int:
    if not isinstance(value, int):
        raise ValueError(f"{ctx} must be an int")
    return value


def _require_str(value: object, *, ctx: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{ctx} must be a string")
    return value


def _parse_profile(entries: object, *, key_name: str) -> list[ProfileEntry]:
    parsed: list[ProfileEntry] = []
    for idx, entry in enumerate(_require_list(entries, ctx=key_name)):
        if not (isinstance(entry, list) and len(entry) == 2):
            raise ValueError(f"{key_name}[{idx}] must be a [name, time] pair")
        name, value = entry
        name = _require_str(name, ctx=f"{key_name}[{idx}].name")
        if not isinstance(value, (int, float)):
            raise ValueError(f"{key_name}[{idx}].time must be numeric")
        parsed.append(ProfileEntry(name=name, time_ns=float(value)))
    return parsed


def load_records(
    path: Path,
) -> Result:
    """Return parsed records from a JSONL file."""
    exec_times: list[ExecTimeRecord] = []
    depth_breakdown: list[MemoStatsNode] = []
    node_stat: list[MemoNodeStat] = []
    rule_stat: list[MemoRuleStat] = []
    node_counts: MemoNodeCounts | None = None
    hashtable_stat: list[MemoHashtableStat] = []
    seen_memo_stats = False
    with path.open() as f:
        for line_no, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rec = _require_dict(json.loads(line), ctx="record")
                name = rec.get("name")
                if name == "exec_time":
                    step = _require_int(rec.get("step"), ctx="exec_time.step")
                    without_memo_step = _require_int(
                        rec.get("without_memo_step"), ctx="exec_time.without_memo_step"
                    )
                    memo_profile = _parse_profile(
                        rec.get("memo_profile"), key_name="memo_profile"
                    )
                    cek_profile = _parse_profile(
                        rec.get("cek_profile"), key_name="cek_profile"
                    )
                    plain_profile = _parse_profile(
                        rec.get("plain_profile"), key_name="plain_profile"
                    )
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
                    stats = _require_list(
                        rec.get("depth_breakdown"), ctx="depth_breakdown"
                    )
                    nodes: list[MemoStatsNode] = []
                    for idx, entry in enumerate(stats):
                        entry = _require_dict(
                            entry, ctx=f"depth_breakdown[{idx}]"
                        )
                        depth = _require_int(
                            entry.get("depth"),
                            ctx=f"depth_breakdown[{idx}].depth",
                        )
                        node_count = _require_int(
                            entry.get("node_count"),
                            ctx=f"depth_breakdown[{idx}].node_count",
                        )
                        nodes.append(MemoStatsNode(depth=depth, node_count=node_count))
                    depth_breakdown = nodes
                    stem_nodes = _require_int(
                        rec.get("stem_nodes"), ctx="stem_nodes"
                    )
                    branch_nodes = _require_int(
                        rec.get("branch_nodes"), ctx="branch_nodes"
                    )
                    total_nodes = _require_int(
                        rec.get("total_nodes"), ctx="total_nodes"
                    )
                    node_counts = MemoNodeCounts(
                            stem_nodes=stem_nodes,
                            branch_nodes=branch_nodes,
                            total_nodes=total_nodes)
                    raw_hashtable_stat = _require_list(
                        rec.get("hashtable_stat"), ctx="hashtable_stat"
                    )
                    hashtable_entries: list[MemoHashtableStat] = []
                    for idx, entry in enumerate(raw_hashtable_stat):
                        entry = _require_dict(
                            entry, ctx=f"hashtable_stat[{idx}]"
                        )
                        depth = _require_int(
                            entry.get("depth"),
                            ctx=f"hashtable_stat[{idx}].depth",
                        )
                        size = _require_int(
                            entry.get("size"),
                            ctx=f"hashtable_stat[{idx}].size",
                        )
                        hashtable_entries.append(MemoHashtableStat(depth=depth, size=size))
                    hashtable_stat = hashtable_entries
                    raw_node_stat = _require_list(
                        rec.get("node_stat"), ctx="node_stat"
                    )
                    node_stat_entries: list[MemoNodeStat] = []
                    for idx, entry in enumerate(raw_node_stat):
                        entry = _require_dict(
                            entry, ctx=f"node_stat[{idx}]"
                        )
                        depth = _require_int(
                            entry.get("depth"),
                            ctx=f"node_stat[{idx}].depth",
                        )
                        insert_time = _require_int(
                            entry.get("insert_time"),
                            ctx=f"node_stat[{idx}].insert_time",
                        )
                        node_state = _require_str(
                            entry.get("node_state"),
                            ctx=f"node_stat[{idx}].node_state",
                        )
                        if node_state not in ("stem", "branch"):
                            raise ValueError(f"node_stat[{idx}].node_state must be stem or branch")
                        node_stat_entries.append(
                            MemoNodeStat(
                                depth=depth,
                                insert_time=insert_time,
                                node_state=node_state,
                            )
                        )
                    node_stat = node_stat_entries
                    raw_rule_stat = _require_list(
                        rec.get("rule_stat", []), ctx="rule_stat"
                    )
                    rule_stat_entries: list[MemoRuleStat] = []
                    for idx, entry in enumerate(raw_rule_stat):
                        entry = _require_dict(
                            entry, ctx=f"rule_stat[{idx}]"
                        )
                        size = _require_int(
                            entry.get("size"), ctx=f"rule_stat[{idx}].size"
                        )
                        pvar_length = _require_int(
                            entry.get("pvar_length"), ctx=f"rule_stat[{idx}].pvar_length"
                        )
                        sc = _require_int(
                            entry.get("sc"), ctx=f"rule_stat[{idx}].sc"
                        )
                        hit_count = _require_int(
                            entry.get("hit_count"),
                            ctx=f"rule_stat[{idx}].hit_count",
                        )
                        insert_time = _require_int(
                            entry.get("insert_time"),
                            ctx=f"rule_stat[{idx}].insert_time",
                        )
                        depth = _require_int(
                            entry.get("depth"), ctx=f"rule_stat[{idx}].depth"
                        )
                        rule = _require_str(
                            entry.get("rule"), ctx=f"rule_stat[{idx}].rule"
                        )
                        rule_stat_entries.append(
                            MemoRuleStat(
                                size=size,
                                pvar_length=pvar_length,
                                sc=sc,
                                hit_count=hit_count,
                                insert_time=insert_time,
                                depth=depth,
                                rule=rule,
                            )
                        )
                    rule_stat = rule_stat_entries
                else:
                    raise ValueError(f"unexpected record name: {name}")
            except Exception as exc:  # pylint: disable=broad-except
                raise RuntimeError(f"failed to parse line {line_no}") from exc
    if not exec_times:
        raise RuntimeError("no exec_time records found in file")
    return Result(
        exec_times=exec_times,
        depth_breakdown=depth_breakdown,
        node_stat=node_stat,
        rule_stat=rule_stat,
        node_counts=node_counts,
        hashtable_stat=hashtable_stat,
    )
