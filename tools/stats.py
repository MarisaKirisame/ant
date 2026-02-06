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
    rread_length: int
    reads_size: int
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
                    stem_nodes = rec.get("stem_nodes")
                    branch_nodes = rec.get("branch_nodes")
                    total_nodes = rec.get("total_nodes")
                    if (
                        isinstance(stem_nodes, int)
                        and isinstance(branch_nodes, int)
                        and isinstance(total_nodes, int)
                    ):
                        node_counts = MemoNodeCounts(
                            stem_nodes=stem_nodes,
                            branch_nodes=branch_nodes,
                            total_nodes=total_nodes,
                        )
                    raw_hashtable_stat = rec.get("hashtable_stat", [])
                    if not isinstance(raw_hashtable_stat, list):
                        raise ValueError("hashtable_stat must be a list")
                    hashtable_entries: list[MemoHashtableStat] = []
                    for idx, entry in enumerate(raw_hashtable_stat):
                        if not isinstance(entry, dict):
                            raise ValueError(f"hashtable_stat[{idx}] must be an object")
                        depth = entry.get("depth")
                        size = entry.get("size")
                        if not isinstance(depth, int):
                            raise ValueError(f"hashtable_stat[{idx}].depth must be an int")
                        if not isinstance(size, int):
                            raise ValueError(f"hashtable_stat[{idx}].size must be an int")
                        hashtable_entries.append(MemoHashtableStat(depth=depth, size=size))
                    hashtable_stat = hashtable_entries
                    raw_node_stat = rec.get("node_stat", [])
                    if not isinstance(raw_node_stat, list):
                        raise ValueError("node_stat must be a list")
                    node_stat_entries: list[MemoNodeStat] = []
                    for idx, entry in enumerate(raw_node_stat):
                        if not isinstance(entry, dict):
                            raise ValueError(f"node_stat[{idx}] must be an object")
                        depth = entry.get("depth")
                        rread_length = entry.get("rread_length")
                        reads_size = entry.get("reads_size")
                        insert_time = entry.get("insert_time")
                        node_state = entry.get("node_state")
                        if not isinstance(depth, int):
                            raise ValueError(f"node_stat[{idx}].depth must be an int")
                        if not isinstance(rread_length, int):
                            raise ValueError(f"node_stat[{idx}].rread_length must be an int")
                        if not isinstance(reads_size, int):
                            raise ValueError(f"node_stat[{idx}].reads_size must be an int")
                        if not isinstance(insert_time, int):
                            raise ValueError(f"node_stat[{idx}].insert_time must be an int")
                        if node_state is None:
                            node_state = "unknown"
                        if not isinstance(node_state, str):
                            raise ValueError(f"node_stat[{idx}].node_state must be a string")
                        if node_state not in ("stem", "branch", "unknown"):
                            raise ValueError(f"node_stat[{idx}].node_state must be stem or branch")
                        node_stat_entries.append(
                            MemoNodeStat(
                                depth=depth,
                                rread_length=rread_length,
                                reads_size=reads_size,
                                insert_time=insert_time,
                                node_state=node_state,
                            )
                        )
                    node_stat = node_stat_entries
                    raw_rule_stat = rec.get("rule_stat", [])
                    if not isinstance(raw_rule_stat, list):
                        raise ValueError("rule_stat must be a list")
                    rule_stat_entries: list[MemoRuleStat] = []
                    for idx, entry in enumerate(raw_rule_stat):
                        if not isinstance(entry, dict):
                            raise ValueError(f"rule_stat[{idx}] must be an object")
                        size = entry.get("size")
                        pvar_length = entry.get("pvar_length")
                        sc = entry.get("sc")
                        hit_count = entry.get("hit_count")
                        insert_time = entry.get("insert_time")
                        depth = entry.get("depth")
                        rule = entry.get("rule")
                        if not isinstance(size, int):
                            raise ValueError(f"rule_stat[{idx}].size must be an int")
                        if not isinstance(pvar_length, int):
                            raise ValueError(f"rule_stat[{idx}].pvar_length must be an int")
                        if not isinstance(sc, int):
                            raise ValueError(f"rule_stat[{idx}].sc must be an int")
                        if not isinstance(hit_count, int):
                            raise ValueError(f"rule_stat[{idx}].hit_count must be an int")
                        if not isinstance(insert_time, int):
                            raise ValueError(f"rule_stat[{idx}].insert_time must be an int")
                        if not isinstance(depth, int):
                            raise ValueError(f"rule_stat[{idx}].depth must be an int")
                        if not isinstance(rule, str):
                            raise ValueError(f"rule_stat[{idx}].rule must be a string")
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
