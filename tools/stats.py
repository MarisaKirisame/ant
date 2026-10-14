#!/usr/bin/env python3
"""Parsing helpers and data structures for speedup stats.

Each exec_time record is expected to include both step counts and profiles.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, replace
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
    memo_heap_words: int | None
    cek_heap_words: int | None
    memo_resting_adjusted_live_words: int | None
    cek_resting_adjusted_live_words: int | None
    input_size: int | None = None
    trace_exec_index: int | None = None
    trace_source_indices: list[int] | None = None
    trace_source_exercise_ids: list[int | None] | None = None
    cache_eviction: bool = False


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


def _optional_int(value: object, *, ctx: str) -> int | None:
    if value is None:
        return None
    return _require_int(value, ctx=ctx)


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
                    if seen_memo_stats:
                        raise ValueError("exec_time record appears after memo_stats")
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
                    memo_heap_words = _optional_int(
                        rec.get("memo_heap_words"), ctx="exec_time.memo_heap_words"
                    )
                    cek_heap_words = _optional_int(
                        rec.get("cek_heap_words"), ctx="exec_time.cek_heap_words"
                    )
                    memo_resting_adjusted_live_words = _optional_int(
                        rec.get("memo_resting_adjusted_live_words"),
                        ctx="exec_time.memo_resting_adjusted_live_words",
                    )
                    cek_resting_adjusted_live_words = _optional_int(
                        rec.get("cek_resting_adjusted_live_words"),
                        ctx="exec_time.cek_resting_adjusted_live_words",
                    )
                    exec_times.append(
                        ExecTimeRecord(
                            step=step,
                            without_memo_step=without_memo_step,
                            memo_profile=memo_profile,
                            cek_profile=cek_profile,
                            plain_profile=plain_profile,
                            memo_heap_words=memo_heap_words,
                            cek_heap_words=cek_heap_words,
                            memo_resting_adjusted_live_words=memo_resting_adjusted_live_words,
                            cek_resting_adjusted_live_words=cek_resting_adjusted_live_words,
                            input_size=rec.get("input_size"),
                            trace_exec_index=rec.get("trace_exec_index"),
                            trace_source_indices=rec.get("trace_source_indices"),
                            trace_source_exercise_ids=rec.get("trace_source_exercise_ids"),
                            cache_eviction=rec.get("cache_eviction", False),
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


def load_trace(path: Path, *, eviction: bool) -> Result:
    result = load_records(path)
    if result.node_counts is None:
        raise ValueError(f"{path}: incomplete experiment (missing memo_stats)")
    size = result.exec_times[0].input_size
    for index, row in enumerate(result.exec_times):
        if (type(size) is not int or size <= 0 or row.input_size != size
                or row.trace_exec_index != index or not row.trace_source_indices
                or row.trace_source_exercise_ids is None
                or len(row.trace_source_indices) != len(row.trace_source_exercise_ids)):
            raise ValueError(f"{path}: invalid or missing trace metadata at execution {index}")
        if row.cache_eviction is not eviction:
            raise ValueError(f"{path}: expected cache_eviction={eviction}")
    return result


def aligned_cache_results(cache_path: Path, reference_path: Path) -> tuple[Result, Result]:
    cached = load_trace(cache_path, eviction=True)
    reference = load_trace(reference_path, eviction=False)
    fields = ("input_size", "trace_exec_index", "trace_source_indices", "trace_source_exercise_ids")
    count = len(cached.exec_times)
    if count > len(reference.exec_times) or any(
        getattr(new, field) != getattr(old, field)
        for new, old in zip(cached.exec_times, reference.exec_times) for field in fields
    ):
        raise ValueError(f"{cache_path} does not align with {reference_path}")
    # A smoke run may reuse just the first execution of each reference trace.
    return cached, replace(reference, exec_times=reference.exec_times[:count])


def peak_memory_overhead(ours: Result, baseline: Result, *, baseline_key: str = "cek_resting_adjusted_live_words") -> float | None:
    """Ratio of peak live memory over the same executions on each side."""
    values = [
        (new.memo_resting_adjusted_live_words, getattr(old, baseline_key))
        for new, old in zip(ours.exec_times, baseline.exec_times, strict=True)
    ]
    measured = [(new, old) for new, old in values if new is not None and old is not None and new > 0 and old > 0]
    if not measured:
        return None
    return max(new for new, _ in measured) / max(old for _, old in measured)
