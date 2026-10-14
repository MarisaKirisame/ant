from dominate import tags as tag

counter = 0

CHORDATA = "Chordata"
CHORDATA_CACHE = "Chordata (eviction)"
BASELINE = "Baseline"
HAZEL = "Hazel"
PLAIN_OCAML = "Plain OCaml"
IMPLEMENTATION_LABELS = {
    "memo_profile": CHORDATA,
    "cek_profile": BASELINE,
    "plain_profile": PLAIN_OCAML,
}


def comparison_title(metric: str, *, ours: str = CHORDATA, baseline: str = BASELINE, context: str | None = None) -> str:
    title = f"{ours} vs {baseline}: {metric}"
    return f"{title}\n{context}" if context else title


def speedup_label(*, ours: str = CHORDATA, baseline: str = BASELINE) -> str:
    return f"Speedup ({baseline} / {ours})"


def fresh() -> int:
    global counter
    ret = counter
    counter += 1
    return ret


def fmt_number(value: float) -> str:
    """Format report measurements to two significant digits, retaining zeros."""
    return f"{value:#.2g}".removesuffix(".")


def fmt_speedup(value: float) -> str:
    return fmt_number(value)


def stat_card(label: str, value: str) -> None:
    with tag.div(cls="stat"):
        tag.span(label, cls="label")
        tag.span(value, cls="value")
