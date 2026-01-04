from dominate import tags as tag

counter = 0


def fresh() -> int:
    global counter
    ret = counter
    counter += 1
    return ret


def fmt_speedup(value: float) -> str:
    """Format a speedup value with sensible precision for very small numbers."""
    return f"{value:.4g}"


def stat_card(label: str, value: str) -> None:
    with tag.div(cls="stat"):
        tag.span(label, cls="label")
        tag.span(value, cls="value")
