from dominate import tags as tag

counter = 0


def fresh() -> int:
    global counter
    ret = counter
    counter += 1
    return ret


def fmt_speedup(value: float) -> str:
    """Format a ratio with one decimal place for reports and paper output."""
    return f"{value:.1f}"


def stat_card(label: str, value: str) -> None:
    with tag.div(cls="stat"):
        tag.span(label, cls="label")
        tag.span(value, cls="value")
