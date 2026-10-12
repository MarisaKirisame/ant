from dominate import tags as tag

counter = 0


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
