#!/usr/bin/env python3
"""Build and check the Ant mdBook documentation."""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent
SRC = ROOT / "src"
THEME = ROOT / "theme"
TEMPLATE = ROOT / "book.toml.in"
RENDERED = ROOT / ".mdbook"
DEFAULT_OUTPUT = ROOT / "book"
MACRO = r"\Ant"
TEMPLATE_NAME = "{{ANT_NAME}}"
TEMPLATE_BUILD_DIR = "{{BUILD_DIR}}"


class DocError(RuntimeError):
    pass


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Render and validate the Ant mdBook documentation.")
    parser.add_argument("command", choices=("build", "check", "clean", "serve"))
    parser.add_argument("--name", default=None, help="Rendered language/project name. Defaults to ANT_DOC_NAME or ant.")
    parser.add_argument(
        "--dest",
        default=str(DEFAULT_OUTPUT),
        help="mdBook output directory. Defaults to website/book.",
    )
    parser.add_argument(
        "--mdbook",
        default=os.environ.get("MDBOOK", "mdbook"),
        help="mdBook executable. Defaults to MDBOOK or mdbook.",
    )
    parser.add_argument(
        "--skip-mdbook",
        action="store_true",
        help="Run only renderer/static checks. Intended for environments without mdbook installed.",
    )
    parser.add_argument(
        "--host",
        default=os.environ.get("MDBOOK_HOST", "127.0.0.1"),
        help="Host for `serve`. Defaults to MDBOOK_HOST or 127.0.0.1.",
    )
    parser.add_argument(
        "--port",
        default=os.environ.get("MDBOOK_PORT", "3000"),
        help="Port for `serve`. Defaults to MDBOOK_PORT or 3000.",
    )
    parser.add_argument(
        "--open",
        action="store_true",
        help="Open the served book in a browser.",
    )
    return parser.parse_args(argv)


def display_name(args: argparse.Namespace) -> str:
    name = args.name if args.name is not None else os.environ.get("ANT_DOC_NAME", "ant")
    if not name or not name.strip():
        raise DocError("ANT_DOC_NAME/--name must not be empty.")
    return name.strip()


def destination(args: argparse.Namespace) -> Path:
    return Path(args.dest).expanduser().resolve()


def render_text(text: str, name: str) -> str:
    return text.replace(MACRO, name)


def reset_dir(path: Path) -> None:
    if path.exists():
        shutil.rmtree(path)
    path.mkdir(parents=True)


def copy_rendered_tree(src: Path, dst: Path, name: str) -> None:
    for path in src.rglob("*"):
        rel = path.relative_to(src)
        out = dst / rel
        if path.is_dir():
            out.mkdir(parents=True, exist_ok=True)
        elif path.suffix in {".md", ".css", ".js", ".html", ".toml"}:
            out.parent.mkdir(parents=True, exist_ok=True)
            out.write_text(render_text(path.read_text(encoding="utf-8"), name), encoding="utf-8")
        else:
            out.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, out)


def render_book(name: str, out_dir: Path) -> Path:
    if not SRC.exists():
        raise DocError(f"Missing source directory: {SRC}")
    if not TEMPLATE.exists():
        raise DocError(f"Missing mdBook template: {TEMPLATE}")

    reset_dir(RENDERED)
    copy_rendered_tree(SRC, RENDERED / "src", name)
    if THEME.exists():
        copy_rendered_tree(THEME, RENDERED / "theme", name)

    book_toml = TEMPLATE.read_text(encoding="utf-8")
    book_toml = book_toml.replace(TEMPLATE_NAME, name)
    book_toml = book_toml.replace(TEMPLATE_BUILD_DIR, out_dir.as_posix())
    book_toml = render_text(book_toml, name)
    (RENDERED / "book.toml").write_text(book_toml, encoding="utf-8")
    return RENDERED


def source_markdown_files() -> list[Path]:
    return sorted(SRC.rglob("*.md"))


def rendered_markdown_files() -> list[Path]:
    return sorted((RENDERED / "src").rglob("*.md"))


def check_sources_use_macro() -> list[str]:
    issues: list[str] = []
    markdown = source_markdown_files()
    if not markdown:
        issues.append("website/src has no Markdown files.")
        return issues

    combined = "\n".join(path.read_text(encoding="utf-8") for path in markdown)
    if MACRO not in combined:
        issues.append(f"Documentation source should use {MACRO} for the substitutable name.")
    if "ant Language" in combined or "Ant Language" in combined:
        issues.append(f"Use {MACRO} instead of spelling the document title directly in source Markdown.")
    return issues


def check_rendered_macros() -> list[str]:
    issues: list[str] = []
    for path in rendered_markdown_files() + [RENDERED / "book.toml"]:
        text = path.read_text(encoding="utf-8")
        if MACRO in text:
            issues.append(f"Unexpanded {MACRO} macro in {path.relative_to(ROOT)}.")
        if TEMPLATE_NAME in text or TEMPLATE_BUILD_DIR in text:
            issues.append(f"Unexpanded template placeholder in {path.relative_to(ROOT)}.")
    return issues


def markdown_links(text: str) -> list[str]:
    links = re.findall(r"(?<!!)\[[^\]]+\]\(([^)]+)\)", text)
    return [link.split("#", 1)[0] for link in links if link and not re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", link)]


def check_local_links() -> list[str]:
    issues: list[str] = []
    for path in rendered_markdown_files():
        for link in markdown_links(path.read_text(encoding="utf-8")):
            if not link:
                continue
            target = (path.parent / link).resolve()
            try:
                target.relative_to(RENDERED.resolve())
            except ValueError:
                issues.append(f"{path.relative_to(RENDERED)} links outside the rendered book: {link}")
                continue
            if not target.exists():
                issues.append(f"{path.relative_to(RENDERED)} has missing link target: {link}")
    return issues


def check_summary() -> list[str]:
    summary = RENDERED / "src" / "SUMMARY.md"
    if not summary.exists():
        return ["Missing src/SUMMARY.md."]
    text = summary.read_text(encoding="utf-8")
    links = [link for link in markdown_links(text) if link]
    if not links:
        return ["SUMMARY.md has no chapter links."]
    return []


def run_static_checks() -> None:
    issues = []
    issues.extend(check_sources_use_macro())
    issues.extend(check_rendered_macros())
    issues.extend(check_summary())
    issues.extend(check_local_links())
    if issues:
        raise DocError("Documentation checks failed:\n" + "\n".join(f"- {issue}" for issue in issues))


def run_mdbook(args: list[str], mdbook: str) -> None:
    if shutil.which(mdbook) is None:
        raise DocError(
            f"Could not find mdBook executable '{mdbook}'. Install mdBook or set MDBOOK=/path/to/mdbook."
        )
    subprocess.run([mdbook, *args], cwd=RENDERED, check=True)


def ensure_clean_target(path: Path) -> None:
    root = ROOT.resolve()
    try:
        path.relative_to(root)
    except ValueError as exc:
        raise DocError(f"Refusing to clean path outside website/: {path}") from exc


def command_build(args: argparse.Namespace) -> None:
    name = display_name(args)
    out_dir = destination(args)
    render_book(name, out_dir)
    if not args.skip_mdbook:
        run_mdbook(["build"], args.mdbook)


def command_check(args: argparse.Namespace) -> None:
    name = display_name(args)
    out_dir = destination(args)
    render_book(name, out_dir)
    run_static_checks()
    if not args.skip_mdbook:
        run_mdbook(["build"], args.mdbook)


def command_clean(args: argparse.Namespace) -> None:
    out_dir = destination(args)
    ensure_clean_target(out_dir)
    for path in (RENDERED, out_dir):
        if path.exists():
            shutil.rmtree(path)


def command_serve(args: argparse.Namespace) -> None:
    name = display_name(args)
    out_dir = destination(args)
    render_book(name, out_dir)
    run_static_checks()
    serve_args = ["serve", "--hostname", args.host, "--port", args.port]
    if args.open:
        serve_args.append("--open")
    print(f"Serving documentation at http://{args.host}:{args.port}/", flush=True)
    run_mdbook(serve_args, args.mdbook)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    try:
        if args.command == "build":
            command_build(args)
        elif args.command == "check":
            command_check(args)
        elif args.command == "clean":
            command_clean(args)
        elif args.command == "serve":
            command_serve(args)
        else:
            raise AssertionError(args.command)
    except subprocess.CalledProcessError as exc:
        print(f"Command failed with exit code {exc.returncode}: {' '.join(exc.cmd)}", file=sys.stderr)
        return exc.returncode
    except DocError as exc:
        print(str(exc), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
