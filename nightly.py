#!/usr/bin/env python3
"""Project maintenance entrypoint replicated from the old nightly.sh."""

from __future__ import annotations

import glob
import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Iterable, Mapping, MutableMapping, Optional


REPO_ROOT = Path(__file__).resolve().parent


def _normalize_switch(switch: str) -> str:
    if switch in (".", "..") or os.sep in switch:
        return str(Path(switch).resolve())
    return switch


def _detect_local_switch() -> Optional[str]:
    if (REPO_ROOT / "_opam").exists() or (REPO_ROOT / ".opam-switch").exists():
        return str(REPO_ROOT)
    return None


def _resolve_switch() -> str:
    env_switch = os.environ.get("OPAM_SWITCH")
    if env_switch:
        return _normalize_switch(env_switch)
    local_switch = _detect_local_switch()
    if local_switch:
        return local_switch
    return "ant"


SWITCH = _resolve_switch()
PACKAGES = [
    "core",
    "dune",
    "menhir",
    "ppx_deriving",
    "ppx_sexp_conv",
    "yojson",
    "core_unix",
    "batteries",
    "pprint",
    "cmdliner",
    "core_bench",
]

TOOLS_DIR = Path(__file__).resolve().parent / "tools"
if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

import generate_report as report_module  # noqa: E402


def run(
    command: Iterable[str],
    *,
    env: Optional[Mapping[str, str]] = None,
    check: bool = True,
    silent: bool = False,
    capture: bool = False,
) -> subprocess.CompletedProcess[str]:
    """Execute a shell command, optionally silencing or capturing output."""

    cmd_list = list(command)
    if capture:
        stdout = subprocess.PIPE
        stderr = subprocess.PIPE
    elif silent:
        stdout = subprocess.DEVNULL
        stderr = subprocess.DEVNULL
    else:
        stdout = None
        stderr = None
    result = subprocess.run(
        cmd_list,
        env=env,
        check=False,
        stdout=stdout,
        stderr=stderr,
        text=True,
    )

    if check and result.returncode != 0:
        pretty = " ".join(shlex.quote(part) for part in cmd_list)
        raise subprocess.CalledProcessError(result.returncode, pretty, result.stdout, result.stderr)

    return result


def _switch_exists(switch: str) -> bool:
    result = run(
        ["opam", "switch", "list", "--short"],
        check=False,
        capture=True,
    )
    if result.returncode != 0 or not result.stdout:
        return False
    target = _normalize_switch(switch)
    for line in result.stdout.splitlines():
        name = line.strip()
        if not name:
            continue
        if _normalize_switch(name) == target:
            return True
    return False


def ensure_switch() -> None:
    """Ensure the opam switch exists and enforces the invariant."""

    if not _switch_exists(SWITCH):
        run(["opam", "switch", "create", SWITCH, "--empty"])
    run(
        [
            "opam",
            "switch",
            "set-invariant",
            "--switch",
            SWITCH,
            "--update-invariant",
            "ocaml>=5.2",
            "-y",
        ]
    )


def opam_exec(
    args: Iterable[str], *, env: Optional[Mapping[str, str]] = None, **kwargs
) -> None:
    """Run a command inside the configured opam switch."""

    run(["opam", "exec", "--switch", SWITCH, "--", *args], env=env, **kwargs)


def install_dependencies() -> None:
    ensure_switch()
    run(["opam", "update"])
    run(["opam", "upgrade", "--switch", SWITCH, "--fixup", "-y"])
    run(["opam", "install", "--switch", SWITCH, "-y", *PACKAGES])


def build_project() -> None:
    ensure_switch()
    env = _opam_env_with_ocamlrunparam()
    opam_exec(["dune", "build"], env=env)


def generate_ml_files(env: Optional[Mapping[str, str]] = None) -> None:
    ensure_switch()
    opam_exec(
        [
            "dune",
            "exec",
            "ant",
            "--",
            "examples/Test.ant",
            "generated/TestSeq.ml",
            "--compile",
            "--backend",
            "seq",
        ],
        env=env,
    )
    opam_exec(
        [
            "dune",
            "exec",
            "ant",
            "--",
            "examples/Test.ant",
            "generated/TestCEK.ml",
            "--compile",
            "--backend",
            "memo",
        ],
        env=env,
    )
    opam_exec(
        [
            "dune",
            "exec",
            "ant",
            "--",
            "examples/Test.ant",
            "generated/TestPlain.ml",
            "--compile",
            "--backend",
            "plain",
        ],
        env=env,
    )
    #opam_exec(
    #    [
    #        "dune",
    #        "exec",
    #        "ant",
    #        "--",
    #        "examples/Live.ant",
    #        "generated/LiveSeq.ml",
    #        "--compile",
    #        "--backend",
    #        "seq",
    #    ],
    #    env=env,
    #)
    opam_exec(
        [
            "dune",
            "exec",
            "ant",
            "--",
            "examples/Live.ant",
            "generated/LiveCEK.ml",
            "--compile",
            "--backend",
            "memo",
        ],
        env=env,
    )
    opam_exec(
        [
            "dune",
            "exec",
            "ant",
            "--",
            "examples/Live.ant",
            "generated/LivePlain.ml",
            "--compile",
            "--backend",
            "plain",
            "--type-alias",
            "LiveCEK",
        ],
        env=env,
    )
    opam_exec(
        [
            "dune",
            "exec",
            "ant",
            "--",
            "examples/TailRec.ant",
            "generated/TailRecCEK.ml",
            "--compile",
            "--backend",
            "memo",
        ],
        env=env,
    )


def run_project() -> None:
    ensure_switch()
    env = _opam_env_with_ocamlrunparam()
    generate_ml_files(env=env)
    opam_exec(["dune", "fmt"], env=env, check=False, silent=True)
    for mode in ("map",):
        opam_exec(["dune", "exec", "GeneratedMain", mode], env=env)


def profile_project() -> None:
    _remove_perf_data_files()
    ensure_switch()
    env = _opam_env_with_ocamlrunparam()
    generate_ml_files(env=env)
    opam_exec(["dune", "build", "generated/GeneratedMain.exe"], env=env)
    binary = os.path.join("_build", "default", "generated", "GeneratedMain.exe")
    modes = ("live-simple", "live-list-extend", "live-left-to-right", "live-demand-driven", "hazel", "tailrec")
    for mode in modes:
        if sys.platform == "darwin":
            # On macOS, use xctrace (modern replacement for instruments CLI)
            opam_exec(
                ["xctrace", "record", "--template", "Time Profiler", "--output", f"perf-{mode}.trace", "--launch", "--", binary, mode],
                env=env,
            )
        else:
            opam_exec(
                ["perf", "record", "-o", f"perf-{mode}.data", "--", binary, mode],
                env=env,
            )


def report_project() -> None:
    run_project()
    report_module.generate_reports()




def compile_generated() -> None:
    ensure_switch()
    env = _opam_env_with_ocamlrunparam()
    generate_ml_files(env=env)


def _opam_env_with_ocamlrunparam() -> MutableMapping[str, str]:
    env: MutableMapping[str, str] = os.environ.copy()
    env["OCAMLRUNPARAM"] = "b"
    env["DUNE_PROFILE"] = "release"
    return env


def _remove_perf_data_files() -> None:
    patterns = glob.glob("perf-*.data") + glob.glob("perf-*.data.old") + glob.glob("perf-*.trace")
    for path in patterns:
        try:
            if os.path.isdir(path):
                shutil.rmtree(path)
            else:
                os.remove(path)
        except FileNotFoundError:
            continue


def main(argv: Iterable[str]) -> int:
    args = list(argv)
    if len(args) > 1:
        print("Only a single stage argument is supported.", file=sys.stderr)
        print(
            "Usage: nightly.py [dependency|build|run|profile|report|compile-generated|all]",
            file=sys.stderr,
        )
        return 1

    stage = args[0] if args else "all"

    if stage == "dependency":
        install_dependencies()
    elif stage == "build":
        build_project()
    elif stage == "run":
        run_project()
    elif stage == "profile":
        profile_project()
    elif stage == "report":
        report_project()
    elif stage == "compile-generated":
        compile_generated()
    elif stage == "all":
        install_dependencies()
        build_project()
        run_project()
        report_module.generate_reports()
    else:
        print(f"Unknown stage: {stage}", file=sys.stderr)
        print(
            "Usage: nightly.py [dependency|build|run|profile|report|compile-generated|all]",
            file=sys.stderr,
        )
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
