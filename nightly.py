#!/usr/bin/env python3
"""Project maintenance entrypoint replicated from the old nightly.sh."""

from __future__ import annotations

import os
import shlex
import subprocess
import sys
from typing import Iterable, Mapping, MutableMapping, Optional


SWITCH = os.environ.get("OPAM_SWITCH", "ant")
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


def run(
    command: Iterable[str],
    *,
    env: Optional[Mapping[str, str]] = None,
    check: bool = True,
    silent: bool = False,
) -> subprocess.CompletedProcess[str]:
    """Execute a shell command, optionally silencing output."""

    cmd_list = list(command)
    stdout = subprocess.DEVNULL if silent else None
    stderr = subprocess.DEVNULL if silent else None
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


def ensure_switch() -> None:
    """Ensure the opam switch exists and enforces the invariant."""

    run(["opam", "switch", "create", SWITCH, "--empty"], check=False)
    run(["opam", "switch", SWITCH])
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
    opam_exec(["dune", "build"])


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
            "--print-ant",
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
            "--cek",
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
    #        "--print-ant",
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
            "--cek",
        ],
        env=env,
    )


def run_project() -> None:
    ensure_switch()
    env = _opam_env_with_ocamlrunparam()
    generate_ml_files(env=env)
    opam_exec(["dune", "fmt"], env=env, check=False, silent=True)
    opam_exec(["dune", "exec", "GeneratedMain", "live"], env=env)


def compile_generated() -> None:
    ensure_switch()
    env = _opam_env_with_ocamlrunparam()
    generate_ml_files(env=env)


def _opam_env_with_ocamlrunparam() -> MutableMapping[str, str]:
    env: MutableMapping[str, str] = os.environ.copy()
    env["OCAMLRUNPARAM"] = "b"
    return env


def main(argv: Iterable[str]) -> int:
    args = list(argv)
    if len(args) > 1:
        print("Only a single stage argument is supported.", file=sys.stderr)
        print(
            "Usage: nightly.py [dependency|build|run|compile-generated|all]",
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
    elif stage == "compile-generated":
        compile_generated()
    elif stage == "all":
        install_dependencies()
        build_project()
        run_project()
    else:
        print(f"Unknown stage: {stage}", file=sys.stderr)
        print(
            "Usage: nightly.py [dependency|build|run|compile-generated|all]",
            file=sys.stderr,
        )
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
