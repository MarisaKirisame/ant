#!/usr/bin/env python3
"""Project maintenance entrypoint replicated from the old nightly.sh."""

from __future__ import annotations

import dataclasses
import glob
import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Iterable, Mapping, Optional


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
HAZEL_SWITCH = _normalize_switch(os.environ.get("ANT_HAZEL_OPAM_SWITCH", "ant-hazel"))
OPAM_ARCHIVE_REPOSITORY_URL = "git+https://github.com/ocaml/opam-repository-archive"
TOOLCHAIN_PACKAGES = [
    "dune>=3.24.0",
]
DEV_PACKAGES = [
    "bisect_ppx=2.8.3",
    "ocaml-lsp-server",
    "ocamlformat",
]

TOOLS_DIR = Path(__file__).resolve().parent / "tools"
if str(TOOLS_DIR) not in sys.path:
    sys.path.insert(0, str(TOOLS_DIR))

import generate_report as report_module  # noqa: E402
import coverage_comment  # noqa: E402


@dataclasses.dataclass(frozen=True)
class RunOptions:
    smoke: bool = False


RUN_OPTIONS = RunOptions()


def _smoke_run() -> bool:
    return RUN_OPTIONS.smoke


def run(
    command: Iterable[str],
    *,
    env: Optional[Mapping[str, str]] = None,
    cwd: Optional[Path] = None,
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
        cwd=cwd,
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


def ensure_opam_switch(switch: str) -> None:
    """Ensure an opam switch exists and enforces the project OCaml invariant."""

    if not _switch_exists(switch):
        run(["opam", "switch", "create", switch, "--empty"])
    run(
        [
            "opam",
            "switch",
            "set-invariant",
            "--switch",
            switch,
            "--update-invariant",
            "ocaml>=5.2",
            "-y",
        ]
    )


def ensure_switch() -> None:
    ensure_opam_switch(SWITCH)


def opam_exec_in_switch(
    switch: str, args: Iterable[str], *, env: Optional[Mapping[str, str]] = None, **kwargs
) -> subprocess.CompletedProcess[str]:
    return run(["opam", "exec", "--switch", switch, "--", *args], env=env, **kwargs)


def opam_exec(
    args: Iterable[str], *, env: Optional[Mapping[str, str]] = None, **kwargs
) -> subprocess.CompletedProcess[str]:
    """Run a command inside the configured opam switch."""

    return opam_exec_in_switch(SWITCH, args, env=env, **kwargs)


def ensure_opam_archive_repository(switch: str) -> None:
    result = run(["opam", "repository", "list", "--switch", switch, "--short"], capture=True)
    repositories = {line.strip() for line in result.stdout.splitlines() if line.strip()}
    if "archive" not in repositories:
        run(
            [
                "opam",
                "repository",
                "add",
                "--switch",
                switch,
                "--rank=-1",
                "archive",
                OPAM_ARCHIVE_REPOSITORY_URL,
                "-y",
            ]
        )


def install_dependencies() -> None:
    ensure_switch()
    run(["opam", "update"])
    run(["opam", "upgrade", "--switch", SWITCH, "--fixup", "-y"])
    run(["opam", "install", "--switch", SWITCH, "-y", *TOOLCHAIN_PACKAGES])
    run(["opam", "install", "--switch", SWITCH, "-y", *DEV_PACKAGES])
    opam_exec(["dune", "pkg", "lock"])


def hazel_dependency() -> None:
    ensure_opam_switch(HAZEL_SWITCH)
    run(["git", "submodule", "update", "--init", "--recursive", "hazel"])
    ensure_opam_archive_repository(HAZEL_SWITCH)
    run(["opam", "install", "--switch", HAZEL_SWITCH, "-y", "--deps-only", "--locked", "."], cwd=REPO_ROOT / "hazel")
    opam_exec_in_switch(
        HAZEL_SWITCH,
        ["dune", "build", "src/CLI/cli.bc.js", "--profile", "dev"],
        cwd=REPO_ROOT / "hazel",
    )


def build_project() -> None:
    ensure_switch()
    opam_exec(["dune", "build", "--profile", "release"])


def generate_ml_files() -> None:
    ensure_switch()
    files = ("Live", "Arith", "List")
    backends = (("memo", "CEK"), ("plain", "Plain"))

    for file in files:
        for backend, suffix in backends:
            command = [
                "dune",
                "exec",
                "--profile",
                "release",
                "ant",
                "--",
                f"examples/{file}.ant",
                f"generated/{file}{suffix}.ml",
                "--compile",
                "--backend",
                backend,
            ]
            if backend == "plain":
                command.extend(["--type-alias", f"{file}CEK"])
            opam_exec(command)

base_modes = ("append", "filter", "map", "qs", "is", "ms", "pair", "rev")
variant_prefixes = ("", "th_", "at_")
hazel_modes = tuple(
    f"{prefix}{mode}" if prefix else mode
    for prefix in variant_prefixes
    for mode in base_modes
)
hazel_bad = set(["th_ms", "th_pair"])
hazel_modes = tuple(m for m in hazel_modes if m not in hazel_bad)
arith_modes = ("arith",)
modes = arith_modes + hazel_modes
default_scaling_sizes = (10, 20, 40, 100, 200, 400)
smoke_scaling_sizes = (2,)
default_entropy_scaling_sizes = (10, 20, 40, 100, 200, 400, 1000, 2000, 4000)
smoke_entropy_scaling_sizes = (2,)
smoke_input_size = 2
default_arith_scaling_sample_count = 5
smoke_arith_sample_count = 1
smoke_hazel_max_candidates = 1
smoke_hazel_timeout_seconds = 5
hazel_compare_input_size = 10
hazel_compare_mode_timeout = "10m"


def _scaling_sizes() -> tuple[int, ...]:
    return smoke_scaling_sizes if _smoke_run() else default_scaling_sizes


def _entropy_scaling_sizes() -> tuple[int, ...]:
    return smoke_entropy_scaling_sizes if _smoke_run() else default_entropy_scaling_sizes


def _result_path_for_mode(mode: str) -> Path:
    if mode == "arith":
        return Path("results/arith/arith.json")
    return Path("results/hazel") / f"{mode}.json"


def _compare_result_path_for_mode(mode: str) -> Path:
    return Path("results/hazel-compare") / f"{mode}.json"


def _no_evict_result_path_for_mode(mode: str) -> Path:
    return Path("results/hazel-no-evict") / f"{mode}.json"


def _remove_result_files_for_modes(selected_modes: Iterable[str]) -> None:
    for mode in selected_modes:
        _result_path_for_mode(mode).unlink(missing_ok=True)


def _remove_compare_result_files_for_modes(selected_modes: Iterable[str]) -> None:
    for mode in selected_modes:
        _compare_result_path_for_mode(mode).unlink(missing_ok=True)


def _remove_no_evict_result_files_for_modes(selected_modes: Iterable[str]) -> None:
    for mode in selected_modes:
        _no_evict_result_path_for_mode(mode).unlink(missing_ok=True)


def run_modes(selected_modes: tuple[str, ...]) -> None:
    _remove_result_files_for_modes(selected_modes)
    ensure_switch()
    generate_ml_files()
    opam_exec(["dune", "fmt"], check=False, silent=True)
    for mode in selected_modes:
        print(f"Running {mode}...", flush=True)
        if _smoke_run() and mode == "arith":
            opam_exec(
                [
                    "dune",
                    "exec",
                    "--profile",
                    "release",
                    "GeneratedMain",
                    "--",
                    "arith",
                    str(smoke_input_size),
                    str(smoke_arith_sample_count),
                    str(_result_path_for_mode(mode)),
                ]
            )
        elif _smoke_run():
            opam_exec(
                [
                    "dune",
                    "exec",
                    "--profile",
                    "release",
                    "GeneratedMain",
                    "--",
                    mode,
                    str(smoke_input_size),
                    str(smoke_hazel_max_candidates),
                ]
            )
        else:
            opam_exec(["dune", "exec", "--profile", "release", "GeneratedMain", mode])


def run_no_evict_modes(selected_modes: tuple[str, ...]) -> None:
    _remove_no_evict_result_files_for_modes(selected_modes)
    ensure_switch()
    generate_ml_files()
    opam_exec(["dune", "fmt"], check=False, silent=True)
    for mode in selected_modes:
        print(f"Running {mode} no-evict...", flush=True)
        if _smoke_run():
            opam_exec(
                [
                    "dune",
                    "exec",
                    "--profile",
                    "release",
                    "GeneratedMain",
                    "--",
                    "hazel-no-evict",
                    mode,
                    str(smoke_input_size),
                    str(smoke_hazel_max_candidates),
                    str(_no_evict_result_path_for_mode(mode)),
                ]
            )
        else:
            opam_exec(
                [
                    "dune",
                    "exec",
                    "--profile",
                    "release",
                    "GeneratedMain",
                    "--",
                    "hazel-no-evict",
                    mode,
                    str(_no_evict_result_path_for_mode(mode)),
                ]
            )


def run_compare_modes(selected_modes: tuple[str, ...]) -> None:
    _remove_compare_result_files_for_modes(selected_modes)
    ensure_switch()
    generate_ml_files()
    opam_exec(["dune", "fmt"], check=False, silent=True)
    for mode in selected_modes:
        print(f"Running {mode} compare...", flush=True)
        command = [
            "timeout",
            "--kill-after=5s",
            hazel_compare_mode_timeout,
            "dune",
            "exec",
            "--profile",
            "release",
            "GeneratedMain",
            "--",
            "hazel-compare",
            mode,
        ]
        if _smoke_run():
            command.extend([str(smoke_input_size), str(smoke_hazel_max_candidates), str(smoke_hazel_timeout_seconds)])
        else:
            command.append(str(hazel_compare_input_size))
        command.append(str(_compare_result_path_for_mode(mode)))
        result = opam_exec(command, check=False)
        if result.returncode != 0:
            _compare_result_path_for_mode(mode).unlink(missing_ok=True)
            status = "timed out" if result.returncode in (124, 137) else f"failed with exit {result.returncode}"
            print(f"Running {mode} compare... {status}; continuing", flush=True)


def run_project() -> None:
    run_modes(modes)


def hazel_project() -> None:
    run_modes(hazel_modes)


def _hazel_experiment_modes() -> tuple[str, ...]:
    return hazel_modes


def _hazel_compare_experiment_modes() -> tuple[str, ...]:
    return hazel_modes


def hazel_experiment_project(*, generate_report: bool = True) -> None:
    selected_modes = _hazel_experiment_modes()
    compare_modes = _hazel_compare_experiment_modes()
    # Prevent stale rows from non-selected benchmarks from appearing in reports.
    _remove_result_files_for_modes(hazel_modes)
    run_modes(selected_modes)
    run_compare_modes(compare_modes)
    if generate_report:
        report_module.generate_hazel_reports(include_hazel_compare=True, modes=selected_modes, hazel_compare_modes=compare_modes)


def hazel_no_evict_project(*, generate_report: bool = True) -> None:
    selected_modes = _hazel_experiment_modes()
    _remove_no_evict_result_files_for_modes(hazel_modes)
    run_no_evict_modes(selected_modes)
    if generate_report:
        report_module.generate_hazel_no_evict_reports(modes=selected_modes)


def arith_project() -> None:
    run_modes(arith_modes)


def _prepare_scaling_run() -> None:
    ensure_switch()
    generate_ml_files()
    opam_exec(["dune", "fmt"], check=False, silent=True)


def arith_scaling_project() -> None:
    _prepare_scaling_run()
    output_dir = Path("results/arith")
    output_dir.mkdir(parents=True, exist_ok=True)
    for size in _scaling_sizes():
        output_path = output_dir / f"{size}.json"
        output_path.unlink(missing_ok=True)
        command = ["dune", "exec", "--profile", "release", "GeneratedMain", "--", "arith-scaling", str(size)]
        sample_count = smoke_arith_sample_count if _smoke_run() else default_arith_scaling_sample_count
        command.append(str(sample_count))
        command.append(str(output_path))
        opam_exec(command)


def hazel_scaling_project() -> None:
    _prepare_scaling_run()
    for mode in hazel_modes:
        output_dir = Path("results/hazel") / mode
        output_dir.mkdir(parents=True, exist_ok=True)
        for size in _scaling_sizes():
            output_path = output_dir / f"{size}.json"
            output_path.unlink(missing_ok=True)
            print(f"Running {mode} scaling size {size}...", flush=True)
            command = ["dune", "exec", "--profile", "release", "GeneratedMain", "--", "hazel-scaling", mode, str(size)]
            if _smoke_run():
                command.append(str(smoke_hazel_max_candidates))
            command.append(str(output_path))
            opam_exec(command)


def hazel_no_evict_scaling_project() -> None:
    _prepare_scaling_run()
    for mode in hazel_modes:
        output_dir = Path("results/hazel-no-evict") / mode
        output_dir.mkdir(parents=True, exist_ok=True)
        for size in _scaling_sizes():
            output_path = output_dir / f"{size}.json"
            output_path.unlink(missing_ok=True)
            print(f"Running {mode} no-evict scaling size {size}...", flush=True)
            command = ["dune", "exec", "--profile", "release", "GeneratedMain", "--", "hazel-scaling-no-evict", mode, str(size)]
            if _smoke_run():
                command.append(str(smoke_hazel_max_candidates))
            command.append(str(output_path))
            opam_exec(command)


def scaling_project() -> None:
    arith_scaling_project()
    hazel_scaling_project()
    hazel_no_evict_scaling_project()
    report_module.generate_scaling_reports(modes=hazel_modes, sizes=_scaling_sizes())


def scaling_report_project() -> None:
    report_module.generate_scaling_reports(modes=hazel_modes, sizes=_scaling_sizes())


def entropy_scaling_project() -> None:
    _prepare_scaling_run()
    input_kinds = ("random", "block", "mod1", "same")
    programs = (
        "map",
        "append",
        # "insertion_sort",
        "merge_sort",
        "quick_sort",
        # "reverse",
        "simple_filter",
        "pair",
    )
    for program in programs:
        for input_kind in input_kinds:
            (Path("results/entropy") / program / input_kind).mkdir(parents=True, exist_ok=True)
    for size in _entropy_scaling_sizes():
        for program in programs:
            for input_kind in input_kinds:
                (Path("results/entropy") / program / input_kind / f"{size}.json").unlink(missing_ok=True)
        opam_exec(["dune", "exec", "--profile", "release", "GeneratedMain", "--", "entropy-scaling", str(size)])
    entropy_report_project()


def entropy_report_project() -> None:
    report_module.generate_entropy_scaling_report(sizes=_entropy_scaling_sizes())
    if Path("results/arith").exists() and Path("results/hazel").exists():
        report_module.generate_scaling_reports(modes=hazel_modes, sizes=_scaling_sizes())


def profile_project() -> None:
    _remove_perf_data_files()
    ensure_switch()
    generate_ml_files()
    opam_exec(["dune", "build", "--profile", "release", "bin/GeneratedMain.exe"])
    binary = os.path.join("_build", "default", "bin", "GeneratedMain.exe")
    for mode in modes:
        if sys.platform == "darwin":
            # On macOS, use xctrace (modern replacement for instruments CLI)
            opam_exec(
                ["xctrace", "record", "--template", "Time Profiler", "--output", f"perf-{mode}.trace", "--launch", "--", binary, mode]
            )
        else:
            opam_exec(["perf", "record", "-o", f"perf-{mode}.data", "--", binary, mode])


def report_project() -> None:
    clean_output()
    report_module.generate_reports()


def hazel_report_project() -> None:
    clean_output()
    report_module.generate_hazel_reports()


def arith_report_project() -> None:
    clean_output()
    report_module.generate_arith_reports()


def experiment_project() -> None:
    clean_results()
    clean_output()
    arith_project()
    hazel_experiment_project(generate_report=False)
    hazel_no_evict_project(generate_report=False)
    report_module.generate_reports()


def hazel_tex_project() -> None:
    report_module.generate_tex_table(output_path=Path("output/hazel/hazel_result.tex"))


def arith_tex_project() -> None:
    report_module.generate_arith_tex(output_path=Path("output/arith/arith_result.tex"))


def compile_generated() -> None:
    ensure_switch()
    generate_ml_files()


def coverage_project() -> None:
    ensure_switch()
    coverage_dir = REPO_ROOT / "_coverage"
    shutil.rmtree(coverage_dir, ignore_errors=True)
    coverage_dir.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    env["BISECT_FILE"] = str(coverage_dir / "bisect")

    opam_exec(
        [
            "dune",
            "runtest",
            "--ignore-lock-dir",
            "--instrument-with",
            "bisect_ppx",
            "--force",
        ],
        env=env,
    )

    summary = opam_exec(
        ["bisect-ppx-report", "summary", "--coverage-path", str(coverage_dir)],
        capture=True,
    ).stdout
    per_file = opam_exec(
        ["bisect-ppx-report", "summary", "--coverage-path", str(coverage_dir), "--per-file"],
        capture=True,
    ).stdout

    summary_path = coverage_dir / "coverage-summary.txt"
    per_file_path = coverage_dir / "coverage-per-file.txt"
    comment_path = coverage_dir / "coverage-comment.md"
    summary_path.write_text(summary, encoding="utf-8")
    per_file_path.write_text(per_file, encoding="utf-8")
    comment_path.write_text(
        coverage_comment.render_comment(summary, per_file),
        encoding="utf-8",
    )


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


def _remove_result_files() -> None:
    for path in [Path("results/arith/arith.json"), *Path("results/hazel").glob("*.json")]:
        path.unlink(missing_ok=True)


def clean_results() -> None:
    shutil.rmtree(REPO_ROOT / "results", ignore_errors=True)


def clean_output() -> None:
    shutil.rmtree(REPO_ROOT / "output", ignore_errors=True)


def main(argv: Iterable[str]) -> int:
    global RUN_OPTIONS
    args = list(argv)
    usage = (
        "Usage: nightly.py "
        "[dependency|hazel-dependency|build|coverage|run|profile|hazel|hazel-experiment|hazel-no-evict|"
        "hazel-report|arith|arith-report|arith-scaling|hazel-scaling|"
        "hazel-no-evict-scaling|scaling|scaling-report|entropy-scaling|entropy-report|report|experiment|"
        "hazel-tex|arith-tex|compile-generated|all] [--smoke]"
    )

    smoke = False
    if "--smoke" in args:
        smoke = True
        args.remove("--smoke")
    if any(arg.startswith("-") for arg in args):
        unknown = next(arg for arg in args if arg.startswith("-"))
        print(f"Unknown option: {unknown}", file=sys.stderr)
        print(usage, file=sys.stderr)
        return 1
    if len(args) > 1:
        print("Only a single stage argument is supported before options.", file=sys.stderr)
        print(usage, file=sys.stderr)
        return 1

    stage = args[0] if args else "all"
    RUN_OPTIONS = RunOptions(smoke=smoke)

    if stage == "dependency":
        install_dependencies()
    elif stage == "hazel-dependency":
        hazel_dependency()
    elif stage == "build":
        build_project()
    elif stage == "coverage":
        coverage_project()
    elif stage == "run":
        run_project()
    elif stage == "profile":
        profile_project()
    elif stage == "hazel":
        hazel_project()
    elif stage == "hazel-experiment":
        hazel_experiment_project()
    elif stage == "hazel-no-evict":
        hazel_no_evict_project()
    elif stage == "hazel-report":
        hazel_report_project()
    elif stage == "arith":
        arith_project()
    elif stage == "arith-report":
        arith_report_project()
    elif stage == "arith-scaling":
        arith_scaling_project()
    elif stage == "hazel-scaling":
        hazel_scaling_project()
    elif stage == "hazel-no-evict-scaling":
        hazel_no_evict_scaling_project()
    elif stage == "scaling":
        scaling_project()
    elif stage == "scaling-report":
        clean_output()
        scaling_report_project()
    elif stage == "entropy-scaling":
        entropy_scaling_project()
    elif stage == "entropy-report":
        clean_output()
        entropy_report_project()
    elif stage == "report":
        report_project()
    elif stage == "experiment":
        experiment_project()
    elif stage == "hazel-tex":
        hazel_tex_project()
    elif stage == "arith-tex":
        arith_tex_project()
    elif stage == "compile-generated":
        compile_generated()
    elif stage == "all":
        clean_results()
        clean_output()
        install_dependencies()
        hazel_dependency()
        build_project()
        arith_project()
        hazel_experiment_project(generate_report=False)
        hazel_no_evict_project(generate_report=False)
        scaling_project()
        entropy_scaling_project()
        report_module.generate_reports()
    else:
        print(f"Unknown stage: {stage}", file=sys.stderr)
        print(usage, file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
