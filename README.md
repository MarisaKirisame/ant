# Chordata Artifact

This repository contains the implementation and evaluation artifact for
Chordata, including the compiler and runtime, arithmetic benchmarks,
anonymized Hazel editing traces, experiment drivers, report generation, and
the Agda mechanization.

The three trace contributors are identified only as `user1`, `user2`, and
`user3`. These identifiers are stable across benchmarks so paired aggregate
statistics can be reproduced.

## Requirements

Run the artifact on a 64-bit Linux host with Docker Engine installed. At least
32 GiB of free disk space is recommended. No host installation of OCaml, OPAM,
Python, Node, UV, Agda, or Hazel is required.

The account running the artifact must have permission to access the Docker
daemon without `sudo`. If Docker was just installed or the account was just
added to the `docker` group, log out and back in before running the artifact so
the new group membership takes effect.

Run commands from the top-level artifact directory. The initial invocation
builds the `chordata-artifact` image and downloads pinned dependencies.
Experiment and proof containers subsequently run with Docker networking
disabled. The image contains OCaml 5.4.1 for Chordata, OCaml 5.2.0 for Hazel,
Agda 2.7.0.1, and Agda standard library 2.2.

## The Four Commands

| Command | Purpose |
|---|---|
| `make smoke` | Verify the complete build, measurement, plotting, and reporting path with tiny inputs |
| `make quick` | Run the central 22-trace Hazel evaluation at the paper's input size 400 |
| `make eval` | Run the complete paper evaluation and regenerate every report |
| `make proof` | Type-check the complete Agda mechanization |

### Smoke

`make smoke` is the kick-the-tires check. It runs reduced instances of every
experiment family: input size 2, one Hazel candidate per trace, one arithmetic
sample, and scaling size 2. Success means the command exits with status zero
and creates:

- raw measurements in `docker-output/results/`; and
- the report index at `docker-output/output/index.html`.

The smoke run checks that the entire path works. Its numerical values are not
intended to reproduce the paper.

### Quick

`make quick` runs all 22 terminating participant/benchmark traces, every
recorded program state, and input size 400. This is the same Chordata-versus-CEK
Hazel trace configuration used by the full evaluation and takes approximately
15 minutes on the reference machine.

Quick clears earlier smoke or full-evaluation measurements before it starts,
so its focused report cannot include stale results from omitted experiments.
The resulting report is
`docker-output/output/hazel/index.html`. This focused run omits the official
Hazel evaluator, eviction ablation, arithmetic benchmark, scaling study, and
input-regularity study.

### Eval

`make eval` runs every experiment family and regenerates the complete report at
`docker-output/output/index.html`. It includes:

- the 22 terminating Hazel traces at input size 400;
- the eviction/no-eviction comparison;
- official Hazel and Chordata CEK at input size 10;
- arithmetic evaluation;
- Hazel and arithmetic scaling; and
- the input-regularity experiment.

Allow approximately three hours for the full evaluation; slower hosts may take
longer. Official-Hazel timeouts on difficult sorting traces, such as
`user2_qs` on the authors' test machine, are expected and do not abort report
generation. Timing and memory values vary by machine, so compare paired
configurations from the same run.

### Proof

`make proof` checks all modules under `proof/`. Successful output type-checks
`all.agda`, which imports the core definitions and the compatibility,
existence, and validity theorems.

## Reading the Evaluation

After `make eval`, open `docker-output/output/index.html`. That page links the
reports supporting every empirical claim. [EVALUATION.md](EVALUATION.md) maps
the paper's claims directly to those report pages and describes the expected
qualitative results.

The artifact does not include measurements or reports generated on the
authors' machine. Each evaluation command starts from empty result and report
directories, so everything under `docker-output/` comes from the evaluator's
machine. `EVALUATION.md` lists the final-paper values only as comparison
checkpoints and explains which generated macros and report pages correspond to
them.

## Package Contents

- `lib/`, `PPX/`, and `bin/`: Chordata implementation and experiment runners.
- `examples/` and `bench/`: example programs and arithmetic benchmarks.
- `data/`: anonymized Hazel editing traces.
- `hazel/`: the vendored Hazel evaluator used by the comparison.
- `proof/`: Agda mechanization.

The full evaluation is CPU- and memory-intensive and currently targets Linux
`amd64`. Performance measurements should not be compared across machines
without accounting for hardware and system load.
