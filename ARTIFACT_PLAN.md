# OOPSLA 2026 Artifact Submission Plan

## Goal

Submit the Chordata artifact for OOPSLA 2026 Artifact Evaluation and target:

- **Artifact Evaluated — Functional**
- **Results Validated — Results Reproduced**
- **Artifact Available**, by publishing the final artifact in an archival location with a DOI

The **Reusable** badge is a stretch goal. It requires additional documentation explaining how to extend Chordata with new programs, traces, and benchmarks.

## Deadline and Submission

- **Artifact submission (Round 2): Thursday, July 23, 2026, AoE (UTC-12)**
- **Kick-the-Tires:** July 27–August 3, 2026
- **Notification:** August 30, 2026
- **Submission site:** <https://oopsla26aec.hotcrp.com/>
- **Guidelines:** <https://2026.splashcon.org/track/splash-2026-artifact-evaluation#Submission-Guidelines>

The artifact must be submitted as a stable URL with a DOI. Zenodo is the recommended host because it supports revised versions while retaining a stable record.

Artifact evaluation is single-blind. The artifact does not need to be anonymized.

## Required Artifact Contents

The submitted package should contain:

- Chordata source code.
- All Git submodule contents, not only Git submodule pointers:
  - Hazel.
  - `lib/hashtbl`.
  - `lib/hwsl2_core`.
  - `lib/monoid_hash`.
- The complete Agda mechanization promised by the paper.
- All Hazel editing traces used in the paper.
- Arithmetic benchmarks.
- Scaling and entropy benchmarks.
- Cache-eviction and no-eviction configurations.
- Official-Hazel comparison code and inputs.
- Scripts that build Chordata, run experiments, and generate reports.
- Plotting dependencies and plot-generation scripts.
- No pre-generated measurements or reports from the authors' machine; the
  documentation instead records final-paper checkpoints and maps them to files
  regenerated on the evaluator's machine.
- A copy of the submitted paper, or an unambiguous reference to its numbered sections and claims.
- The project license and the licenses of redistributed dependencies and datasets.

Do not create the package with a plain `git archive`: that would omit the contents of Git submodules.

## Packaging Decision

Preferred package:

1. A Docker image or Docker build context containing all dependencies.
2. A conventional ZIP or `.tar.gz` source archive alongside it.
3. Final-paper checkpoint values in the evaluation guide, without
   machine-specific pre-generated outputs.

The packaged environment should contain:

- A documented Linux base image and supported CPU architecture.
- OCaml 5.4.1.
- The pinned OPAM and Dune dependencies.
- Python 3.12, UV, and the locked Python dependencies.
- Agda and the required Agda standard library version.
- Hazel dependencies.
- All plotting tools.
- Pre-built Chordata and Hazel binaries where practical.

If Docker cannot be completed in time, submit a source archive with precise OPAM/UV installation instructions, but test those instructions on a fresh machine first.

The artifact must not contain telemetry or analytics that could compromise reviewer anonymity.

## Documentation Structure

Add a top-level `README.md` organized as follows.

### 1. Artifact Overview

- What Chordata does.
- Which parts of the paper the artifact supports.
- Inventory of the package.
- Supported OS and architecture.
- Expected CPU, memory, and disk requirements.

### 2. Getting Started

- How to obtain and unpack the artifact.
- Exact dependency versions.
- Docker, VM, or native installation instructions.
- The directory from which each command must be run.

### 3. Kick-the-Tires

Provide `make smoke`, a sanity test that takes no more than approximately 30
minutes.

Document:

- Expected console output.
- Expected generated files.
- How to recognize success.
- Approximate runtime on the tested machine.

### 4. Full Evaluation

Document `make quick` as the focused Hazel evaluation and `make eval` as the
complete end-to-end evaluation. Keep internal experiment stages out of the
evaluator-facing interface. For each evaluation level, state:

- Expected runtime.
- Output directory and filenames.
- Expected timeouts or failures.
- Which paper table, figure, or claim it supports.
- How hardware-dependent results may differ.
- Which qualitative trend must remain true.

### 5. Reproducibility

Include a claim-by-claim checklist mapping paper claims to:

- Paper section, theorem, table, or figure.
- Artifact source/data.
- The linked page to inspect after `make eval`.
- Expected result or acceptable range.

### 6. Reusability

Explain:

- How to compile and run a new `.ant` program.
- How to add a benchmark.
- How to add or replace a Hazel trace.
- How to run with and without memoization.
- How to run with and without eviction.
- How to regenerate reports and plots.
- Known limitations and unsupported platforms.

### 7. Troubleshooting

Document common failures involving:

- OPAM switches.
- OCaml version mismatches.
- Hazel dependencies.
- Missing submodules.
- Plotting or display libraries.
- Expected benchmark timeouts.
- Hardware-sensitive performance results.

## Claim-to-Evaluation Checklist

| Paper claim | Artifact action | Expected evidence |
|---|---|---|
| The formal semantics and stated theorems are mechanized | Run Agda over the complete mechanization | All designated modules type-check; document any postulates or trusted components |
| Chordata accelerates Hazel evaluation | Run the 22-trace Hazel experiment | Regenerated Hazel speedup table and aggregate speedup |
| The reported memory overhead is measured using the paper's final methodology | Run the Hazel memory experiment | Regenerated per-benchmark and aggregate memory results |
| Cache eviction trades time for reduced memory | Run eviction and no-eviction configurations | Two tables plus a documented comparison |
| Chordata is compared fairly with the official Hazel evaluator | Run official Hazel and CEK baseline at the same documented input size | Comparison data and plot, including documented timeouts |
| Shortcut memoization accelerates algebraic simplification | Run the arithmetic experiment | Regenerated arithmetic table and scatterplot |
| Performance changes with input size | Run Hazel and arithmetic scaling experiments | Regenerated scaling plots |
| Repetition within an execution enables reuse | Run entropy/input-structure experiments | Regenerated entropy line and scatter plots |
| The implementation works end to end | Run the smoke pipeline | Successful build and complete smoke report |

## Current Repository Gaps

- [x] Replace participant initials in trace, mode, and result identifiers with `user1`, `user2`, and `user3`.
- [x] Audit the trace contents for additional participant-identifying metadata. No direct names, contact information, machine paths, hostnames, session identifiers, or timestamps were found. Preserve incidental identifiers inside recorded programs because rewriting them would change the benchmark traces.
- [x] Add a deterministic artifact packaging script that excludes repository history and local state, vendors populated submodules, and normalizes archive ownership and timestamps.
- [x] Add a top-level artifact `README.md`.
- [x] Add a Docker build with pinned, separate Chordata and Hazel OCaml switches plus smoke/full evaluation targets.
- [x] Locate and include the complete Agda mechanization.
- [x] Add an explicit Agda verification command.
- [x] Reconcile `setup.sh` and the experiment driver with the pinned OCaml 5.4.1 toolchain.
- [ ] Replace placeholder metadata in `dune-project` and `ant.opam`.
- [ ] Ensure all submodule contents are vendored into the submitted archive.
- [ ] Document the precise origin and license of all Hazel traces.
- [ ] Document quick and full experiment runtimes.
- [ ] Document expected timeouts and hardware-sensitive deviations.
- [ ] Add a script that checks expected output files after the smoke run.
- [ ] Add a script or document that compares regenerated macros with the paper.
- [x] Document the paper's final result macros as checkpoints without bundling
  machine-specific reference results.
- [x] Test installation and evaluation on a clean container.
- [ ] Build the final archive and inspect it without relying on the working repository.
- [ ] Publish the archive to Zenodo and obtain a DOI.
- [ ] Submit the DOI-backed stable URL to HotCRP.

## Prioritized Execution Plan

### P0 — Required Before Submission

1. Locate and add the Agda proof artifact.
2. Write the top-level artifact README.
3. Fix the OCaml/toolchain installation mismatch.
4. Vendor all submodules and benchmark inputs.
5. Define and test the smoke command.
6. Define the four-command evaluator interface and expected outputs.
7. Package the artifact.
8. Test the package from a clean environment.
9. Upload it to Zenodo.
10. Submit the Zenodo DOI URL to HotCRP.

### P1 — Strongly Recommended

1. Add automated output validation.
2. Document final-paper checkpoint values without bundling machine-specific
   reference results.
3. Document expected numerical variability.
4. Explain all expected timeouts.
5. Map each claim to a page in the full-evaluation report.
6. Ensure every paper plot and table can be regenerated without copy/paste.

### P2 — Reusable Badge Improvements

1. Document how to add new programs, traces, and benchmarks.
2. Improve code-level documentation around the compiler and memo runtime.
3. Add end-user examples beyond the paper benchmarks.
4. State stable APIs and known limitations.
5. Make the container multi-platform if feasible.

## Final Pre-Submission Audit

- [ ] Confirm that participant consent and any applicable IRB/data-use terms permit redistribution of the full editing traces.
- [ ] Manually review recorded program text and exercise identifiers for quasi-identifying information, even though the automated scan found no direct identifiers.
- [ ] Package a clean source snapshot excluding `.git`, nested submodule `.git` files, `_build`, `.venv`, `__pycache__`, `.agents`, `.codex`, logs, and editor/OS metadata.
- [ ] Normalize archive owner/group and timestamps so local account and filesystem metadata are not shipped.
- [ ] The archive downloads from the DOI URL without authentication.
- [ ] The archive unpacks using standard tools.
- [ ] The top-level README is immediately visible.
- [ ] No command depends on files outside the archive.
- [ ] No command depends on unpublished Git branches.
- [ ] All submodules are populated.
- [ ] All dependencies and versions are documented.
- [ ] The smoke path succeeds from a clean environment.
- [ ] The proof checker succeeds.
- [ ] The full evaluation can be completed in a few hours, or long steps have representative alternatives.
- [ ] Generated tables and plots are easy to compare with the paper.
- [ ] Expected deviations and timeouts are documented.
- [ ] There is no telemetry or reviewer-identifying logging.
- [ ] Licenses permit redistribution.
- [ ] The Zenodo record contains the correct title, authors, description, license, and version.
- [ ] The HotCRP submission points to the stable DOI-backed URL.
