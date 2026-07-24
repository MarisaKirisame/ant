#!/usr/bin/env bash
set -euo pipefail

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
destination=${1:-"$repo_dir/ant-artifact.tar.gz"}
source_date_epoch=${SOURCE_DATE_EPOCH:-0}

stage_dir=$(mktemp -d)
archive_tmp=$(mktemp)
cleanup() {
  rm -rf -- "$stage_dir"
  rm -f -- "$archive_tmp"
}
trap cleanup EXIT

mkdir -p -- "$stage_dir/ant-artifact"

# Copy the complete source snapshot, including populated submodules, while
# omitting repository history, local state, and generated results.
tar \
  --exclude='./.git' \
  --exclude='./.gitmodules' \
  --exclude='./.agents' \
  --exclude='./.codex' \
  --exclude='./.venv' \
  --exclude='./_opam' \
  --exclude='./_build' \
  --exclude='./__pycache__' \
  --exclude='./ant-artifact*.tar.gz' \
  --exclude='./ARTIFACT_PLAN.md' \
  --exclude='./results' \
  --exclude='./output' \
  --exclude='./docker-output' \
  --exclude='./oopsla26-paper618-revised_supplementary_material.zip' \
  --exclude='./speedup.png' \
  --exclude='*/.git' \
  --exclude='*/.agents' \
  --exclude='*/.codex' \
  --exclude='*/.venv' \
  --exclude='*/_opam' \
  --exclude='*/_build' \
  --exclude='*/__pycache__' \
  --exclude='*/docker-output' \
  --exclude='*.pyc' \
  --exclude='*.trace' \
  --exclude='.DS_Store' \
  --exclude='*.code-workspace' \
  --exclude='.vscode' \
  -C "$repo_dir" -cf - . |
  tar -C "$stage_dir/ant-artifact" --no-same-owner -xf -

# Produce a deterministic archive without local usernames, group names, or
# filesystem timestamps.
tar \
  --sort=name \
  --mtime="@$source_date_epoch" \
  --owner=0 \
  --group=0 \
  --numeric-owner \
  --use-compress-program='gzip -n' \
  -C "$stage_dir" -cf "$archive_tmp" ant-artifact

mkdir -p -- "$(dirname -- "$destination")"
mv -- "$archive_tmp" "$destination"
printf 'Created %s\n' "$destination"
