#!/usr/bin/env bash
set -euo pipefail

eval "$(opam env --switch ant --set-switch)"
exec "$@"
