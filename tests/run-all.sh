#!/bin/sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT_DIR"

steel tests/test-core.scm

# Verify Steel parser/AST accepts all plugin files cleanly
for f in lib/core.scm lib/ui.scm oil.scm; do
    steel ast "$f" >/dev/null
    echo "AST OK $f"
done
