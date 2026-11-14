#!/usr/bin/env bash
set -euo pipefail

PATTERN="git"" push"
if grep -rn --exclude=check-git-policy.sh "$PATTERN" Makefile scripts/ tools/ src/ 2>/dev/null | grep -v "Strictly prohibits"; then
    echo "ERROR: prohibited remote push reference found!" >&2
    exit 1
fi
echo "[OK] Git policy verified (push strictly prohibited)."
