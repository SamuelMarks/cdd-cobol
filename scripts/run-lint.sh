#!/usr/bin/env bash
set -euo pipefail

# 1. Repository git policy enforcement
bash scripts/check-git-policy.sh

# 2. Code formatting & encoding verification
python3 scripts/format-code.py --check

# 3. Shell script static analysis (ShellCheck)
if command -v shellcheck >/dev/null 2>&1; then
    shellcheck --severity=warning scripts/*.sh
    echo "[OK] ShellCheck passed on scripts/*.sh."
fi

# 4. Python script bytecode/syntax & JSON/YAML configuration validation
python3 -m py_compile scripts/*.py
python3 - <<'PY'
import json
import pathlib
import sys

root = pathlib.Path(".").resolve()
ignored = {".git", "bin", "build", "dist", "out", "pkg", "release", "stage", "target", ".pre-commit-cache"}

json_count = 0
yaml_files = []
for p in sorted(root.rglob("*")):
    if not p.is_file():
        continue
    rel = p.relative_to(root)
    if any(part in ignored for part in rel.parts):
        continue
    if rel.suffix == ".json":
        json.loads(p.read_text(encoding="utf-8"))
        json_count += 1
    elif rel.suffix in (".yml", ".yaml"):
        yaml_files.append(rel)

try:
    import yaml  # type: ignore
    for yf in yaml_files:
        yaml.safe_load((root / yf).read_text(encoding="utf-8"))
    print(f"[OK] Validated {json_count} JSON file(s) and {len(yaml_files)} YAML file(s).")
except ImportError:
    print(f"[OK] Validated {json_count} JSON file(s).")
PY

# 5. Self-hosted COBOL 100% documentation & strong-typing static linters
make doc-check
make type-check
