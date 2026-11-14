#!/usr/bin/env python3
"""Auto-formatter and format verifier for cdd-cobol.

Supports two modes modeled after `cargo fmt` and `cargo fmt --check`:
  - `--fix`: deterministically formats COBOL (.cbl/.cob/.cpy), Shell, Python,
    JSON, Makefile, YAML, Markdown, and dotfile sources in-place while
    preserving byte-exact golden test fixtures under `tests/fixtures/`.
  - `--check`: verifies that all files match their canonical formatted state,
    contain valid UTF-8 and LF line endings, obey TAB/whitespace rules, keep
    fixed-format COBOL code within column 72, and parse cleanly as JSON.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import subprocess
import sys
from typing import List, Optional, Sequence, Tuple

LF = chr(10)
CR = chr(13)
CRLF = CR + LF
TAB = chr(9)
NUL = bytes([0])

IGNORED_TOP_DIRS = frozenset(
    {
        ".git",
        "bin",
        "build",
        "dist",
        "out",
        "pkg",
        "release",
        "stage",
        "target",
        ".pre-commit-cache",
    }
)

COBOL_SUFFIXES = frozenset(
    {".cbl", ".cob", ".CBL", ".COB", ".cpy", ".CPY", ".pco", ".sqb"}
)
COPYBOOK_SUFFIXES = frozenset({".cpy", ".CPY"})
MAKEFILE_NAMES = frozenset({"Makefile", "GNUmakefile", "makefile"})
MARKDOWN_SUFFIXES = frozenset({".md", ".markdown"})
FOUR_SPACE_SUFFIXES = frozenset({".sh", ".bash", ".zsh", ".py"})
FIXTURE_STREAM_SUFFIXES = frozenset({".stdin", ".stdout", ".stderr", ".crlf"})

BINARY_SUFFIXES = frozenset(
    {
        ".a",
        ".o",
        ".obj",
        ".lo",
        ".so",
        ".dylib",
        ".dll",
        ".exe",
        ".lib",
        ".mod",
        ".int",
        ".gnt",
        ".idy",
        ".pdb",
        ".gcno",
        ".gcda",
        ".tar",
        ".gz",
        ".tgz",
        ".bz2",
        ".xz",
        ".zst",
        ".zip",
        ".7z",
        ".png",
        ".jpg",
        ".jpeg",
        ".gif",
        ".webp",
        ".ico",
        ".pdf",
    }
)

LEVEL_COLUMNS = {
    "01": 8,
    "05": 12,
    "10": 16,
    "15": 20,
    "20": 24,
    "25": 28,
    "66": 8,
    "77": 8,
}

AREA_A_DIVISIONS = (
    "IDENTIFICATION DIVISION",
    "ID DIVISION",
    "ENVIRONMENT DIVISION",
    "DATA DIVISION",
    "PROCEDURE DIVISION",
)

AREA_A_SECTIONS = (
    "CONFIGURATION SECTION.",
    "INPUT-OUTPUT SECTION.",
    "FILE SECTION.",
    "WORKING-STORAGE SECTION.",
    "LOCAL-STORAGE SECTION.",
    "LINKAGE SECTION.",
)

LEVEL_RE = re.compile("^(01|05|10|15|20|25|66|77|88)[ ]+")


def repo_root() -> pathlib.Path:
    """Return the absolute root directory of the repository."""
    return pathlib.Path(__file__).resolve().parent.parent


def is_fixture_path(rel_path: pathlib.Path) -> bool:
    """Return True if `rel_path` is a byte-exact test fixture or stream file."""
    return (
        rel_path.parts[:2] == ("tests", "fixtures")
        or rel_path.suffix in FIXTURE_STREAM_SUFFIXES
    )


def is_makefile_path(rel_path: pathlib.Path) -> bool:
    """Return True if `rel_path` is a GNU Make file."""
    return rel_path.name in MAKEFILE_NAMES or rel_path.suffix == ".mk"


def discover_files(root: pathlib.Path, explicit: Sequence[str]) -> List[pathlib.Path]:
    """Return sorted repository-relative file paths to inspect."""
    if explicit:
        found: List[pathlib.Path] = []
        for raw in explicit:
            candidate = pathlib.Path(raw)
            abs_p = candidate if candidate.is_absolute() else (root / candidate)
            if abs_p.is_file():
                try:
                    rel = abs_p.resolve().relative_to(root.resolve())
                except ValueError:
                    continue
                if not rel.parts or rel.parts[0] in IGNORED_TOP_DIRS:
                    continue
                if rel.suffix.lower() not in BINARY_SUFFIXES:
                    found.append(rel)
        return sorted(set(found))

    try:
        out = subprocess.check_output(
            ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
            cwd=str(root),
            stderr=subprocess.DEVNULL,
        )
        rels: List[pathlib.Path] = []
        for item in out.split(NUL):
            if not item:
                continue
            rel = pathlib.Path(item.decode("utf-8"))
            if rel.parts and rel.parts[0] in IGNORED_TOP_DIRS:
                continue
            if rel.suffix.lower() in BINARY_SUFFIXES:
                continue
            if (root / rel).is_file():
                rels.append(rel)
        if rels:
            return sorted(set(rels))
    except (OSError, subprocess.SubprocessError):
        pass

    fallback: List[pathlib.Path] = []
    for p in root.rglob("*"):
        if not p.is_file():
            continue
        rel = p.relative_to(root)
        if any(part in IGNORED_TOP_DIRS for part in rel.parts):
            continue
        if rel.suffix.lower() in BINARY_SUFFIXES:
            continue
        fallback.append(rel)
    return sorted(set(fallback))


def normalize_newlines(text: str) -> str:
    """Convert CRLF and lone CR sequences to LF."""
    return text.replace(CRLF, LF).replace(CR, LF)


def finalize_eof(lines: List[str]) -> str:
    """Join lines with LF and ensure a single trailing LF when non-empty."""
    trimmed = list(lines)
    while trimmed and trimmed[-1] == "":
        trimmed.pop()
    if not trimmed:
        return ""
    return LF.join(trimmed) + LF


def format_cobol_text(text: str, is_copybook: bool) -> str:
    """Format fixed-format ANSI COBOL-85 source or copybook content."""
    norm = normalize_newlines(text)
    in_data_div = is_copybook
    parent_data_col = 8
    out_lines: List[str] = []

    for raw_line in norm.split(LF):
        expanded = raw_line.expandtabs(4).rstrip(" ")
        if expanded == "":
            out_lines.append("")
            continue

        if len(expanded) < 7:
            out_lines.append(expanded)
            continue

        seq = expanded[:6]
        ind = expanded[6]
        rest = expanded[7:]

        if seq == "      " and ind == "d":
            ind = "D"

        if ind in ("*", "/"):
            out_lines.append(seq + ind + rest)
            continue

        if ind == "-":
            out_lines.append(seq + ind + rest)
            continue

        if seq == "      " and ind in (" ", "D"):
            stripped = rest.lstrip(" ")
            upper = stripped.upper()

            if any(upper.startswith(div) for div in AREA_A_DIVISIONS):
                if upper.startswith("DATA DIVISION"):
                    in_data_div = True
                    parent_data_col = 8
                elif upper.startswith("PROCEDURE DIVISION"):
                    in_data_div = False
                candidate = seq + ind + stripped
                if len(candidate) <= 72:
                    out_lines.append(candidate)
                    continue

            if upper in AREA_A_SECTIONS:
                if upper in (
                    "FILE SECTION.",
                    "WORKING-STORAGE SECTION.",
                    "LOCAL-STORAGE SECTION.",
                    "LINKAGE SECTION.",
                ):
                    in_data_div = True
                    parent_data_col = 8
                candidate = seq + ind + stripped
                if len(candidate) <= 72:
                    out_lines.append(candidate)
                    continue

            if in_data_div:
                m_lvl = LEVEL_RE.match(stripped)
                if m_lvl is not None:
                    lvl = m_lvl.group(1)
                    if lvl == "88":
                        target_col = max(12, parent_data_col + 4)
                    else:
                        target_col = LEVEL_COLUMNS[lvl]
                        if lvl != "66":
                            parent_data_col = target_col
                    pad = " " * (target_col - 8)
                    candidate = seq + ind + pad + stripped
                    if len(candidate) <= 72:
                        out_lines.append(candidate)
                        continue

        out_lines.append(seq + ind + rest)

    return finalize_eof(out_lines)


def format_makefile_text(text: str) -> str:
    """Format Makefile content while preserving literal TAB recipe prefixes."""
    norm = normalize_newlines(text)
    out_lines = [line.rstrip(" " + TAB) for line in norm.split(LF)]
    return finalize_eof(out_lines)


def format_markdown_text(text: str) -> str:
    """Format Markdown content while preserving intentional two-space breaks."""
    norm = normalize_newlines(text)
    out_lines: List[str] = []
    for raw_line in norm.split(LF):
        expanded = raw_line.expandtabs(2)
        core = expanded.rstrip(" ")
        trailing_spaces = len(expanded) - len(core)
        if core and trailing_spaces == 2:
            out_lines.append(core + "  ")
        else:
            out_lines.append(core)
    return finalize_eof(out_lines)


def format_generic_text(text: str, tab_width: int) -> str:
    """Normalize LF, expand TABs, trim trailing whitespace, and finalize EOF."""
    norm = normalize_newlines(text)
    out_lines = [line.expandtabs(tab_width).rstrip(" ") for line in norm.split(LF)]
    return finalize_eof(out_lines)


def compute_formatted(
    rel_path: pathlib.Path, raw_bytes: bytes
) -> Tuple[Optional[bytes], List[str]]:
    """Compute canonical formatted bytes and return `(formatted_bytes, errors)`."""
    errors: List[str] = []
    try:
        text = raw_bytes.decode("utf-8")
    except UnicodeDecodeError:
        errors.append(f"{rel_path}: invalid UTF-8 byte sequence")
        return None, errors

    if is_fixture_path(rel_path):
        if rel_path.suffix == ".json":
            try:
                json.loads(text)
            except ValueError as exc:
                errors.append(f"{rel_path}: invalid JSON ({exc})")
        return raw_bytes, errors

    if rel_path.suffix in COBOL_SUFFIXES:
        formatted = format_cobol_text(text, rel_path.suffix in COPYBOOK_SUFFIXES)
        for lineno, line in enumerate(formatted.splitlines(), 1):
            if len(line) >= 7 and line[6] not in ("*", "/") and len(line) > 72:
                errors.append(
                    f"{rel_path}:{lineno}: COBOL code exceeds column 72 (len={len(line)})"
                )
        return formatted.encode("utf-8"), errors

    if rel_path.suffix == ".json":
        try:
            parsed = json.loads(text)
        except ValueError as exc:
            errors.append(f"{rel_path}: invalid JSON ({exc})")
            return None, errors
        formatted = json.dumps(parsed, indent=2, ensure_ascii=False) + LF
        return formatted.encode("utf-8"), errors

    if is_makefile_path(rel_path):
        formatted = format_makefile_text(text)
        return formatted.encode("utf-8"), errors

    if rel_path.suffix in MARKDOWN_SUFFIXES:
        formatted = format_markdown_text(text)
        return formatted.encode("utf-8"), errors

    tab_size = 4 if rel_path.suffix in FOUR_SPACE_SUFFIXES else 2
    formatted = format_generic_text(text, tab_size)
    return formatted.encode("utf-8"), errors


def run_formatter(mode_fix: bool, explicit_files: Sequence[str]) -> int:
    """Execute `--fix` or `--check` across discovered repository files."""
    root = repo_root()
    files = discover_files(root, explicit_files)

    modified: List[pathlib.Path] = []
    unformatted: List[pathlib.Path] = []
    hard_errors: List[str] = []

    for rel_path in files:
        abs_path = root / rel_path
        raw_bytes = abs_path.read_bytes()
        formatted_bytes, errs = compute_formatted(rel_path, raw_bytes)
        hard_errors.extend(errs)
        if formatted_bytes is None:
            continue

        if formatted_bytes != raw_bytes:
            if mode_fix:
                abs_path.write_bytes(formatted_bytes)
                modified.append(rel_path)
            else:
                unformatted.append(rel_path)

    if mode_fix:
        for rel_path in modified:
            print(f"[FMT-FIX] Formatted {rel_path}")
        if hard_errors:
            for err in hard_errors:
                print(f"ERROR: {err}", file=sys.stderr)
            return 1
        print(
            f"[OK] Format fix complete ({len(modified)} updated, {len(files)} inspected)."
        )
        return 0

    for rel_path in unformatted:
        print(
            f"ERROR: {rel_path}: file is not formatted (run `make fmt`)",
            file=sys.stderr,
        )
    for err in hard_errors:
        print(f"ERROR: {err}", file=sys.stderr)

    if unformatted or hard_errors:
        return 1

    print(f"[OK] Format check passed across {len(files)} file(s).")
    return 0


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    """Parse CLI arguments for `format-code.py`."""
    parser = argparse.ArgumentParser(
        description="Auto-format (--fix) or verify formatting (--check) for cdd-cobol."
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--fix",
        action="store_true",
        help="Automatically format files in-place.",
    )
    group.add_argument(
        "--check",
        action="store_true",
        help="Check formatting without modifying files; exit non-zero on drift.",
    )
    parser.add_argument(
        "files",
        nargs="*",
        help="Optional explicit file paths (defaults to all repository files).",
    )
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    """Entry point returning process exit code."""
    args = parse_args(sys.argv[1:] if argv is None else argv)
    return run_formatter(mode_fix=args.fix, explicit_files=args.files)


if __name__ == "__main__":
    sys.exit(main())
