#!/usr/bin/env python3
"""Add or update '% doc coverage' and '% test coverage' shields in README.md.

Computes exact documentation coverage via `bin/cdd-doc-check` and exact
function/line/branch test coverage via `bin/cdd-cov-check` (automatically
building tools or refreshing coverage artifacts if missing or stale), then
inserts or updates the corresponding Shields.io badges in `README.md`.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
import sys
from typing import List, Optional, Sequence, Tuple

LF = chr(10)
CR = chr(13)
CRLF = CR + LF

DOC_PASS_RE = re.compile(
    r"\[DOC-CHECK\][ ]+Files:[ ]*([0-9]+)[ ]+Items:[ ]*([0-9]+)[ ]+\(([0-9]+(?:[.][0-9]+)?)%[ ]+doc[ ]+coverage\)"
)
COV_LINE_RE = re.compile(
    r"\[COV-CHECK\][ ]+Functions:[ ]*([0-9]+)/([0-9]+)[ ]+\|[ ]+Lines:[ ]*([0-9]+)/([0-9]+)[ ]+\|[ ]+Branches:[ ]*([0-9]+)/([0-9]+)"
)

DOC_BADGE_RE = re.compile(
    r"\[!\[[^\]]*doc coverage[^\]]*\]\(https://img[.]shields[.]io/badge/doc(?:%20|_|\+)coverage-[^)]+\)\]\([^)]+\)"
    r"|!\[[^\]]*doc coverage[^\]]*\]\(https://img[.]shields[.]io/badge/doc(?:%20|_|\+)coverage-[^)]+\)"
)

TEST_BADGE_RE = re.compile(
    r"\[!\[[^\]]*test coverage[^\]]*\]\(https://img[.]shields[.]io/badge/test(?:%20|_|\+)coverage-[^)]+\)\]\([^)]+\)"
    r"|!\[[^\]]*test coverage[^\]]*\]\(https://img[.]shields[.]io/badge/test(?:%20|_|\+)coverage-[^)]+\)"
)


def repo_root() -> pathlib.Path:
    """Return the absolute root directory of the repository."""
    return pathlib.Path(__file__).resolve().parent.parent


def latest_source_mtime(root: pathlib.Path, dirs: Sequence[str]) -> float:
    """Return the newest modification timestamp across `.cbl`/`.cpy` files."""
    newest = 0.0
    for dname in dirs:
        base = root / dname
        if not base.exists():
            continue
        for p in base.rglob("*"):
            if p.is_file() and p.suffix in (".cbl", ".cpy", ".CBL", ".CPY"):
                mtime = p.stat().st_mtime
                if mtime > newest:
                    newest = mtime
    return newest


def badge_color(pct: float) -> str:
    """Map a coverage percentage (0..100) to a Shields.io status color."""
    if pct >= 100.0:
        return "brightgreen"
    if pct >= 90.0:
        return "green"
    if pct >= 80.0:
        return "yellowgreen"
    if pct >= 70.0:
        return "yellow"
    if pct >= 50.0:
        return "orange"
    return "red"


def format_pct_text(pct: float) -> str:
    """Format percentage value cleanly (`100%` when 100.0, else `XX.X%`)."""
    if pct >= 100.0:
        return "100%"
    return f"{pct:.1f}%"


def make_shield_markdown(kind: str, pct: float) -> str:
    """Build Markdown badge for `kind` in `{'doc', 'test'}`."""
    pct_txt = format_pct_text(pct)
    pct_url = pct_txt.replace("%", "%25")
    color = badge_color(pct)
    label_url = f"{kind}%20coverage"
    alt_text = f"{pct_txt} {kind} coverage"
    return (
        f"[![{alt_text}]"
        f"(https://img.shields.io/badge/{label_url}-{pct_url}-{color}.svg)]"
        f"(#quality-gates)"
    )


def ensure_tools_built(root: pathlib.Path) -> None:
    """Compile self-hosted COBOL tools if binaries are missing or stale."""
    doc_bin = root / "bin" / "cdd-doc-check"
    cov_bin = root / "bin" / "cdd-cov-check"
    src_mtime = latest_source_mtime(root, ("include", "src", "tools"))
    if (
        not doc_bin.is_file()
        or not cov_bin.is_file()
        or doc_bin.stat().st_mtime < src_mtime
        or cov_bin.stat().st_mtime < src_mtime
    ):
        subprocess.run(["make", "build-tools"], cwd=str(root), check=True)


def compute_doc_coverage(root: pathlib.Path) -> float:
    """Compute documentation coverage percentage across COBOL/copybook files."""
    ensure_tools_built(root)
    build_dir = root / "build"
    build_dir.mkdir(parents=True, exist_ok=True)

    files: List[str] = []
    for sub in ("include", "src", "tools", "tests"):
        base = root / sub
        if not base.exists():
            continue
        for p in base.rglob("*"):
            if p.is_file() and p.suffix in (".cbl", ".cpy"):
                files.append(str(p.relative_to(root)))
    files.sort()

    lst_file = build_dir / "doc_files.lst"
    lst_file.write_text(LF.join(files) + LF, encoding="utf-8")

    proc = subprocess.run(
        [str(root / "bin" / "cdd-doc-check"), "build/doc_files.lst"],
        cwd=str(root),
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode == 0:
        match = DOC_PASS_RE.search(proc.stdout)
        if match is not None:
            return float(match.group(3))
        return 100.0

    missing_count = sum(1 for line in proc.stderr.splitlines() if ": missing " in line)
    total_items = 0
    single_lst = build_dir / "doc_single.lst"
    try:
        for rel in files:
            single_lst.write_text(rel + LF, encoding="utf-8")
            one = subprocess.run(
                [str(root / "bin" / "cdd-doc-check"), "build/doc_single.lst"],
                cwd=str(root),
                capture_output=True,
                text=True,
                check=False,
            )
            m_one = DOC_PASS_RE.search(one.stdout)
            if m_one is not None:
                total_items += int(m_one.group(2))
            else:
                total_items += sum(
                    1 for ln in one.stderr.splitlines() if ": missing " in ln
                )
    finally:
        single_lst.unlink(missing_ok=True)

    if total_items <= 0:
        return 0.0
    raw_pct = max(0.0, (total_items - missing_count) * 100.0 / total_items)
    return min(99.9, raw_pct) if missing_count > 0 else raw_pct


def compute_test_coverage(root: pathlib.Path) -> float:
    """Compute combined function/line/branch test coverage percentage."""
    ensure_tools_built(root)
    manifest = root / "build" / "cov" / "manifest.dat"
    hits = root / "build" / "cov" / "hits.dat"
    cov_info = root / "build" / "cov" / "coverage.info"
    src_mtime = latest_source_mtime(root, ("include", "src", "tools", "tests"))

    if not manifest.is_file() or not hits.is_file() or hits.stat().st_mtime < src_mtime:
        subprocess.run(["make", "coverage"], cwd=str(root), check=False)

    if not manifest.is_file() or not hits.is_file():
        raise RuntimeError(
            "Coverage artifacts (build/cov/manifest.dat, hits.dat) were not produced."
        )

    cmd = [
        str(root / "bin" / "cdd-cov-check"),
        "build/cov/manifest.dat",
        "build/cov/hits.dat",
    ]
    if cov_info.is_file():
        cmd.append("build/cov/coverage.info")

    proc = subprocess.run(
        cmd,
        cwd=str(root),
        capture_output=True,
        text=True,
        check=False,
    )
    match = COV_LINE_RE.search(proc.stdout)
    if match is None:
        raise RuntimeError(
            f"Unable to parse cdd-cov-check output: {proc.stdout!r} {proc.stderr!r}"
        )

    f_hit, f_tot, l_hit, l_tot, b_hit, b_tot = (
        int(match.group(i)) for i in range(1, 7)
    )
    tot_all = f_tot + l_tot + b_tot
    hit_all = f_hit + l_hit + b_hit
    if tot_all <= 0:
        return 0.0
    if f_hit == f_tot and l_hit == l_tot and b_hit == b_tot:
        return 100.0
    return min(99.9, max(0.0, hit_all * 100.0 / tot_all))


def upsert_shield_in_text(content: str, kind: str, badge_md: str) -> str:
    """Insert or replace the `doc` or `test` shield inside Markdown `content`."""
    norm = content.replace(CRLF, LF).replace(CR, LF)
    pattern = DOC_BADGE_RE if kind == "doc" else TEST_BADGE_RE
    other_pattern = TEST_BADGE_RE if kind == "doc" else DOC_BADGE_RE

    if pattern.search(norm) is not None:
        updated = pattern.sub(badge_md, norm, count=1)
        return updated.rstrip(LF) + LF

    lines = norm.split(LF) if norm else []
    for idx, line in enumerate(lines):
        if other_pattern.search(line) is not None:
            if kind == "doc":
                lines[idx] = f"{badge_md} {line.lstrip()}"
            else:
                lines[idx] = f"{line.rstrip()} {badge_md}"
            return LF.join(lines).rstrip(LF) + LF

    for idx, line in enumerate(lines):
        if line.startswith("# "):
            insert_at = idx + 1
            if insert_at < len(lines) and lines[insert_at].strip() == "":
                lines.insert(insert_at + 1, badge_md)
                lines.insert(insert_at + 2, "")
            else:
                lines.insert(insert_at, "")
                lines.insert(insert_at + 1, badge_md)
            return LF.join(lines).rstrip(LF) + LF

    prefix = [
        "# cdd-cobol",
        "",
        badge_md,
        "",
    ]
    merged = prefix + lines
    return LF.join(merged).rstrip(LF) + LF


def update_shields(
    readme_path: pathlib.Path,
    do_doc: bool,
    do_test: bool,
    check_only: bool,
    doc_pct_override: Optional[float] = None,
    test_pct_override: Optional[float] = None,
) -> Tuple[int, str]:
    """Compute requested coverages and update or verify `readme_path`."""
    root = repo_root()
    original = readme_path.read_text(encoding="utf-8") if readme_path.is_file() else ""
    updated = original
    summaries: List[str] = []

    if do_doc:
        doc_pct = (
            doc_pct_override
            if doc_pct_override is not None
            else compute_doc_coverage(root)
        )
        doc_badge = make_shield_markdown("doc", doc_pct)
        updated = upsert_shield_in_text(updated, "doc", doc_badge)
        summaries.append(f"doc coverage = {format_pct_text(doc_pct)}")

    if do_test:
        test_pct = (
            test_pct_override
            if test_pct_override is not None
            else compute_test_coverage(root)
        )
        test_badge = make_shield_markdown("test", test_pct)
        updated = upsert_shield_in_text(updated, "test", test_badge)
        summaries.append(f"test coverage = {format_pct_text(test_pct)}")

    summary_str = ", ".join(summaries)
    if check_only:
        if not readme_path.is_file() or updated != original:
            return 1, f"ERROR: {readme_path} shields out of date ({summary_str})"
        return 0, f"[OK] {readme_path.name} shields verified ({summary_str})."

    if updated != original or not readme_path.is_file():
        readme_path.write_text(updated, encoding="utf-8")
        return 0, f"[UPDATED] {readme_path.name} shields updated ({summary_str})."

    return 0, f"[OK] {readme_path.name} shields up to date ({summary_str})."


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    """Parse command-line options."""
    parser = argparse.ArgumentParser(
        description="Add or update '% doc coverage' and '% test coverage' shields in README.md."
    )
    parser.add_argument(
        "--doc",
        action="store_true",
        help="Add or update the '% doc coverage' shield.",
    )
    parser.add_argument(
        "--test",
        action="store_true",
        help="Add or update the '% test coverage' shield.",
    )
    parser.add_argument(
        "--readme",
        default=None,
        help="Path to README.md (defaults to repository root README.md).",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify shields are already up to date without writing README.md.",
    )
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    """CLI entry point."""
    args = parse_args(sys.argv[1:] if argv is None else argv)
    do_doc = args.doc or (not args.doc and not args.test)
    do_test = args.test or (not args.doc and not args.test)
    readme = (
        pathlib.Path(args.readme)
        if args.readme is not None
        else (repo_root() / "README.md")
    )
    rc, msg = update_shields(
        readme_path=readme,
        do_doc=do_doc,
        do_test=do_test,
        check_only=args.check,
    )
    stream = sys.stderr if rc != 0 else sys.stdout
    print(msg, file=stream)
    return rc


if __name__ == "__main__":
    sys.exit(main())
