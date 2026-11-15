# cdd-cobol

[![License](https://img.shields.io/badge/license-Apache--2.0%20OR%20MIT-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![CI & Release](https://github.com/SamuelMarks/cdd-cobol/actions/workflows/ci.yml/badge.svg)](https://github.com/SamuelMarks/cdd-cobol/actions/workflows/ci.yml)
[![100% doc coverage](https://img.shields.io/badge/doc%20coverage-100%25-brightgreen.svg)](#quality-gates) [![100% test coverage](https://img.shields.io/badge/test%20coverage-100%25-brightgreen.svg)](#quality-gates)

Portable ANSI COBOL-85 SDK (`libcddcobol.a`) and CLI (`cdd-cobol`) for bidirectional JSON / JSON Schema (Draft 2020-12 / OpenAPI 3.x) and COBOL user-defined type (UDT) code generation, serialization/deserialization (`serde`), and lossless trivia-preserving surgical synchronization (`sync`) across **all 10 historical and modern COBOL standards (1960–2023)**.

## Supported COBOL Dialects

| Dialect Flag | Standard | Format & Trivia | UDT Construct | Serde Idiom |
| :--- | :--- | :--- | :--- | :--- |
| `cobol60` | CODASYL 1960 | Fixed; `REMARKS.`, `NOTE`, cols 1–6 & 73–80 | `01` record, `PICTURE IS` | `PERFORM` sections + `EXAMINE` |
| `cobol61` | CODASYL 1961 | Fixed; `*` col 7, `REMARKS.`, `NOTE` | `01` record, `REDEFINES` | `PERFORM` sections / early `CALL` |
| `cobol61ext` | CODASYL 1962 Ext | Fixed; `*` col 7, `66 RENAMES` | `01` record, `66 RENAMES` | `CALL ... USING` + `COMPUTE` |
| `cobol65` | CODASYL 1965 | Fixed; `*` col 7 | `OCCURS ... INDEXED BY` | `CALL ... USING` + `SEARCH` |
| `cobol68` | ANSI X3.23-1968 | Fixed; `*` and `D` col 7 | `01`–`49`, `66`, `77`, `88`, `COMP` | `LINKAGE SECTION` + `SEARCH ALL` |
| `cobol74` | ANSI X3.23-1974 | Fixed; `*` and `/` col 7 | `OCCURS ... DEPENDING ON` | `INSPECT`, `STRING`, `UNSTRING` |
| `cobol85` | ANSI X3.23-1985 | Fixed; `*` and `/` col 7 | `GLOBAL`/`EXTERNAL`, table `VALUE` | `END-*` scope terminators, ref-mod |
| `cobol2002` | ISO/IEC 1989:2002 | Fixed or Free; `*>` inline | `IS TYPEDEF STRONG`, `CLASS-ID` | `FUNCTION-ID`, OO `METHOD-ID` |
| `cobol2014` | ISO/IEC 1989:2014 | Fixed or Free; `*>` inline | `OCCURS DYNAMIC`, `FLOAT-DECIMAL` | Dynamic-capacity tables, `TRIM` |
| `cobol2023` | ISO/IEC 1989:2023 | Fixed or Free; `*>` inline | `PIC U` UTF-8, `DYNAMIC` items | ISO `JSON GENERATE` / `JSON PARSE` |

## CLI Usage

```sh
# 0. emit: JSON Schema or raw JSON -> COBOL record / typedef / class
./bin/cdd-cobol emit -i schema.json -o record.cpy -d cobol85 --udt-kind record

# 1. parse: COBOL source or copybook -> JSON Schema or sample JSON
./bin/cdd-cobol parse -i record.cpy -o schema.json --json-mode schema --pretty

# 2. serde: Generate dialect-idiomatic to_json / from_json routines
./bin/cdd-cobol serde -i schema.json -o serde.cbl -d cobol85 --serde-kind subprogram

# 3. sync: Surgically patch existing COBOL UDT & serde while preserving 100% of trivia
./bin/cdd-cobol sync -c existing.cbl -j updated-schema.json --in-place
```

## SDK Usage

Include `include/cdd-cobol.cpy` (compiling with `-I include -I src/include`) and link against `build/libcddcobol.a`:

- `CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF CDD-ERROR`
- `CALL "CDDPARS" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF CDD-ERROR`
- `CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF CDD-ERROR`
- `CALL "CDDSYN" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF WS-OUT-BUF CDD-ERROR`

## Build, Format, Lint, Test & Pre-Commit Hooks

### Prerequisites

- GnuCOBOL (`cobc` 3.x+)
- LCOV (`lcov` & `genhtml` 2.x+)
- Python 3.9+ and [`pre-commit`](https://pre-commit.com/)
- `shellcheck` (optional, used during linting)

### Common Commands

```sh
make fmt          # Auto-format COBOL, Shell, Python, JSON, Makefile, YAML, & Markdown
make fmt-check    # Verify code formatting without modifying files
make all          # Build quality tools, libcddcobol.a SDK, and bin/cdd-cobol CLI
make lint         # Run git policy, format-check, ShellCheck, 100% doc-check, & type-check
make test         # Run unit, 10-dialect golden/exec, surgical sync, and CLI E2E suites
make coverage     # Enforce 100.00% function, line, and branch coverage
make shields      # Recompute & update '% doc coverage' and '% test coverage' shields
make check-all    # Run full policy, lint, build, test, and 100% coverage pipeline
```

### PyPI `pre-commit` Hooks

Install and run the configured git `pre-commit` hooks:

```sh
pre-commit install
pre-commit run --all-files
```

The `.pre-commit-config.yaml` pipeline runs in order:
1. **`format-fix`** — Auto-formats repository source and configuration files in-place.
2. **`format-check`** — Rechecks formatting, UTF-8 encoding, LF line endings, and 72-column COBOL bounds.
3. **`build`** — Builds self-hosted quality tools, `build/libcddcobol.a`, and `bin/cdd-cobol`.
4. **`lint`** — Runs git policy verification, ShellCheck, JSON/YAML checks, `cdd-doc-check`, and `cdd-type-check`.
5. **`test`** — Executes all test suites and enforces 100% function, line, and branch coverage.
6. **`update-doc-coverage-shield`** — Computes and adds/updates the `% doc coverage` shield in `README.md`.
7. **`update-test-coverage-shield`** — Computes and adds/updates the `% test coverage` shield in `README.md`.

## Quality Gates

- **100% Documentation Coverage:** Enforced across every file, module, class/typedef/record, field, subprogram/section/paragraph, and `USING` parameter by `tools/CDDDOCC.cbl` (`bin/cdd-doc-check`).
- **100% Test Coverage:** Enforced across every COBOL subprogram/section/paragraph (function), executable statement (line), and decision arm (`IF`/`ELSE`, `EVALUATE`, `SEARCH`, `SIZE ERROR` branch) by `tools/CDDINST.cbl` + `tools/CDDCOVC.cbl` (`bin/cdd-cov-check`) and `cobc -g --coverage` + `lcov`.
- **Strong Typing & Unified Error Enum:** Enforced statically via `tools/CDDTYPC.cbl` (`bin/cdd-type-check`) and `cobc -Wall -Wextra -Werror -Wstrict-typing -Wtruncate -Wcall-params` with tagged envelopes and a single unified `01 CDD-ERROR` copybook (`src/include/cdd-err.cpy`).

---

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or <https://www.apache.org/licenses/Apache-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <https://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
