#!/usr/bin/env bash
set -euo pipefail

COBC="${COBC:-cobc}"
STD_OS85="--fixed -std=cobol85 -freserved-words=default -faccept-display-extensions=ok -fstop-identifier-statement=ok -Wall -Wextra -Werror -Wstrict-typing -Wtruncate -Wcall-params -Wimplicit-define -Wlinkage -Wunreachable -Wcolumn-overflow -Wdangling-text -Wobsolete -Warchaic -Wno-unfinished -Wterminator -I include -I src/include"

mkdir -p build/test-bin build/test-drivers

LIB_OBJS=()
while IFS= read -r o; do
    [ -n "$o" ] && LIB_OBJS+=("$o")
done < <(find build/obj -type f -name '*.o' ! -name 'CDDCLI.o' 2>/dev/null | sort)

for m in src/cli/CDDCLI.cbl tools/CDDDOCC.cbl tools/CDDTYPC.cbl tools/CDDINST.cbl tools/CDDCOVC.cbl; do
    if [ -f "$m" ]; then
        base="$(basename "$m" .cbl)"
        sed -e "s/PROGRAM-ID\. ${base}\./PROGRAM-ID. M${base}./g" -e "s/STOP RUN RETURNING [A-Z0-9-]*\./EXIT PROGRAM./g" -e "s/STOP RUN\./EXIT PROGRAM./g" "$m" > "build/test-drivers/M${base}.cbl"
        $COBC -c $STD_OS85 "build/test-drivers/M${base}.cbl" -o "build/test-drivers/M${base}.o"
        LIB_OBJS+=("build/test-drivers/M${base}.o")
    fi
done

if [ -f tools/CDDCOVR.cbl ]; then
    $COBC -c $STD_OS85 tools/CDDCOVR.cbl -o build/test-drivers/CDDCOVR.o
    LIB_OBJS+=("build/test-drivers/CDDCOVR.o")
fi

TESTS=()
while IFS= read -r t; do
    [ -n "$t" ] && TESTS+=("$t")
done < <(find tests -maxdepth 2 -type f -name '*.cbl' 2>/dev/null | sort)

if [ "${#TESTS[@]}" -eq 0 ]; then
    echo "[TEST] No test files found yet."
    exit 0
fi

for t in "${TESTS[@]}"; do
    b="$(basename "$t" .cbl)"
    echo "[TEST] Running $b..."
    if [ "${#LIB_OBJS[@]}" -gt 0 ]; then
        $COBC -x $STD_OS85 "$t" "${LIB_OBJS[@]}" -o "build/test-bin/$b"
    else
        $COBC -x $STD_OS85 "$t" -o "build/test-bin/$b"
    fi
    if [ -f "tests/fixtures/${b}.stdin" ]; then
        CDD_CLI_BIN=bin/cdd-cobol "./build/test-bin/$b" < "tests/fixtures/${b}.stdin"
    else
        CDD_CLI_BIN=bin/cdd-cobol "./build/test-bin/$b"
    fi
done
echo "[OK] All ${#TESTS[@]} test suite(s) passed."
