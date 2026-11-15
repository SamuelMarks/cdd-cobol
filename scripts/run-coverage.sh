#!/usr/bin/env bash
set -euo pipefail

COBC="${COBC:-cobc}"
LCOV="${LCOV:-lcov}"
GENHTML="${GENHTML:-genhtml}"

STD_OS85="--fixed -std=cobol85 -freserved-words=default -faccept-display-extensions=ok -fstop-identifier-statement=ok -Wall -Wextra -Werror -Wstrict-typing -Wtruncate -Wcall-params -Wimplicit-define -Wlinkage -Wunreachable -Wcolumn-overflow -Wdangling-text -Wobsolete -Warchaic -Wno-unfinished -Wterminator -I include -I src/include"

if "$COBC" --help 2>&1 | grep -q -- '--coverage'; then
    COV_CFLAGS=(-g --coverage)
    COV_LDFLAGS=(-g --coverage)
else
    COV_CFLAGS=(-g -O0 -A --coverage)
    COV_LDFLAGS=(-g -O0 -A --coverage -Q --coverage)
fi

rm -rf build/inst build/cov build/lcov
rm -f ./*.gcda ./*.gcno
mkdir -p build/inst build/cov build/lcov build/lcov/bin

find src -type f -name '*.cbl' | sort > build/cov/src_files.lst

./bin/cdd-inst build/cov/src_files.lst build/inst build/cov/manifest.dat

INST_LIB_OBJS=()
while IFS= read -r f; do
    [ -z "$f" ] && continue
    base="$(basename "$f" .cbl)"
    case "$base" in
        CDDCLI)
            sed -e "s/PROGRAM-ID\. ${base}\./PROGRAM-ID. M${base}./g" -e "s/STOP RUN RETURNING .*\./EXIT PROGRAM./g" -e "s/STOP RUN\./EXIT PROGRAM./g" "build/inst/${base}.cbl" > "build/inst/M${base}.cbl"
            $COBC -c $STD_OS85 -I build/inst "build/inst/M${base}.cbl" -o "build/inst/M${base}.o"
            INST_LIB_OBJS+=("build/inst/M${base}.o")
            ;;
        *)
            $COBC -c $STD_OS85 -I build/inst "build/inst/${base}.cbl" -o "build/inst/${base}.o"
            INST_LIB_OBJS+=("build/inst/${base}.o")
            ;;
    esac
done < build/cov/src_files.lst

for m in tools/CDDDOCC.cbl tools/CDDTYPC.cbl tools/CDDINST.cbl tools/CDDCOVC.cbl; do
    if [ -f "$m" ]; then
        base="$(basename "$m" .cbl)"
        sed -e "s/PROGRAM-ID\. ${base}\./PROGRAM-ID. M${base}./g" -e "s/STOP RUN RETURNING .*\./EXIT PROGRAM./g" -e "s/STOP RUN\./EXIT PROGRAM./g" "$m" > "build/inst/M${base}.cbl"
        $COBC -c $STD_OS85 -I build/inst "build/inst/M${base}.cbl" -o "build/inst/M${base}.o"
        INST_LIB_OBJS+=("build/inst/M${base}.o")
    fi
done

$COBC -c $STD_OS85 -I build/inst tools/CDDCOVR.cbl -o build/inst/CDDCOVR.o

if [ -f build/inst/CDDCLI.cbl ]; then
    mkdir -p build/cov/bin
    NON_MAIN_INST=()
    for o in "${INST_LIB_OBJS[@]}"; do
        case "$o" in
            */MCDD*.o) ;;
            *) NON_MAIN_INST+=("$o") ;;
        esac
    done
    awk -v q="'" '{
        if ($0 ~ /^       STOP RUN/ || $0 ~ /^           STOP RUN/) {
            print "           CALL " q "CDDCOVR" q
            print "           END-CALL"
        }
        print $0
    }' build/inst/CDDCLI.cbl > build/inst/CDDCLI_BIN.cbl
    $COBC -x $STD_OS85 -I build/inst build/inst/CDDCLI_BIN.cbl "${NON_MAIN_INST[@]}" build/inst/CDDCOVR.o -o build/cov/bin/cdd-cobol
fi

ALL_TESTS=()
while IFS= read -r t; do
    [ -n "$t" ] && ALL_TESTS+=("$t")
done < <(find tests -maxdepth 2 -type f -name '*.cbl' | sort)

for t in "${ALL_TESTS[@]}"; do
    b="$(basename "$t" .cbl)"
    awk -v q="'" '{
        if ($0 ~ /^       STOP RUN/ || $0 ~ /^           STOP RUN/) {
            print "           CALL " q "CDDCOVR" q
            print "           END-CALL"
        }
        print $0
    }' "$t" > "build/cov/${b}.cbl"
    $COBC -x $STD_OS85 -I build/inst "build/cov/${b}.cbl" "${INST_LIB_OBJS[@]}" build/inst/CDDCOVR.o -o "build/cov/$b"
    if [ -f "tests/fixtures/${b}.stdin" ]; then
        CDD_CLI_BIN=build/cov/bin/cdd-cobol "./build/cov/$b" < "tests/fixtures/${b}.stdin"
    else
        CDD_CLI_BIN=build/cov/bin/cdd-cobol "./build/cov/$b"
    fi
done

LCOV_OBJS=()
while IFS= read -r f; do
    [ -z "$f" ] && continue
    base="$(basename "$f" .cbl)"
    case "$base" in
        CDDCLI)
            sed -e "s/PROGRAM-ID\. ${base}\./PROGRAM-ID. M${base}./g" -e "s/STOP RUN RETURNING .*\./EXIT PROGRAM./g" -e "s/STOP RUN\./EXIT PROGRAM./g" "$f" > "build/lcov/M${base}.cbl"
            $COBC -c $STD_OS85 "${COV_CFLAGS[@]}" "build/lcov/M${base}.cbl" -o "build/lcov/M${base}.o"
            LCOV_OBJS+=("build/lcov/M${base}.o")
            ;;
        *)
            $COBC -c $STD_OS85 "${COV_CFLAGS[@]}" "$f" -o "build/lcov/${base}.o"
            LCOV_OBJS+=("build/lcov/${base}.o")
            ;;
    esac
done < build/cov/src_files.lst

for m in tools/CDDDOCC.cbl tools/CDDTYPC.cbl tools/CDDINST.cbl tools/CDDCOVC.cbl; do
    if [ -f "$m" ]; then
        base="$(basename "$m" .cbl)"
        sed -e "s/PROGRAM-ID\. ${base}\./PROGRAM-ID. M${base}./g" -e "s/STOP RUN RETURNING .*\./EXIT PROGRAM./g" -e "s/STOP RUN\./EXIT PROGRAM./g" "$m" > "build/lcov/M${base}.cbl"
        $COBC -c $STD_OS85 "build/lcov/M${base}.cbl" -o "build/lcov/M${base}.o"
        LCOV_OBJS+=("build/lcov/M${base}.o")
    fi
done

$COBC -c $STD_OS85 tools/CDDCOVR.cbl -o build/lcov/CDDCOVR.o
LCOV_OBJS+=("build/lcov/CDDCOVR.o")

if [ -f src/cli/CDDCLI.cbl ]; then
    NON_MAIN_LCOV=()
    for o in "${LCOV_OBJS[@]}"; do
        case "$o" in
            */MCDD*.o|*/CDDCOVR.o) ;;
            *) NON_MAIN_LCOV+=("$o") ;;
        esac
    done
    $COBC -x $STD_OS85 "${COV_LDFLAGS[@]}" src/cli/CDDCLI.cbl "${NON_MAIN_LCOV[@]}" -o build/lcov/bin/cdd-cobol
fi

for t in "${ALL_TESTS[@]}"; do
    b="$(basename "$t" .cbl)"
    $COBC -x $STD_OS85 "${COV_LDFLAGS[@]}" "$t" "${LCOV_OBJS[@]}" -o "build/lcov/$b"
    if [ -f "tests/fixtures/${b}.stdin" ]; then
        CDD_CLI_BIN=build/lcov/bin/cdd-cobol "./build/lcov/$b" < "tests/fixtures/${b}.stdin"
    else
        CDD_CLI_BIN=build/lcov/bin/cdd-cobol "./build/lcov/$b"
    fi
done

if [ "${#LCOV_OBJS[@]}" -gt 0 ]; then
    LCOV_IGN="source,inconsistent,unsupported,format,unused,mismatch,negative,branch,gcov,graph,corrupt,empty,count"
    GENHTML_IGN="source,inconsistent,unsupported,format,unused,mismatch,negative,branch,corrupt,empty,count"
    "$LCOV" --capture --directory build/lcov --base-directory . --output-file build/cov/raw.info --rc branch_coverage=1 --ignore-errors "$LCOV_IGN" >/dev/null 2>&1
    "$LCOV" --extract build/cov/raw.info '*/src/*.cbl' '*/src/*/*.cbl' '*/build/lcov/MCDDCLI.cbl' --output-file build/cov/coverage.info --rc branch_coverage=1 --ignore-errors "$LCOV_IGN" >/dev/null 2>&1
    "$GENHTML" build/cov/coverage.info --output-directory build/cov/html --rc branch_coverage=1 --ignore-errors "$GENHTML_IGN" >/dev/null 2>&1 || true
fi

./bin/cdd-cov-check build/cov/manifest.dat build/cov/hits.dat build/cov/coverage.info
rm -f ./*.gcda ./*.gcno ./*.c ./*.c.h ./*.c.l.h ./*.i
