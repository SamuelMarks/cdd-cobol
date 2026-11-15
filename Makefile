# Makefile for cdd-cobol (Portable ANSI COBOL-85 SDK & CLI)
# Strictly prohibits git push per repository policy.

COBC ?= cobc
AR   ?= ar

INC_FLAGS := -I include -I src/include

WARN_FLAGS := -Wall -Wextra -Werror
WARN_FLAGS += -Wstrict-typing -Wtruncate -Wcall-params
WARN_FLAGS += -Wimplicit-define -Wlinkage -Wunreachable
WARN_FLAGS += -Wcolumn-overflow -Wdangling-text
WARN_FLAGS += -Wobsolete -Warchaic -Wno-unfinished
WARN_FLAGS += -Wterminator

STD_PURE85 := --fixed -std=cobol85 $(WARN_FLAGS) $(INC_FLAGS)

STD_OS85 := --fixed -std=cobol85
STD_OS85 += -freserved-words=default
STD_OS85 += -faccept-display-extensions=ok
STD_OS85 += -fstop-identifier-statement=ok
STD_OS85 += $(WARN_FLAGS) $(INC_FLAGS)

CORE_PURE_SRCS := $(filter-out src/core/CDDIO.cbl,$(wildcard src/core/*.cbl))
CORE_OS_SRCS   := $(wildcard src/core/CDDIO.cbl)
JSON_SRCS      := $(wildcard src/json/*.cbl)
COBOL_SRCS     := $(wildcard src/cobol/*.cbl)
IR_SRCS        := $(wildcard src/ir/*.cbl)
CODEGEN_SRCS   := $(wildcard src/codegen/*.cbl)
SDK_SRCS       := $(wildcard src/sdk/*.cbl)
CLI_SRCS       := $(wildcard src/cli/*.cbl)

SDK_PURE_SRCS  := $(CORE_PURE_SRCS) $(JSON_SRCS) $(COBOL_SRCS) $(IR_SRCS) $(CODEGEN_SRCS) $(SDK_SRCS)
ALL_LIB_SRCS   := $(SDK_PURE_SRCS) $(CORE_OS_SRCS)

SDK_OBJS       := $(patsubst src/%.cbl,build/obj/%.o,$(ALL_LIB_SRCS))
CLI_SUB_SRCS   := $(filter-out src/cli/CDDCLI.cbl,$(CLI_SRCS))
CLI_SUB_OBJS   := $(patsubst src/%.cbl,build/obj/%.o,$(CLI_SUB_SRCS))

UNIT_TESTS     := $(wildcard tests/unit/*.cbl)
DIALECT_TESTS  := $(wildcard tests/dialects/*.cbl)
SYNC_TESTS     := $(wildcard tests/sync/*.cbl)
CLI_TESTS      := $(wildcard tests/cli/*.cbl)
ALL_TESTS      := $(UNIT_TESTS) $(DIALECT_TESTS) $(SYNC_TESTS) $(CLI_TESTS)

.PHONY: all check-git-policy fmt fmt-check lint shields build-tools build-sdk build-cli doc-check type-check test coverage check-all clean

all: check-git-policy build-tools build-sdk build-cli

check-git-policy:
	@bash scripts/check-git-policy.sh

fmt:
	@python3 scripts/format-code.py --fix

fmt-check:
	@python3 scripts/format-code.py --check

lint: build-tools
	@bash scripts/run-lint.sh

shields: build-tools
	@python3 scripts/update-readme-shields.py --doc --test

build/obj/core/CDDIO.o: src/core/CDDIO.cbl
	@mkdir -p $(dir $@)
	$(COBC) -c $(STD_OS85) "$<" -o "$@"

build/obj/cli/%.o: src/cli/%.cbl
	@mkdir -p $(dir $@)
	$(COBC) -c $(STD_OS85) "$<" -o "$@"

build/obj/%.o: src/%.cbl
	@mkdir -p $(dir $@)
	$(COBC) -c $(STD_PURE85) "$<" -o "$@"

build-tools: $(SDK_OBJS)
	@mkdir -p bin build/tools
	@test ! -f tools/CDDDOCC.cbl || $(COBC) -x $(STD_OS85) tools/CDDDOCC.cbl $(SDK_OBJS) -o bin/cdd-doc-check
	@test ! -f tools/CDDTYPC.cbl || $(COBC) -x $(STD_OS85) tools/CDDTYPC.cbl $(SDK_OBJS) -o bin/cdd-type-check
	@test ! -f tools/CDDINST.cbl || $(COBC) -x $(STD_OS85) tools/CDDINST.cbl $(SDK_OBJS) -o bin/cdd-inst
	@test ! -f tools/CDDCOVC.cbl || $(COBC) -x $(STD_OS85) tools/CDDCOVC.cbl $(SDK_OBJS) -o bin/cdd-cov-check

build-sdk: $(SDK_OBJS)
	@mkdir -p build
	@test -z "$(SDK_OBJS)" || $(AR) rcs build/libcddcobol.a $(SDK_OBJS)

build-cli: build-sdk $(CLI_SUB_OBJS)
	@mkdir -p bin
	@test ! -f src/cli/CDDCLI.cbl || $(COBC) -x $(STD_OS85) src/cli/CDDCLI.cbl $(CLI_SUB_OBJS) $(SDK_OBJS) -o bin/cdd-cobol

doc-check: build-tools
	@test -x bin/cdd-doc-check
	@mkdir -p build
	@find include src tools tests -type f -name '*.cbl' -o -type f -name '*.cpy' | sort > build/doc_files.lst
	./bin/cdd-doc-check build/doc_files.lst

type-check: build-tools
	@test -x bin/cdd-type-check
	@mkdir -p build
	@find include src tools -type f -name '*.cbl' -o -type f -name '*.cpy' | sort > build/type_files.lst
	./bin/cdd-type-check build/type_files.lst

test: build-tools build-sdk build-cli
	@bash scripts/run-tests.sh

coverage: build-tools build-cli
	@bash scripts/run-coverage.sh

check-all: check-git-policy doc-check type-check build-sdk build-cli test coverage

clean:
	rm -rf build bin ./*.gcda ./*.gcno ./*.c ./*.c.h ./*.c.l.h ./*.i
