      * @file CDDCLIA.cbl
      * @brief CLI subcommand and option parser for cdd-cobol.
      * @module CDDCLIA
      * @details Parses command-line arguments (from OS ARGUMENT-VALUE
      *          or in-memory CDD-ARGV-OVR) across emit, parse, serde,
      *          and sync subcommands and all dialect/format/mode flags
      *          into CDD-CLI-CFG and CDD-SDK-OPTS.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCLIA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-dial.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-ARG-STATE Argument iteration and conflict trackers.
       01  WS-ARG-STATE.
      * @field WS-ARGC Total argument count.
           05  WS-ARGC                   PIC 9(4) COMP VALUE 0.
      * @field WS-AI 1-based argument cursor.
           05  WS-AI                     PIC 9(4) COMP VALUE 1.
      * @field WS-ALEN Trimmed length of current argument.
           05  WS-ALEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-VLEN Trimmed length of option value argument.
           05  WS-VLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-SAW-COMPACT 1 if --compact passed.
           05  WS-SAW-COMPACT            PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-PRETTY 1 if --pretty passed.
           05  WS-SAW-PRETTY             PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-OUT 1 if -o / --output explicitly passed.
           05  WS-SAW-OUT                PIC 9(2) COMP VALUE 0.
      * @record WS-CUR-ARG Current raw argument string.
       01  WS-CUR-ARG                    PIC X(512) VALUE SPACES.
      * @record WS-VAL-ARG Value argument string for binary flags.
       01  WS-VAL-ARG                    PIC X(512) VALUE SPACES.
      * @record WS-UP-VAL Uppercased value scratch string.
       01  WS-UP-VAL                     PIC X(64) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-cli.cpy".
       COPY "cdd-opts.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDCLIA Parse CLI arguments into CDD-CLI-CFG & OPTS.
      * @param [inout] CDD-CLI-CFG Output CLI command and path config.
      * @param [inout] CDD-SDK-OPTS Output SDK dialect/mode options.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-CLI-CFG and CDD-SDK-OPTS.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-CLI-NO-SUBCOMMAND,
      *         CDD-ERR-CLI-UNKNOWN-SUBCOMMAND, CDD-ERR-CLI-UNKNOWN-FLAG,
      *         CDD-ERR-CLI-MISSING-VALUE, CDD-ERR-CLI-CONFLICT-FLAGS.
       PROCEDURE DIVISION USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, reset defaults, scan args.
       MAIN-ENTRY.
           IF NOT CDD-CLI-TAG-VALID OR NOT CDD-OPT-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-CLI-CFG or CDD-SDK-OPTS tag"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-CLI-DEFAULTS
           PERFORM FETCH-ARGC
           IF WS-ARGC = 0
               MOVE 5001 TO CDD-ERR-CODE
               MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
               MOVE "Missing subcommand (emit|parse|serde|sync)"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM PARSE-ALL-ARGS
           IF CDD-OK
               PERFORM VALIDATE-FINAL-CLI-RULES
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-CLI-DEFAULTS Initialize default CLI & SDK opts.
       INIT-CLI-DEFAULTS.
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-CLI-CMD
           MOVE 0 TO CDD-CLI-HELP-TOPIC
           MOVE 0 TO CDD-CLI-IN-PLACE
           MOVE 1 TO CDD-CLI-IN-LEN
           MOVE "-" TO CDD-CLI-IN-PATH
           MOVE 1 TO CDD-CLI-OUT-LEN
           MOVE "-" TO CDD-CLI-OUT-PATH
           MOVE 0 TO CDD-CLI-COB-LEN
           MOVE SPACES TO CDD-CLI-COB-PATH
           MOVE 0 TO CDD-CLI-JSN-LEN
           MOVE SPACES TO CDD-CLI-JSN-PATH
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 0 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-UDT-KIND
           MOVE 0 TO CDD-OPT-SERDE-KIND
           MOVE 0 TO CDD-OPT-JSON-MODE
           MOVE 1 TO CDD-OPT-CASE
           MOVE 1 TO CDD-OPT-STYLE
           MOVE 0 TO CDD-OPT-PURE-COBOL
           MOVE 0 TO CDD-OPT-VENDOR-JSON
           MOVE 0 TO CDD-OPT-X-COBOL
           MOVE 0 TO CDD-OPT-SYNC-SERDE
           MOVE 0 TO CDD-OPT-C60-VERBOSE
           MOVE 0 TO CDD-OPT-NAME-LEN
           MOVE SPACES TO CDD-OPT-TARGET-NAME
           MOVE 0 TO WS-SAW-COMPACT
           MOVE 0 TO WS-SAW-PRETTY
           MOVE 0 TO WS-SAW-OUT.
      * @paragraph FETCH-ARGC Read argument count from mem or OS.
       FETCH-ARGC.
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE CDD-AGV-COUNT TO WS-ARGC
           ELSE
               ACCEPT WS-ARGC FROM ARGUMENT-NUMBER
               END-ACCEPT
           END-IF.
      * @paragraph FETCH-ARG-AT-AI Load argument WS-AI into WS-CUR-ARG.
       FETCH-ARG-AT-AI.
           MOVE SPACES TO WS-CUR-ARG
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE CDD-AGV-ITEM(WS-AI) TO WS-CUR-ARG(1:256)
           ELSE
               DISPLAY WS-AI UPON ARGUMENT-NUMBER
               END-DISPLAY
               ACCEPT WS-CUR-ARG FROM ARGUMENT-VALUE
               END-ACCEPT
           END-IF
           MOVE 512 TO WS-ALEN
           PERFORM UNTIL WS-ALEN = 0
               OR WS-CUR-ARG(WS-ALEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-ALEN
               END-SUBTRACT
           END-PERFORM.
      * @paragraph REQUIRE-NEXT-VALUE Advance WS-AI and load WS-VAL-ARG.
       REQUIRE-NEXT-VALUE.
           IF WS-AI >= WS-ARGC
               MOVE 5004 TO CDD-ERR-CODE
               MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
               MOVE "CLI option requires a value argument"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO WS-AI
               END-ADD
               PERFORM FETCH-ARG-AT-AI
               IF WS-ALEN = 0
                   MOVE 5004 TO CDD-ERR-CODE
                   MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                   MOVE "Empty CLI option value" TO CDD-ERR-MESSAGE
               ELSE
                   MOVE WS-CUR-ARG TO WS-VAL-ARG
                   MOVE WS-ALEN TO WS-VLEN
                   MOVE SPACES TO WS-UP-VAL
                   IF WS-VLEN <= 64
                       MOVE WS-VAL-ARG(1:WS-VLEN)
                           TO WS-UP-VAL(1:WS-VLEN)
                   ELSE
                       MOVE WS-VAL-ARG(1:64) TO WS-UP-VAL(1:64)
                   END-IF
                   INSPECT WS-UP-VAL
                       CONVERTING "abcdefghijklmnopqrstuvwxyz"
                       TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               END-IF
           END-IF.
      * @paragraph PARSE-ALL-ARGS Iterate WS-AI from 1 to WS-ARGC.
       PARSE-ALL-ARGS.
           MOVE 1 TO WS-AI
           PERFORM UNTIL WS-AI > WS-ARGC OR NOT CDD-OK
               PERFORM FETCH-ARG-AT-AI
               IF WS-AI = 1 AND WS-CUR-ARG(1:1) NOT = "-"
                   PERFORM PARSE-SUBCOMMAND-TOKEN
               ELSE
                   PERFORM PARSE-OPTION-FLAG
               END-IF
               ADD 1 TO WS-AI
               END-ADD
           END-PERFORM.
      * @paragraph PARSE-SUBCOMMAND-TOKEN Map emit|parse|serde|sync.
       PARSE-SUBCOMMAND-TOKEN.
           EVALUATE WS-CUR-ARG
               WHEN "emit"
                   MOVE 1 TO CDD-CLI-CMD
               WHEN "parse"
                   MOVE 2 TO CDD-CLI-CMD
               WHEN "serde"
                   MOVE 3 TO CDD-CLI-CMD
               WHEN "sync"
                   MOVE 4 TO CDD-CLI-CMD
               WHEN OTHER
                   MOVE 5002 TO CDD-ERR-CODE
                   MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                   MOVE "Unknown subcommand (expected emit|parse|serde|s
      -                "ync)" TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph PARSE-OPTION-FLAG Handle global and subcommand flags.
       PARSE-OPTION-FLAG.
           EVALUATE WS-CUR-ARG
               WHEN "-h"
               WHEN "--help"
                   MOVE CDD-CLI-CMD TO CDD-CLI-HELP-TOPIC
                   MOVE 5 TO CDD-CLI-CMD
               WHEN "-V"
               WHEN "--version"
                   MOVE 6 TO CDD-CLI-CMD
               WHEN "-i"
               WHEN "--input"
                   PERFORM REQUIRE-NEXT-VALUE
                   IF CDD-OK
                       MOVE WS-VLEN TO CDD-CLI-IN-LEN
                       MOVE WS-VAL-ARG TO CDD-CLI-IN-PATH
                   END-IF
               WHEN "-o"
               WHEN "--output"
                   PERFORM REQUIRE-NEXT-VALUE
                   IF CDD-OK
                       MOVE 1 TO WS-SAW-OUT
                       MOVE WS-VLEN TO CDD-CLI-OUT-LEN
                       MOVE WS-VAL-ARG TO CDD-CLI-OUT-PATH
                   END-IF
               WHEN "-c"
               WHEN "--cobol"
                   PERFORM REQUIRE-NEXT-VALUE
                   IF CDD-OK
                       MOVE WS-VLEN TO CDD-CLI-COB-LEN
                       MOVE WS-VAL-ARG TO CDD-CLI-COB-PATH
                   END-IF
               WHEN "-j"
               WHEN "--json"
                   PERFORM REQUIRE-NEXT-VALUE
                   IF CDD-OK
                       MOVE WS-VLEN TO CDD-CLI-JSN-LEN
                       MOVE WS-VAL-ARG TO CDD-CLI-JSN-PATH
                   END-IF
               WHEN "--in-place"
                   MOVE 1 TO CDD-CLI-IN-PLACE
               WHEN "-d"
               WHEN "--dialect"
                   PERFORM PARSE-DIALECT-OPT
               WHEN OTHER
                   PERFORM PARSE-OPTION-FLAG-PART2
           END-EVALUATE.
      * @paragraph PARSE-OPTION-FLAG-PART2 Handle remaining CLI flags.
       PARSE-OPTION-FLAG-PART2.
           EVALUATE WS-CUR-ARG
               WHEN "--udt-kind"
                   PERFORM PARSE-UDT-KIND-OPT
               WHEN "--serde-kind"
                   PERFORM PARSE-SERDE-KIND-OPT
               WHEN "--format"
                   PERFORM PARSE-FORMAT-OPT
               WHEN "--json-mode"
                   PERFORM PARSE-JSON-MODE-OPT
               WHEN "--name"
               WHEN "--udt"
                   PERFORM REQUIRE-NEXT-VALUE
                   IF CDD-OK
                       IF WS-VLEN > 63
                           MOVE 63 TO CDD-OPT-NAME-LEN
                       ELSE
                           COMPUTE CDD-OPT-NAME-LEN = WS-VLEN
                           END-COMPUTE
                       END-IF
                       MOVE WS-VAL-ARG(1:CDD-OPT-NAME-LEN)
                           TO CDD-OPT-TARGET-NAME
                   END-IF
               WHEN "--case"
                   PERFORM PARSE-CASE-OPT
               WHEN "--sync-serde"
                   MOVE 1 TO CDD-OPT-SYNC-SERDE
               WHEN "--no-sync-serde"
                   MOVE 2 TO CDD-OPT-SYNC-SERDE
               WHEN "--pure-cobol"
                   MOVE 1 TO CDD-OPT-PURE-COBOL
               WHEN "--vendor-json"
                   MOVE 1 TO CDD-OPT-VENDOR-JSON
               WHEN "--x-cobol"
                   MOVE 1 TO CDD-OPT-X-COBOL
               WHEN "--cobol60-verbose-size"
                   MOVE 1 TO CDD-OPT-C60-VERBOSE
               WHEN "--compact"
                   MOVE 1 TO WS-SAW-COMPACT
                   MOVE 2 TO CDD-OPT-STYLE
               WHEN "--pretty"
                   MOVE 1 TO WS-SAW-PRETTY
                   MOVE 1 TO CDD-OPT-STYLE
               WHEN OTHER
                   MOVE 5003 TO CDD-ERR-CODE
                   MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                   MOVE "Unknown CLI option flag" TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph PARSE-DIALECT-OPT Validate --dialect via CDDDIAL.
       PARSE-DIALECT-OPT.
           PERFORM REQUIRE-NEXT-VALUE
           IF CDD-OK
               MOVE "CDDDLR01" TO CDD-DLR-TAG
               MOVE 3 TO CDD-DLR-OP
               IF WS-VLEN > 64
                   MOVE 64 TO CDD-DLR-WORD-LEN
               ELSE
                   COMPUTE CDD-DLR-WORD-LEN = WS-VLEN
                   END-COMPUTE
               END-IF
               MOVE WS-VAL-ARG(1:CDD-DLR-WORD-LEN) TO CDD-DLR-WORD
               CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
               END-CALL
               IF CDD-OK
                   MOVE CDD-DLR-DIALECT TO CDD-OPT-DIALECT
               END-IF
           END-IF.
      * @paragraph PARSE-UDT-KIND-OPT Parse record|typedef|class.
       PARSE-UDT-KIND-OPT.
           PERFORM REQUIRE-NEXT-VALUE
           IF CDD-OK
               EVALUATE WS-UP-VAL
                   WHEN "RECORD"
                       MOVE 1 TO CDD-OPT-UDT-KIND
                   WHEN "TYPEDEF"
                       MOVE 2 TO CDD-OPT-UDT-KIND
                   WHEN "CLASS"
                       MOVE 3 TO CDD-OPT-UDT-KIND
                   WHEN OTHER
                       MOVE 5003 TO CDD-ERR-CODE
                       MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                       MOVE "Invalid --udt-kind value"
                           TO CDD-ERR-MESSAGE
               END-EVALUATE
           END-IF.
      * @paragraph PARSE-SERDE-KIND-OPT Parse auto|section|subprogram..
       PARSE-SERDE-KIND-OPT.
           PERFORM REQUIRE-NEXT-VALUE
           IF CDD-OK
               EVALUATE WS-UP-VAL
                   WHEN "AUTO"
                       MOVE 0 TO CDD-OPT-SERDE-KIND
                   WHEN "SECTION"
                       MOVE 1 TO CDD-OPT-SERDE-KIND
                   WHEN "SUBPROGRAM"
                       MOVE 2 TO CDD-OPT-SERDE-KIND
                   WHEN "FUNCTION"
                       MOVE 3 TO CDD-OPT-SERDE-KIND
                   WHEN "METHOD"
                       MOVE 4 TO CDD-OPT-SERDE-KIND
                   WHEN OTHER
                       MOVE 5003 TO CDD-ERR-CODE
                       MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                       MOVE "Invalid --serde-kind value"
                           TO CDD-ERR-MESSAGE
               END-EVALUATE
           END-IF.
      * @paragraph PARSE-FORMAT-OPT Parse auto|fixed|free.
       PARSE-FORMAT-OPT.
           PERFORM REQUIRE-NEXT-VALUE
           IF CDD-OK
               EVALUATE WS-UP-VAL
                   WHEN "AUTO"
                       MOVE 0 TO CDD-OPT-FORMAT
                   WHEN "FIXED"
                       MOVE 1 TO CDD-OPT-FORMAT
                   WHEN "FREE"
                       MOVE 2 TO CDD-OPT-FORMAT
                   WHEN OTHER
                       MOVE 5003 TO CDD-ERR-CODE
                       MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                       MOVE "Invalid --format value"
                           TO CDD-ERR-MESSAGE
               END-EVALUATE
           END-IF.
      * @paragraph PARSE-JSON-MODE-OPT Parse auto|schema|sample.
       PARSE-JSON-MODE-OPT.
           PERFORM REQUIRE-NEXT-VALUE
           IF CDD-OK
               EVALUATE WS-UP-VAL
                   WHEN "AUTO"
                       MOVE 0 TO CDD-OPT-JSON-MODE
                   WHEN "SCHEMA"
                       MOVE 1 TO CDD-OPT-JSON-MODE
                   WHEN "SAMPLE"
                       MOVE 2 TO CDD-OPT-JSON-MODE
                   WHEN OTHER
                       MOVE 5003 TO CDD-ERR-CODE
                       MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                       MOVE "Invalid --json-mode value"
                           TO CDD-ERR-MESSAGE
               END-EVALUATE
           END-IF.
      * @paragraph PARSE-CASE-OPT Parse camel|snake|kebab|preserve.
       PARSE-CASE-OPT.
           PERFORM REQUIRE-NEXT-VALUE
           IF CDD-OK
               EVALUATE WS-UP-VAL
                   WHEN "CAMEL"
                       MOVE 1 TO CDD-OPT-CASE
                   WHEN "SNAKE"
                       MOVE 2 TO CDD-OPT-CASE
                   WHEN "KEBAB"
                       MOVE 3 TO CDD-OPT-CASE
                   WHEN "PRESERVE"
                       MOVE 4 TO CDD-OPT-CASE
                   WHEN OTHER
                       MOVE 5003 TO CDD-ERR-CODE
                       MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                       MOVE "Invalid --case value"
                           TO CDD-ERR-MESSAGE
               END-EVALUATE
           END-IF.
      * @paragraph VALIDATE-FINAL-CLI-RULES Check conflicts & sync reqs.
       VALIDATE-FINAL-CLI-RULES.
           IF CDD-CLI-CMD = 0
               MOVE 5001 TO CDD-ERR-CODE
               MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
               MOVE "No subcommand specified" TO CDD-ERR-MESSAGE
           ELSE
               IF WS-SAW-COMPACT = 1 AND WS-SAW-PRETTY = 1
                   MOVE 5005 TO CDD-ERR-CODE
                   MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                   MOVE "--compact and --pretty are mutually exclusive"
                       TO CDD-ERR-MESSAGE
               ELSE
                   IF CDD-CLI-IN-PLACE = 1 AND WS-SAW-OUT = 1
                       MOVE 5005 TO CDD-ERR-CODE
                       MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                       MOVE "--in-place conflicts with --output"
                           TO CDD-ERR-MESSAGE
                   END-IF
               END-IF
           END-IF
           IF CDD-OK AND CDD-CLI-CMD-SYNC
               IF CDD-CLI-COB-LEN = 0 OR CDD-CLI-JSN-LEN = 0
                   MOVE 5004 TO CDD-ERR-CODE
                   MOVE "CDDCLIA" TO CDD-ERR-SUBPROG
                   MOVE "sync requires both --cobol and --json paths"
                       TO CDD-ERR-MESSAGE
               END-IF
           END-IF.
