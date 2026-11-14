      * @file CDDERRF.cbl
      * @brief Unified error initializer, constructor, and formatter.
      * @module CDDERRF
      * @details Provides deterministic initialization, construction,
      *          and human-readable formatting for every variant of the
      *          single unified CDD-ERROR enum without ever aborting.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDERRF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-STATE Internal formatting workspace for CDDERRF.
       01  WS-STATE.
      * @field WS-LABEL Short canonical label for active error code.
           05  WS-LABEL                  PIC X(32) VALUE SPACES.
      * @field WS-LABEL-LEN Active character count in WS-LABEL.
           05  WS-LABEL-LEN              PIC 9(4) COMP VALUE 0.
      * @field WS-MSG-LEN Active character count in CDD-ERR-MESSAGE.
           05  WS-MSG-LEN                PIC 9(4) COMP VALUE 0.
      * @field WS-SUB-LEN Active character count in CDD-ERR-SUBPROG.
           05  WS-SUB-LEN                PIC 9(4) COMP VALUE 0.
      * @field WS-PTR 1-based write pointer into CDD-ERQ-FMT-TEXT.
           05  WS-PTR                    PIC 9(4) COMP VALUE 1.
      * @field WS-CODE-DISP Zero-padded 4-digit display error code.
           05  WS-CODE-DISP              PIC 9(4) VALUE 0.
      * @field WS-LINE-DISP Display buffer for line number.
           05  WS-LINE-DISP              PIC 9(8) VALUE 0.
      * @field WS-COL-DISP Display buffer for column number.
           05  WS-COL-DISP               PIC 9(4) VALUE 0.
       LINKAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDERRF Clear, populate, or format CDD-ERROR.
      * @param [inout] CDD-ERR-REQ Request/output formatting envelope.
      * @param [inout] CDD-ERROR Unified error enum instance.
      * @return Populated or formatted CDD-ERROR and CDD-ERR-REQ.
      * @errors CDD-OK or CDD-ERR-TYPE-TAG-MISMATCH on invalid tag.
       PROCEDURE DIVISION USING CDD-ERR-REQ CDD-ERROR.
      * @paragraph MAIN-ENTRY Dispatch requested error operation.
       MAIN-ENTRY.
           IF NOT CDD-ERQ-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDERRF" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-ERR-REQ tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           EVALUATE TRUE
               WHEN CDD-ERQ-OP-CLEAR
                   PERFORM DO-CLEAR
               WHEN CDD-ERQ-OP-SET
                   PERFORM DO-SET
               WHEN OTHER
                   PERFORM DO-FORMAT
           END-EVALUATE
           EXIT PROGRAM.
      * @paragraph DO-CLEAR Reset CDD-ERROR fields to clean CDD-OK.
       DO-CLEAR.
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-ERR-LINE
           MOVE 0 TO CDD-ERR-COL
           MOVE 0 TO CDD-ERR-OFFSET
           MOVE 0 TO CDD-ERR-DIALECT
           MOVE SPACES TO CDD-ERR-SUBPROG
           MOVE SPACES TO CDD-ERR-MESSAGE.
      * @paragraph DO-SET Populate CDD-ERROR from CDD-ERR-REQ fields.
       DO-SET.
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE CDD-ERQ-CODE TO CDD-ERR-CODE
           MOVE CDD-ERQ-LINE TO CDD-ERR-LINE
           MOVE CDD-ERQ-COL TO CDD-ERR-COL
           MOVE CDD-ERQ-SUBPROG TO CDD-ERR-SUBPROG
           IF CDD-ERQ-DETAIL = SPACES
               PERFORM RESOLVE-LABEL
               MOVE WS-LABEL TO CDD-ERR-MESSAGE
           ELSE
               MOVE CDD-ERQ-DETAIL TO CDD-ERR-MESSAGE
           END-IF.
      * @paragraph DO-FORMAT Format CDD-ERROR into CDD-ERQ-FMT-TEXT.
       DO-FORMAT.
           PERFORM RESOLVE-LABEL
           MOVE CDD-ERR-CODE TO WS-CODE-DISP
           MOVE CDD-ERR-LINE TO WS-LINE-DISP
           MOVE CDD-ERR-COL TO WS-COL-DISP
           MOVE SPACES TO CDD-ERQ-FMT-TEXT
           MOVE 1 TO WS-PTR
           PERFORM TRIM-SUBPROG
           PERFORM TRIM-LABEL
           PERFORM TRIM-MESSAGE
           STRING "[" DELIMITED BY SIZE
                  WS-CODE-DISP DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-LABEL(1:WS-LABEL-LEN) DELIMITED BY SIZE
                  "] " DELIMITED BY SIZE
                  CDD-ERR-SUBPROG(1:WS-SUB-LEN) DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-LINE-DISP DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-COL-DISP DELIMITED BY SIZE
                  " - " DELIMITED BY SIZE
                  CDD-ERR-MESSAGE(1:WS-MSG-LEN) DELIMITED BY SIZE
                  INTO CDD-ERQ-FMT-TEXT
                  WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-ERQ-FMT-LEN
           END-SUBTRACT.
      * @paragraph TRIM-SUBPROG Measure non-blank subprogram name.
       TRIM-SUBPROG.
           MOVE 8 TO WS-SUB-LEN
           PERFORM UNTIL WS-SUB-LEN = 1
               OR CDD-ERR-SUBPROG(WS-SUB-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-SUB-LEN
               END-SUBTRACT
           END-PERFORM
           IF CDD-ERR-SUBPROG = SPACES
               MOVE "CDD" TO CDD-ERR-SUBPROG
               MOVE 3 TO WS-SUB-LEN
           END-IF.
      * @paragraph TRIM-LABEL Measure non-blank enum variant label.
       TRIM-LABEL.
           MOVE 32 TO WS-LABEL-LEN
           PERFORM UNTIL WS-LABEL-LEN = 1
               OR WS-LABEL(WS-LABEL-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-LABEL-LEN
               END-SUBTRACT
           END-PERFORM.
      * @paragraph TRIM-MESSAGE Measure non-blank detail message.
       TRIM-MESSAGE.
           IF CDD-ERR-MESSAGE = SPACES
               MOVE WS-LABEL TO CDD-ERR-MESSAGE
           END-IF
           MOVE 256 TO WS-MSG-LEN
           PERFORM UNTIL WS-MSG-LEN = 1
               OR CDD-ERR-MESSAGE(WS-MSG-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-MSG-LEN
               END-SUBTRACT
           END-PERFORM.
      * @paragraph RESOLVE-LABEL Map CDD-ERR-CODE to variant name.
       RESOLVE-LABEL.
           EVALUATE TRUE
               WHEN CDD-OK
                   MOVE "OK" TO WS-LABEL
               WHEN CDD-ERR-JSON-EOF
                   MOVE "JSON_EOF" TO WS-LABEL
               WHEN CDD-ERR-JSON-BAD-TOKEN
                   MOVE "JSON_BAD_TOKEN" TO WS-LABEL
               WHEN CDD-ERR-JSON-BAD-STRING
                   MOVE "JSON_BAD_STRING" TO WS-LABEL
               WHEN CDD-ERR-JSON-BAD-ESCAPE
                   MOVE "JSON_BAD_ESCAPE" TO WS-LABEL
               WHEN CDD-ERR-JSON-BAD-UNICODE
                   MOVE "JSON_BAD_UNICODE" TO WS-LABEL
               WHEN CDD-ERR-JSON-BAD-NUMBER
                   MOVE "JSON_BAD_NUMBER" TO WS-LABEL
               WHEN CDD-ERR-JSON-TRAILING-DATA
                   MOVE "JSON_TRAILING_DATA" TO WS-LABEL
               WHEN CDD-ERR-JSON-DEPTH-LIMIT
                   MOVE "JSON_DEPTH_LIMIT" TO WS-LABEL
               WHEN CDD-ERR-JSON-DUP-KEY
                   MOVE "JSON_DUP_KEY" TO WS-LABEL
               WHEN CDD-ERR-JSON-CAPACITY
                   MOVE "JSON_CAPACITY" TO WS-LABEL
               WHEN CDD-ERR-SCHEMA-INVALID-ROOT
                   MOVE "SCHEMA_INVALID_ROOT" TO WS-LABEL
               WHEN CDD-ERR-SCHEMA-BAD-TYPE
                   MOVE "SCHEMA_BAD_TYPE" TO WS-LABEL
               WHEN CDD-ERR-SCHEMA-UNRESOLVED-REF
                   MOVE "SCHEMA_UNRESOLVED_REF" TO WS-LABEL
               WHEN CDD-ERR-SCHEMA-REF-CYCLE
                   MOVE "SCHEMA_REF_CYCLE" TO WS-LABEL
               WHEN CDD-ERR-SCHEMA-CONFLICT-BOUNDS
                   MOVE "SCHEMA_CONFLICT_BOUNDS" TO WS-LABEL
               WHEN CDD-ERR-INFER-EMPTY-CONFLICT
                   MOVE "INFER_EMPTY_CONFLICT" TO WS-LABEL
               WHEN OTHER
                   PERFORM RESOLVE-LABEL-PART2
           END-EVALUATE.
      * @paragraph RESOLVE-LABEL-PART2 Map COBOL/IR/Gen/CLI/Safe codes.
       RESOLVE-LABEL-PART2.
           EVALUATE TRUE
               WHEN CDD-ERR-COB-BAD-INDICATOR
                   MOVE "COB_BAD_INDICATOR" TO WS-LABEL
               WHEN CDD-ERR-COB-AREA-A-SYNTAX
                   MOVE "COB_AREA_A_SYNTAX" TO WS-LABEL
               WHEN CDD-ERR-COB-LINE-OVERFLOW
                   MOVE "COB_LINE_OVERFLOW" TO WS-LABEL
               WHEN CDD-ERR-COB-UNCLOSED-LITERAL
                   MOVE "COB_UNCLOSED_LITERAL" TO WS-LABEL
               WHEN CDD-ERR-COB-BAD-CONTINUATION
                   MOVE "COB_BAD_CONTINUATION" TO WS-LABEL
               WHEN CDD-ERR-COB-INVALID-LEVEL
                   MOVE "COB_INVALID_LEVEL" TO WS-LABEL
               WHEN CDD-ERR-COB-LEVEL-HIERARCHY
                   MOVE "COB_LEVEL_HIERARCHY" TO WS-LABEL
               WHEN CDD-ERR-COB-INVALID-PIC
                   MOVE "COB_INVALID_PIC" TO WS-LABEL
               WHEN CDD-ERR-COB-INVALID-OCCURS
                   MOVE "COB_INVALID_OCCURS" TO WS-LABEL
               WHEN CDD-ERR-COB-DIALECT-DISALLOWED
                   MOVE "COB_DIALECT_DISALLOWED" TO WS-LABEL
               WHEN CDD-ERR-COB-CST-CAPACITY
                   MOVE "COB_CST_CAPACITY" TO WS-LABEL
               WHEN CDD-ERR-IR-CAPACITY
                   MOVE "IR_CAPACITY" TO WS-LABEL
               WHEN CDD-ERR-IR-UNSUPPORTED-MAP
                   MOVE "IR_UNSUPPORTED_MAP" TO WS-LABEL
               WHEN CDD-ERR-IR-NAME-COLLISION
                   MOVE "IR_NAME_COLLISION" TO WS-LABEL
               WHEN CDD-ERR-IR-IDENTIFIER-LEN
                   MOVE "IR_IDENTIFIER_LEN" TO WS-LABEL
               WHEN CDD-ERR-DIFF-TARGET-MISSING
                   MOVE "DIFF_TARGET_MISSING" TO WS-LABEL
               WHEN OTHER
                   PERFORM RESOLVE-LABEL-PART3
           END-EVALUATE.
      * @paragraph RESOLVE-LABEL-PART3 Map Gen/Sync/CLI/IO/Safe codes.
       RESOLVE-LABEL-PART3.
           EVALUATE TRUE
               WHEN CDD-ERR-GEN-BUF-OVERFLOW
                   MOVE "GEN_BUF_OVERFLOW" TO WS-LABEL
               WHEN CDD-ERR-GEN-INVALID-UDT-KIND
                   MOVE "GEN_INVALID_UDT_KIND" TO WS-LABEL
               WHEN CDD-ERR-GEN-INVALID-SERDE-KIND
                   MOVE "GEN_INVALID_SERDE_KIND" TO WS-LABEL
               WHEN CDD-ERR-SYNC-UDT-NOT-FOUND
                   MOVE "SYNC_UDT_NOT_FOUND" TO WS-LABEL
               WHEN CDD-ERR-SYNC-AMBIGUOUS-UDT
                   MOVE "SYNC_AMBIGUOUS_UDT" TO WS-LABEL
               WHEN CDD-ERR-SYNC-INVARIANT-FAIL
                   MOVE "SYNC_INVARIANT_FAIL" TO WS-LABEL
               WHEN CDD-ERR-CLI-NO-SUBCOMMAND
                   MOVE "CLI_NO_SUBCOMMAND" TO WS-LABEL
               WHEN CDD-ERR-CLI-UNKNOWN-SUBCOMMAND
                   MOVE "CLI_UNKNOWN_SUBCOMMAND" TO WS-LABEL
               WHEN CDD-ERR-CLI-UNKNOWN-FLAG
                   MOVE "CLI_UNKNOWN_FLAG" TO WS-LABEL
               WHEN CDD-ERR-CLI-MISSING-VALUE
                   MOVE "CLI_MISSING_VALUE" TO WS-LABEL
               WHEN CDD-ERR-CLI-CONFLICT-FLAGS
                   MOVE "CLI_CONFLICT_FLAGS" TO WS-LABEL
               WHEN CDD-ERR-IO-OPEN-READ
                   MOVE "IO_OPEN_READ" TO WS-LABEL
               WHEN CDD-ERR-IO-OPEN-WRITE
                   MOVE "IO_OPEN_WRITE" TO WS-LABEL
               WHEN CDD-ERR-IO-READ-FAILURE
                   MOVE "IO_READ_FAILURE" TO WS-LABEL
               WHEN CDD-ERR-IO-WRITE-FAILURE
                   MOVE "IO_WRITE_FAILURE" TO WS-LABEL
               WHEN CDD-ERR-IO-ATOMIC-RENAME
                   MOVE "IO_ATOMIC_RENAME" TO WS-LABEL
               WHEN CDD-ERR-TYPE-TAG-MISMATCH
                   MOVE "TYPE_TAG_MISMATCH" TO WS-LABEL
               WHEN CDD-ERR-BOUNDS-OUT-OF-RANGE
                   MOVE "BOUNDS_OUT_OF_RANGE" TO WS-LABEL
               WHEN CDD-ERR-UNINITIALIZED-ARENA
                   MOVE "UNINITIALIZED_ARENA" TO WS-LABEL
               WHEN OTHER
                   MOVE "UNKNOWN_ERROR" TO WS-LABEL
           END-EVALUATE.
