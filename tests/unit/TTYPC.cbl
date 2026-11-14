      * @file TTYPC.cbl
      * @brief Unit tests for CDDTYPC strong-typing static analyzer.
      * @module TTYPC
      * @details Verifies CLI usage error, missing list error, clean
      *          pass on strongly-typed sources, and failure on raw
      *          CDD-ERR-CODE comparisons and CALL arity mismatches.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TTYPC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-NL-CONV Helper for LF byte constant.
       01  WS-NL-CONV.
      * @field WS-U16 16-bit integer initialized to 10 (LF).
           05  WS-U16                    PIC 9(4) COMP VALUE 10.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-LF Low byte LF (10).
               10  WS-LF                 PIC X(1).
      * @record WS-STATE Test assertion state.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR Write cursor in CDD-BUF-DATA.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @subprogram TTYPC Run positive and negative tests on CDDTYPC.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Execute all CDDTYPC test cases.
       MAIN-ENTRY.
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           MOVE 1 TO CDD-AGV-QUIET
           MOVE 0 TO CDD-AGV-COUNT
           CALL "MCDDTYPC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "build/no_such_type.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDTYPC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 3
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM TEST-POSITIVE-TYPES
           PERFORM TEST-NEGATIVE-TYPES
           MOVE 0 TO CDD-AGV-MODE
           MOVE 0 TO CDD-AGV-QUIET
           IF WS-FAILS > 0
               DISPLAY "TTYPC FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-POSITIVE-TYPES Verify core modules pass check.
       TEST-POSITIVE-TYPES.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/ttypc_ok.lst" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "src/include/cdd-core.cpy" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "src/core/CDDAREN.cbl" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/ttypc_ok.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDTYPC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-NEGATIVE-TYPES Verify violations return 1.
       TEST-NEGATIVE-TYPES.
           MOVE "build/ttypc_bad.cbl" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE 2 TO CDD-PATH-OP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "       IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROGRAM-ID. BADTYP." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       MAIN-PARA." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           IF CDD-ERR-CODE = 1001" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '               CALL "CDDAREN" USING WS-X'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "               END-CALL" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           END-IF." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/ttypc_bad.lst" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "src/core/CDDAREN.cbl" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "build/ttypc_bad.cbl" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/ttypc_bad.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDTYPC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
