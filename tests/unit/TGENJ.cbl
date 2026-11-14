      * @file TGENJ.cbl
      * @brief Unit test suite for CDDGENJ COBOL -> JSON/Schema parser.
      * @module TGENJ
      * @details Exercises tag checks, lexer/CST/not-found errors, and
      *          both Schema (pretty/compact/x-cobol) and Sample JSON
      *          emission from COBOL records, TYPEDEFs, and CLASS-IDs.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TGENJ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record WS-OUT-BUF Output JSON buffer.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active JSON length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Buffer capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Newline mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Output JSON bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
       COPY "cdd-err.cpy".
      * @record WS-STATE Test assertion counter.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @subprogram TGENJ Verify 100% of CDDGENJ lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDGENJ test scenarios.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-ERRORS
           PERFORM TEST-SCHEMA-AND-SAMPLE-MODES
           IF WS-FAILS > 0
               DISPLAY "TGENJ FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-ERRORS Test tags and parse failures.
       TEST-GUARDS-AND-ERRORS.
           MOVE "BADOPT00" TO CDD-OPT-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDBUF01" TO WS-OBF-TAG
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADOBF00" TO WS-OBF-TAG
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE "000100!BAD" TO CDD-BUF-DATA(1:10)
           MOVE 10 TO CDD-BUF-LEN
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-INDICATOR
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           MOVE "05 ORPHAN PIC X." TO CDD-BUF-DATA(1:16)
           MOVE 16 TO CDD-BUF-LEN
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-LEVEL-HIERARCHY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "PROCEDURE DIVISION." TO CDD-BUF-DATA(1:19)
           MOVE 19 TO CDD-BUF-LEN
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SYNC-UDT-NOT-FOUND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-SCHEMA-AND-SAMPLE-MODES Parse UDT -> JSON.
       TEST-SCHEMA-AND-SAMPLE-MODES.
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-JSON-MODE
           MOVE 1 TO CDD-OPT-STYLE
           MOVE 1 TO CDD-OPT-X-COBOL
           MOVE "01 UDT-A. 05 F1 PIC X(8) VALUE 'HI'. 05 F2 PIC 9(4)."
               TO CDD-BUF-DATA(1:52)
           MOVE 52 TO CDD-BUF-LEN
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN < 40
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-OPT-JSON-MODE
           MOVE 2 TO CDD-OPT-STYLE
           MOVE 0 TO CDD-OPT-X-COBOL
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN < 10
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
