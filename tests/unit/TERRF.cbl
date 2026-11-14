      * @file TERRF.cbl
      * @brief Unit tests for CDDERRF unified error enum formatter.
      * @module TERRF
      * @details Exercises every operation and all 45+ CDD-ERROR enum
      *          variants plus invalid tags and fallback branches.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TERRF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @record WS-CODE-LIST Exhaustive table of all error enum codes.
       01  WS-CODE-LIST.
      * @field WS-CODES-RAW Packed 4-digit codes (46 entries = 184B).
           05  WS-CODES-RAW.
      * @field WS-C1 Codes 1..15.
               10  WS-C1                 PIC X(60) VALUE
               "00001001100210031004100510061007100810091010110111021103
      -        "1104".
      * @field WS-C2 Codes 16..30.
               10  WS-C2                 PIC X(60) VALUE
               "11051106200120022003200420052006200720082009201020113001
      -        "3002".
      * @field WS-C3 Codes 31..46.
               10  WS-C3                 PIC X(64) VALUE
               "30033004300540014002400340044005400650015002500350045005
      -        "50065007".
      * @field WS-C4 Codes 47..53.
               10  WS-C4                 PIC X(28) VALUE
               "5008500950109001900290039999".
      * @field WS-CODE-ARR Array overlay of 53 4-digit display codes.
           05  WS-CODE-ARR REDEFINES WS-CODES-RAW
                                         PIC 9(4) OCCURS 53 TIMES.
      * @record WS-TEST-VARS Loop and assertion state.
       01  WS-TEST-VARS.
      * @field WS-I Loop index (1..53).
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-FAILS Failure count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @subprogram TERRF Run exhaustive unit tests on CDDERRF.
      * @return Exit status 0 on pass; 1 on assertion failure.
      * @errors Fails test process if any assertion fails.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Execute tag, clear, set, and format tests.
       MAIN-ENTRY.
           MOVE "BADTAG00" TO CDD-ERQ-TAG
           CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDERQ01" TO CDD-ERQ-TAG
           MOVE 1 TO CDD-ERQ-OP
           CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM TEST-ALL-CODES
           PERFORM TEST-CUSTOM-DETAIL
           IF WS-FAILS > 0
               DISPLAY "TERRF FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-ALL-CODES Format every single CDD-ERROR code.
       TEST-ALL-CODES.
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 53
               MOVE 2 TO CDD-ERQ-OP
               MOVE WS-CODE-ARR(WS-I) TO CDD-ERQ-CODE
               MOVE 12 TO CDD-ERQ-LINE
               MOVE 4 TO CDD-ERQ-COL
               MOVE SPACES TO CDD-ERQ-SUBPROG
               MOVE SPACES TO CDD-ERQ-DETAIL
               CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
               END-CALL
               MOVE SPACES TO CDD-ERR-MESSAGE
               MOVE 3 TO CDD-ERQ-OP
               CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
               END-CALL
               IF CDD-ERQ-FMT-LEN = 0
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM.
      * @paragraph TEST-CUSTOM-DETAIL Verify custom detail and subprog.
       TEST-CUSTOM-DETAIL.
           MOVE 2 TO CDD-ERQ-OP
           MOVE 1001 TO CDD-ERQ-CODE
           MOVE 42 TO CDD-ERQ-LINE
           MOVE 8 TO CDD-ERQ-COL
           MOVE "CDDJLEX" TO CDD-ERQ-SUBPROG
           MOVE "Unexpected end of JSON object" TO CDD-ERQ-DETAIL
           CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
           END-CALL
           MOVE 3 TO CDD-ERQ-OP
           CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
           END-CALL
           IF CDD-ERQ-FMT-LEN < 20
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
