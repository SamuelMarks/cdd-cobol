      * @file TTYPV.cbl
      * @brief Unit tests for CDDTYPV tag and bounds validator.
      * @module TTYPV
      * @details Covers invalid request tag, envelope tag mismatch,
      *          tag-only pass, low bounds fail, high bounds fail, and
      *          valid bounds pass.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TTYPV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test failure counter.
       01  WS-STATE.
      * @field WS-FAILS Number of failed assertions.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @subprogram TTYPV Verify 100% of CDDTYPV branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Displays failure diagnostic to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all 6 CDDTYPV validation cases.
       MAIN-ENTRY.
           MOVE "BADTAG00" TO CDD-TVR-TAG
           CALL "CDDTYPV" USING CDD-TYPV-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDTVR01" TO CDD-TVR-TAG
           MOVE "CDDBUF01" TO CDD-TVR-EXP-TAG
           MOVE "WRONGTAG" TO CDD-TVR-ACT-TAG
           MOVE "TTYPV" TO CDD-TVR-CALLER
           CALL "CDDTYPV" USING CDD-TYPV-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-TVR-ACT-TAG
           MOVE 0 TO CDD-TVR-CHK-BOUNDS
           CALL "CDDTYPV" USING CDD-TYPV-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-TVR-CHK-BOUNDS
           MOVE 1 TO CDD-TVR-MIN
           MOVE 10 TO CDD-TVR-MAX
           MOVE 0 TO CDD-TVR-IDX
           CALL "CDDTYPV" USING CDD-TYPV-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 11 TO CDD-TVR-IDX
           CALL "CDDTYPV" USING CDD-TYPV-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 5 TO CDD-TVR-IDX
           CALL "CDDTYPV" USING CDD-TYPV-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           IF WS-FAILS > 0
               DISPLAY "TTYPV FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
