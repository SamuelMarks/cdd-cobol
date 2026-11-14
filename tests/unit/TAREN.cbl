      * @file TAREN.cbl
      * @brief Unit tests for CDDAREN fixed-capacity pool allocator.
      * @module TAREN
      * @details Verifies tag checking, uninitialized reset/alloc
      *          guards, slot allocation, capacity exhaustion, and
      *          pool reset.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test failure counter.
       01  WS-STATE.
      * @field WS-FAILS Number of failed assertions.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @subprogram TAREN Verify 100% of CDDAREN branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Displays failure diagnostic to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Exercise all CDDAREN operations.
       MAIN-ENTRY.
           MOVE "BADTAG00" TO CDD-ARN-TAG
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDARN01" TO CDD-ARN-TAG
           MOVE 0 TO CDD-ARN-READY
           MOVE 3 TO CDD-ARN-OP
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-UNINITIALIZED-ARENA
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-ARN-OP
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-UNINITIALIZED-ARENA
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-ARN-OP
           MOVE 2 TO CDD-ARN-CAP
           MOVE 3001 TO CDD-ARN-CAPACITY-CODE
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR NOT CDD-ARN-IS-READY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-ARN-OP
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-ARN-OUT-IDX NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-ARN-OUT-IDX NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IR-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-ARN-OP
           CALL "CDDAREN" USING CDD-ARENA-CTL CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-ARN-USED NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           IF WS-FAILS > 0
               DISPLAY "TAREN FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
