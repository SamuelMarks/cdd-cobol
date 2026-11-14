      * @file TCLIH.cbl
      * @brief Unit test suite for CDDCLIH CLI help/version/err output.
      * @module TCLIH
      * @details Exercises invalid tag check, global help, subcommand
      *          help topics 1..4, version banner, and error output in
      *          both quiet and console modes.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCLIH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-cli.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test assertion counter.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-T Topic loop index (0..4).
           05  WS-T                      PIC 9(2) COMP VALUE 0.
      * @subprogram TCLIH Verify 100% of CDDCLIH lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Exercise all CDDCLIH operations.
       MAIN-ENTRY.
           MOVE "BADCLH00" TO CDD-CLH-TAG
           CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCLH01" TO CDD-CLH-TAG
           MOVE 1 TO CDD-CLH-QUIET
           MOVE 1 TO CDD-CLH-OP
           MOVE 0 TO WS-T
           PERFORM UNTIL WS-T > 4
               MOVE WS-T TO CDD-CLH-TOPIC
               CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
               END-CALL
               IF CDD-CLH-LAST-TEXT = SPACES
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               ADD 1 TO WS-T
               END-ADD
           END-PERFORM
           MOVE 0 TO CDD-CLH-QUIET
           MOVE 2 TO CDD-CLH-OP
           CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
           END-CALL
           MOVE 3 TO CDD-CLH-OP
           MOVE 5001 TO CDD-ERR-CODE
           MOVE "CDDCLI" TO CDD-ERR-SUBPROG
           MOVE "No subcommand" TO CDD-ERR-MESSAGE
           CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
           END-CALL
           MOVE 1 TO CDD-CLH-QUIET
           CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
           END-CALL
           IF WS-FAILS > 0
               DISPLAY "TCLIH FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
