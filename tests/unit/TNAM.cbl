      * @file TNAM.cbl
      * @brief Unit test suite for CDDNAM dialect-aware name converter.
      * @module TNAM
      * @details Exercises tag/dialect guards, >128 input clamping,
      *          camelCase, snake_case, $schema, 2fa, punctuation-only,
      *          30-char vs 63-char limits, era reserved-word escaping,
      *          and reverse camel/snake/kebab/preserve mapping.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TNAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-I Loop index.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @subprogram TNAM Verify 100% of CDDNAM lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDNAM unit test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS
           PERFORM TEST-J2C-CONVERSIONS
           PERFORM TEST-C2J-CONVERSIONS
           IF WS-FAILS > 0
               DISPLAY "TNAM FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS Verify tag and invalid dialect errors.
       TEST-GUARDS.
           MOVE "BADNAM00" TO CDD-NAM-TAG
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDNAM01" TO CDD-NAM-TAG
           MOVE 1 TO CDD-NAM-OP
           MOVE 99 TO CDD-NAM-DIALECT
           MOVE 3 TO CDD-NAM-IN-LEN
           MOVE "abc" TO CDD-NAM-IN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-J2C-CONVERSIONS Test JSON->COBOL across eras.
       TEST-J2C-CONVERSIONS.
           MOVE 1 TO CDD-NAM-OP
           MOVE 7 TO CDD-NAM-DIALECT
           MOVE "$schema" TO CDD-NAM-IN
           MOVE 7 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:8) NOT = "S-SCHEMA"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "2fa_enabled" TO CDD-NAM-IN
           MOVE 11 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:13) NOT = "N-2FA-ENABLED"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "firstName2Test__" TO CDD-NAM-IN
           MOVE 16 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:16) NOT = "FIRST-NAME2-TEST"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "___" TO CDD-NAM-IN
           MOVE 3 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:3) NOT = "FLD"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:3) NOT = "FLD"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "data" TO CDD-NAM-IN
           MOVE 4 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:8) NOT = "DATA-FLD"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-NAM-DIALECT
           MOVE "json" TO CDD-NAM-IN
           MOVE 4 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:4) NOT = "JSON"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO CDD-NAM-DIALECT
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:8) NOT = "JSON-FLD"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 128
               MOVE "a" TO CDD-NAM-IN(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE "A" TO CDD-NAM-IN(25:1)
           MOVE "A" TO CDD-NAM-IN(26:1)
           MOVE "_" TO CDD-NAM-IN(24:1)
           MOVE 130 TO CDD-NAM-IN-LEN
           MOVE 7 TO CDD-NAM-DIALECT
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT-LEN > 30
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-C2J-CONVERSIONS Test camel/snake/kebab/preserve.
       TEST-C2J-CONVERSIONS.
           MOVE 2 TO CDD-NAM-OP
           MOVE 0 TO CDD-NAM-IN-LEN
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:5) NOT = "field"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "-FIRST-NAME-FLD" TO CDD-NAM-IN
           MOVE 15 TO CDD-NAM-IN-LEN
           MOVE 1 TO CDD-NAM-CASE
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:9) NOT = "firstName"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "FIRST-NAME-FLD" TO CDD-NAM-IN
           MOVE 14 TO CDD-NAM-IN-LEN
           MOVE 2 TO CDD-NAM-CASE
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:10) NOT = "first_name"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-NAM-CASE
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:10) NOT = "first-name"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 4 TO CDD-NAM-CASE
           CALL "CDDNAM" USING CDD-NAM-REQ CDD-ERROR
           END-CALL
           IF CDD-NAM-OUT(1:10) NOT = "FIRST-NAME"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
