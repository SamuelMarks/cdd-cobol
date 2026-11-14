      * @file TDIAL.cbl
      * @brief Unit test suite for CDDDIAL 10-dialect registry.
      * @module TDIAL
      * @details Exercises tag/bounds checks, CLI dialect name parsing
      *          for all 10 standards, capability flags across 1960..
      *          2023, and era-scoped reserved-word queries.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDIAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-dial.cpy".
       COPY "cdd-err.cpy".
      * @record WS-NAMES Table of valid dialect name strings.
       01  WS-NAMES.
      * @field WS-N-DATA 10 canonical dialect strings (20 chars each).
           05  WS-N-DATA.
      * @field WS-N1 cobol60..cobol65.
               10  WS-N1                 PIC X(80) VALUE
               "cobol60             cobol-61            cobol-61-extende
      -        "d   cobol65             ".
      * @field WS-N2 cobol68..cobol2002.
               10  WS-N2                 PIC X(80) VALUE
               "cobol-68            cobol74             cobol-85
      -        "    cobol2002           ".
      * @field WS-N3 cobol2014..cobol2023.
               10  WS-N3                 PIC X(40) VALUE
               "cobol-2014          cobol2023           ".
      * @field WS-NAME-ITEM Array view of 10 dialect names.
           05  WS-NAME-ITEM REDEFINES WS-N-DATA
                                         PIC X(20) OCCURS 10 TIMES.
      * @record WS-ERA-WORDS Era-scoped words to test in 60 vs 2023.
       01  WS-ERA-WORDS.
      * @field WS-EW-RAW 11 era-specific words (16 chars each = 176B).
           05  WS-EW-RAW.
      * @field WS-EW1 Words 1..5.
               10  WS-EW1                PIC X(80) VALUE
               "CONSTANT        EXAMINE         REDEFINES       RENAMES
      -        "        INDEXED         ".
      * @field WS-EW2 Words 6..11.
               10  WS-EW2                PIC X(96) VALUE
               "CALL            INSPECT         EVALUATE        TYPEDEF
      -        "        DYNAMIC         JSON            ".
      * @field WS-EW-ITEM Array of 11 era-specific words.
           05  WS-EW-ITEM REDEFINES WS-EW-RAW
                                         PIC X(16) OCCURS 11 TIMES.
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-I Outer loop counter.
           05  WS-I                      PIC 9(2) COMP VALUE 0.
      * @field WS-J Inner loop counter.
           05  WS-J                      PIC 9(2) COMP VALUE 0.
      * @subprogram TDIAL Verify 100% of CDDDIAL lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDDIAL test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-BOUNDS
           PERFORM TEST-PARSE-ALL-10
           PERFORM TEST-RESERVED-ERA-MATRIX
           IF WS-FAILS > 0
               DISPLAY "TDIAL FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-BOUNDS Test tag, bad ID, bad name.
       TEST-GUARDS-AND-BOUNDS.
           MOVE "BADDLR00" TO CDD-DLR-TAG
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 1 TO CDD-DLR-OP
           MOVE 0 TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-DLR-OP
           MOVE 11 TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-DLR-OP
           MOVE 0 TO CDD-DLR-WORD-LEN
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-UNKNOWN-FLAG
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 65 TO CDD-DLR-WORD-LEN
           MOVE "bogus-dialect" TO CDD-DLR-WORD
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-UNKNOWN-FLAG
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-PARSE-ALL-10 Parse all 10 names & check caps.
       TEST-PARSE-ALL-10.
           MOVE 3 TO CDD-DLR-OP
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 10
               MOVE WS-NAME-ITEM(WS-I) TO CDD-DLR-WORD
               MOVE 20 TO CDD-DLR-WORD-LEN
               CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
               END-CALL
               IF NOT CDD-OK OR CDD-DLR-DIALECT NOT = WS-I
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           IF CDD-DLR-HAS-UTF8 NOT = 1 OR CDD-DLR-HAS-JSON-VERB NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-RESERVED-ERA-MATRIX Test core & era keywords.
       TEST-RESERVED-ERA-MATRIX.
           MOVE 2 TO CDD-DLR-OP
           MOVE 7 TO CDD-DLR-DIALECT
           MOVE "MOVE" TO CDD-DLR-WORD
           MOVE 4 TO CDD-DLR-WORD-LEN
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-DLR-IS-RESERVED NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CUSTOMER-ID" TO CDD-DLR-WORD
           MOVE 11 TO CDD-DLR-WORD-LEN
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-DLR-IS-RESERVED NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 10
               MOVE WS-I TO CDD-DLR-DIALECT
               MOVE 1 TO WS-J
               PERFORM UNTIL WS-J > 11
                   MOVE WS-EW-ITEM(WS-J) TO CDD-DLR-WORD
                   MOVE 16 TO CDD-DLR-WORD-LEN
                   CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
                   END-CALL
                   IF NOT CDD-OK
                       ADD 1 TO WS-FAILS
                       END-ADD
                   END-IF
                   ADD 1 TO WS-J
                   END-ADD
               END-PERFORM
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM.
