      * @file TCLEX.cbl
      * @brief Unit test suite for CDDCLEX lossless 10-dialect lexer.
      * @module TCLEX
      * @details Verifies tag/dialect/format checks, fixed & free
      *          reference formats, cols 1-6/7/8-72/73-80 trivia,
      *          REMARKS & NOTE in COBOL-60..68, *>, >>SOURCE FORMAT
      *          switching, PIC/PICTURE IS strings, continuations,
      *          literals, error codes, and the byte-for-byte stream
      *          concatenation invariant across all 10 dialects.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCLEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-err.cpy".
      * @record WS-RECON-BUF Reconstructed byte buffer for invariant.
       01  WS-RECON-BUF                  PIC X(4096) VALUE SPACES.
      * @record WS-BYTE-CONV Helper for CR/LF/TAB byte constants.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-LF LF character (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR CR character (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-TAB TAB character (9).
           05  WS-TAB                    PIC X(1) VALUE SPACE.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(8) COMP VALUE 0.
      * @field WS-RPOS Reconstruction write offset.
           05  WS-RPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-DIAL Dialect loop index (1..10).
           05  WS-DIAL                   PIC 9(2) COMP VALUE 1.
      * @subprogram TCLEX Verify 100% of CDDCLEX lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDCLEX unit test groups.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 9 TO WS-U16
           MOVE WS-B-LO TO WS-TAB
           PERFORM TEST-GUARDS-AND-FORMAT-MODES
           PERFORM TEST-COBOL60-REMARKS-NOTE-CONT
           PERFORM TEST-ALL-10-DIALECTS-INVARIANT
           PERFORM TEST-FREE-FMT-AND-DIRECTIVES
           PERFORM TEST-LEXER-ERROR-BRANCHES
           IF WS-FAILS > 0
               DISPLAY "TCLEX FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph VERIFY-BYTE-INVARIANT Check concat(slices)==CDD-BUF.
       VERIFY-BYTE-INVARIANT.
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           ELSE
               MOVE SPACES TO WS-RECON-BUF
               MOVE 1 TO WS-RPOS
               MOVE 1 TO WS-I
               PERFORM UNTIL WS-I > CDD-CTK-COUNT
                   IF CDD-CTI-OFFSET(WS-I) NOT = WS-RPOS
                       ADD 1 TO WS-FAILS
                       END-ADD
                   END-IF
                   MOVE CDD-BUF-DATA(
                       CDD-CTI-OFFSET(WS-I):CDD-CTI-LEN(WS-I))
                       TO WS-RECON-BUF(WS-RPOS:CDD-CTI-LEN(WS-I))
                   ADD CDD-CTI-LEN(WS-I) TO WS-RPOS
                   END-ADD
                   ADD 1 TO WS-I
                   END-ADD
               END-PERFORM
               IF WS-RPOS - 1 NOT = CDD-BUF-LEN
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               IF CDD-BUF-LEN > 0
                   AND WS-RECON-BUF(1:CDD-BUF-LEN)
                       NOT = CDD-BUF-DATA(1:CDD-BUF-LEN)
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
           END-IF.
      * @paragraph TEST-GUARDS-AND-FORMAT-MODES Tags, bad dial, free.
       TEST-GUARDS-AND-FORMAT-MODES.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADCTK00" TO CDD-CTK-TAG
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 99 TO CDD-CTK-DIALECT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-DIALECT-DISALLOWED
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 8 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE 0 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-COBOL60-REMARKS-NOTE-CONT Test 1960s trivia.
       TEST-COBOL60-REMARKS-NOTE-CONT.
           MOVE 1 TO CDD-CTK-DIALECT
           MOVE 0 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "000100 IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  "                                         DECK0001"
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000110 REMARKS. INLINE REMARK TEXT"
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000120     HISTORICAL COMMENT LINE."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000125 AUTHOR. GRACE." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000126 " DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000130 ENVIRONMENT DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000140 DATA DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000150 WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000160*COMMENT" DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000165*" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "123" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000170 01  REC-A PICTURE is S9(4)V99 VALUE +12.50.'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000180     05  STR-A PIC X(12) VALUE "HELLO ""WORLD'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000185-        "MID' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000190-        "CONT"' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000191     05  S2 PIC X(8) VALUE "AB'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000192-        "CD".' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000193     05  SALES DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '000195     05  N-FLD PIC 99.99' DELIMITED BY SIZE
                  WS-TAB DELIMITED BY SIZE
                  "VALUE .5." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000196     90  NOT-LVL VALUE 100." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000197-    VALUE X'41' ==PS== (1 + 2 : 3)."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000200 PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000205 P0. NOTE ." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000210 MAIN-P. NOTE THIS SPANS" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000220     TWO LINES." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000230     STOP RUN." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT.
      * @paragraph TEST-ALL-10-DIALECTS-INVARIANT Check 1..10 fixed.
       TEST-ALL-10-DIALECTS-INVARIANT.
           MOVE 1 TO WS-DIAL
           PERFORM UNTIL WS-DIAL > 10
               MOVE WS-DIAL TO CDD-CTK-DIALECT
               MOVE 1 TO CDD-CTK-FMT-REQ
               CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
               END-CALL
               PERFORM VERIFY-BYTE-INVARIANT
               ADD 1 TO WS-DIAL
               END-ADD
           END-PERFORM.
      * @paragraph TEST-FREE-FMT-AND-DIRECTIVES Test 2002..2023 free.
       TEST-FREE-FMT-AND-DIRECTIVES.
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 0 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "  >>SOURCE FORMAT IS FREE" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "*> Free comment" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "ID DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "01 U-REC IS TYPEDEF STRONG. *> inline"
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 F1 PIC U(8) VALUE U'A'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 F2 PIC 99 VALUE 12.34." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  ">>SOURCE FORMAT IS FIXED" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  FX-REC PIC X." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT
           MOVE "ident" TO CDD-BUF-DATA(1:5)
           MOVE 5 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT
           MOVE "IDENT" TO CDD-BUF-DATA(1:5)
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT
           MOVE "*> hi" TO CDD-BUF-DATA(1:5)
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT
           MOVE "       01 A PIC X" TO CDD-BUF-DATA(1:17)
           MOVE 17 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT
           MOVE "000100 01 A PIC X." TO CDD-BUF-DATA(1:18)
           MOVE 18 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT
           MOVE "X" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT.
      * @paragraph TEST-LEXER-ERROR-BRANCHES Test col7, cont, unclosed.
       TEST-LEXER-ERROR-BRANCHES.
           MOVE 7 TO CDD-CTK-DIALECT
           MOVE 1 TO CDD-CTK-FMT-REQ
           MOVE "000100!BAD COL 7" TO CDD-BUF-DATA(1:16)
           MOVE 16 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-INDICATOR
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "000100-" TO CDD-BUF-DATA(1:7)
           MOVE 7 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-CONTINUATION
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "000100-     " TO CDD-BUF-DATA(1:12)
           MOVE 12 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-CONTINUATION
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '000100     MOVE "UNCLOSED' TO CDD-BUF-DATA(1:25)
           MOVE 25 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-UNCLOSED-LITERAL
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '000100     MOVE "UNCLOSED' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000110-    NOSPACEQUOTE" DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-CONTINUATION
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '000100     MOVE "UNCLOSED' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000110     MOVE 1 TO X." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-UNCLOSED-LITERAL
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "000100     MOVE X'41" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "123" DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-UNCLOSED-LITERAL
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING 'MOVE "UNCLOSED' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "MOVE 1 TO A" DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-UNCLOSED-LITERAL
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-CTK-CAP
           MOVE "01 A PIC X." TO CDD-BUF-DATA(1:11)
           MOVE 11 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-CST-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 8192 TO CDD-CTK-CAP
           PERFORM TEST-LONG-DIRECTIVE-AND-WORD.
      * @paragraph TEST-LONG-DIRECTIVE-AND-WORD Test >128 dir/word/ps.
       TEST-LONG-DIRECTIVE-AND-WORD.
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE ">>DEFINE " TO CDD-BUF-DATA(1:9)
           MOVE 10 TO WS-I
           PERFORM UNTIL WS-I > 145
               MOVE "A" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE WS-LF TO CDD-BUF-DATA(146:1)
           MOVE 147 TO WS-I
           PERFORM UNTIL WS-I > 285
               MOVE "W" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE " ==UNCLOSED" TO CDD-BUF-DATA(286:11)
           MOVE 296 TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           PERFORM VERIFY-BYTE-INVARIANT.
