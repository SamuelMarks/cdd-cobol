      * @file TJLEX.cbl
      * @brief Unit test suite for CDDJLEX RFC 8259 JSON lexer.
      * @module TJLEX
      * @details Exercises 100% of functions, lines, and branches in
      *          CDDJLEX including tag validation, whitespace/LF/CR/TAB,
      *          all 6 punctuation tokens, true/false/null keywords and
      *          invalid prefixes, integers/decimals/exponents/errors,
      *          strings/escapes/control-bytes/overlong/unclosed, and
      *          token capacity exhaustion.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJLEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-err.cpy".
      * @record WS-BYTE-CONV Helper for control byte generation.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STATE Failure counter and loop scratch.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-BS Backslash char (92).
           05  WS-BS                     PIC X(1) VALUE SPACE.
      * @field WS-LF Line feed char (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR Carriage return char (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-TAB Tab char (9).
           05  WS-TAB                    PIC X(1) VALUE SPACE.
      * @field WS-BEL Control char (7).
           05  WS-BEL                    PIC X(1) VALUE SPACE.
      * @subprogram TJLEX Verify 100% of CDDJLEX lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDJLEX test groups.
       MAIN-ENTRY.
           MOVE 92 TO WS-U16
           MOVE WS-B-LO TO WS-BS
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 9 TO WS-U16
           MOVE WS-B-LO TO WS-TAB
           MOVE 7 TO WS-U16
           MOVE WS-B-LO TO WS-BEL
           PERFORM TEST-GUARDS-AND-VALID-STREAM
           PERFORM TEST-KEYWORD-AND-TOKEN-ERRORS
           PERFORM TEST-STRING-ERRORS
           PERFORM TEST-NUM-VARIANTS-AND-ERRORS
           IF WS-FAILS > 0
               DISPLAY "TJLEX FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-VALID-STREAM Valid tokens & cap err.
       TEST-GUARDS-AND-VALID-STREAM.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADJTK00" TO CDD-JTK-TAG
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE 4096 TO CDD-JTK-CAP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-I
           STRING WS-TAB DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '{"a":[true,false,null,"",0,-12,3.14,1e+2,2E-3,'
                      DELIMITED BY SIZE
                  '4e5,"x' DELIMITED BY SIZE
                  WS-BS DELIMITED BY SIZE
                  'n"]}   ' DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-I
           END-STRING
           SUBTRACT 1 FROM WS-I GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-JTK-COUNT NOT = 28
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-JTK-CAP
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 4096 TO CDD-JTK-CAP
           MOVE 0 TO CDD-BUF-LEN
           MOVE 0 TO CDD-JTK-CAP
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 4096 TO CDD-JTK-CAP.
      * @paragraph TEST-KEYWORD-AND-TOKEN-ERRORS Test bad keywords/char.
       TEST-KEYWORD-AND-TOKEN-ERRORS.
           MOVE "@" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "tr" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "trux" TO CDD-BUF-DATA(1:4)
           MOVE 4 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "fa" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "falsx" TO CDD-BUF-DATA(1:5)
           MOVE 5 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "nu" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "nulx" TO CDD-BUF-DATA(1:4)
           MOVE 4 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-STRING-ERRORS Test all string failure modes.
       TEST-STRING-ERRORS.
           MOVE '"unclosed' TO CDD-BUF-DATA(1:9)
           MOVE 9 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-STRING
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '"' TO CDD-BUF-DATA(1:1)
           MOVE WS-BEL TO CDD-BUF-DATA(2:1)
           MOVE '"' TO CDD-BUF-DATA(3:1)
           MOVE 3 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-STRING
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '"' TO CDD-BUF-DATA(1:1)
           MOVE WS-BS TO CDD-BUF-DATA(2:1)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '"' TO CDD-BUF-DATA(1:1)
           MOVE WS-BS TO CDD-BUF-DATA(2:1)
           MOVE WS-BEL TO CDD-BUF-DATA(3:1)
           MOVE '"' TO CDD-BUF-DATA(4:1)
           MOVE 4 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '"' TO CDD-BUF-DATA(1:1)
           MOVE WS-BS TO CDD-BUF-DATA(2:1)
           MOVE 'q"' TO CDD-BUF-DATA(3:2)
           MOVE 4 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '"' TO CDD-BUF-DATA(1:1)
           MOVE 2 TO WS-I
           PERFORM UNTIL WS-I > 1027
               MOVE "x" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE '"' TO CDD-BUF-DATA(1028:1)
           MOVE 1028 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-STRING
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-NUM-VARIANTS-AND-ERRORS Test number grammar.
       TEST-NUM-VARIANTS-AND-ERRORS.
           MOVE "0 1 2 3 4 5 6 7 8 9" TO CDD-BUF-DATA(1:19)
           MOVE 19 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "-" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "-a" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "-/" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "01" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "0/" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "1." TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "1e" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "1e+" TO CDD-BUF-DATA(1:3)
           MOVE 3 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "0" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
