      * @file TJPAR.cbl
      * @brief Unit test suite for CDDJPAR explicit-stack DOM parser.
      * @module TJPAR
      * @details Exercises 100% of functions, lines, and branches in
      *          CDDJPAR including envelope tags, empty streams, all
      *          value kinds, nested objects/arrays, duplicate keys,
      *          depth limits, node capacity, trailing tokens, and all
      *          syntax error states.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJPAR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-err.cpy".
      * @record WS-BYTE-CONV Helper for ASCII 92 backslash byte.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer initialized to 92.
           05  WS-U16                    PIC 9(4) COMP VALUE 92.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-BS Low byte backslash (92).
               10  WS-BS                 PIC X(1).
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-I Loop index.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-JLEN Length of active JSON snippet.
           05  WS-JLEN                   PIC 9(4) COMP VALUE 0.
      * @record WS-SNIP JSON snippet scratch buffer.
       01  WS-SNIP                       PIC X(512) VALUE SPACES.
      * @subprogram TJPAR Verify 100% of CDDJPAR lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Execute all CDDJPAR test groups.
       MAIN-ENTRY.
           PERFORM TEST-ENVELOPE-GUARDS
           PERFORM TEST-VALID-TREES-AND-SCALARS
           PERFORM TEST-LONG-KEYS-STRINGS-NUMBERS
           PERFORM TEST-LIMITS-AND-DUPLICATES
           PERFORM TEST-SYNTAX-ERRORS
           IF WS-FAILS > 0
               DISPLAY "TJPAR FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph LEX-AND-PARSE Helper to lex WS-SNIP(1:WS-JLEN) & dom.
       LEX-AND-PARSE.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE 4096 TO CDD-JTK-CAP
           MOVE SPACES TO CDD-BUF-DATA
           IF WS-JLEN > 0
               MOVE WS-SNIP(1:WS-JLEN) TO CDD-BUF-DATA(1:WS-JLEN)
           END-IF
           MOVE WS-JLEN TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF CDD-OK
               CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
                   CDD-ERROR
               END-CALL
           END-IF.
      * @paragraph TEST-ENVELOPE-GUARDS Test tag checks & 0-tok stream.
       TEST-ENVELOPE-GUARDS.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADJTK00" TO CDD-JTK-TAG
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE "BADJDM00" TO CDD-JDM-TAG
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE 0 TO CDD-JTK-COUNT
           MOVE 0 TO CDD-JDM-MAX-DEPTH
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-EOF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 100 TO CDD-JDM-MAX-DEPTH
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-EOF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 32 TO CDD-JDM-MAX-DEPTH
           MOVE 1024 TO CDD-JDM-CAP
           MOVE 1 TO CDD-JDM-REJECT-DUP.
      * @paragraph TEST-VALID-TREES-AND-SCALARS Parse nested & roots.
       TEST-VALID-TREES-AND-SCALARS.
           MOVE '{"a":1,"b":2.5,"c":"hi","d":"","e":true,"f":false,'
               TO WS-SNIP(1:50)
           MOVE '"g":null,"h":{},"i":[],"j":[10,20]}'
               TO WS-SNIP(51:35)
           MOVE 85 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK OR CDD-JDM-ROOT NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '""' TO WS-SNIP(1:2)
           MOVE 2 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "42" TO WS-SNIP(1:2)
           MOVE 2 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "3.14" TO WS-SNIP(1:4)
           MOVE 4 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "true" TO WS-SNIP(1:4)
           MOVE 4 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "false" TO WS-SNIP(1:5)
           MOVE 5 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "null" TO WS-SNIP(1:4)
           MOVE 4 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[]" TO WS-SNIP(1:2)
           MOVE 2 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-LONG-KEYS-STRINGS-NUMBERS Test >128 / >256 caps.
       TEST-LONG-KEYS-STRINGS-NUMBERS.
           MOVE '{"":1}' TO WS-SNIP(1:6)
           MOVE 6 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE '{"' TO CDD-BUF-DATA(1:2)
           MOVE 3 TO WS-I
           PERFORM UNTIL WS-I > 142
               MOVE "k" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE '":"' TO CDD-BUF-DATA(143:3)
           MOVE 146 TO WS-I
           PERFORM UNTIL WS-I > 415
               MOVE "v" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE '","n":1' TO CDD-BUF-DATA(416:7)
           MOVE 423 TO WS-I
           PERFORM UNTIL WS-I > 690
               MOVE "0" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE ".5}" TO CDD-BUF-DATA(691:3)
           MOVE 693 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "1" TO CDD-BUF-DATA(1:1)
           MOVE 2 TO WS-I
           PERFORM UNTIL WS-I > 270
               MOVE "0" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE 270 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-LIMITS-AND-DUPLICATES Test dup, depth, cap.
       TEST-LIMITS-AND-DUPLICATES.
           MOVE 1 TO CDD-JDM-REJECT-DUP
           MOVE '{"a":1,"b":2,"a":3}' TO WS-SNIP(1:19)
           MOVE 19 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-DUP-KEY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-JDM-REJECT-DUP
           PERFORM LEX-AND-PARSE
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-JDM-REJECT-DUP
           MOVE 1 TO CDD-JDM-MAX-DEPTH
           MOVE '{"a":{"b":1}}' TO WS-SNIP(1:13)
           MOVE 13 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-DEPTH-LIMIT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[[1]]" TO WS-SNIP(1:5)
           MOVE 5 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-DEPTH-LIMIT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 32 TO CDD-JDM-MAX-DEPTH
           MOVE 0 TO CDD-JDM-CAP
           MOVE "{}" TO WS-SNIP(1:2)
           MOVE 2 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[]" TO WS-SNIP(1:2)
           MOVE 2 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '"s"' TO WS-SNIP(1:3)
           MOVE 3 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "1" TO WS-SNIP(1:1)
           MOVE 1 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "1.5" TO WS-SNIP(1:3)
           MOVE 3 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "true" TO WS-SNIP(1:4)
           MOVE 4 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "false" TO WS-SNIP(1:5)
           MOVE 5 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "null" TO WS-SNIP(1:4)
           MOVE 4 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1024 TO CDD-JDM-CAP.
      * @paragraph TEST-SYNTAX-ERRORS Test EOF, trailing, bad syntax.
       TEST-SYNTAX-ERRORS.
           MOVE 0 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-EOF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "{" TO WS-SNIP(1:1)
           MOVE 1 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-EOF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "{} 123" TO WS-SNIP(1:6)
           MOVE 6 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-TRAILING-DATA
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "}" TO WS-SNIP(1:1)
           MOVE 1 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "{1:2}" TO WS-SNIP(1:5)
           MOVE 5 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"a" 1}' TO WS-SNIP(1:7)
           MOVE 7 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"a":1 2}' TO WS-SNIP(1:9)
           MOVE 9 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"a":1,}' TO WS-SNIP(1:8)
           MOVE 8 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[1 2]" TO WS-SNIP(1:5)
           MOVE 5 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[1,]" TO WS-SNIP(1:4)
           MOVE 4 TO WS-JLEN
           PERFORM LEX-AND-PARSE
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM TEST-SYNTHETIC-TOK-EDGE-CASES.
      * @paragraph TEST-SYNTHETIC-TOK-EDGE-CASES Direct token stream.
       TEST-SYNTHETIC-TOK-EDGE-CASES.
           MOVE "1" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           MOVE 1 TO CDD-JTK-COUNT
           MOVE 8 TO CDD-JTK-KIND(1)
           MOVE 1 TO CDD-JTK-OFFSET(1)
           MOVE 1 TO CDD-JTK-LEN(1)
           MOVE 1 TO CDD-JTK-LINE(1)
           MOVE 1 TO CDD-JTK-COL(1)
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BS TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           MOVE 2 TO CDD-JTK-COUNT
           MOVE 7 TO CDD-JTK-KIND(1)
           MOVE 1 TO CDD-JTK-OFFSET(1)
           MOVE 1 TO CDD-JTK-LEN(1)
           MOVE 13 TO CDD-JTK-KIND(2)
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-JTK-COUNT
           MOVE 1 TO CDD-JTK-KIND(1)
           MOVE 7 TO CDD-JTK-KIND(2)
           MOVE 1 TO CDD-JTK-OFFSET(2)
           MOVE 1 TO CDD-JTK-LEN(2)
           MOVE 13 TO CDD-JTK-KIND(3)
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
