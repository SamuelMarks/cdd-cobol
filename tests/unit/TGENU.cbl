      * @file TGENU.cbl
      * @brief Unit test suite for CDDGENU 10-dialect UDT generator.
      * @module TGENU
      * @details Exercises tag/dialect/format/UDT-kind guards, fixed &
      *          free layouts, CRLF/LF, col-72 wrapping, deep nesting,
      *          COBOL-60 verbose SIZE/CLASS/POINT, 66 RENAMES,
      *          REDEFINES, SYNC/JUST, N/U/DYNAMIC/BINARY/DECFLOAT,
      *          CLASS-ID, TYPEDEF, and buffer overflow.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TGENU.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @record WS-JSON-BUF Input JSON Schema buffer.
       01  WS-JSON-BUF.
      * @field WS-JBF-TAG Magic tag ('CDDBUF01').
           05  WS-JBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-JBF-LEN Active JSON length.
           05  WS-JBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-JBF-CAP Buffer capacity.
           05  WS-JBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-JBF-NL Newline mode.
           05  WS-JBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-JBF-DATA Raw JSON Schema bytes.
           05  WS-JBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @subprogram TGENU Verify 100% of CDDGENU lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDGENU unit test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-INVALID-OPTS
           PERFORM TEST-RICH-SCHEMA-EMIT
           PERFORM TEST-SPECIAL-IR-CONSTRUCTS
           IF WS-FAILS > 0
               DISPLAY "TGENU FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-INVALID-OPTS Test tags & era checks.
       TEST-GUARDS-AND-INVALID-OPTS.
           MOVE "BADOPT00" TO CDD-OPT-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 99 TO CDD-OPT-DIALECT
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-DIALECT-DISALLOWED
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 2 TO CDD-IR-UDT-KIND
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-UDT-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-IR-UDT-KIND
           MOVE 3 TO CDD-IR-COB-NLEN
           MOVE "EMP" TO CDD-IR-COB-NAME
           MOVE 0 TO CDD-IR-TITLE-LEN
           MOVE 0 TO CDD-IR-DESC-LEN
           MOVE 0 TO CDD-IR-ROOT-FIRST
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-RICH-SCHEMA-EMIT Emit rich IR across modes.
       TEST-RICH-SCHEMA-EMIT.
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDBUF01" TO WS-JBF-TAG
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE SPACES TO WS-JBF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"Cust","description":"Cust UDT",'
                      DELIMITED BY SIZE
                  '"properties":{' DELIMITED BY SIZE
                  '"veryLongFieldNamePushingPastCol40":{"type":"string"'
                      DELIMITED BY SIZE
                  ',"description":"Field doc","enum":["A","B"]},'
                      DELIMITED BY SIZE
                  '"optStr":{"type":["string","null"]},'
                      DELIMITED BY SIZE
                  '"i32":{"type":"integer","format":"int32"},'
                      DELIMITED BY SIZE
                  '"i64":{"type":"integer","format":"int64"},'
                      DELIMITED BY SIZE
                  '"uInt":{"type":"integer","minimum":0,"enum":[1,2]},'
                      DELIMITED BY SIZE
                  '"sInt":{"type":"integer","minimum":-9},'
                      DELIMITED BY SIZE
                  '"dec":{"type":"number","multipleOf":0.01},'
                      DELIMITED BY SIZE
                  '"uDec":{"type":"number","minimum":0},'
                      DELIMITED BY SIZE
                  '"flt":{"type":"number","format":"double"},'
                      DELIMITED BY SIZE
                  '"dfl":{"type":"number","format":"decimal"},'
                      DELIMITED BY SIZE
                  '"flg":{"type":"boolean"},' DELIMITED BY SIZE
                  '"af":{"type":"array","minItems":2,"maxItems":2,'
                      DELIMITED BY SIZE
                  '"items":{"type":"string"}},' DELIMITED BY SIZE
                  '"ao":{"type":"array","minItems":1,"maxItems":4,'
                      DELIMITED BY SIZE
                  '"items":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"k":{"type":"string"}}}},' DELIMITED BY SIZE
                  '"ad":{"type":"array","items":{"type":"integer"}}}}'
                      DELIMITED BY SIZE
                  INTO WS-JBF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-JBF-LEN
           END-SUBTRACT
           MOVE 10 TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 1 TO CDD-MPR-JSON-MODE
           MOVE 0 TO CDD-MPR-NAME-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ WS-JSON-BUF CDD-IR
               CDD-ERROR
           END-CALL
           MOVE 1 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-C60-VERBOSE
           MOVE 2 TO CDD-BUF-NL-KIND
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-OPT-C60-VERBOSE
           MOVE 1 TO CDD-BUF-NL-KIND
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           MOVE 3 TO CDD-IR-UDT-KIND
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-OPT-FORMAT
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-IR-UDT-KIND
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 20 TO CDD-BUF-CAP
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1048576 TO CDD-BUF-CAP.
      * @paragraph TEST-SPECIAL-IR-CONSTRUCTS Test N/U/66/SYNC/deep.
       TEST-SPECIAL-IR-CONSTRUCTS.
           MOVE 1 TO CDD-IR-UDT-KIND
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 0 TO CDD-IRF-MAX-LEN(1)
           MOVE 3 TO CDD-IRF-KIND(2)
           MOVE 12 TO CDD-IRF-MAX-LEN(2)
           MOVE 4 TO CDD-IRF-KIND(3)
           MOVE 0 TO CDD-IRF-MAX-LEN(3)
           MOVE 3 TO CDD-IRF-SYNC-JUST(3)
           MOVE 2 TO CDD-IRF-REDEF-LEN(3)
           MOVE "AF" TO CDD-IRF-REDEFINES(3)
           MOVE 4 TO CDD-IRF-KIND(4)
           MOVE 8 TO CDD-IRF-MAX-LEN(4)
           MOVE 0 TO CDD-IRF-JKEY-LEN(4)
           MOVE 5 TO CDD-IRF-KIND(5)
           MOVE 0 TO CDD-IRF-DIGITS(5)
           MOVE 6 TO CDD-IRF-KIND(7)
           MOVE 2 TO CDD-IRF-DIGITS(7)
           MOVE 2 TO CDD-IRF-SCALE(7)
           MOVE 0 TO CDD-IRF-SCALE(8)
           MOVE 0 TO CDD-IRF-ARR-MAX(12)
           MOVE 0 TO CDD-IRF-ARR-MAX(13)
           ADD 1 TO CDD-IR-FLD-COUNT
           END-ADD
           MOVE CDD-IR-FLD-COUNT TO WS-I
           MOVE 0 TO CDD-IRF-PARENT(WS-I)
           MOVE 0 TO CDD-IRF-NEXT-SIB(WS-I)
           MOVE WS-I TO CDD-IRF-NEXT-SIB(CDD-IR-ROOT-LAST)
           MOVE WS-I TO CDD-IR-ROOT-LAST
           MOVE 66 TO CDD-IRF-COB-LEVEL(WS-I)
           MOVE 5 TO CDD-IRF-CNAM-LEN(WS-I)
           MOVE "REN-A" TO CDD-IRF-COB-NAME(WS-I)
           MOVE 2 TO CDD-IRF-REDEF-LEN(WS-I)
           MOVE "AF" TO CDD-IRF-REDEFINES(WS-I)
           MOVE 0 TO CDD-IRF-JKEY-LEN(WS-I)
           MOVE 0 TO CDD-IRF-DESC-LEN(WS-I)
           MOVE 0 TO CDD-IRF-ARR-MODE(WS-I)
           ADD 1 TO CDD-IR-FLD-COUNT
           END-ADD
           MOVE CDD-IR-FLD-COUNT TO WS-I
           MOVE 0 TO CDD-IRF-PARENT(WS-I)
           MOVE 0 TO CDD-IRF-NEXT-SIB(WS-I)
           MOVE WS-I TO CDD-IRF-NEXT-SIB(CDD-IR-ROOT-LAST)
           MOVE WS-I TO CDD-IR-ROOT-LAST
           MOVE 1 TO CDD-IRF-KIND(WS-I)
           MOVE 0 TO CDD-IRF-FIRST-CHILD(WS-I)
           MOVE 0 TO CDD-IRF-COB-LEVEL(WS-I)
           MOVE 5 TO CDD-IRF-CNAM-LEN(WS-I)
           MOVE "EMPTY" TO CDD-IRF-COB-NAME(WS-I)
           MOVE 0 TO CDD-IRF-JKEY-LEN(WS-I)
           MOVE 0 TO CDD-IRF-DESC-LEN(WS-I)
           MOVE 0 TO CDD-IRF-ARR-MODE(WS-I)
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 8 TO CDD-OPT-DIALECT
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-OPT-DIALECT
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-C60-VERBOSE
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-OPT-C60-VERBOSE
           PERFORM TEST-DEEP-NESTING-LEVELS.
      * @paragraph TEST-DEEP-NESTING-LEVELS Exercise 11-level group.
       TEST-DEEP-NESTING-LEVELS.
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-IR-ROOT-FIRST
           MOVE 1 TO CDD-IR-ROOT-LAST
           MOVE 11 TO CDD-IR-FLD-COUNT
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 11
               COMPUTE CDD-IRF-PARENT(WS-I) = WS-I - 1
               END-COMPUTE
               MOVE 0 TO CDD-IRF-NEXT-SIB(WS-I)
               MOVE 2 TO CDD-IRF-CNAM-LEN(WS-I)
               MOVE "G1" TO CDD-IRF-COB-NAME(WS-I)
               MOVE 0 TO CDD-IRF-JKEY-LEN(WS-I)
               MOVE 0 TO CDD-IRF-DESC-LEN(WS-I)
               MOVE 0 TO CDD-IRF-COB-LEVEL(WS-I)
               MOVE 0 TO CDD-IRF-ARR-MODE(WS-I)
               IF WS-I < 11
                   MOVE 1 TO CDD-IRF-KIND(WS-I)
                   COMPUTE CDD-IRF-FIRST-CHILD(WS-I) = WS-I + 1
                   END-COMPUTE
               ELSE
                   MOVE 2 TO CDD-IRF-KIND(WS-I)
                   MOVE 0 TO CDD-IRF-FIRST-CHILD(WS-I)
                   MOVE 60 TO CDD-IRF-MAX-LEN(WS-I)
                   MOVE 2 TO CDD-IRF-ARR-MODE(WS-I)
                   MOVE 1 TO CDD-IRF-ARR-MIN(WS-I)
                   MOVE 5 TO CDD-IRF-ARR-MAX(WS-I)
                   MOVE 0 TO CDD-IRF-ENUM-COUNT(WS-I)
                   MOVE 0 TO CDD-IRF-NULLABLE(WS-I)
                   MOVE 0 TO CDD-IRF-REDEF-LEN(WS-I)
                   MOVE 0 TO CDD-IRF-SYNC-JUST(WS-I)
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
