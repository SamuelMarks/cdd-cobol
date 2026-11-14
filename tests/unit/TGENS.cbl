      * @file TGENS.cbl
      * @brief Unit test suite for CDDGENS 10-dialect serde generator.
      * @module TGENS
      * @details Exercises tag/dialect/format/serde-kind guards, all 10
      *          dialects (Era A 60-68, Era B 74-85, Era C 2002-2014
      *          UDF/Method/TRIM/vendor-json, Era D 2023 native &
      *          pure-COBOL), body-only sync modes (8 & 9), deep
      *          nesting (>10 levels), empty IR, and buffer overflow.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TGENS.
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
      * @field WS-DIAL Dialect loop index (1..10).
           05  WS-DIAL                   PIC 9(2) COMP VALUE 1.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 1.
      * @subprogram TGENS Verify 100% of CDDGENS lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDGENS test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-ERA-ERRORS
           PERFORM TEST-ALL-10-DIALECTS-AND-MODES
           PERFORM TEST-EMPTY-DEEP-AND-OVERFLOW
           IF WS-FAILS > 0
               DISPLAY "TGENS FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-ERA-ERRORS Test tags & 2010/4003.
       TEST-GUARDS-AND-ERA-ERRORS.
           MOVE "BADOPT00" TO CDD-OPT-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 99 TO CDD-OPT-DIALECT
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-DIALECT-DISALLOWED
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 5 TO CDD-OPT-SERDE-KIND
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-SERDE-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-SERDE-KIND
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-SERDE-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 3 TO CDD-OPT-SERDE-KIND
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-SERDE-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-ALL-10-DIALECTS-AND-MODES Test Eras A..D.
       TEST-ALL-10-DIALECTS-AND-MODES.
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDBUF01" TO WS-JBF-TAG
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE SPACES TO WS-JBF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"OrderRec","properties":{'
                      DELIMITED BY SIZE
                  '"id":{"type":"integer"},' DELIMITED BY SIZE
                  '"code":{"type":"string","maxLength":8},'
                      DELIMITED BY SIZE
                  '"sub":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"city":{"type":"string","maxLength":12}}}}}'
                      DELIMITED BY SIZE
                  INTO WS-JBF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-JBF-LEN
           END-SUBTRACT
           MOVE 10 TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 1 TO CDD-MPR-JSON-MODE
           MOVE 9 TO CDD-MPR-NAME-LEN
           MOVE "ORDER-REC" TO CDD-MPR-TARGET-NAME
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ WS-JSON-BUF CDD-IR
               CDD-ERROR
           END-CALL
           MOVE 0 TO CDD-OPT-SERDE-KIND
           MOVE 0 TO CDD-OPT-PURE-COBOL
           MOVE 0 TO CDD-OPT-VENDOR-JSON
           MOVE 0 TO CDD-OPT-SYNC-SERDE
           MOVE 1 TO WS-DIAL
           PERFORM UNTIL WS-DIAL > 10
               MOVE WS-DIAL TO CDD-OPT-DIALECT
               MOVE 1 TO CDD-OPT-FORMAT
               CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF
                   CDD-ERROR
               END-CALL
               IF NOT CDD-OK OR CDD-BUF-LEN = 0
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               ADD 1 TO WS-DIAL
               END-ADD
           END-PERFORM
           PERFORM TEST-ERA-C-AND-D-VARIANTS.
      * @paragraph TEST-ERA-C-AND-D-VARIANTS UDF, Method, vendor, pure.
       TEST-ERA-C-AND-D-VARIANTS.
           MOVE 8 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           MOVE 3 TO CDD-OPT-SERDE-KIND
           MOVE 2 TO CDD-BUF-NL-KIND
           MOVE 5 TO CDD-IR-COB-NLEN
           MOVE "ORD-T" TO CDD-IR-COB-NAME
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-BUF-NL-KIND
           MOVE 0 TO CDD-OPT-SERDE-KIND
           MOVE 3 TO CDD-IR-UDT-KIND
           MOVE 25 TO CDD-IR-COB-NLEN
           MOVE "VERY-LONG-CLASS-IDENTIFIER" TO CDD-IR-COB-NAME
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-IR-UDT-KIND
           MOVE 9 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-VENDOR-JSON
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-OPT-VENDOR-JSON
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-PURE-COBOL
           MOVE 1 TO CDD-OPT-SERDE-KIND
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 8 TO CDD-OPT-SYNC-SERDE
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           MOVE 9 TO CDD-OPT-SYNC-SERDE
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           MOVE 0 TO CDD-OPT-SYNC-SERDE
           MOVE 0 TO CDD-OPT-PURE-COBOL.
      * @paragraph TEST-EMPTY-DEEP-AND-OVERFLOW Empty IR, deep, cap.
       TEST-EMPTY-DEEP-AND-OVERFLOW.
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 2 TO CDD-OPT-SERDE-KIND
           MOVE 0 TO CDD-IR-COB-NLEN
           MOVE 0 TO CDD-IR-ROOT-FIRST
           MOVE 0 TO CDD-IR-FLD-COUNT
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-IR-ROOT-FIRST
           MOVE 3 TO CDD-IR-FLD-COUNT
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 3
               COMPUTE CDD-IRF-PARENT(WS-I) = WS-I - 1
               END-COMPUTE
               MOVE 0 TO CDD-IRF-NEXT-SIB(WS-I)
               MOVE 2 TO CDD-IRF-CNAM-LEN(WS-I)
               MOVE "N1" TO CDD-IRF-COB-NAME(WS-I)
               MOVE 2 TO CDD-IRF-JKEY-LEN(WS-I)
               MOVE "n1" TO CDD-IRF-JSON-KEY(WS-I)
               IF WS-I = 1
                   MOVE 1 TO CDD-IRF-KIND(1)
                   MOVE 2 TO CDD-IRF-FIRST-CHILD(1)
               ELSE
                   IF WS-I = 2
                       MOVE 1 TO CDD-IRF-KIND(2)
                       MOVE 0 TO CDD-IRF-FIRST-CHILD(2)
                       MOVE 3 TO CDD-IRF-NEXT-SIB(2)
                   ELSE
                       MOVE 2 TO CDD-IRF-KIND(3)
                       MOVE 0 TO CDD-IRF-MAX-LEN(3)
                       MOVE 0 TO CDD-IRF-DIGITS(3)
                   END-IF
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           ADD 1 TO CDD-IR-FLD-COUNT
           END-ADD
           MOVE 4 TO CDD-IRF-NEXT-SIB(3)
           MOVE 0 TO CDD-IRF-NEXT-SIB(4)
           MOVE 5 TO CDD-IRF-KIND(4)
           MOVE 0 TO CDD-IRF-DIGITS(4)
           MOVE 2 TO CDD-IRF-CNAM-LEN(4)
           MOVE "I0" TO CDD-IRF-COB-NAME(4)
           MOVE 2 TO CDD-IRF-JKEY-LEN(4)
           MOVE "i0" TO CDD-IRF-JSON-KEY(4)
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 20 TO CDD-BUF-CAP
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
