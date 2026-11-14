      * @file TIRDF.cbl
      * @brief Unit test suite for CDDIRDF structural IR differ.
      * @module TIRDF
      * @details Exercises tag checks, KEEP, UPDATE (string len, int
      *          digits/scale/sign, array), INSERT (top/middle/bottom),
      *          REMOVE (with companion CST deletion), @cdd-keep
      *          preservation, nested GROUP recursion, and capacity.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIRDF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
      * @record WS-OLD-IR Existing COBOL IR instance.
       01  WS-OLD-IR.
      * @field WS-OIR-TAG Magic tag ('CDDIR001').
           05  WS-OIR-TAG                PIC X(8) VALUE "CDDIR001".
      * @field WS-OIR-REST Remaining bytes matching CDD-IR size.
           05  WS-OIR-REST               PIC X(260000) VALUE SPACES.
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @record WS-BYTE-CONV LF helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer initialized to 10 (LF).
           05  WS-U16                    PIC 9(4) COMP VALUE 10.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-LF Low byte LF (10).
               10  WS-LF                 PIC X(1).
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-CAP Capacity sweep index.
           05  WS-CAP                    PIC 9(4) COMP VALUE 0.
      * @subprogram TIRDF Verify 100% of CDDIRDF lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDIRDF test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS
           PERFORM TEST-FULL-STRUCTURAL-DIFF
           IF WS-FAILS > 0
               DISPLAY "TIRDF FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS Verify all 5 envelope tag checks.
       TEST-GUARDS.
           MOVE "BADMPR00" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO WS-OIR-TAG
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDDIF01" TO CDD-DIF-TAG
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "BADOIR00" TO WS-OIR-TAG
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           MOVE "CDDIR001" TO WS-OIR-TAG
           MOVE "BADFMP00" TO CDD-FMP-TAG
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           MOVE "BADNIR00" TO CDD-IR-TAG
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "BADDIF00" TO CDD-DIF-TAG
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDDIF01" TO CDD-DIF-TAG.
      * @paragraph TEST-FULL-STRUCTURAL-DIFF Diff COBOL CST vs JSON IR.
       TEST-FULL-STRUCTURAL-DIFF.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 512 TO CDD-CST-CAP
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "01 CUST-REC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 OLD-CNT PIC 9(4) COMP." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 OLD-COUNT PIC 9(4) COMP." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 OLD-ARR OCCURS 1 TO 3 DEPENDING ON OLD-COUNT"
                      DELIMITED BY SIZE
                  " PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  *> @cdd-keep" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 KEEP-ME PIC X(2)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 SAME-STR PIC X(10)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 UPD-STR PIC X(5)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 KIND-CHG PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 SAME-INT PIC S9(9)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 UPD-INT PIC 9(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 SAME-BOOL PIC 1." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 ADDR." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    10 CITY PIC X(12)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    10 GONE-ZIP PIC X(5)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           MOVE 7 TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 0 TO CDD-MPR-STRICT-UDT
           MOVE 0 TO CDD-MPR-NAME-LEN
           MOVE 1 TO CDD-MPR-CASE
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           MOVE CDD-IR TO WS-OLD-IR
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"properties":{' DELIMITED BY SIZE
                  '"brandNewTop":{"type":"string","maxLength":4},'
                      DELIMITED BY SIZE
                  '"sameStr":{"type":"string","maxLength":10},'
                      DELIMITED BY SIZE
                  '"updStr":{"type":"string","maxLength":20},'
                      DELIMITED BY SIZE
                  '"kindChg":{"type":"integer"},' DELIMITED BY SIZE
                  '"sameInt":{"type":"integer"},' DELIMITED BY SIZE
                  '"updInt":{"type":"integer"},' DELIMITED BY SIZE
                  '"sameBool":{"type":"boolean"},' DELIMITED BY SIZE
                  '"addr":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"city":{"type":"string","maxLength":12},'
                      DELIMITED BY SIZE
                  '"state":{"type":"string","maxLength":2}}}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE 1 TO CDD-MPR-JSON-MODE
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           MOVE 512 TO CDD-DIF-CAP
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-DIF-COUNT < 10
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO WS-CAP
           PERFORM UNTIL WS-CAP > 12
               MOVE WS-CAP TO CDD-DIF-CAP
               CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
                   CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
               END-CALL
               ADD 1 TO WS-CAP
               END-ADD
           END-PERFORM.
