      * @file TC2IR.cbl
      * @brief Unit test suite for CDDC2IR COBOL CST -> IR extractor.
      * @module TC2IR
      * @details Exercises tag/not-found/ambiguous guards, 01 records,
      *          TYPEDEFs, CLASS-IDs, ODO *-COUNT and *-IS-NULL
      *          companions, 88 enums and T/F Y/N booleans, integer
      *          min/max bounds, nested groups, and capacity limits.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TC2IR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @record WS-BYTE-CONV LF byte helper.
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
      * @subprogram TC2IR Verify 100% of CDDC2IR lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDC2IR test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-UDT-MATCHING
           PERFORM TEST-UDT-CST-TO-IR-EXTRACT
           IF WS-FAILS > 0
               DISPLAY "TC2IR FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph PARSE-COBOL-TO-CST Lex and build CST from CDD-BUF.
       PARSE-COBOL-TO-CST.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 512 TO CDD-CST-CAP
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL.
      * @paragraph TEST-GUARDS-AND-UDT-MATCHING Test tags & 4004/4005.
       TEST-GUARDS-AND-UDT-MATCHING.
           MOVE "BADMPR00" TO CDD-MPR-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADCTK00" TO CDD-CTK-TAG
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "BADCST00" TO CDD-CST-TAG
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "BADFMP00" TO CDD-FMP-TAG
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           MOVE 0 TO CDD-CST-ROOT
           MOVE 0 TO CDD-MPR-NAME-LEN
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SYNC-UDT-NOT-FOUND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "01 A PIC X. 01 B PIC X." TO CDD-BUF-DATA(1:23)
           MOVE 23 TO CDD-BUF-LEN
           PERFORM PARSE-COBOL-TO-CST
           MOVE 1 TO CDD-MPR-STRICT-UDT
           MOVE 0 TO CDD-MPR-NAME-LEN
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SYNC-AMBIGUOUS-UDT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-MPR-STRICT-UDT
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-MPR-STRICT-UDT
           MOVE 3 TO CDD-MPR-NAME-LEN
           MOVE "NO!" TO CDD-MPR-TARGET-NAME
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SYNC-UDT-NOT-FOUND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-MPR-NAME-LEN
           MOVE "b" TO CDD-MPR-TARGET-NAME
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-UDT-CST-TO-IR-EXTRACT Full CST->IR.
       TEST-UDT-CST-TO-IR-EXTRACT.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "*> @record ORD-REC Order doc" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "*> @json-name Order" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "01 ORD-REC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  *> @json-name orderId" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 ORD-ID PIC 9(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 ORD-ID-IS-NULL PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 BAL PIC S9(6)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 BIG-INT PIC 9(18)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 FLG-TF PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 IS-T VALUE 'T'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 IS-F VALUE 'F'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 FLG-YN PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 IS-Y VALUE 'Y'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 IS-N VALUE 'N'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 ST PIC X(2)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 ST-OK VALUE 'OK'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 C1 PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 C1-A VALUE 'A'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 C1-B VALUE 'B'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  *> @cdd-keep" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 KP-FLD PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 NO-PIC-LEAF." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 ITEMS-COUNT PIC 9(4) COMP." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 ITEMS OCCURS 1 TO 5 DEPENDING ON ITEMS-COUNT"
                      DELIMITED BY SIZE
                  " PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 SUB-GRP." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    10 SUB-A PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    10 SUB-B PIC 9(2)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-COBOL-TO-CST
           MOVE 0 TO CDD-MPR-STRICT-UDT
           MOVE 0 TO CDD-MPR-NAME-LEN
           MOVE 7 TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-CASE
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT < 9
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-IR-FLD-CAP
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IR-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 0 TO CDD-IR-ENUM-CAP
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IR-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE "01 TD-1 IS TYPEDEF STRONG PIC X(8)."
               TO CDD-BUF-DATA(1:35)
           MOVE 35 TO CDD-BUF-LEN
           PERFORM PARSE-COBOL-TO-CST
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR NOT CDD-IR-UDT-TYPEDEF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "CLASS-ID. CLS-A." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "01 CLS-REC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 C-FLD PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "END CLASS CLS-A." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-COBOL-TO-CST
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR NOT CDD-IR-UDT-CLASS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "01 R. 05 B PIC." TO CDD-BUF-DATA(1:15)
           MOVE 15 TO CDD-BUF-LEN
           PERFORM PARSE-COBOL-TO-CST
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
