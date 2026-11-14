      * @file TCSTB.cbl
      * @brief Unit test suite for CDDCSTB lossless COBOL CST builder.
      * @module TCSTB
      * @details Exercises envelope/capacity checks, level hierarchy
      *          errors, 01 records, TYPEDEFs, CLASS-IDs, 02-49/66/77/
      *          88 items, FILE/REPORT section opacity, doc/@json-name/
      *          @cdd-keep annotations, and PROCEDURE DIVISION routines.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
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
      * @field WS-CAP Capacity sweep loop variable.
           05  WS-CAP                    PIC 9(4) COMP VALUE 1.
      * @field WS-I Character loop index.
           05  WS-I                      PIC 9(4) COMP VALUE 1.
      * @subprogram TCSTB Verify 100% of CDDCSTB lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDCSTB test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-EMPTY
           PERFORM TEST-FULL-PROGRAM-AND-CLASS
           PERFORM TEST-HIERARCHY-AND-CAP-ERRORS
           IF WS-FAILS > 0
               DISPLAY "TCSTB FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph LEX-AND-BUILD Lex CDD-BUF and build CDD-CST.
       LEX-AND-BUILD.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE 8192 TO CDD-CTK-CAP
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF CDD-OK
               CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   CDD-ERROR
               END-CALL
           END-IF.
      * @paragraph TEST-GUARDS-AND-EMPTY Test tags, cap 0, empty stream.
       TEST-GUARDS-AND-EMPTY.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADCTK00" TO CDD-CTK-TAG
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "BADCST00" TO CDD-CST-TAG
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE 0 TO CDD-CST-CAP
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-CST-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 512 TO CDD-CST-CAP
           MOVE 0 TO CDD-CTK-COUNT
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-FULL-PROGRAM-AND-CLASS Test records/class/rtns.
       TEST-FULL-PROGRAM-AND-CLASS.
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 1 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "       IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROGRAM-ID. DEMO." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       ENVIRONMENT DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       FILE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  IGNORED-FILE-REC PIC X."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "      * @record CUST-REC Customer record doc"
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  CUST-REC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "      * @field CUST-ID Identifier" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "      * @json-name customerId" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           05  CUST-ID PIC 9(6)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           05  ADDR-GRP." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "               10  CITY PIC X(20)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "                   88  IS-NYC VALUE 'NYC'."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           66  ALIAS-A RENAMES CITY."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "      * @cdd-keep" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           05  HELPER-X PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           66  ALIAS-B RENAMES HELPER-X."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       77  STANDALONE-77 PIC 9(4)."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  T-DEF IS TYPEDEF STRONG PIC X(8)."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       1   SINGLE-DIGIT-01 PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       CUST-TO-JSON SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       P-BODY." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           CONTINUE." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       ID DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       CLASS-ID. ITEM-CLS." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  CLS-DATA PIC X(4). *> inline doc   "
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       METHOD-ID. TO-JSON." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           EXIT METHOD." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       END METHOD TO-JSON." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       END OBJECT." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       END CLASS ITEM-CLS." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "      * Trailing EOF comment" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM LEX-AND-BUILD
           IF NOT CDD-OK OR CDD-CST-COUNT < 10
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO WS-CAP
           PERFORM UNTIL WS-CAP > 35
               MOVE WS-CAP TO CDD-CST-CAP
               PERFORM LEX-AND-BUILD
               ADD 1 TO WS-CAP
               END-ADD
           END-PERFORM
           MOVE 512 TO CDD-CST-CAP
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "*> @json-name" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "*>   " DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "01 PIC X." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "PARA-ONE." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  MOVE 1 TO X" DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM LEX-AND-BUILD
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CLASS-ID. UNCLOSED-CLS." TO CDD-BUF-DATA(1:23)
           MOVE 23 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "END CLASS. END METHOD." TO CDD-BUF-DATA(1:22)
           MOVE 22 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           MOVE SPACES TO CDD-BUF-DATA
           MOVE "*> " TO CDD-BUF-DATA(1:3)
           MOVE 4 TO WS-I
           PERFORM UNTIL WS-I > 265
               MOVE "C" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE WS-LF TO CDD-BUF-DATA(266:1)
           MOVE "*> @json-name " TO CDD-BUF-DATA(267:14)
           MOVE 281 TO WS-I
           PERFORM UNTIL WS-I > 420
               MOVE "J" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE WS-LF TO CDD-BUF-DATA(421:1)
           MOVE "*> @record" TO CDD-BUF-DATA(422:10)
           MOVE WS-LF TO CDD-BUF-DATA(432:1)
           MOVE "01 " TO CDD-BUF-DATA(433:3)
           MOVE 436 TO WS-I
           PERFORM UNTIL WS-I > 505
               MOVE "N" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE "." TO CDD-BUF-DATA(506:1)
           MOVE 506 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-HIERARCHY-AND-CAP-ERRORS Test 2007 & 2011 errs.
       TEST-HIERARCHY-AND-CAP-ERRORS.
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE "05 ORPHAN-FLD PIC X." TO CDD-BUF-DATA(1:20)
           MOVE 20 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           IF NOT CDD-ERR-COB-LEVEL-HIERARCHY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "88 ORPHAN-88 VALUE 'Y'." TO CDD-BUF-DATA(1:23)
           MOVE 23 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           IF NOT CDD-ERR-COB-LEVEL-HIERARCHY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "66 ORPHAN-66 RENAMES A." TO CDD-BUF-DATA(1:23)
           MOVE 23 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           IF NOT CDD-ERR-COB-LEVEL-HIERARCHY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "01 BAD-REC PIC." TO CDD-BUF-DATA(1:15)
           MOVE 15 TO CDD-BUF-LEN
           PERFORM LEX-AND-BUILD
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "01 A. 05 B PIC X." TO CDD-BUF-DATA(1:17)
           MOVE 17 TO CDD-BUF-LEN
           MOVE 2 TO CDD-CST-CAP
           PERFORM LEX-AND-BUILD
           IF NOT CDD-ERR-COB-CST-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 512 TO CDD-CST-CAP.
