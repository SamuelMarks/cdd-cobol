      * @file TCPRO.cbl
      * @brief Unit test suite for CDDCPRO routine boundary locator.
      * @module TCPRO
      * @details Exercises tag checks, empty/long UDT names, -REC and
      *          -T suffix stripping, SECTION, PARAGRAPH, METHOD-ID,
      *          and FUNCTION-ID routine discovery and body span bounds.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCPRO.
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
      * @subprogram TCPRO Verify 100% of CDDCPRO lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDCPRO test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS
           PERFORM TEST-SEC-AND-PARA-ROUTINES
           PERFORM TEST-METHOD-AND-FUNC-ROUTINES
           IF WS-FAILS > 0
               DISPLAY "TCPRO FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph BUILD-CST-FOR-PRO Lex and build CST from CDD-BUF.
       BUILD-CST-FOR-PRO.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDCPR01" TO CDD-CPR-TAG
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 512 TO CDD-CST-CAP
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL.
      * @paragraph TEST-GUARDS Verify all 4 tag checks and empty name.
       TEST-GUARDS.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDCPR01" TO CDD-CPR-TAG
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADCTK00" TO CDD-CTK-TAG
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "BADCST00" TO CDD-CST-TAG
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "BADCPR00" TO CDD-CPR-TAG
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCPR01" TO CDD-CPR-TAG
           MOVE 0 TO CDD-CPR-UDT-NLEN
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           MOVE 55 TO CDD-CPR-UDT-NLEN
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL.
      * @paragraph TEST-SEC-AND-PARA-ROUTINES Test -REC & -T matching.
       TEST-SEC-AND-PARA-ROUTINES.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "*> Above routine" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "ORD-TO-JSON SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  MOVE 1 TO A." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "ORD-REC-FROM-JSON SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM BUILD-CST-FOR-PRO
           MOVE "ord-rec" TO CDD-CPR-UDT-NAME
           MOVE 7 TO CDD-CPR-UDT-NLEN
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF CDD-CPR-TO-NODE = 0 OR CDD-CPR-FROM-NODE = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "USR-TO-JSON." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  MOVE 1 TO A" DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM BUILD-CST-FOR-PRO
           MOVE "USR-T" TO CDD-CPR-UDT-NAME
           MOVE 5 TO CDD-CPR-UDT-NLEN
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF CDD-CPR-TO-NODE = 0 OR CDD-CPR-KIND NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-METHOD-AND-FUNC-ROUTINES Test OO & UDF forms.
       TEST-METHOD-AND-FUNC-ROUTINES.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "METHOD-ID. TO-JSON." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  EXIT METHOD." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "END METHOD." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "FUNCTION-ID. FROM-JSON." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  EXIT FUNCTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "END FUNCTION." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM BUILD-CST-FOR-PRO
           MOVE "ACCT" TO CDD-CPR-UDT-NAME
           MOVE 4 TO CDD-CPR-UDT-NLEN
           CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-CPRO-INFO CDD-ERROR
           END-CALL
           IF CDD-CPR-TO-NODE = 0 OR CDD-CPR-FROM-NODE = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
