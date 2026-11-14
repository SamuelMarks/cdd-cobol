      * @file TCSTS.cbl
      * @brief Unit test suite for CDDCSTS lossless CST serializer.
      * @module TCSTS
      * @details Proves CDDCSTS(CDDCSTB(CDDCLEX(src))) == src byte-for-
      *          byte across all 10 dialects and exercises KEEP,
      *          DELETE, SYNTH, and SPLICE surgical patch modes plus
      *          buffer capacity checks.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-err.cpy".
      * @record WS-OUT-BUF Serialized output COBOL byte buffer.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active serialized byte length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Maximum capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Line-ending mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Serialized output COBOL bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-BYTE-CONV CR/LF helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer.
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
      * @field WS-LF LF byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR CR byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-DIAL Dialect loop index (1..10).
           05  WS-DIAL                   PIC 9(2) COMP VALUE 1.
      * @subprogram TCSTS Verify 100% of CDDCSTS lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDCSTS test groups.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           PERFORM TEST-GUARDS-AND-EMPTY
           PERFORM TEST-ROUNDTRIP-ALL-10-DIALECTS
           PERFORM TEST-CLASS-AND-PATCH-MODES
           IF WS-FAILS > 0
               DISPLAY "TCSTS FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-EMPTY Test all 4 tags & empty CST.
       TEST-GUARDS-AND-EMPTY.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDBUF01" TO WS-OBF-TAG
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADCTK00" TO CDD-CTK-TAG
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "BADCST00" TO CDD-CST-TAG
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "BADOBF00" TO WS-OBF-TAG
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE 0 TO CDD-CST-ROOT
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-ROUNDTRIP-ALL-10-DIALECTS Prove exact identity.
       TEST-ROUNDTRIP-ALL-10-DIALECTS.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "000100 IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  "                                         TAG00001"
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000110 DATA DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000120 WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000130* LEADING COMMENT" DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000140 01  CUST-REC." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000150     05  F-1       PIC X(10)."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000160     05  F-2       PIC 9(4)."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000170 PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000180 CUST-TO-JSON SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000190     STOP RUN." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 512 TO CDD-CST-CAP
           MOVE 1 TO WS-DIAL
           PERFORM UNTIL WS-DIAL > 10
               MOVE WS-DIAL TO CDD-CTK-DIALECT
               MOVE 1 TO CDD-CTK-FMT-REQ
               CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
               END-CALL
               CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   CDD-ERROR
               END-CALL
               CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   WS-OUT-BUF CDD-ERROR
               END-CALL
               IF NOT CDD-OK
                   OR WS-OBF-LEN NOT = CDD-BUF-LEN
                   OR WS-OBF-DATA(1:WS-OBF-LEN)
                       NOT = CDD-BUF-DATA(1:CDD-BUF-LEN)
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               ADD 1 TO WS-DIAL
               END-ADD
           END-PERFORM.
      * @paragraph TEST-CLASS-AND-PATCH-MODES Test DELETE/SYNTH/SPLICE.
       TEST-CLASS-AND-PATCH-MODES.
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "CLASS-ID. C1." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "01 R1." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 A PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "    88 A-OK VALUE 'Y'." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 B PIC 9(2)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "END CLASS C1." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               CDD-ERROR
           END-CALL
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF WS-OBF-LEN NOT = CDD-BUF-LEN
               OR WS-OBF-DATA(1:WS-OBF-LEN)
                   NOT = CDD-BUF-DATA(1:CDD-BUF-LEN)
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "PIC X(20)" TO CDD-CST-PATCH-BUF(1:9)
           MOVE "  05 C PIC X." TO CDD-CST-PATCH-BUF(10:13)
           MOVE 3 TO CDD-CSN-PATCH-MODE(5)
           MOVE CDD-CSN-CODE-START(5) TO CDD-CSN-SPLICE-T1(5)
           MOVE CDD-CSN-CODE-END(5) TO CDD-CSN-SPLICE-T2(5)
           MOVE 1 TO CDD-CSN-PATCH-OFF(5)
           MOVE 9 TO CDD-CSN-PATCH-LEN(5)
           MOVE 1 TO CDD-CSN-PATCH-MODE(6)
           MOVE 3 TO CDD-CSN-PATCH-MODE(7)
           MOVE CDD-CSN-CODE-START(7) TO CDD-CSN-SPLICE-T1(7)
           MOVE CDD-CSN-CODE-END(7) TO CDD-CSN-SPLICE-T2(7)
           MOVE 1 TO CDD-CSN-PATCH-OFF(7)
           MOVE 9 TO CDD-CSN-PATCH-LEN(7)
           ADD 1 TO CDD-CST-COUNT
           END-ADD
           MOVE 2 TO CDD-CSN-PATCH-MODE(CDD-CST-COUNT)
           MOVE 10 TO CDD-CSN-PATCH-OFF(CDD-CST-COUNT)
           MOVE 13 TO CDD-CSN-PATCH-LEN(CDD-CST-COUNT)
           MOVE 0 TO CDD-CSN-FIRST-CHILD(CDD-CST-COUNT)
           MOVE 0 TO CDD-CSN-FOOT-START(CDD-CST-COUNT)
           MOVE 0 TO CDD-CSN-NEXT-SIB(CDD-CST-COUNT)
           MOVE CDD-CST-COUNT TO CDD-CSN-NEXT-SIB(7)
           ADD 1 TO CDD-CST-COUNT
           END-ADD
           MOVE 2 TO CDD-CSN-PATCH-MODE(CDD-CST-COUNT)
           MOVE 10 TO CDD-CSN-PATCH-OFF(CDD-CST-COUNT)
           MOVE 0 TO CDD-CSN-PATCH-LEN(CDD-CST-COUNT)
           MOVE 6 TO CDD-CSN-FIRST-CHILD(CDD-CST-COUNT)
           MOVE 0 TO CDD-CSN-FOOT-START(CDD-CST-COUNT)
           MOVE 0 TO CDD-CSN-NEXT-SIB(CDD-CST-COUNT)
           MOVE CDD-CST-COUNT TO CDD-CSN-NEXT-SIB(CDD-CST-COUNT - 1)
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-CSN-PATCH-MODE(2)
           MOVE CDD-CSN-TOK-START(2) TO CDD-CSN-SPLICE-T1(2)
           MOVE CDD-CSN-CODE-END(2) TO CDD-CSN-SPLICE-T2(2)
           MOVE 1 TO CDD-CSN-PATCH-OFF(2)
           MOVE 9 TO CDD-CSN-PATCH-LEN(2)
           MOVE 4 TO WS-OBF-CAP
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 20 TO WS-OBF-CAP
           CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
