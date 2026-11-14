      * @file TSDK.cbl
      * @brief Public SDK integration test using ONLY cdd-cobol.cpy.
      * @module TSDK
      * @details Consumer COBOL test importing solely 'cdd-cobol.cpy'
      *          and invoking all 4 public SDK subprograms: CDDEMIT,
      *          CDDPARS, CDDSERD (JSON & COBOL inputs), and CDDSYN.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSDK.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-cobol.cpy".
      * @record WS-JSON-BUF Secondary buffer for sync JSON input.
       01  WS-JSON-BUF.
      * @field WS-JBF-TAG Magic tag ('CDDBUF01').
           05  WS-JBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-JBF-LEN Active JSON length.
           05  WS-JBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-JBF-CAP Capacity.
           05  WS-JBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-JBF-NL Newline mode.
           05  WS-JBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-JBF-DATA JSON bytes.
           05  WS-JBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-OUT-BUF Output buffer for SDK calls.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active output length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Newline mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Output bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-STATE Test assertion counter.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @subprogram TSDK Verify 100% of public SDK subprograms.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Exercise CDDEMIT, CDDPARS, CDDSERD, CDDSYN.
       MAIN-ENTRY.
           IF NOT CDD-VER-TAG-VALID
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM TEST-EMIT-AND-GUARDS
           PERFORM TEST-PARS-SERD-AND-SYN
           IF WS-FAILS > 0
               DISPLAY "TSDK FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-EMIT-AND-GUARDS Test CDDEMIT & tag checks.
       TEST-EMIT-AND-GUARDS.
           MOVE "BADOPT00" TO CDD-OPT-TAG
           CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADOBF00" TO WS-OBF-TAG
           CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE "!" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"id":10,"name":"Ada"}' TO CDD-BUF-DATA(1:22)
           MOVE 22 TO CDD-BUF-LEN
           CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-PARS-SERD-AND-SYN Test CDDPARS, CDDSERD, CDDSYN.
       TEST-PARS-SERD-AND-SYN.
           MOVE WS-OBF-DATA(1:WS-OBF-LEN) TO CDD-BUF-DATA(1:WS-OBF-LEN)
           MOVE WS-OBF-LEN TO CDD-BUF-LEN
           CALL "CDDPARS" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "BADOPT00" TO CDD-OPT-TAG
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADOBF00" TO WS-OBF-TAG
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO WS-OBF-TAG
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"id":10,"name":"Ada","extra":true}'
               TO WS-JBF-DATA(1:35)
           MOVE 35 TO WS-JBF-LEN
           CALL "CDDSYN" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "   " TO CDD-BUF-DATA(1:3)
           MOVE 3 TO CDD-BUF-LEN
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SYNC-UDT-NOT-FOUND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "000100!BAD" TO CDD-BUF-DATA(1:10)
           MOVE 10 TO CDD-BUF-LEN
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-INDICATOR
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "       05 ORPHAN PIC X." TO CDD-BUF-DATA(1:23)
           MOVE 23 TO CDD-BUF-LEN
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-LEVEL-HIERARCHY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '  {"id":10}' TO CDD-BUF-DATA(1:11)
           MOVE 11 TO CDD-BUF-LEN
           CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
