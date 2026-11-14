      * @file TCOVC.cbl
      * @brief Self-tests for CDDINST and CDDCOVC coverage gate tools.
      * @module TCOVC
      * @details Verifies instrumentation of single-armed IF statements
      *          (synthesized ELSE branch) and EVALUATE arms, proving
      *          unhit branches fail CDDCOVC (1) and complete hit sets
      *          pass CDDCOVC (0).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOVC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-NL-CONV Helper for LF byte constant.
       01  WS-NL-CONV.
      * @field WS-U16 16-bit integer initialized to 10 (LF).
           05  WS-U16                    PIC 9(4) COMP VALUE 10.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-LF Low byte LF (10).
               10  WS-LF                 PIC X(1).
      * @record WS-STATE Test state and counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR Write cursor in CDD-BUF-DATA.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-I Probe loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-PID-DISP 5-digit probe ID display.
           05  WS-PID-DISP               PIC 9(5) VALUE 0.
      * @subprogram TCOVC Run self-tests on CDDINST and CDDCOVC.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Test usage, instrumentation, & cov gate.
       MAIN-ENTRY.
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           MOVE 1 TO CDD-AGV-QUIET
           MOVE 0 TO CDD-AGV-COUNT
           CALL "MCDDINST"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           CALL "MCDDCOVC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM INSTRUMENT-SAMPLE
           PERFORM VERIFY-UNHIT-BRANCH-FAILS
           PERFORM VERIFY-FULL-HITS-PASS
           MOVE 0 TO CDD-AGV-MODE
           MOVE 0 TO CDD-AGV-QUIET
           IF WS-FAILS > 0
               DISPLAY "TCOVC FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph INSTRUMENT-SAMPLE Instrument synthetic COBOL source.
       INSTRUMENT-SAMPLE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/tcov_src.cbl" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "       IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROGRAM-ID. SMPL." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  WS-A PIC 9(2) COMP VALUE 1."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       MAIN-P." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           IF WS-A = 1" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "               ADD 1 TO WS-A" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "               END-ADD" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           END-IF" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           EXIT PROGRAM." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/tcov_src.lst" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE "build/tcov_src.cbl" TO CDD-BUF-DATA(1:18)
           MOVE 18 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE 3 TO CDD-AGV-COUNT
           MOVE "build/tcov_src.lst" TO CDD-AGV-ITEM(1)
           MOVE "build/test-drivers" TO CDD-AGV-ITEM(2)
           MOVE "build/tcov_man.dat" TO CDD-AGV-ITEM(3)
           CALL "MCDDINST"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph VERIFY-UNHIT-BRANCH-FAILS Omit synthesized ELSE hit.
       VERIFY-UNHIT-BRANCH-FAILS.
           MOVE "build/tcov_hits.dat" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE 2 TO CDD-PATH-OP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 4
               MOVE WS-I TO WS-PID-DISP
               STRING WS-PID-DISP DELIMITED BY SIZE
                      " 00000001" DELIMITED BY SIZE
                      WS-LF DELIMITED BY SIZE
                      INTO CDD-BUF-DATA WITH POINTER WS-PTR
               END-STRING
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-AGV-COUNT
           MOVE "build/tcov_man.dat" TO CDD-AGV-ITEM(1)
           MOVE "build/tcov_hits.dat" TO CDD-AGV-ITEM(2)
           CALL "MCDDCOVC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph VERIFY-FULL-HITS-PASS Mark all 6 probes hit -> pass.
       VERIFY-FULL-HITS-PASS.
           MOVE "build/tcov_hits.dat" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE 2 TO CDD-PATH-OP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 6
               MOVE WS-I TO WS-PID-DISP
               STRING WS-PID-DISP DELIMITED BY SIZE
                      " 00000001" DELIMITED BY SIZE
                      WS-LF DELIMITED BY SIZE
                      INTO CDD-BUF-DATA WITH POINTER WS-PTR
               END-STRING
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-AGV-COUNT
           MOVE "build/tcov_man.dat" TO CDD-AGV-ITEM(1)
           MOVE "build/tcov_hits.dat" TO CDD-AGV-ITEM(2)
           CALL "MCDDCOVC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
