      * @file CDDCOVC.cbl
      * @brief 100% function, line, and branch coverage gate verifier.
      * @module CDDCOVC
      * @details Aggregates probe manifest records (manifest.dat) with
      *          merged runtime hit counters (hits.dat) and optional
      *          lcov coverage.info records, failing with exact
      *          file:line diagnostics if Function, Line, or Branch
      *          coverage is below 100.00%.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCOVC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-HIT-TABLE Loaded probe hit counts by probe ID.
       01  WS-HIT-TABLE.
      * @field WS-HIT-VAL Hit count for probe ID 1..32768.
           05  WS-HIT-VAL                PIC 9(8) COMP
                                         OCCURS 32768 TIMES.
      * @record WS-COV-STATS Aggregated coverage totals and hits.
       01  WS-COV-STATS.
      * @field WS-ARGC Command-line argument count.
           05  WS-ARGC                   PIC 9(4) COMP VALUE 0.
      * @field WS-ARG-MAN Path to manifest.dat.
           05  WS-ARG-MAN                PIC X(256) VALUE SPACES.
      * @field WS-ARG-HITS Path to hits.dat.
           05  WS-ARG-HITS               PIC X(256) VALUE SPACES.
      * @field WS-EXIT-CODE Process exit status (0=100%, 1=<100%).
           05  WS-EXIT-CODE              PIC 9(4) COMP VALUE 0.
      * @field WS-MAN-ST File status for MANIFEST-FILE.
           05  WS-MAN-ST                 PIC X(2) VALUE "00".
      * @field WS-MAN-EOF 1 once EOF reached on MANIFEST-FILE.
           05  WS-MAN-EOF                PIC 9(2) COMP VALUE 0.
      * @field WS-IDX Loop counter (1..32768).
           05  WS-IDX                    PIC 9(8) COMP VALUE 0.
      * @field WS-POS Byte cursor in CDD-BUF-DATA.
           05  WS-POS                    PIC 9(8) COMP VALUE 1.
      * @field WS-PID-TXT 5-digit probe ID display slice.
           05  WS-PID-TXT                PIC 9(5) VALUE 0.
      * @field WS-CNT-TXT 8-digit hit count display slice.
           05  WS-CNT-TXT                PIC 9(8) VALUE 0.
      * @field WS-LNO-TXT 6-digit line number display slice.
           05  WS-LNO-TXT                PIC 9(6) VALUE 0.
      * @field WS-PID Parsed probe ID.
           05  WS-PID                    PIC 9(8) COMP VALUE 0.
      * @field WS-CNT Parsed hit count.
           05  WS-CNT                    PIC 9(8) COMP VALUE 0.
      * @field WS-KIND Probe kind ('F', 'L', or 'B').
           05  WS-KIND                   PIC X(1) VALUE SPACE.
      * @field WS-FPATH Probe source file path.
           05  WS-FPATH                  PIC X(64) VALUE SPACES.
      * @field WS-FLEN Trimmed length of WS-FPATH.
           05  WS-FLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-F-TOT Total function/paragraph probes.
           05  WS-F-TOT                  PIC 9(6) COMP VALUE 0.
      * @field WS-F-HIT Covered function/paragraph probes.
           05  WS-F-HIT                  PIC 9(6) COMP VALUE 0.
      * @field WS-L-TOT Total executable line probes.
           05  WS-L-TOT                  PIC 9(6) COMP VALUE 0.
      * @field WS-L-HIT Covered executable line probes.
           05  WS-L-HIT                  PIC 9(6) COMP VALUE 0.
      * @field WS-B-TOT Total branch arms.
           05  WS-B-TOT                  PIC 9(6) COMP VALUE 0.
      * @field WS-B-HIT Covered branch arms.
           05  WS-B-HIT                  PIC 9(6) COMP VALUE 0.
      * @field WS-UNCOV-SHOWN Uncovered lines printed (cap 40).
           05  WS-UNCOV-SHOWN            PIC 9(4) COMP VALUE 0.
      * @field WS-DISP-FT Display total functions.
           05  WS-DISP-FT                PIC 9(6) VALUE 0.
      * @field WS-DISP-FH Display hit functions.
           05  WS-DISP-FH                PIC 9(6) VALUE 0.
      * @field WS-DISP-LT Display total lines.
           05  WS-DISP-LT                PIC 9(6) VALUE 0.
      * @field WS-DISP-LH Display hit lines.
           05  WS-DISP-LH                PIC 9(6) VALUE 0.
      * @field WS-DISP-BT Display total branches.
           05  WS-DISP-BT                PIC 9(6) VALUE 0.
      * @field WS-DISP-BH Display hit branches.
           05  WS-DISP-BH                PIC 9(6) VALUE 0.
      * @subprogram CDDCOVC Enforce 100.00% fn, line, & branch coverage.
      * @return Exit status 0 when 100% covered; non-zero otherwise.
      * @errors Prints uncovered file:line locations to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Load hits, evaluate manifest, enforce.
       MAIN-ENTRY.
           MOVE 0 TO WS-EXIT-CODE
           PERFORM CLEAR-HIT-TABLE
           PERFORM PARSE-CLI-ARGS
           IF WS-EXIT-CODE = 0
               PERFORM LOAD-HITS-FILE
           END-IF
           IF WS-EXIT-CODE = 0
               PERFORM EVALUATE-MANIFEST
           END-IF
           IF WS-EXIT-CODE = 0
               PERFORM CHECK-GATES
           END-IF
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE WS-EXIT-CODE TO CDD-AGV-EXIT-CODE
           END-IF
           STOP RUN RETURNING WS-EXIT-CODE.
      * @paragraph CLEAR-HIT-TABLE Zero WS-HIT-VAL(1..32768).
       CLEAR-HIT-TABLE.
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > 32768
               MOVE 0 TO WS-HIT-VAL(WS-IDX)
               ADD 1 TO WS-IDX
               END-ADD
           END-PERFORM
           MOVE 0 TO WS-F-TOT
           MOVE 0 TO WS-F-HIT
           MOVE 0 TO WS-L-TOT
           MOVE 0 TO WS-L-HIT
           MOVE 0 TO WS-B-TOT
           MOVE 0 TO WS-B-HIT
           MOVE 0 TO WS-UNCOV-SHOWN.
      * @paragraph PARSE-CLI-ARGS Read manifest and hits paths.
       PARSE-CLI-ARGS.
           MOVE SPACES TO WS-ARG-MAN
           MOVE SPACES TO WS-ARG-HITS
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE CDD-AGV-COUNT TO WS-ARGC
               IF WS-ARGC >= 2
                   MOVE CDD-AGV-ITEM(1) TO WS-ARG-MAN
                   MOVE CDD-AGV-ITEM(2) TO WS-ARG-HITS
               END-IF
           ELSE
               ACCEPT WS-ARGC FROM ARGUMENT-NUMBER
               END-ACCEPT
               IF WS-ARGC >= 2
                   MOVE 1 TO WS-IDX
                   DISPLAY WS-IDX UPON ARGUMENT-NUMBER
                   END-DISPLAY
                   ACCEPT WS-ARG-MAN FROM ARGUMENT-VALUE
                   END-ACCEPT
                   MOVE 2 TO WS-IDX
                   DISPLAY WS-IDX UPON ARGUMENT-NUMBER
                   END-DISPLAY
                   ACCEPT WS-ARG-HITS FROM ARGUMENT-VALUE
                   END-ACCEPT
               END-IF
           END-IF
           IF WS-ARGC < 2 OR WS-ARG-MAN = SPACES
               MOVE 2 TO WS-EXIT-CODE
           END-IF.
      * @paragraph LOAD-HITS-FILE Read hits.dat into WS-HIT-TABLE.
       LOAD-HITS-FILE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 1 TO CDD-PATH-OP
           MOVE 256 TO WS-FLEN
           PERFORM UNTIL WS-FLEN = 1
               OR WS-ARG-HITS(WS-FLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-FLEN
               END-SUBTRACT
           END-PERFORM
           MOVE WS-FLEN TO CDD-PATH-LEN
           MOVE WS-ARG-HITS TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF CDD-OK
               MOVE 1 TO WS-POS
               PERFORM UNTIL WS-POS + 14 > CDD-BUF-LEN
                   MOVE CDD-BUF-DATA(WS-POS:5) TO WS-PID-TXT
                   MOVE WS-PID-TXT TO WS-PID
                   MOVE CDD-BUF-DATA(WS-POS + 6:8) TO WS-CNT-TXT
                   MOVE WS-CNT-TXT TO WS-CNT
                   IF WS-PID >= 1 AND WS-PID <= 32768
                       ADD WS-CNT TO WS-HIT-VAL(WS-PID)
                       END-ADD
                   END-IF
                   ADD 15 TO WS-POS
                   END-ADD
               END-PERFORM
           END-IF.
      * @paragraph EVALUATE-MANIFEST Score every 48-byte manifest row.
       EVALUATE-MANIFEST.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 1 TO CDD-PATH-OP
           MOVE 256 TO WS-FLEN
           PERFORM UNTIL WS-FLEN = 1
               OR WS-ARG-MAN(WS-FLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-FLEN
               END-SUBTRACT
           END-PERFORM
           MOVE WS-FLEN TO CDD-PATH-LEN
           MOVE WS-ARG-MAN TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               MOVE 3 TO WS-EXIT-CODE
           ELSE
               MOVE 1 TO WS-POS
               PERFORM UNTIL WS-POS + 47 > CDD-BUF-LEN
                   PERFORM SCORE-MANIFEST-ROW
                   ADD 48 TO WS-POS
                   END-ADD
               END-PERFORM
           END-IF.
      * @paragraph SCORE-MANIFEST-ROW Tallies F/L/B and logs misses.
       SCORE-MANIFEST-ROW.
           MOVE CDD-BUF-DATA(WS-POS:5) TO WS-PID-TXT
           MOVE WS-PID-TXT TO WS-PID
           MOVE CDD-BUF-DATA(WS-POS + 6:1) TO WS-KIND
           MOVE CDD-BUF-DATA(WS-POS + 8:6) TO WS-LNO-TXT
           MOVE SPACES TO WS-FPATH
           MOVE CDD-BUF-DATA(WS-POS + 15:32) TO WS-FPATH(1:32)
           EVALUATE WS-KIND
               WHEN "F"
                   ADD 1 TO WS-F-TOT
                   END-ADD
                   IF WS-HIT-VAL(WS-PID) > 0
                       ADD 1 TO WS-F-HIT
                       END-ADD
                   ELSE
                       PERFORM PRINT-UNCOVERED
                   END-IF
               WHEN "L"
                   ADD 1 TO WS-L-TOT
                   END-ADD
                   IF WS-HIT-VAL(WS-PID) > 0
                       ADD 1 TO WS-L-HIT
                       END-ADD
                   ELSE
                       PERFORM PRINT-UNCOVERED
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-B-TOT
                   END-ADD
                   IF WS-HIT-VAL(WS-PID) > 0
                       ADD 1 TO WS-B-HIT
                       END-ADD
                   ELSE
                       PERFORM PRINT-UNCOVERED
                   END-IF
           END-EVALUATE.
      * @paragraph PRINT-UNCOVERED Report uncovered probe to SYSERR.
       PRINT-UNCOVERED.
           IF NOT CDD-AGV-QUIET-YES AND WS-UNCOV-SHOWN < 40
               ADD 1 TO WS-UNCOV-SHOWN
               END-ADD
               MOVE 64 TO WS-FLEN
               PERFORM UNTIL WS-FLEN = 1
                   OR WS-FPATH(WS-FLEN:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-FLEN
                   END-SUBTRACT
               END-PERFORM
               DISPLAY "UNCOVERED [" WS-KIND "] "
                   WS-FPATH(1:WS-FLEN) ":" WS-LNO-TXT
                   " (probe #" WS-PID-TXT ")" UPON SYSERR
               END-DISPLAY
           END-IF.
      * @paragraph CHECK-GATES Require 100.00% across F, L, and B.
       CHECK-GATES.
           MOVE WS-F-TOT TO WS-DISP-FT
           MOVE WS-F-HIT TO WS-DISP-FH
           MOVE WS-L-TOT TO WS-DISP-LT
           MOVE WS-L-HIT TO WS-DISP-LH
           MOVE WS-B-TOT TO WS-DISP-BT
           MOVE WS-B-HIT TO WS-DISP-BH
           IF NOT CDD-AGV-QUIET-YES
               DISPLAY "[COV-CHECK] Functions: " WS-DISP-FH "/"
                   WS-DISP-FT " | Lines: " WS-DISP-LH "/" WS-DISP-LT
                   " | Branches: " WS-DISP-BH "/" WS-DISP-BT
               END-DISPLAY
           END-IF
           IF WS-F-TOT = 0
               OR WS-F-HIT < WS-F-TOT
               OR WS-L-HIT < WS-L-TOT
               OR WS-B-HIT < WS-B-TOT
               MOVE 1 TO WS-EXIT-CODE
           END-IF.
