      * @file CDDCOVR.cbl
      * @brief Runtime coverage probe hit accumulator and file merger.
      * @module CDDCOVR
      * @details Reads the shared ANSI COBOL-85 EXTERNAL probe counter
      *          array (CDD-COV-EXT), merges prior hit counts from
      *          build/cov/hits.dat if present, and persists updated
      *          cumulative probe counts deterministically.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCOVR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @record CDD-COV-EXT Shared probe hit table across modules.
       01  CDD-COV-EXT IS EXTERNAL.
      * @field CDD-COV-HITS Hit counter per probe ID (1..32768).
           05  CDD-COV-HITS              PIC 9(8) COMP
                                         OCCURS 32768 TIMES.
      * @record WS-COV-IO Internal merge workspace for hits.dat.
       01  WS-COV-IO.
      * @field WS-IDX Probe index loop counter (1..32768).
           05  WS-IDX                    PIC 9(8) COMP VALUE 0.
      * @field WS-MAX-HIT Highest probe index with non-zero count.
           05  WS-MAX-HIT                PIC 9(8) COMP VALUE 0.
      * @field WS-LINE-POS Cursor in CDD-BUF-DATA while parsing/writing.
           05  WS-LINE-POS               PIC 9(8) COMP VALUE 1.
      * @field WS-PID-DISP Fixed 5-digit probe ID text field.
           05  WS-PID-DISP               PIC 9(5) VALUE 0.
      * @field WS-CNT-DISP Fixed 8-digit hit count text field.
           05  WS-CNT-DISP               PIC 9(8) VALUE 0.
      * @field WS-PARSED-PID Parsed probe ID from existing hits.dat.
           05  WS-PARSED-PID             PIC 9(5) COMP VALUE 0.
      * @field WS-PARSED-CNT Parsed hit count from existing hits.dat.
           05  WS-PARSED-CNT             PIC 9(8) COMP VALUE 0.
      * @record WS-BYTE-CONV Pure 2-byte big-endian LF byte helper.
       01  WS-BYTE-CONV.
      * @field WS-U16-VAL Unsigned 16-bit integer.
           05  WS-U16-VAL                PIC 9(4) COMP VALUE 10.
      * @field WS-U16-BYTES Byte overlay over WS-U16-VAL.
           05  WS-U16-BYTES REDEFINES WS-U16-VAL.
      * @field WS-HI High byte.
               10  WS-HI                 PIC X(1).
      * @field WS-LF Low byte initialized to LF (10).
               10  WS-LF                 PIC X(1).
      * @subprogram CDDCOVR Merge and persist external coverage hits.
      * @return Updated build/cov/hits.dat file on disk.
      * @errors Propagates internal I/O status silently if directory
      *         is absent outside coverage runs.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Merge existing hits and write updated log.
       MAIN-ENTRY.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE "build/cov/hits.dat" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE 1 TO CDD-PATH-OP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF CDD-OK AND CDD-BUF-LEN > 0
               PERFORM MERGE-EXISTING-HITS
           END-IF
           PERFORM FORMAT-HITS-BUFFER
           MOVE 2 TO CDD-PATH-OP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           EXIT PROGRAM.
      * @paragraph MERGE-EXISTING-HITS Parse 15-byte hit records.
       MERGE-EXISTING-HITS.
           MOVE 1 TO WS-LINE-POS
           PERFORM UNTIL WS-LINE-POS + 14 > CDD-BUF-LEN
               MOVE CDD-BUF-DATA(WS-LINE-POS:5) TO WS-PID-DISP
               MOVE WS-PID-DISP TO WS-PARSED-PID
               MOVE CDD-BUF-DATA(WS-LINE-POS + 6:8) TO WS-CNT-DISP
               MOVE WS-CNT-DISP TO WS-PARSED-CNT
               IF WS-PARSED-PID >= 1 AND WS-PARSED-PID <= 32768
                   ADD WS-PARSED-CNT TO CDD-COV-HITS(WS-PARSED-PID)
                   END-ADD
               END-IF
               ADD 15 TO WS-LINE-POS
               END-ADD
           END-PERFORM.
      * @paragraph FORMAT-HITS-BUFFER Serialize non-zero counters.
       FORMAT-HITS-BUFFER.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 0 TO CDD-BUF-LEN
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > 32768
               IF CDD-COV-HITS(WS-IDX) > 0
                   AND CDD-BUF-LEN + 15 <= CDD-BUF-CAP
                   COMPUTE WS-PID-DISP = WS-IDX
                   END-COMPUTE
                   MOVE CDD-COV-HITS(WS-IDX) TO WS-CNT-DISP
                   MOVE WS-PID-DISP
                       TO CDD-BUF-DATA(CDD-BUF-LEN + 1:5)
                   MOVE SPACE TO CDD-BUF-DATA(CDD-BUF-LEN + 6:1)
                   MOVE WS-CNT-DISP
                       TO CDD-BUF-DATA(CDD-BUF-LEN + 7:8)
                   MOVE WS-LF TO CDD-BUF-DATA(CDD-BUF-LEN + 15:1)
                   ADD 15 TO CDD-BUF-LEN
                   END-ADD
               END-IF
               ADD 1 TO WS-IDX
               END-ADD
           END-PERFORM.
