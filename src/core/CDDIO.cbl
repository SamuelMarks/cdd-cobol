      * @file CDDIO.cbl
      * @brief Raw byte-stream file, atomic temp-swap, and stdio I/O.
      * @module CDDIO
      * @details Preserves exact LF and CRLF byte sequences, trailing
      *          spaces, and non-newline-terminated EOFs using portable
      *          CBL_* byte-stream routines, plus '-' stdin/stdout I/O.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDIO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STDIN-STREAM ASSIGN TO KEYBOARD
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STDIN-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  STDIN-STREAM.
      * @record STDIN-REC Line buffer for '-' standard input reading.
       01  STDIN-REC                     PIC X(4096).
       WORKING-STORAGE SECTION.
      * @record WS-BYTE-CONV Pure 2-byte big-endian byte extractor.
       01  WS-BYTE-CONV.
      * @field WS-U16-VAL Unsigned 16-bit integer (0..9999).
           05  WS-U16-VAL                PIC 9(4) COMP VALUE 0.
      * @field WS-U16-BYTES Two-byte big-endian view of WS-U16-VAL.
           05  WS-U16-BYTES REDEFINES WS-U16-VAL.
      * @field WS-U16-HI High-order byte of WS-U16-VAL.
               10  WS-U16-HI             PIC X(1).
      * @field WS-U16-LO Low-order byte (0..255) of WS-U16-VAL.
               10  WS-U16-LO             PIC X(1).
      * @record WS-FNAME 01-level filename parameter for CBL_* calls.
       01  WS-FNAME                      PIC X(540) VALUE SPACES.
      * @record WS-TMP-FNAME 01-level .cdd.tmp filename for atomic op.
       01  WS-TMP-FNAME                  PIC X(540) VALUE SPACES.
      * @record WS-FHNDL 01-level 4-byte file handle for CBL_* calls.
       01  WS-FHNDL                      PIC X(4) VALUE SPACES.
      * @record WS-ACCESS 01-level access mode byte (1=read, 2=write).
       01  WS-ACCESS                     PIC X(1) VALUE SPACE.
      * @record WS-DENY 01-level deny mode byte (0=none).
       01  WS-DENY                       PIC X(1) VALUE SPACE.
      * @record WS-DEVICE 01-level device flag byte (0=normal).
       01  WS-DEVICE                     PIC X(1) VALUE SPACE.
      * @record WS-OFFSET 01-level 64-bit byte offset for CBL_*.
       01  WS-OFFSET                     PIC X(8) COMP-X VALUE 0.
      * @record WS-NBYTES 01-level 32-bit byte length for CBL_*.
       01  WS-NBYTES                     PIC X(4) COMP-X VALUE 0.
      * @record WS-FLAGS 01-level flags byte (0=io, 128=query size).
       01  WS-FLAGS                      PIC X(1) VALUE SPACE.
      * @record WS-RAW-DATA 01-level raw transfer buffer for CBL_*.
       01  WS-RAW-DATA                   PIC X(1048576) VALUE SPACES.
      * @record WS-IO-WORK Internal state for stdio and NL detection.
       01  WS-IO-WORK.
      * @field WS-RC Signed status code from CBL_* call.
           05  WS-RC                     PIC S9(9) COMP VALUE 0.
      * @field WS-STDIN-ST Two-char COBOL file status for STDIN-STREAM.
           05  WS-STDIN-ST               PIC X(2) VALUE "00".
      * @field WS-EOF-FLAG End-of-file indicator for stdin loop.
           05  WS-EOF-FLAG               PIC 9(2) COMP VALUE 0.
      * @field WS-SCAN-IDX 1-based scan index for newline detection.
           05  WS-SCAN-IDX               PIC 9(8) COMP VALUE 0.
      * @field WS-SLICE-LEN Trimmed length of current stdin line.
           05  WS-SLICE-LEN              PIC 9(8) COMP VALUE 0.
      * @field WS-LF-CH Cached LF byte (X'0A').
           05  WS-LF-CH                  PIC X(1) VALUE SPACE.
      * @field WS-CR-CH Cached CR byte (X'0D').
           05  WS-CR-CH                  PIC X(1) VALUE SPACE.
      * @field WS-ZERO-CH Cached NUL byte (X'00').
           05  WS-ZERO-CH                PIC X(1) VALUE SPACE.
      * @field WS-X80-CH Cached 0x80 byte (128 filesize flag).
           05  WS-X80-CH                 PIC X(1) VALUE SPACE.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDIO Read or write raw buffers from files/stdio.
      * @param [in] CDD-PATH-REQ Path and I/O operation envelope.
      * @param [inout] CDD-BUF Byte buffer input or output envelope.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-BUF on read; written file/stdout on write.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-IO-OPEN-READ,
      *         CDD-ERR-IO-OPEN-WRITE, CDD-ERR-IO-READ-FAILURE,
      *         CDD-ERR-IO-WRITE-FAILURE, CDD-ERR-IO-ATOMIC-RENAME.
       PROCEDURE DIVISION USING CDD-PATH-REQ CDD-BUF CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate envelopes and dispatch I/O op.
       MAIN-ENTRY.
           IF NOT CDD-PATH-TAG-VALID OR NOT CDD-BUF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDIO" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-PATH-REQ or CDD-BUF tag"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           IF CDD-PATH-LEN = 0 OR CDD-PATH-LEN > 512
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9002 TO CDD-ERR-CODE
               MOVE "CDDIO" TO CDD-ERR-SUBPROG
               MOVE "Path length out of range 1..512"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-CONST-BYTES
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           EVALUATE TRUE
               WHEN CDD-PATH-OP-READ
                   PERFORM DO-READ
               WHEN CDD-PATH-OP-WRITE
                   PERFORM DO-WRITE-DIRECT
               WHEN CDD-PATH-OP-WRITE-FD
                   MOVE CDD-PATH-HNDL TO WS-FHNDL
                   PERFORM WRITE-OPEN-HANDLE
               WHEN CDD-PATH-OP-READ-FD
                   MOVE CDD-PATH-HNDL TO WS-FHNDL
                   PERFORM READ-OPEN-HANDLE
               WHEN OTHER
                   PERFORM DO-WRITE-ATOMIC
           END-EVALUATE
           EXIT PROGRAM.
      * @paragraph INIT-CONST-BYTES Populate single-byte control chars.
       INIT-CONST-BYTES.
           MOVE 0 TO WS-U16-VAL
           MOVE WS-U16-LO TO WS-ZERO-CH
           MOVE 10 TO WS-U16-VAL
           MOVE WS-U16-LO TO WS-LF-CH
           MOVE 13 TO WS-U16-VAL
           MOVE WS-U16-LO TO WS-CR-CH
           MOVE 128 TO WS-U16-VAL
           MOVE WS-U16-LO TO WS-X80-CH.
      * @paragraph DO-READ Read from '-' stdin or regular file.
       DO-READ.
           IF CDD-PATH-LEN = 1 AND CDD-PATH-VAL(1:1) = "-"
               PERFORM READ-FROM-STDIN
           ELSE
               PERFORM READ-FROM-FILE
           END-IF
           IF CDD-OK
               PERFORM DETECT-LINE-ENDING
           END-IF.
      * @paragraph READ-FROM-STDIN Read lines from KEYBOARD stream.
       READ-FROM-STDIN.
           MOVE 0 TO CDD-BUF-LEN
           MOVE 0 TO WS-EOF-FLAG
           OPEN INPUT STDIN-STREAM
           PERFORM UNTIL WS-EOF-FLAG = 1 OR NOT CDD-OK
               MOVE SPACES TO STDIN-REC
               READ STDIN-STREAM
                   AT END
                       MOVE 1 TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM APPEND-STDIN-LINE
               END-READ
           END-PERFORM
           CLOSE STDIN-STREAM.
      * @paragraph APPEND-STDIN-LINE Append trimmed stdin line + LF.
       APPEND-STDIN-LINE.
           MOVE 4096 TO WS-SLICE-LEN
           PERFORM UNTIL WS-SLICE-LEN = 0
               OR STDIN-REC(WS-SLICE-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-SLICE-LEN
               END-SUBTRACT
           END-PERFORM
           IF CDD-BUF-LEN + WS-SLICE-LEN + 1 > CDD-BUF-CAP
               MOVE 4001 TO CDD-ERR-CODE
               MOVE "CDDIO" TO CDD-ERR-SUBPROG
               MOVE "Input exceeds CDD-BUF capacity"
                   TO CDD-ERR-MESSAGE
           ELSE
               IF WS-SLICE-LEN > 0
                   MOVE STDIN-REC(1:WS-SLICE-LEN)
                       TO CDD-BUF-DATA(CDD-BUF-LEN + 1:WS-SLICE-LEN)
                   ADD WS-SLICE-LEN TO CDD-BUF-LEN
                   END-ADD
               END-IF
               ADD 1 TO CDD-BUF-LEN
               END-ADD
               MOVE WS-LF-CH TO CDD-BUF-DATA(CDD-BUF-LEN:1)
           END-IF.
      * @paragraph READ-FROM-FILE Read exact raw bytes via CBL_OPEN.
       READ-FROM-FILE.
           MOVE SPACES TO WS-FNAME
           MOVE CDD-PATH-VAL(1:CDD-PATH-LEN)
               TO WS-FNAME(1:CDD-PATH-LEN)
           MOVE 1 TO WS-U16-VAL
           MOVE WS-U16-LO TO WS-ACCESS
           MOVE WS-ZERO-CH TO WS-DENY
           MOVE WS-ZERO-CH TO WS-DEVICE
           CALL "CBL_OPEN_FILE" USING WS-FNAME WS-ACCESS WS-DENY
               WS-DEVICE WS-FHNDL
               GIVING WS-RC
           END-CALL
           IF WS-RC NOT = 0
               MOVE 5006 TO CDD-ERR-CODE
               MOVE "CDDIO" TO CDD-ERR-SUBPROG
               MOVE "Unable to open input file" TO CDD-ERR-MESSAGE
           ELSE
               PERFORM READ-OPEN-HANDLE
               CALL "CBL_CLOSE_FILE" USING WS-FHNDL GIVING WS-RC
               END-CALL
           END-IF.
      * @paragraph READ-OPEN-HANDLE Query size and read raw file bytes.
       READ-OPEN-HANDLE.
           MOVE 0 TO WS-OFFSET
           MOVE 0 TO WS-NBYTES
           MOVE WS-X80-CH TO WS-FLAGS
           CALL "CBL_READ_FILE" USING WS-FHNDL WS-OFFSET
               WS-NBYTES WS-FLAGS WS-RAW-DATA
               GIVING WS-RC
           END-CALL
           IF WS-OFFSET > CDD-BUF-CAP
               MOVE 4001 TO CDD-ERR-CODE
               MOVE "CDDIO" TO CDD-ERR-SUBPROG
               MOVE "File size exceeds CDD-BUF capacity"
                   TO CDD-ERR-MESSAGE
           ELSE
               COMPUTE CDD-BUF-LEN = WS-OFFSET
               END-COMPUTE
               IF CDD-BUF-LEN > 0
                   MOVE SPACES TO WS-RAW-DATA
                   MOVE 0 TO WS-OFFSET
                   COMPUTE WS-NBYTES = CDD-BUF-LEN
                   END-COMPUTE
                   MOVE WS-ZERO-CH TO WS-FLAGS
                   CALL "CBL_READ_FILE" USING WS-FHNDL WS-OFFSET
                       WS-NBYTES WS-FLAGS WS-RAW-DATA
                       GIVING WS-RC
                   END-CALL
                   IF WS-RC NOT = 0
                       MOVE 5008 TO CDD-ERR-CODE
                       MOVE "CDDIO" TO CDD-ERR-SUBPROG
                       MOVE "Byte-stream read failure"
                           TO CDD-ERR-MESSAGE
                   ELSE
                       MOVE WS-RAW-DATA(1:CDD-BUF-LEN)
                           TO CDD-BUF-DATA(1:CDD-BUF-LEN)
                   END-IF
               END-IF
           END-IF.
      * @paragraph DETECT-LINE-ENDING Set LF vs CRLF flag on CDD-BUF.
       DETECT-LINE-ENDING.
           MOVE 1 TO CDD-BUF-NL-KIND
           MOVE 1 TO WS-SCAN-IDX
           PERFORM UNTIL WS-SCAN-IDX >= CDD-BUF-LEN
               OR CDD-BUF-NL-CRLF
               IF CDD-BUF-DATA(WS-SCAN-IDX:1) = WS-CR-CH
                   AND CDD-BUF-DATA(WS-SCAN-IDX + 1:1) = WS-LF-CH
                   MOVE 2 TO CDD-BUF-NL-KIND
               END-IF
               ADD 1 TO WS-SCAN-IDX
               END-ADD
           END-PERFORM.
      * @paragraph DO-WRITE-DIRECT Write buffer to '-' stdout or file.
       DO-WRITE-DIRECT.
           IF CDD-PATH-LEN = 1 AND CDD-PATH-VAL(1:1) = "-"
               IF CDD-BUF-LEN > 0
                   DISPLAY CDD-BUF-DATA(1:CDD-BUF-LEN)
                       WITH NO ADVANCING
                   END-DISPLAY
               END-IF
           ELSE
               MOVE SPACES TO WS-FNAME
               MOVE CDD-PATH-VAL(1:CDD-PATH-LEN)
                   TO WS-FNAME(1:CDD-PATH-LEN)
               PERFORM WRITE-RAW-FNAME
           END-IF.
      * @paragraph WRITE-RAW-FNAME Write exact CDD-BUF-LEN bytes.
       WRITE-RAW-FNAME.
           MOVE 2 TO WS-U16-VAL
           MOVE WS-U16-LO TO WS-ACCESS
           MOVE WS-ZERO-CH TO WS-DENY
           MOVE WS-ZERO-CH TO WS-DEVICE
           CALL "CBL_CREATE_FILE" USING WS-FNAME WS-ACCESS WS-DENY
               WS-DEVICE WS-FHNDL
               GIVING WS-RC
           END-CALL
           IF WS-RC NOT = 0
               MOVE 5007 TO CDD-ERR-CODE
               MOVE "CDDIO" TO CDD-ERR-SUBPROG
               MOVE "Unable to open output file for write"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM WRITE-OPEN-HANDLE
               CALL "CBL_CLOSE_FILE" USING WS-FHNDL GIVING WS-RC
               END-CALL
           END-IF.
      * @paragraph WRITE-OPEN-HANDLE Write CDD-BUF-LEN bytes to WS-FHNDL.
       WRITE-OPEN-HANDLE.
           IF CDD-BUF-LEN > 0
               MOVE CDD-BUF-DATA(1:CDD-BUF-LEN)
                   TO WS-RAW-DATA(1:CDD-BUF-LEN)
               MOVE 0 TO WS-OFFSET
               COMPUTE WS-NBYTES = CDD-BUF-LEN
               END-COMPUTE
               MOVE WS-ZERO-CH TO WS-FLAGS
               CALL "CBL_WRITE_FILE" USING WS-FHNDL WS-OFFSET
                   WS-NBYTES WS-FLAGS WS-RAW-DATA
                   GIVING WS-RC
               END-CALL
               IF WS-RC NOT = 0
                   MOVE 5009 TO CDD-ERR-CODE
                   MOVE "CDDIO" TO CDD-ERR-SUBPROG
                   MOVE "Byte-stream write failure"
                       TO CDD-ERR-MESSAGE
               END-IF
           END-IF.
      * @paragraph DO-WRITE-ATOMIC Write .cdd.tmp then rename over dest.
       DO-WRITE-ATOMIC.
           MOVE SPACES TO WS-TMP-FNAME
           MOVE CDD-PATH-VAL(1:CDD-PATH-LEN)
               TO WS-TMP-FNAME(1:CDD-PATH-LEN)
           MOVE ".cdd.tmp" TO WS-TMP-FNAME(CDD-PATH-LEN + 1:8)
           MOVE WS-TMP-FNAME TO WS-FNAME
           PERFORM WRITE-RAW-FNAME
           IF CDD-OK
               MOVE SPACES TO WS-FNAME
               MOVE CDD-PATH-VAL(1:CDD-PATH-LEN)
                   TO WS-FNAME(1:CDD-PATH-LEN)
               CALL "CBL_RENAME_FILE" USING WS-TMP-FNAME WS-FNAME
                   GIVING WS-RC
               END-CALL
               IF WS-RC NOT = 0
                   CALL "CBL_DELETE_FILE" USING WS-TMP-FNAME
                       GIVING WS-RC
                   END-CALL
                   MOVE 5010 TO CDD-ERR-CODE
                   MOVE "CDDIO" TO CDD-ERR-SUBPROG
                   MOVE "Atomic rename failed" TO CDD-ERR-MESSAGE
               END-IF
           END-IF.
