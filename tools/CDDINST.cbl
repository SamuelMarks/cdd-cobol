      * @file CDDINST.cbl
      * @brief Source-level COBOL-85 function, line, and branch prober.
      * @module CDDINST
      * @details Instruments portable ANSI COBOL-85 source files with
      *          shared EXTERNAL probe counters for every paragraph and
      *          section entry (Function), executable verb (Line), and
      *          conditional arm including synthesized ELSE blocks on
      *          single-armed IF statements (Branch).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDINST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-LIST-BUF Buffer holding newline-separated src paths.
       01  WS-LIST-BUF.
      * @field WS-LBUF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-LBUF-TAG               PIC X(8) VALUE "CDDBUF01".
      * @field WS-LBUF-LEN Active byte length of list buffer.
           05  WS-LBUF-LEN               PIC 9(8) COMP VALUE 0.
      * @field WS-LBUF-CAP Capacity of list buffer.
           05  WS-LBUF-CAP               PIC 9(8) COMP VALUE 1048576.
      * @field WS-LBUF-NL Newline mode.
           05  WS-LBUF-NL                PIC 9(2) COMP VALUE 1.
      * @field WS-LBUF-DATA Raw file paths text.
           05  WS-LBUF-DATA              PIC X(1048576) VALUE SPACES.
      * @record WS-OUT-BUF Output buffer for instrumented .cbl source.
       01  WS-OUT-BUF.
      * @field WS-OBUF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-OBUF-TAG               PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBUF-LEN Active byte length of instrumented source.
           05  WS-OBUF-LEN               PIC 9(8) COMP VALUE 0.
      * @field WS-OBUF-CAP Capacity of instrumented source buffer.
           05  WS-OBUF-CAP               PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBUF-NL Newline mode.
           05  WS-OBUF-NL                PIC 9(2) COMP VALUE 1.
      * @field WS-OBUF-DATA Instrumented COBOL source text.
           05  WS-OBUF-DATA              PIC X(1048576) VALUE SPACES.
      * @record WS-MAN-BUF 1 MB buffer for 48-byte manifest rows.
       01  WS-MAN-BUF.
      * @field WS-MBUF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-MBUF-TAG               PIC X(8) VALUE "CDDBUF01".
      * @field WS-MBUF-LEN Active byte length of manifest buffer.
           05  WS-MBUF-LEN               PIC 9(8) COMP VALUE 0.
      * @field WS-MBUF-CAP Capacity of manifest buffer (21845 rows).
           05  WS-MBUF-CAP               PIC 9(8) COMP VALUE 1048576.
      * @field WS-MBUF-NL Newline mode.
           05  WS-MBUF-NL                PIC 9(2) COMP VALUE 1.
      * @field WS-MBUF-DATA Fixed 48-byte manifest records.
           05  WS-MBUF-DATA              PIC X(1048576) VALUE SPACES.
      * @record WS-IF-STACK Nesting stack for IF/ELSE/END-IF tracking.
       01  WS-IF-STACK.
      * @field WS-IF-DEPTH Active IF nesting depth (0..32).
           05  WS-IF-DEPTH               PIC 9(4) COMP VALUE 0.
      * @field WS-IF-FRAME Stack entry per open IF statement.
           05  WS-IF-FRAME               OCCURS 32 TIMES.
      * @field WS-IF-SAW-ELSE 1 once ELSE seen at this IF level.
               10  WS-IF-SAW-ELSE        PIC 9(2) COMP.
      * @field WS-IF-NEED-TRUE 1 until first THEN statement probed.
               10  WS-IF-NEED-TRUE       PIC 9(2) COMP.
      * @field WS-IF-LINE Source line number of opening IF verb.
               10  WS-IF-LINE            PIC 9(8) COMP.
      * @record WS-INST-STATE CLI arguments and instrumentation state.
       01  WS-INST-STATE.
      * @field WS-ARGC Command-line argument count.
           05  WS-ARGC                   PIC 9(4) COMP VALUE 0.
      * @field WS-ARG-LIST Path to input source list file.
           05  WS-ARG-LIST               PIC X(256) VALUE SPACES.
      * @field WS-ARG-OUTDIR Output directory for instrumented files.
           05  WS-ARG-OUTDIR             PIC X(256) VALUE SPACES.
      * @field WS-ARG-MAN Output path for probe manifest file.
           05  WS-ARG-MAN                PIC X(256) VALUE SPACES.
      * @field WS-OUTDIR-LEN Trimmed length of WS-ARG-OUTDIR.
           05  WS-OUTDIR-LEN             PIC 9(4) COMP VALUE 0.
      * @field WS-EXIT-CODE Process exit status.
           05  WS-EXIT-CODE              PIC 9(4) COMP VALUE 0.
      * @field WS-NEXT-PID Next 1-based probe ID to assign.
           05  WS-NEXT-PID               PIC 9(8) COMP VALUE 0.
      * @field WS-LPOS Cursor in WS-LBUF-DATA.
           05  WS-LPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-LSTART Start offset of path in WS-LBUF-DATA.
           05  WS-LSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-CUR-PATH Current input source path.
           05  WS-CUR-PATH               PIC X(512) VALUE SPACES.
      * @field WS-CUR-PLEN Length of WS-CUR-PATH.
           05  WS-CUR-PLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-BASE-LEN Length of WS-BASENAME.
           05  WS-BASE-LEN               PIC 9(4) COMP VALUE 0.
      * @field WS-SPOS Cursor in CDD-BUF-DATA.
           05  WS-SPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-SSTART Start offset of current source line.
           05  WS-SSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-SLEN Length of current source line.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-LINE-NO 1-based current line number.
           05  WS-LINE-NO                PIC 9(8) COMP VALUE 0.
      * @field WS-IN-PROC-HDR 1 while reading PROC DIV header.
           05  WS-IN-PROC-HDR            PIC 9(2) COMP VALUE 0.
      * @field WS-IN-PROC-BODY 1 inside executable PROCEDURE DIVISION.
           05  WS-IN-PROC-BODY           PIC 9(2) COMP VALUE 0.
      * @field WS-PEND-WHEN 1 when waiting for arm's first verb.
           05  WS-PEND-WHEN              PIC 9(2) COMP VALUE 0.
      * @field WS-PEND-LINE Line number of pending WHEN/AT END arm.
           05  WS-PEND-LINE              PIC 9(8) COMP VALUE 0.
      * @field WS-IS-VERB 1 if WS-TOK1 starts an executable statement.
           05  WS-IS-VERB                PIC 9(2) COMP VALUE 0.
      * @field WS-PROBE-KIND Active probe kind ('F', 'L', or 'B').
           05  WS-PROBE-KIND             PIC X(1) VALUE SPACE.
      * @field WS-PROBE-LINE Line number associated with active probe.
           05  WS-PROBE-LINE             PIC 9(8) COMP VALUE 0.
      * @record WS-BASENAME Extracted filename (e.g. CDDAREN.cbl).
       01  WS-BASENAME                   PIC X(128) VALUE SPACES.
      * @record WS-LINE-TEXT Raw source line buffer.
       01  WS-LINE-TEXT                  PIC X(256) VALUE SPACES.
      * @record WS-CODE-AREA Uppercased cols 8..72 code slice.
       01  WS-CODE-AREA                  PIC X(120) VALUE SPACES.
      * @record WS-EMIT-LINE Scratch line buffer for emitted lines.
       01  WS-EMIT-LINE                  PIC X(120) VALUE SPACES.
      * @record WS-SCAN-VARS Scratch indices and display buffers.
       01  WS-SCAN-VARS.
      * @field WS-TI Token scan index.
           05  WS-TI                     PIC 9(4) COMP VALUE 1.
      * @field WS-TLEN Length scratch variable.
           05  WS-TLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-TOK-CUR Current scanned token.
           05  WS-TOK-CUR                PIC X(64) VALUE SPACES.
      * @field WS-TOK-K Character write index in WS-TOK-CUR.
           05  WS-TOK-K                  PIC 9(4) COMP VALUE 0.
      * @field WS-TOK1 First token on code line.
           05  WS-TOK1                   PIC X(64) VALUE SPACES.
      * @field WS-TOK2 Second token on code line.
           05  WS-TOK2                   PIC X(64) VALUE SPACES.
      * @field WS-HAS-DOT 1 if code line ends with period.
           05  WS-HAS-DOT                PIC 9(2) COMP VALUE 0.
      * @field WS-PID-DISP 5-digit display probe ID.
           05  WS-PID-DISP               PIC 9(5) VALUE 0.
      * @field WS-LNO-DISP 6-digit display line number.
           05  WS-LNO-DISP               PIC 9(6) VALUE 0.
      * @field WS-MAN-ROW 80-byte manifest line builder.
           05  WS-MAN-ROW                PIC X(80) VALUE SPACES.
      * @record WS-NL-BYTES Big-endian LF/CR byte constants.
       01  WS-NL-BYTES.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @field WS-LF LF character (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR CR character (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @subprogram CDDINST Instrument COBOL sources and write manifest.
      * @return Exit status 0 on success; non-zero on usage or I/O err.
      * @errors Reports argument or file I/O errors to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Parse args, instrument files, save table.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 0 TO WS-EXIT-CODE
           MOVE 0 TO WS-NEXT-PID
           MOVE 0 TO WS-MBUF-LEN
           PERFORM READ-ARGS
           IF WS-EXIT-CODE = 0
               PERFORM LOAD-SRC-LIST
           END-IF
           IF WS-EXIT-CODE = 0
               PERFORM PROCESS-ALL-FILES
           END-IF
           IF WS-EXIT-CODE = 0
               PERFORM WRITE-MANIFEST-FILE
           END-IF
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE WS-EXIT-CODE TO CDD-AGV-EXIT-CODE
           END-IF
           STOP RUN RETURNING WS-EXIT-CODE.
      * @paragraph READ-ARGS Read <src-list> <out-dir> <manifest-out>.
       READ-ARGS.
           MOVE SPACES TO WS-ARG-LIST
           MOVE SPACES TO WS-ARG-OUTDIR
           MOVE SPACES TO WS-ARG-MAN
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE CDD-AGV-COUNT TO WS-ARGC
               IF WS-ARGC >= 3
                   MOVE CDD-AGV-ITEM(1) TO WS-ARG-LIST
                   MOVE CDD-AGV-ITEM(2) TO WS-ARG-OUTDIR
                   MOVE CDD-AGV-ITEM(3) TO WS-ARG-MAN
               END-IF
           ELSE
               ACCEPT WS-ARGC FROM ARGUMENT-NUMBER
               END-ACCEPT
               IF WS-ARGC >= 3
                   MOVE 1 TO WS-U16
                   DISPLAY WS-U16 UPON ARGUMENT-NUMBER
                   END-DISPLAY
                   ACCEPT WS-ARG-LIST FROM ARGUMENT-VALUE
                   END-ACCEPT
                   MOVE 2 TO WS-U16
                   DISPLAY WS-U16 UPON ARGUMENT-NUMBER
                   END-DISPLAY
                   ACCEPT WS-ARG-OUTDIR FROM ARGUMENT-VALUE
                   END-ACCEPT
                   MOVE 3 TO WS-U16
                   DISPLAY WS-U16 UPON ARGUMENT-NUMBER
                   END-DISPLAY
                   ACCEPT WS-ARG-MAN FROM ARGUMENT-VALUE
                   END-ACCEPT
               END-IF
           END-IF
           IF WS-ARGC < 3 OR WS-ARG-LIST = SPACES
               MOVE 2 TO WS-EXIT-CODE
           ELSE
               MOVE 256 TO WS-OUTDIR-LEN
               PERFORM UNTIL WS-OUTDIR-LEN = 1
                   OR WS-ARG-OUTDIR(WS-OUTDIR-LEN:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-OUTDIR-LEN
                   END-SUBTRACT
               END-PERFORM
           END-IF.
      * @paragraph LOAD-SRC-LIST Read source path list via CDDIO.
       LOAD-SRC-LIST.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO WS-LBUF-TAG
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 1 TO CDD-PATH-OP
           MOVE 256 TO WS-TI
           PERFORM UNTIL WS-TI = 1
               OR WS-ARG-LIST(WS-TI:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TI
               END-SUBTRACT
           END-PERFORM
           MOVE WS-TI TO CDD-PATH-LEN
           MOVE WS-ARG-LIST TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ WS-LIST-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               MOVE 3 TO WS-EXIT-CODE
           END-IF.
      * @paragraph PROCESS-ALL-FILES Instrument each file in list.
       PROCESS-ALL-FILES.
           MOVE 1 TO WS-LPOS
           MOVE 1 TO WS-LSTART
           PERFORM UNTIL WS-LPOS > WS-LBUF-LEN OR WS-EXIT-CODE NOT = 0
               IF WS-LBUF-DATA(WS-LPOS:1) = WS-LF
                   PERFORM INSTRUMENT-ONE-ENTRY
                   ADD 1 TO WS-LPOS GIVING WS-LSTART
                   END-ADD
               END-IF
               ADD 1 TO WS-LPOS
               END-ADD
           END-PERFORM
           IF WS-LSTART <= WS-LBUF-LEN AND WS-EXIT-CODE = 0
               PERFORM INSTRUMENT-ONE-ENTRY
           END-IF.
      * @paragraph INSTRUMENT-ONE-ENTRY Load, instrument, write file.
       INSTRUMENT-ONE-ENTRY.
           COMPUTE WS-CUR-PLEN = WS-LPOS - WS-LSTART
           END-COMPUTE
           IF WS-CUR-PLEN > 0
               AND WS-LBUF-DATA(WS-LSTART + WS-CUR-PLEN - 1:1) = WS-CR
               SUBTRACT 1 FROM WS-CUR-PLEN
               END-SUBTRACT
           END-IF
           IF WS-CUR-PLEN > 0
               MOVE SPACES TO WS-CUR-PATH
               MOVE WS-LBUF-DATA(WS-LSTART:WS-CUR-PLEN)
                   TO WS-CUR-PATH(1:WS-CUR-PLEN)
               PERFORM EXTRACT-BASENAME
               PERFORM INSTRUMENT-CUR-FILE
           END-IF.
      * @paragraph EXTRACT-BASENAME Find filename after last '/'.
       EXTRACT-BASENAME.
           MOVE WS-CUR-PLEN TO WS-TI
           PERFORM UNTIL WS-TI = 0
               OR WS-CUR-PATH(WS-TI:1) = "/"
               SUBTRACT 1 FROM WS-TI
               END-SUBTRACT
           END-PERFORM
           COMPUTE WS-BASE-LEN = WS-CUR-PLEN - WS-TI
           END-COMPUTE
           MOVE SPACES TO WS-BASENAME
           MOVE WS-CUR-PATH(WS-TI + 1:WS-BASE-LEN)
               TO WS-BASENAME(1:WS-BASE-LEN).
      * @paragraph INSTRUMENT-CUR-FILE Read input and emit instrumented.
       INSTRUMENT-CUR-FILE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 1 TO CDD-PATH-OP
           MOVE WS-CUR-PLEN TO CDD-PATH-LEN
           MOVE WS-CUR-PATH TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               MOVE 3 TO WS-EXIT-CODE
           ELSE
               MOVE 0 TO WS-OBUF-LEN
               MOVE SPACES TO WS-OBUF-DATA
               MOVE 0 TO WS-LINE-NO
               MOVE 0 TO WS-IN-PROC-HDR
               MOVE 0 TO WS-IN-PROC-BODY
               MOVE 0 TO WS-IF-DEPTH
               MOVE 0 TO WS-PEND-WHEN
               PERFORM SCAN-AND-INSTRUMENT
               PERFORM WRITE-INSTRUMENTED-FILE
           END-IF.
      * @paragraph WRITE-INSTRUMENTED-FILE Save <outdir>/<basename>.
       WRITE-INSTRUMENTED-FILE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO WS-OBUF-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE SPACES TO CDD-PATH-VAL
           MOVE WS-ARG-OUTDIR(1:WS-OUTDIR-LEN)
               TO CDD-PATH-VAL(1:WS-OUTDIR-LEN)
           MOVE "/" TO CDD-PATH-VAL(WS-OUTDIR-LEN + 1:1)
           MOVE WS-BASENAME(1:WS-BASE-LEN)
               TO CDD-PATH-VAL(WS-OUTDIR-LEN + 2:WS-BASE-LEN)
           COMPUTE CDD-PATH-LEN = WS-OUTDIR-LEN + 1 + WS-BASE-LEN
           END-COMPUTE
           CALL "CDDIO" USING CDD-PATH-REQ WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               MOVE 3 TO WS-EXIT-CODE
           END-IF.
      * @paragraph WRITE-MANIFEST-FILE Write 1 MB manifest buffer.
       WRITE-MANIFEST-FILE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO WS-MBUF-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE 256 TO WS-TI
           PERFORM UNTIL WS-TI = 1
               OR WS-ARG-MAN(WS-TI:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TI
               END-SUBTRACT
           END-PERFORM
           MOVE WS-TI TO CDD-PATH-LEN
           MOVE WS-ARG-MAN TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ WS-MAN-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               MOVE 3 TO WS-EXIT-CODE
           END-IF.
      * @paragraph SCAN-AND-INSTRUMENT Walk lines of CDD-BUF.
       SCAN-AND-INSTRUMENT.
           MOVE 1 TO WS-SPOS
           MOVE 1 TO WS-SSTART
           PERFORM UNTIL WS-SPOS > CDD-BUF-LEN
               IF CDD-BUF-DATA(WS-SPOS:1) = WS-LF
                   PERFORM INSTRUMENT-LINE
                   ADD 1 TO WS-SPOS GIVING WS-SSTART
                   END-ADD
               END-IF
               ADD 1 TO WS-SPOS
               END-ADD
           END-PERFORM
           IF WS-SSTART <= CDD-BUF-LEN
               PERFORM INSTRUMENT-LINE
           END-IF.
      * @paragraph EMIT-RAW-LINE Append WS-LINE-TEXT(1:WS-SLEN) + LF.
       EMIT-RAW-LINE.
           IF WS-OBUF-LEN + WS-SLEN + 1 <= WS-OBUF-CAP
               IF WS-SLEN > 0
                   MOVE WS-LINE-TEXT(1:WS-SLEN)
                       TO WS-OBUF-DATA(WS-OBUF-LEN + 1:WS-SLEN)
                   ADD WS-SLEN TO WS-OBUF-LEN
                   END-ADD
               END-IF
               ADD 1 TO WS-OBUF-LEN
               END-ADD
               MOVE WS-LF TO WS-OBUF-DATA(WS-OBUF-LEN:1)
           END-IF.
      * @paragraph EMIT-FIXED-STMT Append WS-EMIT-LINE trimmed + LF.
       EMIT-FIXED-STMT.
           MOVE 72 TO WS-TLEN
           PERFORM UNTIL WS-TLEN = 0
               OR WS-EMIT-LINE(WS-TLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TLEN
               END-SUBTRACT
           END-PERFORM
           IF WS-TLEN > 0
               AND WS-OBUF-LEN + WS-TLEN + 1 <= WS-OBUF-CAP
               MOVE WS-EMIT-LINE(1:WS-TLEN)
                   TO WS-OBUF-DATA(WS-OBUF-LEN + 1:WS-TLEN)
               ADD WS-TLEN TO WS-OBUF-LEN
               END-ADD
               ADD 1 TO WS-OBUF-LEN
               END-ADD
               MOVE WS-LF TO WS-OBUF-DATA(WS-OBUF-LEN:1)
           END-IF.
      * @paragraph EMIT-PROBE Register manifest entry and emit ADD 1.
       EMIT-PROBE.
           IF WS-NEXT-PID < 32768
               ADD 1 TO WS-NEXT-PID
               END-ADD
               COMPUTE WS-PID-DISP = WS-NEXT-PID
               END-COMPUTE
               COMPUTE WS-LNO-DISP = WS-PROBE-LINE
               END-COMPUTE
               MOVE SPACES TO WS-MAN-ROW
               MOVE WS-PID-DISP TO WS-MAN-ROW(1:5)
               MOVE WS-PROBE-KIND TO WS-MAN-ROW(7:1)
               MOVE WS-LNO-DISP TO WS-MAN-ROW(9:6)
               IF WS-CUR-PLEN <= 32
                   MOVE WS-CUR-PATH(1:WS-CUR-PLEN)
                       TO WS-MAN-ROW(16:WS-CUR-PLEN)
               ELSE
                   MOVE WS-CUR-PATH(1:32) TO WS-MAN-ROW(16:32)
               END-IF
               MOVE WS-LF TO WS-MAN-ROW(48:1)
               IF WS-MBUF-LEN + 48 <= WS-MBUF-CAP
                   MOVE WS-MAN-ROW(1:48)
                       TO WS-MBUF-DATA(WS-MBUF-LEN + 1:48)
                   ADD 48 TO WS-MBUF-LEN
                   END-ADD
               END-IF
               MOVE SPACES TO WS-EMIT-LINE
               STRING "           ADD 1 TO CDD-COV-HITS("
                   DELIMITED BY SIZE
                   WS-PID-DISP DELIMITED BY SIZE
                   ")" DELIMITED BY SIZE
                   INTO WS-EMIT-LINE
               END-STRING
               PERFORM EMIT-FIXED-STMT
               MOVE SPACES TO WS-EMIT-LINE
               MOVE "           END-ADD" TO WS-EMIT-LINE
               PERFORM EMIT-FIXED-STMT
           END-IF.
      * @paragraph INSTRUMENT-LINE Analyze and instrument single line.
       INSTRUMENT-LINE.
           ADD 1 TO WS-LINE-NO
           END-ADD
           COMPUTE WS-SLEN = WS-SPOS - WS-SSTART
           END-COMPUTE
           IF WS-SLEN > 0
               AND CDD-BUF-DATA(WS-SSTART + WS-SLEN - 1:1) = WS-CR
               SUBTRACT 1 FROM WS-SLEN
               END-SUBTRACT
           END-IF
           IF WS-SLEN > 256
               MOVE 256 TO WS-SLEN
           END-IF
           MOVE SPACES TO WS-LINE-TEXT
           IF WS-SLEN > 0
               MOVE CDD-BUF-DATA(WS-SSTART:WS-SLEN)
                   TO WS-LINE-TEXT(1:WS-SLEN)
           END-IF
           IF WS-SLEN < 8
               OR WS-LINE-TEXT(7:1) = "*"
               OR WS-LINE-TEXT(7:1) = "/"
               OR WS-LINE-TEXT(7:1) = "-"
               PERFORM EMIT-RAW-LINE
           ELSE
               PERFORM PARSE-LINE-TOKENS
               IF WS-CODE-AREA = SPACES
                   PERFORM EMIT-RAW-LINE
               ELSE
                   PERFORM HANDLE-CODE-LINE
               END-IF
           END-IF.
      * @paragraph PARSE-LINE-TOKENS Extract uppercased tokens 1 and 2.
       PARSE-LINE-TOKENS.
           MOVE SPACES TO WS-CODE-AREA
           IF WS-SLEN >= 72
               MOVE WS-LINE-TEXT(8:65) TO WS-CODE-AREA(1:65)
           ELSE
               COMPUTE WS-TLEN = WS-SLEN - 7
               END-COMPUTE
               MOVE WS-LINE-TEXT(8:WS-TLEN) TO WS-CODE-AREA(1:WS-TLEN)
           END-IF
           INSPECT WS-CODE-AREA
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           MOVE SPACES TO WS-TOK1
           MOVE SPACES TO WS-TOK2
           MOVE 0 TO WS-HAS-DOT
           MOVE 1 TO WS-TI
           PERFORM SCAN-WORD
           MOVE WS-TOK-CUR TO WS-TOK1
           PERFORM SCAN-WORD
           MOVE WS-TOK-CUR TO WS-TOK2
           MOVE 65 TO WS-TLEN
           PERFORM UNTIL WS-TLEN = 0
               OR WS-CODE-AREA(WS-TLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TLEN
               END-SUBTRACT
           END-PERFORM
           IF WS-TLEN > 0 AND WS-CODE-AREA(WS-TLEN:1) = "."
               MOVE 1 TO WS-HAS-DOT
           END-IF.
      * @paragraph SCAN-WORD Read next space/period-delimited word.
       SCAN-WORD.
           MOVE SPACES TO WS-TOK-CUR
           MOVE 0 TO WS-TOK-K
           PERFORM UNTIL WS-TI > 65
               OR (WS-CODE-AREA(WS-TI:1) NOT = SPACE
               AND WS-CODE-AREA(WS-TI:1) NOT = ".")
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM
           PERFORM UNTIL WS-TI > 65
               OR WS-CODE-AREA(WS-TI:1) = SPACE
               OR WS-CODE-AREA(WS-TI:1) = "."
               IF WS-TOK-K < 64
                   ADD 1 TO WS-TOK-K
                   END-ADD
                   MOVE WS-CODE-AREA(WS-TI:1)
                       TO WS-TOK-CUR(WS-TOK-K:1)
               END-IF
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM.
      * @paragraph HANDLE-CODE-LINE Dispatch WS, PROC hdr, or PROC body.
       HANDLE-CODE-LINE.
           IF WS-IN-PROC-BODY = 0
               PERFORM HANDLE-PRE-PROC-LINE
           ELSE
               PERFORM HANDLE-PROC-BODY-LINE
           END-IF.
      * @paragraph HANDLE-PRE-PROC-LINE Inject EXTERNAL table in WS.
       HANDLE-PRE-PROC-LINE.
           PERFORM EMIT-RAW-LINE
           IF WS-TOK1 = "WORKING-STORAGE" AND WS-TOK2 = "SECTION"
               MOVE SPACES TO WS-EMIT-LINE
               MOVE "       01  CDD-COV-EXT IS EXTERNAL."
                   TO WS-EMIT-LINE
               PERFORM EMIT-FIXED-STMT
               MOVE SPACES TO WS-EMIT-LINE
               MOVE "           05  CDD-COV-HITS PIC 9(8) COMP"
                   TO WS-EMIT-LINE
               PERFORM EMIT-FIXED-STMT
               MOVE SPACES TO WS-EMIT-LINE
               MOVE "               OCCURS 32768 TIMES."
                   TO WS-EMIT-LINE
               PERFORM EMIT-FIXED-STMT
           END-IF
           IF WS-TOK1 = "PROCEDURE" AND WS-TOK2 = "DIVISION"
               IF WS-HAS-DOT = 1
                   MOVE 1 TO WS-IN-PROC-BODY
               ELSE
                   MOVE 1 TO WS-IN-PROC-HDR
               END-IF
           ELSE
               IF WS-IN-PROC-HDR = 1 AND WS-HAS-DOT = 1
                   MOVE 0 TO WS-IN-PROC-HDR
                   MOVE 1 TO WS-IN-PROC-BODY
               END-IF
           END-IF.
      * @paragraph HANDLE-PROC-BODY-LINE Instrument paragraphs & stmts.
       HANDLE-PROC-BODY-LINE.
           IF WS-CODE-AREA(1:4) NOT = SPACES
               AND (WS-TOK2 = "SECTION"
               OR (WS-TOK2 = SPACES AND WS-HAS-DOT = 1))
               PERFORM EMIT-RAW-LINE
               MOVE "F" TO WS-PROBE-KIND
               MOVE WS-LINE-NO TO WS-PROBE-LINE
               PERFORM EMIT-PROBE
           ELSE
               PERFORM HANDLE-PROC-STMT-LINE
           END-IF.
      * @paragraph HANDLE-PROC-STMT-LINE Instrument branches and verbs.
       HANDLE-PROC-STMT-LINE.
           EVALUATE TRUE
               WHEN WS-TOK1 = "END-IF"
                   PERFORM HANDLE-END-IF
               WHEN WS-TOK1 = "ELSE"
                   PERFORM HANDLE-ELSE
               WHEN WS-TOK1 = "WHEN"
                   MOVE 1 TO WS-PEND-WHEN
                   MOVE WS-LINE-NO TO WS-PEND-LINE
                   PERFORM EMIT-RAW-LINE
               WHEN (WS-TOK1 = "AT" AND WS-TOK2 = "END")
                   OR (WS-TOK1 = "NOT" AND WS-TOK2 = "AT")
                   OR (WS-TOK1 = "ON" AND WS-TOK2 = "SIZE")
                   OR (WS-TOK1 = "NOT" AND WS-TOK2 = "ON")
                   MOVE 1 TO WS-PEND-WHEN
                   MOVE WS-LINE-NO TO WS-PEND-LINE
                   PERFORM EMIT-RAW-LINE
               WHEN OTHER
                   PERFORM CHECK-EXECUTABLE-VERB
                   IF WS-IS-VERB = 1
                       PERFORM INSTRUMENT-VERB-LINE
                   ELSE
                       PERFORM EMIT-RAW-LINE
                   END-IF
           END-EVALUATE.
      * @paragraph HANDLE-ELSE Emit ELSE and immediate FALSE probe.
       HANDLE-ELSE.
           IF WS-IF-DEPTH > 0
               MOVE 1 TO WS-IF-SAW-ELSE(WS-IF-DEPTH)
           END-IF
           PERFORM EMIT-RAW-LINE
           MOVE "B" TO WS-PROBE-KIND
           MOVE WS-LINE-NO TO WS-PROBE-LINE
           PERFORM EMIT-PROBE.
      * @paragraph HANDLE-END-IF Inject synthetic ELSE if missing.
       HANDLE-END-IF.
           IF WS-IF-DEPTH > 0
               IF WS-IF-SAW-ELSE(WS-IF-DEPTH) = 0
                   MOVE SPACES TO WS-EMIT-LINE
                   MOVE "           ELSE" TO WS-EMIT-LINE
                   PERFORM EMIT-FIXED-STMT
                   MOVE "B" TO WS-PROBE-KIND
                   MOVE WS-IF-LINE(WS-IF-DEPTH) TO WS-PROBE-LINE
                   PERFORM EMIT-PROBE
               END-IF
               SUBTRACT 1 FROM WS-IF-DEPTH
               END-SUBTRACT
           END-IF
           PERFORM EMIT-RAW-LINE.
      * @paragraph CHECK-EXECUTABLE-VERB Test if WS-TOK1 is a verb.
       CHECK-EXECUTABLE-VERB.
           MOVE 0 TO WS-IS-VERB
           EVALUATE WS-TOK1
               WHEN "ACCEPT"
               WHEN "ADD"
               WHEN "CALL"
               WHEN "CLOSE"
               WHEN "COMPUTE"
               WHEN "CONTINUE"
               WHEN "DISPLAY"
               WHEN "DIVIDE"
               WHEN "EVALUATE"
               WHEN "EXIT"
               WHEN "IF"
               WHEN "INITIALIZE"
               WHEN "INSPECT"
               WHEN "MOVE"
               WHEN "MULTIPLY"
               WHEN "OPEN"
               WHEN "PERFORM"
               WHEN "READ"
               WHEN "SEARCH"
               WHEN "SET"
               WHEN "STOP"
               WHEN "STRING"
               WHEN "SUBTRACT"
               WHEN "UNSTRING"
               WHEN "WRITE"
                   MOVE 1 TO WS-IS-VERB
               WHEN OTHER
                   MOVE 0 TO WS-IS-VERB
           END-EVALUATE.
      * @paragraph INSTRUMENT-VERB-LINE Emit pending branch + line hit.
       INSTRUMENT-VERB-LINE.
           IF WS-IF-DEPTH > 0
               IF WS-IF-NEED-TRUE(WS-IF-DEPTH) = 1
                   MOVE 0 TO WS-IF-NEED-TRUE(WS-IF-DEPTH)
                   MOVE "B" TO WS-PROBE-KIND
                   MOVE WS-IF-LINE(WS-IF-DEPTH) TO WS-PROBE-LINE
                   PERFORM EMIT-PROBE
               END-IF
           END-IF
           IF WS-PEND-WHEN = 1
               MOVE 0 TO WS-PEND-WHEN
               MOVE "B" TO WS-PROBE-KIND
               MOVE WS-PEND-LINE TO WS-PROBE-LINE
               PERFORM EMIT-PROBE
           END-IF
           MOVE "L" TO WS-PROBE-KIND
           MOVE WS-LINE-NO TO WS-PROBE-LINE
           PERFORM EMIT-PROBE
           IF WS-TOK1 = "IF" AND WS-IF-DEPTH < 32
               ADD 1 TO WS-IF-DEPTH
               END-ADD
               MOVE 0 TO WS-IF-SAW-ELSE(WS-IF-DEPTH)
               MOVE 1 TO WS-IF-NEED-TRUE(WS-IF-DEPTH)
               MOVE WS-LINE-NO TO WS-IF-LINE(WS-IF-DEPTH)
           END-IF
           PERFORM EMIT-RAW-LINE.
