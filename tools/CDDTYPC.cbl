      * @file CDDTYPC.cbl
      * @brief Self-hosted strong-typing and single-error-enum linter.
      * @module CDDTYPC
      * @details Enforces cdd-cobol strong-typing rules across all SDK,
      *          CLI, and tool sources: (1) every 01 copybook envelope
      *          in include/ and src/include/ defines an 8-byte *-TAG
      *          and 88 *-TAG-VALID predicate, (2) zero STOP RUN calls
      *          exist inside src/core..src/sdk, (3) every src/ library
      *          subprogram takes CDD-ERROR as its final USING param,
      *          (4) CALL sites match formal parameter arity, and
      *          (5) raw numeric comparisons on CDD-ERR-CODE are banned.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDTYPC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-LIST-BUF Buffer holding newline-separated file list.
       01  WS-LIST-BUF.
      * @field WS-LBUF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-LBUF-TAG               PIC X(8) VALUE "CDDBUF01".
      * @field WS-LBUF-LEN Active byte length of file list.
           05  WS-LBUF-LEN               PIC 9(8) COMP VALUE 0.
      * @field WS-LBUF-CAP Capacity of file list buffer.
           05  WS-LBUF-CAP               PIC 9(8) COMP VALUE 1048576.
      * @field WS-LBUF-NL Newline kind of file list buffer.
           05  WS-LBUF-NL                PIC 9(2) COMP VALUE 1.
      * @field WS-LBUF-DATA Raw file list text.
           05  WS-LBUF-DATA              PIC X(1048576) VALUE SPACES.
      * @record WS-SYM-TABLE Harvested subprogram parameter signatures.
       01  WS-SYM-TABLE.
      * @field WS-SYM-CNT Number of harvested subprograms (0..128).
           05  WS-SYM-CNT                PIC 9(4) COMP VALUE 0.
      * @field WS-SYM-ENTRY Harvested subprogram signature slot.
           05  WS-SYM-ENTRY              OCCURS 128 TIMES.
      * @field WS-SYM-NAME 8-char PROGRAM-ID of subprogram.
               10  WS-SYM-NAME           PIC X(8).
      * @field WS-SYM-ARITY Formal USING parameter count.
               10  WS-SYM-ARITY          PIC 9(4) COMP.
      * @record WS-TYPE-STATE State and counters for static analysis.
       01  WS-TYPE-STATE.
      * @field WS-ARGC Command-line argument count.
           05  WS-ARGC                   PIC 9(4) COMP VALUE 0.
      * @field WS-ARG-VAL Command-line argument string.
           05  WS-ARG-VAL                PIC X(256) VALUE SPACES.
      * @field WS-EXIT-CODE Exit status (0=pass, 1=violation, 2/3=err).
           05  WS-EXIT-CODE              PIC 9(4) COMP VALUE 0.
      * @field WS-PASS-NO Pass number (1=harvest, 2=verify).
           05  WS-PASS-NO                PIC 9(2) COMP VALUE 1.
      * @field WS-LPOS Scan cursor in WS-LBUF-DATA.
           05  WS-LPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-LSTART Start offset of path in WS-LBUF-DATA.
           05  WS-LSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-CUR-PATH Path of current file being analyzed.
           05  WS-CUR-PATH               PIC X(512) VALUE SPACES.
      * @field WS-CUR-PLEN Length of WS-CUR-PATH.
           05  WS-CUR-PLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-IS-INC-CPY 1 if file is in include/ or src/include/.
           05  WS-IS-INC-CPY             PIC 9(2) COMP VALUE 0.
      * @field WS-IS-LIB-SRC 1 if file is a non-CLI src/ library file.
           05  WS-IS-LIB-SRC             PIC 9(2) COMP VALUE 0.
      * @field WS-SPOS Scan cursor in CDD-BUF-DATA.
           05  WS-SPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-SSTART Start offset of current line.
           05  WS-SSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-SLEN Length of current line.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-LINE-NO 1-based current line number.
           05  WS-LINE-NO                PIC 9(8) COMP VALUE 0.
      * @field WS-LINE-TEXT Current physical line buffer.
           05  WS-LINE-TEXT              PIC X(256) VALUE SPACES.
      * @field WS-CUR-PROG Active PROGRAM-ID in current file.
           05  WS-CUR-PROG               PIC X(8) VALUE SPACES.
      * @field WS-IN-PROC-HDR 1 while scanning PROCEDURE DIVISION hdr.
           05  WS-IN-PROC-HDR            PIC 9(2) COMP VALUE 0.
      * @field WS-IN-USING 1 after USING keyword in PROC DIV header.
           05  WS-IN-USING               PIC 9(2) COMP VALUE 0.
      * @field WS-USING-CNT Formal parameter count in PROC DIV header.
           05  WS-USING-CNT              PIC 9(4) COMP VALUE 0.
      * @field WS-LAST-USING Last formal parameter name in PROC DIV.
           05  WS-LAST-USING             PIC X(64) VALUE SPACES.
      * @field WS-IN-REC-01 1 when checking 01 envelope in copybook.
           05  WS-IN-REC-01              PIC 9(2) COMP VALUE 0.
      * @field WS-REC-HAS-TAG 1 if 01 record declared *-TAG PIC X(8).
           05  WS-REC-HAS-TAG            PIC 9(2) COMP VALUE 0.
      * @field WS-REC-HAS-88 1 if 01 record declared 88 *-TAG-VALID.
           05  WS-REC-HAS-88             PIC 9(2) COMP VALUE 0.
      * @field WS-REC-LINE Line number of active 01 record.
           05  WS-REC-LINE               PIC 9(8) COMP VALUE 0.
      * @field WS-IN-CALL 1 while scanning multi-line CALL statement.
           05  WS-IN-CALL                PIC 9(2) COMP VALUE 0.
      * @field WS-CALL-IN-USING 1 after USING in CALL statement.
           05  WS-CALL-IN-USING          PIC 9(2) COMP VALUE 0.
      * @field WS-CALL-TARGET Target subprogram name in CALL literal.
           05  WS-CALL-TARGET            PIC X(16) VALUE SPACES.
      * @field WS-CALL-ARGC Actual parameter count at CALL site.
           05  WS-CALL-ARGC              PIC 9(4) COMP VALUE 0.
      * @field WS-CALL-LINE Starting line of active CALL statement.
           05  WS-CALL-LINE              PIC 9(8) COMP VALUE 0.
      * @field WS-FILES-CNT Count of verified files in pass 2.
           05  WS-FILES-CNT              PIC 9(6) COMP VALUE 0.
      * @field WS-VIOLATIONS Total strong-typing violations detected.
           05  WS-VIOLATIONS             PIC 9(8) COMP VALUE 0.
      * @record WS-SCAN-WORK Scratch variables for tokenizing code.
       01  WS-SCAN-WORK.
      * @field WS-TI Scan cursor in WS-CODE-AREA.
           05  WS-TI                     PIC 9(4) COMP VALUE 1.
      * @field WS-TLEN Slice length helper.
           05  WS-TLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-TOK-CUR Extracted uppercase token or quoted literal.
           05  WS-TOK-CUR                PIC X(64) VALUE SPACES.
      * @field WS-TOK-K Character write index in WS-TOK-CUR.
           05  WS-TOK-K                  PIC 9(4) COMP VALUE 0.
      * @field WS-TOK1 First token of line.
           05  WS-TOK1                   PIC X(64) VALUE SPACES.
      * @field WS-TOK2 Second token of line.
           05  WS-TOK2                   PIC X(64) VALUE SPACES.
      * @field WS-HAS-DOT 1 if line ends with period.
           05  WS-HAS-DOT                PIC 9(2) COMP VALUE 0.
      * @field WS-SYM-IDX Loop index into WS-SYM-TABLE.
           05  WS-SYM-IDX                PIC 9(4) COMP VALUE 0.
      * @field WS-DIAG-LINE Display line number for error output.
           05  WS-DIAG-LINE              PIC 9(8) VALUE 0.
      * @field WS-DISP-FILES Display file count.
           05  WS-DISP-FILES             PIC 9(6) VALUE 0.
      * @field WS-VIOL-MSG Violation message text.
           05  WS-VIOL-MSG               PIC X(64) VALUE SPACES.
      * @record WS-CODE-AREA Uppercased cols 8..72 code slice.
       01  WS-CODE-AREA                  PIC X(120) VALUE SPACES.
      * @record WS-NL-BYTES Big-endian LF/CR byte helper.
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
      * @subprogram CDDTYPC Enforce strong-typing and error rules.
      * @return Sets exit code 0 when 0 violations; 1 otherwise.
      * @errors Emits violation diagnostics to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run 2-pass static strong-typing verifier.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 0 TO WS-EXIT-CODE
           MOVE 0 TO WS-SYM-CNT
           MOVE 0 TO WS-FILES-CNT
           MOVE 0 TO WS-VIOLATIONS
           PERFORM READ-CLI-ARG
           IF WS-EXIT-CODE = 0
               PERFORM LOAD-LIST-FILE
           END-IF
           IF WS-EXIT-CODE = 0
               MOVE 1 TO WS-PASS-NO
               PERFORM RUN-PASS
               MOVE 2 TO WS-PASS-NO
               PERFORM RUN-PASS
           END-IF
           IF WS-EXIT-CODE = 0 AND WS-VIOLATIONS > 0
               MOVE 1 TO WS-EXIT-CODE
           END-IF
           IF WS-EXIT-CODE = 0 AND NOT CDD-AGV-QUIET-YES
               MOVE WS-FILES-CNT TO WS-DISP-FILES
               DISPLAY "[TYPE-CHECK] Files: " WS-DISP-FILES
                   " Violations: 0 (Strong-typing verified)"
               END-DISPLAY
           END-IF
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE WS-EXIT-CODE TO CDD-AGV-EXIT-CODE
           END-IF
           STOP RUN RETURNING WS-EXIT-CODE.
      * @paragraph READ-CLI-ARG Read file-list path from mem or OS.
       READ-CLI-ARG.
           MOVE SPACES TO WS-ARG-VAL
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE CDD-AGV-COUNT TO WS-ARGC
               IF WS-ARGC >= 1
                   MOVE CDD-AGV-ITEM(1) TO WS-ARG-VAL
               END-IF
           ELSE
               ACCEPT WS-ARGC FROM ARGUMENT-NUMBER
               END-ACCEPT
               IF WS-ARGC >= 1
                   MOVE 1 TO WS-U16
                   DISPLAY WS-U16 UPON ARGUMENT-NUMBER
                   END-DISPLAY
                   ACCEPT WS-ARG-VAL FROM ARGUMENT-VALUE
                   END-ACCEPT
               END-IF
           END-IF
           IF WS-ARGC < 1 OR WS-ARG-VAL = SPACES
               IF NOT CDD-AGV-QUIET-YES
                   DISPLAY "Usage: cdd-type-check <file-list.lst>"
                       UPON SYSERR
                   END-DISPLAY
               END-IF
               MOVE 2 TO WS-EXIT-CODE
           END-IF.
      * @paragraph LOAD-LIST-FILE Load file paths into WS-LIST-BUF.
       LOAD-LIST-FILE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO WS-LBUF-TAG
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 1 TO CDD-PATH-OP
           MOVE 256 TO WS-TI
           PERFORM UNTIL WS-TI = 1
               OR WS-ARG-VAL(WS-TI:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TI
               END-SUBTRACT
           END-PERFORM
           MOVE WS-TI TO CDD-PATH-LEN
           MOVE WS-ARG-VAL TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ WS-LIST-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               IF NOT CDD-AGV-QUIET-YES
                   DISPLAY "cdd-type-check: cannot read list file"
                       UPON SYSERR
                   END-DISPLAY
               END-IF
               MOVE 3 TO WS-EXIT-CODE
           END-IF.
      * @paragraph RUN-PASS Iterate over all files for WS-PASS-NO.
       RUN-PASS.
           MOVE 1 TO WS-LPOS
           MOVE 1 TO WS-LSTART
           PERFORM UNTIL WS-LPOS > WS-LBUF-LEN OR WS-EXIT-CODE NOT = 0
               IF WS-LBUF-DATA(WS-LPOS:1) = WS-LF
                   PERFORM PROCESS-LIST-ENTRY
                   ADD 1 TO WS-LPOS GIVING WS-LSTART
                   END-ADD
               END-IF
               ADD 1 TO WS-LPOS
               END-ADD
           END-PERFORM
           IF WS-LSTART <= WS-LBUF-LEN AND WS-EXIT-CODE = 0
               PERFORM PROCESS-LIST-ENTRY
           END-IF.
      * @paragraph PROCESS-LIST-ENTRY Load and analyze single file.
       PROCESS-LIST-ENTRY.
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
               PERFORM ANALYZE-FILE
           END-IF.
      * @paragraph ANALYZE-FILE Read file bytes and run active pass.
       ANALYZE-FILE.
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
               PERFORM CLASSIFY-PATH
               IF WS-PASS-NO = 2
                   ADD 1 TO WS-FILES-CNT
                   END-ADD
               END-IF
               PERFORM SCAN-SOURCE-LINES
               IF WS-PASS-NO = 2 AND WS-IN-REC-01 = 1
                   PERFORM VERIFY-COPYBOOK-REC-TAG
               END-IF
           END-IF.
      * @paragraph CLASSIFY-PATH Detect copybook vs src/ library path.
       CLASSIFY-PATH.
           MOVE 0 TO WS-IS-INC-CPY
           MOVE 0 TO WS-IS-LIB-SRC
           IF WS-CUR-PLEN >= 12
               IF WS-CUR-PATH(1:8) = "include/"
                   OR WS-CUR-PATH(1:12) = "src/include/"
                   MOVE 1 TO WS-IS-INC-CPY
               END-IF
           END-IF
           IF WS-CUR-PLEN >= 9
               IF WS-CUR-PATH(1:9) = "src/core/"
                   OR WS-CUR-PATH(1:9) = "src/json/"
                   OR WS-CUR-PATH(1:10) = "src/cobol/"
                   OR WS-CUR-PATH(1:7) = "src/ir/"
                   OR WS-CUR-PATH(1:12) = "src/codegen/"
                   OR WS-CUR-PATH(1:8) = "src/sdk/"
                   MOVE 1 TO WS-IS-LIB-SRC
               END-IF
           END-IF
           MOVE SPACES TO WS-CUR-PROG
           MOVE 0 TO WS-IN-PROC-HDR
           MOVE 0 TO WS-IN-USING
           MOVE 0 TO WS-USING-CNT
           MOVE SPACES TO WS-LAST-USING
           MOVE 0 TO WS-IN-REC-01
           MOVE 0 TO WS-REC-HAS-TAG
           MOVE 0 TO WS-REC-HAS-88
           MOVE 0 TO WS-IN-CALL
           MOVE 0 TO WS-LINE-NO.
      * @paragraph SCAN-SOURCE-LINES Iterate lines in CDD-BUF.
       SCAN-SOURCE-LINES.
           MOVE 1 TO WS-SPOS
           MOVE 1 TO WS-SSTART
           PERFORM UNTIL WS-SPOS > CDD-BUF-LEN
               IF CDD-BUF-DATA(WS-SPOS:1) = WS-LF
                   PERFORM INSPECT-LINE
                   ADD 1 TO WS-SPOS GIVING WS-SSTART
                   END-ADD
               END-IF
               ADD 1 TO WS-SPOS
               END-ADD
           END-PERFORM
           IF WS-SSTART <= CDD-BUF-LEN
               PERFORM INSPECT-LINE
           END-IF.
      * @paragraph INSPECT-LINE Skip comments and dispatch pass logic.
       INSPECT-LINE.
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
           IF WS-SLEN >= 8
               AND WS-LINE-TEXT(7:1) NOT = "*"
               AND WS-LINE-TEXT(7:1) NOT = "/"
               AND WS-LINE-TEXT(7:1) NOT = "-"
               PERFORM PREPARE-CODE-SLICE
               IF WS-CODE-AREA NOT = SPACES
                   IF WS-PASS-NO = 1
                       PERFORM PASS1-LINE
                   ELSE
                       PERFORM PASS2-LINE
                   END-IF
               END-IF
           END-IF.
      * @paragraph PREPARE-CODE-SLICE Extract cols 8..72 and 2 tokens.
       PREPARE-CODE-SLICE.
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
           PERFORM NEXT-TOKEN
           MOVE WS-TOK-CUR TO WS-TOK1
           PERFORM NEXT-TOKEN
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
      * @paragraph NEXT-TOKEN Scan next token or quoted string literal.
       NEXT-TOKEN.
           MOVE SPACES TO WS-TOK-CUR
           MOVE 0 TO WS-TOK-K
           PERFORM UNTIL WS-TI > 65
               OR (WS-CODE-AREA(WS-TI:1) NOT = SPACE
               AND WS-CODE-AREA(WS-TI:1) NOT = ".")
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM
           IF WS-TI <= 65 AND WS-CODE-AREA(WS-TI:1) = '"'
               MOVE '"' TO WS-TOK-CUR(1:1)
               MOVE 1 TO WS-TOK-K
               ADD 1 TO WS-TI
               END-ADD
               PERFORM UNTIL WS-TI > 65
                   OR WS-CODE-AREA(WS-TI:1) = '"'
                   IF WS-TOK-K < 63
                       ADD 1 TO WS-TOK-K
                       END-ADD
                       MOVE WS-CODE-AREA(WS-TI:1)
                           TO WS-TOK-CUR(WS-TOK-K:1)
                   END-IF
                   ADD 1 TO WS-TI
                   END-ADD
               END-PERFORM
               IF WS-TI <= 65 AND WS-CODE-AREA(WS-TI:1) = '"'
                   ADD 1 TO WS-TOK-K
                   END-ADD
                   MOVE '"' TO WS-TOK-CUR(WS-TOK-K:1)
                   ADD 1 TO WS-TI
                   END-ADD
               END-IF
           ELSE
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
               END-PERFORM
           END-IF.
      * @paragraph LOG-VIOLATION Emit violation diagnostic to SYSERR.
       LOG-VIOLATION.
           ADD 1 TO WS-VIOLATIONS
           END-ADD
           IF NOT CDD-AGV-QUIET-YES
               DISPLAY WS-CUR-PATH(1:WS-CUR-PLEN) ":" WS-DIAG-LINE
                   ": type-error: " WS-VIOL-MSG UPON SYSERR
               END-DISPLAY
           END-IF.
      * @paragraph PASS1-LINE Harvest PROGRAM-ID and USING arity.
       PASS1-LINE.
           IF WS-TOK1 = "PROGRAM-ID" AND WS-TOK2 NOT = SPACES
               MOVE WS-TOK2(1:8) TO WS-CUR-PROG
           END-IF
           IF WS-TOK1 = "PROCEDURE" AND WS-TOK2 = "DIVISION"
               MOVE 1 TO WS-IN-PROC-HDR
               MOVE 0 TO WS-IN-USING
               MOVE 0 TO WS-USING-CNT
               MOVE SPACES TO WS-LAST-USING
           END-IF
           IF WS-IN-PROC-HDR = 1
               PERFORM HARVEST-PROC-USING
           END-IF.
      * @paragraph HARVEST-PROC-USING Count formal args and store sym.
       HARVEST-PROC-USING.
           MOVE 1 TO WS-TI
           PERFORM UNTIL WS-TI > 65
               PERFORM NEXT-TOKEN
               IF WS-TOK-CUR NOT = SPACES
                   IF WS-TOK-CUR = "USING"
                       MOVE 1 TO WS-IN-USING
                   ELSE
                       IF WS-TOK-CUR = "RETURNING"
                           OR WS-TOK-CUR = "GIVING"
                           MOVE 0 TO WS-IN-USING
                       ELSE
                           IF WS-IN-USING = 1
                               AND WS-TOK-CUR NOT = "BY"
                               AND WS-TOK-CUR NOT = "REFERENCE"
                               AND WS-TOK-CUR NOT = "CONTENT"
                               AND WS-TOK-CUR NOT = "VALUE"
                               ADD 1 TO WS-USING-CNT
                               END-ADD
                               MOVE WS-TOK-CUR TO WS-LAST-USING
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-HAS-DOT = 1
               MOVE 0 TO WS-IN-PROC-HDR
               IF WS-CUR-PROG NOT = SPACES AND WS-SYM-CNT < 128
                   ADD 1 TO WS-SYM-CNT
                   END-ADD
                   MOVE WS-CUR-PROG TO WS-SYM-NAME(WS-SYM-CNT)
                   MOVE WS-USING-CNT TO WS-SYM-ARITY(WS-SYM-CNT)
               END-IF
               IF WS-IS-LIB-SRC = 1
                   AND WS-LAST-USING NOT = "CDD-ERROR"
                   COMPUTE WS-DIAG-LINE = WS-LINE-NO
                   END-COMPUTE
                   MOVE "final USING param must be CDD-ERROR"
                       TO WS-VIOL-MSG
                   PERFORM LOG-VIOLATION
               END-IF
           END-IF.
      * @paragraph PASS2-LINE Verify copybook tags, STOP RUN, CALLs.
       PASS2-LINE.
           IF WS-IS-INC-CPY = 1
               PERFORM CHECK-COPYBOOK-LINE
           END-IF
           IF WS-IS-LIB-SRC = 1
               AND WS-TOK1 = "STOP" AND WS-TOK2 = "RUN"
               COMPUTE WS-DIAG-LINE = WS-LINE-NO
               END-COMPUTE
               MOVE "STOP RUN forbidden in SDK/library modules"
                   TO WS-VIOL-MSG
               PERFORM LOG-VIOLATION
           END-IF
           PERFORM CHECK-RAW-ENUM-COMPARE
           IF WS-IN-CALL = 1
               PERFORM SCAN-CALL-TOKENS
           ELSE
               IF WS-TOK1 = "CALL"
                   MOVE 1 TO WS-IN-CALL
                   MOVE 0 TO WS-CALL-IN-USING
                   MOVE 0 TO WS-CALL-ARGC
                   MOVE SPACES TO WS-CALL-TARGET
                   MOVE WS-LINE-NO TO WS-CALL-LINE
                   PERFORM SCAN-CALL-TOKENS
               END-IF
           END-IF.
      * @paragraph CHECK-COPYBOOK-LINE Verify *-TAG and 88 *-TAG-VALID.
       CHECK-COPYBOOK-LINE.
           IF WS-TOK1 = "01"
               IF WS-IN-REC-01 = 1
                   PERFORM VERIFY-COPYBOOK-REC-TAG
               END-IF
               MOVE 1 TO WS-IN-REC-01
               MOVE 0 TO WS-REC-HAS-TAG
               MOVE 0 TO WS-REC-HAS-88
               MOVE WS-LINE-NO TO WS-REC-LINE
           ELSE
               IF WS-IN-REC-01 = 1 AND WS-TOK1 = "05"
                   PERFORM CHECK-HAS-TAG-SUFFIX
               END-IF
               IF WS-IN-REC-01 = 1 AND WS-TOK1 = "88"
                   PERFORM CHECK-HAS-VALID-SUFFIX
               END-IF
           END-IF.
      * @paragraph CHECK-HAS-TAG-SUFFIX Detect *-TAG field name.
       CHECK-HAS-TAG-SUFFIX.
           MOVE 64 TO WS-TLEN
           PERFORM UNTIL WS-TLEN = 0
               OR WS-TOK2(WS-TLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TLEN
               END-SUBTRACT
           END-PERFORM
           IF WS-TLEN >= 4
               IF WS-TOK2(WS-TLEN - 3:4) = "-TAG"
                   MOVE 1 TO WS-REC-HAS-TAG
               END-IF
           END-IF.
      * @paragraph CHECK-HAS-VALID-SUFFIX Detect *-TAG-VALID 88 item.
       CHECK-HAS-VALID-SUFFIX.
           MOVE 64 TO WS-TLEN
           PERFORM UNTIL WS-TLEN = 0
               OR WS-TOK2(WS-TLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TLEN
               END-SUBTRACT
           END-PERFORM
           IF WS-TLEN >= 10
               IF WS-TOK2(WS-TLEN - 9:10) = "-TAG-VALID"
                   MOVE 1 TO WS-REC-HAS-88
               END-IF
           END-IF.
      * @paragraph VERIFY-COPYBOOK-REC-TAG Report missing tag or 88.
       VERIFY-COPYBOOK-REC-TAG.
           IF WS-REC-HAS-TAG = 0 OR WS-REC-HAS-88 = 0
               COMPUTE WS-DIAG-LINE = WS-REC-LINE
               END-COMPUTE
               MOVE "copybook 01 record missing *-TAG / *-TAG-VALID"
                   TO WS-VIOL-MSG
               PERFORM LOG-VIOLATION
           END-IF.
      * @paragraph CHECK-RAW-ENUM-COMPARE Ban IF/WHEN CDD-ERR-CODE =.
       CHECK-RAW-ENUM-COMPARE.
           IF (WS-TOK1 = "IF" OR WS-TOK1 = "WHEN")
               AND WS-TOK2 = "CDD-ERR-CODE"
               COMPUTE WS-DIAG-LINE = WS-LINE-NO
               END-COMPUTE
               MOVE "use 88 condition names instead of CDD-ERR-CODE ="
                   TO WS-VIOL-MSG
               PERFORM LOG-VIOLATION
           END-IF.
      * @paragraph SCAN-CALL-TOKENS Parse CALL target and USING arity.
       SCAN-CALL-TOKENS.
           MOVE 1 TO WS-TI
           PERFORM UNTIL WS-TI > 65
               PERFORM NEXT-TOKEN
               IF WS-TOK-CUR NOT = SPACES
                   PERFORM PROCESS-CALL-TOK
               END-IF
           END-PERFORM
           IF WS-HAS-DOT = 1 AND WS-IN-CALL = 1
               PERFORM FINISH-CALL-CHECK
           END-IF.
      * @paragraph PROCESS-CALL-TOK Update CALL state machine on token.
       PROCESS-CALL-TOK.
           EVALUATE TRUE
               WHEN WS-TOK-CUR = "CALL"
                   CONTINUE
               WHEN WS-CALL-TARGET = SPACES
                   AND WS-TOK-CUR(1:1) = '"'
                   PERFORM EXTRACT-QUOTED-TARGET
               WHEN WS-TOK-CUR = "USING"
                   MOVE 1 TO WS-CALL-IN-USING
               WHEN WS-TOK-CUR = "GIVING"
                   OR WS-TOK-CUR = "RETURNING"
                   MOVE 0 TO WS-CALL-IN-USING
               WHEN WS-TOK-CUR = "END-CALL"
                   PERFORM FINISH-CALL-CHECK
               WHEN WS-CALL-IN-USING = 1
                   AND WS-TOK-CUR NOT = "BY"
                   AND WS-TOK-CUR NOT = "REFERENCE"
                   AND WS-TOK-CUR NOT = "CONTENT"
                   AND WS-TOK-CUR NOT = "VALUE"
                   ADD 1 TO WS-CALL-ARGC
                   END-ADD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph EXTRACT-QUOTED-TARGET Strip quotes from CALL name.
       EXTRACT-QUOTED-TARGET.
           MOVE 64 TO WS-TLEN
           PERFORM UNTIL WS-TLEN = 0
               OR WS-TOK-CUR(WS-TLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-TLEN
               END-SUBTRACT
           END-PERFORM
           IF WS-TLEN > 2
               SUBTRACT 2 FROM WS-TLEN
               END-SUBTRACT
               IF WS-TLEN > 8
                   MOVE 8 TO WS-TLEN
               END-IF
               MOVE WS-TOK-CUR(2:WS-TLEN)
                   TO WS-CALL-TARGET(1:WS-TLEN)
           END-IF.
      * @paragraph FINISH-CALL-CHECK Validate CALL against WS-SYM-TABLE.
       FINISH-CALL-CHECK.
           MOVE 0 TO WS-IN-CALL
           MOVE 0 TO WS-CALL-IN-USING
           IF WS-CALL-TARGET(1:3) = "CDD"
               MOVE 1 TO WS-SYM-IDX
               PERFORM UNTIL WS-SYM-IDX > WS-SYM-CNT
                   IF WS-SYM-NAME(WS-SYM-IDX) = WS-CALL-TARGET(1:8)
                       IF WS-SYM-ARITY(WS-SYM-IDX) NOT = WS-CALL-ARGC
                           COMPUTE WS-DIAG-LINE = WS-CALL-LINE
                           END-COMPUTE
                           MOVE "CALL parameter count mismatch"
                               TO WS-VIOL-MSG
                           PERFORM LOG-VIOLATION
                       END-IF
                   END-IF
                   ADD 1 TO WS-SYM-IDX
                   END-ADD
               END-PERFORM
           END-IF.
