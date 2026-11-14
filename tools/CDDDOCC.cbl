      * @file CDDDOCC.cbl
      * @brief Self-hosted 100% documentation coverage gate enforcer.
      * @module CDDDOCC
      * @details Verifies 100% structured docblock coverage across all
      *          files (@file/@brief), modules (@module/@details),
      *          records/classes (@record/@typedef/@class), fields
      *          (@field), subprograms/sections/paragraphs, and formal
      *          USING arguments (@param/@return/@errors).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDDOCC.
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
      * @record WS-DOC-STATE Per-file and global doc checker counters.
       01  WS-DOC-STATE.
      * @field WS-ARGC Command-line argument count.
           05  WS-ARGC                   PIC 9(4) COMP VALUE 0.
      * @field WS-ARG-VAL Command-line argument string buffer.
           05  WS-ARG-VAL                PIC X(256) VALUE SPACES.
      * @field WS-EXIT-CODE Process exit status (0=pass, 1=fail).
           05  WS-EXIT-CODE              PIC 9(4) COMP VALUE 0.
      * @field WS-LPOS Scan cursor in WS-LBUF-DATA.
           05  WS-LPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-LSTART Start offset of current path in list.
           05  WS-LSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-CUR-PATH Path of file currently being checked.
           05  WS-CUR-PATH               PIC X(512) VALUE SPACES.
      * @field WS-CUR-PLEN Length of WS-CUR-PATH.
           05  WS-CUR-PLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-IS-CPY 1 if file ends in .cpy, 0 if .cbl.
           05  WS-IS-CPY                 PIC 9(2) COMP VALUE 0.
      * @field WS-SPOS Scan cursor in CDD-BUF-DATA.
           05  WS-SPOS                   PIC 9(8) COMP VALUE 1.
      * @field WS-SSTART Start offset of current source line.
           05  WS-SSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-SLEN Length of current source line.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-LINE-NO 1-based current source line number.
           05  WS-LINE-NO                PIC 9(8) COMP VALUE 0.
      * @field WS-LINE-TEXT Current source line padded to 256 chars.
           05  WS-LINE-TEXT              PIC X(256) VALUE SPACES.
      * @field WS-DIV-MODE Active COBOL division (1=ID,2=ENV,3=DAT,4=PR).
           05  WS-DIV-MODE               PIC 9(2) COMP VALUE 1.
      * @field WS-IN-PROC-HDR 1 while reading multi-line PROC DIV hdr.
           05  WS-IN-PROC-HDR            PIC 9(2) COMP VALUE 0.
      * @field WS-IN-USING 1 after USING keyword in PROC DIV header.
           05  WS-IN-USING               PIC 9(2) COMP VALUE 0.
      * @field WS-USING-CNT Count of formal USING arguments in header.
           05  WS-USING-CNT              PIC 9(4) COMP VALUE 0.
      * @field WS-FIRST-CODE-SEEN 1 once first code line checked.
           05  WS-FIRST-CODE-SEEN        PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-FILE 1 when @file seen in file header.
           05  WS-SAW-FILE               PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-BRIEF 1 when @brief seen in file header.
           05  WS-SAW-BRIEF              PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-MOD 1 when @module/@program/@class seen.
           05  WS-SAW-MOD                PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-DETAILS 1 when @details seen.
           05  WS-SAW-DETAILS            PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-REC 1 when @record/@typedef/@class seen.
           05  WS-SAW-REC                PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-FIELD 1 when @field seen.
           05  WS-SAW-FIELD              PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-SUBP 1 when @subprogram seen.
           05  WS-SAW-SUBP               PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-RET 1 when @return seen.
           05  WS-SAW-RET                PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-ERRS 1 when @errors seen.
           05  WS-SAW-ERRS               PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-SEC 1 when @section seen.
           05  WS-SAW-SEC                PIC 9(2) COMP VALUE 0.
      * @field WS-SAW-PARA 1 when @paragraph seen.
           05  WS-SAW-PARA               PIC 9(2) COMP VALUE 0.
      * @field WS-PARAM-CNT Number of @param tags in current docblock.
           05  WS-PARAM-CNT              PIC 9(4) COMP VALUE 0.
      * @field WS-FILES-CNT Total files checked.
           05  WS-FILES-CNT              PIC 9(6) COMP VALUE 0.
      * @field WS-ITEMS-TOT Total documentable constructs inspected.
           05  WS-ITEMS-TOT              PIC 9(8) COMP VALUE 0.
      * @field WS-VIOLATIONS Total missing documentation items found.
           05  WS-VIOLATIONS             PIC 9(8) COMP VALUE 0.
      * @record WS-TOK-WORK Tokenizer scratch fields for line scanning.
       01  WS-TOK-WORK.
      * @field WS-TI Scan cursor in WS-LINE-TEXT or WS-CODE-AREA.
           05  WS-TI                     PIC 9(4) COMP VALUE 1.
      * @field WS-TLEN End index for token scanning.
           05  WS-TLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-TOK1 First uppercase token on code line.
           05  WS-TOK1                   PIC X(64) VALUE SPACES.
      * @field WS-TOK2 Second uppercase token on code line.
           05  WS-TOK2                   PIC X(64) VALUE SPACES.
      * @field WS-TOK-CUR Current token during USING scan.
           05  WS-TOK-CUR                PIC X(64) VALUE SPACES.
      * @field WS-TOK-K Write cursor in current token buffer.
           05  WS-TOK-K                  PIC 9(4) COMP VALUE 0.
      * @field WS-HAS-DOT 1 if current line ends statement with period.
           05  WS-HAS-DOT                PIC 9(2) COMP VALUE 0.
      * @field WS-LVL-NUM Numeric 2-digit level value.
           05  WS-LVL-NUM                PIC 9(2) COMP VALUE 0.
      * @field WS-IS-COMMENT 1 if current physical line is a comment.
           05  WS-IS-COMMENT             PIC 9(2) COMP VALUE 0.
      * @field WS-DIAG-LINE Display line number for violation msg.
           05  WS-DIAG-LINE              PIC 9(8) VALUE 0.
      * @field WS-DISP-FILES Display counter for checked files.
           05  WS-DISP-FILES             PIC 9(6) VALUE 0.
      * @field WS-DISP-ITEMS Display counter for checked items.
           05  WS-DISP-ITEMS             PIC 9(8) VALUE 0.
      * @field WS-VIOL-MSG Violation description string.
           05  WS-VIOL-MSG               PIC X(64) VALUE SPACES.
      * @record WS-CODE-AREA Code portion of current line (cols 8..72).
       01  WS-CODE-AREA                  PIC X(120) VALUE SPACES.
      * @record WS-NL-BYTES Big-endian LF and CR byte constants.
       01  WS-NL-BYTES.
      * @field WS-U16 Unsigned 16-bit scratch integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Two-byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @field WS-LF LF character (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR CR character (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @subprogram CDDDOCC Verify 100% doc coverage over file list.
      * @return Sets exit code 0 when 100% documented; 1 otherwise.
      * @errors Emits diagnostics to SYSERR on missing tags or I/O err.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Parse file-list arg and run doc checks.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 0 TO WS-EXIT-CODE
           MOVE 0 TO WS-FILES-CNT
           MOVE 0 TO WS-ITEMS-TOT
           MOVE 0 TO WS-VIOLATIONS
           PERFORM READ-CLI-ARG
           IF WS-EXIT-CODE = 0
               PERFORM LOAD-AND-SCAN-LIST
           END-IF
           IF WS-EXIT-CODE = 0 AND WS-VIOLATIONS > 0
               MOVE 1 TO WS-EXIT-CODE
           END-IF
           IF WS-EXIT-CODE = 0 AND NOT CDD-AGV-QUIET-YES
               MOVE WS-FILES-CNT TO WS-DISP-FILES
               MOVE WS-ITEMS-TOT TO WS-DISP-ITEMS
               DISPLAY "[DOC-CHECK] Files: " WS-DISP-FILES
                   " Items: " WS-DISP-ITEMS " (100.00% doc coverage)"
               END-DISPLAY
           END-IF
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE WS-EXIT-CODE TO CDD-AGV-EXIT-CODE
           END-IF
           STOP RUN RETURNING WS-EXIT-CODE.
      * @paragraph READ-CLI-ARG Fetch manifest path from mem or OS.
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
                   DISPLAY "Usage: cdd-doc-check <file-list.lst>"
                       UPON SYSERR
                   END-DISPLAY
               END-IF
               MOVE 2 TO WS-EXIT-CODE
           END-IF.
      * @paragraph LOAD-AND-SCAN-LIST Read file list and check files.
       LOAD-AND-SCAN-LIST.
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
                   DISPLAY "cdd-doc-check: cannot read list file"
                       UPON SYSERR
                   END-DISPLAY
               END-IF
               MOVE 3 TO WS-EXIT-CODE
           ELSE
               PERFORM ITERATE-FILE-LIST
           END-IF.
      * @paragraph ITERATE-FILE-LIST Loop over lines in WS-LIST-BUF.
       ITERATE-FILE-LIST.
           MOVE 1 TO WS-LPOS
           MOVE 1 TO WS-LSTART
           PERFORM UNTIL WS-LPOS > WS-LBUF-LEN OR WS-EXIT-CODE NOT = 0
               IF WS-LBUF-DATA(WS-LPOS:1) = WS-LF
                   PERFORM EXTRACT-AND-CHECK-PATH
                   ADD 1 TO WS-LPOS GIVING WS-LSTART
                   END-ADD
               END-IF
               ADD 1 TO WS-LPOS
               END-ADD
           END-PERFORM
           IF WS-LSTART <= WS-LBUF-LEN AND WS-EXIT-CODE = 0
               PERFORM EXTRACT-AND-CHECK-PATH
           END-IF.
      * @paragraph EXTRACT-AND-CHECK-PATH Trim single path and check.
       EXTRACT-AND-CHECK-PATH.
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
               PERFORM CHECK-SINGLE-FILE
           END-IF.
      * @paragraph CHECK-SINGLE-FILE Load COBOL/copybook file and scan.
       CHECK-SINGLE-FILE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 1 TO CDD-PATH-OP
           MOVE WS-CUR-PLEN TO CDD-PATH-LEN
           MOVE WS-CUR-PATH TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               IF NOT CDD-AGV-QUIET-YES
                   DISPLAY "cdd-doc-check: I/O error on "
                       WS-CUR-PATH(1:WS-CUR-PLEN) UPON SYSERR
                   END-DISPLAY
               END-IF
               MOVE 3 TO WS-EXIT-CODE
           ELSE
               ADD 1 TO WS-FILES-CNT
               END-ADD
               PERFORM INIT-FILE-DOC-STATE
               PERFORM SCAN-FILE-LINES
           END-IF.
      * @paragraph INIT-FILE-DOC-STATE Reset per-file parser state.
       INIT-FILE-DOC-STATE.
           MOVE 0 TO WS-IS-CPY
           IF WS-CUR-PLEN >= 4
               IF WS-CUR-PATH(WS-CUR-PLEN - 3:4) = ".cpy"
                   OR WS-CUR-PATH(WS-CUR-PLEN - 3:4) = ".CPY"
                   MOVE 1 TO WS-IS-CPY
               END-IF
           END-IF
           IF WS-IS-CPY = 1
               MOVE 3 TO WS-DIV-MODE
           ELSE
               MOVE 1 TO WS-DIV-MODE
           END-IF
           MOVE 0 TO WS-IN-PROC-HDR
           MOVE 0 TO WS-IN-USING
           MOVE 0 TO WS-USING-CNT
           MOVE 0 TO WS-FIRST-CODE-SEEN
           MOVE 0 TO WS-SAW-FILE
           MOVE 0 TO WS-SAW-BRIEF
           MOVE 0 TO WS-SAW-MOD
           MOVE 0 TO WS-SAW-DETAILS
           MOVE 0 TO WS-SAW-REC
           MOVE 0 TO WS-SAW-FIELD
           MOVE 0 TO WS-SAW-SUBP
           MOVE 0 TO WS-SAW-RET
           MOVE 0 TO WS-SAW-ERRS
           MOVE 0 TO WS-SAW-SEC
           MOVE 0 TO WS-SAW-PARA
           MOVE 0 TO WS-PARAM-CNT
           MOVE 0 TO WS-LINE-NO.
      * @paragraph SCAN-FILE-LINES Slice CDD-BUF into lines and check.
       SCAN-FILE-LINES.
           MOVE 1 TO WS-SPOS
           MOVE 1 TO WS-SSTART
           PERFORM UNTIL WS-SPOS > CDD-BUF-LEN
               IF CDD-BUF-DATA(WS-SPOS:1) = WS-LF
                   PERFORM PROCESS-SOURCE-LINE
                   ADD 1 TO WS-SPOS GIVING WS-SSTART
                   END-ADD
               END-IF
               ADD 1 TO WS-SPOS
               END-ADD
           END-PERFORM
           IF WS-SSTART <= CDD-BUF-LEN
               PERFORM PROCESS-SOURCE-LINE
           END-IF.
      * @paragraph PROCESS-SOURCE-LINE Classify comment vs code line.
       PROCESS-SOURCE-LINE.
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
           PERFORM CHECK-IF-COMMENT
           IF WS-IS-COMMENT = 1
               PERFORM PARSE-DOC-TAGS
           ELSE
               IF WS-LINE-TEXT NOT = SPACES
                   PERFORM CHECK-CODE-LINE
               END-IF
           END-IF.
      * @paragraph CHECK-IF-COMMENT Detect col-7 '*'/'/' or '*>' line.
       CHECK-IF-COMMENT.
           MOVE 0 TO WS-IS-COMMENT
           IF WS-SLEN >= 7
               IF WS-LINE-TEXT(7:1) = "*" OR WS-LINE-TEXT(7:1) = "/"
                   MOVE 1 TO WS-IS-COMMENT
               END-IF
           END-IF
           IF WS-IS-COMMENT = 0
               MOVE 1 TO WS-TI
               PERFORM UNTIL WS-TI > WS-SLEN
                   OR WS-LINE-TEXT(WS-TI:1) NOT = SPACE
                   ADD 1 TO WS-TI
                   END-ADD
               END-PERFORM
               IF WS-TI < WS-SLEN
                   AND WS-LINE-TEXT(WS-TI:2) = "*>"
                   MOVE 1 TO WS-IS-COMMENT
               END-IF
           END-IF.
      * @paragraph PARSE-DOC-TAGS Scan leading @tag after '*' or '*>'.
       PARSE-DOC-TAGS.
           MOVE 1 TO WS-TI
           IF WS-SLEN >= 7
               AND (WS-LINE-TEXT(7:1) = "*" OR WS-LINE-TEXT(7:1) = "/")
               MOVE 8 TO WS-TI
           ELSE
               PERFORM UNTIL WS-TI > WS-SLEN
                   OR WS-LINE-TEXT(WS-TI:1) NOT = SPACE
                   ADD 1 TO WS-TI
                   END-ADD
               END-PERFORM
               IF WS-TI < WS-SLEN AND WS-LINE-TEXT(WS-TI:2) = "*>"
                   ADD 2 TO WS-TI
                   END-ADD
               END-IF
           END-IF
           PERFORM UNTIL WS-TI > WS-SLEN
               OR WS-LINE-TEXT(WS-TI:1) NOT = SPACE
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM
           IF WS-TI <= WS-SLEN AND WS-LINE-TEXT(WS-TI:1) = "@"
               PERFORM MATCH-AT-TAG
           END-IF.
      * @paragraph MATCH-AT-TAG Set doc state flag for @tag at WS-TI.
       MATCH-AT-TAG.
           EVALUATE TRUE
               WHEN WS-TI + 4 <= 256
                   AND WS-LINE-TEXT(WS-TI:5) = "@file"
                   MOVE 1 TO WS-SAW-FILE
               WHEN WS-TI + 5 <= 256
                   AND WS-LINE-TEXT(WS-TI:6) = "@brief"
                   MOVE 1 TO WS-SAW-BRIEF
               WHEN WS-TI + 6 <= 256
                   AND WS-LINE-TEXT(WS-TI:7) = "@module"
                   MOVE 1 TO WS-SAW-MOD
               WHEN WS-TI + 7 <= 256
                   AND WS-LINE-TEXT(WS-TI:8) = "@program"
                   MOVE 1 TO WS-SAW-MOD
               WHEN WS-TI + 7 <= 256
                   AND WS-LINE-TEXT(WS-TI:8) = "@details"
                   MOVE 1 TO WS-SAW-DETAILS
               WHEN WS-TI + 6 <= 256
                   AND WS-LINE-TEXT(WS-TI:7) = "@record"
                   MOVE 1 TO WS-SAW-REC
               WHEN WS-TI + 7 <= 256
                   AND WS-LINE-TEXT(WS-TI:8) = "@typedef"
                   MOVE 1 TO WS-SAW-REC
               WHEN WS-TI + 5 <= 256
                   AND WS-LINE-TEXT(WS-TI:6) = "@class"
                   MOVE 1 TO WS-SAW-REC
                   MOVE 1 TO WS-SAW-MOD
               WHEN WS-TI + 5 <= 256
                   AND WS-LINE-TEXT(WS-TI:6) = "@field"
                   MOVE 1 TO WS-SAW-FIELD
               WHEN WS-TI + 10 <= 256
                   AND WS-LINE-TEXT(WS-TI:11) = "@subprogram"
                   MOVE 1 TO WS-SAW-SUBP
               WHEN WS-TI + 5 <= 256
                   AND WS-LINE-TEXT(WS-TI:6) = "@param"
                   ADD 1 TO WS-PARAM-CNT
                   END-ADD
               WHEN WS-TI + 6 <= 256
                   AND WS-LINE-TEXT(WS-TI:7) = "@return"
                   MOVE 1 TO WS-SAW-RET
               WHEN WS-TI + 6 <= 256
                   AND WS-LINE-TEXT(WS-TI:7) = "@errors"
                   MOVE 1 TO WS-SAW-ERRS
               WHEN WS-TI + 7 <= 256
                   AND WS-LINE-TEXT(WS-TI:8) = "@section"
                   MOVE 1 TO WS-SAW-SEC
               WHEN WS-TI + 9 <= 256
                   AND WS-LINE-TEXT(WS-TI:10) = "@paragraph"
                   MOVE 1 TO WS-SAW-PARA
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph REPORT-VIOLATION Increment failure count and print.
       REPORT-VIOLATION.
           ADD 1 TO WS-VIOLATIONS
           END-ADD
           IF NOT CDD-AGV-QUIET-YES
               COMPUTE WS-DIAG-LINE = WS-LINE-NO
               END-COMPUTE
               DISPLAY WS-CUR-PATH(1:WS-CUR-PLEN) ":" WS-DIAG-LINE
                   ": missing " WS-VIOL-MSG UPON SYSERR
               END-DISPLAY
           END-IF.
      * @paragraph CHECK-CODE-LINE Validate required docs on code line.
       CHECK-CODE-LINE.
           IF WS-FIRST-CODE-SEEN = 0
               MOVE 1 TO WS-FIRST-CODE-SEEN
               ADD 2 TO WS-ITEMS-TOT
               END-ADD
               IF WS-SAW-FILE = 0
                   MOVE "@file header tag" TO WS-VIOL-MSG
                   PERFORM REPORT-VIOLATION
               END-IF
               IF WS-SAW-BRIEF = 0
                   MOVE "@brief header tag" TO WS-VIOL-MSG
                   PERFORM REPORT-VIOLATION
               END-IF
           END-IF
           IF WS-SLEN >= 7 AND WS-LINE-TEXT(7:1) = "-"
               CONTINUE
           ELSE
               PERFORM EXTRACT-CODE-TOKENS
               IF WS-IN-PROC-HDR = 1
                   PERFORM SCAN-PROC-HDR-LINE
               ELSE
                   PERFORM DISPATCH-CODE-CONSTRUCT
               END-IF
           END-IF.
      * @paragraph EXTRACT-CODE-TOKENS Slice cols 8..72 and get 2 toks.
       EXTRACT-CODE-TOKENS.
           MOVE SPACES TO WS-CODE-AREA
           IF WS-SLEN >= 8
               IF WS-SLEN >= 72
                   MOVE WS-LINE-TEXT(8:65) TO WS-CODE-AREA(1:65)
               ELSE
                   COMPUTE WS-TLEN = WS-SLEN - 7
                   END-COMPUTE
                   MOVE WS-LINE-TEXT(8:WS-TLEN)
                       TO WS-CODE-AREA(1:WS-TLEN)
               END-IF
           END-IF
           INSPECT WS-CODE-AREA
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           MOVE SPACES TO WS-TOK1
           MOVE SPACES TO WS-TOK2
           MOVE 0 TO WS-HAS-DOT
           MOVE 1 TO WS-TI
           PERFORM NEXT-CODE-TOKEN
           MOVE WS-TOK-CUR TO WS-TOK1
           PERFORM NEXT-CODE-TOKEN
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
      * @paragraph NEXT-CODE-TOKEN Read next token from WS-CODE-AREA.
       NEXT-CODE-TOKEN.
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
      * @paragraph DISPATCH-CODE-CONSTRUCT Check division/item rules.
       DISPATCH-CODE-CONSTRUCT.
           EVALUATE TRUE
               WHEN WS-TOK1 = "IDENTIFICATION" AND WS-TOK2 = "DIVISION"
                   MOVE 1 TO WS-DIV-MODE
               WHEN WS-TOK1 = "ENVIRONMENT" AND WS-TOK2 = "DIVISION"
                   MOVE 2 TO WS-DIV-MODE
               WHEN WS-TOK1 = "DATA" AND WS-TOK2 = "DIVISION"
                   MOVE 3 TO WS-DIV-MODE
               WHEN WS-TOK1 = "PROCEDURE" AND WS-TOK2 = "DIVISION"
                   PERFORM BEGIN-PROC-DIVISION
               WHEN WS-TOK1 = "PROGRAM-ID"
                   OR WS-TOK1 = "FUNCTION-ID"
                   OR WS-TOK1 = "CLASS-ID"
                   PERFORM CHECK-MODULE-DOC
               WHEN WS-DIV-MODE = 3
                   PERFORM CHECK-DATA-ITEM-DOC
               WHEN WS-DIV-MODE = 4
                   PERFORM CHECK-PROC-ITEM-DOC
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph CHECK-MODULE-DOC Require @module and @details tags.
       CHECK-MODULE-DOC.
           ADD 2 TO WS-ITEMS-TOT
           END-ADD
           IF WS-SAW-MOD = 0
               MOVE "@module/@program/@class tag" TO WS-VIOL-MSG
               PERFORM REPORT-VIOLATION
           END-IF
           IF WS-SAW-DETAILS = 0
               MOVE "@details module tag" TO WS-VIOL-MSG
               PERFORM REPORT-VIOLATION
           END-IF
           MOVE 0 TO WS-SAW-MOD
           MOVE 0 TO WS-SAW-DETAILS.
      * @paragraph CHECK-DATA-ITEM-DOC Check 01 record and field docs.
       CHECK-DATA-ITEM-DOC.
           IF WS-TOK1 = "01"
               ADD 1 TO WS-ITEMS-TOT
               END-ADD
               IF WS-SAW-REC = 0
                   MOVE "@record/@typedef/@class on 01 item"
                       TO WS-VIOL-MSG
                   PERFORM REPORT-VIOLATION
               END-IF
               MOVE 0 TO WS-SAW-REC
               MOVE 0 TO WS-SAW-FIELD
           ELSE
               IF WS-TOK1(1:1) >= "0" AND WS-TOK1(1:1) <= "9"
                   AND WS-TOK1(2:1) >= "0" AND WS-TOK1(2:1) <= "9"
                   AND WS-TOK1(3:1) = SPACE
                   MOVE WS-TOK1(1:2) TO WS-LVL-NUM
                   IF (WS-LVL-NUM >= 2 AND WS-LVL-NUM <= 49)
                       OR WS-LVL-NUM = 66
                       OR WS-LVL-NUM = 77
                       OR WS-LVL-NUM = 88
                       ADD 1 TO WS-ITEMS-TOT
                       END-ADD
                       IF WS-SAW-FIELD = 0
                           MOVE "@field tag on data item"
                               TO WS-VIOL-MSG
                           PERFORM REPORT-VIOLATION
                       END-IF
                       MOVE 0 TO WS-SAW-FIELD
                   END-IF
               END-IF
           END-IF.
      * @paragraph BEGIN-PROC-DIVISION Check @subprogram/@return/@err.
       BEGIN-PROC-DIVISION.
           MOVE 4 TO WS-DIV-MODE
           ADD 3 TO WS-ITEMS-TOT
           END-ADD
           IF WS-SAW-SUBP = 0
               MOVE "@subprogram tag above PROCEDURE DIVISION"
                   TO WS-VIOL-MSG
               PERFORM REPORT-VIOLATION
           END-IF
           IF WS-SAW-RET = 0
               MOVE "@return tag above PROCEDURE DIVISION"
                   TO WS-VIOL-MSG
               PERFORM REPORT-VIOLATION
           END-IF
           IF WS-SAW-ERRS = 0
               MOVE "@errors tag above PROCEDURE DIVISION"
                   TO WS-VIOL-MSG
               PERFORM REPORT-VIOLATION
           END-IF
           MOVE 0 TO WS-SAW-SUBP
           MOVE 0 TO WS-SAW-RET
           MOVE 0 TO WS-SAW-ERRS
           MOVE 0 TO WS-IN-USING
           MOVE 0 TO WS-USING-CNT
           MOVE 1 TO WS-IN-PROC-HDR
           PERFORM SCAN-PROC-HDR-LINE.
      * @paragraph SCAN-PROC-HDR-LINE Count USING args until period.
       SCAN-PROC-HDR-LINE.
           MOVE 1 TO WS-TI
           PERFORM UNTIL WS-TI > 65
               PERFORM NEXT-CODE-TOKEN
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
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF WS-HAS-DOT = 1
               MOVE 0 TO WS-IN-PROC-HDR
               ADD WS-USING-CNT TO WS-ITEMS-TOT
               END-ADD
               IF WS-PARAM-CNT NOT = WS-USING-CNT
                   MOVE "1-to-1 @param tags for USING arguments"
                       TO WS-VIOL-MSG
                   PERFORM REPORT-VIOLATION
               END-IF
               MOVE 0 TO WS-PARAM-CNT
           END-IF.
      * @paragraph CHECK-PROC-ITEM-DOC Check Area A sections/paragraphs.
       CHECK-PROC-ITEM-DOC.
           IF WS-CODE-AREA(1:4) NOT = SPACES
               AND WS-TOK1 NOT = SPACES
               AND WS-TOK1 NOT = "DECLARATIVES"
               AND WS-TOK1 NOT = "END"
               IF WS-TOK2 = "SECTION"
                   ADD 1 TO WS-ITEMS-TOT
                   END-ADD
                   IF WS-SAW-SEC = 0
                       MOVE "@section tag above SECTION"
                           TO WS-VIOL-MSG
                       PERFORM REPORT-VIOLATION
                   END-IF
                   MOVE 0 TO WS-SAW-SEC
               ELSE
                   IF WS-TOK2 = SPACES AND WS-HAS-DOT = 1
                       ADD 1 TO WS-ITEMS-TOT
                       END-ADD
                       IF WS-SAW-PARA = 0
                           MOVE "@paragraph tag above paragraph"
                               TO WS-VIOL-MSG
                           PERFORM REPORT-VIOLATION
                       END-IF
                       MOVE 0 TO WS-SAW-PARA
                   END-IF
               END-IF
           END-IF.
