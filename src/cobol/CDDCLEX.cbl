      * @file CDDCLEX.cbl
      * @brief Lossless 10-dialect fixed/free trivia-preserving lexer.
      * @module CDDCLEX
      * @details Partitions raw COBOL source bytes across all 10 eras
      *          (COBOL-60 through COBOL-2023) into a contiguous,
      *          zero-loss stream of semantic tokens and trivia slices
      *          (cols 1-6 seq, col 7 indicators, Area A/B spaces,
      *          cols 73-80 tags, REMARKS, NOTE, *>, >>SOURCE FORMAT
      *          directives, PICTURE strings, and continuations).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCLEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-dial.cpy".
      * @record WS-BYTE-CONV Pure 2-byte big-endian LF/CR/TAB helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-LEX-STATE Line, column, division, and PIC trackers.
       01  WS-LEX-STATE.
      * @field WS-LF Cached LF byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR Cached CR byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-TAB Cached TAB byte (9).
           05  WS-TAB                    PIC X(1) VALUE SPACE.
      * @field WS-POS Global cursor in CDD-BUF-DATA.
           05  WS-POS                    PIC 9(8) COMP VALUE 1.
      * @field WS-LINE-NO 1-based physical line counter.
           05  WS-LINE-NO                PIC 9(8) COMP VALUE 0.
      * @field WS-LSTART 1-based start offset of current line body.
           05  WS-LSTART                 PIC 9(8) COMP VALUE 1.
      * @field WS-LLEN Length of current line excluding CR/LF.
           05  WS-LLEN                   PIC 9(8) COMP VALUE 0.
      * @field WS-NL-OFF Start offset of CR/LF sequence (0=EOF).
           05  WS-NL-OFF                 PIC 9(8) COMP VALUE 0.
      * @field WS-NL-LEN Length of line ending (0, 1, or 2).
           05  WS-NL-LEN                 PIC 9(4) COMP VALUE 0.
      * @field WS-CSTART Start offset of code area on current line.
           05  WS-CSTART                 PIC 9(8) COMP VALUE 0.
      * @field WS-CEND Inclusive end offset of code area on line.
           05  WS-CEND                   PIC 9(8) COMP VALUE 0.
      * @field WS-CPOS Scan cursor inside [WS-CSTART..WS-CEND].
           05  WS-CPOS                   PIC 9(8) COMP VALUE 0.
      * @field WS-TSTART Start offset of token/trivia being built.
           05  WS-TSTART                 PIC 9(8) COMP VALUE 0.
      * @field WS-TLEN Byte length of token/trivia being built.
           05  WS-TLEN                   PIC 9(8) COMP VALUE 0.
      * @field WS-TCOL 1-based column of WS-TSTART.
           05  WS-TCOL                   PIC 9(4) COMP VALUE 1.
      * @field WS-TKIND Kind code to push.
           05  WS-TKIND                  PIC 9(2) COMP VALUE 0.
      * @field WS-TTRIV 1 if trivia item, 0 if code token.
           05  WS-TTRIV                  PIC 9(2) COMP VALUE 0.
      * @field WS-PIC-MODE 1 after PIC/PICTURE waiting for PIC string.
           05  WS-PIC-MODE               PIC 9(2) COMP VALUE 0.
      * @field WS-NOTE-MODE 1 while scanning COBOL-60..68 NOTE sentence.
           05  WS-NOTE-MODE              PIC 9(2) COMP VALUE 0.
      * @field WS-REM-MODE 1 after REMARKS., 2 in REMARKS body lines.
           05  WS-REM-MODE               PIC 9(2) COMP VALUE 0.
      * @field WS-OPEN-QUOTE Non-space quote char if cont across lines.
           05  WS-OPEN-QUOTE             PIC X(1) VALUE SPACE.
      * @field WS-FIRST-LINE-TOK 1 before first code token on line.
           05  WS-FIRST-LINE-TOK         PIC 9(2) COMP VALUE 1.
      * @field WS-DIV-STATE Active division (1=ID,2=ENV,3=DATA,4=PROC).
           05  WS-DIV-STATE              PIC 9(2) COMP VALUE 3.
      * @field WS-PREV-WORD Previous uppercased word on stream.
           05  WS-PREV-WORD              PIC X(32) VALUE SPACES.
      * @field WS-IND-CH Indicator character in column 7.
           05  WS-IND-CH                 PIC X(1) VALUE SPACE.
      * @field WS-CH Current character at WS-CPOS.
           05  WS-CH                     PIC X(1) VALUE SPACE.
      * @field WS-QUOTE-CH Active string delimiter (' or ").
           05  WS-QUOTE-CH               PIC X(1) VALUE SPACE.
      * @field WS-ALL-DIGITS 1 if scanned word contains only 0..9.
           05  WS-ALL-DIGITS             PIC 9(2) COMP VALUE 0.
      * @field WS-LVL-VAL Parsed 2-digit level candidate.
           05  WS-LVL-VAL                PIC 9(2) COMP VALUE 0.
      * @field WS-SCAN-K Scratch offset helper.
           05  WS-SCAN-K                 PIC 9(8) COMP VALUE 0.
      * @field WS-HAS-DIR 1 if line begins with '>>' directive.
           05  WS-HAS-DIR                PIC 9(2) COMP VALUE 0.
      * @record WS-UP-BUF Uppercased slice scratch buffer.
       01  WS-UP-BUF                     PIC X(128) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDCLEX Tokenize COBOL source with 100% trivia.
      * @param [in] CDD-BUF Raw COBOL source or copybook buffer.
      * @param [inout] CDD-CTOK-STREAM Output token + trivia slices.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Contiguous byte-partitioned CDD-CTOK-STREAM.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-COB-DIALECT-DISALLOWED,
      *         CDD-ERR-COB-BAD-INDICATOR, CDD-ERR-COB-UNCLOSED-LITERAL,
      *         CDD-ERR-COB-CST-CAPACITY.
       PROCEDURE DIVISION USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, query dialect, scan lines.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID OR NOT CDD-CTK-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-BUF or CDD-CTOK-STREAM tag"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-LEXER
           IF CDD-OK
               PERFORM SCAN-ALL-LINES
           END-IF
           IF CDD-OK AND WS-OPEN-QUOTE NOT = SPACE
               MOVE 2004 TO CDD-ERR-CODE
               MOVE WS-LINE-NO TO CDD-ERR-LINE
               MOVE 1 TO CDD-ERR-COL
               MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
               MOVE "Unclosed COBOL string literal at EOF"
                   TO CDD-ERR-MESSAGE
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-LEXER Load dialect rules and initial format.
       INIT-LEXER.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 9 TO WS-U16
           MOVE WS-B-LO TO WS-TAB
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-CTK-COUNT
           MOVE 1 TO WS-POS
           MOVE 0 TO WS-LINE-NO
           MOVE 0 TO WS-PIC-MODE
           MOVE 0 TO WS-NOTE-MODE
           MOVE 0 TO WS-REM-MODE
           MOVE SPACE TO WS-OPEN-QUOTE
           MOVE 3 TO WS-DIV-STATE
           MOVE SPACES TO WS-PREV-WORD
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 1 TO CDD-DLR-OP
           MOVE CDD-CTK-DIALECT TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-OK
               PERFORM RESOLVE-INITIAL-FORMAT
           END-IF.
      * @paragraph RESOLVE-INITIAL-FORMAT Set fixed/free mode & check.
       RESOLVE-INITIAL-FORMAT.
           IF CDD-CTK-REQ-FREE
               IF CDD-DLR-ALLOW-FREE = 0
                   MOVE 2010 TO CDD-ERR-CODE
                   MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
                   MOVE "Free format disallowed before COBOL-2002"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE 2 TO CDD-CTK-ACT-FMT
               END-IF
           ELSE
               IF CDD-CTK-REQ-FIXED
                   MOVE 1 TO CDD-CTK-ACT-FMT
               ELSE
                   PERFORM AUTO-DETECT-SOURCE-FMT
               END-IF
           END-IF.
      * @paragraph AUTO-DETECT-SOURCE-FMT Heuristic format check.
       AUTO-DETECT-SOURCE-FMT.
           MOVE 1 TO CDD-CTK-ACT-FMT
           IF CDD-DLR-ALLOW-FREE = 1 AND CDD-BUF-LEN >= 2
               MOVE 1 TO WS-SCAN-K
               PERFORM UNTIL WS-SCAN-K > CDD-BUF-LEN
                   OR WS-SCAN-K > 6
                   OR CDD-BUF-DATA(WS-SCAN-K:1) NOT = SPACE
                   ADD 1 TO WS-SCAN-K
                   END-ADD
               END-PERFORM
               IF WS-SCAN-K < CDD-BUF-LEN
                   AND WS-SCAN-K <= 6
                   IF CDD-BUF-DATA(WS-SCAN-K:2) = ">>"
                       OR CDD-BUF-DATA(WS-SCAN-K:2) = "*>"
                       OR (CDD-BUF-DATA(WS-SCAN-K:1) >= "A"
                       AND CDD-BUF-DATA(WS-SCAN-K:1) <= "Z")
                       OR (CDD-BUF-DATA(WS-SCAN-K:1) >= "a"
                       AND CDD-BUF-DATA(WS-SCAN-K:1) <= "z")
                       MOVE 2 TO CDD-CTK-ACT-FMT
                   END-IF
               END-IF
           END-IF.
      * @paragraph PUSH-SLICE Append item to CDD-CTOK-STREAM safely.
       PUSH-SLICE.
           IF WS-TLEN > 0 AND CDD-OK
               IF CDD-CTK-COUNT >= CDD-CTK-CAP
                   MOVE 2011 TO CDD-ERR-CODE
                   MOVE WS-LINE-NO TO CDD-ERR-LINE
                   MOVE WS-TCOL TO CDD-ERR-COL
                   MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
                   MOVE "COBOL token stream capacity exceeded"
                       TO CDD-ERR-MESSAGE
               ELSE
                   ADD 1 TO CDD-CTK-COUNT
                   END-ADD
                   MOVE WS-TTRIV TO CDD-CTI-IS-TRIVIA(CDD-CTK-COUNT)
                   MOVE WS-TKIND TO CDD-CTI-KIND(CDD-CTK-COUNT)
                   MOVE WS-TSTART TO CDD-CTI-OFFSET(CDD-CTK-COUNT)
                   MOVE WS-TLEN TO CDD-CTI-LEN(CDD-CTK-COUNT)
                   MOVE WS-LINE-NO TO CDD-CTI-LINE(CDD-CTK-COUNT)
                   MOVE WS-TCOL TO CDD-CTI-COL(CDD-CTK-COUNT)
               END-IF
           END-IF.
      * @paragraph SCAN-ALL-LINES Slice buffer into physical lines.
       SCAN-ALL-LINES.
           PERFORM UNTIL WS-POS > CDD-BUF-LEN OR NOT CDD-OK
               ADD 1 TO WS-LINE-NO
               END-ADD
               MOVE WS-POS TO WS-LSTART
               MOVE 0 TO WS-NL-OFF
               MOVE 0 TO WS-NL-LEN
               PERFORM UNTIL WS-POS > CDD-BUF-LEN
                   IF CDD-BUF-DATA(WS-POS:1) = WS-LF
                       MOVE WS-POS TO WS-NL-OFF
                       MOVE 1 TO WS-NL-LEN
                       IF WS-POS > WS-LSTART
                           AND CDD-BUF-DATA(WS-POS - 1:1) = WS-CR
                           SUBTRACT 1 FROM WS-NL-OFF
                           END-SUBTRACT
                           MOVE 2 TO WS-NL-LEN
                       END-IF
                       ADD 1 TO WS-POS
                       END-ADD
                       EXIT PERFORM
                   END-IF
                   ADD 1 TO WS-POS
                   END-ADD
               END-PERFORM
               IF WS-NL-OFF > 0
                   COMPUTE WS-LLEN = WS-NL-OFF - WS-LSTART
                   END-COMPUTE
               ELSE
                   COMPUTE WS-LLEN = CDD-BUF-LEN - WS-LSTART + 1
                   END-COMPUTE
               END-IF
               PERFORM PROCESS-PHYSICAL-LINE
               IF CDD-OK AND WS-NL-LEN > 0
                   MOVE 1 TO WS-TTRIV
                   MOVE 6 TO WS-TKIND
                   MOVE WS-NL-OFF TO WS-TSTART
                   MOVE WS-NL-LEN TO WS-TLEN
                   COMPUTE WS-TCOL = WS-LLEN + 1
                   END-COMPUTE
                   PERFORM PUSH-SLICE
               END-IF
           END-PERFORM.
      * @paragraph PROCESS-PHYSICAL-LINE Dispatch directive/fixed/free.
       PROCESS-PHYSICAL-LINE.
           PERFORM CHECK-DIRECTIVE-LINE
           IF WS-HAS-DIR = 1
               PERFORM EMIT-DIRECTIVE-LINE
           ELSE
               IF CDD-CTK-IS-FREE
                   IF WS-LLEN > 0
                       IF WS-OPEN-QUOTE NOT = SPACE
                           MOVE 2004 TO CDD-ERR-CODE
                           MOVE WS-LINE-NO TO CDD-ERR-LINE
                           MOVE 1 TO CDD-ERR-COL
                           MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
                           MOVE "Unclosed string in free format"
                               TO CDD-ERR-MESSAGE
                       ELSE
                           MOVE WS-LSTART TO WS-CSTART
                           COMPUTE WS-CEND =
                               WS-LSTART + WS-LLEN - 1
                           END-COMPUTE
                           MOVE 1 TO WS-FIRST-LINE-TOK
                           PERFORM LEX-CODE-RANGE
                       END-IF
                   END-IF
               ELSE
                   PERFORM PROCESS-FIXED-LINE
               END-IF
           END-IF.
      * @paragraph CHECK-DIRECTIVE-LINE Detect leading '>>' in 2002+.
       CHECK-DIRECTIVE-LINE.
           MOVE 0 TO WS-HAS-DIR
           IF CDD-DLR-ALLOW-FREE = 1 AND WS-LLEN >= 2
               MOVE WS-LSTART TO WS-SCAN-K
               PERFORM UNTIL WS-SCAN-K >= WS-LSTART + WS-LLEN - 1
                   OR CDD-BUF-DATA(WS-SCAN-K:1) NOT = SPACE
                   ADD 1 TO WS-SCAN-K
                   END-ADD
               END-PERFORM
               IF CDD-BUF-DATA(WS-SCAN-K:2) = ">>"
                   MOVE 1 TO WS-HAS-DIR
               END-IF
           END-IF.
      * @paragraph EMIT-DIRECTIVE-LINE Emit >> line & switch format.
       EMIT-DIRECTIVE-LINE.
           IF WS-SCAN-K > WS-LSTART
               MOVE 1 TO WS-TTRIV
               MOVE 4 TO WS-TKIND
               MOVE WS-LSTART TO WS-TSTART
               COMPUTE WS-TLEN = WS-SCAN-K - WS-LSTART
               END-COMPUTE
               MOVE 1 TO WS-TCOL
               PERFORM PUSH-SLICE
           END-IF
           MOVE 1 TO WS-TTRIV
           MOVE 7 TO WS-TKIND
           MOVE WS-SCAN-K TO WS-TSTART
           COMPUTE WS-TLEN = (WS-LSTART + WS-LLEN) - WS-SCAN-K
           END-COMPUTE
           COMPUTE WS-TCOL = WS-SCAN-K - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE
           MOVE SPACES TO WS-UP-BUF
           IF WS-TLEN <= 128
               MOVE CDD-BUF-DATA(WS-SCAN-K:WS-TLEN)
                   TO WS-UP-BUF(1:WS-TLEN)
           ELSE
               MOVE CDD-BUF-DATA(WS-SCAN-K:128) TO WS-UP-BUF(1:128)
           END-IF
           INSPECT WS-UP-BUF
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           MOVE 0 TO WS-U16
           INSPECT WS-UP-BUF TALLYING WS-U16 FOR ALL "FREE"
           IF WS-U16 > 0
               MOVE 2 TO CDD-CTK-ACT-FMT
           END-IF
           MOVE 0 TO WS-U16
           INSPECT WS-UP-BUF TALLYING WS-U16 FOR ALL "FIXED"
           IF WS-U16 > 0
               MOVE 1 TO CDD-CTK-ACT-FMT
           END-IF.
      * @paragraph PROCESS-FIXED-LINE Slice cols 1-6, 7, 8-72, 73+.
       PROCESS-FIXED-LINE.
           IF WS-LLEN > 0
               IF WS-LLEN <= 6
                   IF WS-OPEN-QUOTE NOT = SPACE
                       PERFORM FAIL-UNCLOSED-STR
                   ELSE
                       MOVE 1 TO WS-TTRIV
                       MOVE 1 TO WS-TKIND
                       MOVE WS-LSTART TO WS-TSTART
                       MOVE WS-LLEN TO WS-TLEN
                       MOVE 1 TO WS-TCOL
                       PERFORM PUSH-SLICE
                   END-IF
               ELSE
                   MOVE 1 TO WS-TTRIV
                   MOVE 1 TO WS-TKIND
                   MOVE WS-LSTART TO WS-TSTART
                   MOVE 6 TO WS-TLEN
                   MOVE 1 TO WS-TCOL
                   PERFORM PUSH-SLICE
                   MOVE CDD-BUF-DATA(WS-LSTART + 6:1) TO WS-IND-CH
                   MOVE 1 TO WS-TTRIV
                   MOVE 2 TO WS-TKIND
                   COMPUTE WS-TSTART = WS-LSTART + 6
                   END-COMPUTE
                   MOVE 1 TO WS-TLEN
                   MOVE 7 TO WS-TCOL
                   PERFORM PUSH-SLICE
                   PERFORM DISPATCH-FIXED-COL7
                   IF CDD-OK AND WS-LLEN > 72
                       MOVE 1 TO WS-TTRIV
                       MOVE 5 TO WS-TKIND
                       COMPUTE WS-TSTART = WS-LSTART + 72
                       END-COMPUTE
                       COMPUTE WS-TLEN = WS-LLEN - 72
                       END-COMPUTE
                       MOVE 73 TO WS-TCOL
                       PERFORM PUSH-SLICE
                   END-IF
               END-IF
           END-IF.
      * @paragraph DISPATCH-FIXED-COL7 Handle col 7 space/*/D/-/slash.
       DISPATCH-FIXED-COL7.
           COMPUTE WS-CSTART = WS-LSTART + 7
           END-COMPUTE
           IF WS-LLEN >= 72
               COMPUTE WS-CEND = WS-LSTART + 71
               END-COMPUTE
           ELSE
               COMPUTE WS-CEND = WS-LSTART + WS-LLEN - 1
               END-COMPUTE
           END-IF
           EVALUATE WS-IND-CH
               WHEN "*"
               WHEN "/"
               WHEN "D"
               WHEN "d"
                   IF WS-CEND >= WS-CSTART
                       MOVE 1 TO WS-TTRIV
                       MOVE 3 TO WS-TKIND
                       MOVE WS-CSTART TO WS-TSTART
                       COMPUTE WS-TLEN = WS-CEND - WS-CSTART + 1
                       END-COMPUTE
                       MOVE 8 TO WS-TCOL
                       PERFORM PUSH-SLICE
                   END-IF
               WHEN "-"
                   PERFORM HANDLE-CONTINUATION-LINE
               WHEN " "
                   IF WS-OPEN-QUOTE NOT = SPACE
                       PERFORM FAIL-UNCLOSED-STR
                   ELSE
                       IF WS-CEND >= WS-CSTART
                           PERFORM CHECK-REMARKS-TERMINATION
                           IF WS-REM-MODE = 2
                               MOVE 1 TO WS-TTRIV
                               MOVE 9 TO WS-TKIND
                               MOVE WS-CSTART TO WS-TSTART
                               COMPUTE WS-TLEN =
                                   WS-CEND - WS-CSTART + 1
                               END-COMPUTE
                               MOVE 8 TO WS-TCOL
                               PERFORM PUSH-SLICE
                           ELSE
                               MOVE 1 TO WS-FIRST-LINE-TOK
                               PERFORM LEX-CODE-RANGE
                           END-IF
                       END-IF
                   END-IF
               WHEN OTHER
                   MOVE 2001 TO CDD-ERR-CODE
                   MOVE WS-LINE-NO TO CDD-ERR-LINE
                   MOVE 7 TO CDD-ERR-COL
                   MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
                   MOVE "Invalid indicator character in column 7"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph HANDLE-CONTINUATION-LINE Continue literal across '-'.
       HANDLE-CONTINUATION-LINE.
           IF WS-CEND < WS-CSTART
               MOVE 2005 TO CDD-ERR-CODE
               MOVE WS-LINE-NO TO CDD-ERR-LINE
               MOVE 7 TO CDD-ERR-COL
               MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
               MOVE "Empty continuation line after '-' in col 7"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE WS-CSTART TO WS-CPOS
               PERFORM SKIP-WS-SLICE
               IF WS-CPOS > WS-CEND
                   MOVE 2005 TO CDD-ERR-CODE
                   MOVE WS-LINE-NO TO CDD-ERR-LINE
                   MOVE 12 TO CDD-ERR-COL
                   MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
                   MOVE "Blank continuation line after '-'"
                       TO CDD-ERR-MESSAGE
               ELSE
                   IF WS-OPEN-QUOTE NOT = SPACE
                       IF CDD-BUF-DATA(WS-CPOS:1) NOT = WS-OPEN-QUOTE
                           MOVE 2005 TO CDD-ERR-CODE
                           MOVE WS-LINE-NO TO CDD-ERR-LINE
                           COMPUTE CDD-ERR-COL =
                               WS-CPOS - WS-LSTART + 1
                           END-COMPUTE
                           MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
                           MOVE "Expected quote on continuation line"
                               TO CDD-ERR-MESSAGE
                       ELSE
                           MOVE WS-OPEN-QUOTE TO WS-QUOTE-CH
                           MOVE SPACE TO WS-OPEN-QUOTE
                           PERFORM SCAN-QUOTED-LITERAL
                           IF CDD-OK AND WS-CPOS <= WS-CEND
                               MOVE 0 TO WS-FIRST-LINE-TOK
                               PERFORM LEX-CODE-RANGE-FROM-CPOS
                           END-IF
                       END-IF
                   ELSE
                       MOVE 0 TO WS-FIRST-LINE-TOK
                       PERFORM LEX-CODE-RANGE-FROM-CPOS
                   END-IF
               END-IF
           END-IF.
      * @paragraph CHECK-REMARKS-TERMINATION End REMARKS on Area A hdr.
       CHECK-REMARKS-TERMINATION.
           IF WS-REM-MODE = 2
               IF CDD-BUF-DATA(WS-CSTART:4) NOT = SPACES
                   MOVE SPACES TO WS-UP-BUF
                   COMPUTE WS-TLEN = WS-CEND - WS-CSTART + 1
                   END-COMPUTE
                   MOVE CDD-BUF-DATA(WS-CSTART:WS-TLEN)
                       TO WS-UP-BUF(1:WS-TLEN)
                   INSPECT WS-UP-BUF
                       CONVERTING "abcdefghijklmnopqrstuvwxyz"
                       TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   IF WS-UP-BUF(1:11) = "ENVIRONMENT"
                       OR WS-UP-BUF(1:4) = "DATA"
                       OR WS-UP-BUF(1:9) = "PROCEDURE"
                       MOVE 0 TO WS-REM-MODE
                   END-IF
               END-IF
           END-IF.
      * @paragraph LEX-CODE-RANGE Scan code area [WS-CSTART..WS-CEND].
       LEX-CODE-RANGE.
           MOVE WS-CSTART TO WS-CPOS
           PERFORM LEX-CODE-RANGE-FROM-CPOS.
      * @paragraph LEX-CODE-RANGE-FROM-CPOS Scan tokens from WS-CPOS.
       LEX-CODE-RANGE-FROM-CPOS.
           PERFORM UNTIL WS-CPOS > WS-CEND OR NOT CDD-OK
               PERFORM SKIP-WS-SLICE
               IF WS-CPOS <= WS-CEND
                   IF WS-NOTE-MODE = 1
                       PERFORM SCAN-NOTE-BODY
                   ELSE
                       IF WS-REM-MODE = 2
                           MOVE 1 TO WS-TTRIV
                           MOVE 9 TO WS-TKIND
                           MOVE WS-CPOS TO WS-TSTART
                           COMPUTE WS-TLEN = WS-CEND - WS-CPOS + 1
                           END-COMPUTE
                           COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
                           END-COMPUTE
                           PERFORM PUSH-SLICE
                           ADD WS-TLEN TO WS-CPOS
                           END-ADD
                       ELSE
                           PERFORM SCAN-CODE-ITEM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      * @paragraph SKIP-WS-SLICE Emit contiguous spaces/tabs as trivia.
       SKIP-WS-SLICE.
           MOVE WS-CPOS TO WS-TSTART
           PERFORM UNTIL WS-CPOS > WS-CEND
               MOVE CDD-BUF-DATA(WS-CPOS:1) TO WS-CH
               IF WS-CH = SPACE OR WS-CH = WS-TAB
                   ADD 1 TO WS-CPOS
                   END-ADD
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF WS-CPOS > WS-TSTART
               MOVE 1 TO WS-TTRIV
               MOVE 4 TO WS-TKIND
               COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
               END-COMPUTE
               COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
               END-COMPUTE
               PERFORM PUSH-SLICE
           END-IF.
      * @paragraph SCAN-NOTE-BODY Consume NOTE trivia up to period.
       SCAN-NOTE-BODY.
           MOVE WS-CPOS TO WS-TSTART
           PERFORM UNTIL WS-CPOS > WS-CEND
               IF CDD-BUF-DATA(WS-CPOS:1) = "."
                   MOVE 0 TO WS-NOTE-MODE
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-CPOS
               END-ADD
           END-PERFORM
           IF WS-CPOS > WS-TSTART
               MOVE 1 TO WS-TTRIV
               MOVE 10 TO WS-TKIND
               COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
               END-COMPUTE
               COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
               END-COMPUTE
               PERFORM PUSH-SLICE
           END-IF.
      * @paragraph SCAN-CODE-ITEM Dispatch comment, PIC, lit, or word.
       SCAN-CODE-ITEM.
           MOVE CDD-BUF-DATA(WS-CPOS:1) TO WS-CH
           IF WS-CPOS < WS-CEND
               AND CDD-BUF-DATA(WS-CPOS:2) = "*>"
               MOVE 1 TO WS-TTRIV
               MOVE 8 TO WS-TKIND
               MOVE WS-CPOS TO WS-TSTART
               COMPUTE WS-TLEN = WS-CEND - WS-CPOS + 1
               END-COMPUTE
               COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
               END-COMPUTE
               PERFORM PUSH-SLICE
               COMPUTE WS-CPOS = WS-CEND + 1
               END-COMPUTE
           ELSE
               IF WS-PIC-MODE = 1
                   IF WS-CPOS + 1 <= WS-CEND
                       AND (CDD-BUF-DATA(WS-CPOS:2) = "IS"
                       OR CDD-BUF-DATA(WS-CPOS:2) = "is")
                       AND (WS-CPOS + 1 = WS-CEND
                       OR CDD-BUF-DATA(WS-CPOS + 2:1) = SPACE
                       OR CDD-BUF-DATA(WS-CPOS + 2:1) = WS-TAB)
                       PERFORM SCAN-STANDARD-TOKEN
                   ELSE
                       PERFORM SCAN-PIC-CHARACTER-STRING
                   END-IF
               ELSE
                   PERFORM SCAN-STANDARD-TOKEN
               END-IF
           END-IF.
      * @paragraph SCAN-PIC-CHARACTER-STRING Lex PIC specification.
       SCAN-PIC-CHARACTER-STRING.
           MOVE 0 TO WS-PIC-MODE
           MOVE WS-CPOS TO WS-TSTART
           PERFORM UNTIL WS-CPOS > WS-CEND
               MOVE CDD-BUF-DATA(WS-CPOS:1) TO WS-CH
               IF WS-CH = SPACE OR WS-CH = WS-TAB
                   EXIT PERFORM
               END-IF
               IF WS-CH = "."
                   IF WS-CPOS = WS-CEND
                       OR CDD-BUF-DATA(WS-CPOS + 1:1) = SPACE
                       OR CDD-BUF-DATA(WS-CPOS + 1:1) = WS-TAB
                       EXIT PERFORM
                   END-IF
               END-IF
               ADD 1 TO WS-CPOS
               END-ADD
           END-PERFORM
           MOVE 0 TO WS-TTRIV
           MOVE 23 TO WS-TKIND
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE.
      * @paragraph SCAN-STANDARD-TOKEN Lex quotes, pseudo, punct, word.
       SCAN-STANDARD-TOKEN.
           MOVE CDD-BUF-DATA(WS-CPOS:1) TO WS-CH
           EVALUATE TRUE
               WHEN WS-CH = '"' OR WS-CH = "'"
                   MOVE WS-CH TO WS-QUOTE-CH
                   PERFORM SCAN-QUOTED-LITERAL
               WHEN WS-CPOS < WS-CEND
                   AND CDD-BUF-DATA(WS-CPOS:2) = "=="
                   PERFORM SCAN-PSEUDO-TEXT
               WHEN WS-CH = "."
                   PERFORM EMIT-DOT-OR-DECIMAL
               WHEN WS-CH = "("
                   MOVE 0 TO WS-TTRIV
                   MOVE 28 TO WS-TKIND
                   MOVE WS-CPOS TO WS-TSTART
                   MOVE 1 TO WS-TLEN
                   COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
                   END-COMPUTE
                   PERFORM PUSH-SLICE
                   ADD 1 TO WS-CPOS
                   END-ADD
               WHEN WS-CH = ")"
                   MOVE 0 TO WS-TTRIV
                   MOVE 29 TO WS-TKIND
                   MOVE WS-CPOS TO WS-TSTART
                   MOVE 1 TO WS-TLEN
                   COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
                   END-COMPUTE
                   PERFORM PUSH-SLICE
                   ADD 1 TO WS-CPOS
                   END-ADD
               WHEN WS-CH = ":" OR WS-CH = "," OR WS-CH = ";"
                   OR WS-CH = "=" OR WS-CH = "<" OR WS-CH = ">"
                   OR WS-CH = "*" OR WS-CH = "/" OR WS-CH = "&"
                   MOVE 0 TO WS-TTRIV
                   MOVE 30 TO WS-TKIND
                   MOVE WS-CPOS TO WS-TSTART
                   MOVE 1 TO WS-TLEN
                   COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
                   END-COMPUTE
                   PERFORM PUSH-SLICE
                   ADD 1 TO WS-CPOS
                   END-ADD
               WHEN (WS-CH = "+" OR WS-CH = "-")
                   AND WS-CPOS < WS-CEND
                   AND CDD-BUF-DATA(WS-CPOS + 1:1) >= "0"
                   AND CDD-BUF-DATA(WS-CPOS + 1:1) <= "9"
                   PERFORM SCAN-NUMERIC-LITERAL
               WHEN WS-CH = "+" OR WS-CH = "-"
                   MOVE 0 TO WS-TTRIV
                   MOVE 30 TO WS-TKIND
                   MOVE WS-CPOS TO WS-TSTART
                   MOVE 1 TO WS-TLEN
                   COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
                   END-COMPUTE
                   PERFORM PUSH-SLICE
                   ADD 1 TO WS-CPOS
                   END-ADD
               WHEN OTHER
                   PERFORM SCAN-WORD-OR-PREF-LIT
           END-EVALUATE
           IF WS-TKIND = 27
               MOVE 1 TO WS-FIRST-LINE-TOK
           ELSE
               MOVE 0 TO WS-FIRST-LINE-TOK
           END-IF.
      * @paragraph EMIT-DOT-OR-DECIMAL Handle '.' separator vs .5 num.
       EMIT-DOT-OR-DECIMAL.
           IF WS-CPOS < WS-CEND
               AND CDD-BUF-DATA(WS-CPOS + 1:1) >= "0"
               AND CDD-BUF-DATA(WS-CPOS + 1:1) <= "9"
               PERFORM SCAN-NUMERIC-LITERAL
           ELSE
               MOVE 0 TO WS-TTRIV
               MOVE 27 TO WS-TKIND
               MOVE WS-CPOS TO WS-TSTART
               MOVE 1 TO WS-TLEN
               COMPUTE WS-TCOL = WS-CPOS - WS-LSTART + 1
               END-COMPUTE
               PERFORM PUSH-SLICE
               ADD 1 TO WS-CPOS
               END-ADD
               IF WS-REM-MODE = 1
                   MOVE 2 TO WS-REM-MODE
               END-IF
           END-IF.
      * @paragraph SCAN-QUOTED-LITERAL Scan '...' or "..." with ''/"".
       SCAN-QUOTED-LITERAL.
           MOVE WS-CPOS TO WS-TSTART
           ADD 1 TO WS-CPOS
           END-ADD
           MOVE WS-QUOTE-CH TO WS-OPEN-QUOTE
           PERFORM UNTIL WS-CPOS > WS-CEND
               IF CDD-BUF-DATA(WS-CPOS:1) = WS-QUOTE-CH
                   IF WS-CPOS < WS-CEND
                       AND CDD-BUF-DATA(WS-CPOS + 1:1) = WS-QUOTE-CH
                       ADD 2 TO WS-CPOS
                       END-ADD
                   ELSE
                       ADD 1 TO WS-CPOS
                       END-ADD
                       MOVE SPACE TO WS-OPEN-QUOTE
                       EXIT PERFORM
                   END-IF
               ELSE
                   ADD 1 TO WS-CPOS
                   END-ADD
               END-IF
           END-PERFORM
           MOVE 0 TO WS-TTRIV
           MOVE 25 TO WS-TKIND
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE.
      * @paragraph SCAN-PSEUDO-TEXT Scan ==...== pseudo-text token.
       SCAN-PSEUDO-TEXT.
           MOVE WS-CPOS TO WS-TSTART
           ADD 2 TO WS-CPOS
           END-ADD
           PERFORM UNTIL WS-CPOS > WS-CEND
               IF WS-CPOS < WS-CEND
                   AND CDD-BUF-DATA(WS-CPOS:2) = "=="
                   ADD 2 TO WS-CPOS
                   END-ADD
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-CPOS
               END-ADD
           END-PERFORM
           MOVE 0 TO WS-TTRIV
           MOVE 26 TO WS-TKIND
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE.
      * @paragraph SCAN-NUMERIC-LITERAL Scan signed/decimal number.
       SCAN-NUMERIC-LITERAL.
           MOVE WS-CPOS TO WS-TSTART
           IF CDD-BUF-DATA(WS-CPOS:1) = "+"
               OR CDD-BUF-DATA(WS-CPOS:1) = "-"
               ADD 1 TO WS-CPOS
               END-ADD
           END-IF
           PERFORM UNTIL WS-CPOS > WS-CEND
               MOVE CDD-BUF-DATA(WS-CPOS:1) TO WS-CH
               IF WS-CH >= "0" AND WS-CH <= "9"
                   ADD 1 TO WS-CPOS
                   END-ADD
               ELSE
                   IF WS-CH = "."
                       AND WS-CPOS < WS-CEND
                       AND CDD-BUF-DATA(WS-CPOS + 1:1) >= "0"
                       AND CDD-BUF-DATA(WS-CPOS + 1:1) <= "9"
                       ADD 1 TO WS-CPOS
                       END-ADD
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
           MOVE 0 TO WS-TTRIV
           MOVE 24 TO WS-TKIND
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE.
      * @paragraph SCAN-WORD-OR-PREF-LIT Scan X'..' or word/level/num.
       SCAN-WORD-OR-PREF-LIT.
           IF WS-CPOS < WS-CEND
               AND (WS-CH = "X" OR WS-CH = "x"
               OR WS-CH = "B" OR WS-CH = "b"
               OR WS-CH = "N" OR WS-CH = "n"
               OR WS-CH = "U" OR WS-CH = "u")
               AND (CDD-BUF-DATA(WS-CPOS + 1:1) = "'"
               OR CDD-BUF-DATA(WS-CPOS + 1:1) = '"')
               MOVE CDD-BUF-DATA(WS-CPOS + 1:1) TO WS-QUOTE-CH
               MOVE WS-CPOS TO WS-TSTART
               ADD 2 TO WS-CPOS
               END-ADD
               MOVE WS-QUOTE-CH TO WS-OPEN-QUOTE
               PERFORM FINISH-PREF-QUOTED
           ELSE
               PERFORM SCAN-PLAIN-WORD
           END-IF.
      * @paragraph FINISH-PREF-QUOTED Scan closing quote of X'...'.
       FINISH-PREF-QUOTED.
           PERFORM UNTIL WS-CPOS > WS-CEND
               IF CDD-BUF-DATA(WS-CPOS:1) = WS-QUOTE-CH
                   ADD 1 TO WS-CPOS
                   END-ADD
                   MOVE SPACE TO WS-OPEN-QUOTE
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-CPOS
               END-ADD
           END-PERFORM
           MOVE 0 TO WS-TTRIV
           MOVE 25 TO WS-TKIND
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE.
      * @paragraph SCAN-PLAIN-WORD Scan word, level, or integer token.
       SCAN-PLAIN-WORD.
           MOVE WS-CPOS TO WS-TSTART
           MOVE 1 TO WS-ALL-DIGITS
           PERFORM UNTIL WS-CPOS > WS-CEND
               MOVE CDD-BUF-DATA(WS-CPOS:1) TO WS-CH
               IF WS-CH = SPACE OR WS-CH = WS-TAB
                   OR WS-CH = "." OR WS-CH = "(" OR WS-CH = ")"
                   OR WS-CH = ":" OR WS-CH = "," OR WS-CH = ";"
                   EXIT PERFORM
               END-IF
               IF WS-CH < "0" OR WS-CH > "9"
                   MOVE 0 TO WS-ALL-DIGITS
               END-IF
               ADD 1 TO WS-CPOS
               END-ADD
           END-PERFORM
           IF WS-ALL-DIGITS = 1
               AND WS-CPOS < WS-CEND
               AND CDD-BUF-DATA(WS-CPOS:1) = "."
               AND CDD-BUF-DATA(WS-CPOS + 1:1) >= "0"
               AND CDD-BUF-DATA(WS-CPOS + 1:1) <= "9"
               ADD 1 TO WS-CPOS
               END-ADD
               PERFORM UNTIL WS-CPOS > WS-CEND
                   OR CDD-BUF-DATA(WS-CPOS:1) < "0"
                   OR CDD-BUF-DATA(WS-CPOS:1) > "9"
                   ADD 1 TO WS-CPOS
                   END-ADD
               END-PERFORM
               MOVE 0 TO WS-ALL-DIGITS
               MOVE 24 TO WS-TKIND
           ELSE
               PERFORM CLASSIFY-SCANNED-WORD
           END-IF
           MOVE 0 TO WS-TTRIV
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           COMPUTE WS-TCOL = WS-TSTART - WS-LSTART + 1
           END-COMPUTE
           PERFORM PUSH-SLICE.
      * @paragraph CLASSIFY-SCANNED-WORD Detect level, PIC, div, NOTE.
       CLASSIFY-SCANNED-WORD.
           COMPUTE WS-TLEN = WS-CPOS - WS-TSTART
           END-COMPUTE
           MOVE SPACES TO WS-UP-BUF
           IF WS-TLEN <= 128
               MOVE CDD-BUF-DATA(WS-TSTART:WS-TLEN)
                   TO WS-UP-BUF(1:WS-TLEN)
           ELSE
               MOVE CDD-BUF-DATA(WS-TSTART:128) TO WS-UP-BUF(1:128)
           END-IF
           INSPECT WS-UP-BUF
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           IF WS-ALL-DIGITS = 1
               IF WS-FIRST-LINE-TOK = 1
                   AND WS-DIV-STATE = 3
                   AND (WS-TLEN = 1 OR WS-TLEN = 2)
                   MOVE CDD-BUF-DATA(WS-TSTART:WS-TLEN) TO WS-LVL-VAL
                   IF (WS-LVL-VAL >= 1 AND WS-LVL-VAL <= 49)
                       OR WS-LVL-VAL = 66
                       OR WS-LVL-VAL = 77
                       OR WS-LVL-VAL = 78
                       OR WS-LVL-VAL = 88
                       MOVE 22 TO WS-TKIND
                   ELSE
                       MOVE 24 TO WS-TKIND
                   END-IF
               ELSE
                   MOVE 24 TO WS-TKIND
               END-IF
           ELSE
               MOVE 21 TO WS-TKIND
               PERFORM UPDATE-KEYWORD-STATES
           END-IF
           MOVE WS-UP-BUF(1:32) TO WS-PREV-WORD.
      * @paragraph UPDATE-KEYWORD-STATES Track division, PIC, NOTE/REM.
       UPDATE-KEYWORD-STATES.
           IF WS-UP-BUF(1:32) = "DIVISION"
               EVALUATE WS-PREV-WORD
                   WHEN "IDENTIFICATION"
                   WHEN "ID"
                       MOVE 1 TO WS-DIV-STATE
                   WHEN "ENVIRONMENT"
                       MOVE 2 TO WS-DIV-STATE
                   WHEN "DATA"
                       MOVE 3 TO WS-DIV-STATE
                   WHEN "PROCEDURE"
                       MOVE 4 TO WS-DIV-STATE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
           IF WS-UP-BUF(1:32) = "PIC"
               OR WS-UP-BUF(1:32) = "PICTURE"
               MOVE 1 TO WS-PIC-MODE
           END-IF
           IF CDD-DLR-HAS-REM-NOTE = 1
               IF WS-DIV-STATE = 1 AND WS-UP-BUF(1:32) = "REMARKS"
                   MOVE 1 TO WS-REM-MODE
               END-IF
               IF WS-DIV-STATE = 4 AND WS-UP-BUF(1:32) = "NOTE"
                   MOVE 1 TO WS-NOTE-MODE
               END-IF
           END-IF.
      * @paragraph FAIL-UNCLOSED-STR Report unclosed quoted literal.
       FAIL-UNCLOSED-STR.
           MOVE 2004 TO CDD-ERR-CODE
           MOVE WS-LINE-NO TO CDD-ERR-LINE
           MOVE 7 TO CDD-ERR-COL
           MOVE "CDDCLEX" TO CDD-ERR-SUBPROG
           MOVE "Unclosed COBOL string literal across line"
               TO CDD-ERR-MESSAGE.
