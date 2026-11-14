      * @file CDDCPIC.cbl
      * @brief Exhaustive 10-dialect PICTURE, SIZE, USAGE & OCCURS parser.
      * @module CDDCPIC
      * @details Parses compact and expanded PIC/PICTURE strings (X, A,
      *          N, U, 1, 9, S, V, P, numeric-edited), COBOL-60 verbose
      *          SIZE/CLASS/POINT/SIGNED clauses, all 1960-2023 USAGE
      *          representations, and fixed/ODO/DYNAMIC OCCURS clauses.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCPIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
      * @record WS-PIC-STATE Scratch state for PIC and clause scanning.
       01  WS-PIC-STATE.
      * @field WS-I Scan cursor in PIC string or token stream.
           05  WS-I                      PIC 9(8) COMP VALUE 1.
      * @field WS-PLEN Length of active PIC character-string.
           05  WS-PLEN                   PIC 9(2) COMP VALUE 0.
      * @field WS-CH Uppercased character in PIC string.
           05  WS-CH                     PIC X(1) VALUE SPACE.
      * @field WS-SYM Active repeating symbol ('X','9','P', etc.).
           05  WS-SYM                    PIC X(1) VALUE SPACE.
      * @field WS-REP Repeat count parsed from '(n)' or 1.
           05  WS-REP                    PIC 9(8) COMP VALUE 1.
      * @field WS-SEEN-V 1 after 'V' or decimal '.' in PIC string.
           05  WS-SEEN-V                 PIC 9(2) COMP VALUE 0.
      * @field WS-INT-CNT Integer digit positions counted.
           05  WS-INT-CNT                PIC 9(4) COMP VALUE 0.
      * @field WS-FRAC-CNT Fractional digit positions counted.
           05  WS-FRAC-CNT               PIC 9(4) COMP VALUE 0.
      * @field WS-STR-CNT Alphanumeric/national/UTF-8 char count.
           05  WS-STR-CNT                PIC 9(8) COMP VALUE 0.
      * @field WS-PIC-CLASS Detected PIC class (1=X/A,2=N,3=U,4=1,5=9).
           05  WS-PIC-CLASS              PIC 9(2) COMP VALUE 0.
      * @field WS-PAREN-START Start of digits inside '(...)'.
           05  WS-PAREN-START            PIC 9(8) COMP VALUE 0.
      * @field WS-PAREN-LEN Digit count inside '(...)'.
           05  WS-PAREN-LEN              PIC 9(4) COMP VALUE 0.
      * @field WS-TI Token stream cursor during OP-CLAUSES.
           05  WS-TI                     PIC 9(8) COMP VALUE 0.
      * @field WS-NXT-TI Lookahead semantic token index.
           05  WS-NXT-TI                 PIC 9(8) COMP VALUE 0.
      * @field WS-TOK-LEN Clamped length of current token text.
           05  WS-TOK-LEN                PIC 9(4) COMP VALUE 0.
      * @field WS-CLAUSE-START Start token of active type/occurs clause.
           05  WS-CLAUSE-START           PIC 9(8) COMP VALUE 0.
      * @field WS-CLAUSE-END End token of active type/occurs clause.
           05  WS-CLAUSE-END             PIC 9(8) COMP VALUE 0.
      * @field WS-C60-SIZE COBOL-60 SIZE IS <n> value.
           05  WS-C60-SIZE               PIC 9(8) COMP VALUE 0.
      * @field WS-C60-CLASS COBOL-60 CLASS (1=AN, 2=NUM).
           05  WS-C60-CLASS              PIC 9(2) COMP VALUE 1.
      * @field WS-C60-POINT COBOL-60 POINT RIGHT/LEFT scale.
           05  WS-C60-POINT              PIC 9(4) COMP VALUE 0.
      * @record WS-UP-PIC Uppercased PIC string buffer.
       01  WS-UP-PIC                     PIC X(32) VALUE SPACES.
      * @record WS-TOK-WORD Uppercased semantic token text.
       01  WS-TOK-WORD                   PIC X(64) VALUE SPACES.
      * @record WS-NXT-WORD Uppercased lookahead semantic token text.
       01  WS-NXT-WORD                   PIC X(64) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDCPIC Parse PICTURE string or field clause span.
      * @param [in] CDD-BUF Source byte buffer.
      * @param [in] CDD-CTOK-STREAM Tokenized COBOL stream.
      * @param [inout] CDD-CPIC-INFO Input span/PIC and parsed facets.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated scalar kind, digits, scale, usage, & occurs.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-COB-INVALID-PIC,
      *         CDD-ERR-COB-INVALID-OCCURS.
       PROCEDURE DIVISION USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags and dispatch PIC vs clauses.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID
               OR NOT CDD-CTK-TAG-VALID
               OR NOT CDD-CPI-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDCPIC"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           PERFORM RESET-PIC-OUTPUTS
           IF CDD-CPI-OP-PIC-STR
               PERFORM PARSE-STANDALONE-PIC
           ELSE
               PERFORM SCAN-FIELD-CLAUSES
           END-IF
           EXIT PROGRAM.
      * @paragraph RESET-PIC-OUTPUTS Clear output fields before parse.
       RESET-PIC-OUTPUTS.
           MOVE 0 TO CDD-CPI-HAS-PIC
           MOVE 1 TO CDD-CPI-KIND
           MOVE 0 TO CDD-CPI-SIGNED
           MOVE 0 TO CDD-CPI-MAX-LEN
           MOVE 0 TO CDD-CPI-DIGITS
           MOVE 0 TO CDD-CPI-SCALE
           MOVE 0 TO CDD-CPI-USAGE
           MOVE 0 TO CDD-CPI-ARR-MODE
           MOVE 0 TO CDD-CPI-ARR-MIN
           MOVE 0 TO CDD-CPI-ARR-MAX
           MOVE 0 TO CDD-CPI-IS-TYPEDEF
           MOVE 0 TO CDD-CPI-SYNC-JUST
           MOVE 0 TO CDD-CPI-REDEF-LEN
           MOVE SPACES TO CDD-CPI-REDEFINES
           MOVE 0 TO CDD-CPI-VAL-LEN
           MOVE 0 TO CDD-CPI-VAL-IS-STR
           MOVE SPACES TO CDD-CPI-VAL-TEXT
           MOVE 0 TO CDD-CPI-TYPE-TOK1
           MOVE 0 TO CDD-CPI-TYPE-TOK2
           MOVE 0 TO WS-C60-SIZE
           MOVE 1 TO WS-C60-CLASS
           MOVE 0 TO WS-C60-POINT.
      * @paragraph PARSE-STANDALONE-PIC Parse CDD-CPI-PIC-IN string.
       PARSE-STANDALONE-PIC.
           IF CDD-CPI-PIC-IN-LEN = 0 OR CDD-CPI-PIC-IN-LEN > 32
               MOVE 2008 TO CDD-ERR-CODE
               MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
               MOVE "Empty or oversized PICTURE string"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE SPACES TO WS-UP-PIC
               MOVE CDD-CPI-PIC-IN-LEN TO WS-PLEN
               MOVE CDD-CPI-PIC-IN(1:WS-PLEN) TO WS-UP-PIC(1:WS-PLEN)
               INSPECT WS-UP-PIC(1:WS-PLEN)
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               MOVE 1 TO CDD-CPI-HAS-PIC
               PERFORM EVALUATE-PIC-SYMBOLS
           END-IF.
      * @paragraph EVALUATE-PIC-SYMBOLS Walk symbols & '(n)' repeats.
       EVALUATE-PIC-SYMBOLS.
           MOVE 0 TO WS-SEEN-V
           MOVE 0 TO WS-INT-CNT
           MOVE 0 TO WS-FRAC-CNT
           MOVE 0 TO WS-STR-CNT
           MOVE 0 TO WS-PIC-CLASS
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > WS-PLEN OR NOT CDD-OK
               MOVE WS-UP-PIC(WS-I:1) TO WS-SYM
               ADD 1 TO WS-I
               END-ADD
               MOVE 1 TO WS-REP
               IF WS-I <= WS-PLEN AND WS-UP-PIC(WS-I:1) = "("
                   PERFORM PARSE-REPEAT-COUNT
               END-IF
               IF CDD-OK
                   PERFORM APPLY-PIC-SYMBOL
               END-IF
           END-PERFORM
           IF CDD-OK
               PERFORM FINALIZE-PIC-KIND
           END-IF.
      * @paragraph PARSE-REPEAT-COUNT Parse '(n)' multiplier at WS-I.
       PARSE-REPEAT-COUNT.
           ADD 1 TO WS-I
           END-ADD
           MOVE WS-I TO WS-PAREN-START
           PERFORM UNTIL WS-I > WS-PLEN OR WS-UP-PIC(WS-I:1) = ")"
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           IF WS-I > WS-PLEN
               MOVE 2008 TO CDD-ERR-CODE
               MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
               MOVE "Unclosed '(' in PICTURE string"
                   TO CDD-ERR-MESSAGE
           ELSE
               COMPUTE WS-PAREN-LEN = WS-I - WS-PAREN-START
               END-COMPUTE
               ADD 1 TO WS-I
               END-ADD
               MOVE "CDDSRQ01" TO CDD-SRQ-TAG
               MOVE 6 TO CDD-SRQ-OP
               MOVE WS-PAREN-LEN TO CDD-SRQ-IN-LEN
               IF WS-PAREN-LEN > 0
                   MOVE WS-UP-PIC(WS-PAREN-START:WS-PAREN-LEN)
                       TO CDD-SRQ-IN-TEXT(1:WS-PAREN-LEN)
               END-IF
               CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
               END-CALL
               IF NOT CDD-OK OR CDD-SRQ-NUM-VAL = 0
                   MOVE 2008 TO CDD-ERR-CODE
                   MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
                   MOVE "Invalid repeat count in PICTURE string"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE CDD-SRQ-NUM-VAL TO WS-REP
               END-IF
           END-IF.
      * @paragraph APPLY-PIC-SYMBOL Accumulate counts per PIC symbol.
       APPLY-PIC-SYMBOL.
           EVALUATE WS-SYM
               WHEN "X"
               WHEN "A"
                   MOVE 1 TO WS-PIC-CLASS
                   ADD WS-REP TO WS-STR-CNT
                   END-ADD
               WHEN "N"
                   MOVE 2 TO WS-PIC-CLASS
                   ADD WS-REP TO WS-STR-CNT
                   END-ADD
               WHEN "U"
                   MOVE 3 TO WS-PIC-CLASS
                   ADD WS-REP TO WS-STR-CNT
                   END-ADD
               WHEN "1"
                   MOVE 4 TO WS-PIC-CLASS
                   ADD WS-REP TO WS-STR-CNT
                   END-ADD
               WHEN "S"
               WHEN "+"
               WHEN "-"
                   MOVE 1 TO CDD-CPI-SIGNED
               WHEN "V"
               WHEN "."
                   MOVE 1 TO WS-SEEN-V
               WHEN "9"
               WHEN "Z"
               WHEN "$"
               WHEN "P"
                   MOVE 5 TO WS-PIC-CLASS
                   IF WS-SEEN-V = 0
                       IF WS-SYM = "P" AND WS-INT-CNT = 0
                           MOVE 1 TO WS-SEEN-V
                           ADD WS-REP TO WS-FRAC-CNT
                           END-ADD
                       ELSE
                           ADD WS-REP TO WS-INT-CNT
                           END-ADD
                       END-IF
                   ELSE
                       ADD WS-REP TO WS-FRAC-CNT
                       END-ADD
                   END-IF
               WHEN ","
               WHEN "/"
               WHEN "B"
               WHEN "0"
                   CONTINUE
               WHEN OTHER
                   MOVE 2008 TO CDD-ERR-CODE
                   MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
                   MOVE "Unsupported character in PICTURE string"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph FINALIZE-PIC-KIND Map accumulated counts to kind.
       FINALIZE-PIC-KIND.
           EVALUATE WS-PIC-CLASS
               WHEN 1
                   MOVE 2 TO CDD-CPI-KIND
                   MOVE WS-STR-CNT TO CDD-CPI-MAX-LEN
               WHEN 2
                   MOVE 4 TO CDD-CPI-KIND
                   MOVE WS-STR-CNT TO CDD-CPI-MAX-LEN
                   MOVE 10 TO CDD-CPI-USAGE
               WHEN 3
                   MOVE 3 TO CDD-CPI-KIND
                   MOVE WS-STR-CNT TO CDD-CPI-MAX-LEN
                   MOVE 11 TO CDD-CPI-USAGE
               WHEN 4
                   MOVE 9 TO CDD-CPI-KIND
                   MOVE 1 TO CDD-CPI-DIGITS
                   MOVE 9 TO CDD-CPI-USAGE
               WHEN 5
                   COMPUTE CDD-CPI-DIGITS = WS-INT-CNT + WS-FRAC-CNT
                   END-COMPUTE
                   MOVE WS-FRAC-CNT TO CDD-CPI-SCALE
                   IF WS-FRAC-CNT > 0
                       MOVE 6 TO CDD-CPI-KIND
                   ELSE
                       MOVE 5 TO CDD-CPI-KIND
                   END-IF
               WHEN OTHER
                   MOVE 2008 TO CDD-ERR-CODE
                   MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
                   MOVE "PICTURE string lacks data positions"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph LOAD-SEM-TOK Load uppercased word at token WS-TI.
       LOAD-SEM-TOK.
           MOVE SPACES TO WS-TOK-WORD
           IF CDD-CTI-LEN(WS-TI) > 64
               MOVE 64 TO WS-TOK-LEN
           ELSE
               COMPUTE WS-TOK-LEN = CDD-CTI-LEN(WS-TI)
               END-COMPUTE
           END-IF
           MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-TI):WS-TOK-LEN)
               TO WS-TOK-WORD(1:WS-TOK-LEN)
           INSPECT WS-TOK-WORD(1:WS-TOK-LEN)
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
      * @paragraph NEXT-SEM-FROM-TI Find next non-trivia tok > WS-TI.
       NEXT-SEM-FROM-TI.
           COMPUTE WS-NXT-TI = WS-TI + 1
           END-COMPUTE
           PERFORM UNTIL WS-NXT-TI > CDD-CPI-TOK-END
               OR CDD-CTI-CODE-TOK(WS-NXT-TI)
               ADD 1 TO WS-NXT-TI
               END-ADD
           END-PERFORM
           MOVE SPACES TO WS-NXT-WORD
           IF WS-NXT-TI <= CDD-CPI-TOK-END
               IF CDD-CTI-LEN(WS-NXT-TI) > 64
                   MOVE 64 TO WS-TOK-LEN
               ELSE
                   COMPUTE WS-TOK-LEN = CDD-CTI-LEN(WS-NXT-TI)
                   END-COMPUTE
               END-IF
               MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-NXT-TI):WS-TOK-LEN)
                   TO WS-NXT-WORD(1:WS-TOK-LEN)
               INSPECT WS-NXT-WORD(1:WS-TOK-LEN)
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-IF.
      * @paragraph RECORD-TYPE-SPAN Expand [TYPE-TOK1..TYPE-TOK2] span.
       RECORD-TYPE-SPAN.
           IF CDD-CPI-TYPE-TOK1 = 0
               MOVE WS-CLAUSE-START TO CDD-CPI-TYPE-TOK1
           END-IF
           MOVE WS-CLAUSE-END TO CDD-CPI-TYPE-TOK2.
      * @paragraph SCAN-FIELD-CLAUSES Parse clauses in [START..END].
       SCAN-FIELD-CLAUSES.
           MOVE SPACES TO CDD-CPI-PIC-IN
           MOVE 0 TO CDD-CPI-PIC-IN-LEN
           MOVE CDD-CPI-TOK-START TO WS-TI
           PERFORM UNTIL WS-TI > CDD-CPI-TOK-END OR NOT CDD-OK
               IF CDD-CTI-CODE-TOK(WS-TI)
                   PERFORM LOAD-SEM-TOK
                   PERFORM DISPATCH-CLAUSE-TOK
               END-IF
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM
           IF CDD-OK AND WS-C60-SIZE > 0 AND CDD-CPI-HAS-PIC = 0
               PERFORM FINALIZE-COBOL60-SIZE
           END-IF.
      * @paragraph DISPATCH-CLAUSE-TOK Handle PIC, SIZE, USAGE, OCCURS.
       DISPATCH-CLAUSE-TOK.
           EVALUATE WS-TOK-WORD
               WHEN "PIC"
               WHEN "PICTURE"
                   PERFORM HANDLE-PIC-CLAUSE
               WHEN "ANY"
                   PERFORM NEXT-SEM-FROM-TI
                   IF WS-NXT-WORD = "LENGTH"
                       MOVE 0 TO CDD-CPI-MAX-LEN
                       MOVE WS-NXT-TI TO WS-CLAUSE-END
                       PERFORM RECORD-TYPE-SPAN
                       MOVE WS-NXT-TI TO WS-TI
                   END-IF
               WHEN "DYNAMIC"
                   IF CDD-CPI-HAS-PIC = 1
                       AND CDD-CPI-ARR-MODE = 0
                       MOVE 0 TO CDD-CPI-MAX-LEN
                       MOVE WS-TI TO WS-CLAUSE-END
                       PERFORM RECORD-TYPE-SPAN
                   END-IF
               WHEN "SIZE"
                   PERFORM HANDLE-C60-SIZE
               WHEN "CLASS"
                   PERFORM HANDLE-C60-CLASS
               WHEN "POINT"
                   PERFORM HANDLE-C60-POINT
               WHEN "SIGNED"
                   MOVE 1 TO CDD-CPI-SIGNED
               WHEN "USAGE"
                   MOVE WS-TI TO WS-CLAUSE-START
                   PERFORM NEXT-SEM-FROM-TI
                   IF WS-NXT-WORD = "IS"
                       MOVE WS-NXT-TI TO WS-TI
                       PERFORM NEXT-SEM-FROM-TI
                   END-IF
                   IF WS-NXT-TI <= CDD-CPI-TOK-END
                       MOVE WS-NXT-TI TO WS-TI
                       MOVE WS-NXT-WORD TO WS-TOK-WORD
                       PERFORM APPLY-USAGE-WORD
                   END-IF
               WHEN "COMP"
               WHEN "COMPUTATIONAL"
               WHEN "COMP-1"
               WHEN "COMP-2"
               WHEN "COMP-3"
               WHEN "PACKED-DECIMAL"
               WHEN "COMP-4"
               WHEN "COMP-5"
               WHEN "BINARY"
               WHEN "BINARY-CHAR"
               WHEN "BINARY-SHORT"
               WHEN "BINARY-LONG"
               WHEN "BINARY-DOUBLE"
               WHEN "FLOAT-SHORT"
               WHEN "FLOAT-LONG"
               WHEN "FLOAT-EXTENDED"
               WHEN "FLOAT-DECIMAL-16"
               WHEN "FLOAT-DECIMAL-34"
               WHEN "BIT"
               WHEN "NATIONAL"
               WHEN "UTF-8"
                   MOVE WS-TI TO WS-CLAUSE-START
                   PERFORM APPLY-USAGE-WORD
               WHEN "OCCURS"
                   PERFORM HANDLE-OCCURS-CLAUSE
               WHEN "REDEFINES"
               WHEN "RENAMES"
                   PERFORM NEXT-SEM-FROM-TI
                   IF WS-NXT-TI <= CDD-CPI-TOK-END
                       MOVE WS-NXT-WORD TO CDD-CPI-REDEFINES
                       COMPUTE CDD-CPI-REDEF-LEN = WS-TOK-LEN
                       END-COMPUTE
                       MOVE WS-NXT-TI TO WS-TI
                   END-IF
               WHEN "SYNCHRONIZED"
               WHEN "SYNC"
                   IF CDD-CPI-SYNC-JUST < 2
                       MOVE 1 TO CDD-CPI-SYNC-JUST
                   ELSE
                       MOVE 3 TO CDD-CPI-SYNC-JUST
                   END-IF
               WHEN "JUSTIFIED"
               WHEN "JUST"
                   IF CDD-CPI-SYNC-JUST = 1 OR CDD-CPI-SYNC-JUST = 3
                       MOVE 3 TO CDD-CPI-SYNC-JUST
                   ELSE
                       MOVE 2 TO CDD-CPI-SYNC-JUST
                   END-IF
               WHEN "TYPEDEF"
                   MOVE 1 TO CDD-CPI-IS-TYPEDEF
               WHEN "VALUE"
               WHEN "VALUES"
                   PERFORM HANDLE-VALUE-CLAUSE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph HANDLE-PIC-CLAUSE Extract PIC token & parse it.
       HANDLE-PIC-CLAUSE.
           MOVE WS-TI TO WS-CLAUSE-START
           PERFORM NEXT-SEM-FROM-TI
           IF WS-NXT-WORD = "IS"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
           END-IF
           IF WS-NXT-TI > CDD-CPI-TOK-END
               OR CDD-CTI-TOK-DOT(WS-NXT-TI)
               MOVE 2008 TO CDD-ERR-CODE
               MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
               MOVE "Missing specification after PIC/PICTURE"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE WS-NXT-TI TO WS-TI
               MOVE WS-NXT-TI TO WS-CLAUSE-END
               PERFORM RECORD-TYPE-SPAN
               IF CDD-CTI-LEN(WS-TI) > 32
                   MOVE 2008 TO CDD-ERR-CODE
                   MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
                   MOVE "PICTURE string exceeds 32 chars"
                       TO CDD-ERR-MESSAGE
               ELSE
                   COMPUTE CDD-CPI-PIC-IN-LEN = CDD-CTI-LEN(WS-TI)
                   END-COMPUTE
                   MOVE SPACES TO CDD-CPI-PIC-IN
                   MOVE CDD-BUF-DATA(
                       CDD-CTI-OFFSET(WS-TI):CDD-CPI-PIC-IN-LEN)
                       TO CDD-CPI-PIC-IN(1:CDD-CPI-PIC-IN-LEN)
                   PERFORM PARSE-STANDALONE-PIC
               END-IF
           END-IF.
      * @paragraph HANDLE-C60-SIZE Parse COBOL-60 SIZE [IS] <n>.
       HANDLE-C60-SIZE.
           MOVE WS-TI TO WS-CLAUSE-START
           PERFORM NEXT-SEM-FROM-TI
           IF WS-NXT-WORD = "IS"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
           END-IF
           IF WS-NXT-TI <= CDD-CPI-TOK-END
               MOVE "CDDSRQ01" TO CDD-SRQ-TAG
               MOVE 6 TO CDD-SRQ-OP
               MOVE WS-TOK-LEN TO CDD-SRQ-IN-LEN
               MOVE WS-NXT-WORD(1:WS-TOK-LEN)
                   TO CDD-SRQ-IN-TEXT(1:WS-TOK-LEN)
               CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
               END-CALL
               IF CDD-OK
                   MOVE CDD-SRQ-NUM-VAL TO WS-C60-SIZE
                   MOVE WS-NXT-TI TO WS-TI
                   MOVE WS-NXT-TI TO WS-CLAUSE-END
                   PERFORM RECORD-TYPE-SPAN
               ELSE
                   MOVE 0 TO CDD-ERR-CODE
               END-IF
           END-IF.
      * @paragraph HANDLE-C60-CLASS Parse COBOL-60 CLASS [IS] NUM|AN.
       HANDLE-C60-CLASS.
           PERFORM NEXT-SEM-FROM-TI
           IF WS-NXT-WORD = "IS"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
           END-IF
           IF WS-NXT-WORD = "NUMERIC"
               MOVE 2 TO WS-C60-CLASS
               MOVE WS-NXT-TI TO WS-TI
           ELSE
               IF WS-NXT-WORD = "ALPHANUMERIC"
                   OR WS-NXT-WORD = "ALPHABETIC"
                   MOVE 1 TO WS-C60-CLASS
                   MOVE WS-NXT-TI TO WS-TI
               END-IF
           END-IF.
      * @paragraph HANDLE-C60-POINT Parse POINT [LOCATION] [IS] L|R <n>.
       HANDLE-C60-POINT.
           PERFORM NEXT-SEM-FROM-TI
           IF WS-NXT-WORD = "LOCATION"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
           END-IF
           IF WS-NXT-WORD = "IS"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
           END-IF
           IF WS-NXT-WORD = "LEFT" OR WS-NXT-WORD = "RIGHT"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
               IF WS-NXT-TI <= CDD-CPI-TOK-END
                   MOVE "CDDSRQ01" TO CDD-SRQ-TAG
                   MOVE 6 TO CDD-SRQ-OP
                   MOVE WS-TOK-LEN TO CDD-SRQ-IN-LEN
                   MOVE WS-NXT-WORD(1:WS-TOK-LEN)
                       TO CDD-SRQ-IN-TEXT(1:WS-TOK-LEN)
                   CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
                   END-CALL
                   IF CDD-OK
                       COMPUTE WS-C60-POINT = CDD-SRQ-NUM-VAL
                       END-COMPUTE
                       MOVE WS-NXT-TI TO WS-TI
                   ELSE
                       MOVE 0 TO CDD-ERR-CODE
                   END-IF
               END-IF
           END-IF.
      * @paragraph FINALIZE-COBOL60-SIZE Map COBOL-60 SIZE/CLASS/POINT.
       FINALIZE-COBOL60-SIZE.
           MOVE 1 TO CDD-CPI-HAS-PIC
           IF WS-C60-CLASS = 2
               COMPUTE CDD-CPI-DIGITS = WS-C60-SIZE
               END-COMPUTE
               MOVE WS-C60-POINT TO CDD-CPI-SCALE
               IF WS-C60-POINT > 0
                   MOVE 6 TO CDD-CPI-KIND
               ELSE
                   MOVE 5 TO CDD-CPI-KIND
               END-IF
           ELSE
               MOVE 2 TO CDD-CPI-KIND
               MOVE WS-C60-SIZE TO CDD-CPI-MAX-LEN
           END-IF.
      * @paragraph APPLY-USAGE-WORD Map USAGE keyword across 60..2023.
       APPLY-USAGE-WORD.
           MOVE WS-TI TO WS-CLAUSE-END
           EVALUATE WS-TOK-WORD
               WHEN "DISPLAY"
                   MOVE 0 TO CDD-CPI-USAGE
               WHEN "COMP"
               WHEN "COMPUTATIONAL"
               WHEN "COMP-4"
               WHEN "COMP-5"
               WHEN "BINARY"
                   MOVE 1 TO CDD-CPI-USAGE
               WHEN "COMP-3"
               WHEN "PACKED-DECIMAL"
                   MOVE 2 TO CDD-CPI-USAGE
               WHEN "COMP-1"
               WHEN "FLOAT-SHORT"
                   MOVE 3 TO CDD-CPI-USAGE
                   MOVE 7 TO CDD-CPI-KIND
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "COMP-2"
               WHEN "FLOAT-LONG"
               WHEN "FLOAT-EXTENDED"
                   MOVE 4 TO CDD-CPI-USAGE
                   MOVE 7 TO CDD-CPI-KIND
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "BINARY-CHAR"
                   MOVE 5 TO CDD-CPI-USAGE
                   MOVE 5 TO CDD-CPI-KIND
                   MOVE 2 TO CDD-CPI-DIGITS
                   MOVE 1 TO CDD-CPI-SIGNED
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "BINARY-SHORT"
                   MOVE 5 TO CDD-CPI-USAGE
                   MOVE 5 TO CDD-CPI-KIND
                   MOVE 4 TO CDD-CPI-DIGITS
                   MOVE 1 TO CDD-CPI-SIGNED
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "BINARY-LONG"
                   MOVE 5 TO CDD-CPI-USAGE
                   MOVE 5 TO CDD-CPI-KIND
                   MOVE 9 TO CDD-CPI-DIGITS
                   MOVE 1 TO CDD-CPI-SIGNED
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "BINARY-DOUBLE"
                   MOVE 6 TO CDD-CPI-USAGE
                   MOVE 5 TO CDD-CPI-KIND
                   MOVE 18 TO CDD-CPI-DIGITS
                   MOVE 1 TO CDD-CPI-SIGNED
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "FLOAT-DECIMAL-16"
                   MOVE 7 TO CDD-CPI-USAGE
                   MOVE 8 TO CDD-CPI-KIND
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "FLOAT-DECIMAL-34"
                   MOVE 8 TO CDD-CPI-USAGE
                   MOVE 8 TO CDD-CPI-KIND
                   MOVE 1 TO CDD-CPI-HAS-PIC
               WHEN "BIT"
                   MOVE 9 TO CDD-CPI-USAGE
               WHEN "NATIONAL"
                   MOVE 10 TO CDD-CPI-USAGE
               WHEN "UTF-8"
                   MOVE 11 TO CDD-CPI-USAGE
               WHEN "TYPE"
                   PERFORM NEXT-SEM-FROM-TI
                   IF WS-NXT-TI <= CDD-CPI-TOK-END
                       MOVE WS-NXT-TI TO WS-TI
                       MOVE WS-NXT-TI TO WS-CLAUSE-END
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM RECORD-TYPE-SPAN.
      * @paragraph HANDLE-OCCURS-CLAUSE Parse fixed, ODO, or DYNAMIC.
       HANDLE-OCCURS-CLAUSE.
           MOVE WS-TI TO WS-CLAUSE-START
           PERFORM NEXT-SEM-FROM-TI
           IF WS-NXT-TI > CDD-CPI-TOK-END
               MOVE 2009 TO CDD-ERR-CODE
               MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
               MOVE "Missing count or DYNAMIC after OCCURS"
                   TO CDD-ERR-MESSAGE
           ELSE
               IF WS-NXT-WORD = "DYNAMIC"
                   MOVE 3 TO CDD-CPI-ARR-MODE
                   MOVE 0 TO CDD-CPI-ARR-MIN
                   MOVE 0 TO CDD-CPI-ARR-MAX
                   MOVE WS-NXT-TI TO WS-TI
                   MOVE WS-NXT-TI TO WS-CLAUSE-END
                   PERFORM CONSUME-OCCURS-TAIL
                   PERFORM RECORD-TYPE-SPAN
               ELSE
                   PERFORM PARSE-OCCURS-BOUNDS
               END-IF
           END-IF.
      * @paragraph PARSE-OCCURS-BOUNDS Parse <max> or <min> TO <max>.
       PARSE-OCCURS-BOUNDS.
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 6 TO CDD-SRQ-OP
           MOVE WS-TOK-LEN TO CDD-SRQ-IN-LEN
           MOVE WS-NXT-WORD(1:WS-TOK-LEN)
               TO CDD-SRQ-IN-TEXT(1:WS-TOK-LEN)
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               MOVE 2009 TO CDD-ERR-CODE
               MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
               MOVE "Invalid numeric bound in OCCURS clause"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE WS-NXT-TI TO WS-TI
               MOVE WS-NXT-TI TO WS-CLAUSE-END
               COMPUTE CDD-CPI-ARR-MIN = CDD-SRQ-NUM-VAL
               END-COMPUTE
               COMPUTE CDD-CPI-ARR-MAX = CDD-SRQ-NUM-VAL
               END-COMPUTE
               MOVE 1 TO CDD-CPI-ARR-MODE
               PERFORM NEXT-SEM-FROM-TI
               IF WS-NXT-WORD = "TO"
                   MOVE 2 TO CDD-CPI-ARR-MODE
                   MOVE WS-NXT-TI TO WS-TI
                   PERFORM NEXT-SEM-FROM-TI
                   IF WS-NXT-TI > CDD-CPI-TOK-END
                       MOVE 2009 TO CDD-ERR-CODE
                       MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
                       MOVE "Missing max bound after OCCURS TO"
                           TO CDD-ERR-MESSAGE
                   ELSE
                       MOVE WS-TOK-LEN TO CDD-SRQ-IN-LEN
                       MOVE WS-NXT-WORD(1:WS-TOK-LEN)
                           TO CDD-SRQ-IN-TEXT(1:WS-TOK-LEN)
                       CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
                       END-CALL
                       IF NOT CDD-OK
                           OR CDD-SRQ-NUM-VAL < CDD-CPI-ARR-MIN
                           MOVE 2009 TO CDD-ERR-CODE
                           MOVE "CDDCPIC" TO CDD-ERR-SUBPROG
                           MOVE "Invalid OCCURS min TO max range"
                               TO CDD-ERR-MESSAGE
                       ELSE
                           COMPUTE CDD-CPI-ARR-MAX = CDD-SRQ-NUM-VAL
                           END-COMPUTE
                           MOVE WS-NXT-TI TO WS-TI
                           MOVE WS-NXT-TI TO WS-CLAUSE-END
                       END-IF
                   END-IF
               END-IF
               IF CDD-OK
                   PERFORM CONSUME-OCCURS-TAIL
                   PERFORM RECORD-TYPE-SPAN
               END-IF
           END-IF.
      * @paragraph CONSUME-OCCURS-TAIL Consume TIMES/DEPENDING/INDEXED.
       CONSUME-OCCURS-TAIL.
           PERFORM UNTIL WS-TI >= CDD-CPI-TOK-END
               PERFORM NEXT-SEM-FROM-TI
               IF WS-NXT-WORD = "TIMES"
                   OR WS-NXT-WORD = "DEPENDING"
                   OR WS-NXT-WORD = "ON"
                   OR WS-NXT-WORD = "CAPACITY"
                   OR WS-NXT-WORD = "IN"
                   OR WS-NXT-WORD = "INITIALIZED"
                   OR WS-NXT-WORD = "INDEXED"
                   OR WS-NXT-WORD = "BY"
                   MOVE WS-NXT-TI TO WS-TI
                   MOVE WS-NXT-TI TO WS-CLAUSE-END
                   IF WS-NXT-WORD = "DEPENDING"
                       OR WS-NXT-WORD = "CAPACITY"
                       OR WS-NXT-WORD = "BY"
                       PERFORM NEXT-SEM-FROM-TI
                       IF WS-NXT-WORD = "ON" OR WS-NXT-WORD = "IN"
                           MOVE WS-NXT-TI TO WS-TI
                           PERFORM NEXT-SEM-FROM-TI
                       END-IF
                       IF WS-NXT-TI <= CDD-CPI-TOK-END
                           AND NOT CDD-CTI-TOK-DOT(WS-NXT-TI)
                           MOVE WS-NXT-TI TO WS-TI
                           MOVE WS-NXT-TI TO WS-CLAUSE-END
                       END-IF
                   END-IF
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      * @paragraph HANDLE-VALUE-CLAUSE Extract first VALUE literal.
       HANDLE-VALUE-CLAUSE.
           PERFORM NEXT-SEM-FROM-TI
           IF WS-NXT-WORD = "IS" OR WS-NXT-WORD = "ARE"
               MOVE WS-NXT-TI TO WS-TI
               PERFORM NEXT-SEM-FROM-TI
           END-IF
           IF WS-NXT-TI <= CDD-CPI-TOK-END
               AND NOT CDD-CTI-TOK-DOT(WS-NXT-TI)
               MOVE WS-NXT-TI TO WS-TI
               IF CDD-CTI-TOK-STRLIT(WS-TI)
                   MOVE 1 TO CDD-CPI-VAL-IS-STR
                   PERFORM EXTRACT-UNQUOTED-LIT
               ELSE
                   MOVE 0 TO CDD-CPI-VAL-IS-STR
                   PERFORM EXTRACT-WORD-VALUE
               END-IF
           END-IF.
      * @paragraph EXTRACT-UNQUOTED-LIT Strip outer quotes from literal.
       EXTRACT-UNQUOTED-LIT.
           IF CDD-CTI-LEN(WS-TI) > 2
               COMPUTE WS-TOK-LEN = CDD-CTI-LEN(WS-TI) - 2
               END-COMPUTE
               IF WS-TOK-LEN > 64
                   MOVE 64 TO WS-TOK-LEN
               END-IF
               MOVE WS-TOK-LEN TO CDD-CPI-VAL-LEN
               MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-TI) + 1:WS-TOK-LEN)
                   TO CDD-CPI-VAL-TEXT(1:WS-TOK-LEN)
           ELSE
               MOVE 0 TO CDD-CPI-VAL-LEN
               MOVE SPACES TO CDD-CPI-VAL-TEXT
           END-IF.
      * @paragraph EXTRACT-WORD-VALUE Map ZERO/SPACE or numeric literal.
       EXTRACT-WORD-VALUE.
           EVALUATE WS-NXT-WORD
               WHEN "ZERO"
               WHEN "ZEROS"
               WHEN "ZEROES"
                   MOVE "0" TO CDD-CPI-VAL-TEXT
                   MOVE 1 TO CDD-CPI-VAL-LEN
               WHEN "SPACE"
               WHEN "SPACES"
                   MOVE 1 TO CDD-CPI-VAL-IS-STR
                   MOVE 0 TO CDD-CPI-VAL-LEN
                   MOVE SPACES TO CDD-CPI-VAL-TEXT
               WHEN "TRUE"
                   MOVE "true" TO CDD-CPI-VAL-TEXT
                   MOVE 4 TO CDD-CPI-VAL-LEN
               WHEN "FALSE"
                   MOVE "false" TO CDD-CPI-VAL-TEXT
                   MOVE 5 TO CDD-CPI-VAL-LEN
               WHEN OTHER
                   MOVE WS-TOK-LEN TO CDD-CPI-VAL-LEN
                   MOVE CDD-BUF-DATA(
                       CDD-CTI-OFFSET(WS-TI):WS-TOK-LEN)
                       TO CDD-CPI-VAL-TEXT(1:WS-TOK-LEN)
           END-EVALUATE.
