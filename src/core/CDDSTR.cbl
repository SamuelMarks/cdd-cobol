      * @file CDDSTR.cbl
      * @brief Bounded string, case, JSON escape, and integer helpers.
      * @module CDDSTR
      * @details Implements pure ANSI COBOL-85 string trimming, case
      *          conversion, RFC 8259 JSON escaping/unescaping (with
      *          \uXXXX UTF-8 encoding), and unsigned integer parsing
      *          and formatting with strict capacity checks.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDSTR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-BYTE-MAP Pure COBOL-85 1-byte unsigned/char union.
       01  WS-BYTE-MAP.
      * @field WS-U8 Unsigned byte code (0..255).
           05  WS-U8                     PIC 9(2) COMP VALUE 0.
      * @field WS-CH Single character alias over WS-U8.
           05  WS-CH REDEFINES WS-U8     PIC X(1).
      * @record WS-WORK Internal string processing workspace.
       01  WS-WORK.
      * @field WS-I 1-based input scan index.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-O 1-based output write index.
           05  WS-O                      PIC 9(4) COMP VALUE 0.
      * @field WS-CUR Current character being inspected.
           05  WS-CUR                    PIC X(1) VALUE SPACE.
      * @field WS-NXT Next character for escape dispatch.
           05  WS-NXT                    PIC X(1) VALUE SPACE.
      * @field WS-BYTE-VAL Numeric byte value of WS-CUR.
           05  WS-BYTE-VAL               PIC 9(4) COMP VALUE 0.
      * @field WS-HEX-HI High nibble value for hex escapes.
           05  WS-HEX-HI                 PIC 9(4) COMP VALUE 0.
      * @field WS-HEX-LO Low nibble value for hex escapes.
           05  WS-HEX-LO                 PIC 9(4) COMP VALUE 0.
      * @field WS-CP Unicode codepoint accumulator.
           05  WS-CP                     PIC 9(8) COMP VALUE 0.
      * @field WS-CP2 Low surrogate codepoint accumulator.
           05  WS-CP2                    PIC 9(8) COMP VALUE 0.
      * @field WS-DIGIT Parsed decimal or hex digit (0..15).
           05  WS-DIGIT                  PIC 9(4) COMP VALUE 0.
      * @field WS-HEX-IDX Loop index for 4-digit \uXXXX scan.
           05  WS-HEX-IDX                PIC 9(2) COMP VALUE 0.
      * @field WS-TMP-U32 Temporary integer for formatting digits.
           05  WS-TMP-U32                PIC 9(8) COMP VALUE 0.
      * @field WS-REM Remainder digit during integer formatting.
           05  WS-REM                    PIC 9(2) COMP VALUE 0.
      * @field WS-REV-BUF Reversed digit buffer for FMT-U32.
           05  WS-REV-BUF                PIC X(16) VALUE SPACES.
      * @field WS-REV-LEN Active digit count in WS-REV-BUF.
           05  WS-REV-LEN                PIC 9(2) COMP VALUE 0.
      * @field WS-HEX-CHARS Lookup table of uppercase hex digits.
           05  WS-HEX-CHARS              PIC X(16)
               VALUE "0123456789ABCDEF".
      * @field WS-DEC-ACC64 Overflow-safe accumulator for PARSE-U32.
           05  WS-DEC-ACC64              PIC 9(12) COMP VALUE 0.
      * @field WS-TMP-TEXT Working buffer for non-overlapping copy.
           05  WS-TMP-TEXT               PIC X(1024) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDSTR Execute bounded string/numeric operation.
      * @param [inout] CDD-STR-REQ Operation selector and buffers.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return CDD-SRQ-OUT-TEXT/LEN or CDD-SRQ-NUM-VAL populated.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-BOUNDS-OUT-OF-RANGE,
      *         CDD-ERR-JSON-BAD-ESCAPE, CDD-ERR-JSON-BAD-UNICODE,
      *         CDD-ERR-JSON-BAD-NUMBER, CDD-ERR-GEN-BUF-OVERFLOW.
       PROCEDURE DIVISION USING CDD-STR-REQ CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tag/lengths and dispatch op.
       MAIN-ENTRY.
           IF NOT CDD-SRQ-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-STR-REQ tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           IF CDD-SRQ-IN-LEN > 1024
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9002 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "CDD-SRQ-IN-LEN exceeds 1024" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           EVALUATE TRUE
               WHEN CDD-SRQ-OP-RTRIM
                   PERFORM DO-RTRIM
               WHEN CDD-SRQ-OP-UPPER
                   PERFORM DO-UPPER
               WHEN CDD-SRQ-OP-LOWER
                   PERFORM DO-LOWER
               WHEN CDD-SRQ-OP-JESC
                   PERFORM DO-JSON-ESCAPE
               WHEN CDD-SRQ-OP-JUNESC
                   PERFORM DO-JSON-UNESCAPE
               WHEN CDD-SRQ-OP-PARSE-U32
                   PERFORM DO-PARSE-U32
               WHEN OTHER
                   PERFORM DO-FMT-U32
           END-EVALUATE
           EXIT PROGRAM.
      * @paragraph DO-RTRIM Strip trailing spaces from input slice.
       DO-RTRIM.
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           MOVE CDD-SRQ-IN-LEN TO WS-I
           PERFORM UNTIL WS-I = 0
               OR CDD-SRQ-IN-TEXT(WS-I:1) NOT = SPACE
               SUBTRACT 1 FROM WS-I
               END-SUBTRACT
           END-PERFORM
           MOVE WS-I TO CDD-SRQ-OUT-LEN
           IF WS-I > 0
               MOVE CDD-SRQ-IN-TEXT(1:WS-I) TO WS-TMP-TEXT(1:WS-I)
               MOVE WS-TMP-TEXT(1:WS-I) TO CDD-SRQ-OUT-TEXT(1:WS-I)
           END-IF.
      * @paragraph DO-UPPER Convert ASCII lowercase letters to upper.
       DO-UPPER.
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           MOVE CDD-SRQ-IN-LEN TO CDD-SRQ-OUT-LEN
           IF CDD-SRQ-IN-LEN > 0
               MOVE CDD-SRQ-IN-TEXT(1:CDD-SRQ-IN-LEN)
                   TO WS-TMP-TEXT(1:CDD-SRQ-IN-LEN)
               INSPECT WS-TMP-TEXT(1:CDD-SRQ-IN-LEN)
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               MOVE WS-TMP-TEXT(1:CDD-SRQ-IN-LEN)
                   TO CDD-SRQ-OUT-TEXT(1:CDD-SRQ-IN-LEN)
           END-IF.
      * @paragraph DO-LOWER Convert ASCII uppercase letters to lower.
       DO-LOWER.
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           MOVE CDD-SRQ-IN-LEN TO CDD-SRQ-OUT-LEN
           IF CDD-SRQ-IN-LEN > 0
               MOVE CDD-SRQ-IN-TEXT(1:CDD-SRQ-IN-LEN)
                   TO WS-TMP-TEXT(1:CDD-SRQ-IN-LEN)
               INSPECT WS-TMP-TEXT(1:CDD-SRQ-IN-LEN)
                   CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   TO "abcdefghijklmnopqrstuvwxyz"
               MOVE WS-TMP-TEXT(1:CDD-SRQ-IN-LEN)
                   TO CDD-SRQ-OUT-TEXT(1:CDD-SRQ-IN-LEN)
           END-IF.
      * @paragraph APPEND-OUT Safely append WS-CH to output buffer.
       APPEND-OUT.
           IF WS-O >= 1024
               MOVE 4001 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "String output buffer overflow"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO WS-O
               END-ADD
               MOVE WS-CH TO CDD-SRQ-OUT-TEXT(WS-O:1)
           END-IF.
      * @paragraph DO-JSON-ESCAPE Escape bytes per RFC 8259 rules.
       DO-JSON-ESCAPE.
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           MOVE 0 TO WS-O
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > CDD-SRQ-IN-LEN OR NOT CDD-OK
               MOVE CDD-SRQ-IN-TEXT(WS-I:1) TO WS-CH
               MOVE WS-U8 TO WS-BYTE-VAL
               EVALUATE WS-BYTE-VAL
                   WHEN 34
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE 34 TO WS-U8
                       PERFORM APPEND-OUT
                   WHEN 92
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                   WHEN 8
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE "b" TO WS-CH
                       PERFORM APPEND-OUT
                   WHEN 12
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE "f" TO WS-CH
                       PERFORM APPEND-OUT
                   WHEN 10
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE "n" TO WS-CH
                       PERFORM APPEND-OUT
                   WHEN 13
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE "r" TO WS-CH
                       PERFORM APPEND-OUT
                   WHEN 9
                       MOVE 92 TO WS-U8
                       PERFORM APPEND-OUT
                       MOVE "t" TO WS-CH
                       PERFORM APPEND-OUT
                   WHEN OTHER
                       IF WS-BYTE-VAL < 32
                           PERFORM ESCAPE-CTRL-U00XX
                       ELSE
                           PERFORM APPEND-OUT
                       END-IF
               END-EVALUATE
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE WS-O TO CDD-SRQ-OUT-LEN.
      * @paragraph ESCAPE-CTRL-U00XX Format control char as \u00XX.
       ESCAPE-CTRL-U00XX.
           DIVIDE WS-BYTE-VAL BY 16
               GIVING WS-HEX-HI REMAINDER WS-HEX-LO
           END-DIVIDE
           ADD 1 TO WS-HEX-HI
           END-ADD
           ADD 1 TO WS-HEX-LO
           END-ADD
           MOVE 92 TO WS-U8
           PERFORM APPEND-OUT
           MOVE "u" TO WS-CH
           PERFORM APPEND-OUT
           MOVE "0" TO WS-CH
           PERFORM APPEND-OUT
           MOVE "0" TO WS-CH
           PERFORM APPEND-OUT
           MOVE WS-HEX-CHARS(WS-HEX-HI:1) TO WS-CH
           PERFORM APPEND-OUT
           MOVE WS-HEX-CHARS(WS-HEX-LO:1) TO WS-CH
           PERFORM APPEND-OUT.
      * @paragraph DO-JSON-UNESCAPE Unescape RFC 8259 string body.
       DO-JSON-UNESCAPE.
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           MOVE 0 TO WS-O
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > CDD-SRQ-IN-LEN OR NOT CDD-OK
               MOVE CDD-SRQ-IN-TEXT(WS-I:1) TO WS-CH
               IF WS-U8 = 92
                   PERFORM UNESCAPE-SEQ
               ELSE
                   PERFORM APPEND-OUT
                   ADD 1 TO WS-I
                   END-ADD
               END-IF
           END-PERFORM
           MOVE WS-O TO CDD-SRQ-OUT-LEN.
      * @paragraph UNESCAPE-SEQ Decode single backslash escape sequence.
       UNESCAPE-SEQ.
           IF WS-I >= CDD-SRQ-IN-LEN
               MOVE 1004 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "Trailing backslash in JSON string"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO WS-I
               END-ADD
               MOVE CDD-SRQ-IN-TEXT(WS-I:1) TO WS-NXT
               EVALUATE WS-NXT
                   WHEN '"'
                       MOVE 34 TO WS-U8
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "/"
                       MOVE "/" TO WS-CH
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "b"
                       MOVE 8 TO WS-U8
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "f"
                       MOVE 12 TO WS-U8
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "n"
                       MOVE 10 TO WS-U8
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "r"
                       MOVE 13 TO WS-U8
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "t"
                       MOVE 9 TO WS-U8
                       PERFORM APPEND-OUT
                       ADD 1 TO WS-I
                       END-ADD
                   WHEN "u"
                       PERFORM UNESCAPE-UNICODE
                   WHEN OTHER
                       MOVE WS-NXT TO WS-CH
                       IF WS-U8 = 92
                           PERFORM APPEND-OUT
                           ADD 1 TO WS-I
                           END-ADD
                       ELSE
                           MOVE 1004 TO CDD-ERR-CODE
                           MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                           MOVE "Invalid JSON escape character"
                               TO CDD-ERR-MESSAGE
                       END-IF
               END-EVALUATE
           END-IF.
      * @paragraph PARSE-4HEX Parse 4 hex digits starting at WS-I + 1.
       PARSE-4HEX.
           MOVE 0 TO WS-CP
           IF WS-I + 4 > CDD-SRQ-IN-LEN
               MOVE 1005 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "Truncated unicode escape" TO CDD-ERR-MESSAGE
           ELSE
               MOVE 1 TO WS-HEX-IDX
               PERFORM UNTIL WS-HEX-IDX > 4 OR NOT CDD-OK
                   ADD 1 TO WS-I
                   END-ADD
                   MOVE CDD-SRQ-IN-TEXT(WS-I:1) TO WS-CUR
                   PERFORM HEX-CHAR-TO-VAL
                   IF CDD-OK
                       COMPUTE WS-CP = WS-CP * 16 + WS-DIGIT
                       END-COMPUTE
                   END-IF
                   ADD 1 TO WS-HEX-IDX
                   END-ADD
               END-PERFORM
           END-IF.
      * @paragraph HEX-CHAR-TO-VAL Convert single hex char to 0..15.
       HEX-CHAR-TO-VAL.
           EVALUATE TRUE
               WHEN WS-CUR >= "0" AND WS-CUR <= "9"
                   MOVE WS-CUR TO WS-CH
                   SUBTRACT 48 FROM WS-U8 GIVING WS-DIGIT
                   END-SUBTRACT
               WHEN WS-CUR >= "A" AND WS-CUR <= "F"
                   MOVE WS-CUR TO WS-CH
                   SUBTRACT 55 FROM WS-U8 GIVING WS-DIGIT
                   END-SUBTRACT
               WHEN WS-CUR >= "a" AND WS-CUR <= "f"
                   MOVE WS-CUR TO WS-CH
                   SUBTRACT 87 FROM WS-U8 GIVING WS-DIGIT
                   END-SUBTRACT
               WHEN OTHER
                   MOVE 1005 TO CDD-ERR-CODE
                   MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                   MOVE "Invalid hex digit in unicode escape"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph UNESCAPE-UNICODE Decode \uXXXX and surrogate pairs.
       UNESCAPE-UNICODE.
           PERFORM PARSE-4HEX
           IF CDD-OK
               ADD 1 TO WS-I
               END-ADD
               IF WS-CP >= 55296 AND WS-CP <= 56319
                   PERFORM DECODE-SURROGATE-PAIR
               ELSE
                   IF WS-CP >= 56320 AND WS-CP <= 57343
                       MOVE 1005 TO CDD-ERR-CODE
                       MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                       MOVE "Lone UTF-16 low surrogate"
                           TO CDD-ERR-MESSAGE
                   ELSE
                       PERFORM EMIT-UTF8-CP
                   END-IF
               END-IF
           END-IF.
      * @paragraph DECODE-SURROGATE-PAIR Combine UTF-16 surrogate pair.
       DECODE-SURROGATE-PAIR.
           IF WS-I + 5 > CDD-SRQ-IN-LEN
               MOVE 1005 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "Missing low surrogate escape" TO CDD-ERR-MESSAGE
           ELSE
               MOVE CDD-SRQ-IN-TEXT(WS-I:1) TO WS-CH
               IF WS-U8 NOT = 92
                   OR CDD-SRQ-IN-TEXT(WS-I + 1:1) NOT = "u"
                   MOVE 1005 TO CDD-ERR-CODE
                   MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                   MOVE "Expected \uDC00-\uDFFF low surrogate"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE WS-CP TO WS-CP2
                   ADD 1 TO WS-I
                   END-ADD
                   PERFORM PARSE-4HEX
                   IF CDD-OK
                       ADD 1 TO WS-I
                       END-ADD
                       IF WS-CP < 56320 OR WS-CP > 57343
                           MOVE 1005 TO CDD-ERR-CODE
                           MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                           MOVE "Invalid low surrogate range"
                               TO CDD-ERR-MESSAGE
                       ELSE
                           COMPUTE WS-CP = 65536
                               + ((WS-CP2 - 55296) * 1024)
                               + (WS-CP - 56320)
                           END-COMPUTE
                           PERFORM EMIT-UTF8-CP
                       END-IF
                   END-IF
               END-IF
           END-IF.
      * @paragraph EMIT-UTF8-CP Encode codepoint WS-CP as UTF-8 bytes.
       EMIT-UTF8-CP.
           IF WS-CP <= 127
               COMPUTE WS-U8 = WS-CP
               END-COMPUTE
               PERFORM APPEND-OUT
           ELSE
               IF WS-CP <= 2047
                   COMPUTE WS-U8 = 192 + (WS-CP / 64)
                   END-COMPUTE
                   PERFORM APPEND-OUT
                   COMPUTE WS-U8 = 128 + WS-CP - ((WS-CP / 64) * 64)
                   END-COMPUTE
                   PERFORM APPEND-OUT
               ELSE
                   IF WS-CP <= 65535
                       COMPUTE WS-U8 = 224 + (WS-CP / 4096)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                       COMPUTE WS-TMP-U32 = WS-CP / 64
                       END-COMPUTE
                       COMPUTE WS-U8 = 128 + WS-TMP-U32
                           - ((WS-TMP-U32 / 64) * 64)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                       COMPUTE WS-U8 = 128 + WS-CP
                           - ((WS-CP / 64) * 64)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                   ELSE
                       COMPUTE WS-U8 = 240 + (WS-CP / 262144)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                       COMPUTE WS-TMP-U32 = WS-CP / 4096
                       END-COMPUTE
                       COMPUTE WS-U8 = 128 + WS-TMP-U32
                           - ((WS-TMP-U32 / 64) * 64)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                       COMPUTE WS-TMP-U32 = WS-CP / 64
                       END-COMPUTE
                       COMPUTE WS-U8 = 128 + WS-TMP-U32
                           - ((WS-TMP-U32 / 64) * 64)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                       COMPUTE WS-U8 = 128 + WS-CP
                           - ((WS-CP / 64) * 64)
                       END-COMPUTE
                       PERFORM APPEND-OUT
                   END-IF
               END-IF
           END-IF.
      * @paragraph DO-PARSE-U32 Parse unsigned integer from input slice.
       DO-PARSE-U32.
           MOVE 0 TO CDD-SRQ-NUM-VAL
           MOVE 0 TO WS-DEC-ACC64
           IF CDD-SRQ-IN-LEN = 0
               MOVE 1006 TO CDD-ERR-CODE
               MOVE "CDDSTR" TO CDD-ERR-SUBPROG
               MOVE "Empty unsigned integer string"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE 1 TO WS-I
               PERFORM UNTIL WS-I > CDD-SRQ-IN-LEN OR NOT CDD-OK
                   MOVE CDD-SRQ-IN-TEXT(WS-I:1) TO WS-CUR
                   IF WS-CUR < "0" OR WS-CUR > "9"
                       MOVE 1006 TO CDD-ERR-CODE
                       MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                       MOVE "Non-digit in unsigned integer"
                           TO CDD-ERR-MESSAGE
                   ELSE
                       MOVE WS-CUR TO WS-CH
                       SUBTRACT 48 FROM WS-U8 GIVING WS-DIGIT
                       END-SUBTRACT
                       COMPUTE WS-DEC-ACC64 =
                           (WS-DEC-ACC64 * 10) + WS-DIGIT
                       END-COMPUTE
                       IF WS-DEC-ACC64 > 99999999
                           MOVE 1006 TO CDD-ERR-CODE
                           MOVE "CDDSTR" TO CDD-ERR-SUBPROG
                           MOVE "Unsigned integer overflow"
                               TO CDD-ERR-MESSAGE
                       END-IF
                   END-IF
                   ADD 1 TO WS-I
                   END-ADD
               END-PERFORM
               IF CDD-OK
                   COMPUTE CDD-SRQ-NUM-VAL = WS-DEC-ACC64
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph DO-FMT-U32 Format unsigned integer without padding.
       DO-FMT-U32.
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           IF CDD-SRQ-NUM-VAL = 0
               MOVE "0" TO CDD-SRQ-OUT-TEXT(1:1)
               MOVE 1 TO CDD-SRQ-OUT-LEN
           ELSE
               MOVE CDD-SRQ-NUM-VAL TO WS-TMP-U32
               MOVE 0 TO WS-REV-LEN
               PERFORM UNTIL WS-TMP-U32 = 0
                   DIVIDE WS-TMP-U32 BY 10
                       GIVING WS-TMP-U32 REMAINDER WS-REM
                   END-DIVIDE
                   ADD 48 TO WS-REM GIVING WS-U8
                   END-ADD
                   ADD 1 TO WS-REV-LEN
                   END-ADD
                   MOVE WS-CH TO WS-REV-BUF(WS-REV-LEN:1)
               END-PERFORM
               MOVE WS-REV-LEN TO CDD-SRQ-OUT-LEN
               MOVE 1 TO WS-O
               PERFORM UNTIL WS-REV-LEN = 0
                   MOVE WS-REV-BUF(WS-REV-LEN:1)
                       TO CDD-SRQ-OUT-TEXT(WS-O:1)
                   ADD 1 TO WS-O
                   END-ADD
                   SUBTRACT 1 FROM WS-REV-LEN
                   END-SUBTRACT
               END-PERFORM
           END-IF.
