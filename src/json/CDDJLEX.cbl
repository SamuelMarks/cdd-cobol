      * @file CDDJLEX.cbl
      * @brief RFC 8259 compliant JSON lexer with line/col spans.
      * @module CDDJLEX
      * @details Scans raw JSON bytes into CDD-JTOK-STREAM, validating
      *          strings, backslash/unicode escapes (via CDDSTR),
      *          RFC 8259 integer/decimal/exponent numbers, keywords,
      *          and punctuation with 1-based line and column spans.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDJLEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
      * @record WS-BYTE-CONV Pure 2-byte big-endian byte converter.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Two-byte view.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-LEX-STATE Cursor, span, and number/string state.
       01  WS-LEX-STATE.
      * @field WS-POS 1-based byte cursor in CDD-BUF-DATA.
           05  WS-POS                    PIC 9(8) COMP VALUE 1.
      * @field WS-LINE Current 1-based source line.
           05  WS-LINE                   PIC 9(8) COMP VALUE 1.
      * @field WS-COL Current 1-based source column.
           05  WS-COL                    PIC 9(4) COMP VALUE 1.
      * @field WS-TOK-LINE Saved token start line.
           05  WS-TOK-LINE               PIC 9(8) COMP VALUE 1.
      * @field WS-TOK-COL Saved token start column.
           05  WS-TOK-COL                PIC 9(4) COMP VALUE 1.
      * @field WS-TOK-OFF Saved token start byte offset.
           05  WS-TOK-OFF                PIC 9(8) COMP VALUE 1.
      * @field WS-TOK-LEN Saved token byte length.
           05  WS-TOK-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-TOK-KIND Pending token kind to append.
           05  WS-TOK-KIND               PIC 9(2) COMP VALUE 0.
      * @field WS-CH Current byte character.
           05  WS-CH                     PIC X(1) VALUE SPACE.
      * @field WS-BYTE Numeric byte value (0..255) of WS-CH.
           05  WS-BYTE                   PIC 9(4) COMP VALUE 0.
      * @field WS-STR-CLOSED 1 once closing quote '"' encountered.
           05  WS-STR-CLOSED             PIC 9(2) COMP VALUE 0.
      * @field WS-IS-FLOAT 1 if '.' or 'e'/'E' seen in number.
           05  WS-IS-FLOAT               PIC 9(2) COMP VALUE 0.
      * @field WS-DIGITS Digit counter in integer/frac/exp parts.
           05  WS-DIGITS                 PIC 9(4) COMP VALUE 0.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDJLEX Tokenize CDD-BUF JSON text into tokens.
      * @param [in] CDD-BUF Input buffer containing raw JSON bytes.
      * @param [inout] CDD-JTOK-STREAM Output token stream envelope.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-JTOK-STREAM ending with EOF token.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-JSON-BAD-TOKEN,
      *         CDD-ERR-JSON-BAD-STRING, CDD-ERR-JSON-BAD-ESCAPE,
      *         CDD-ERR-JSON-BAD-UNICODE, CDD-ERR-JSON-BAD-NUMBER,
      *         CDD-ERR-JSON-CAPACITY.
       PROCEDURE DIVISION USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate envelopes and scan JSON stream.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID OR NOT CDD-JTK-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-BUF or CDD-JTOK-STREAM tag"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-JTK-COUNT
           MOVE 1 TO WS-POS
           MOVE 1 TO WS-LINE
           MOVE 1 TO WS-COL
           PERFORM UNTIL WS-POS > CDD-BUF-LEN OR NOT CDD-OK
               PERFORM SKIP-WHITESPACE
               IF WS-POS <= CDD-BUF-LEN
                   PERFORM SCAN-NEXT-TOKEN
               END-IF
           END-PERFORM
           IF CDD-OK
               MOVE 13 TO WS-TOK-KIND
               MOVE WS-POS TO WS-TOK-OFF
               MOVE 0 TO WS-TOK-LEN
               MOVE WS-LINE TO WS-TOK-LINE
               MOVE WS-COL TO WS-TOK-COL
               PERFORM PUSH-TOKEN
           END-IF
           EXIT PROGRAM.
      * @paragraph PEEK-BYTE Load CDD-BUF-DATA(WS-POS:1) into WS-BYTE.
       PEEK-BYTE.
           MOVE CDD-BUF-DATA(WS-POS:1) TO WS-CH
           MOVE 0 TO WS-U16
           MOVE WS-CH TO WS-B-LO
           MOVE WS-U16 TO WS-BYTE.
      * @paragraph ADVANCE-CHAR Advance WS-POS and update line/col.
       ADVANCE-CHAR.
           IF WS-BYTE = 10
               ADD 1 TO WS-LINE
               END-ADD
               MOVE 1 TO WS-COL
           ELSE
               ADD 1 TO WS-COL
               END-ADD
           END-IF
           ADD 1 TO WS-POS
           END-ADD.
      * @paragraph SKIP-WHITESPACE Skip RFC 8259 space/tab/CR/LF bytes.
       SKIP-WHITESPACE.
           PERFORM UNTIL WS-POS > CDD-BUF-LEN
               PERFORM PEEK-BYTE
               IF WS-BYTE = 32
                   OR WS-BYTE = 9
                   OR WS-BYTE = 10
                   OR WS-BYTE = 13
                   PERFORM ADVANCE-CHAR
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      * @paragraph PUSH-TOKEN Append token to CDD-JTOK-STREAM safely.
       PUSH-TOKEN.
           IF CDD-JTK-COUNT >= CDD-JTK-CAP
               MOVE 1010 TO CDD-ERR-CODE
               MOVE WS-TOK-LINE TO CDD-ERR-LINE
               MOVE WS-TOK-COL TO CDD-ERR-COL
               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
               MOVE "JSON token stream capacity exceeded"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-JTK-COUNT
               END-ADD
               MOVE WS-TOK-KIND TO CDD-JTK-KIND(CDD-JTK-COUNT)
               MOVE WS-TOK-OFF TO CDD-JTK-OFFSET(CDD-JTK-COUNT)
               MOVE WS-TOK-LEN TO CDD-JTK-LEN(CDD-JTK-COUNT)
               MOVE WS-TOK-LINE TO CDD-JTK-LINE(CDD-JTK-COUNT)
               MOVE WS-TOK-COL TO CDD-JTK-COL(CDD-JTK-COUNT)
           END-IF.
      * @paragraph SCAN-NEXT-TOKEN Dispatch token at WS-POS.
       SCAN-NEXT-TOKEN.
           PERFORM PEEK-BYTE
           MOVE WS-LINE TO WS-TOK-LINE
           MOVE WS-COL TO WS-TOK-COL
           MOVE WS-POS TO WS-TOK-OFF
           EVALUATE WS-CH
               WHEN "{"
                   MOVE 1 TO WS-TOK-KIND
                   MOVE 1 TO WS-TOK-LEN
                   PERFORM ADVANCE-CHAR
                   PERFORM PUSH-TOKEN
               WHEN "}"
                   MOVE 2 TO WS-TOK-KIND
                   MOVE 1 TO WS-TOK-LEN
                   PERFORM ADVANCE-CHAR
                   PERFORM PUSH-TOKEN
               WHEN "["
                   MOVE 3 TO WS-TOK-KIND
                   MOVE 1 TO WS-TOK-LEN
                   PERFORM ADVANCE-CHAR
                   PERFORM PUSH-TOKEN
               WHEN "]"
                   MOVE 4 TO WS-TOK-KIND
                   MOVE 1 TO WS-TOK-LEN
                   PERFORM ADVANCE-CHAR
                   PERFORM PUSH-TOKEN
               WHEN ":"
                   MOVE 5 TO WS-TOK-KIND
                   MOVE 1 TO WS-TOK-LEN
                   PERFORM ADVANCE-CHAR
                   PERFORM PUSH-TOKEN
               WHEN ","
                   MOVE 6 TO WS-TOK-KIND
                   MOVE 1 TO WS-TOK-LEN
                   PERFORM ADVANCE-CHAR
                   PERFORM PUSH-TOKEN
               WHEN '"'
                   PERFORM SCAN-STRING
               WHEN "t"
                   PERFORM SCAN-TRUE
               WHEN "f"
                   PERFORM SCAN-FALSE
               WHEN "n"
                   PERFORM SCAN-NULL
               WHEN "-"
               WHEN "0"
               WHEN "1"
               WHEN "2"
               WHEN "3"
               WHEN "4"
               WHEN "5"
               WHEN "6"
               WHEN "7"
               WHEN "8"
               WHEN "9"
                   PERFORM SCAN-NUMBER
               WHEN OTHER
                   MOVE 1002 TO CDD-ERR-CODE
                   MOVE WS-TOK-LINE TO CDD-ERR-LINE
                   MOVE WS-TOK-COL TO CDD-ERR-COL
                   MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
                   MOVE "Unexpected character in JSON input"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE.
      * @paragraph SCAN-STRING Scan quoted JSON string & check escapes.
       SCAN-STRING.
           PERFORM ADVANCE-CHAR
           MOVE WS-POS TO WS-TOK-OFF
           MOVE 0 TO WS-STR-CLOSED
           PERFORM UNTIL WS-POS > CDD-BUF-LEN
               OR WS-STR-CLOSED = 1
               OR NOT CDD-OK
               PERFORM PEEK-BYTE
               IF WS-BYTE < 32
                   MOVE 1003 TO CDD-ERR-CODE
                   MOVE WS-LINE TO CDD-ERR-LINE
                   MOVE WS-COL TO CDD-ERR-COL
                   MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
                   MOVE "Unescaped control char in JSON string"
                       TO CDD-ERR-MESSAGE
               ELSE
                   IF WS-BYTE = 34
                       COMPUTE WS-TOK-LEN = WS-POS - WS-TOK-OFF
                       END-COMPUTE
                       PERFORM ADVANCE-CHAR
                       MOVE 1 TO WS-STR-CLOSED
                   ELSE
                       IF WS-BYTE = 92
                           PERFORM ADVANCE-CHAR
                           IF WS-POS <= CDD-BUF-LEN
                               PERFORM PEEK-BYTE
                               IF WS-BYTE < 32
                                   MOVE 1004 TO CDD-ERR-CODE
                                   MOVE WS-LINE TO CDD-ERR-LINE
                                   MOVE WS-COL TO CDD-ERR-COL
                                   MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
                                   MOVE "Control char after backslash"
                                       TO CDD-ERR-MESSAGE
                               ELSE
                                   PERFORM ADVANCE-CHAR
                               END-IF
                           ELSE
                               MOVE 1004 TO CDD-ERR-CODE
                               MOVE WS-TOK-LINE TO CDD-ERR-LINE
                               MOVE WS-TOK-COL TO CDD-ERR-COL
                               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
                               MOVE "Unterminated escape at EOF"
                                   TO CDD-ERR-MESSAGE
                           END-IF
                       ELSE
                           PERFORM ADVANCE-CHAR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF CDD-OK
               IF WS-STR-CLOSED = 0
                   MOVE 1003 TO CDD-ERR-CODE
                   MOVE WS-TOK-LINE TO CDD-ERR-LINE
                   MOVE WS-TOK-COL TO CDD-ERR-COL
                   MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
                   MOVE "Unterminated JSON string literal"
                       TO CDD-ERR-MESSAGE
               ELSE
                   PERFORM VALIDATE-STRING-BODY
                   IF CDD-OK
                       MOVE 7 TO WS-TOK-KIND
                       PERFORM PUSH-TOKEN
                   END-IF
               END-IF
           END-IF.
      * @paragraph VALIDATE-STRING-BODY Verify escapes via CDDSTR.
       VALIDATE-STRING-BODY.
           IF WS-TOK-LEN > 1024
               MOVE 1003 TO CDD-ERR-CODE
               MOVE WS-TOK-LINE TO CDD-ERR-LINE
               MOVE WS-TOK-COL TO CDD-ERR-COL
               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
               MOVE "JSON string exceeds 1024 bytes"
                   TO CDD-ERR-MESSAGE
           ELSE
               IF WS-TOK-LEN > 0
                   MOVE "CDDSRQ01" TO CDD-SRQ-TAG
                   MOVE 5 TO CDD-SRQ-OP
                   COMPUTE CDD-SRQ-IN-LEN = WS-TOK-LEN
                   END-COMPUTE
                   MOVE CDD-BUF-DATA(WS-TOK-OFF:WS-TOK-LEN)
                       TO CDD-SRQ-IN-TEXT(1:WS-TOK-LEN)
                   CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
                   END-CALL
                   IF NOT CDD-OK
                       MOVE WS-TOK-LINE TO CDD-ERR-LINE
                       MOVE WS-TOK-COL TO CDD-ERR-COL
                   END-IF
               END-IF
           END-IF.
      * @paragraph SCAN-TRUE Match exact 'true' keyword.
       SCAN-TRUE.
           IF WS-POS + 3 <= CDD-BUF-LEN
               AND CDD-BUF-DATA(WS-POS:4) = "true"
               ADD 4 TO WS-POS
               END-ADD
               ADD 4 TO WS-COL
               END-ADD
               MOVE 10 TO WS-TOK-KIND
               MOVE 4 TO WS-TOK-LEN
               PERFORM PUSH-TOKEN
           ELSE
               MOVE 1002 TO CDD-ERR-CODE
               MOVE WS-TOK-LINE TO CDD-ERR-LINE
               MOVE WS-TOK-COL TO CDD-ERR-COL
               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
               MOVE "Invalid literal starting with 't'"
                   TO CDD-ERR-MESSAGE
           END-IF.
      * @paragraph SCAN-FALSE Match exact 'false' keyword.
       SCAN-FALSE.
           IF WS-POS + 4 <= CDD-BUF-LEN
               AND CDD-BUF-DATA(WS-POS:5) = "false"
               ADD 5 TO WS-POS
               END-ADD
               ADD 5 TO WS-COL
               END-ADD
               MOVE 11 TO WS-TOK-KIND
               MOVE 5 TO WS-TOK-LEN
               PERFORM PUSH-TOKEN
           ELSE
               MOVE 1002 TO CDD-ERR-CODE
               MOVE WS-TOK-LINE TO CDD-ERR-LINE
               MOVE WS-TOK-COL TO CDD-ERR-COL
               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
               MOVE "Invalid literal starting with 'f'"
                   TO CDD-ERR-MESSAGE
           END-IF.
      * @paragraph SCAN-NULL Match exact 'null' keyword.
       SCAN-NULL.
           IF WS-POS + 3 <= CDD-BUF-LEN
               AND CDD-BUF-DATA(WS-POS:4) = "null"
               ADD 4 TO WS-POS
               END-ADD
               ADD 4 TO WS-COL
               END-ADD
               MOVE 12 TO WS-TOK-KIND
               MOVE 4 TO WS-TOK-LEN
               PERFORM PUSH-TOKEN
           ELSE
               MOVE 1002 TO CDD-ERR-CODE
               MOVE WS-TOK-LINE TO CDD-ERR-LINE
               MOVE WS-TOK-COL TO CDD-ERR-COL
               MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
               MOVE "Invalid literal starting with 'n'"
                   TO CDD-ERR-MESSAGE
           END-IF.
      * @paragraph SCAN-NUMBER Scan RFC 8259 integer or float literal.
       SCAN-NUMBER.
           MOVE 0 TO WS-IS-FLOAT
           IF WS-CH = "-"
               PERFORM ADVANCE-CHAR
           END-IF
           IF WS-POS > CDD-BUF-LEN
               PERFORM FAIL-BAD-NUMBER
           ELSE
               PERFORM PEEK-BYTE
               IF WS-CH = "0"
                   PERFORM ADVANCE-CHAR
                   IF WS-POS <= CDD-BUF-LEN
                       PERFORM PEEK-BYTE
                       IF WS-CH >= "0" AND WS-CH <= "9"
                           PERFORM FAIL-BAD-NUMBER
                       END-IF
                   END-IF
               ELSE
                   IF WS-CH >= "1" AND WS-CH <= "9"
                       PERFORM SCAN-DIGIT-RUN
                   ELSE
                       PERFORM FAIL-BAD-NUMBER
                   END-IF
               END-IF
           END-IF
           IF CDD-OK AND WS-POS <= CDD-BUF-LEN
               PERFORM PEEK-BYTE
               IF WS-CH = "."
                   MOVE 1 TO WS-IS-FLOAT
                   PERFORM ADVANCE-CHAR
                   PERFORM SCAN-FRAC-PART
               END-IF
           END-IF
           IF CDD-OK AND WS-POS <= CDD-BUF-LEN
               PERFORM PEEK-BYTE
               IF WS-CH = "e" OR WS-CH = "E"
                   MOVE 1 TO WS-IS-FLOAT
                   PERFORM ADVANCE-CHAR
                   PERFORM SCAN-EXP-PART
               END-IF
           END-IF
           IF CDD-OK
               COMPUTE WS-TOK-LEN = WS-POS - WS-TOK-OFF
               END-COMPUTE
               IF WS-IS-FLOAT = 1
                   MOVE 9 TO WS-TOK-KIND
               ELSE
                   MOVE 8 TO WS-TOK-KIND
               END-IF
               PERFORM PUSH-TOKEN
           END-IF.
      * @paragraph SCAN-DIGIT-RUN Consume 1+ ASCII digits.
       SCAN-DIGIT-RUN.
           MOVE 0 TO WS-DIGITS
           PERFORM UNTIL WS-POS > CDD-BUF-LEN
               PERFORM PEEK-BYTE
               IF WS-CH >= "0" AND WS-CH <= "9"
                   ADD 1 TO WS-DIGITS
                   END-ADD
                   PERFORM ADVANCE-CHAR
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      * @paragraph SCAN-FRAC-PART Require 1+ digits after decimal dot.
       SCAN-FRAC-PART.
           PERFORM SCAN-DIGIT-RUN
           IF WS-DIGITS = 0
               PERFORM FAIL-BAD-NUMBER
           END-IF.
      * @paragraph SCAN-EXP-PART Optional +|- then 1+ exponent digits.
       SCAN-EXP-PART.
           IF WS-POS <= CDD-BUF-LEN
               PERFORM PEEK-BYTE
               IF WS-CH = "+" OR WS-CH = "-"
                   PERFORM ADVANCE-CHAR
               END-IF
           END-IF
           PERFORM SCAN-DIGIT-RUN
           IF WS-DIGITS = 0
               PERFORM FAIL-BAD-NUMBER
           END-IF.
      * @paragraph FAIL-BAD-NUMBER Populate CDD-ERR-JSON-BAD-NUMBER.
       FAIL-BAD-NUMBER.
           MOVE 1006 TO CDD-ERR-CODE
           MOVE WS-TOK-LINE TO CDD-ERR-LINE
           MOVE WS-TOK-COL TO CDD-ERR-COL
           MOVE "CDDJLEX" TO CDD-ERR-SUBPROG
           MOVE "Malformed RFC 8259 JSON number" TO CDD-ERR-MESSAGE.
