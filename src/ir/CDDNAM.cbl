      * @file CDDNAM.cbl
      * @brief Dialect-aware JSON <-> COBOL identifier converter.
      * @module CDDNAM
      * @details Converts arbitrary JSON property keys (camelCase,
      *          snake_case, $schema, 2fa, reserved words) into valid
      *          30-char (COBOL-60..85) or 63-char (COBOL-2002..2023)
      *          COBOL identifiers, and reverses COBOL identifiers to
      *          camelCase, snake_case, kebab-case, or verbatim keys.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDNAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-dial.cpy".
      * @record WS-NAM-STATE Conversion cursors and limits.
       01  WS-NAM-STATE.
      * @field WS-I Input scan index.
           05  WS-I                      PIC 9(4) COMP VALUE 1.
      * @field WS-O Output write index.
           05  WS-O                      PIC 9(4) COMP VALUE 0.
      * @field WS-ILEN Clamped input length.
           05  WS-ILEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-MAX-ID Era max identifier length (26 or 59 base).
           05  WS-MAX-ID                 PIC 9(2) COMP VALUE 26.
      * @field WS-CH Current character.
           05  WS-CH                     PIC X(1) VALUE SPACE.
      * @field WS-PREV-WAS-LOWER 1 if previous char was a..z or 0..9.
           05  WS-PREV-WAS-LOWER         PIC 9(2) COMP VALUE 0.
      * @field WS-LAST-HYPHEN 1 if last emitted char was '-'.
           05  WS-LAST-HYPHEN            PIC 9(2) COMP VALUE 1.
      * @field WS-CAP-NEXT 1 to uppercase next letter in camelCase.
           05  WS-CAP-NEXT               PIC 9(2) COMP VALUE 0.
      * @record WS-WORK-BUF Intermediate identifier scratch buffer.
       01  WS-WORK-BUF                   PIC X(128) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDNAM Convert between JSON keys and COBOL names.
      * @param [inout] CDD-NAM-REQ Conversion direction, dialect, text.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Converted identifier in CDD-NAM-OUT and CDD-NAM-OUT-LEN.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-BOUNDS-OUT-OF-RANGE.
       PROCEDURE DIVISION USING CDD-NAM-REQ CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tag/dialect and dispatch J2C/C2J.
       MAIN-ENTRY.
           IF NOT CDD-NAM-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDNAM" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-NAM-REQ tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE SPACES TO CDD-NAM-OUT
           MOVE 0 TO CDD-NAM-OUT-LEN
           IF CDD-NAM-IN-LEN > 128
               MOVE 128 TO WS-ILEN
           ELSE
               MOVE CDD-NAM-IN-LEN TO WS-ILEN
           END-IF
           IF CDD-NAM-OP-J2C
               PERFORM CONVERT-JSON-TO-COB
           ELSE
               PERFORM CONVERT-COB-TO-JSON
           END-IF
           EXIT PROGRAM.
      * @paragraph CONVERT-JSON-TO-COB Sanitize, bound, escape keyword.
       CONVERT-JSON-TO-COB.
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 1 TO CDD-DLR-OP
           MOVE CDD-NAM-DIALECT TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-OK
               COMPUTE WS-MAX-ID = CDD-DLR-MAX-IDENT-LEN - 4
               END-COMPUTE
               PERFORM SANITIZE-RAW-KEY
               PERFORM TRIM-TRAILING-HYPHENS
               PERFORM ESCAPE-IF-RESERVED
               MOVE WS-O TO CDD-NAM-OUT-LEN
               MOVE WS-WORK-BUF(1:WS-O) TO CDD-NAM-OUT(1:WS-O)
           END-IF.
      * @paragraph SANITIZE-RAW-KEY Map camelCase/symbols to A-Z0-9-.
       SANITIZE-RAW-KEY.
           MOVE SPACES TO WS-WORK-BUF
           MOVE 0 TO WS-O
           MOVE 0 TO WS-PREV-WAS-LOWER
           MOVE 1 TO WS-LAST-HYPHEN
           IF WS-ILEN > 0 AND CDD-NAM-IN(1:1) = "$"
               MOVE "S-" TO WS-WORK-BUF(1:2)
               MOVE 2 TO WS-O
               MOVE 1 TO WS-LAST-HYPHEN
               MOVE 2 TO WS-I
           ELSE
               IF WS-ILEN > 0
                   AND CDD-NAM-IN(1:1) >= "0"
                   AND CDD-NAM-IN(1:1) <= "9"
                   MOVE "N-" TO WS-WORK-BUF(1:2)
                   MOVE 2 TO WS-O
                   MOVE 1 TO WS-LAST-HYPHEN
                   MOVE 1 TO WS-I
               ELSE
                   MOVE 1 TO WS-I
               END-IF
           END-IF
           PERFORM UNTIL WS-I > WS-ILEN OR WS-O >= WS-MAX-ID
               MOVE CDD-NAM-IN(WS-I:1) TO WS-CH
               PERFORM APPEND-SANITIZED-CHAR
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM.
      * @paragraph APPEND-SANITIZED-CHAR Emit uppercased char or '-'.
       APPEND-SANITIZED-CHAR.
           EVALUATE TRUE
               WHEN WS-CH >= "A" AND WS-CH <= "Z"
                   IF WS-PREV-WAS-LOWER = 1
                       AND WS-LAST-HYPHEN = 0
                       AND WS-O + 1 < WS-MAX-ID
                       ADD 1 TO WS-O
                       END-ADD
                       MOVE "-" TO WS-WORK-BUF(WS-O:1)
                   END-IF
                   ADD 1 TO WS-O
                   END-ADD
                   MOVE WS-CH TO WS-WORK-BUF(WS-O:1)
                   MOVE 0 TO WS-PREV-WAS-LOWER
                   MOVE 0 TO WS-LAST-HYPHEN
               WHEN WS-CH >= "a" AND WS-CH <= "z"
                   ADD 1 TO WS-O
                   END-ADD
                   MOVE WS-CH TO WS-WORK-BUF(WS-O:1)
                   INSPECT WS-WORK-BUF(WS-O:1)
                       CONVERTING "abcdefghijklmnopqrstuvwxyz"
                       TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   MOVE 1 TO WS-PREV-WAS-LOWER
                   MOVE 0 TO WS-LAST-HYPHEN
               WHEN WS-CH >= "0" AND WS-CH <= "9"
                   ADD 1 TO WS-O
                   END-ADD
                   MOVE WS-CH TO WS-WORK-BUF(WS-O:1)
                   MOVE 1 TO WS-PREV-WAS-LOWER
                   MOVE 0 TO WS-LAST-HYPHEN
               WHEN OTHER
                   IF WS-LAST-HYPHEN = 0 AND WS-O < WS-MAX-ID
                       ADD 1 TO WS-O
                       END-ADD
                       MOVE "-" TO WS-WORK-BUF(WS-O:1)
                       MOVE 1 TO WS-LAST-HYPHEN
                   END-IF
                   MOVE 0 TO WS-PREV-WAS-LOWER
           END-EVALUATE.
      * @paragraph TRIM-TRAILING-HYPHENS Strip '-' suffix or default.
       TRIM-TRAILING-HYPHENS.
           PERFORM UNTIL WS-O = 0 OR WS-WORK-BUF(WS-O:1) NOT = "-"
               MOVE SPACE TO WS-WORK-BUF(WS-O:1)
               SUBTRACT 1 FROM WS-O
               END-SUBTRACT
           END-PERFORM
           IF WS-O = 0
               MOVE "FLD" TO WS-WORK-BUF(1:3)
               MOVE 3 TO WS-O
           END-IF.
      * @paragraph ESCAPE-IF-RESERVED Append '-FLD' if dialect keyword.
       ESCAPE-IF-RESERVED.
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 2 TO CDD-DLR-OP
           MOVE CDD-NAM-DIALECT TO CDD-DLR-DIALECT
           COMPUTE CDD-DLR-WORD-LEN = WS-O
           END-COMPUTE
           MOVE WS-WORK-BUF(1:WS-O) TO CDD-DLR-WORD
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-DLR-IS-RESERVED = 1
               MOVE "-FLD" TO WS-WORK-BUF(WS-O + 1:4)
               ADD 4 TO WS-O
               END-ADD
           END-IF.
      * @paragraph CONVERT-COB-TO-JSON Map COBOL name to target case.
       CONVERT-COB-TO-JSON.
           IF WS-ILEN > 4
               AND (CDD-NAM-IN(WS-ILEN - 3:4) = "-FLD"
               OR CDD-NAM-IN(WS-ILEN - 3:4) = "-fld")
               SUBTRACT 4 FROM WS-ILEN
               END-SUBTRACT
           END-IF
           IF WS-ILEN = 0
               MOVE "field" TO CDD-NAM-OUT(1:5)
               MOVE 5 TO CDD-NAM-OUT-LEN
           ELSE
               IF CDD-NAM-CASE-PRESERVE
                   MOVE CDD-NAM-IN(1:WS-ILEN) TO WS-WORK-BUF(1:WS-ILEN)
                   MOVE WS-WORK-BUF(1:WS-ILEN)
                       TO CDD-NAM-OUT(1:WS-ILEN)
                   MOVE WS-ILEN TO CDD-NAM-OUT-LEN
               ELSE
                   PERFORM FORMAT-CASED-JSON-KEY
               END-IF
           END-IF.
      * @paragraph FORMAT-CASED-JSON-KEY Emit camel, snake, or kebab.
       FORMAT-CASED-JSON-KEY.
           MOVE SPACES TO WS-WORK-BUF
           MOVE CDD-NAM-IN(1:WS-ILEN) TO WS-WORK-BUF(1:WS-ILEN)
           INSPECT WS-WORK-BUF(1:WS-ILEN)
               CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               TO "abcdefghijklmnopqrstuvwxyz"
           MOVE 0 TO WS-O
           MOVE 0 TO WS-CAP-NEXT
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > WS-ILEN
               MOVE WS-WORK-BUF(WS-I:1) TO WS-CH
               IF WS-CH = "-"
                   EVALUATE TRUE
                       WHEN CDD-NAM-CASE-CAMEL
                           IF WS-O > 0
                               MOVE 1 TO WS-CAP-NEXT
                           END-IF
                       WHEN CDD-NAM-CASE-SNAKE
                           ADD 1 TO WS-O
                           END-ADD
                           MOVE "_" TO CDD-NAM-OUT(WS-O:1)
                       WHEN OTHER
                           ADD 1 TO WS-O
                           END-ADD
                           MOVE "-" TO CDD-NAM-OUT(WS-O:1)
                   END-EVALUATE
               ELSE
                   ADD 1 TO WS-O
                   END-ADD
                   MOVE WS-CH TO CDD-NAM-OUT(WS-O:1)
                   IF WS-CAP-NEXT = 1
                       INSPECT CDD-NAM-OUT(WS-O:1)
                           CONVERTING "abcdefghijklmnopqrstuvwxyz"
                           TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       MOVE 0 TO WS-CAP-NEXT
                   END-IF
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE WS-O TO CDD-NAM-OUT-LEN.
