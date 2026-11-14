      * @file CDDDIAL.cbl
      * @brief 10-dialect capability matrix and reserved-word registry.
      * @module CDDDIAL
      * @details Provides exact historical grammar capabilities, format
      *          rules, identifier limits, CLI name parsing, and
      *          era-sensitive reserved-word lookup across COBOL-60,
      *          COBOL-61, COBOL-61 Extended, COBOL-65, COBOL-68,
      *          COBOL-74, COBOL-85, COBOL-2002, COBOL-2014, and
      *          COBOL-2023.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDDIAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-WORK Uppercased candidate word buffer.
       01  WS-WORK.
      * @field WS-UPWORD Uppercased copy of CDD-DLR-WORD.
           05  WS-UPWORD                 PIC X(64) VALUE SPACES.
      * @field WS-WLEN Validated length of candidate word.
           05  WS-WLEN                   PIC 9(2) COMP VALUE 0.
       LINKAGE SECTION.
       COPY "cdd-dial.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDDIAL Query 10-dialect capabilities & keywords.
      * @param [inout] CDD-DIAL-REQ Dialect query and output flags.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated capability flags, reserved flag, or enum ID.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-BOUNDS-OUT-OF-RANGE,
      *         CDD-ERR-CLI-BAD-ARGS (5004/5003 on unknown dialect).
       PROCEDURE DIVISION USING CDD-DIAL-REQ CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tag and dispatch registry op.
       MAIN-ENTRY.
           IF NOT CDD-DLR-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDDIAL" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-DIAL-REQ tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           EVALUATE TRUE
               WHEN CDD-DLR-OP-PARSE
                   PERFORM DO-PARSE-DIALECT
               WHEN CDD-DLR-OP-RESERVED
                   PERFORM DO-CHECK-RESERVED
               WHEN OTHER
                   PERFORM DO-FILL-CAPS
           END-EVALUATE
           EXIT PROGRAM.
      * @paragraph PREPARE-WORD Normalize CDD-DLR-WORD to uppercase.
       PREPARE-WORD.
           MOVE SPACES TO WS-UPWORD
           IF CDD-DLR-WORD-LEN > 64
               MOVE 64 TO WS-WLEN
           ELSE
               MOVE CDD-DLR-WORD-LEN TO WS-WLEN
           END-IF
           IF WS-WLEN > 0
               MOVE CDD-DLR-WORD(1:WS-WLEN) TO WS-UPWORD(1:WS-WLEN)
               INSPECT WS-UPWORD(1:WS-WLEN)
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-IF.
      * @paragraph DO-PARSE-DIALECT Map CLI name string to 1..10 ID.
       DO-PARSE-DIALECT.
           PERFORM PREPARE-WORD
           EVALUATE WS-UPWORD
               WHEN "COBOL60"
               WHEN "COBOL-60"
                   MOVE 1 TO CDD-DLR-DIALECT
               WHEN "COBOL61"
               WHEN "COBOL-61"
                   MOVE 2 TO CDD-DLR-DIALECT
               WHEN "COBOL61EXT"
               WHEN "COBOL-61-EXTENDED"
                   MOVE 3 TO CDD-DLR-DIALECT
               WHEN "COBOL65"
               WHEN "COBOL-65"
                   MOVE 4 TO CDD-DLR-DIALECT
               WHEN "COBOL68"
               WHEN "COBOL-68"
                   MOVE 5 TO CDD-DLR-DIALECT
               WHEN "COBOL74"
               WHEN "COBOL-74"
                   MOVE 6 TO CDD-DLR-DIALECT
               WHEN "COBOL85"
               WHEN "COBOL-85"
                   MOVE 7 TO CDD-DLR-DIALECT
               WHEN "COBOL2002"
               WHEN "COBOL-2002"
                   MOVE 8 TO CDD-DLR-DIALECT
               WHEN "COBOL2014"
               WHEN "COBOL-2014"
                   MOVE 9 TO CDD-DLR-DIALECT
               WHEN "COBOL2023"
               WHEN "COBOL-2023"
                   MOVE 10 TO CDD-DLR-DIALECT
               WHEN OTHER
                   MOVE 5003 TO CDD-ERR-CODE
                   MOVE "CDDDIAL" TO CDD-ERR-SUBPROG
                   MOVE "Unknown COBOL dialect name"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE
           IF CDD-OK
               PERFORM DO-FILL-CAPS
           END-IF.
      * @paragraph DO-FILL-CAPS Populate capability flags for 1..10.
       DO-FILL-CAPS.
           IF CDD-DLR-DIALECT < 1 OR CDD-DLR-DIALECT > 10
               MOVE 9002 TO CDD-ERR-CODE
               MOVE "CDDDIAL" TO CDD-ERR-SUBPROG
               MOVE "Dialect ID out of range 1..10"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM RESET-BASE-CAPS
               PERFORM APPLY-ERA-PROGRESSION
           END-IF.
      * @paragraph RESET-BASE-CAPS Initialize baseline COBOL-60 flags.
       RESET-BASE-CAPS.
           MOVE 0 TO CDD-DLR-ALLOW-FREE
           MOVE 1 TO CDD-DLR-HAS-REM-NOTE
           MOVE 0 TO CDD-DLR-HAS-SLASH-C7
           MOVE 0 TO CDD-DLR-HAS-DEBUG-C7
           MOVE 0 TO CDD-DLR-HAS-INLINE-CMT
           MOVE 30 TO CDD-DLR-MAX-IDENT-LEN
           MOVE 1 TO CDD-DLR-HAS-CONST-SEC
           MOVE 0 TO CDD-DLR-HAS-REDEFINES
           MOVE 0 TO CDD-DLR-HAS-RENAMES-66
           MOVE 0 TO CDD-DLR-HAS-INDEXED-BY
           MOVE 0 TO CDD-DLR-HAS-ODO
           MOVE 0 TO CDD-DLR-HAS-GLOBAL
           MOVE 0 TO CDD-DLR-HAS-TYPEDEF-OO
           MOVE 0 TO CDD-DLR-HAS-OCCURS-DYN
           MOVE 0 TO CDD-DLR-HAS-UTF8
           MOVE 0 TO CDD-DLR-HAS-CALL
           MOVE 1 TO CDD-DLR-HAS-EXAMINE
           MOVE 0 TO CDD-DLR-HAS-INSPECT-STR
           MOVE 0 TO CDD-DLR-HAS-SCOPE-RMOD
           MOVE 0 TO CDD-DLR-HAS-UDF-METHOD
           MOVE 0 TO CDD-DLR-HAS-JSON-VERB.
      * @paragraph APPLY-ERA-PROGRESSION Layer capabilities 61..2023.
       APPLY-ERA-PROGRESSION.
           IF CDD-DLR-DIALECT >= 2
               MOVE 1 TO CDD-DLR-HAS-REDEFINES
           END-IF
           IF CDD-DLR-DIALECT >= 3
               MOVE 1 TO CDD-DLR-HAS-RENAMES-66
               MOVE 1 TO CDD-DLR-HAS-CALL
           END-IF
           IF CDD-DLR-DIALECT >= 4
               MOVE 1 TO CDD-DLR-HAS-INDEXED-BY
           END-IF
           IF CDD-DLR-DIALECT >= 5
               MOVE 0 TO CDD-DLR-HAS-CONST-SEC
               MOVE 1 TO CDD-DLR-HAS-DEBUG-C7
           END-IF
           IF CDD-DLR-DIALECT >= 6
               MOVE 0 TO CDD-DLR-HAS-REM-NOTE
               MOVE 1 TO CDD-DLR-HAS-SLASH-C7
               MOVE 1 TO CDD-DLR-HAS-ODO
               MOVE 0 TO CDD-DLR-HAS-EXAMINE
               MOVE 1 TO CDD-DLR-HAS-INSPECT-STR
           END-IF
           IF CDD-DLR-DIALECT >= 7
               MOVE 1 TO CDD-DLR-HAS-GLOBAL
               MOVE 1 TO CDD-DLR-HAS-SCOPE-RMOD
           END-IF
           IF CDD-DLR-DIALECT >= 8
               MOVE 1 TO CDD-DLR-ALLOW-FREE
               MOVE 1 TO CDD-DLR-HAS-INLINE-CMT
               MOVE 63 TO CDD-DLR-MAX-IDENT-LEN
               MOVE 1 TO CDD-DLR-HAS-TYPEDEF-OO
               MOVE 1 TO CDD-DLR-HAS-UDF-METHOD
           END-IF
           IF CDD-DLR-DIALECT >= 9
               MOVE 1 TO CDD-DLR-HAS-OCCURS-DYN
           END-IF
           IF CDD-DIAL-COBOL2023
               MOVE 1 TO CDD-DLR-HAS-UTF8
               MOVE 1 TO CDD-DLR-HAS-JSON-VERB
           END-IF.
      * @paragraph DO-CHECK-RESERVED Check universal + era keywords.
       DO-CHECK-RESERVED.
           MOVE 0 TO CDD-DLR-IS-RESERVED
           IF CDD-DLR-DIALECT < 1 OR CDD-DLR-DIALECT > 10
               MOVE 9002 TO CDD-ERR-CODE
               MOVE "CDDDIAL" TO CDD-ERR-SUBPROG
               MOVE "Dialect ID out of range 1..10"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM PREPARE-WORD
               PERFORM CHECK-CORE-KEYWORDS
               IF CDD-DLR-IS-RESERVED = 0
                   PERFORM CHECK-ERA-KEYWORDS
               END-IF
           END-IF.
      * @paragraph CHECK-CORE-KEYWORDS Universal COBOL reserved words.
       CHECK-CORE-KEYWORDS.
           EVALUATE WS-UPWORD
               WHEN "ACCEPT"
               WHEN "ADD"
               WHEN "ALL"
               WHEN "ALTER"
               WHEN "AND"
               WHEN "ARE"
               WHEN "ASSIGN"
               WHEN "AT"
               WHEN "BY"
               WHEN "CHARACTER"
               WHEN "CHARACTERS"
               WHEN "CLASS"
               WHEN "CLOSE"
               WHEN "COMP"
               WHEN "COMPUTATIONAL"
               WHEN "COMPUTE"
               WHEN "CONFIGURATION"
               WHEN "COPY"
               WHEN "COUNT"
               WHEN "DATA"
               WHEN "DATE"
               WHEN "DEPENDING"
               WHEN "DISPLAY"
               WHEN "DIVIDE"
               WHEN "DIVISION"
               WHEN "ELSE"
               WHEN "END"
               WHEN "ENVIRONMENT"
               WHEN "EQUAL"
               WHEN "EXIT"
               WHEN "FD"
               WHEN "FILE"
               WHEN "FILLER"
               WHEN "FIRST"
               WHEN "FOR"
               WHEN "FROM"
               WHEN "GIVING"
               WHEN "GO"
               WHEN "IF"
               WHEN "IN"
               WHEN "INPUT"
               WHEN "INTO"
               WHEN "IS"
               WHEN "JUSTIFIED"
               WHEN "KEY"
               WHEN "LEADING"
               WHEN "LEFT"
               WHEN "LENGTH"
               WHEN "LESS"
               WHEN "LINKAGE"
               WHEN "MODE"
               WHEN "MOVE"
               WHEN "MULTIPLY"
               WHEN "NEXT"
               WHEN "NO"
               WHEN "NOT"
               WHEN "NUMERIC"
               WHEN "OCCURS"
               WHEN "OF"
               WHEN "ON"
               WHEN "OPEN"
               WHEN "OR"
               WHEN "OTHER"
               WHEN "OUTPUT"
               WHEN "PERFORM"
               WHEN "PIC"
               WHEN "PICTURE"
               WHEN "POINT"
               WHEN "PROCEDURE"
               WHEN "PROGRAM"
               WHEN "PROGRAM-ID"
               WHEN "QUOTE"
               WHEN "QUOTES"
               WHEN "READ"
               WHEN "RECORD"
               WHEN "RECORDS"
               WHEN "REMAINDER"
               WHEN "RETURN"
               WHEN "RIGHT"
               WHEN "ROUNDED"
               WHEN "RUN"
               WHEN "SECTION"
               WHEN "SELECT"
               WHEN "SENTENCE"
               WHEN "SEPARATE"
               WHEN "SIGN"
               WHEN "SIZE"
               WHEN "SPACE"
               WHEN "SPACES"
               WHEN "SPECIAL-NAMES"
               WHEN "STANDARD"
               WHEN "STATUS"
               WHEN "STOP"
               WHEN "SUBTRACT"
               WHEN "SYNCHRONIZED"
               WHEN "TALLY"
               WHEN "TALLYING"
               WHEN "THAN"
               WHEN "THEN"
               WHEN "THROUGH"
               WHEN "THRU"
               WHEN "TIME"
               WHEN "TIMES"
               WHEN "TO"
               WHEN "TRUE"
               WHEN "TYPE"
               WHEN "UNTIL"
               WHEN "UP"
               WHEN "UPON"
               WHEN "USAGE"
               WHEN "USING"
               WHEN "VALUE"
               WHEN "VALUES"
               WHEN "VARYING"
               WHEN "WHEN"
               WHEN "WITH"
               WHEN "WORKING-STORAGE"
               WHEN "WRITE"
               WHEN "ZERO"
               WHEN "ZEROES"
               WHEN "ZEROS"
                   MOVE 1 TO CDD-DLR-IS-RESERVED
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph CHECK-ERA-KEYWORDS Era-scoped COBOL reserved words.
       CHECK-ERA-KEYWORDS.
           EVALUATE WS-UPWORD
               WHEN "CONSTANT"
                   IF CDD-DLR-DIALECT <= 4
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "EXAMINE"
               WHEN "NOTE"
               WHEN "REMARKS"
                   IF CDD-DLR-DIALECT <= 5
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "REDEFINES"
                   IF CDD-DLR-DIALECT >= 2
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "RENAMES"
               WHEN "REPORT"
               WHEN "SD"
               WHEN "SORT"
                   IF CDD-DLR-DIALECT >= 3
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "INDEX"
               WHEN "INDEXED"
               WHEN "SEARCH"
               WHEN "SET"
                   IF CDD-DLR-DIALECT >= 4
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "CALL"
                   IF CDD-DLR-DIALECT >= 3
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "INSPECT"
               WHEN "STRING"
               WHEN "UNSTRING"
                   IF CDD-DLR-DIALECT >= 6
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "EVALUATE"
               WHEN "EXTERNAL"
               WHEN "FUNCTION"
               WHEN "GLOBAL"
               WHEN "INITIALIZE"
               WHEN "REFERENCE"
               WHEN "REPLACE"
                   IF CDD-DLR-DIALECT >= 7
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "ALLOCATE"
               WHEN "BINARY-CHAR"
               WHEN "BINARY-DOUBLE"
               WHEN "BINARY-LONG"
               WHEN "BINARY-SHORT"
               WHEN "BIT"
               WHEN "CLASS-ID"
               WHEN "FACTORY"
               WHEN "FLOAT-LONG"
               WHEN "FLOAT-SHORT"
               WHEN "FREE"
               WHEN "FUNCTION-ID"
               WHEN "GET"
               WHEN "INTERFACE-ID"
               WHEN "INVOKE"
               WHEN "LOCAL-STORAGE"
               WHEN "METHOD"
               WHEN "METHOD-ID"
               WHEN "NATIONAL"
               WHEN "OBJECT"
               WHEN "PROPERTY"
               WHEN "RETURNING"
               WHEN "STRONG"
               WHEN "TYPEDEF"
                   IF CDD-DLR-DIALECT >= 8
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "CAPACITY"
               WHEN "DYNAMIC"
               WHEN "FLOAT-DECIMAL-16"
               WHEN "FLOAT-DECIMAL-34"
                   IF CDD-DLR-DIALECT >= 9
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN "BYTE-LENGTH"
               WHEN "COMMIT"
               WHEN "JSON"
               WHEN "ROLLBACK"
               WHEN "SEND"
               WHEN "UTF-8"
               WHEN "XOR"
                   IF CDD-DIAL-COBOL2023
                       MOVE 1 TO CDD-DLR-IS-RESERVED
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
