      * @file cdd-err.cpy
      * @brief Single unified strongly-typed error enum for cdd-cobol.
      * @record CDD-ERROR Unified error envelope across SDK and CLI.
       01  CDD-ERROR.
      * @field CDD-ERR-TAG Magic type-safety tag ('CDDERR01').
           05  CDD-ERR-TAG               PIC X(8) VALUE "CDDERR01".
      * @field CDD-ERR-TAG-VALID Condition confirming valid error tag.
               88  CDD-ERR-TAG-VALID              VALUE "CDDERR01".
      * @field CDD-ERR-CODE Discriminated error enum variant code.
           05  CDD-ERR-CODE              PIC 9(4) COMP VALUE 0.
      * @field CDD-OK Operation succeeded with zero errors.
               88  CDD-OK                         VALUE 0000.
      * @field CDD-ERR-JSON-EOF Unexpected end of JSON input.
               88  CDD-ERR-JSON-EOF               VALUE 1001.
      * @field CDD-ERR-JSON-BAD-TOKEN Unexpected or malformed token.
               88  CDD-ERR-JSON-BAD-TOKEN         VALUE 1002.
      * @field CDD-ERR-JSON-BAD-STRING Unterminated or invalid string.
               88  CDD-ERR-JSON-BAD-STRING        VALUE 1003.
      * @field CDD-ERR-JSON-BAD-ESCAPE Invalid backslash escape.
               88  CDD-ERR-JSON-BAD-ESCAPE        VALUE 1004.
      * @field CDD-ERR-JSON-BAD-UNICODE Invalid uXXXX hex escape.
               88  CDD-ERR-JSON-BAD-UNICODE       VALUE 1005.
      * @field CDD-ERR-JSON-BAD-NUMBER Invalid RFC 8259 number literal.
               88  CDD-ERR-JSON-BAD-NUMBER        VALUE 1006.
      * @field CDD-ERR-JSON-TRAILING-DATA Extra tokens after JSON root.
               88  CDD-ERR-JSON-TRAILING-DATA     VALUE 1007.
      * @field CDD-ERR-JSON-DEPTH-LIMIT JSON nesting exceeded limit.
               88  CDD-ERR-JSON-DEPTH-LIMIT       VALUE 1008.
      * @field CDD-ERR-JSON-DUP-KEY Duplicate object member key found.
               88  CDD-ERR-JSON-DUP-KEY           VALUE 1009.
      * @field CDD-ERR-JSON-CAPACITY JSON DOM node pool exhausted.
               88  CDD-ERR-JSON-CAPACITY          VALUE 1010.
      * @field CDD-ERR-SCHEMA-INVALID-ROOT Invalid JSON Schema root.
               88  CDD-ERR-SCHEMA-INVALID-ROOT    VALUE 1101.
      * @field CDD-ERR-SCHEMA-BAD-TYPE Unsupported JSON Schema type.
               88  CDD-ERR-SCHEMA-BAD-TYPE        VALUE 1102.
      * @field CDD-ERR-SCHEMA-UNRESOLVED-REF Target $ref not found.
               88  CDD-ERR-SCHEMA-UNRESOLVED-REF  VALUE 1103.
      * @field CDD-ERR-SCHEMA-REF-CYCLE Circular $ref detected.
               88  CDD-ERR-SCHEMA-REF-CYCLE       VALUE 1104.
      * @field CDD-ERR-SCHEMA-CONFLICT-BOUNDS Min exceeds max bound.
               88  CDD-ERR-SCHEMA-CONFLICT-BOUNDS VALUE 1105.
      * @field CDD-ERR-INFER-EMPTY-CONFLICT Incompatible sample types.
               88  CDD-ERR-INFER-EMPTY-CONFLICT   VALUE 1106.
      * @field CDD-ERR-COB-BAD-INDICATOR Invalid col-7 indicator char.
               88  CDD-ERR-COB-BAD-INDICATOR      VALUE 2001.
      * @field CDD-ERR-COB-AREA-A-SYNTAX Invalid syntax in Area A.
               88  CDD-ERR-COB-AREA-A-SYNTAX      VALUE 2002.
      * @field CDD-ERR-COB-LINE-OVERFLOW Fixed-format text past col 72.
               88  CDD-ERR-COB-LINE-OVERFLOW      VALUE 2003.
      * @field CDD-ERR-COB-UNCLOSED-LITERAL Unclosed string literal.
               88  CDD-ERR-COB-UNCLOSED-LITERAL   VALUE 2004.
      * @field CDD-ERR-COB-BAD-CONTINUATION Malformed continuation.
               88  CDD-ERR-COB-BAD-CONTINUATION   VALUE 2005.
      * @field CDD-ERR-COB-INVALID-LEVEL Level not in 01-49/66/77/88.
               88  CDD-ERR-COB-INVALID-LEVEL      VALUE 2006.
      * @field CDD-ERR-COB-LEVEL-HIERARCHY Broken level nesting order.
               88  CDD-ERR-COB-LEVEL-HIERARCHY    VALUE 2007.
      * @field CDD-ERR-COB-INVALID-PIC Malformed PICTURE clause.
               88  CDD-ERR-COB-INVALID-PIC        VALUE 2008.
      * @field CDD-ERR-COB-INVALID-OCCURS Malformed OCCURS clause.
               88  CDD-ERR-COB-INVALID-OCCURS     VALUE 2009.
      * @field CDD-ERR-COB-DIALECT-DISALLOWED Feature illegal in era.
               88  CDD-ERR-COB-DIALECT-DISALLOWED VALUE 2010.
      * @field CDD-ERR-COB-CST-CAPACITY COBOL CST pool exhausted.
               88  CDD-ERR-COB-CST-CAPACITY       VALUE 2011.
      * @field CDD-ERR-IR-CAPACITY Canonical IR node pool exhausted.
               88  CDD-ERR-IR-CAPACITY            VALUE 3001.
      * @field CDD-ERR-IR-UNSUPPORTED-MAP Unsupported type mapping.
               88  CDD-ERR-IR-UNSUPPORTED-MAP     VALUE 3002.
      * @field CDD-ERR-IR-NAME-COLLISION Unresolvable field collision.
               88  CDD-ERR-IR-NAME-COLLISION      VALUE 3003.
      * @field CDD-ERR-IR-IDENTIFIER-LEN Identifier exceeds era max.
               88  CDD-ERR-IR-IDENTIFIER-LEN      VALUE 3004.
      * @field CDD-ERR-DIFF-TARGET-MISSING Diff target node missing.
               88  CDD-ERR-DIFF-TARGET-MISSING    VALUE 3005.
      * @field CDD-ERR-GEN-BUF-OVERFLOW Output buffer capacity hit.
               88  CDD-ERR-GEN-BUF-OVERFLOW       VALUE 4001.
      * @field CDD-ERR-GEN-INVALID-UDT-KIND UDT kind invalid for era.
               88  CDD-ERR-GEN-INVALID-UDT-KIND   VALUE 4002.
      * @field CDD-ERR-GEN-INVALID-SERDE-KIND Serde kind invalid.
               88  CDD-ERR-GEN-INVALID-SERDE-KIND VALUE 4003.
      * @field CDD-ERR-SYNC-UDT-NOT-FOUND Target UDT absent in source.
               88  CDD-ERR-SYNC-UDT-NOT-FOUND     VALUE 4004.
      * @field CDD-ERR-SYNC-AMBIGUOUS-UDT Multiple UDTs need --udt.
               88  CDD-ERR-SYNC-AMBIGUOUS-UDT     VALUE 4005.
      * @field CDD-ERR-SYNC-INVARIANT-FAIL CST trivia check failed.
               88  CDD-ERR-SYNC-INVARIANT-FAIL    VALUE 4006.
      * @field CDD-ERR-CLI-NO-SUBCOMMAND Missing CLI subcommand.
               88  CDD-ERR-CLI-NO-SUBCOMMAND      VALUE 5001.
      * @field CDD-ERR-CLI-UNKNOWN-SUBCOMMAND Unknown subcommand name.
               88  CDD-ERR-CLI-UNKNOWN-SUBCOMMAND VALUE 5002.
      * @field CDD-ERR-CLI-UNKNOWN-FLAG Unknown CLI option flag.
               88  CDD-ERR-CLI-UNKNOWN-FLAG       VALUE 5003.
      * @field CDD-ERR-CLI-MISSING-VALUE Option requires an argument.
               88  CDD-ERR-CLI-MISSING-VALUE      VALUE 5004.
      * @field CDD-ERR-CLI-CONFLICT-FLAGS Mutually exclusive flags.
               88  CDD-ERR-CLI-CONFLICT-FLAGS     VALUE 5005.
      * @field CDD-ERR-IO-OPEN-READ Failed opening file for reading.
               88  CDD-ERR-IO-OPEN-READ           VALUE 5006.
      * @field CDD-ERR-IO-OPEN-WRITE Failed opening file for writing.
               88  CDD-ERR-IO-OPEN-WRITE          VALUE 5007.
      * @field CDD-ERR-IO-READ-FAILURE Byte stream read failed.
               88  CDD-ERR-IO-READ-FAILURE        VALUE 5008.
      * @field CDD-ERR-IO-WRITE-FAILURE Byte stream write failed.
               88  CDD-ERR-IO-WRITE-FAILURE       VALUE 5009.
      * @field CDD-ERR-IO-ATOMIC-RENAME Atomic temp swap failed.
               88  CDD-ERR-IO-ATOMIC-RENAME       VALUE 5010.
      * @field CDD-ERR-TYPE-TAG-MISMATCH Envelope tag check failed.
               88  CDD-ERR-TYPE-TAG-MISMATCH      VALUE 9001.
      * @field CDD-ERR-BOUNDS-OUT-OF-RANGE Table subscript out-of-range.
               88  CDD-ERR-BOUNDS-OUT-OF-RANGE    VALUE 9002.
      * @field CDD-ERR-UNINITIALIZED-ARENA Pool used prior to reset.
               88  CDD-ERR-UNINITIALIZED-ARENA    VALUE 9003.
      * @field CDD-ERR-LINE 1-based source line number of failure.
           05  CDD-ERR-LINE              PIC 9(8) COMP VALUE 0.
      * @field CDD-ERR-COL 1-based source column number of failure.
           05  CDD-ERR-COL               PIC 9(4) COMP VALUE 0.
      * @field CDD-ERR-OFFSET 1-based byte offset in input buffer.
           05  CDD-ERR-OFFSET            PIC 9(8) COMP VALUE 0.
      * @field CDD-ERR-DIALECT Active dialect code at error point.
           05  CDD-ERR-DIALECT           PIC 9(2) COMP VALUE 0.
      * @field CDD-ERR-SUBPROG 8-character PROGRAM-ID raising error.
           05  CDD-ERR-SUBPROG           PIC X(8) VALUE SPACES.
      * @field CDD-ERR-MESSAGE Detailed diagnostic message text.
           05  CDD-ERR-MESSAGE           PIC X(256) VALUE SPACES.
