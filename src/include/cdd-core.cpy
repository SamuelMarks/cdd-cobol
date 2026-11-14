      * @file cdd-core.cpy
      * @brief Strongly-typed request envelopes for core subprograms.
      * @record CDD-ERR-REQ Request/response envelope for CDDERRF.
       01  CDD-ERR-REQ.
      * @field CDD-ERQ-TAG Magic type-safety tag ('CDDERQ01').
           05  CDD-ERQ-TAG               PIC X(8) VALUE "CDDERQ01".
      * @field CDD-ERQ-TAG-VALID Condition confirming valid ERQ tag.
               88  CDD-ERQ-TAG-VALID              VALUE "CDDERQ01".
      * @field CDD-ERQ-OP Operation selector for CDDERRF.
           05  CDD-ERQ-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-ERQ-OP-CLEAR Reset CDD-ERROR to CDD-OK state.
               88  CDD-ERQ-OP-CLEAR               VALUE 1.
      * @field CDD-ERQ-OP-SET Populate CDD-ERROR with code and span.
               88  CDD-ERQ-OP-SET                 VALUE 2.
      * @field CDD-ERQ-OP-FORMAT Render human-readable diagnostic.
               88  CDD-ERQ-OP-FORMAT              VALUE 3.
      * @field CDD-ERQ-CODE Error code to assign during OP-SET.
           05  CDD-ERQ-CODE              PIC 9(4) COMP VALUE 0.
      * @field CDD-ERQ-LINE 1-based line number for OP-SET.
           05  CDD-ERQ-LINE              PIC 9(8) COMP VALUE 0.
      * @field CDD-ERQ-COL 1-based column number for OP-SET.
           05  CDD-ERQ-COL               PIC 9(4) COMP VALUE 0.
      * @field CDD-ERQ-SUBPROG 8-char caller subprogram identifier.
           05  CDD-ERQ-SUBPROG           PIC X(8) VALUE SPACES.
      * @field CDD-ERQ-DETAIL Optional caller detail message text.
           05  CDD-ERQ-DETAIL            PIC X(160) VALUE SPACES.
      * @field CDD-ERQ-FMT-LEN Active length of formatted diagnostic.
           05  CDD-ERQ-FMT-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-ERQ-FMT-TEXT Formatted diagnostic output string.
           05  CDD-ERQ-FMT-TEXT          PIC X(320) VALUE SPACES.
      * @record CDD-TYPV-REQ Request envelope for CDDTYPV validator.
       01  CDD-TYPV-REQ.
      * @field CDD-TVR-TAG Magic type-safety tag ('CDDTVR01').
           05  CDD-TVR-TAG               PIC X(8) VALUE "CDDTVR01".
      * @field CDD-TVR-TAG-VALID Condition confirming valid TVR tag.
               88  CDD-TVR-TAG-VALID              VALUE "CDDTVR01".
      * @field CDD-TVR-EXP-TAG Expected 8-char envelope magic tag.
           05  CDD-TVR-EXP-TAG           PIC X(8) VALUE SPACES.
      * @field CDD-TVR-ACT-TAG Actual 8-char envelope magic tag.
           05  CDD-TVR-ACT-TAG           PIC X(8) VALUE SPACES.
      * @field CDD-TVR-CHK-BOUNDS Flag enabling subscript check.
           05  CDD-TVR-CHK-BOUNDS        PIC 9(2) COMP VALUE 0.
      * @field CDD-TVR-BOUNDS-NO Validate tag only without bounds.
               88  CDD-TVR-BOUNDS-NO              VALUE 0.
      * @field CDD-TVR-BOUNDS-YES Validate both tag and subscript.
               88  CDD-TVR-BOUNDS-YES             VALUE 1.
      * @field CDD-TVR-IDX Subscript value to validate.
           05  CDD-TVR-IDX               PIC 9(8) COMP VALUE 0.
      * @field CDD-TVR-MIN Minimum allowed subscript (usually 1).
           05  CDD-TVR-MIN               PIC 9(8) COMP VALUE 1.
      * @field CDD-TVR-MAX Maximum allowed subscript capacity.
           05  CDD-TVR-MAX               PIC 9(8) COMP VALUE 1.
      * @field CDD-TVR-CALLER 8-char caller subprogram name.
           05  CDD-TVR-CALLER            PIC X(8) VALUE SPACES.
      * @record CDD-ARENA-CTL Pool allocation control envelope.
       01  CDD-ARENA-CTL.
      * @field CDD-ARN-TAG Magic type-safety tag ('CDDARN01').
           05  CDD-ARN-TAG               PIC X(8) VALUE "CDDARN01".
      * @field CDD-ARN-TAG-VALID Condition confirming valid ARN tag.
               88  CDD-ARN-TAG-VALID              VALUE "CDDARN01".
      * @field CDD-ARN-OP Pool operation selector.
           05  CDD-ARN-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-ARN-OP-INIT Initialize pool and mark ready.
               88  CDD-ARN-OP-INIT                VALUE 1.
      * @field CDD-ARN-OP-ALLOC Allocate one slot from pool.
               88  CDD-ARN-OP-ALLOC               VALUE 2.
      * @field CDD-ARN-OP-RESET Reset active count to zero.
               88  CDD-ARN-OP-RESET               VALUE 3.
      * @field CDD-ARN-READY Pool initialization state flag.
           05  CDD-ARN-READY             PIC 9(2) COMP VALUE 0.
      * @field CDD-ARN-IS-UNINIT Pool has not been initialized yet.
               88  CDD-ARN-IS-UNINIT              VALUE 0.
      * @field CDD-ARN-IS-READY Pool is initialized and active.
               88  CDD-ARN-IS-READY               VALUE 1.
      * @field CDD-ARN-USED Current number of allocated slots.
           05  CDD-ARN-USED              PIC 9(8) COMP VALUE 0.
      * @field CDD-ARN-CAP Maximum slot capacity of pool.
           05  CDD-ARN-CAP               PIC 9(8) COMP VALUE 0.
      * @field CDD-ARN-OUT-IDX Newly allocated 1-based slot index.
           05  CDD-ARN-OUT-IDX           PIC 9(8) COMP VALUE 0.
      * @field CDD-ARN-CAPACITY-CODE Domain capacity error code.
           05  CDD-ARN-CAPACITY-CODE     PIC 9(4) COMP VALUE 3001.
      * @record CDD-STR-REQ Request/response envelope for CDDSTR.
       01  CDD-STR-REQ.
      * @field CDD-SRQ-TAG Magic type-safety tag ('CDDSRQ01').
           05  CDD-SRQ-TAG               PIC X(8) VALUE "CDDSRQ01".
      * @field CDD-SRQ-TAG-VALID Condition confirming valid SRQ tag.
               88  CDD-SRQ-TAG-VALID              VALUE "CDDSRQ01".
      * @field CDD-SRQ-OP String/numeric operation selector.
           05  CDD-SRQ-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-SRQ-OP-RTRIM Compute length without trailing space.
               88  CDD-SRQ-OP-RTRIM               VALUE 1.
      * @field CDD-SRQ-OP-UPPER Convert ASCII slice to uppercase.
               88  CDD-SRQ-OP-UPPER               VALUE 2.
      * @field CDD-SRQ-OP-LOWER Convert ASCII slice to lowercase.
               88  CDD-SRQ-OP-LOWER               VALUE 3.
      * @field CDD-SRQ-OP-JESC Escape raw bytes into JSON string body.
               88  CDD-SRQ-OP-JESC                VALUE 4.
      * @field CDD-SRQ-OP-JUNESC Unescape JSON string body to raw.
               88  CDD-SRQ-OP-JUNESC              VALUE 5.
      * @field CDD-SRQ-OP-PARSE-U32 Parse unsigned integer from slice.
               88  CDD-SRQ-OP-PARSE-U32           VALUE 6.
      * @field CDD-SRQ-OP-FMT-U32 Format unsigned integer to slice.
               88  CDD-SRQ-OP-FMT-U32             VALUE 7.
      * @field CDD-SRQ-IN-LEN Input byte length (0..1024).
           05  CDD-SRQ-IN-LEN            PIC 9(4) COMP VALUE 0.
      * @field CDD-SRQ-IN-TEXT Input string buffer.
           05  CDD-SRQ-IN-TEXT           PIC X(1024) VALUE SPACES.
      * @field CDD-SRQ-OUT-LEN Output byte length (0..1024).
           05  CDD-SRQ-OUT-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-SRQ-OUT-TEXT Output string buffer.
           05  CDD-SRQ-OUT-TEXT          PIC X(1024) VALUE SPACES.
      * @field CDD-SRQ-NUM-VAL Unsigned 32-bit integer value.
           05  CDD-SRQ-NUM-VAL           PIC 9(8) COMP VALUE 0.
