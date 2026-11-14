      * @file cdd-jsch.cpy
      * @brief Request envelopes for JSON Schema analyzer & emitter.
      * @record CDD-JSCH-REQ Request/response envelope for CDDJSCH.
       01  CDD-JSCH-REQ.
      * @field CDD-JSQ-TAG Magic type-safety tag ('CDDJSQ01').
           05  CDD-JSQ-TAG               PIC X(8) VALUE "CDDJSQ01".
      * @field CDD-JSQ-TAG-VALID Condition confirming valid JSQ tag.
               88  CDD-JSQ-TAG-VALID              VALUE "CDDJSQ01".
      * @field CDD-JSQ-OP Operation selector (detect vs analyze).
           05  CDD-JSQ-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-JSQ-OP-DETECT Auto-detect Schema vs sample JSON.
               88  CDD-JSQ-OP-DETECT              VALUE 1.
      * @field CDD-JSQ-OP-ANALYZE Analyze JSON Schema DOM into CDD-IR.
               88  CDD-JSQ-OP-ANALYZE             VALUE 2.
      * @field CDD-JSQ-IS-SCHEMA Output flag set by OP-DETECT (0 or 1).
           05  CDD-JSQ-IS-SCHEMA         PIC 9(2) COMP VALUE 0.
      * @field CDD-JSQ-DETECTED-SAMPLE DOM is a raw JSON payload.
               88  CDD-JSQ-DETECTED-SAMPLE        VALUE 0.
      * @field CDD-JSQ-DETECTED-SCHEMA DOM is a JSON Schema object.
               88  CDD-JSQ-DETECTED-SCHEMA        VALUE 1.
      * @record CDD-JEMT-REQ Request envelope for CDDJEMT JSON emitter.
       01  CDD-JEMT-REQ.
      * @field CDD-JEM-TAG Magic type-safety tag ('CDDJEM01').
           05  CDD-JEM-TAG               PIC X(8) VALUE "CDDJEM01".
      * @field CDD-JEM-TAG-VALID Condition confirming valid JEM tag.
               88  CDD-JEM-TAG-VALID              VALUE "CDDJEM01".
      * @field CDD-JEM-MODE Target JSON output mode.
           05  CDD-JEM-MODE              PIC 9(2) COMP VALUE 1.
      * @field CDD-JEM-MODE-SCHEMA Emit Draft 2020-12 JSON Schema.
               88  CDD-JEM-MODE-SCHEMA            VALUE 1.
      * @field CDD-JEM-MODE-SAMPLE Emit default/sample JSON payload.
               88  CDD-JEM-MODE-SAMPLE            VALUE 2.
      * @field CDD-JEM-MODE-DOM Emit serialized CDD-JDOM tree.
               88  CDD-JEM-MODE-DOM               VALUE 3.
      * @field CDD-JEM-STYLE Whitespace formatting mode.
           05  CDD-JEM-STYLE             PIC 9(2) COMP VALUE 1.
      * @field CDD-JEM-STYLE-PRETTY Indented 2-space pretty JSON.
               88  CDD-JEM-STYLE-PRETTY           VALUE 1.
      * @field CDD-JEM-STYLE-COMPACT Minimal single-line compact JSON.
               88  CDD-JEM-STYLE-COMPACT          VALUE 2.
      * @field CDD-JEM-X-COBOL Emit x-cobol metadata in JSON Schema.
           05  CDD-JEM-X-COBOL           PIC 9(2) COMP VALUE 0.
      * @field CDD-JEM-XCOB-NO Omit x-cobol vendor extension.
               88  CDD-JEM-XCOB-NO                VALUE 0.
      * @field CDD-JEM-XCOB-YES Include x-cobol vendor extension.
               88  CDD-JEM-XCOB-YES               VALUE 1.
