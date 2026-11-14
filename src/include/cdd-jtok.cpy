      * @file cdd-jtok.cpy
      * @brief Strongly-typed RFC 8259 JSON token stream envelope.
      * @record CDD-JTOK-STREAM Token table produced by CDDJLEX.
       01  CDD-JTOK-STREAM.
      * @field CDD-JTK-TAG Magic type-safety tag ('CDDJTK01').
           05  CDD-JTK-TAG               PIC X(8) VALUE "CDDJTK01".
      * @field CDD-JTK-TAG-VALID Condition confirming valid JTK tag.
               88  CDD-JTK-TAG-VALID              VALUE "CDDJTK01".
      * @field CDD-JTK-COUNT Number of emitted tokens including EOF.
           05  CDD-JTK-COUNT             PIC 9(8) COMP VALUE 0.
      * @field CDD-JTK-CAP Maximum token slot capacity (4096).
           05  CDD-JTK-CAP               PIC 9(8) COMP VALUE 4096.
      * @field CDD-JTK-ITEM Array of lexical JSON tokens.
           05  CDD-JTK-ITEM              OCCURS 4096 TIMES.
      * @field CDD-JTK-KIND Discriminated JSON token kind code.
               10  CDD-JTK-KIND          PIC 9(2) COMP.
      * @field CDD-JTK-IS-LBRACE Left curly brace '{'.
                   88  CDD-JTK-IS-LBRACE          VALUE 1.
      * @field CDD-JTK-IS-RBRACE Right curly brace '}'.
                   88  CDD-JTK-IS-RBRACE          VALUE 2.
      * @field CDD-JTK-IS-LBRACK Left square bracket '['.
                   88  CDD-JTK-IS-LBRACK          VALUE 3.
      * @field CDD-JTK-IS-RBRACK Right square bracket ']'.
                   88  CDD-JTK-IS-RBRACK          VALUE 4.
      * @field CDD-JTK-IS-COLON Key-value separator ':'.
                   88  CDD-JTK-IS-COLON           VALUE 5.
      * @field CDD-JTK-IS-COMMA Element separator ','.
                   88  CDD-JTK-IS-COMMA           VALUE 6.
      * @field CDD-JTK-IS-STRING Quoted JSON string literal.
                   88  CDD-JTK-IS-STRING          VALUE 7.
      * @field CDD-JTK-IS-INTEGER Exact integer JSON number.
                   88  CDD-JTK-IS-INTEGER         VALUE 8.
      * @field CDD-JTK-IS-NUMBER Fractional or exponent JSON number.
                   88  CDD-JTK-IS-NUMBER          VALUE 9.
      * @field CDD-JTK-IS-TRUE Boolean literal 'true'.
                   88  CDD-JTK-IS-TRUE            VALUE 10.
      * @field CDD-JTK-IS-FALSE Boolean literal 'false'.
                   88  CDD-JTK-IS-FALSE           VALUE 11.
      * @field CDD-JTK-IS-NULL Null literal 'null'.
                   88  CDD-JTK-IS-NULL            VALUE 12.
      * @field CDD-JTK-IS-EOF End-of-input sentinel token.
                   88  CDD-JTK-IS-EOF             VALUE 13.
      * @field CDD-JTK-OFFSET 1-based byte offset in CDD-BUF-DATA.
               10  CDD-JTK-OFFSET        PIC 9(8) COMP.
      * @field CDD-JTK-LEN Byte length of token slice.
               10  CDD-JTK-LEN           PIC 9(8) COMP.
      * @field CDD-JTK-LINE 1-based source line of token start.
               10  CDD-JTK-LINE          PIC 9(8) COMP.
      * @field CDD-JTK-COL 1-based source column of token start.
               10  CDD-JTK-COL           PIC 9(4) COMP.
