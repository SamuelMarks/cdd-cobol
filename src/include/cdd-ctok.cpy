      * @file cdd-ctok.cpy
      * @brief Lossless COBOL token and trivia stream slice table.
      * @record CDD-CTOK-STREAM Contiguous byte-partitioning stream.
       01  CDD-CTOK-STREAM.
      * @field CDD-CTK-TAG Magic type-safety tag ('CDDCTK01').
           05  CDD-CTK-TAG               PIC X(8) VALUE "CDDCTK01".
      * @field CDD-CTK-TAG-VALID Condition confirming valid CTK tag.
               88  CDD-CTK-TAG-VALID              VALUE "CDDCTK01".
      * @field CDD-CTK-DIALECT Target COBOL standard ID (1..10).
           05  CDD-CTK-DIALECT           PIC 9(2) COMP VALUE 7.
      * @field CDD-CTK-FMT-REQ Requested source format (0=auto).
           05  CDD-CTK-FMT-REQ           PIC 9(2) COMP VALUE 0.
      * @field CDD-CTK-REQ-AUTO Auto-detect fixed vs free format.
               88  CDD-CTK-REQ-AUTO               VALUE 0.
      * @field CDD-CTK-REQ-FIXED Force 80-col fixed reference format.
               88  CDD-CTK-REQ-FIXED              VALUE 1.
      * @field CDD-CTK-REQ-FREE Force COBOL-2002+ free format.
               88  CDD-CTK-REQ-FREE               VALUE 2.
      * @field CDD-CTK-ACT-FMT Active detected/resolved format (1|2).
           05  CDD-CTK-ACT-FMT           PIC 9(2) COMP VALUE 1.
      * @field CDD-CTK-IS-FIXED Stream uses fixed reference format.
               88  CDD-CTK-IS-FIXED               VALUE 1.
      * @field CDD-CTK-IS-FREE Stream uses free source format.
               88  CDD-CTK-IS-FREE                VALUE 2.
      * @field CDD-CTK-COUNT Total emitted token + trivia slices.
           05  CDD-CTK-COUNT             PIC 9(8) COMP VALUE 0.
      * @field CDD-CTK-CAP Maximum slice capacity (8192).
           05  CDD-CTK-CAP               PIC 9(8) COMP VALUE 8192.
      * @field CDD-CTK-ITEM Contiguous lexical and trivia slices.
           05  CDD-CTK-ITEM              OCCURS 8192 TIMES.
      * @field CDD-CTI-IS-TRIVIA 1 if trivia slice, 0 if code token.
               10  CDD-CTI-IS-TRIVIA     PIC 9(2) COMP.
      * @field CDD-CTI-CODE-TOK Semantic COBOL grammar token.
                   88  CDD-CTI-CODE-TOK           VALUE 0.
      * @field CDD-CTI-TRIVIA-TOK Non-semantic trivia slice.
                   88  CDD-CTI-TRIVIA-TOK         VALUE 1.
      * @field CDD-CTI-KIND Discriminated token or trivia kind.
               10  CDD-CTI-KIND          PIC 9(2) COMP.
      * @field CDD-CTI-TRV-SEQ Fixed cols 1..6 sequence area.
                   88  CDD-CTI-TRV-SEQ            VALUE 1.
      * @field CDD-CTI-TRV-IND Fixed col 7 indicator character.
                   88  CDD-CTI-TRV-IND            VALUE 2.
      * @field CDD-CTI-TRV-CMTLINE Fixed col 8..72 comment text.
                   88  CDD-CTI-TRV-CMTLINE        VALUE 3.
      * @field CDD-CTI-TRV-WS Spaces/tabs in code area.
                   88  CDD-CTI-TRV-WS             VALUE 4.
      * @field CDD-CTI-TRV-IDENT Fixed cols 73..80 deck ident tag.
                   88  CDD-CTI-TRV-IDENT          VALUE 5.
      * @field CDD-CTI-TRV-NL LF or CRLF physical line ending.
                   88  CDD-CTI-TRV-NL             VALUE 6.
      * @field CDD-CTI-TRV-DIR '>>' compiler directive line.
                   88  CDD-CTI-TRV-DIR            VALUE 7.
      * @field CDD-CTI-TRV-INLCMT '*>' inline or free comment.
                   88  CDD-CTI-TRV-INLCMT         VALUE 8.
      * @field CDD-CTI-TRV-REMARKS COBOL-60..68 REMARKS body.
                   88  CDD-CTI-TRV-REMARKS        VALUE 9.
      * @field CDD-CTI-TRV-NOTE COBOL-60..68 NOTE sentence body.
                   88  CDD-CTI-TRV-NOTE           VALUE 10.
      * @field CDD-CTI-TOK-WORD Identifier or reserved keyword.
                   88  CDD-CTI-TOK-WORD           VALUE 21.
      * @field CDD-CTI-TOK-LEVEL Data level number (01..49/66/77/88).
                   88  CDD-CTI-TOK-LEVEL          VALUE 22.
      * @field CDD-CTI-TOK-PIC PICTURE character-string specification.
                   88  CDD-CTI-TOK-PIC            VALUE 23.
      * @field CDD-CTI-TOK-NUMLIT Numeric literal value.
                   88  CDD-CTI-TOK-NUMLIT         VALUE 24.
      * @field CDD-CTI-TOK-STRLIT Quoted/prefixed string literal.
                   88  CDD-CTI-TOK-STRLIT         VALUE 25.
      * @field CDD-CTI-TOK-PSEUDO '==...==' pseudo-text literal.
                   88  CDD-CTI-TOK-PSEUDO         VALUE 26.
      * @field CDD-CTI-TOK-DOT Separator period '.'.
                   88  CDD-CTI-TOK-DOT            VALUE 27.
      * @field CDD-CTI-TOK-LPAREN Left parenthesis '('.
                   88  CDD-CTI-TOK-LPAREN         VALUE 28.
      * @field CDD-CTI-TOK-RPAREN Right parenthesis ')'.
                   88  CDD-CTI-TOK-RPAREN         VALUE 29.
      * @field CDD-CTI-TOK-PUNCT Operator or punctuation token.
                   88  CDD-CTI-TOK-PUNCT          VALUE 30.
      * @field CDD-CTI-OFFSET 1-based byte offset in CDD-BUF-DATA.
               10  CDD-CTI-OFFSET        PIC 9(8) COMP.
      * @field CDD-CTI-LEN Byte length of slice (>= 1).
               10  CDD-CTI-LEN           PIC 9(8) COMP.
      * @field CDD-CTI-LINE 1-based physical line number.
               10  CDD-CTI-LINE          PIC 9(8) COMP.
      * @field CDD-CTI-COL 1-based physical column on line.
               10  CDD-CTI-COL           PIC 9(4) COMP.
