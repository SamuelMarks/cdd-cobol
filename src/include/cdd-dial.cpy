      * @file cdd-dial.cpy
      * @brief 10-dialect capability matrix & reserved-word envelope.
      * @record CDD-DIAL-REQ Dialect query and capability descriptor.
       01  CDD-DIAL-REQ.
      * @field CDD-DLR-TAG Magic type-safety tag ('CDDDLR01').
           05  CDD-DLR-TAG               PIC X(8) VALUE "CDDDLR01".
      * @field CDD-DLR-TAG-VALID Condition confirming valid DLR tag.
               88  CDD-DLR-TAG-VALID              VALUE "CDDDLR01".
      * @field CDD-DLR-OP Dialect registry operation selector.
           05  CDD-DLR-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-OP-CAPS Populate capability matrix for dialect.
               88  CDD-DLR-OP-CAPS                VALUE 1.
      * @field CDD-DLR-OP-RESERVED Check if CDD-DLR-WORD is reserved.
               88  CDD-DLR-OP-RESERVED            VALUE 2.
      * @field CDD-DLR-OP-PARSE Parse CLI dialect string to enum ID.
               88  CDD-DLR-OP-PARSE               VALUE 3.
      * @field CDD-DLR-DIALECT Target COBOL standard ID (1..10).
           05  CDD-DLR-DIALECT           PIC 9(2) COMP VALUE 7.
      * @field CDD-DIAL-COBOL60 CODASYL COBOL-60 initial standard.
               88  CDD-DIAL-COBOL60               VALUE 1.
      * @field CDD-DIAL-COBOL61 CODASYL COBOL-61 revision.
               88  CDD-DIAL-COBOL61               VALUE 2.
      * @field CDD-DIAL-COBOL61EXT CODASYL COBOL-61 Extended (1962).
               88  CDD-DIAL-COBOL61EXT            VALUE 3.
      * @field CDD-DIAL-COBOL65 CODASYL COBOL-65 edition.
               88  CDD-DIAL-COBOL65               VALUE 4.
      * @field CDD-DIAL-COBOL68 ANSI X3.23-1968 COBOL-68 standard.
               88  CDD-DIAL-COBOL68               VALUE 5.
      * @field CDD-DIAL-COBOL74 ANSI X3.23-1974 COBOL-74 standard.
               88  CDD-DIAL-COBOL74               VALUE 6.
      * @field CDD-DIAL-COBOL85 ANSI X3.23-1985 COBOL-85 standard.
               88  CDD-DIAL-COBOL85               VALUE 7.
      * @field CDD-DIAL-COBOL2002 ISO/IEC 1989:2002 COBOL-2002.
               88  CDD-DIAL-COBOL2002             VALUE 8.
      * @field CDD-DIAL-COBOL2014 ISO/IEC 1989:2014 COBOL-2014.
               88  CDD-DIAL-COBOL2014             VALUE 9.
      * @field CDD-DIAL-COBOL2023 ISO/IEC 1989:2023 COBOL-2023.
               88  CDD-DIAL-COBOL2023             VALUE 10.
      * @field CDD-DLR-WORD-LEN Active length in CDD-DLR-WORD.
           05  CDD-DLR-WORD-LEN          PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-WORD Candidate identifier or dialect name.
           05  CDD-DLR-WORD              PIC X(64) VALUE SPACES.
      * @field CDD-DLR-IS-RESERVED 1 if CDD-DLR-WORD is reserved.
           05  CDD-DLR-IS-RESERVED       PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-ALLOW-FREE 1 if free-format source allowed.
           05  CDD-DLR-ALLOW-FREE        PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-REM-NOTE 1 if REMARKS and NOTE valid.
           05  CDD-DLR-HAS-REM-NOTE      PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-SLASH-C7 1 if '/' page eject in col 7.
           05  CDD-DLR-HAS-SLASH-C7      PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-DEBUG-C7 1 if 'D' debug indicator in col 7.
           05  CDD-DLR-HAS-DEBUG-C7      PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-INLINE-CMT 1 if '*>' inline comment valid.
           05  CDD-DLR-HAS-INLINE-CMT    PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-MAX-IDENT-LEN Max identifier length (30 or 63).
           05  CDD-DLR-MAX-IDENT-LEN     PIC 9(2) COMP VALUE 30.
      * @field CDD-DLR-HAS-CONST-SEC 1 if CONSTANT SECTION exists.
           05  CDD-DLR-HAS-CONST-SEC     PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-REDEFINES 1 if REDEFINES standardized.
           05  CDD-DLR-HAS-REDEFINES     PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-RENAMES-66 1 if 66 RENAMES supported.
           05  CDD-DLR-HAS-RENAMES-66    PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-INDEXED-BY 1 if INDEXED BY / SEARCH valid.
           05  CDD-DLR-HAS-INDEXED-BY    PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-ODO 1 if OCCURS DEPENDING ON supported.
           05  CDD-DLR-HAS-ODO           PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-GLOBAL 1 if GLOBAL / EXTERNAL supported.
           05  CDD-DLR-HAS-GLOBAL        PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-TYPEDEF-OO 1 if TYPEDEF & CLASS-ID valid.
           05  CDD-DLR-HAS-TYPEDEF-OO    PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-OCCURS-DYN 1 if OCCURS DYNAMIC supported.
           05  CDD-DLR-HAS-OCCURS-DYN    PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-UTF8 1 if PIC U UTF-8 supported.
           05  CDD-DLR-HAS-UTF8          PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-CALL 1 if CALL ... USING supported.
           05  CDD-DLR-HAS-CALL          PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-EXAMINE 1 if EXAMINE verb active.
           05  CDD-DLR-HAS-EXAMINE       PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-INSPECT-STR 1 if INSPECT/STRING/UNSTRING.
           05  CDD-DLR-HAS-INSPECT-STR   PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-SCOPE-RMOD 1 if END-* & ref-mod supported.
           05  CDD-DLR-HAS-SCOPE-RMOD    PIC 9(2) COMP VALUE 1.
      * @field CDD-DLR-HAS-UDF-METHOD 1 if FUNCTION-ID & METHOD-ID.
           05  CDD-DLR-HAS-UDF-METHOD    PIC 9(2) COMP VALUE 0.
      * @field CDD-DLR-HAS-JSON-VERB 1 if native JSON GENERATE/PARSE.
           05  CDD-DLR-HAS-JSON-VERB     PIC 9(2) COMP VALUE 0.
