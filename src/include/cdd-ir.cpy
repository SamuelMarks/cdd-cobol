      * @file cdd-ir.cpy
      * @brief Canonical Intermediate Representation for UDTs & Schemas.
      * @record CDD-IR Bidirectional schema and COBOL UDT IR container.
       01  CDD-IR.
      * @field CDD-IR-TAG Magic type-safety tag ('CDDIR001').
           05  CDD-IR-TAG                PIC X(8) VALUE "CDDIR001".
      * @field CDD-IR-TAG-VALID Condition confirming valid IR tag.
               88  CDD-IR-TAG-VALID               VALUE "CDDIR001".
      * @field CDD-IR-UDT-KIND Target or parsed COBOL UDT construct.
           05  CDD-IR-UDT-KIND           PIC 9(2) COMP VALUE 1.
      * @field CDD-IR-UDT-RECORD 01-level hierarchical group record.
               88  CDD-IR-UDT-RECORD              VALUE 1.
      * @field CDD-IR-UDT-TYPEDEF COBOL-2002+ IS TYPEDEF STRONG type.
               88  CDD-IR-UDT-TYPEDEF             VALUE 2.
      * @field CDD-IR-UDT-CLASS COBOL-2002+ CLASS-ID OO class.
               88  CDD-IR-UDT-CLASS               VALUE 3.
      * @field CDD-IR-COB-NLEN Length of root COBOL UDT identifier.
           05  CDD-IR-COB-NLEN           PIC 9(2) COMP VALUE 0.
      * @field CDD-IR-COB-NAME Root COBOL UDT name (up to 63 chars).
           05  CDD-IR-COB-NAME           PIC X(64) VALUE SPACES.
      * @field CDD-IR-TITLE-LEN Length of JSON Schema title string.
           05  CDD-IR-TITLE-LEN          PIC 9(4) COMP VALUE 0.
      * @field CDD-IR-JSON-TITLE Original JSON Schema title.
           05  CDD-IR-JSON-TITLE         PIC X(128) VALUE SPACES.
      * @field CDD-IR-DESC-LEN Length of root UDT description.
           05  CDD-IR-DESC-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-IR-DESC Root UDT doc comment / schema description.
           05  CDD-IR-DESC               PIC X(256) VALUE SPACES.
      * @field CDD-IR-ROOT-FIRST First top-level field index (0=none).
           05  CDD-IR-ROOT-FIRST         PIC 9(4) COMP VALUE 0.
      * @field CDD-IR-ROOT-LAST Last top-level field index (0=none).
           05  CDD-IR-ROOT-LAST          PIC 9(4) COMP VALUE 0.
      * @field CDD-IR-FLD-COUNT Active allocated field count.
           05  CDD-IR-FLD-COUNT          PIC 9(4) COMP VALUE 0.
      * @field CDD-IR-FLD-CAP Maximum IR field pool capacity (256).
           05  CDD-IR-FLD-CAP            PIC 9(4) COMP VALUE 256.
      * @field CDD-IR-FIELD Pool of hierarchical UDT/schema fields.
           05  CDD-IR-FIELD              OCCURS 256 TIMES.
      * @field CDD-IRF-PARENT Parent group field index (0=top-level).
               10  CDD-IRF-PARENT        PIC 9(4) COMP.
      * @field CDD-IRF-FIRST-CHILD First child field if GROUP (0=none).
               10  CDD-IRF-FIRST-CHILD   PIC 9(4) COMP.
      * @field CDD-IRF-LAST-CHILD Last child field if GROUP (0=none).
               10  CDD-IRF-LAST-CHILD    PIC 9(4) COMP.
      * @field CDD-IRF-NEXT-SIB Next ordered sibling field (0=none).
               10  CDD-IRF-NEXT-SIB      PIC 9(4) COMP.
      * @field CDD-IRF-JKEY-LEN Length of original JSON property key.
               10  CDD-IRF-JKEY-LEN      PIC 9(4) COMP.
      * @field CDD-IRF-JSON-KEY Original JSON property key name.
               10  CDD-IRF-JSON-KEY      PIC X(128).
      * @field CDD-IRF-CNAM-LEN Length of COBOL field identifier.
               10  CDD-IRF-CNAM-LEN      PIC 9(2) COMP.
      * @field CDD-IRF-COB-NAME Sanitized COBOL field identifier.
               10  CDD-IRF-COB-NAME      PIC X(64).
      * @field CDD-IRF-DESC-LEN Length of field doc description.
               10  CDD-IRF-DESC-LEN      PIC 9(4) COMP.
      * @field CDD-IRF-DESC Field doc comment / schema description.
               10  CDD-IRF-DESC          PIC X(160).
      * @field CDD-IRF-KIND Canonical field data type discriminant.
               10  CDD-IRF-KIND          PIC 9(2) COMP.
      * @field CDD-IRF-IS-GROUP Nested object / COBOL group item.
                   88  CDD-IRF-IS-GROUP           VALUE 1.
      * @field CDD-IRF-IS-STRING Alphanumeric PIC X(n) string.
                   88  CDD-IRF-IS-STRING          VALUE 2.
      * @field CDD-IRF-IS-UTF8 COBOL-2023 PIC U(n) UTF-8 string.
                   88  CDD-IRF-IS-UTF8            VALUE 3.
      * @field CDD-IRF-IS-NATIONAL COBOL-2002+ PIC N(n) UTF-16 string.
                   88  CDD-IRF-IS-NATIONAL        VALUE 4.
      * @field CDD-IRF-IS-INTEGER Exact integer PIC [S]9(p) / BINARY.
                   88  CDD-IRF-IS-INTEGER         VALUE 5.
      * @field CDD-IRF-IS-DECIMAL Fixed-point PIC [S]9(p)V9(s).
                   88  CDD-IRF-IS-DECIMAL         VALUE 6.
      * @field CDD-IRF-IS-FLOAT Binary float COMP-1/COMP-2/FLOAT-*.
                   88  CDD-IRF-IS-FLOAT           VALUE 7.
      * @field CDD-IRF-IS-DECFLOAT IEEE decimal float (COBOL-2014+).
                   88  CDD-IRF-IS-DECFLOAT        VALUE 8.
      * @field CDD-IRF-IS-BOOL Boolean PIC 1 or PIC X + 88 T/F.
                   88  CDD-IRF-IS-BOOL            VALUE 9.
      * @field CDD-IRF-REQUIRED Whether property is in JSON required.
               10  CDD-IRF-REQUIRED      PIC 9(2) COMP.
      * @field CDD-IRF-OPT Property is optional in JSON.
                   88  CDD-IRF-OPT                VALUE 0.
      * @field CDD-IRF-REQ Property is required in JSON.
                   88  CDD-IRF-REQ                VALUE 1.
      * @field CDD-IRF-NULLABLE Whether JSON null is permitted.
               10  CDD-IRF-NULLABLE      PIC 9(2) COMP.
      * @field CDD-IRF-NONNULL Field does not accept null.
                   88  CDD-IRF-NONNULL            VALUE 0.
      * @field CDD-IRF-CAN-NULL Field accepts null union.
                   88  CDD-IRF-CAN-NULL           VALUE 1.
      * @field CDD-IRF-SIGNED Numeric signedness flag.
               10  CDD-IRF-SIGNED        PIC 9(2) COMP.
      * @field CDD-IRF-UNSIGNED Unsigned numeric field.
                   88  CDD-IRF-UNSIGNED           VALUE 0.
      * @field CDD-IRF-IS-SIGNED Signed ('S') numeric field.
                   88  CDD-IRF-IS-SIGNED          VALUE 1.
      * @field CDD-IRF-MIN-LEN Minimum string character length.
               10  CDD-IRF-MIN-LEN       PIC 9(8) COMP.
      * @field CDD-IRF-MAX-LEN Maximum string character length (0=dyn).
               10  CDD-IRF-MAX-LEN       PIC 9(8) COMP.
      * @field CDD-IRF-DIGITS Total decimal digits (integer + scale).
               10  CDD-IRF-DIGITS        PIC 9(4) COMP.
      * @field CDD-IRF-SCALE Digits after implied decimal point V.
               10  CDD-IRF-SCALE         PIC 9(4) COMP.
      * @field CDD-IRF-HAS-BOUNDS 1 if explicit min/max numeric set.
               10  CDD-IRF-HAS-BOUNDS    PIC 9(2) COMP.
      * @field CDD-IRF-MIN-VAL Minimum integer/scaled bound.
               10  CDD-IRF-MIN-VAL       PIC S9(12) COMP.
      * @field CDD-IRF-MAX-VAL Maximum integer/scaled bound.
               10  CDD-IRF-MAX-VAL       PIC S9(12) COMP.
      * @field CDD-IRF-FORMAT JSON Schema / OpenAPI format hint.
               10  CDD-IRF-FORMAT        PIC X(16).
      * @field CDD-IRF-USAGE COBOL USAGE representation code.
               10  CDD-IRF-USAGE         PIC 9(2) COMP.
      * @field CDD-IRF-USG-DISPLAY USAGE DISPLAY (default).
                   88  CDD-IRF-USG-DISPLAY        VALUE 0.
      * @field CDD-IRF-USG-COMP USAGE COMP / BINARY.
                   88  CDD-IRF-USG-COMP           VALUE 1.
      * @field CDD-IRF-USG-COMP3 USAGE COMP-3 / PACKED-DECIMAL.
                   88  CDD-IRF-USG-COMP3          VALUE 2.
      * @field CDD-IRF-USG-COMP1 USAGE COMP-1 / FLOAT-SHORT.
                   88  CDD-IRF-USG-COMP1          VALUE 3.
      * @field CDD-IRF-USG-COMP2 USAGE COMP-2 / FLOAT-LONG.
                   88  CDD-IRF-USG-COMP2          VALUE 4.
      * @field CDD-IRF-USG-BINLONG COBOL-2002+ BINARY-LONG.
                   88  CDD-IRF-USG-BINLONG        VALUE 5.
      * @field CDD-IRF-USG-BINDBL COBOL-2002+ BINARY-DOUBLE.
                   88  CDD-IRF-USG-BINDBL         VALUE 6.
      * @field CDD-IRF-USG-DEC16 COBOL-2014+ FLOAT-DECIMAL-16.
                   88  CDD-IRF-USG-DEC16          VALUE 7.
      * @field CDD-IRF-USG-DEC34 COBOL-2014+ FLOAT-DECIMAL-34.
                   88  CDD-IRF-USG-DEC34          VALUE 8.
      * @field CDD-IRF-USG-BIT COBOL-2002+ BIT boolean.
                   88  CDD-IRF-USG-BIT            VALUE 9.
      * @field CDD-IRF-USG-NAT COBOL-2002+ NATIONAL.
                   88  CDD-IRF-USG-NAT            VALUE 10.
      * @field CDD-IRF-USG-UTF8 COBOL-2023 UTF-8.
                   88  CDD-IRF-USG-UTF8           VALUE 11.
      * @field CDD-IRF-PIC-LEN Active length of CDD-IRF-PIC-TEXT.
               10  CDD-IRF-PIC-LEN       PIC 9(2) COMP.
      * @field CDD-IRF-PIC-TEXT Explicit COBOL PIC string if parsed.
               10  CDD-IRF-PIC-TEXT      PIC X(32).
      * @field CDD-IRF-COB-LEVEL Original COBOL level number (02..77).
               10  CDD-IRF-COB-LEVEL     PIC 9(2) COMP.
      * @field CDD-IRF-REDEF-LEN Length of REDEFINES target name.
               10  CDD-IRF-REDEF-LEN     PIC 9(2) COMP.
      * @field CDD-IRF-REDEFINES Target field if REDEFINES/RENAMES.
               10  CDD-IRF-REDEFINES     PIC X(64).
      * @field CDD-IRF-SYNC-JUST SYNCHRONIZED (1) / JUSTIFIED (2) bits.
               10  CDD-IRF-SYNC-JUST     PIC 9(2) COMP.
      * @field CDD-IRF-DEF-LEN Active length of default literal.
               10  CDD-IRF-DEF-LEN       PIC 9(4) COMP.
      * @field CDD-IRF-DEF-VAL Default VALUE or sample JSON literal.
               10  CDD-IRF-DEF-VAL       PIC X(64).
      * @field CDD-IRF-ARR-MODE Array cardinality mode.
               10  CDD-IRF-ARR-MODE      PIC 9(2) COMP.
      * @field CDD-IRF-ARR-NONE Scalar or non-array group field.
                   88  CDD-IRF-ARR-NONE           VALUE 0.
      * @field CDD-IRF-ARR-FIXED Fixed OCCURS n TIMES.
                   88  CDD-IRF-ARR-FIXED          VALUE 1.
      * @field CDD-IRF-ARR-ODO Variable OCCURS DEPENDING ON.
                   88  CDD-IRF-ARR-ODO            VALUE 2.
      * @field CDD-IRF-ARR-DYNAMIC COBOL-2014+ OCCURS DYNAMIC.
                   88  CDD-IRF-ARR-DYNAMIC        VALUE 3.
      * @field CDD-IRF-ARR-MIN Minimum array item count.
               10  CDD-IRF-ARR-MIN       PIC 9(6) COMP.
      * @field CDD-IRF-ARR-MAX Maximum array item count.
               10  CDD-IRF-ARR-MAX       PIC 9(6) COMP.
      * @field CDD-IRF-ENUM-FIRST Starting index in CDD-IR-ENUM (0=no).
               10  CDD-IRF-ENUM-FIRST    PIC 9(4) COMP.
      * @field CDD-IRF-ENUM-COUNT Number of enum values for field.
               10  CDD-IRF-ENUM-COUNT    PIC 9(4) COMP.
      * @field CDD-IRF-KEEP-HELPER 1 if marked * @cdd-keep in COBOL.
               10  CDD-IRF-KEEP-HELPER   PIC 9(2) COMP.
      * @field CDD-IR-ENUM-CNT Active count of 88/enum entries.
           05  CDD-IR-ENUM-CNT           PIC 9(4) COMP VALUE 0.
      * @field CDD-IR-ENUM-CAP Capacity of enum pool (256).
           05  CDD-IR-ENUM-CAP           PIC 9(4) COMP VALUE 256.
      * @field CDD-IR-ENUM Pool of 88-level condition / JSON enum vals.
           05  CDD-IR-ENUM               OCCURS 256 TIMES.
      * @field CDD-IRE-CNAM-LEN Length of COBOL 88 condition name.
               10  CDD-IRE-CNAM-LEN      PIC 9(2) COMP.
      * @field CDD-IRE-COB-NAME COBOL 88 condition identifier.
               10  CDD-IRE-COB-NAME      PIC X(64).
      * @field CDD-IRE-VAL-LEN Length of enum literal value.
               10  CDD-IRE-VAL-LEN       PIC 9(4) COMP.
      * @field CDD-IRE-VAL Enum value text.
               10  CDD-IRE-VAL           PIC X(64).
      * @field CDD-IRE-IS-STR 1 if string literal, 0 if numeric.
               10  CDD-IRE-IS-STR        PIC 9(2) COMP.
