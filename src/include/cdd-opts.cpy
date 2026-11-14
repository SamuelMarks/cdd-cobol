      * @file cdd-opts.cpy
      * @brief Strongly-typed SDK & codegen configuration envelope.
      * @record CDD-SDK-OPTS Options controlling dialect, format, & mode.
       01  CDD-SDK-OPTS.
      * @field CDD-OPT-TAG Magic type-safety tag ('CDDOPT01').
           05  CDD-OPT-TAG               PIC X(8) VALUE "CDDOPT01".
      * @field CDD-OPT-TAG-VALID Condition confirming valid OPT tag.
               88  CDD-OPT-TAG-VALID              VALUE "CDDOPT01".
      * @field CDD-OPT-DIALECT Target COBOL standard ID (1..10).
           05  CDD-OPT-DIALECT           PIC 9(2) COMP VALUE 7.
      * @field CDD-OPT-DIAL-60 CODASYL COBOL-60.
               88  CDD-OPT-DIAL-60                VALUE 1.
      * @field CDD-OPT-DIAL-61 CODASYL COBOL-61.
               88  CDD-OPT-DIAL-61                VALUE 2.
      * @field CDD-OPT-DIAL-61EXT CODASYL COBOL-61 Extended.
               88  CDD-OPT-DIAL-61EXT             VALUE 3.
      * @field CDD-OPT-DIAL-65 CODASYL COBOL-65.
               88  CDD-OPT-DIAL-65                VALUE 4.
      * @field CDD-OPT-DIAL-68 ANSI COBOL-68.
               88  CDD-OPT-DIAL-68                VALUE 5.
      * @field CDD-OPT-DIAL-74 ANSI COBOL-74.
               88  CDD-OPT-DIAL-74                VALUE 6.
      * @field CDD-OPT-DIAL-85 ANSI COBOL-85.
               88  CDD-OPT-DIAL-85                VALUE 7.
      * @field CDD-OPT-DIAL-2002 ISO COBOL-2002.
               88  CDD-OPT-DIAL-2002              VALUE 8.
      * @field CDD-OPT-DIAL-2014 ISO COBOL-2014.
               88  CDD-OPT-DIAL-2014              VALUE 9.
      * @field CDD-OPT-DIAL-2023 ISO COBOL-2023.
               88  CDD-OPT-DIAL-2023              VALUE 10.
      * @field CDD-OPT-FORMAT Source format (0=auto, 1=fixed, 2=free).
           05  CDD-OPT-FORMAT            PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-FMT-AUTO Auto-select format per dialect/input.
               88  CDD-OPT-FMT-AUTO               VALUE 0.
      * @field CDD-OPT-FMT-FIXED Fixed 80-column reference format.
               88  CDD-OPT-FMT-FIXED              VALUE 1.
      * @field CDD-OPT-FMT-FREE Free-format source (COBOL-2002+).
               88  CDD-OPT-FMT-FREE               VALUE 2.
      * @field CDD-OPT-UDT-KIND Target UDT construct (1=rec,2=td,3=cls).
           05  CDD-OPT-UDT-KIND          PIC 9(2) COMP VALUE 1.
      * @field CDD-OPT-UDT-RECORD 01 hierarchical group record.
               88  CDD-OPT-UDT-RECORD             VALUE 1.
      * @field CDD-OPT-UDT-TYPEDEF 01 IS TYPEDEF STRONG (2002+).
               88  CDD-OPT-UDT-TYPEDEF            VALUE 2.
      * @field CDD-OPT-UDT-CLASS CLASS-ID OO definition (2002+).
               88  CDD-OPT-UDT-CLASS              VALUE 3.
      * @field CDD-OPT-SERDE-KIND Routine form (0=auto,1=sec..4=meth).
           05  CDD-OPT-SERDE-KIND        PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-SRD-AUTO Era-idiomatic default serde construct.
               88  CDD-OPT-SRD-AUTO               VALUE 0.
      * @field CDD-OPT-SRD-SECTION PERFORM-able PROCEDURE SECTION.
               88  CDD-OPT-SRD-SECTION            VALUE 1.
      * @field CDD-OPT-SRD-SUBPROG CALL-able PROGRAM-ID subprogram.
               88  CDD-OPT-SRD-SUBPROG            VALUE 2.
      * @field CDD-OPT-SRD-FUNC COBOL-2002+ FUNCTION-ID UDF.
               88  CDD-OPT-SRD-FUNC               VALUE 3.
      * @field CDD-OPT-SRD-METHOD COBOL-2002+ OO METHOD-ID.
               88  CDD-OPT-SRD-METHOD             VALUE 4.
      * @field CDD-OPT-JSON-MODE JSON mode (0=auto,1=schema,2=sample).
           05  CDD-OPT-JSON-MODE         PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-JMODE-AUTO Auto-detect Schema vs sample JSON.
               88  CDD-OPT-JMODE-AUTO             VALUE 0.
      * @field CDD-OPT-JMODE-SCHEMA Treat/emit Draft 2020-12 Schema.
               88  CDD-OPT-JMODE-SCHEMA           VALUE 1.
      * @field CDD-OPT-JMODE-SAMPLE Treat/emit sample JSON payload.
               88  CDD-OPT-JMODE-SAMPLE           VALUE 2.
      * @field CDD-OPT-CASE JSON naming case (1=camel..4=preserve).
           05  CDD-OPT-CASE              PIC 9(2) COMP VALUE 1.
      * @field CDD-OPT-STYLE JSON whitespace (1=pretty, 2=compact).
           05  CDD-OPT-STYLE             PIC 9(2) COMP VALUE 1.
      * @field CDD-OPT-PURE-COBOL 1 to force pure-COBOL serde on 2023.
           05  CDD-OPT-PURE-COBOL        PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-VENDOR-JSON 1 to emit vendor JSON verbs on 2014.
           05  CDD-OPT-VENDOR-JSON       PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-X-COBOL 1 to emit x-cobol JSON Schema extension.
           05  CDD-OPT-X-COBOL           PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-SYNC-SERDE Serde sync mode (0=auto,1=yes,2=no).
           05  CDD-OPT-SYNC-SERDE        PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-C60-VERBOSE 1 to emit COBOL-60 SIZE/CLASS syntax.
           05  CDD-OPT-C60-VERBOSE       PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-NAME-LEN Active length of CDD-OPT-TARGET-NAME.
           05  CDD-OPT-NAME-LEN          PIC 9(2) COMP VALUE 0.
      * @field CDD-OPT-TARGET-NAME Explicit --name / --udt identifier.
           05  CDD-OPT-TARGET-NAME       PIC X(64) VALUE SPACES.
