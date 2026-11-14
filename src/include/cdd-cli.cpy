      * @file cdd-cli.cpy
      * @brief Strongly-typed CLI configuration and presenter envelopes.
      * @record CDD-CLI-CFG Parsed CLI subcommand, paths, & SDK options.
       01  CDD-CLI-CFG.
      * @field CDD-CLI-TAG Magic type-safety tag ('CDDCLI01').
           05  CDD-CLI-TAG               PIC X(8) VALUE "CDDCLI01".
      * @field CDD-CLI-TAG-VALID Condition confirming valid CLI tag.
               88  CDD-CLI-TAG-VALID              VALUE "CDDCLI01".
      * @field CDD-CLI-CMD Parsed subcommand code (1..6).
           05  CDD-CLI-CMD               PIC 9(2) COMP VALUE 0.
      * @field CDD-CLI-CMD-EMIT Feature 0: JSON -> COBOL UDT.
               88  CDD-CLI-CMD-EMIT               VALUE 1.
      * @field CDD-CLI-CMD-PARSE Feature 1: COBOL UDT -> JSON/Schema.
               88  CDD-CLI-CMD-PARSE              VALUE 2.
      * @field CDD-CLI-CMD-SERDE Feature 2: Generate serde routines.
               88  CDD-CLI-CMD-SERDE              VALUE 3.
      * @field CDD-CLI-CMD-SYNC Feature 3: Surgical UDT + serde sync.
               88  CDD-CLI-CMD-SYNC               VALUE 4.
      * @field CDD-CLI-CMD-HELP Display global or subcommand help.
               88  CDD-CLI-CMD-HELP               VALUE 5.
      * @field CDD-CLI-CMD-VER Display cdd-cobol version banner.
               88  CDD-CLI-CMD-VER                VALUE 6.
      * @field CDD-CLI-HELP-TOPIC Target subcommand for --help (0=all).
           05  CDD-CLI-HELP-TOPIC        PIC 9(2) COMP VALUE 0.
      * @field CDD-CLI-IN-PLACE 1 if --in-place atomic write requested.
           05  CDD-CLI-IN-PLACE          PIC 9(2) COMP VALUE 0.
      * @field CDD-CLI-IN-LEN Length of -i / --input path ('-'=stdin).
           05  CDD-CLI-IN-LEN            PIC 9(4) COMP VALUE 1.
      * @field CDD-CLI-IN-PATH Input file path (default '-').
           05  CDD-CLI-IN-PATH           PIC X(512) VALUE "-".
      * @field CDD-CLI-OUT-LEN Length of -o / --output path ('-'=out).
           05  CDD-CLI-OUT-LEN           PIC 9(4) COMP VALUE 1.
      * @field CDD-CLI-OUT-PATH Output file path (default '-').
           05  CDD-CLI-OUT-PATH          PIC X(512) VALUE "-".
      * @field CDD-CLI-COB-LEN Length of -c / --cobol path for sync.
           05  CDD-CLI-COB-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-CLI-COB-PATH Existing COBOL path for sync.
           05  CDD-CLI-COB-PATH          PIC X(512) VALUE SPACES.
      * @field CDD-CLI-JSN-LEN Length of -j / --json path for sync.
           05  CDD-CLI-JSN-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-CLI-JSN-PATH Target JSON/Schema path for sync.
           05  CDD-CLI-JSN-PATH          PIC X(512) VALUE SPACES.
      * @record CDD-CLIH-REQ Request envelope for CDDCLIH presenter.
       01  CDD-CLIH-REQ.
      * @field CDD-CLH-TAG Magic type-safety tag ('CDDCLH01').
           05  CDD-CLH-TAG               PIC X(8) VALUE "CDDCLH01".
      * @field CDD-CLH-TAG-VALID Condition confirming valid CLH tag.
               88  CDD-CLH-TAG-VALID              VALUE "CDDCLH01".
      * @field CDD-CLH-OP Presenter operation (1=help,2=ver,3=err).
           05  CDD-CLH-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-CLH-OP-HELP Render global or subcommand help.
               88  CDD-CLH-OP-HELP                VALUE 1.
      * @field CDD-CLH-OP-VERSION Render version string to stdout.
               88  CDD-CLH-OP-VERSION             VALUE 2.
      * @field CDD-CLH-OP-ERROR Render formatted CDD-ERROR to stderr.
               88  CDD-CLH-OP-ERROR               VALUE 3.
      * @field CDD-CLH-TOPIC Help topic (0=global, 1..4=subcommand).
           05  CDD-CLH-TOPIC             PIC 9(2) COMP VALUE 0.
      * @field CDD-CLH-QUIET 1 to suppress console writes in unit test.
           05  CDD-CLH-QUIET             PIC 9(2) COMP VALUE 0.
      * @field CDD-CLH-LAST-TEXT Last rendered line for test assertion.
           05  CDD-CLH-LAST-TEXT         PIC X(320) VALUE SPACES.
