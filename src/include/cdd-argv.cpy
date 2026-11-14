      * @file cdd-argv.cpy
      * @brief External CLI argument override table for in-process tests.
      * @record CDD-ARGV-OVR Shared CLI argument table across modules.
       01  CDD-ARGV-OVR IS EXTERNAL.
      * @field CDD-AGV-TAG Magic type-safety tag ('CDDAGV01').
           05  CDD-AGV-TAG               PIC X(8).
      * @field CDD-AGV-TAG-VALID Condition confirming active override.
               88  CDD-AGV-TAG-VALID              VALUE "CDDAGV01".
      * @field CDD-AGV-MODE Override mode selector (0=OS, 1=override).
           05  CDD-AGV-MODE              PIC 9(2) COMP.
      * @field CDD-AGV-USE-OS Read real OS command-line arguments.
               88  CDD-AGV-USE-OS                 VALUE 0.
      * @field CDD-AGV-USE-MEM Read arguments from CDD-AGV-ITEM table.
               88  CDD-AGV-USE-MEM                VALUE 1.
      * @field CDD-AGV-COUNT Number of active arguments (0..32).
           05  CDD-AGV-COUNT             PIC 9(4) COMP.
      * @field CDD-AGV-EXIT-CODE Captured exit code in memory mode.
           05  CDD-AGV-EXIT-CODE         PIC 9(4) COMP.
      * @field CDD-AGV-QUIET Suppress console output during unit tests.
           05  CDD-AGV-QUIET             PIC 9(2) COMP.
      * @field CDD-AGV-QUIET-NO Normal console output enabled.
               88  CDD-AGV-QUIET-NO               VALUE 0.
      * @field CDD-AGV-QUIET-YES Console output suppressed for test.
               88  CDD-AGV-QUIET-YES              VALUE 1.
      * @field CDD-AGV-ITEM Argument value slots (up to 32 args).
           05  CDD-AGV-ITEM              PIC X(256) OCCURS 32 TIMES.
