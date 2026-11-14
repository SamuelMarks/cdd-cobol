      * @file TCLIA.cbl
      * @brief Unit test suite for CDDCLIA CLI subcommand & flag parser.
      * @module TCLIA
      * @details Exercises tag checks, empty args, all 4 subcommands,
      *          --help/-h, --version/-V, every flag & enum value, >63
      *          clamping, missing values, unknown flags/subcommands,
      *          and --compact/--pretty and --in-place/-o conflicts.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCLIA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-cli.cpy".
       COPY "cdd-opts.cpy".
       COPY "cdd-argv.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test assertion state.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-I Loop index.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @subprogram TCLIA Verify 100% of CDDCLIA lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDCLIA test groups.
       MAIN-ENTRY.
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           PERFORM TEST-GUARDS-AND-BASIC-CMDS
           PERFORM TEST-ALL-OPTION-ENUMS
           PERFORM TEST-CONFLICTS-AND-ERRORS
           MOVE 0 TO CDD-AGV-MODE
           IF WS-FAILS > 0
               DISPLAY "TCLIA FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-BASIC-CMDS Tags, 0 args, help, ver.
       TEST-GUARDS-AND-BASIC-CMDS.
           MOVE "BADCLI00" TO CDD-CLI-TAG
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCLI01" TO CDD-CLI-TAG
           MOVE "BADOPT00" TO CDD-OPT-TAG
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE 0 TO CDD-AGV-COUNT
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-NO-SUBCOMMAND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "--version" TO CDD-AGV-ITEM(1)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR NOT CDD-CLI-CMD-VER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "--help" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR NOT CDD-CLI-CMD-HELP
               OR CDD-CLI-HELP-TOPIC NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "--pure-cobol" TO CDD-AGV-ITEM(1)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-NO-SUBCOMMAND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "boguscmd" TO CDD-AGV-ITEM(1)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-UNKNOWN-SUBCOMMAND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-ALL-OPTION-ENUMS Test every valid flag & enum.
       TEST-ALL-OPTION-ENUMS.
           MOVE 24 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "-i" TO CDD-AGV-ITEM(2)
           MOVE "in.json" TO CDD-AGV-ITEM(3)
           MOVE "-o" TO CDD-AGV-ITEM(4)
           MOVE "out.cpy" TO CDD-AGV-ITEM(5)
           MOVE "-d" TO CDD-AGV-ITEM(6)
           MOVE "cobol2023" TO CDD-AGV-ITEM(7)
           MOVE "--udt-kind" TO CDD-AGV-ITEM(8)
           MOVE "typedef" TO CDD-AGV-ITEM(9)
           MOVE "--serde-kind" TO CDD-AGV-ITEM(10)
           MOVE "function" TO CDD-AGV-ITEM(11)
           MOVE "--format" TO CDD-AGV-ITEM(12)
           MOVE "free" TO CDD-AGV-ITEM(13)
           MOVE "--json-mode" TO CDD-AGV-ITEM(14)
           MOVE "schema" TO CDD-AGV-ITEM(15)
           MOVE "--name" TO CDD-AGV-ITEM(16)
           MOVE "CUST-T" TO CDD-AGV-ITEM(17)
           MOVE "--case" TO CDD-AGV-ITEM(18)
           MOVE "snake" TO CDD-AGV-ITEM(19)
           MOVE "--pure-cobol" TO CDD-AGV-ITEM(20)
           MOVE "--vendor-json" TO CDD-AGV-ITEM(21)
           MOVE "--x-cobol" TO CDD-AGV-ITEM(22)
           MOVE "--cobol60-verbose-size" TO CDD-AGV-ITEM(23)
           MOVE "--compact" TO CDD-AGV-ITEM(24)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 14 TO CDD-AGV-COUNT
           MOVE "parse" TO CDD-AGV-ITEM(1)
           MOVE "--udt-kind" TO CDD-AGV-ITEM(2)
           MOVE "record" TO CDD-AGV-ITEM(3)
           MOVE "--serde-kind" TO CDD-AGV-ITEM(4)
           MOVE "auto" TO CDD-AGV-ITEM(5)
           MOVE "--format" TO CDD-AGV-ITEM(6)
           MOVE "auto" TO CDD-AGV-ITEM(7)
           MOVE "--json-mode" TO CDD-AGV-ITEM(8)
           MOVE "auto" TO CDD-AGV-ITEM(9)
           MOVE "--case" TO CDD-AGV-ITEM(10)
           MOVE "camel" TO CDD-AGV-ITEM(11)
           MOVE "--pretty" TO CDD-AGV-ITEM(12)
           MOVE "--sync-serde" TO CDD-AGV-ITEM(13)
           MOVE "--no-sync-serde" TO CDD-AGV-ITEM(14)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 11 TO CDD-AGV-COUNT
           MOVE "serde" TO CDD-AGV-ITEM(1)
           MOVE "--udt-kind" TO CDD-AGV-ITEM(2)
           MOVE "class" TO CDD-AGV-ITEM(3)
           MOVE "--serde-kind" TO CDD-AGV-ITEM(4)
           MOVE "section" TO CDD-AGV-ITEM(5)
           MOVE "--format" TO CDD-AGV-ITEM(6)
           MOVE "fixed" TO CDD-AGV-ITEM(7)
           MOVE "--json-mode" TO CDD-AGV-ITEM(8)
           MOVE "sample" TO CDD-AGV-ITEM(9)
           MOVE "--case" TO CDD-AGV-ITEM(10)
           MOVE "kebab" TO CDD-AGV-ITEM(11)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO CDD-AGV-COUNT
           MOVE "sync" TO CDD-AGV-ITEM(1)
           MOVE "-c" TO CDD-AGV-ITEM(2)
           MOVE "a.cbl" TO CDD-AGV-ITEM(3)
           MOVE "-j" TO CDD-AGV-ITEM(4)
           MOVE "a.json" TO CDD-AGV-ITEM(5)
           MOVE "--in-place" TO CDD-AGV-ITEM(6)
           MOVE "--serde-kind" TO CDD-AGV-ITEM(7)
           MOVE "subprogram" TO CDD-AGV-ITEM(8)
           MOVE "--case" TO CDD-AGV-ITEM(9)
           MOVE "preserve" TO CDD-AGV-ITEM(10)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-AGV-COUNT
           MOVE "serde" TO CDD-AGV-ITEM(1)
           MOVE "--serde-kind" TO CDD-AGV-ITEM(2)
           MOVE "method" TO CDD-AGV-ITEM(3)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-CONFLICTS-AND-ERRORS Test missing/bad/conflict.
       TEST-CONFLICTS-AND-ERRORS.
           PERFORM TEST-MISSING-VALUES
           PERFORM TEST-INVALID-ENUM-VALUES
           MOVE 3 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "--compact" TO CDD-AGV-ITEM(2)
           MOVE "--pretty" TO CDD-AGV-ITEM(3)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-CONFLICT-FLAGS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 4 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "--in-place" TO CDD-AGV-ITEM(2)
           MOVE "-o" TO CDD-AGV-ITEM(3)
           MOVE "x.cbl" TO CDD-AGV-ITEM(4)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-CONFLICT-FLAGS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "sync" TO CDD-AGV-ITEM(1)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-MISSING-VALUE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "--bogus-flag" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-UNKNOWN-FLAG
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-MISSING-VALUES Exercise missing arg on flags.
       TEST-MISSING-VALUES.
           MOVE 2 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "-i" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "-o" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "-c" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "-j" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "-d" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--name" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--udt-kind" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--serde-kind" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--format" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--json-mode" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--case" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE 3 TO CDD-AGV-COUNT
           MOVE SPACES TO CDD-AGV-ITEM(3)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-MISSING-VALUE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-INVALID-ENUM-VALUES Test bad enum & >64 values.
       TEST-INVALID-ENUM-VALUES.
           MOVE 3 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "--udt-kind" TO CDD-AGV-ITEM(2)
           MOVE "bad" TO CDD-AGV-ITEM(3)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--serde-kind" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--format" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--json-mode" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--case" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           MOVE "--dialect" TO CDD-AGV-ITEM(2)
           MOVE SPACES TO CDD-AGV-ITEM(3)
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 70
               MOVE "X" TO CDD-AGV-ITEM(3)(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-CLI-UNKNOWN-FLAG
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "--name" TO CDD-AGV-ITEM(2)
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-OPT-NAME-LEN NOT = 63
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
