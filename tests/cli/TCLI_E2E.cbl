      * @file TCLI_E2E.cbl
      * @brief End-to-end CLI driver & black-box process test suite.
      * @module TCLIE2E
      * @details Exercises CDDCLI in-process (MCDDCLI) AND via real OS
      *          subprocess ($CDD_CLI_BIN) across emit, parse, serde,
      *          sync, --in-place atomic updates, stdin/stdout pipes,
      *          help/version, and deterministic exit codes 0, 1, 2, 3.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCLIE2E.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-argv.cpy".
       COPY "cdd-err.cpy".
      * @record WS-SYS-CMD Shell command string for subprocess tests.
       01  WS-SYS-CMD                    PIC X(512) VALUE SPACES.
      * @record WS-BYTE-CONV NUL byte helper for SYSTEM calls.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer initialized to 0 (NUL).
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-NUL Low byte NUL (0).
               10  WS-NUL                PIC X(1).
      * @record WS-STATE Test assertion and return code state.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-RC Return status from SYSTEM call.
           05  WS-RC                     PIC S9(9) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(4) COMP VALUE 1.
      * @subprogram TCLIE2E Run in-process and black-box CLI E2E tests.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run in-process and subprocess E2E suites.
       MAIN-ENTRY.
           PERFORM PREPARE-E2E-FIXTURES
           PERFORM TEST-INPROC-OPS-AND-EXITS
           PERFORM TEST-BLACKBOX-SUBPROCESS-PIPES
           IF WS-FAILS > 0
               DISPLAY "TCLI_E2E FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph PREPARE-E2E-FIXTURES Write sample JSON & bad JSON.
       PREPARE-E2E-FIXTURES.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/e2e_v1.json" TO CDD-PATH-VAL
           MOVE 17 TO CDD-PATH-LEN
           MOVE '{"id":1,"name":"Ada"}' TO CDD-BUF-DATA(1:21)
           MOVE 21 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/e2e_v2.json" TO CDD-PATH-VAL
           MOVE '{"id":1,"name":"Ada","active":true}'
               TO CDD-BUF-DATA(1:35)
           MOVE 35 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/e2e_bad.json" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE "!" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL.
      * @paragraph TEST-INPROC-OPS-AND-EXITS Test exit 0/1/2/3.
       TEST-INPROC-OPS-AND-EXITS.
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           MOVE 1 TO CDD-AGV-QUIET
           MOVE 0 TO CDD-AGV-COUNT
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "--help" TO CDD-AGV-ITEM(1)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "--version" TO CDD-AGV-ITEM(1)
           MOVE 0 TO CDD-AGV-QUIET
           CALL "MCDDCLI"
           END-CALL
           MOVE 1 TO CDD-AGV-QUIET
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 5 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "-i" TO CDD-AGV-ITEM(2)
           MOVE "build/e2e_v1.json" TO CDD-AGV-ITEM(3)
           MOVE "-o" TO CDD-AGV-ITEM(4)
           MOVE "build/e2e_udt.cpy" TO CDD-AGV-ITEM(5)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "parse" TO CDD-AGV-ITEM(1)
           MOVE "build/e2e_udt.cpy" TO CDD-AGV-ITEM(3)
           MOVE "build/e2e_sch.json" TO CDD-AGV-ITEM(5)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "serde" TO CDD-AGV-ITEM(1)
           MOVE "build/e2e_udt.cpy" TO CDD-AGV-ITEM(3)
           MOVE "build/e2e_srd.cbl" TO CDD-AGV-ITEM(5)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM TEST-INPROC-SYNC-AND-ERRORS
           MOVE 0 TO CDD-AGV-MODE
           MOVE 0 TO CDD-AGV-QUIET.
      * @paragraph TEST-INPROC-SYNC-AND-ERRORS Test sync & exit 1/3.
       TEST-INPROC-SYNC-AND-ERRORS.
           MOVE 6 TO CDD-AGV-COUNT
           MOVE "sync" TO CDD-AGV-ITEM(1)
           MOVE "-c" TO CDD-AGV-ITEM(2)
           MOVE "build/e2e_udt.cpy" TO CDD-AGV-ITEM(3)
           MOVE "-j" TO CDD-AGV-ITEM(4)
           MOVE "build/e2e_v2.json" TO CDD-AGV-ITEM(5)
           MOVE "--in-place" TO CDD-AGV-ITEM(6)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-AGV-COUNT
           MOVE "-o" TO CDD-AGV-ITEM(6)
           MOVE "build/e2e_syn.cpy" TO CDD-AGV-ITEM(7)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build/no_dir/out.cpy" TO CDD-AGV-ITEM(7)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 3
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build/e2e_bad.json" TO CDD-AGV-ITEM(5)
           MOVE "build/e2e_syn.cpy" TO CDD-AGV-ITEM(7)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build/missing.cbl" TO CDD-AGV-ITEM(3)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 3
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 5 TO CDD-AGV-COUNT
           MOVE "emit" TO CDD-AGV-ITEM(1)
           MOVE "-i" TO CDD-AGV-ITEM(2)
           MOVE "build/missing.json" TO CDD-AGV-ITEM(3)
           MOVE "-o" TO CDD-AGV-ITEM(4)
           MOVE "build/out.cpy" TO CDD-AGV-ITEM(5)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 3
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build/e2e_bad.json" TO CDD-AGV-ITEM(3)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build/e2e_v1.json" TO CDD-AGV-ITEM(3)
           MOVE "build/no_dir/out.cpy" TO CDD-AGV-ITEM(5)
           CALL "MCDDCLI"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 3
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-BLACKBOX-SUBPROCESS-PIPES Run real OS binary.
       TEST-BLACKBOX-SUBPROCESS-PIPES.
           MOVE SPACES TO WS-SYS-CMD
           MOVE 1 TO WS-PTR
           STRING '${CDD_CLI_BIN:-./bin/cdd-cobol} emit '
                      DELIMITED BY SIZE
                  '-d cobol85 --name PIPE-REC ' DELIMITED BY SIZE
                  '< build/e2e_v1.json > build/e2e_pipe.cpy'
                      DELIMITED BY SIZE
                  WS-NUL DELIMITED BY SIZE
                  INTO WS-SYS-CMD WITH POINTER WS-PTR
           END-STRING
           CALL "SYSTEM" USING WS-SYS-CMD GIVING WS-RC
           END-CALL
           IF WS-RC NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
