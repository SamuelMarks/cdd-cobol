      * @file CDDCLI.cbl
      * @brief Main entry point for the cdd-cobol CLI executable.
      * @module CDDCLI
      * @details Orchestrates CLI argument parsing (CDDCLIA), help and
      *          version output (CDDCLIH), raw/stdio/atomic file I/O
      *          (CDDIO), and public SDK operations (CDDEMIT, CDDPARS,
      *          CDDSERD, CDDSYN) with deterministic exit codes (0..3).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCLI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-cli.cpy".
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-SEC-BUF Secondary input buffer (JSON in sync mode).
       01  WS-SEC-BUF.
      * @field WS-SBF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-SBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-SBF-LEN Active byte length.
           05  WS-SBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-SBF-CAP Capacity.
           05  WS-SBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-SBF-NL Newline mode.
           05  WS-SBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-SBF-DATA Raw JSON bytes.
           05  WS-SBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-OUT-BUF Output buffer written to file or stdout.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active output byte length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Newline mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Serialized output bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
       COPY "cdd-err.cpy".
      * @record WS-CLI-STATE Process exit code tracker.
       01  WS-CLI-STATE.
      * @field WS-EXIT-CODE Exit code (0=ok, 1=sdk, 2=usage, 3=io).
           05  WS-EXIT-CODE              PIC 9(4) COMP VALUE 0.
      * @subprogram CDDCLI Execute cdd-cobol CLI command and set exit.
      * @return Exit code 0=OK, 1=parse/codegen, 2=CLI usage, 3=I/O.
      * @errors Formats any CDD-ERROR diagnostic to SYSERR via CDDCLIH.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Parse CLI, dispatch command, map exit.
       MAIN-ENTRY.
           MOVE "CDDCLI01" TO CDD-CLI-TAG
           MOVE "CDDCLH01" TO CDD-CLH-TAG
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDBUF01" TO WS-SBF-TAG
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 1048576 TO CDD-BUF-CAP
           MOVE 1048576 TO WS-SBF-CAP
           MOVE 1048576 TO WS-OBF-CAP
           MOVE 0 TO WS-EXIT-CODE
           IF CDD-AGV-TAG-VALID AND CDD-AGV-QUIET-YES
               MOVE 1 TO CDD-CLH-QUIET
           ELSE
               MOVE 0 TO CDD-CLH-QUIET
           END-IF
           CALL "CDDCLIA" USING CDD-CLI-CFG CDD-SDK-OPTS CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               PERFORM PRESENT-CLI-FAILURE
           ELSE
               PERFORM DISPATCH-SUBCOMMAND
           END-IF
           IF CDD-AGV-TAG-VALID AND CDD-AGV-USE-MEM
               MOVE WS-EXIT-CODE TO CDD-AGV-EXIT-CODE
           END-IF
           STOP RUN RETURNING WS-EXIT-CODE.
      * @paragraph DISPATCH-SUBCOMMAND Execute help, version, or SDK op.
       DISPATCH-SUBCOMMAND.
           EVALUATE TRUE
               WHEN CDD-CLI-CMD-HELP
                   MOVE 1 TO CDD-CLH-OP
                   MOVE CDD-CLI-HELP-TOPIC TO CDD-CLH-TOPIC
                   CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
                   END-CALL
               WHEN CDD-CLI-CMD-VER
                   MOVE 2 TO CDD-CLH-OP
                   CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
                   END-CALL
               WHEN CDD-CLI-CMD-SYNC
                   PERFORM RUN-SYNC-COMMAND
               WHEN OTHER
                   PERFORM RUN-SINGLE-INPUT-COMMAND
           END-EVALUATE.
      * @paragraph RUN-SINGLE-INPUT-COMMAND Run emit, parse, or serde.
       RUN-SINGLE-INPUT-COMMAND.
           MOVE 1 TO CDD-PATH-OP
           MOVE CDD-CLI-IN-LEN TO CDD-PATH-LEN
           MOVE CDD-CLI-IN-PATH TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               PERFORM PRESENT-CLI-FAILURE
           ELSE
               MOVE CDD-BUF-NL-KIND TO WS-OBF-NL
               EVALUATE TRUE
                   WHEN CDD-CLI-CMD-EMIT
                       CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF
                           WS-OUT-BUF CDD-ERROR
                       END-CALL
                   WHEN CDD-CLI-CMD-PARSE
                       CALL "CDDPARS" USING CDD-SDK-OPTS CDD-BUF
                           WS-OUT-BUF CDD-ERROR
                       END-CALL
                   WHEN OTHER
                       CALL "CDDSERD" USING CDD-SDK-OPTS CDD-BUF
                           WS-OUT-BUF CDD-ERROR
                       END-CALL
               END-EVALUATE
               IF NOT CDD-OK
                   PERFORM PRESENT-CLI-FAILURE
               ELSE
                   MOVE 2 TO CDD-PATH-OP
                   MOVE CDD-CLI-OUT-LEN TO CDD-PATH-LEN
                   MOVE CDD-CLI-OUT-PATH TO CDD-PATH-VAL
                   CALL "CDDIO" USING CDD-PATH-REQ WS-OUT-BUF CDD-ERROR
                   END-CALL
                   IF NOT CDD-OK
                       PERFORM PRESENT-CLI-FAILURE
                   END-IF
               END-IF
           END-IF.
      * @paragraph RUN-SYNC-COMMAND Read COBOL + JSON, sync, write out.
       RUN-SYNC-COMMAND.
           MOVE 1 TO CDD-PATH-OP
           MOVE CDD-CLI-COB-LEN TO CDD-PATH-LEN
           MOVE CDD-CLI-COB-PATH TO CDD-PATH-VAL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF CDD-OK
               MOVE 1 TO CDD-PATH-OP
               MOVE CDD-CLI-JSN-LEN TO CDD-PATH-LEN
               MOVE CDD-CLI-JSN-PATH TO CDD-PATH-VAL
               CALL "CDDIO" USING CDD-PATH-REQ WS-SEC-BUF CDD-ERROR
               END-CALL
           END-IF
           IF NOT CDD-OK
               PERFORM PRESENT-CLI-FAILURE
           ELSE
               MOVE CDD-BUF-NL-KIND TO WS-OBF-NL
               CALL "CDDSYN" USING CDD-SDK-OPTS CDD-BUF WS-SEC-BUF
                   WS-OUT-BUF CDD-ERROR
               END-CALL
               IF NOT CDD-OK
                   PERFORM PRESENT-CLI-FAILURE
               ELSE
                   IF CDD-CLI-IN-PLACE = 1
                       MOVE 3 TO CDD-PATH-OP
                       MOVE CDD-CLI-COB-LEN TO CDD-PATH-LEN
                       MOVE CDD-CLI-COB-PATH TO CDD-PATH-VAL
                   ELSE
                       MOVE 2 TO CDD-PATH-OP
                       MOVE CDD-CLI-OUT-LEN TO CDD-PATH-LEN
                       MOVE CDD-CLI-OUT-PATH TO CDD-PATH-VAL
                   END-IF
                   CALL "CDDIO" USING CDD-PATH-REQ WS-OUT-BUF CDD-ERROR
                   END-CALL
                   IF NOT CDD-OK
                       PERFORM PRESENT-CLI-FAILURE
                   END-IF
               END-IF
           END-IF.
      * @paragraph PRESENT-CLI-FAILURE Format error & map exit 1/2/3.
       PRESENT-CLI-FAILURE.
           MOVE 3 TO CDD-CLH-OP
           CALL "CDDCLIH" USING CDD-CLIH-REQ CDD-ERROR
           END-CALL
           EVALUATE TRUE
               WHEN CDD-ERR-CLI-NO-SUBCOMMAND
               WHEN CDD-ERR-CLI-UNKNOWN-SUBCOMMAND
               WHEN CDD-ERR-CLI-UNKNOWN-FLAG
               WHEN CDD-ERR-CLI-MISSING-VALUE
               WHEN CDD-ERR-CLI-CONFLICT-FLAGS
                   MOVE 2 TO WS-EXIT-CODE
               WHEN CDD-ERR-IO-OPEN-READ
               WHEN CDD-ERR-IO-OPEN-WRITE
               WHEN CDD-ERR-IO-READ-FAILURE
               WHEN CDD-ERR-IO-WRITE-FAILURE
               WHEN CDD-ERR-IO-ATOMIC-RENAME
                   MOVE 3 TO WS-EXIT-CODE
               WHEN OTHER
                   MOVE 1 TO WS-EXIT-CODE
           END-EVALUATE.
