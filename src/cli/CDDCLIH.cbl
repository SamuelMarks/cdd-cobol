      * @file CDDCLIH.cbl
      * @brief CLI help, version banner, and error diagnostic presenter.
      * @module CDDCLIH
      * @details Renders global and subcommand --help usage screens,
      *          --version output, and CDDERRF-formatted diagnostics to
      *          SYSERR for the cdd-cobol CLI application.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCLIH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
      * @record WS-MSG Scratch line for console output.
       01  WS-MSG                        PIC X(120) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-cli.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDCLIH Render help, version, or formatted error.
      * @param [inout] CDD-CLIH-REQ Presenter op, topic, & quiet flag.
      * @param [inout] CDD-ERROR Unified error enum instance.
      * @return Rendered output and CDD-CLH-LAST-TEXT summary.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH.
       PROCEDURE DIVISION USING CDD-CLIH-REQ CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tag and dispatch presenter mode.
       MAIN-ENTRY.
           IF NOT CDD-CLH-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCLIH" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-CLIH-REQ tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           EVALUATE TRUE
               WHEN CDD-CLH-OP-VERSION
                   PERFORM SHOW-VERSION
               WHEN CDD-CLH-OP-ERROR
                   PERFORM SHOW-ERROR
               WHEN OTHER
                   PERFORM SHOW-HELP
           END-EVALUATE
           EXIT PROGRAM.
      * @paragraph PRINT-STDOUT-MSG Emit WS-MSG unless quiet mode set.
       PRINT-STDOUT-MSG.
           MOVE WS-MSG TO CDD-CLH-LAST-TEXT
           IF CDD-CLH-QUIET = 0
               DISPLAY WS-MSG
               END-DISPLAY
           END-IF.
      * @paragraph SHOW-VERSION Display semantic version banner.
       SHOW-VERSION.
           MOVE "cdd-cobol 1.0.0 (COBOL-60..COBOL-2023 SDK & CLI)"
               TO WS-MSG
           PERFORM PRINT-STDOUT-MSG.
      * @paragraph SHOW-HELP Display global or per-subcommand usage.
       SHOW-HELP.
           EVALUATE CDD-CLH-TOPIC
               WHEN 1
                   MOVE "Usage: cdd-cobol emit -i <json> -o <cpy> [-d di
      -                "alect] [--udt-kind record|typedef|class]"
                       TO WS-MSG
                   PERFORM PRINT-STDOUT-MSG
               WHEN 2
                   MOVE "Usage: cdd-cobol parse -i <cob> -o <json> [--js
      -                "on-mode schema|sample] [--pretty|--compact]"
                       TO WS-MSG
                   PERFORM PRINT-STDOUT-MSG
               WHEN 3
                   MOVE "Usage: cdd-cobol serde -i <in> -o <cbl> [-d dia
      -                "lect] [--serde-kind auto|section|subprogram]"
                       TO WS-MSG
                   PERFORM PRINT-STDOUT-MSG
               WHEN 4
                   MOVE "Usage: cdd-cobol sync -c <cob> -j <json> [-o <o
      -                "ut> | --in-place] [--udt <NAME>]"
                       TO WS-MSG
                   PERFORM PRINT-STDOUT-MSG
               WHEN OTHER
                   PERFORM SHOW-GLOBAL-HELP
           END-EVALUATE.
      * @paragraph SHOW-GLOBAL-HELP Display top-level CLI overview.
       SHOW-GLOBAL-HELP.
           MOVE "cdd-cobol - JSON/Schema <-> COBOL (1960-2023) CLI"
               TO WS-MSG
           PERFORM PRINT-STDOUT-MSG
           MOVE "Subcommands: emit | parse | serde | sync"
               TO WS-MSG
           PERFORM PRINT-STDOUT-MSG
           MOVE "Dialects: cobol60 cobol61 cobol61ext cobol65 cobol68 co
      -        "bol74 cobol85 cobol2002 cobol2014 cobol2023"
               TO WS-MSG
           PERFORM PRINT-STDOUT-MSG.
      * @paragraph SHOW-ERROR Format CDD-ERROR via CDDERRF to SYSERR.
       SHOW-ERROR.
           MOVE "CDDERQ01" TO CDD-ERQ-TAG
           MOVE 3 TO CDD-ERQ-OP
           CALL "CDDERRF" USING CDD-ERR-REQ CDD-ERROR
           END-CALL
           MOVE CDD-ERQ-FMT-TEXT TO CDD-CLH-LAST-TEXT
           IF CDD-CLH-QUIET = 0
               DISPLAY CDD-ERQ-FMT-TEXT(1:CDD-ERQ-FMT-LEN) UPON SYSERR
               END-DISPLAY
           END-IF.
