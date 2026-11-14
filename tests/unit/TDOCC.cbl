      * @file TDOCC.cbl
      * @brief Unit tests for CDDDOCC 100% documentation gate tool.
      * @module TDOCC
      * @details Verifies CLI usage error, missing list I/O error,
      *          100% compliant file pass, and violation detection on
      *          undocumented files, modules, records, fields, params,
      *          sections, and paragraphs.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDOCC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-NL-CONV Helper for LF byte constant.
       01  WS-NL-CONV.
      * @field WS-U16 16-bit integer initialized to 10 (LF).
           05  WS-U16                    PIC 9(4) COMP VALUE 10.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-LF Low byte LF (10).
               10  WS-LF                 PIC X(1).
      * @record WS-STATE Test assertion failure counter.
       01  WS-STATE.
      * @field WS-FAILS Number of failed assertions.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR Write pointer into CDD-BUF-DATA.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @subprogram TDOCC Run positive and negative tests on CDDDOCC.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Execute all CDDDOCC test scenarios.
       MAIN-ENTRY.
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           MOVE 1 TO CDD-AGV-QUIET
           MOVE 0 TO CDD-AGV-COUNT
           CALL "MCDDDOCC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "build/no_such_doc_list.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDDOCC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 3
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM TEST-POSITIVE-LIST
           PERFORM TEST-NEGATIVE-FIXTURE
           MOVE 0 TO CDD-AGV-MODE
           MOVE 0 TO CDD-AGV-QUIET
           IF WS-FAILS > 0
               DISPLAY "TDOCC FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-POSITIVE-LIST Verify clean pass on core files.
       TEST-POSITIVE-LIST.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/tdocc_ok.lst" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "src/include/cdd-err.cpy" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "src/core/CDDAREN.cbl" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/tdocc_ok.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDDOCC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-NEGATIVE-FIXTURE Verify missing tags fail (1).
       TEST-NEGATIVE-FIXTURE.
           MOVE "build/tdocc_bad.cbl" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE 2 TO CDD-PATH-OP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "       IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROGRAM-ID. BADDOC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  BAD-REC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           05  BAD-FLD PIC X(4)." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROCEDURE DIVISION USING BAD-REC."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       BAD-SEC SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       BAD-PARA." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           EXIT PROGRAM." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/tdocc_bad.lst" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE "build/tdocc_bad.cbl" TO CDD-BUF-DATA(1:19)
           MOVE 19 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "build/tdocc_bad.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDDOCC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
