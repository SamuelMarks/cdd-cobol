      * @file TEMIT_60_2023.cbl
      * @brief 10-dialect JSON -> COBOL -> JSON & doc-check suite.
      * @module TEMIT60
      * @details Emits COBOL UDTs across all 10 standards (COBOL-60
      *          through COBOL-2023), verifies every generated UDT
      *          passes MCDDDOCC at 100.00% doc coverage, and round-
      *          trips each UDT back to JSON Schema and Sample JSON.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMIT60.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-COB-BUF Generated COBOL UDT buffer.
       01  WS-COB-BUF.
      * @field WS-CBF-TAG Magic tag ('CDDBUF01').
           05  WS-CBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-CBF-LEN Active COBOL length.
           05  WS-CBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-CBF-CAP Buffer capacity.
           05  WS-CBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-CBF-NL Newline mode.
           05  WS-CBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-CBF-DATA Generated COBOL bytes.
           05  WS-CBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-RT-BUF Round-tripped JSON output buffer.
       01  WS-RT-BUF.
      * @field WS-RBF-TAG Magic tag ('CDDBUF01').
           05  WS-RBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-RBF-LEN Active JSON length.
           05  WS-RBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-RBF-CAP Buffer capacity.
           05  WS-RBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-RBF-NL Newline mode.
           05  WS-RBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-RBF-DATA Round-tripped JSON bytes.
           05  WS-RBF-DATA               PIC X(1048576) VALUE SPACES.
       COPY "cdd-err.cpy".
      * @record WS-STATE Dialect loop and assertion state.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-DIAL Dialect loop index (1..10).
           05  WS-DIAL                   PIC 9(2) COMP VALUE 1.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @subprogram TEMIT60 Verify emit + doc-check + parse on 1..10.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run 10-dialect round-trip conformance.
       MAIN-ENTRY.
           PERFORM PREPARE-SCHEMA-FIXTURE
           MOVE 1 TO WS-DIAL
           PERFORM UNTIL WS-DIAL > 10
               PERFORM VERIFY-DIALECT-ROUNDTRIP
               ADD 1 TO WS-DIAL
               END-ADD
           END-PERFORM
           IF WS-FAILS > 0
               DISPLAY "TEMIT_60_2023 FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph PREPARE-SCHEMA-FIXTURE Populate input JSON Schema.
       PREPARE-SCHEMA-FIXTURE.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDBUF01" TO WS-CBF-TAG
           MOVE "CDDBUF01" TO WS-RBF-TAG
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"Account","description":"Customer acct",'
                      DELIMITED BY SIZE
                  '"required":["id","active"],' DELIMITED BY SIZE
                  '"properties":{' DELIMITED BY SIZE
                  '"id":{"type":"integer","minimum":0},'
                      DELIMITED BY SIZE
                  '"balance":{"type":"number","multipleOf":0.01},'
                      DELIMITED BY SIZE
                  '"active":{"type":"boolean"},' DELIMITED BY SIZE
                  '"status":{"type":"string","enum":["OPEN","HOLD"]},'
                      DELIMITED BY SIZE
                  '"tags":{"type":"array","minItems":1,"maxItems":3,'
                      DELIMITED BY SIZE
                  '"items":{"type":"string","maxLength":10}},'
                      DELIMITED BY SIZE
                  '"meta":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"branch":{"type":"string","maxLength":6}}}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT.
      * @paragraph VERIFY-DIALECT-ROUNDTRIP Emit, doc-check, & parse.
       VERIFY-DIALECT-ROUNDTRIP.
           MOVE WS-DIAL TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 1 TO CDD-MPR-JSON-MODE
           MOVE 0 TO CDD-MPR-NAME-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           MOVE WS-DIAL TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-UDT-KIND
           MOVE 0 TO CDD-OPT-C60-VERBOSE
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR WS-COB-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-CBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           ELSE
               PERFORM VERIFY-GEN-DOC-COVERAGE
               MOVE 1 TO CDD-OPT-JSON-MODE
               MOVE 1 TO CDD-OPT-STYLE
               CALL "CDDGENJ" USING CDD-SDK-OPTS WS-COB-BUF WS-RT-BUF
                   CDD-ERROR
               END-CALL
               IF NOT CDD-OK OR WS-RBF-LEN < 50
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
           END-IF.
      * @paragraph VERIFY-GEN-DOC-COVERAGE Run MCDDDOCC on emitted UDT.
       VERIFY-GEN-DOC-COVERAGE.
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/gen_udt.cpy" TO CDD-PATH-VAL
           MOVE 17 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ WS-COB-BUF CDD-ERROR
           END-CALL
           MOVE "build/gen_udt.lst" TO CDD-PATH-VAL
           MOVE 17 TO CDD-PATH-LEN
           MOVE "build/gen_udt.cpy" TO WS-RBF-DATA(1:17)
           MOVE 17 TO WS-RBF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ WS-RT-BUF CDD-ERROR
           END-CALL
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           MOVE 1 TO CDD-AGV-QUIET
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "build/gen_udt.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDDOCC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-AGV-MODE
           MOVE 0 TO CDD-AGV-QUIET.
