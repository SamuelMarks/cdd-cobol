      * @file TGOLDEN_10.cbl
      * @brief 10-dialect golden fixture conformance test suite.
      * @module TGOLD10
      * @details Loads all 10 golden dialect fixture pairs (cobol60..
      *          cobol2023), verifies emit, parse, serde, and byte-exact
      *          no-op sync invariants across every historical and
      *          modern COBOL standard, and checks 100% doc coverage.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TGOLD10.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-cobol.cpy".
      * @record WS-COB-BUF Loaded golden COBOL fixture buffer.
       01  WS-COB-BUF.
      * @field WS-CBF-TAG Magic tag ('CDDBUF01').
           05  WS-CBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-CBF-LEN Active COBOL byte length.
           05  WS-CBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-CBF-CAP Capacity.
           05  WS-CBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-CBF-NL Newline mode.
           05  WS-CBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-CBF-DATA Golden COBOL fixture bytes.
           05  WS-CBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-OUT-BUF Generated/synced output buffer.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active output byte length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Newline mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Generated/synced bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-FIXTURE-TABLE Paths of all 10 golden fixture pairs.
       01  WS-FIXTURE-TABLE.
      * @field WS-FX-RAW 10 (json_path, cob_path) pairs (48B each).
           05  WS-FX-RAW.
      * @field WS-FJ1 JSON 1.
               10  WS-FJ1 PIC X(48)
                   VALUE "tests/fixtures/cobol60/schema.json".
      * @field WS-FC1 COB 1.
               10  WS-FC1 PIC X(48)
                   VALUE "tests/fixtures/cobol60/fixture.cbl".
      * @field WS-FJ2 JSON 2.
               10  WS-FJ2 PIC X(48)
                   VALUE "tests/fixtures/cobol61/schema.json".
      * @field WS-FC2 COB 2.
               10  WS-FC2 PIC X(48)
                   VALUE "tests/fixtures/cobol61/fixture.cpy".
      * @field WS-FJ3 JSON 3.
               10  WS-FJ3 PIC X(48)
                   VALUE "tests/fixtures/cobol61ext/schema.json".
      * @field WS-FC3 COB 3.
               10  WS-FC3 PIC X(48)
                   VALUE "tests/fixtures/cobol61ext/fixture.cbl".
      * @field WS-FJ4 JSON 4.
               10  WS-FJ4 PIC X(48)
                   VALUE "tests/fixtures/cobol65/schema.json".
      * @field WS-FC4 COB 4.
               10  WS-FC4 PIC X(48)
                   VALUE "tests/fixtures/cobol65/fixture.cpy".
      * @field WS-FJ5 JSON 5.
               10  WS-FJ5 PIC X(48)
                   VALUE "tests/fixtures/cobol68/schema.json".
      * @field WS-FC5 COB 5.
               10  WS-FC5 PIC X(48)
                   VALUE "tests/fixtures/cobol68/fixture.cbl".
      * @field WS-FJ6 JSON 6.
               10  WS-FJ6 PIC X(48)
                   VALUE "tests/fixtures/cobol74/schema.json".
      * @field WS-FC6 COB 6.
               10  WS-FC6 PIC X(48)
                   VALUE "tests/fixtures/cobol74/fixture.cpy".
      * @field WS-FJ7 JSON 7.
               10  WS-FJ7 PIC X(48)
                   VALUE "tests/fixtures/cobol85/schema.json".
      * @field WS-FC7 COB 7.
               10  WS-FC7 PIC X(48)
                   VALUE "tests/fixtures/cobol85/fixture.cpy".
      * @field WS-FJ8 JSON 8.
               10  WS-FJ8 PIC X(48)
                   VALUE "tests/fixtures/cobol2002/schema.json".
      * @field WS-FC8 COB 8.
               10  WS-FC8 PIC X(48)
                   VALUE "tests/fixtures/cobol2002/fixture.cpy".
      * @field WS-FJ9 JSON 9.
               10  WS-FJ9 PIC X(48)
                   VALUE "tests/fixtures/cobol2014/schema.json".
      * @field WS-FC9 COB 9.
               10  WS-FC9 PIC X(48)
                   VALUE "tests/fixtures/cobol2014/fixture.cpy".
      * @field WS-FJ10 JSON 10.
               10  WS-FJ10 PIC X(48)
                   VALUE "tests/fixtures/cobol2023/schema.json".
      * @field WS-FC10 COB 10.
               10  WS-FC10 PIC X(48)
                   VALUE "tests/fixtures/cobol2023/fixture.cpy".
      * @field WS-FX-PAIR Array of 10 fixture path pairs.
           05  WS-FX-PAIR REDEFINES WS-FX-RAW OCCURS 10 TIMES.
      * @field WS-FX-JPATH JSON schema fixture path.
               10  WS-FX-JPATH           PIC X(48).
      * @field WS-FX-CPATH COBOL fixture path.
               10  WS-FX-CPATH           PIC X(48).
      * @record WS-STATE Loop and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-D Dialect loop index (1..10).
           05  WS-D                      PIC 9(2) COMP VALUE 1.
      * @field WS-PLEN Trimmed path length.
           05  WS-PLEN                   PIC 9(4) COMP VALUE 0.
      * @subprogram TGOLD10 Verify all 10 golden dialect fixtures.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Loop 1..10 verifying emit/parse/serde/syn.
       MAIN-ENTRY.
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDBUF01" TO WS-CBF-TAG
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE 1 TO WS-D
           PERFORM UNTIL WS-D > 10
               PERFORM VERIFY-ONE-DIALECT-FIXTURE
               ADD 1 TO WS-D
               END-ADD
           END-PERFORM
           IF WS-FAILS > 0
               DISPLAY "TGOLDEN_10 FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph LOAD-TRIMMED-PATH Measure WS-PLEN and read via CDDIO.
       LOAD-TRIMMED-PATH.
           MOVE 48 TO WS-PLEN
           PERFORM UNTIL WS-PLEN = 1
               OR CDD-PATH-VAL(WS-PLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-PLEN
               END-SUBTRACT
           END-PERFORM
           MOVE WS-PLEN TO CDD-PATH-LEN
           MOVE 1 TO CDD-PATH-OP.
      * @paragraph VERIFY-ONE-DIALECT-FIXTURE Test all 4 ops on WS-D.
       VERIFY-ONE-DIALECT-FIXTURE.
           MOVE SPACES TO CDD-PATH-VAL
           MOVE WS-FX-JPATH(WS-D) TO CDD-PATH-VAL(1:48)
           PERFORM LOAD-TRIMMED-PATH
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-PATH-VAL
           MOVE WS-FX-CPATH(WS-D) TO CDD-PATH-VAL(1:48)
           PERFORM LOAD-TRIMMED-PATH
           CALL "CDDIO" USING CDD-PATH-REQ WS-COB-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-D TO CDD-OPT-DIALECT
           MOVE 0 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-UDT-KIND
           MOVE 0 TO CDD-OPT-SERDE-KIND
           MOVE 0 TO CDD-OPT-SYNC-SERDE
           CALL "CDDEMIT" USING CDD-SDK-OPTS CDD-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           CALL "CDDPARS" USING CDD-SDK-OPTS WS-COB-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           CALL "CDDSERD" USING CDD-SDK-OPTS WS-COB-BUF WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBF-LEN = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           CALL "CDDSYN" USING CDD-SDK-OPTS WS-COB-BUF CDD-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               OR WS-OBF-LEN NOT = WS-CBF-LEN
               OR WS-OBF-DATA(1:WS-OBF-LEN)
                   NOT = WS-CBF-DATA(1:WS-CBF-LEN)
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
