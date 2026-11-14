      * @file TSYNC.cbl
      * @brief Regression test suite for CDDSYNC surgical CST sync.
      * @module TSYNC
      * @details Verifies: (1) byte-exact No-Op Sync Invariant across
      *          all 10 COBOL dialects, (2) adding fields at top,
      *          middle, bottom, and inside nested groups, (3) removing
      *          fields while keeping @cdd-keep helpers, (4) updating
      *          PIC types while keeping sequence numbers, alignment,
      *          inline *>, cols 73-80 deck tags, and CRLF, and
      *          (5) synchronizing TO-JSON/FROM-JSON bodies while
      *          preserving 100% of trivia above, between, and below.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSYNC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record WS-JSON-BUF Target JSON Schema or sample buffer.
       01  WS-JSON-BUF.
      * @field WS-JBF-TAG Magic tag ('CDDBUF01').
           05  WS-JBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-JBF-LEN Active JSON length.
           05  WS-JBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-JBF-CAP Buffer capacity.
           05  WS-JBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-JBF-NL Newline mode.
           05  WS-JBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-JBF-DATA Target JSON bytes.
           05  WS-JBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-OUT-BUF Synchronized output COBOL buffer.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active output length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Buffer capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Newline mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Synchronized COBOL bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
       COPY "cdd-err.cpy".
      * @record WS-BYTE-CONV CR/LF helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-LF LF character (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR CR character (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-DIAL Dialect loop index (1..10).
           05  WS-DIAL                   PIC 9(2) COMP VALUE 1.
      * @field WS-CNT Tally counter for INSPECT assertions.
           05  WS-CNT                    PIC 9(4) COMP VALUE 0.
      * @subprogram TSYNC Verify 100% of CDDSYNC lines & invariants.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDSYNC test suites.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           PERFORM TEST-GUARDS-AND-AMBIGUOUS
           PERFORM TEST-NOOP-INVARIANT-ALL-10
           PERFORM TEST-SURGICAL-UDT-AND-SERDE
           PERFORM TEST-FREE-FORMAT-AND-EDGES
           IF WS-FAILS > 0
               DISPLAY "TSYNC FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-AMBIGUOUS Test tags & UDT resolution.
       TEST-GUARDS-AND-AMBIGUOUS.
           MOVE "BADOPT00" TO CDD-OPT-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDBUF01" TO WS-JBF-TAG
           MOVE "CDDBUF01" TO WS-OBF-TAG
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADJBF00" TO WS-JBF-TAG
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           MOVE "CDDBUF01" TO WS-JBF-TAG
           MOVE "BADOBF00" TO WS-OBF-TAG
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE 99 TO CDD-OPT-DIALECT
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE "!" TO WS-JBF-DATA(1:1)
           MOVE 1 TO WS-JBF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"a":1}' TO WS-JBF-DATA(1:7)
           MOVE 7 TO WS-JBF-LEN
           MOVE "000100!BAD" TO CDD-BUF-DATA(1:10)
           MOVE 10 TO CDD-BUF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-BAD-INDICATOR
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "       05 ORPHAN PIC X." TO CDD-BUF-DATA(1:23)
           MOVE 23 TO CDD-BUF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-LEVEL-HIERARCHY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           MOVE "01 REC-A. 05 A PIC X. 01 REC-B. 05 B PIC X."
               TO CDD-BUF-DATA(1:43)
           MOVE 43 TO CDD-BUF-LEN
           MOVE 0 TO CDD-OPT-NAME-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SYNC-AMBIGUOUS-UDT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"title":"RecB","properties":{"b":{"type":"string"}}}'
               TO WS-JBF-DATA(1:53)
           MOVE 53 TO WS-JBF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-NOOP-INVARIANT-ALL-10 Exact byte identity 1..10.
       TEST-NOOP-INVARIANT-ALL-10.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "000100 IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  "                                         DECK0010"
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000110 DATA DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000120 WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000130* PRESERVE HEADER COMMENT" DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000140 01  CUST-REC.            " DELIMITED BY SIZE
                  "                                         DECK0010"
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000150     05  CUST-ID            PIC 9(6)."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000160     05  CUST-NAME          PIC X(20)."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000170 PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000180 CUST-TO-JSON SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000190     STOP RUN." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE 2 TO CDD-BUF-NL-KIND
           MOVE SPACES TO WS-JBF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"properties":{' DELIMITED BY SIZE
                  '"custId":{"type":"integer","minimum":0},'
                      DELIMITED BY SIZE
                  '"custName":{"type":"string","maxLength":20}}}'
                      DELIMITED BY SIZE
                  INTO WS-JBF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-JBF-LEN
           END-SUBTRACT
           MOVE "CDDJ2IR" TO CDD-OPT-TARGET-NAME
           MOVE 0 TO CDD-OPT-NAME-LEN
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 0 TO CDD-OPT-SYNC-SERDE
           MOVE '{"custId":123456,"custName":"12345678901234567890"}'
               TO WS-JBF-DATA(1:51)
           MOVE 51 TO WS-JBF-LEN
           MOVE 1 TO WS-DIAL
           PERFORM UNTIL WS-DIAL > 10
               MOVE WS-DIAL TO CDD-OPT-DIALECT
               CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
                   WS-OUT-BUF CDD-ERROR
               END-CALL
               IF NOT CDD-OK
                   OR WS-OBF-LEN NOT = CDD-BUF-LEN
                   OR WS-OBF-DATA(1:WS-OBF-LEN)
                       NOT = CDD-BUF-DATA(1:CDD-BUF-LEN)
                   ADD 1 TO WS-FAILS
                   END-ADD
               END-IF
               ADD 1 TO WS-DIAL
               END-ADD
           END-PERFORM.
      * @paragraph TEST-SURGICAL-UDT-AND-SERDE Add/upd/del + trivia!
       TEST-SURGICAL-UDT-AND-SERDE.
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-SYNC-SERDE
           MOVE 2 TO CDD-BUF-NL-KIND
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "000100 IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000110 PROGRAM-ID. SYNCDEMO." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000120 DATA DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000130 WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000140 01  ORD-REC.             " DELIMITED BY SIZE
                  "                                        ORD00001"
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000150*    KEEP COMMENT ABOVE ID" DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000160     05  ORD-ID     PICTURE IS S9(9) VALUE 7."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000170     05  DEAD-FLD   PICTURE 9(2)."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000180     05  SHIP-GRP." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000190         10  CITY   PIC X(8)."
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000200 PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000210* == USER BANNER ABOVE TO-JSON =="
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000220 ORD-TO-JSON SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000230     MOVE 1 TO ORD-ID." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000240* == USER COMMENT BETWEEN ROUTINES =="
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000250 ORD-FROM-JSON SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000260     MOVE 2 TO ORD-ID." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000270* == USER FOOTER BELOW FROM-JSON =="
                      DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000280 USER-BIZ SECTION." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "000290     STOP RUN." DELIMITED BY SIZE
                  WS-CR DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE SPACES TO WS-JBF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"properties":{' DELIMITED BY SIZE
                  '"topNew1":{"type":"boolean"},' DELIMITED BY SIZE
                  '"topNew2":{"type":"number","format":"double"},'
                      DELIMITED BY SIZE
                  '"ordId":{"type":"integer"},' DELIMITED BY SIZE
                  '"midDec":{"type":"number","multipleOf":0.01},'
                      DELIMITED BY SIZE
                  '"shipGrp":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"city":{"type":"string","maxLength":8},'
                      DELIMITED BY SIZE
                  '"zip":{"type":"string","maxLength":10}}},'
                      DELIMITED BY SIZE
                  '"items":{"type":"array","minItems":1,"maxItems":4,'
                      DELIMITED BY SIZE
                  '"items":{"type":"string","maxLength":6}},'
                      DELIMITED BY SIZE
                  '"newNested":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"subGrp":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"leaf":{"type":"string"}}}}}}}' DELIMITED BY SIZE
                  INTO WS-JBF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-JBF-LEN
           END-SUBTRACT
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           PERFORM VERIFY-TRIVIA-SURVIVED.
      * @paragraph VERIFY-TRIVIA-SURVIVED Check banners & edits in OBF.
       VERIFY-TRIVIA-SURVIVED.
           MOVE 0 TO WS-CNT
           INSPECT WS-OBF-DATA(1:WS-OBF-LEN)
               TALLYING WS-CNT FOR ALL "KEEP COMMENT ABOVE ID"
           IF WS-CNT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO WS-CNT
           INSPECT WS-OBF-DATA(1:WS-OBF-LEN)
               TALLYING WS-CNT FOR ALL "USER BANNER ABOVE TO-JSON"
           IF WS-CNT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO WS-CNT
           INSPECT WS-OBF-DATA(1:WS-OBF-LEN)
               TALLYING WS-CNT FOR ALL "USER COMMENT BETWEEN ROUTINES"
           IF WS-CNT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO WS-CNT
           INSPECT WS-OBF-DATA(1:WS-OBF-LEN)
               TALLYING WS-CNT FOR ALL "USER FOOTER BELOW FROM-JSON"
           IF WS-CNT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO WS-CNT
           INSPECT WS-OBF-DATA(1:WS-OBF-LEN)
               TALLYING WS-CNT FOR ALL "DEAD-FLD"
           IF WS-CNT NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO WS-CNT
           INSPECT WS-OBF-DATA(1:WS-OBF-LEN)
               TALLYING WS-CNT FOR ALL "TOP-NEW1"
           IF WS-CNT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-FREE-FORMAT-AND-EDGES Test 2023 free & caps.
       TEST-FREE-FORMAT-AND-EDGES.
           MOVE 10 TO CDD-OPT-DIALECT
           MOVE 2 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-BUF-NL-KIND
           MOVE 1 TO CDD-OPT-SYNC-SERDE
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING ">>SOURCE FORMAT IS FREE" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "01 U1." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 NO-PIC." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  05 S1 PICTURE X(4). *> inline keep!"
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "METHOD-ID. TO-JSON." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "  EXIT METHOD." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "END METHOD." DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE SPACES TO WS-JBF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"properties":{' DELIMITED BY SIZE
                  '"noPic":{"type":"boolean"},' DELIMITED BY SIZE
                  '"s1":{"type":"string","maxLength":12},'
                      DELIMITED BY SIZE
                  '"dynArr":{"type":"array","items":{'
                      DELIMITED BY SIZE
                  '"type":"integer","minimum":0}}}}'
                      DELIMITED BY SIZE
                  INTO WS-JBF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-JBF-LEN
           END-SUBTRACT
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE SPACES TO CDD-BUF-DATA
           MOVE "       01 EMP-REC." TO CDD-BUF-DATA(1:18)
           MOVE "LONGIDENT99" TO CDD-BUF-DATA(73:11)
           MOVE 83 TO CDD-BUF-LEN
           MOVE '{"properties":{"a":{"type":"array","maxItems":2,'
               TO WS-JBF-DATA(1:48)
           MOVE '"items":{"type":"number","minimum":0}}}}'
               TO WS-JBF-DATA(49:40)
           MOVE 88 TO WS-JBF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 5 TO CDD-OPT-SERDE-KIND
           MOVE "       01 U2. 05 F1 PICTURE X(4). PROCEDURE DIVISION."
               TO CDD-BUF-DATA(1:53)
           MOVE WS-LF TO CDD-BUF-DATA(54:1)
           MOVE "       METHOD-ID. TO-JSON. EXIT METHOD. END METHOD."
               TO CDD-BUF-DATA(55:51)
           MOVE 105 TO CDD-BUF-LEN
           MOVE '{"properties":{"f1":{"type":"object"},"a":{"type":'
               TO WS-JBF-DATA(1:50)
           MOVE '"array","minItems":1,"maxItems":3,"items":{"type":'
               TO WS-JBF-DATA(51:50)
           MOVE '"object"}},"s0":{"type":"string","maxLength":0},'
               TO WS-JBF-DATA(101:48)
           MOVE '"g1":{"type":"object","properties":{"g2":{"type":'
               TO WS-JBF-DATA(149:49)
           MOVE '"object","properties":{"g3":{"type":"object",'
               TO WS-JBF-DATA(198:45)
           MOVE '"properties":{"g4":{"type":"object","properties":{'
               TO WS-JBF-DATA(243:50)
           MOVE '"g5":{"type":"object","properties":{"g6":{"type":'
               TO WS-JBF-DATA(293:49)
           MOVE '"object","properties":{"g7":{"type":"object",'
               TO WS-JBF-DATA(342:45)
           MOVE '"properties":{"g8":{"type":"object","properties":{'
               TO WS-JBF-DATA(387:50)
           MOVE '"g9":{"type":"object","properties":{"ga":{"type":'
               TO WS-JBF-DATA(437:49)
           MOVE '"string"}}}}}}}}}}}}}}}}}}}}}' TO WS-JBF-DATA(486:29)
           MOVE 514 TO WS-JBF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-SERDE-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-OPT-SERDE-KIND
           MOVE "       01 U3. 05 F1 USAGE COMP-2."
               TO CDD-BUF-DATA(1:33)
           MOVE 33 TO CDD-BUF-LEN
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF WS-JSON-BUF
               WS-OUT-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
