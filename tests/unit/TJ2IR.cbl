      * @file TJ2IR.cbl
      * @brief Unit test suite for CDDJ2IR JSON/Schema -> IR mapper.
      * @module TJ2IR
      * @details Exercises tag/dialect/UDT-kind guards, explicit and
      *          auto schema/sample modes, name disambiguation (-1..-9),
      *          enum condition naming, and 1960..2023 era adaptations.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJ2IR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @subprogram TJ2IR Verify 100% of CDDJ2IR lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDJ2IR test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-UDT-KINDS
           PERFORM TEST-SCHEMA-SAMPLE-AND-COLLIDE
           IF WS-FAILS > 0
               DISPLAY "TJ2IR FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-UDT-KINDS Test tags & era UDT kinds.
       TEST-GUARDS-AND-UDT-KINDS.
           MOVE "BADMPR00" TO CDD-MPR-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 99 TO CDD-MPR-DIALECT
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-MPR-DIALECT
           MOVE 0 TO CDD-MPR-UDT-KIND
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-UDT-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 4 TO CDD-MPR-UDT-KIND
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-UDT-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-MPR-UDT-KIND
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-INVALID-UDT-KIND
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE "!" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-TOKEN
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "{" TO CDD-BUF-DATA(1:1)
           MOVE 1 TO CDD-BUF-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-EOF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-SCHEMA-SAMPLE-AND-COLLIDE Test modes & eras.
       TEST-SCHEMA-SAMPLE-AND-COLLIDE.
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"Ord","properties":{' DELIMITED BY SIZE
                  '"ord":{"type":"string","enum":["A","B"]},'
                      DELIMITED BY SIZE
                  '"a_b":{"type":"integer"},' DELIMITED BY SIZE
                  '"a-b":{"type":"boolean"},' DELIMITED BY SIZE
                  '"ad":{"type":"array","items":{"type":"string"}},'
                      DELIMITED BY SIZE
                  '"ao":{"type":"array","minItems":1,"maxItems":5,'
                      DELIMITED BY SIZE
                  '"items":{"type":"integer"}}}}' DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE 1 TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 1 TO CDD-MPR-JSON-MODE
           MOVE 0 TO CDD-MPR-NAME-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT NOT = 5
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-MPR-DIALECT
           MOVE 0 TO CDD-MPR-JSON-MODE
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO CDD-MPR-DIALECT
           MOVE 2 TO CDD-MPR-UDT-KIND
           MOVE 6 TO CDD-MPR-NAME-LEN
           MOVE "CUST-T" TO CDD-MPR-TARGET-NAME
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-MPR-NAME-LEN
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 2 TO CDD-MPR-JSON-MODE
           MOVE '{"x":1,"y":"hi"}' TO CDD-BUF-DATA(1:16)
           MOVE 16 TO CDD-BUF-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-MPR-JSON-MODE
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "42" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
