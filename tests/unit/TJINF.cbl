      * @file TJINF.cbl
      * @brief Unit test suite for CDDJINF raw JSON payload inferrer.
      * @module TJINF
      * @details Exercises tag checks, object and object-array sample
      *          merging, string/date/uuid/empty length widening,
      *          int/decimal/float promotion, boolean and null-only
      *          handling, nested groups, and type conflict errors.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJINF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @subprogram TJINF Verify 100% of CDDJINF lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDJINF test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-INVALID-ROOTS
           PERFORM TEST-RICH-SAMPLE-MERGE
           PERFORM TEST-TYPE-CONFLICTS-AND-CAP
           IF WS-FAILS > 0
               DISPLAY "TJINF FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph PARSE-JSON Lex and parse CDD-BUF into CDD-JDOM.
       PARSE-JSON.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 4096 TO CDD-JTK-CAP
           MOVE 1024 TO CDD-JDM-CAP
           MOVE 32 TO CDD-JDM-MAX-DEPTH
           MOVE 1 TO CDD-JDM-REJECT-DUP
           MOVE 256 TO CDD-IR-FLD-CAP
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL.
      * @paragraph TEST-GUARDS-AND-INVALID-ROOTS Test tags & bad roots.
       TEST-GUARDS-AND-INVALID-ROOTS.
           MOVE "BADJDM00" TO CDD-JDM-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 0 TO CDD-JDM-ROOT
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "42" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[]" TO CDD-BUF-DATA(1:2)
           MOVE 2 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '[{"a":1}, 2]' TO CDD-BUF-DATA(1:12)
           MOVE 12 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-RICH-SAMPLE-MERGE Merge single & array samples.
       TEST-RICH-SAMPLE-MERGE.
           MOVE '{"only":1}' TO CDD-BUF-DATA(1:10)
           MOVE 10 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '[{"id":999,"amt":1234.567},{"id":5,"amt":1.2}]'
               TO CDD-BUF-DATA(1:46)
           MOVE 46 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '[{"id":5,"amt":10,"flt":1e2,"dt":"2026-11-14",'
                      DELIMITED BY SIZE
                  '"uid":"123e4567-e89b-12d3-a456-426614174000",'
                      DELIMITED BY SIZE
                  '"empty":"","b1":true,"b2":false,"nil":null,'
                      DELIMITED BY SIZE
                  '"arr":[],"tags":["a","abcd"],' DELIMITED BY SIZE
                  '"sub":{"c1":1,"c2":"hi"}},' DELIMITED BY SIZE
                  '{"id":-999,"amt":1234.567,"flt":2,"dt":"2026-/1-9:",'
                      DELIMITED BY SIZE
                  '"empty":"now","b1":false,"tags":["x"],'
                      DELIMITED BY SIZE
                  '"sub":{"c1":22,"c3":true}}]' DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT < 12
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-TYPE-CONFLICTS-AND-CAP Test all 1106/3001 errs.
       TEST-TYPE-CONFLICTS-AND-CAP.
           MOVE '{"a":[[1]]}' TO CDD-BUF-DATA(1:11)
           MOVE 11 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '[{"a":1},{"a":{}}]' TO CDD-BUF-DATA(1:18)
           MOVE 18 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '[{"a":{}},{"a":"x"}]' TO CDD-BUF-DATA(1:20)
           MOVE 20 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '[{"a":"x"},{"a":1}]' TO CDD-BUF-DATA(1:19)
           MOVE 19 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '[{"a":1},{"a":true}]' TO CDD-BUF-DATA(1:20)
           MOVE 20 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-INFER-EMPTY-CONFLICT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"a":1}' TO CDD-BUF-DATA(1:7)
           MOVE 7 TO CDD-BUF-LEN
           PERFORM PARSE-JSON
           MOVE 0 TO CDD-IR-FLD-CAP
           CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IR-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
