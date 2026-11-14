      * @file TJSCH.cbl
      * @brief Unit test suite for CDDJSCH JSON Schema analyzer.
      * @module TJSCH
      * @details Exercises schema vs sample auto-detection, all Draft
      *          2020-12 / OpenAPI types, formats, bounds, multipleOf,
      *          enums, nullable unions, nested objects, arrays, $ref
      *          resolution, cycle detection, and capacity errors.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJSCH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-jsch.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @subprogram TJSCH Verify 100% of CDDJSCH lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDJSCH test groups.
       MAIN-ENTRY.
           PERFORM TEST-GUARDS-AND-DETECT
           PERFORM TEST-FULL-SCHEMA-FEATURES
           PERFORM TEST-REFS-AND-CYCLES
           PERFORM TEST-ERROR-AND-CAP-BRANCHES
           IF WS-FAILS > 0
               DISPLAY "TJSCH FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph PARSE-BUF-TO-DOM Lex and parse CDD-BUF into CDD-JDOM.
       PARSE-BUF-TO-DOM.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "CDDJSQ01" TO CDD-JSQ-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 4096 TO CDD-JTK-CAP
           MOVE 1024 TO CDD-JDM-CAP
           MOVE 32 TO CDD-JDM-MAX-DEPTH
           MOVE 1 TO CDD-JDM-REJECT-DUP
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF CDD-OK
               CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
                   CDD-ERROR
               END-CALL
           END-IF.
      * @paragraph TEST-GUARDS-AND-DETECT Test tags and OP-DETECT.
       TEST-GUARDS-AND-DETECT.
           MOVE "BADJSQ00" TO CDD-JSQ-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJSQ01" TO CDD-JSQ-TAG
           MOVE "BADJDM00" TO CDD-JDM-TAG
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 1 TO CDD-JSQ-OP
           MOVE 0 TO CDD-JDM-ROOT
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF CDD-JSQ-IS-SCHEMA NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-INVALID-ROOT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "[1,2]" TO CDD-BUF-DATA(1:5)
           MOVE 5 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 1 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-INVALID-ROOT
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"id":10}' TO CDD-BUF-DATA(1:9)
           MOVE 9 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 1 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-JSQ-DETECTED-SAMPLE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"$schema":"x","properties":{},"$defs":{}}'
               TO CDD-BUF-DATA(1:42)
           MOVE 42 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-JSQ-DETECTED-SCHEMA
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-FULL-SCHEMA-FEATURES Test all types & facets.
       TEST-FULL-SCHEMA-FEATURES.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"Order","description":"Root doc",'
                      DELIMITED BY SIZE
                  '"required":["id","s1"],' DELIMITED BY SIZE
                  '"properties":{' DELIMITED BY SIZE
                  '"id":{"type":"integer","format":"int32",'
                      DELIMITED BY SIZE
                  '"minimum":0,"maximum":999,"default":1},'
                      DELIMITED BY SIZE
                  '"i64":{"type":"integer","format":"int64",'
                      DELIMITED BY SIZE
                  '"minimum":-50,"maximum":50},' DELIMITED BY SIZE
                  '"iplain":{"type":"integer"},' DELIMITED BY SIZE
                  '"s1":{"type":"string","minLength":1,'
                      DELIMITED BY SIZE
                  '"maxLength":12,"description":"Str doc",'
                      DELIMITED BY SIZE
                  '"default":"abc"},' DELIMITED BY SIZE
                  '"sd":{"type":["string","null"],"format":"date",'
                      DELIMITED BY SIZE
                  '"default":""},' DELIMITED BY SIZE
                  '"st":{"type":"string","format":"time"},'
                      DELIMITED BY SIZE
                  '"sdt":{"type":"string","format":"date-time"},'
                      DELIMITED BY SIZE
                  '"su":{"type":"string","format":"uuid"},'
                      DELIMITED BY SIZE
                  '"sp":{"type":"string","format":"email"},'
                      DELIMITED BY SIZE
                  '"en1":{"enum":["LONG_OPTION_VALUE","B"]},'
                      DELIMITED BY SIZE
                  '"en2":{"enum":[10,20]},' DELIMITED BY SIZE
                  '"n1":{"type":"number","multipleOf":0.001,'
                      DELIMITED BY SIZE
                  '"minimum":0},' DELIMITED BY SIZE
                  '"n2":{"type":"number","multipleOf":5,'
                      DELIMITED BY SIZE
                  '"minimum":-10.5,"maximum":10.5},'
                      DELIMITED BY SIZE
                  '"nf":{"type":"number","format":"float"},'
                      DELIMITED BY SIZE
                  '"nd":{"type":"number","format":"double"},'
                      DELIMITED BY SIZE
                  '"ndc":{"type":"number","format":"decimal"},'
                      DELIMITED BY SIZE
                  '"b1":{"type":"boolean","default":true},'
                      DELIMITED BY SIZE
                  '"b2":{"type":"boolean","default":false},'
                      DELIMITED BY SIZE
                  '"nl":{"type":"null"},' DELIMITED BY SIZE
                  '"a1":{"type":"array","items":{"type":"string"}},'
                      DELIMITED BY SIZE
                  '"a2":{"type":"array","minItems":3,"maxItems":3,'
                      DELIMITED BY SIZE
                  '"items":{"type":"integer"}},' DELIMITED BY SIZE
                  '"a3":{"type":"array","minItems":1,"maxItems":4,'
                      DELIMITED BY SIZE
                  '"items":{"properties":{"k":{"type":"string"}}}},'
                      DELIMITED BY SIZE
                  '"sub":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"c1":{"type":"string"},"c2":{"type":"integer"}'
                      DELIMITED BY SIZE
                  '}}}}' DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-BUF-TO-DOM
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT < 20
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-REFS-AND-CYCLES Test $defs, definitions, cycle.
       TEST-REFS-AND-CYCLES.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"$ref":"#/$defs/Main","$defs":{'
                      DELIMITED BY SIZE
                  '"Main":{"title":"","description":"",'
                      DELIMITED BY SIZE
                  '"properties":{"p":{"$ref":"#/definitions/Leaf"}}}},'
                      DELIMITED BY SIZE
                  '"definitions":{"Leaf":{"type":"string"}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-BUF-TO-DOM
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-IR-FLD-COUNT NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"$ref":"#/$defs/Missing"}' TO CDD-BUF-DATA(1:26)
           MOVE 26 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-UNRESOLVED-REF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"type":"object"}' TO CDD-BUF-DATA(1:17)
           MOVE 17 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"properties":{' DELIMITED BY SIZE
                  '"s":{"type":[1,"very_long_type_name_over_32_chars",'
                      DELIMITED BY SIZE
                  '"string"],"description":"","format":"",'
                      DELIMITED BY SIZE
                  '"minLength":-1,"maxLength":-1,"default":null,'
                      DELIMITED BY SIZE
                  '"enum":123},' DELIMITED BY SIZE
                  '"s2":{"type":"string","description":1,'
                      DELIMITED BY SIZE
                  '"format":"very_long_format_over_16_chars"},'
                      DELIMITED BY SIZE
                  '"a":{"type":"array","minItems":-1,"maxItems":-1,'
                      DELIMITED BY SIZE
                  '"items":123},' DELIMITED BY SIZE
                  '"a2":{"type":"array","items":{"minItems":1,'
                      DELIMITED BY SIZE
                  '"type":"string"}}}}' DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"array","items":'
               TO CDD-BUF-DATA(1:43)
           MOVE '{"$ref":"#/$defs/No"}}}}' TO CDD-BUF-DATA(44:24)
           MOVE 67 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-UNRESOLVED-REF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"array","items":'
               TO CDD-BUF-DATA(1:43)
           MOVE '{"type":"string","minLength":5,"maxLength":1}}}}'
               TO CDD-BUF-DATA(44:48)
           MOVE 91 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-CONFLICT-BOUNDS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"x":{"$ref":"#/$defs/Missing"}}}'
               TO CDD-BUF-DATA(1:47)
           MOVE 47 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-UNRESOLVED-REF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"x":{"$ref":"external.json"}}}'
               TO CDD-BUF-DATA(1:45)
           MOVE 45 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-UNRESOLVED-REF
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"$defs":{"A":{"$ref":"#/$defs/B"},'
                      DELIMITED BY SIZE
                  '"B":{"$ref":"#/$defs/A"}},' DELIMITED BY SIZE
                  '"properties":{"x":{"$ref":"#/$defs/A"}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-REF-CYCLE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-ERROR-AND-CAP-BRANCHES Test bounds/type/cap err.
       TEST-ERROR-AND-CAP-BRANCHES.
           MOVE '{"properties":{"a":123}}' TO CDD-BUF-DATA(1:24)
           MOVE 24 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-BAD-TYPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":123}}}'
               TO CDD-BUF-DATA(1:33)
           MOVE 33 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-BAD-TYPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"bogus"}}}'
               TO CDD-BUF-DATA(1:37)
           MOVE 37 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-BAD-TYPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"string","minLength":9,'
               TO CDD-BUF-DATA(1:50)
           MOVE '"maxLength":2}}}' TO CDD-BUF-DATA(51:16)
           MOVE 66 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-CONFLICT-BOUNDS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"integer","minimum":9,'
               TO CDD-BUF-DATA(1:49)
           MOVE '"maximum":2}}}' TO CDD-BUF-DATA(50:14)
           MOVE 63 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-CONFLICT-BOUNDS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"array","minItems":5,'
               TO CDD-BUF-DATA(1:48)
           MOVE '"maxItems":2}}}' TO CDD-BUF-DATA(49:15)
           MOVE 63 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-CONFLICT-BOUNDS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE '{"properties":{"a":{"type":"string"}}}'
               TO CDD-BUF-DATA(1:38)
           MOVE 38 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 0 TO CDD-IR-FLD-CAP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IR-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE '{"properties":{"a":{"type":"string","enum":["x"]}}}'
               TO CDD-BUF-DATA(1:51)
           MOVE 51 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 0 TO CDD-IR-ENUM-CAP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IR-CAPACITY
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 256 TO CDD-IR-ENUM-CAP
           PERFORM TEST-LONG-TEXT-AND-REFDEPTH.
      * @paragraph TEST-LONG-TEXT-AND-REFDEPTH Long text & depth.
       TEST-LONG-TEXT-AND-REFDEPTH.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE '{"title":"' TO CDD-BUF-DATA(1:10)
           MOVE 11 TO WS-I
           PERFORM UNTIL WS-I > 145
               MOVE "T" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE '","properties":{"a":{"type":["null","string"],'
               TO CDD-BUF-DATA(146:46)
           MOVE '"description":"' TO CDD-BUF-DATA(192:15)
           MOVE 207 TO WS-I
           PERFORM UNTIL WS-I > 375
               MOVE "D" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE '","enum":["' TO CDD-BUF-DATA(376:11)
           MOVE 387 TO WS-I
           PERFORM UNTIL WS-I > 460
               MOVE "E" TO CDD-BUF-DATA(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE '",""]}}}' TO CDD-BUF-DATA(461:8)
           MOVE 468 TO CDD-BUF-LEN
           PERFORM PARSE-BUF-TO-DOM
           MOVE 2 TO CDD-JSQ-OP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"$defs":{' DELIMITED BY SIZE
                  '"R1":{"$ref":"#/$defs/R2"},' DELIMITED BY SIZE
                  '"R2":{"$ref":"#/$defs/R3"},' DELIMITED BY SIZE
                  '"R3":{"$ref":"#/$defs/R4"},' DELIMITED BY SIZE
                  '"R4":{"$ref":"#/$defs/R5"},' DELIMITED BY SIZE
                  '"R5":{"$ref":"#/$defs/R6"},' DELIMITED BY SIZE
                  '"R6":{"$ref":"#/$defs/R7"},' DELIMITED BY SIZE
                  '"R7":{"$ref":"#/$defs/R8"},' DELIMITED BY SIZE
                  '"R8":{"$ref":"#/$defs/R9"},' DELIMITED BY SIZE
                  '"R9":{"$ref":"#/$defs/RA"},' DELIMITED BY SIZE
                  '"RA":{"$ref":"#/$defs/RB"},' DELIMITED BY SIZE
                  '"RB":{"$ref":"#/$defs/RC"},' DELIMITED BY SIZE
                  '"RC":{"$ref":"#/$defs/RD"},' DELIMITED BY SIZE
                  '"RD":{"$ref":"#/$defs/RE"},' DELIMITED BY SIZE
                  '"RE":{"$ref":"#/$defs/RF"},' DELIMITED BY SIZE
                  '"RF":{"$ref":"#/$defs/RG"},' DELIMITED BY SIZE
                  '"RG":{"$ref":"#/$defs/RH"},' DELIMITED BY SIZE
                  '"RH":{"type":"string"}},' DELIMITED BY SIZE
                  '"properties":{"x":{"$ref":"#/$defs/R1"}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           PERFORM PARSE-BUF-TO-DOM
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-SCHEMA-REF-CYCLE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
