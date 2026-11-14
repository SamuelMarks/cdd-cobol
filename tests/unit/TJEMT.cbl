      * @file TJEMT.cbl
      * @brief Unit test suite for CDDJEMT JSON Schema & JSON emitter.
      * @module TJEMT
      * @details Exercises Schema emission (pretty/compact, LF/CRLF,
      *          x-cobol metadata, empty & nested groups, all types,
      *          bounds, enums, arrays), Sample JSON emission, DOM
      *          serialization, and buffer capacity overflow.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJEMT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-jsch.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @record WS-OUT-BUF Output JSON buffer for CDDJEMT calls.
       01  WS-OUT-BUF.
      * @field WS-OBUF-TAG Magic type-safety tag ('CDDBUF01').
           05  WS-OBUF-TAG               PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBUF-LEN Active emitted byte count.
           05  WS-OBUF-LEN               PIC 9(8) COMP VALUE 0.
      * @field WS-OBUF-CAP Output buffer capacity.
           05  WS-OBUF-CAP               PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBUF-NL Output line-ending mode (1=LF, 2=CRLF).
           05  WS-OBUF-NL                PIC 9(2) COMP VALUE 1.
      * @field WS-OBUF-DATA Output JSON character storage.
           05  WS-OBUF-DATA              PIC X(1048576) VALUE SPACES.
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @subprogram TJEMT Verify 100% of CDDJEMT lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDJEMT unit test groups.
       MAIN-ENTRY.
           PERFORM TEST-ENVELOPE-GUARDS
           PERFORM TEST-EMPTY-AND-DOM-MODES
           PERFORM TEST-RICH-SCHEMA-AND-SAMPLE
           IF WS-FAILS > 0
               DISPLAY "TJEMT FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-ENVELOPE-GUARDS Check all 4 tag validations.
       TEST-ENVELOPE-GUARDS.
           MOVE "BADJEM00" TO CDD-JEM-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "CDDBUF01" TO WS-OBUF-TAG
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJEM01" TO CDD-JEM-TAG
           MOVE "BADIR000" TO CDD-IR-TAG
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "BADJDM00" TO CDD-JDM-TAG
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "BADBUF00" TO WS-OBUF-TAG
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO WS-OBUF-TAG.
      * @paragraph TEST-EMPTY-AND-DOM-MODES Test empty IR & DOM emit.
       TEST-EMPTY-AND-DOM-MODES.
           MOVE 0 TO CDD-IR-TITLE-LEN
           MOVE 0 TO CDD-IR-DESC-LEN
           MOVE 0 TO CDD-IR-ROOT-FIRST
           MOVE 0 TO CDD-IR-FLD-COUNT
           MOVE 1 TO CDD-JEM-MODE
           MOVE 1 TO CDD-JEM-STYLE
           MOVE 1 TO WS-OBUF-NL
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-JEM-MODE
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-JEM-MODE
           MOVE 0 TO CDD-JDM-ROOT
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE 4096 TO CDD-JTK-CAP
           MOVE 1024 TO CDD-JDM-CAP
           MOVE 32 TO CDD-JDM-MAX-DEPTH
           MOVE 1 TO CDD-JDM-REJECT-DUP
           MOVE '{"a":[1,2.5,"x","",true,false,null,{}],"b":[]}'
               TO CDD-BUF-DATA(1:46)
           MOVE 46 TO CDD-BUF-LEN
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           MOVE 3 TO CDD-JEM-MODE
           MOVE 1 TO CDD-JEM-STYLE
           MOVE 2 TO WS-OBUF-NL
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR WS-OBUF-LEN < 40
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-JEM-STYLE
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-RICH-SCHEMA-AND-SAMPLE Round-trip schema/sample.
       TEST-RICH-SCHEMA-AND-SAMPLE.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"Rec","description":"Doc",'
                      DELIMITED BY SIZE
                  '"required":["i1","s1"],' DELIMITED BY SIZE
                  '"properties":{' DELIMITED BY SIZE
                  '"i1":{"type":"integer","minimum":-5,"maximum":9,'
                      DELIMITED BY SIZE
                  '"default":3},' DELIMITED BY SIZE
                  '"i2":{"type":"integer"},' DELIMITED BY SIZE
                  '"s1":{"type":"string","minLength":2,"maxLength":8,'
                      DELIMITED BY SIZE
                  '"format":"date","description":"Fdoc",'
                      DELIMITED BY SIZE
                  '"default":"hi","enum":["hi","lo"]},'
                      DELIMITED BY SIZE
                  '"sn":{"type":["string","null"]},' DELIMITED BY SIZE
                  '"ie":{"type":"integer","enum":[1,2]},'
                      DELIMITED BY SIZE
                  '"d1":{"type":"number","multipleOf":0.01,'
                      DELIMITED BY SIZE
                  '"default":1.25},' DELIMITED BY SIZE
                  '"d2":{"type":"number","format":"float"},'
                      DELIMITED BY SIZE
                  '"b1":{"type":"boolean","default":true},'
                      DELIMITED BY SIZE
                  '"b2":{"type":"boolean"},' DELIMITED BY SIZE
                  '"af":{"type":"array","minItems":2,"maxItems":2,'
                      DELIMITED BY SIZE
                  '"items":{"type":"string"}},' DELIMITED BY SIZE
                  '"ao":{"type":"array","minItems":1,"maxItems":4,'
                      DELIMITED BY SIZE
                  '"items":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"k":{"type":"string"}}}},' DELIMITED BY SIZE
                  '"ad":{"type":"array","items":{"type":"integer"}},'
                      DELIMITED BY SIZE
                  '"eg":{"type":"object","properties":{}},'
                      DELIMITED BY SIZE
                  '"g2":{"type":"object","required":["x"],'
                      DELIMITED BY SIZE
                  '"properties":{"x":{"type":"string"}}}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
               CDD-ERROR
           END-CALL
           MOVE "CDDJSQ01" TO CDD-JSQ-TAG
           MOVE 2 TO CDD-JSQ-OP
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-JEM-MODE
           MOVE 1 TO CDD-JEM-STYLE
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 5 TO CDD-IRF-PIC-LEN(1)
           MOVE "S9(4)" TO CDD-IRF-PIC-TEXT(1)
           MOVE 0 TO CDD-IRF-MAX-LEN(4)
           ADD 1 TO CDD-IR-FLD-COUNT
           END-ADD
           MOVE 0 TO CDD-IRF-PARENT(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-NEXT-SIB(CDD-IR-FLD-COUNT)
           MOVE CDD-IR-FLD-COUNT TO CDD-IRF-NEXT-SIB(CDD-IR-ROOT-LAST)
           MOVE CDD-IR-FLD-COUNT TO CDD-IR-ROOT-LAST
           MOVE 3 TO CDD-IRF-JKEY-LEN(CDD-IR-FLD-COUNT)
           MOVE "red" TO CDD-IRF-JSON-KEY(CDD-IR-FLD-COUNT)
           MOVE 2 TO CDD-IRF-KIND(CDD-IR-FLD-COUNT)
           MOVE 1 TO CDD-IRF-REQUIRED(CDD-IR-FLD-COUNT)
           MOVE 2 TO CDD-IRF-REDEF-LEN(CDD-IR-FLD-COUNT)
           MOVE "I1" TO CDD-IRF-REDEFINES(CDD-IR-FLD-COUNT)
           MOVE 4 TO CDD-IRF-MAX-LEN(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-MIN-LEN(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-DESC-LEN(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-ARR-MODE(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-NULLABLE(CDD-IR-FLD-COUNT)
           MOVE SPACES TO CDD-IRF-FORMAT(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-HAS-BOUNDS(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-ENUM-COUNT(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-USAGE(CDD-IR-FLD-COUNT)
           MOVE 0 TO CDD-IRF-PIC-LEN(CDD-IR-FLD-COUNT)
           MOVE 1 TO CDD-JEM-MODE
           MOVE 1 TO CDD-JEM-STYLE
           MOVE 1 TO CDD-JEM-X-COBOL
           MOVE 1 TO WS-OBUF-NL
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-JEM-STYLE
           MOVE 0 TO CDD-JEM-X-COBOL
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-JEM-MODE
           MOVE 1 TO CDD-JEM-STYLE
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 10 TO WS-OBUF-CAP
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM WS-OUT-BUF
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
