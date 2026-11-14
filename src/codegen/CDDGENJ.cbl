      * @file CDDGENJ.cbl
      * @brief Feature 1 (parse): COBOL UDT buffer -> Schema or Sample.
      * @module CDDGENJ
      * @details Orchestrates CDDCLEX, CDDCSTB, CDDC2IR, and CDDJEMT to
      *          extract 01 records, TYPEDEFs, or CLASS-IDs across all
      *          10 COBOL dialects and emit Draft 2020-12 JSON Schema
      *          or default/sample JSON payloads.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDGENJ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-jsch.cpy".
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record CDD-OUT-BUF Output JSON Schema or sample JSON buffer.
       01  CDD-OUT-BUF.
      * @field CDD-OBF-TAG Magic type-safety tag ('CDDBUF01').
           05  CDD-OBF-TAG               PIC X(8).
      * @field CDD-OBF-TAG-VALID Condition confirming valid OBF tag.
               88  CDD-OBF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-OBF-LEN Active output byte length.
           05  CDD-OBF-LEN               PIC 9(8) COMP.
      * @field CDD-OBF-CAP Output buffer capacity.
           05  CDD-OBF-CAP               PIC 9(8) COMP.
      * @field CDD-OBF-NL-KIND Output newline mode (1=LF, 2=CRLF).
           05  CDD-OBF-NL-KIND           PIC 9(2) COMP.
      * @field CDD-OBF-DATA Serialized JSON output bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDGENJ Parse COBOL UDT buffer to JSON/Schema.
      * @param [in] CDD-SDK-OPTS Dialect, format, mode, and case opts.
      * @param [in] CDD-BUF Input COBOL program or copybook buffer.
      * @param [inout] CDD-OUT-BUF Output JSON Schema or sample buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Emitted JSON Schema or sample JSON in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, lexer/CST/UDT/emitter errs.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-BUF CDD-OUT-BUF
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, lex, CST, C2IR, and emit.
       MAIN-ENTRY.
           IF NOT CDD-OPT-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               OR NOT CDD-OBF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDGENJ" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDGENJ"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM EXTRACT-IR-FROM-COBOL
           IF CDD-OK
               PERFORM EMIT-JSON-OUTPUT
           END-IF
           EXIT PROGRAM.
      * @paragraph EXTRACT-IR-FROM-COBOL Run CDDCLEX, CDDCSTB, CDDC2IR.
       EXTRACT-IR-FROM-COBOL.
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           MOVE CDD-OPT-DIALECT TO CDD-CTK-DIALECT
           MOVE CDD-OPT-FORMAT TO CDD-CTK-FMT-REQ
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 512 TO CDD-CST-CAP
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           IF CDD-OK
               CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   CDD-ERROR
               END-CALL
           END-IF
           IF CDD-OK
               MOVE CDD-OPT-DIALECT TO CDD-MPR-DIALECT
               MOVE CDD-OPT-UDT-KIND TO CDD-MPR-UDT-KIND
               MOVE CDD-OPT-CASE TO CDD-MPR-CASE
               MOVE 0 TO CDD-MPR-STRICT-UDT
               MOVE CDD-OPT-NAME-LEN TO CDD-MPR-NAME-LEN
               MOVE CDD-OPT-TARGET-NAME TO CDD-MPR-TARGET-NAME
               CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF
                   CDD-CTOK-STREAM CDD-CST CDD-IR CDD-IR-FIELD-MAP
                   CDD-ERROR
               END-CALL
           END-IF.
      * @paragraph EMIT-JSON-OUTPUT Invoke CDDJEMT for schema/sample.
       EMIT-JSON-OUTPUT.
           MOVE "CDDJEM01" TO CDD-JEM-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           IF CDD-OPT-JMODE-SAMPLE
               MOVE 2 TO CDD-JEM-MODE
           ELSE
               MOVE 1 TO CDD-JEM-MODE
           END-IF
           IF CDD-OPT-STYLE = 2
               MOVE 2 TO CDD-JEM-STYLE
           ELSE
               MOVE 1 TO CDD-JEM-STYLE
           END-IF
           MOVE CDD-OPT-X-COBOL TO CDD-JEM-X-COBOL
           CALL "CDDJEMT" USING CDD-JEMT-REQ CDD-IR CDD-JDOM
               CDD-OUT-BUF CDD-ERROR
           END-CALL.
