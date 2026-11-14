      * @file CDDEMIT.cbl
      * @brief Public SDK Op 0 (emit): JSON/Schema -> COBOL UDT.
      * @module CDDEMIT
      * @details Public SDK entry point validating all envelope tags
      *          and converting a JSON Schema or sample JSON buffer
      *          into a 100% doc-covered COBOL UDT for any of the 10
      *          supported COBOL standards (COBOL-60..COBOL-2023).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDEMIT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record CDD-OUT-BUF Output COBOL UDT byte buffer.
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
      * @field CDD-OBF-DATA Emitted COBOL UDT bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDEMIT Convert JSON/Schema buffer to COBOL UDT.
      * @param [in] CDD-SDK-OPTS Dialect, UDT kind, format, and opts.
      * @param [in] CDD-BUF Input JSON Schema or sample JSON buffer.
      * @param [inout] CDD-OUT-BUF Output COBOL UDT source buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Emitted COBOL UDT in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, JSON/Schema/Codegen errors.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-BUF CDD-OUT-BUF
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, map JSON->IR, emit UDT.
       MAIN-ENTRY.
           IF NOT CDD-OPT-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               OR NOT CDD-OBF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDEMIT" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDEMIT"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE CDD-OPT-DIALECT TO CDD-MPR-DIALECT
           MOVE CDD-OPT-UDT-KIND TO CDD-MPR-UDT-KIND
           MOVE CDD-OPT-JSON-MODE TO CDD-MPR-JSON-MODE
           MOVE CDD-OPT-CASE TO CDD-MPR-CASE
           MOVE CDD-OPT-NAME-LEN TO CDD-MPR-NAME-LEN
           MOVE CDD-OPT-TARGET-NAME TO CDD-MPR-TARGET-NAME
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           IF CDD-OK
               CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR CDD-OUT-BUF
                   CDD-ERROR
               END-CALL
           END-IF
           EXIT PROGRAM.
