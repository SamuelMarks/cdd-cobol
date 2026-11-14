      * @file CDDPARS.cbl
      * @brief Public SDK Op 1 (parse): COBOL UDT -> Schema/Sample JSON.
      * @module CDDPARS
      * @details Public SDK entry point validating all envelope tags
      *          and extracting a COBOL record, TYPEDEF, or CLASS-ID
      *          into Draft 2020-12 JSON Schema or sample JSON.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDPARS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
      * @field CDD-OBF-DATA Emitted JSON output bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDPARS Parse COBOL UDT buffer to JSON/Schema.
      * @param [in] CDD-SDK-OPTS Dialect, format, JSON mode, & style.
      * @param [in] CDD-BUF Input COBOL source or copybook buffer.
      * @param [inout] CDD-OUT-BUF Output JSON Schema or sample buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Emitted JSON Schema or sample JSON in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, COBOL parse/emitter errors.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-BUF CDD-OUT-BUF
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Delegate validated call to CDDGENJ.
       MAIN-ENTRY.
           CALL "CDDGENJ" USING CDD-SDK-OPTS CDD-BUF CDD-OUT-BUF
               CDD-ERROR
           END-CALL
           EXIT PROGRAM.
