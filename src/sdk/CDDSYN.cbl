      * @file CDDSYN.cbl
      * @brief Public SDK Op 3 (sync): Existing COBOL + JSON -> COBOL.
      * @module CDDSYN
      * @details Public SDK entry point performing trivia-preserving
      *          surgical UDT and serde synchronization via CDDSYNC.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDSYN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record CDD-JSON-BUF Input JSON Schema or sample JSON buffer.
       01  CDD-JSON-BUF.
      * @field CDD-JBF-TAG Magic type-safety tag ('CDDBUF01').
           05  CDD-JBF-TAG               PIC X(8).
      * @field CDD-JBF-TAG-VALID Condition confirming valid JBF tag.
               88  CDD-JBF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-JBF-LEN Active JSON byte length.
           05  CDD-JBF-LEN               PIC 9(8) COMP.
      * @field CDD-JBF-CAP JSON buffer capacity.
           05  CDD-JBF-CAP               PIC 9(8) COMP.
      * @field CDD-JBF-NL-KIND JSON buffer newline mode.
           05  CDD-JBF-NL-KIND           PIC 9(2) COMP.
      * @field CDD-JBF-DATA Raw JSON bytes.
           05  CDD-JBF-DATA              PIC X(1048576).
      * @record CDD-OUT-BUF Output synchronized COBOL buffer.
       01  CDD-OUT-BUF.
      * @field CDD-OBF-TAG Magic type-safety tag ('CDDBUF01').
           05  CDD-OBF-TAG               PIC X(8).
      * @field CDD-OBF-TAG-VALID Condition confirming valid OBF tag.
               88  CDD-OBF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-OBF-LEN Active output COBOL byte length.
           05  CDD-OBF-LEN               PIC 9(8) COMP.
      * @field CDD-OBF-CAP Output buffer capacity.
           05  CDD-OBF-CAP               PIC 9(8) COMP.
      * @field CDD-OBF-NL-KIND Output newline mode.
           05  CDD-OBF-NL-KIND           PIC 9(2) COMP.
      * @field CDD-OBF-DATA Synchronized COBOL bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDSYN Surgically update COBOL UDT + serde to JSON.
      * @param [in] CDD-SDK-OPTS Dialect, format, target UDT, & flags.
      * @param [in] CDD-BUF Existing COBOL source buffer.
      * @param [in] CDD-JSON-BUF Target JSON Schema or sample buffer.
      * @param [inout] CDD-OUT-BUF Output synchronized COBOL buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Trivia-preserving updated COBOL source in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, sync/parse/codegen errors.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-BUF CDD-JSON-BUF
           CDD-OUT-BUF CDD-ERROR.
      * @paragraph MAIN-ENTRY Delegate validated call to CDDSYNC.
       MAIN-ENTRY.
           CALL "CDDSYNC" USING CDD-SDK-OPTS CDD-BUF CDD-JSON-BUF
               CDD-OUT-BUF CDD-ERROR
           END-CALL
           EXIT PROGRAM.
