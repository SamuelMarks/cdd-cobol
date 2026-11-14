      * @file CDDSERD.cbl
      * @brief Public SDK Op 2 (serde): JSON or COBOL -> serde routines.
      * @module CDDSERD
      * @details Public SDK entry point accepting either a JSON/Schema
      *          buffer or an existing COBOL UDT buffer (auto-detected)
      *          and emitting dialect-idiomatic to_json and from_json
      *          routines across all 10 COBOL standards.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDSERD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
      * @record WS-SERD-SCAN Auto-detection cursor state.
       01  WS-SERD-SCAN.
      * @field WS-I Byte cursor scanning leading whitespace.
           05  WS-I                      PIC 9(8) COMP VALUE 1.
      * @field WS-IS-JSON 1 if input starts with '{' or '['.
           05  WS-IS-JSON                PIC 9(2) COMP VALUE 0.
      * @field WS-CH Current byte character.
           05  WS-CH                     PIC X(1) VALUE SPACE.
      * @record WS-BYTE-CONV Big-endian TAB/LF/CR helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record CDD-OUT-BUF Output COBOL serde routines buffer.
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
      * @field CDD-OBF-DATA Emitted COBOL serde source bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDSERD Generate to_json/from_json from JSON/COBOL.
      * @param [in] CDD-SDK-OPTS Dialect, serde kind, format, & flags.
      * @param [in] CDD-BUF Input JSON/Schema or COBOL UDT buffer.
      * @param [inout] CDD-OUT-BUF Output COBOL serde routines buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Dialect-idiomatic serde routines in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, parse/IR/codegen errors.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-BUF CDD-OUT-BUF
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, detect input, emit serde.
       MAIN-ENTRY.
           IF NOT CDD-OPT-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               OR NOT CDD-OBF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDSERD" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDSERD"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM DETECT-JSON-VS-COBOL
           IF WS-IS-JSON = 1
               PERFORM BUILD-IR-FROM-JSON
           ELSE
               PERFORM BUILD-IR-FROM-COBOL
           END-IF
           IF CDD-OK
               CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR CDD-OUT-BUF
                   CDD-ERROR
               END-CALL
           END-IF
           EXIT PROGRAM.
      * @paragraph DETECT-JSON-VS-COBOL Check first non-ws byte.
       DETECT-JSON-VS-COBOL.
           MOVE 0 TO WS-IS-JSON
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > CDD-BUF-LEN
               MOVE CDD-BUF-DATA(WS-I:1) TO WS-CH
               MOVE 0 TO WS-U16
               MOVE WS-CH TO WS-B-LO
               IF WS-U16 > 32
                   IF WS-CH = "{" OR WS-CH = "["
                       MOVE 1 TO WS-IS-JSON
                   END-IF
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM.
      * @paragraph BUILD-IR-FROM-JSON Invoke CDDJ2IR on JSON input.
       BUILD-IR-FROM-JSON.
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
           END-CALL.
      * @paragraph BUILD-IR-FROM-COBOL Invoke CDDCLEX/CDDCSTB/CDDC2IR.
       BUILD-IR-FROM-COBOL.
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
