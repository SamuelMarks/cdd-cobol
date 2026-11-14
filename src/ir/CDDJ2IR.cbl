      * @file CDDJ2IR.cbl
      * @brief JSON Schema or sample JSON buffer to Canonical IR mapper.
      * @module CDDJ2IR
      * @details Orchestrates CDDJLEX, CDDJPAR, CDDJSCH/CDDJINF, and
      *          CDDNAM to map JSON Schema or sample JSON documents
      *          into dialect-validated, collision-free Canonical IR.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDJ2IR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-jtok.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-jsch.cpy".
       COPY "cdd-dial.cpy".
      * @record WS-NAM-PARM Local CDD-NAM-REQ for CDDNAM calls.
       01  WS-NAM-PARM.
      * @field WS-NMP-TAG Magic type-safety tag ('CDDNAM01').
           05  WS-NMP-TAG                PIC X(8) VALUE "CDDNAM01".
      * @field WS-NMP-OP Operation (1=J2C).
           05  WS-NMP-OP                 PIC 9(2) COMP VALUE 1.
      * @field WS-NMP-DIALECT Target dialect.
           05  WS-NMP-DIALECT            PIC 9(2) COMP VALUE 7.
      * @field WS-NMP-CASE Case style.
           05  WS-NMP-CASE               PIC 9(2) COMP VALUE 1.
      * @field WS-NMP-ILEN Input length.
           05  WS-NMP-ILEN               PIC 9(4) COMP VALUE 0.
      * @field WS-NMP-IN Input string.
           05  WS-NMP-IN                 PIC X(128) VALUE SPACES.
      * @field WS-NMP-OLEN Output length.
           05  WS-NMP-OLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-NMP-OUT Output string.
           05  WS-NMP-OUT                PIC X(128) VALUE SPACES.
      * @record WS-J2IR-STATE Naming, collision, and dialect state.
       01  WS-J2IR-STATE.
      * @field WS-USE-SCHEMA 1 if input is JSON Schema, 0 if sample.
           05  WS-USE-SCHEMA             PIC 9(2) COMP VALUE 0.
      * @field WS-FIDX Field loop index.
           05  WS-FIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-CHK-IDX Collision scan index.
           05  WS-CHK-IDX                PIC 9(4) COMP VALUE 0.
      * @field WS-COLL-NUM Disambiguation suffix counter (1..9).
           05  WS-COLL-NUM               PIC 9(1) COMP VALUE 0.
      * @field WS-HAS-COLL 1 if duplicate COBOL field name found.
           05  WS-HAS-COLL               PIC 9(2) COMP VALUE 0.
      * @field WS-BASE-LEN Base identifier length before suffix.
           05  WS-BASE-LEN               PIC 9(2) COMP VALUE 0.
      * @field WS-EIDX Enum loop index.
           05  WS-EIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-ECNT Enum offset within field.
           05  WS-ECNT                   PIC 9(4) COMP VALUE 0.
      * @field WS-SUF-DISP Single-digit disambiguation suffix.
           05  WS-SUF-DISP               PIC 9(1) VALUE 1.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(4) COMP VALUE 1.
      * @record WS-CAND-NAME Candidate COBOL name during disambiguation.
       01  WS-CAND-NAME                  PIC X(64) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDJ2IR Parse JSON/Schema buffer into CDD-IR.
      * @param [in] CDD-IR-MAP-REQ Dialect, UDT kind, mode, target name.
      * @param [in] CDD-BUF Input JSON or JSON Schema buffer.
      * @param [inout] CDD-IR Output Canonical IR with COBOL names.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-IR ready for codegen or diffing.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-GEN-INVALID-UDT-KIND,
      *         plus JSON/Schema/Infer errors.
       PROCEDURE DIVISION USING CDD-IR-MAP-REQ CDD-BUF CDD-IR
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags/dialect, parse, name IR.
       MAIN-ENTRY.
           IF NOT CDD-MPR-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDJ2IR" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDJ2IR"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM VALIDATE-DIALECT-AND-UDT
           IF CDD-OK
               PERFORM PARSE-JSON-INTO-IR
           END-IF
           IF CDD-OK
               PERFORM ASSIGN-ROOT-AND-FIELD-NAMES
           END-IF
           EXIT PROGRAM.
      * @paragraph VALIDATE-DIALECT-AND-UDT Check dialect & UDT kind.
       VALIDATE-DIALECT-AND-UDT.
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 1 TO CDD-DLR-OP
           MOVE CDD-MPR-DIALECT TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-OK
               IF CDD-MPR-UDT-KIND < 1 OR CDD-MPR-UDT-KIND > 3
                   MOVE 4002 TO CDD-ERR-CODE
                   MOVE "CDDJ2IR" TO CDD-ERR-SUBPROG
                   MOVE "Invalid UDT kind code" TO CDD-ERR-MESSAGE
               ELSE
                   IF CDD-MPR-UDT-KIND > 1
                       AND CDD-DLR-HAS-TYPEDEF-OO = 0
                       MOVE 4002 TO CDD-ERR-CODE
                       MOVE "CDDJ2IR" TO CDD-ERR-SUBPROG
                       MOVE "TYPEDEF/CLASS requires COBOL-2002+"
                           TO CDD-ERR-MESSAGE
                   END-IF
               END-IF
           END-IF.
      * @paragraph PARSE-JSON-INTO-IR Lex, DOM parse, analyze or infer.
       PARSE-JSON-INTO-IR.
           MOVE "CDDJTK01" TO CDD-JTK-TAG
           MOVE "CDDJDM01" TO CDD-JDM-TAG
           MOVE "CDDJSQ01" TO CDD-JSQ-TAG
           MOVE 4096 TO CDD-JTK-CAP
           MOVE 1024 TO CDD-JDM-CAP
           MOVE 32 TO CDD-JDM-MAX-DEPTH
           MOVE 1 TO CDD-JDM-REJECT-DUP
           CALL "CDDJLEX" USING CDD-BUF CDD-JTOK-STREAM CDD-ERROR
           END-CALL
           IF CDD-OK
               CALL "CDDJPAR" USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
                   CDD-ERROR
               END-CALL
           END-IF
           IF CDD-OK
               IF CDD-MPR-JSON-MODE = 1
                   MOVE 1 TO WS-USE-SCHEMA
               ELSE
                   IF CDD-MPR-JSON-MODE = 2
                       MOVE 0 TO WS-USE-SCHEMA
                   ELSE
                       MOVE 1 TO CDD-JSQ-OP
                       CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM
                           CDD-IR CDD-ERROR
                       END-CALL
                       MOVE CDD-JSQ-IS-SCHEMA TO WS-USE-SCHEMA
                   END-IF
               END-IF
               IF WS-USE-SCHEMA = 1
                   MOVE 2 TO CDD-JSQ-OP
                   CALL "CDDJSCH" USING CDD-JSCH-REQ CDD-JDOM CDD-IR
                       CDD-ERROR
                   END-CALL
               ELSE
                   CALL "CDDJINF" USING CDD-JDOM CDD-IR CDD-ERROR
                   END-CALL
               END-IF
           END-IF.
      * @paragraph ASSIGN-ROOT-AND-FIELD-NAMES Populate COBOL idents.
       ASSIGN-ROOT-AND-FIELD-NAMES.
           MOVE CDD-MPR-UDT-KIND TO CDD-IR-UDT-KIND
           MOVE "CDDNAM01" TO WS-NMP-TAG
           MOVE 1 TO WS-NMP-OP
           MOVE CDD-MPR-DIALECT TO WS-NMP-DIALECT
           IF CDD-MPR-NAME-LEN > 0
               MOVE CDD-MPR-NAME-LEN TO WS-NMP-ILEN
               MOVE CDD-MPR-TARGET-NAME TO WS-NMP-IN
           ELSE
               IF CDD-IR-TITLE-LEN > 0
                   MOVE CDD-IR-TITLE-LEN TO WS-NMP-ILEN
                   MOVE CDD-IR-JSON-TITLE TO WS-NMP-IN
               ELSE
                   MOVE 10 TO WS-NMP-ILEN
                   MOVE "CDD-RECORD" TO WS-NMP-IN
               END-IF
           END-IF
           CALL "CDDNAM" USING WS-NAM-PARM CDD-ERROR
           END-CALL
           COMPUTE CDD-IR-COB-NLEN = WS-NMP-OLEN
           END-COMPUTE
           MOVE WS-NMP-OUT(1:64) TO CDD-IR-COB-NAME
           MOVE 1 TO WS-FIDX
           PERFORM UNTIL WS-FIDX > CDD-IR-FLD-COUNT
               PERFORM NAME-AND-ADAPT-FIELD
               ADD 1 TO WS-FIDX
               END-ADD
           END-PERFORM.
      * @paragraph NAME-AND-ADAPT-FIELD Sanitize, disambiguate, adapt.
       NAME-AND-ADAPT-FIELD.
           MOVE CDD-IRF-JKEY-LEN(WS-FIDX) TO WS-NMP-ILEN
           MOVE CDD-IRF-JSON-KEY(WS-FIDX) TO WS-NMP-IN
           CALL "CDDNAM" USING WS-NAM-PARM CDD-ERROR
           END-CALL
           COMPUTE WS-BASE-LEN = WS-NMP-OLEN
           END-COMPUTE
           MOVE SPACES TO WS-CAND-NAME
           MOVE WS-NMP-OUT(1:WS-BASE-LEN)
               TO WS-CAND-NAME(1:WS-BASE-LEN)
           PERFORM DISAMBIGUATE-FIELD-NAME
           PERFORM ADAPT-ERA-FIELD-FEATURES
           IF CDD-IRF-ENUM-COUNT(WS-FIDX) > 0
               PERFORM NAME-FIELD-ENUMS
           END-IF.
      * @paragraph DISAMBIGUATE-FIELD-NAME Append -1..-9 on collision.
       DISAMBIGUATE-FIELD-NAME.
           MOVE 0 TO WS-COLL-NUM
           MOVE 1 TO WS-HAS-COLL
           PERFORM UNTIL WS-HAS-COLL = 0 OR WS-COLL-NUM >= 9
               MOVE 0 TO WS-HAS-COLL
               IF WS-CAND-NAME = CDD-IR-COB-NAME
                   MOVE 1 TO WS-HAS-COLL
               END-IF
               MOVE 1 TO WS-CHK-IDX
               PERFORM UNTIL WS-CHK-IDX >= WS-FIDX
                   IF CDD-IRF-COB-NAME(WS-CHK-IDX) = WS-CAND-NAME
                       MOVE 1 TO WS-HAS-COLL
                   END-IF
                   ADD 1 TO WS-CHK-IDX
                   END-ADD
               END-PERFORM
               IF WS-HAS-COLL = 1
                   ADD 1 TO WS-COLL-NUM
                   END-ADD
                   MOVE WS-COLL-NUM TO WS-SUF-DISP
                   MOVE SPACES TO WS-CAND-NAME
                   MOVE WS-NMP-OUT(1:WS-BASE-LEN)
                       TO WS-CAND-NAME(1:WS-BASE-LEN)
                   MOVE "-" TO WS-CAND-NAME(WS-BASE-LEN + 1:1)
                   MOVE WS-SUF-DISP TO WS-CAND-NAME(WS-BASE-LEN + 2:1)
               END-IF
           END-PERFORM
           IF WS-COLL-NUM > 0
               COMPUTE CDD-IRF-CNAM-LEN(WS-FIDX) = WS-BASE-LEN + 2
               END-COMPUTE
           ELSE
               MOVE WS-BASE-LEN TO CDD-IRF-CNAM-LEN(WS-FIDX)
           END-IF
           MOVE WS-CAND-NAME TO CDD-IRF-COB-NAME(WS-FIDX).
      * @paragraph ADAPT-ERA-FIELD-FEATURES Adjust array/UTF8/bool era.
       ADAPT-ERA-FIELD-FEATURES.
           IF CDD-IRF-ARR-DYNAMIC(WS-FIDX)
               AND CDD-DLR-HAS-OCCURS-DYN = 0
               IF CDD-DLR-HAS-ODO = 1
                   MOVE 2 TO CDD-IRF-ARR-MODE(WS-FIDX)
               ELSE
                   MOVE 1 TO CDD-IRF-ARR-MODE(WS-FIDX)
               END-IF
           END-IF
           IF CDD-IRF-ARR-ODO(WS-FIDX) AND CDD-DLR-HAS-ODO = 0
               MOVE 1 TO CDD-IRF-ARR-MODE(WS-FIDX)
           END-IF
           IF CDD-IRF-IS-BOOL(WS-FIDX) AND CDD-DLR-HAS-TYPEDEF-OO = 1
               MOVE 9 TO CDD-IRF-USAGE(WS-FIDX)
           END-IF.
      * @paragraph NAME-FIELD-ENUMS Generate unique 88 condition names.
       NAME-FIELD-ENUMS.
           MOVE CDD-IRF-ENUM-FIRST(WS-FIDX) TO WS-EIDX
           MOVE 0 TO WS-ECNT
           PERFORM UNTIL WS-ECNT >= CDD-IRF-ENUM-COUNT(WS-FIDX)
               MOVE SPACES TO WS-NMP-IN
               MOVE 1 TO WS-PTR
               STRING CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-" DELIMITED BY SIZE
                      CDD-IRE-VAL(WS-EIDX)(1:CDD-IRE-VAL-LEN(WS-EIDX))
                          DELIMITED BY SIZE
                      INTO WS-NMP-IN WITH POINTER WS-PTR
               END-STRING
               SUBTRACT 1 FROM WS-PTR GIVING WS-NMP-ILEN
               END-SUBTRACT
               CALL "CDDNAM" USING WS-NAM-PARM CDD-ERROR
               END-CALL
               COMPUTE CDD-IRE-CNAM-LEN(WS-EIDX) = WS-NMP-OLEN
               END-COMPUTE
               MOVE WS-NMP-OUT(1:64) TO CDD-IRE-COB-NAME(WS-EIDX)
               ADD 1 TO WS-EIDX
               END-ADD
               ADD 1 TO WS-ECNT
               END-ADD
           END-PERFORM.
