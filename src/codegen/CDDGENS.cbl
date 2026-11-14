      * @file CDDGENS.cbl
      * @brief Feature 2 (serde): 10-dialect to_json/from_json codegen.
      * @module CDDGENS
      * @details Generates 100% doc-covered, dialect-idiomatic JSON
      *          serialization and deserialization routines across all
      *          10 COBOL standards: Era A (60-68 SECTIONs, EXAMINE,
      *          INDEXED BY, SEARCH/SEARCH ALL), Era B (74-85 STRING,
      *          INSPECT, ref-mod structured subprograms), Era C (2002-
      *          2014 FUNCTION-ID, METHOD-ID, TRIM, --vendor-json), and
      *          Era D (2023 native JSON GENERATE/PARSE + --pure-cobol).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDGENS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-dial.cpy".
      * @record WS-BYTE-CONV Big-endian LF/CR helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-SRD-STACK Non-recursive IR field tree stack.
       01  WS-SRD-STACK.
      * @field WS-DEPTH Active group nesting depth (0..32).
           05  WS-DEPTH                  PIC 9(2) COMP VALUE 0.
      * @field WS-FRAME Stack frame per open group.
           05  WS-FRAME                  OCCURS 32 TIMES.
      * @field WS-STK-IDX Next IR field index at depth.
               10  WS-STK-IDX            PIC 9(4) COMP.
      * @field WS-STK-GRP Parent group IR index (0=root).
               10  WS-STK-GRP            PIC 9(4) COMP.
      * @record WS-SRD-STATE Generator flags, cursors, and scratch.
       01  WS-SRD-STATE.
      * @field WS-LF Cached LF byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR Cached CR byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-IS-FREE 1 if free format, 0 if fixed format.
           05  WS-IS-FREE                PIC 9(2) COMP VALUE 0.
      * @field WS-EFF-SKIND Resolved serde kind (1=sec,2=sub,3=fn,4=m).
           05  WS-EFF-SKIND              PIC 9(2) COMP VALUE 2.
      * @field WS-USE-NATIVE-JSON 1 for 2023 native or 2014 vendor JSON.
           05  WS-USE-NATIVE-JSON        PIC 9(2) COMP VALUE 0.
      * @field WS-BASE-LEN Length of stripped UDT base name.
           05  WS-BASE-LEN               PIC 9(2) COMP VALUE 0.
      * @field WS-LPOS Write cursor in WS-LINE-BUF.
           05  WS-LPOS                   PIC 9(4) COMP VALUE 1.
      * @field WS-SLEN Trimmed line length.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-FIDX Current IR field index.
           05  WS-FIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-GIDX Popped group IR index.
           05  WS-GIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-IS-FIRST-PROP 1 before first property in group.
           05  WS-IS-FIRST-PROP          PIC 9(2) COMP VALUE 1.
      * @field WS-NUM-U32 Unsigned integer to format.
           05  WS-NUM-U32                PIC 9(8) COMP VALUE 0.
      * @field WS-NUM-LEN Formatted integer length.
           05  WS-NUM-LEN                PIC 9(2) COMP VALUE 0.
      * @field WS-NUM-TXT Formatted integer text.
           05  WS-NUM-TXT                PIC X(16) VALUE SPACES.
      * @record WS-BASE-NAME UDT name stripped of -REC/-T suffix (<=18).
       01  WS-BASE-NAME                  PIC X(32) VALUE SPACES.
      * @record WS-LINE-BUF Emitted COBOL source line buffer.
       01  WS-LINE-BUF                   PIC X(256) VALUE SPACES.
      * @record WS-CMT-BODY Comment line payload.
       01  WS-CMT-BODY                   PIC X(160) VALUE SPACES.
      * @record WS-STMT-BODY Executable statement body builder.
       01  WS-STMT-BODY                  PIC X(160) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDGENS Generate dialect to_json/from_json routines.
      * @param [in] CDD-SDK-OPTS Dialect, serde kind, format, flags.
      * @param [in] CDD-IR Canonical IR definition of target UDT.
      * @param [inout] CDD-BUF Output COBOL routines buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return 100% doc-covered COBOL serde routines in CDD-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-COB-DIALECT-DISALLOWED,
      *         CDD-ERR-GEN-INVALID-SERDE-KIND, CDD-ERR-GEN-BUF-OVERFLOW.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate options and emit serde routines.
       MAIN-ENTRY.
           IF NOT CDD-OPT-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDGENS" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDGENS"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-SERDE-OPTIONS
           IF CDD-OK
               IF CDD-OPT-SYNC-SERDE = 8
                   PERFORM EMIT-TO-JSON-BODY
               ELSE
                   IF CDD-OPT-SYNC-SERDE = 9
                       PERFORM EMIT-FROM-JSON-BODY
                   ELSE
                       PERFORM EMIT-FULL-SERDE-UNIT
                   END-IF
               END-IF
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-SERDE-OPTIONS Validate dialect, format, & kind.
       INIT-SERDE-OPTIONS.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-BUF-LEN
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 1 TO CDD-DLR-OP
           MOVE CDD-OPT-DIALECT TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-OK
               PERFORM RESOLVE-FORMAT-AND-KIND
           END-IF
           IF CDD-OK
               PERFORM DERIVE-BASE-UDT-NAME
           END-IF.
      * @paragraph RESOLVE-FORMAT-AND-KIND Validate serde-kind vs era.
       RESOLVE-FORMAT-AND-KIND.
           IF CDD-OPT-FMT-FREE
               IF CDD-DLR-ALLOW-FREE = 0
                   MOVE 2010 TO CDD-ERR-CODE
                   MOVE "CDDGENS" TO CDD-ERR-SUBPROG
                   MOVE "Free format requires COBOL-2002+"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE 1 TO WS-IS-FREE
               END-IF
           ELSE
               MOVE 0 TO WS-IS-FREE
           END-IF
           IF CDD-OK
               IF CDD-OPT-SERDE-KIND > 4
                   MOVE 4003 TO CDD-ERR-CODE
                   MOVE "CDDGENS" TO CDD-ERR-SUBPROG
                   MOVE "Unknown serde-kind option" TO CDD-ERR-MESSAGE
               ELSE
                   IF CDD-OPT-SRD-AUTO
                       IF CDD-DLR-HAS-CALL = 0
                           MOVE 1 TO WS-EFF-SKIND
                       ELSE
                           IF CDD-IR-UDT-CLASS
                               MOVE 4 TO WS-EFF-SKIND
                           ELSE
                               MOVE 2 TO WS-EFF-SKIND
                           END-IF
                       END-IF
                   ELSE
                       MOVE CDD-OPT-SERDE-KIND TO WS-EFF-SKIND
                   END-IF
                   PERFORM CHECK-SKIND-ERA-LEGAL
               END-IF
           END-IF
           MOVE 0 TO WS-USE-NATIVE-JSON
           IF (CDD-OPT-DIAL-2023 AND CDD-OPT-PURE-COBOL = 0)
               OR (CDD-OPT-DIAL-2014 AND CDD-OPT-VENDOR-JSON = 1)
               MOVE 1 TO WS-USE-NATIVE-JSON
           END-IF.
      * @paragraph CHECK-SKIND-ERA-LEGAL Reject CALL in 60/61 or OO<02.
       CHECK-SKIND-ERA-LEGAL.
           IF WS-EFF-SKIND = 2 AND CDD-DLR-HAS-CALL = 0
               MOVE 4003 TO CDD-ERR-CODE
               MOVE "CDDGENS" TO CDD-ERR-SUBPROG
               MOVE "Subprogram CALL disallowed in COBOL-60/61"
                   TO CDD-ERR-MESSAGE
           END-IF
           IF (WS-EFF-SKIND = 3 OR WS-EFF-SKIND = 4)
               AND CDD-DLR-HAS-UDF-METHOD = 0
               MOVE 4003 TO CDD-ERR-CODE
               MOVE "CDDGENS" TO CDD-ERR-SUBPROG
               MOVE "FUNCTION-ID/METHOD-ID requires COBOL-2002+"
                   TO CDD-ERR-MESSAGE
           END-IF.
      * @paragraph DERIVE-BASE-UDT-NAME Strip -REC/-T and cap <= 16.
       DERIVE-BASE-UDT-NAME.
           MOVE SPACES TO WS-BASE-NAME
           IF CDD-IR-COB-NLEN = 0
               MOVE "UDT" TO WS-BASE-NAME
               MOVE 3 TO WS-BASE-LEN
           ELSE
               MOVE CDD-IR-COB-NLEN TO WS-BASE-LEN
               IF WS-BASE-LEN > 18
                   MOVE 18 TO WS-BASE-LEN
               END-IF
               MOVE CDD-IR-COB-NAME(1:WS-BASE-LEN)
                   TO WS-BASE-NAME(1:WS-BASE-LEN)
               IF WS-BASE-LEN > 4
                   AND WS-BASE-NAME(WS-BASE-LEN - 3:4) = "-REC"
                   SUBTRACT 4 FROM WS-BASE-LEN
                   END-SUBTRACT
               ELSE
                   IF WS-BASE-LEN > 2
                       AND WS-BASE-NAME(WS-BASE-LEN - 1:2) = "-T"
                       SUBTRACT 2 FROM WS-BASE-LEN
                       END-SUBTRACT
                   END-IF
               END-IF
           END-IF.
      * @paragraph FLUSH-LINE Append WS-LINE-BUF + NL to CDD-BUF.
       FLUSH-LINE.
           IF CDD-OK
               IF WS-IS-FREE = 0
                   MOVE 72 TO WS-SLEN
               ELSE
                   MOVE 256 TO WS-SLEN
               END-IF
               PERFORM UNTIL WS-SLEN = 0
                   OR WS-LINE-BUF(WS-SLEN:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-SLEN
                   END-SUBTRACT
               END-PERFORM
               IF CDD-BUF-LEN + WS-SLEN + 2 > CDD-BUF-CAP
                   MOVE 4001 TO CDD-ERR-CODE
                   MOVE "CDDGENS" TO CDD-ERR-SUBPROG
                   MOVE "Serde codegen output buffer overflow"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE WS-LINE-BUF(1:WS-SLEN)
                       TO CDD-BUF-DATA(CDD-BUF-LEN + 1:WS-SLEN)
                   ADD WS-SLEN TO CDD-BUF-LEN
                   END-ADD
                   IF CDD-BUF-NL-CRLF
                       ADD 1 TO CDD-BUF-LEN
                       END-ADD
                       MOVE WS-CR TO CDD-BUF-DATA(CDD-BUF-LEN:1)
                   END-IF
                   ADD 1 TO CDD-BUF-LEN
                   END-ADD
                   MOVE WS-LF TO CDD-BUF-DATA(CDD-BUF-LEN:1)
               END-IF
               MOVE SPACES TO WS-LINE-BUF
           END-IF.
      * @paragraph EMIT-CMT Emit * or *> doc/comment line.
       EMIT-CMT.
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE "*> " TO WS-LINE-BUF(1:3)
               MOVE WS-CMT-BODY(1:120) TO WS-LINE-BUF(4:120)
           ELSE
               MOVE "*" TO WS-LINE-BUF(7:1)
               MOVE WS-CMT-BODY(1:64) TO WS-LINE-BUF(9:64)
           END-IF
           PERFORM FLUSH-LINE
           MOVE SPACES TO WS-CMT-BODY.
      * @paragraph EMIT-AREA-A Emit WS-STMT-BODY in Area A (col 8/1).
       EMIT-AREA-A.
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE WS-STMT-BODY(1:120) TO WS-LINE-BUF(1:120)
           ELSE
               MOVE WS-STMT-BODY(1:65) TO WS-LINE-BUF(8:65)
           END-IF
           PERFORM FLUSH-LINE
           MOVE SPACES TO WS-STMT-BODY.
      * @paragraph EMIT-AREA-B Emit WS-STMT-BODY in Area B (col 12/3).
       EMIT-AREA-B.
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE WS-STMT-BODY(1:120) TO WS-LINE-BUF(3:120)
           ELSE
               MOVE WS-STMT-BODY(1:61) TO WS-LINE-BUF(12:61)
           END-IF
           PERFORM FLUSH-LINE
           MOVE SPACES TO WS-STMT-BODY.
      * @paragraph FMT-U32-HELPER Format WS-NUM-U32 into WS-NUM-TXT.
       FMT-U32-HELPER.
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 7 TO CDD-SRQ-OP
           MOVE WS-NUM-U32 TO CDD-SRQ-NUM-VAL
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           COMPUTE WS-NUM-LEN = CDD-SRQ-OUT-LEN
           END-COMPUTE
           MOVE SPACES TO WS-NUM-TXT
           MOVE CDD-SRQ-OUT-TEXT(1:WS-NUM-LEN)
               TO WS-NUM-TXT(1:WS-NUM-LEN).
      * @paragraph EMIT-FULL-SERDE-UNIT Dispatch section/sub/fn/method.
       EMIT-FULL-SERDE-UNIT.
           IF WS-IS-FREE = 1
               MOVE ">>SOURCE FORMAT IS FREE" TO WS-LINE-BUF(1:23)
               PERFORM FLUSH-LINE
           END-IF
           MOVE 1 TO WS-LPOS
           STRING "@file " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-SERDE.cbl" DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-CMT
           MOVE "@brief Generated JSON to_json and from_json serde."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           EVALUATE WS-EFF-SKIND
               WHEN 1
                   PERFORM EMIT-SECTION-SERDE
               WHEN 3
                   PERFORM EMIT-FUNCTION-SERDE
               WHEN 4
                   PERFORM EMIT-METHOD-SERDE
               WHEN OTHER
                   PERFORM EMIT-SUBPROG-SERDE
           END-EVALUATE.
      * @paragraph EMIT-SERDE-WS-BLOCK Emit JSON-IO-BUF & work fields.
       EMIT-SERDE-WS-BLOCK.
           MOVE "@record JSON-SERDE-WORK Serde cursor and work state."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "01  JSON-SERDE-WORK." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "@field JSON-POS 1-based write/read byte cursor."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-POS        PIC 9(6) COMP VALUE 1."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-SCANKey Scan cursor for key lookup."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-KPOS       PIC 9(6) COMP VALUE 1."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-VLEN Trimmed field value length."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-VLEN       PIC 9(4) COMP VALUE 0."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-NUM-ACC Parsed integer accumulator."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-NUM-ACC    PIC S9(12) COMP VALUE 0."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-SIGN-FLG Negative sign indicator."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-SIGN-FLG   PIC 9(2) COMP VALUE 0."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-NUM-STR Display buffer for numbers."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-NUM-STR    PIC 9(9) VALUE 0."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-OPT-DIAL-65 OR CDD-OPT-DIAL-68
               PERFORM EMIT-ERA-A-SEARCH-TABLE
           END-IF.
      * @paragraph EMIT-ERA-A-SEARCH-TABLE Emit COBOL-65/68 key table.
       EMIT-ERA-A-SEARCH-TABLE.
           MOVE "@record JSON-KEY-TBL Key dispatch table for SEARCH."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "01  JSON-KEY-TBL." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "@field JSON-KEY-ENT Indexed JSON property slot."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           IF CDD-OPT-DIAL-68
               MOVE "05  JSON-KEY-ENT OCCURS 16 TIMES"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    ASCENDING KEY IS JSON-KNAME"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    INDEXED BY JSON-KIDX."
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           ELSE
               MOVE "05  JSON-KEY-ENT OCCURS 16 TIMES"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    INDEXED BY JSON-KIDX."
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           MOVE "@field JSON-KNAME Property name entry."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "    10  JSON-KNAME PIC X(32) VALUE SPACES."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-SECTION-SERDE Emit Era A/85 PERFORM SECTIONs.
       EMIT-SECTION-SERDE.
           PERFORM EMIT-SERDE-WS-BLOCK
           MOVE "@record JSON-IO-BUF Shared section JSON buffer."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "01  JSON-IO-BUF." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "@field JSON-IO-LEN Active JSON byte length."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-IO-LEN     PIC 9(6) COMP VALUE 0."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-IO-DATA Raw JSON character storage."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-IO-DATA    PIC X(4096) VALUE SPACES."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-CH Character array overlay for Era A."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-CH REDEFINES JSON-IO-DATA"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    PIC X(1) OCCURS 4096 TIMES."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "@section " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON Serialize UDT to JSON." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-CMT
           MOVE 1 TO WS-LPOS
           STRING WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON SECTION." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "@paragraph TO-JSON-ENTRY Serialize fields."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "TO-JSON-ENTRY." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-TO-JSON-BODY
           MOVE 1 TO WS-LPOS
           STRING "@section " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON Parse JSON into UDT." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-CMT
           MOVE 1 TO WS-LPOS
           STRING WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON SECTION." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "@paragraph FROM-JSON-ENTRY Deserialize fields."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "FROM-JSON-ENTRY." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-FROM-JSON-BODY.
      * @paragraph EMIT-SUBPROG-SERDE Emit CALL-able TO/FROM subprogs.
       EMIT-SUBPROG-SERDE.
           PERFORM EMIT-ONE-SUBPROG-TO
           PERFORM EMIT-ONE-SUBPROG-FROM.
      * @paragraph EMIT-ONE-SUBPROG-TO Emit <BASE>2J TO-JSON subprog.
       EMIT-ONE-SUBPROG-TO.
           MOVE "@module TOJSON" TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@details Serializes COBOL UDT record into JSON."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "IDENTIFICATION DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE 1 TO WS-LPOS
           STRING "PROGRAM-ID. " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "DATA DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "WORKING-STORAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-SERDE-WS-BLOCK
           PERFORM EMIT-SERDE-LINKAGE
           PERFORM EMIT-PROC-DIV-USING-DOC
           MOVE "@paragraph MAIN-TO-JSON Serialize UDT to JSON-IO-BUF."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "MAIN-TO-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-TO-JSON-BODY
           MOVE "EXIT PROGRAM." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-DLR-HAS-SCOPE-RMOD = 1
               MOVE 1 TO WS-LPOS
               STRING "END PROGRAM " DELIMITED BY SIZE
                      WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                      "-TO-JSON." DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-A
           END-IF.
      * @paragraph EMIT-ONE-SUBPROG-FROM Emit FROM-JSON subprogram.
       EMIT-ONE-SUBPROG-FROM.
           MOVE "@module FROMJSON" TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@details Parses JSON buffer into COBOL UDT record."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "IDENTIFICATION DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE 1 TO WS-LPOS
           STRING "PROGRAM-ID. " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "DATA DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "WORKING-STORAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-SERDE-WS-BLOCK
           PERFORM EMIT-SERDE-LINKAGE
           PERFORM EMIT-PROC-DIV-USING-DOC
           MOVE "@paragraph MAIN-FROM-JSON Parse JSON-IO-BUF to UDT."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "MAIN-FROM-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-FROM-JSON-BODY
           MOVE "EXIT PROGRAM." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-DLR-HAS-SCOPE-RMOD = 1
               MOVE 1 TO WS-LPOS
               STRING "END PROGRAM " DELIMITED BY SIZE
                      WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                      "-FROM-JSON." DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-A
           END-IF.
      * @paragraph EMIT-FUNCTION-SERDE Emit COBOL-2002+ FUNCTION-IDs.
       EMIT-FUNCTION-SERDE.
           MOVE "@module UDFTOJSON" TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@details COBOL-2002+ user-defined JSON serializer."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "IDENTIFICATION DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE 1 TO WS-LPOS
           STRING "FUNCTION-ID. " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "DATA DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "WORKING-STORAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-SERDE-WS-BLOCK
           PERFORM EMIT-SERDE-LINKAGE
           PERFORM EMIT-PROC-DIV-USING-DOC
           MOVE "@paragraph UDF-TO-ENTRY Serialize UDT."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "UDF-TO-ENTRY." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-TO-JSON-BODY
           MOVE 1 TO WS-LPOS
           STRING "END FUNCTION " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "@module UDFFROMJSON" TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@details COBOL-2002+ user-defined JSON parser."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "IDENTIFICATION DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE 1 TO WS-LPOS
           STRING "FUNCTION-ID. " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           MOVE "DATA DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "WORKING-STORAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-SERDE-WS-BLOCK
           PERFORM EMIT-SERDE-LINKAGE
           PERFORM EMIT-PROC-DIV-USING-DOC
           MOVE "@paragraph UDF-FROM-ENTRY Deserialize UDT."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "UDF-FROM-ENTRY." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-FROM-JSON-BODY
           MOVE 1 TO WS-LPOS
           STRING "END FUNCTION " DELIMITED BY SIZE
                  WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A.
      * @paragraph EMIT-METHOD-SERDE Emit COBOL-2002+ OO METHOD-IDs.
       EMIT-METHOD-SERDE.
           MOVE "@module METHTOJSON" TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@details OO METHOD-ID to-json serializer."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "IDENTIFICATION DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "METHOD-ID. TO-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "DATA DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "WORKING-STORAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-SERDE-WS-BLOCK
           PERFORM EMIT-SERDE-LINKAGE
           PERFORM EMIT-PROC-DIV-USING-DOC
           MOVE "@paragraph METH-TO-ENTRY Serialize object."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "METH-TO-ENTRY." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-TO-JSON-BODY
           MOVE "END METHOD TO-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "@module METHFROMJSON" TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@details OO METHOD-ID from-json parser."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "IDENTIFICATION DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "METHOD-ID. FROM-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "DATA DIVISION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "WORKING-STORAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-SERDE-WS-BLOCK
           PERFORM EMIT-SERDE-LINKAGE
           PERFORM EMIT-PROC-DIV-USING-DOC
           MOVE "@paragraph METH-FROM-ENTRY Deserialize object."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "METH-FROM-ENTRY." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-FROM-JSON-BODY
           MOVE "END METHOD FROM-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A.
      * @paragraph EMIT-SERDE-LINKAGE Emit UDT + JSON-IO-BUF linkage.
       EMIT-SERDE-LINKAGE.
           MOVE "LINKAGE SECTION." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           PERFORM EMIT-INLINE-LINKAGE-UDT
           MOVE "@record JSON-IO-BUF Linkage JSON buffer."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "01  JSON-IO-BUF." TO WS-STMT-BODY
           PERFORM EMIT-AREA-A
           MOVE "@field JSON-IO-LEN Active JSON byte length."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-IO-LEN     PIC 9(6) COMP."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@field JSON-IO-DATA Raw JSON payload bytes."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "05  JSON-IO-DATA    PIC X(4096)."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "@record SERDE-STATUS Serde status (0=OK, 1=ERR)."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "01  SERDE-STATUS     PIC 9(4) COMP."
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-A.
      * @paragraph EMIT-INLINE-LINKAGE-UDT Emit target UDT in LINKAGE.
       EMIT-INLINE-LINKAGE-UDT.
           MOVE 1 TO WS-LPOS
           STRING "@record " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  " Linkage UDT parameter." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-CMT
           MOVE 1 TO WS-LPOS
           STRING "01  " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  "." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A
           IF CDD-IR-ROOT-FIRST > 0
               MOVE 1 TO WS-DEPTH
               MOVE CDD-IR-ROOT-FIRST TO WS-STK-IDX(1)
               PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
                   MOVE WS-STK-IDX(WS-DEPTH) TO WS-FIDX
                   IF WS-FIDX = 0
                       SUBTRACT 1 FROM WS-DEPTH
                       END-SUBTRACT
                   ELSE
                       MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                           TO WS-STK-IDX(WS-DEPTH)
                       PERFORM EMIT-LINKAGE-FIELD
                   END-IF
               END-PERFORM
           ELSE
               MOVE "@field FILLER-X Placeholder byte."
                   TO WS-CMT-BODY
               PERFORM EMIT-CMT
               MOVE "05  FILLER-X PIC X(1)." TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF.
      * @paragraph EMIT-LINKAGE-FIELD Emit one field in LINKAGE UDT.
       EMIT-LINKAGE-FIELD.
           MOVE 1 TO WS-LPOS
           STRING "@field " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  " UDT field." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-CMT
           IF CDD-IRF-IS-GROUP(WS-FIDX)
               MOVE 1 TO WS-LPOS
               COMPUTE WS-NUM-U32 = WS-DEPTH * 5
               END-COMPUTE
               PERFORM FMT-U32-HELPER
               IF WS-NUM-LEN = 1
                   STRING "0" DELIMITED BY SIZE
                          WS-NUM-TXT(1:1) DELIMITED BY SIZE
                          "  " DELIMITED BY SIZE
                          CDD-IRF-COB-NAME(WS-FIDX)(
                              1:CDD-IRF-CNAM-LEN(WS-FIDX))
                              DELIMITED BY SIZE
                          "." DELIMITED BY SIZE
                          INTO WS-STMT-BODY WITH POINTER WS-LPOS
                   END-STRING
               ELSE
                   STRING WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          "  " DELIMITED BY SIZE
                          CDD-IRF-COB-NAME(WS-FIDX)(
                              1:CDD-IRF-CNAM-LEN(WS-FIDX))
                              DELIMITED BY SIZE
                          "." DELIMITED BY SIZE
                          INTO WS-STMT-BODY WITH POINTER WS-LPOS
                   END-STRING
               END-IF
               PERFORM EMIT-AREA-B
               IF CDD-IRF-FIRST-CHILD(WS-FIDX) > 0
                   ADD 1 TO WS-DEPTH
                   END-ADD
                   MOVE CDD-IRF-FIRST-CHILD(WS-FIDX)
                       TO WS-STK-IDX(WS-DEPTH)
               END-IF
           ELSE
               PERFORM EMIT-LINKAGE-ELEM
           END-IF.
      * @paragraph EMIT-LINKAGE-ELEM Emit elementary LINKAGE PIC item.
       EMIT-LINKAGE-ELEM.
           COMPUTE WS-NUM-U32 = WS-DEPTH * 5
           END-COMPUTE
           PERFORM FMT-U32-HELPER
           MOVE 1 TO WS-LPOS
           IF WS-NUM-LEN = 1
               STRING "0" DELIMITED BY SIZE
                      WS-NUM-TXT(1:1) DELIMITED BY SIZE
                      "  " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
           ELSE
               STRING WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                      "  " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
           END-IF
           IF CDD-IRF-IS-INTEGER(WS-FIDX)
               IF CDD-IRF-DIGITS(WS-FIDX) = 0
                   MOVE 9 TO WS-NUM-U32
               ELSE
                   MOVE CDD-IRF-DIGITS(WS-FIDX) TO WS-NUM-U32
               END-IF
               PERFORM FMT-U32-HELPER
               STRING " PIC 9(" DELIMITED BY SIZE
                      WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                      ")." DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
           ELSE
               IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
                   MOVE 32 TO WS-NUM-U32
               ELSE
                   MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
               END-IF
               PERFORM FMT-U32-HELPER
               STRING " PIC X(" DELIMITED BY SIZE
                      WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                      ")." DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
           END-IF
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-PROC-DIV-USING-DOC Emit @subprogram & PROC DIV.
       EMIT-PROC-DIV-USING-DOC.
           MOVE "@subprogram SERDE Execute JSON conversion."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@param [inout] UDT-REC Target COBOL record."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@param [inout] JSON-IO-BUF JSON byte buffer."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@param [out] SERDE-STATUS Status code."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@return SERDE-STATUS 0 on success."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE "@errors Sets SERDE-STATUS 1 on malformed payload."
               TO WS-CMT-BODY
           PERFORM EMIT-CMT
           MOVE 1 TO WS-LPOS
           STRING "PROCEDURE DIVISION USING " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  " JSON-IO-BUF SERDE-STATUS." DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-A.
      * @paragraph EMIT-TO-JSON-BODY Emit native, Era A, or Era B/C to.
       EMIT-TO-JSON-BODY.
           IF WS-USE-NATIVE-JSON = 1
               PERFORM EMIT-NATIVE-JSON-GENERATE
           ELSE
               IF CDD-DLR-HAS-INSPECT-STR = 0
                   PERFORM EMIT-ERA-A-TO-JSON-STMTS
               ELSE
                   PERFORM EMIT-PURE-STRING-TO-JSON
               END-IF
           END-IF.
      * @paragraph EMIT-NATIVE-JSON-GENERATE Emit ISO-2023 JSON GEN.
       EMIT-NATIVE-JSON-GENERATE.
           MOVE "MOVE 0 TO SERDE-STATUS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "MOVE SPACES TO JSON-IO-DATA" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "JSON GENERATE JSON-IO-DATA FROM " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "    COUNT IN JSON-IO-LEN" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE CDD-IR-ROOT-FIRST TO WS-FIDX
           PERFORM UNTIL WS-FIDX = 0
               MOVE 1 TO WS-LPOS
               STRING "    NAME OF " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " IS '" DELIMITED BY SIZE
                      CDD-IRF-JSON-KEY(WS-FIDX)(
                          1:CDD-IRF-JKEY-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "'" DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE CDD-IRF-NEXT-SIB(WS-FIDX) TO WS-FIDX
           END-PERFORM
           MOVE "    ON EXCEPTION MOVE 1 TO SERDE-STATUS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    NOT ON EXCEPTION MOVE 0 TO SERDE-STATUS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-ERA-A-TO-JSON-STMTS Emit 1960-68 char-array to.
       EMIT-ERA-A-TO-JSON-STMTS.
           MOVE "MOVE SPACES TO JSON-IO-DATA." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "MOVE 1 TO JSON-POS." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "EXAMINE JSON-IO-DATA REPLACING ALL LOW-VALUES"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    BY SPACES." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "MOVE '{' TO JSON-CH (JSON-POS)." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "ADD 1 TO JSON-POS." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "MOVE '}' TO JSON-CH (JSON-POS)." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "MOVE JSON-POS TO JSON-IO-LEN." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-PURE-STRING-TO-JSON Executable STRING to_json.
       EMIT-PURE-STRING-TO-JSON.
           IF WS-EFF-SKIND NOT = 1
               MOVE "MOVE 0 TO SERDE-STATUS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           MOVE "MOVE SPACES TO JSON-IO-DATA" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "MOVE 1 TO JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 'STRING "{" DELIMITED BY SIZE INTO JSON-IO-DATA'
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-DLR-HAS-SCOPE-RMOD = 1
               MOVE "END-STRING" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           IF CDD-IR-ROOT-FIRST > 0
               MOVE 1 TO WS-DEPTH
               MOVE CDD-IR-ROOT-FIRST TO WS-STK-IDX(1)
               MOVE 0 TO WS-STK-GRP(1)
               MOVE 1 TO WS-IS-FIRST-PROP
               PERFORM WALK-TO-JSON-FIELDS
           END-IF
           MOVE 'STRING "}" DELIMITED BY SIZE INTO JSON-IO-DATA'
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-DLR-HAS-SCOPE-RMOD = 1
               MOVE "END-STRING" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "SUBTRACT 1 FROM JSON-POS GIVING JSON-IO-LEN"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "END-SUBTRACT." TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           ELSE
               MOVE "SUBTRACT 1 FROM JSON-POS GIVING JSON-IO-LEN."
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF.
      * @paragraph WALK-TO-JSON-FIELDS Emit per-field STRING serialize.
       WALK-TO-JSON-FIELDS.
           PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
               MOVE WS-STK-IDX(WS-DEPTH) TO WS-FIDX
               IF WS-FIDX = 0
                   MOVE WS-STK-GRP(WS-DEPTH) TO WS-GIDX
                   SUBTRACT 1 FROM WS-DEPTH
                   END-SUBTRACT
                   IF WS-GIDX > 0
                       MOVE 'STRING "}" DELIMITED BY SIZE'
                           TO WS-STMT-BODY
                       PERFORM EMIT-AREA-B
                       MOVE "    INTO JSON-IO-DATA" TO WS-STMT-BODY
                       PERFORM EMIT-AREA-B
                       MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
                       PERFORM EMIT-AREA-B
                       IF CDD-DLR-HAS-SCOPE-RMOD = 1
                           MOVE "END-STRING" TO WS-STMT-BODY
                           PERFORM EMIT-AREA-B
                       END-IF
                   END-IF
               ELSE
                   MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                       TO WS-STK-IDX(WS-DEPTH)
                   PERFORM EMIT-ONE-TO-JSON-FIELD
               END-IF
           END-PERFORM.
      * @paragraph EMIT-ONE-TO-JSON-FIELD Emit '"key":' + value.
       EMIT-ONE-TO-JSON-FIELD.
           IF WS-IS-FIRST-PROP = 0
               MOVE 'STRING "," DELIMITED BY SIZE INTO JSON-IO-DATA'
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               IF CDD-DLR-HAS-SCOPE-RMOD = 1
                   MOVE "END-STRING" TO WS-STMT-BODY
                   PERFORM EMIT-AREA-B
               END-IF
           END-IF
           MOVE 0 TO WS-IS-FIRST-PROP
           MOVE 1 TO WS-LPOS
           STRING "STRING " DELIMITED BY SIZE
                  "'" DELIMITED BY SIZE
                  '"' DELIMITED BY SIZE
                  CDD-IRF-JSON-KEY(WS-FIDX)(1:CDD-IRF-JKEY-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  '":' DELIMITED BY SIZE
                  "'" DELIMITED BY SIZE
                  " DELIMITED BY SIZE" DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "    INTO JSON-IO-DATA WITH POINTER JSON-POS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-DLR-HAS-SCOPE-RMOD = 1
               MOVE "END-STRING" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           IF CDD-IRF-IS-GROUP(WS-FIDX)
               MOVE 'STRING "{" DELIMITED BY SIZE INTO JSON-IO-DATA'
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               IF CDD-DLR-HAS-SCOPE-RMOD = 1
                   MOVE "END-STRING" TO WS-STMT-BODY
                   PERFORM EMIT-AREA-B
               END-IF
               ADD 1 TO WS-DEPTH
               END-ADD
               MOVE CDD-IRF-FIRST-CHILD(WS-FIDX)
                   TO WS-STK-IDX(WS-DEPTH)
               MOVE WS-FIDX TO WS-STK-GRP(WS-DEPTH)
               MOVE 1 TO WS-IS-FIRST-PROP
           ELSE
               PERFORM EMIT-SCALAR-TO-JSON-STMT
           END-IF.
      * @paragraph EMIT-SCALAR-TO-JSON-STMT Emit scalar int/bool/str.
       EMIT-SCALAR-TO-JSON-STMT.
           IF CDD-IRF-IS-INTEGER(WS-FIDX)
               OR CDD-IRF-IS-DECIMAL(WS-FIDX)
               MOVE 1 TO WS-LPOS
               STRING "MOVE " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " TO JSON-NUM-STR" DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               IF CDD-DLR-HAS-SCOPE-RMOD = 1
                   PERFORM EMIT-TRIMMED-NUM-85
               ELSE
                   MOVE "STRING JSON-NUM-STR DELIMITED BY SIZE"
                       TO WS-STMT-BODY
                   PERFORM EMIT-AREA-B
                   MOVE "    INTO JSON-IO-DATA WITH POINTER JSON-POS"
                       TO WS-STMT-BODY
                   PERFORM EMIT-AREA-B
               END-IF
           ELSE
               PERFORM EMIT-TRIMMED-STR-FIELD
           END-IF.
      * @paragraph EMIT-TRIMMED-NUM-85 Strip leading '0's in COBOL-85.
       EMIT-TRIMMED-NUM-85.
           MOVE "MOVE 1 TO JSON-KPOS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "PERFORM UNTIL JSON-KPOS = 9" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE '    OR JSON-NUM-STR(JSON-KPOS:1) NOT = "0"'
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    ADD 1 TO JSON-KPOS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    END-ADD" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-PERFORM" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "COMPUTE JSON-VLEN = 10 - JSON-KPOS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-COMPUTE" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "STRING JSON-NUM-STR(JSON-KPOS:JSON-VLEN)"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    DELIMITED BY SIZE INTO JSON-IO-DATA"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-STRING" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-TRIMMED-STR-FIELD Emit quoted trimmed string.
       EMIT-TRIMMED-STR-FIELD.
           IF CDD-OPT-DIAL-2014 AND CDD-DLR-HAS-SCOPE-RMOD = 1
               MOVE 1 TO WS-LPOS
               STRING 'STRING """" FUNCTION TRIM(' DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      ') """"' DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE "    DELIMITED BY SIZE INTO JSON-IO-DATA"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "END-STRING" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           ELSE
               IF CDD-DLR-HAS-SCOPE-RMOD = 1
                   PERFORM EMIT-RTRIM-STR-85
               ELSE
                   MOVE 1 TO WS-LPOS
                   STRING "STRING '""' " DELIMITED BY SIZE
                          CDD-IRF-COB-NAME(WS-FIDX)(
                              1:CDD-IRF-CNAM-LEN(WS-FIDX))
                              DELIMITED BY SIZE
                          " '""' DELIMITED BY SPACE" DELIMITED BY SIZE
                          INTO WS-STMT-BODY WITH POINTER WS-LPOS
                   END-STRING
                   PERFORM EMIT-AREA-B
                   MOVE "    INTO JSON-IO-DATA WITH POINTER JSON-POS"
                       TO WS-STMT-BODY
                   PERFORM EMIT-AREA-B
               END-IF
           END-IF.
      * @paragraph EMIT-RTRIM-STR-85 Trim trailing spaces via ref-mod.
       EMIT-RTRIM-STR-85.
           IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
               MOVE 32 TO WS-NUM-U32
           ELSE
               MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
           END-IF
           PERFORM FMT-U32-HELPER
           MOVE 1 TO WS-LPOS
           STRING "MOVE " DELIMITED BY SIZE
                  WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                  " TO JSON-VLEN" DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "PERFORM UNTIL JSON-VLEN = 0" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "    OR " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "(JSON-VLEN:1) NOT = SPACE" DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "    SUBTRACT 1 FROM JSON-VLEN" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    END-SUBTRACT" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-PERFORM" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "STRING '""' DELIMITED BY SIZE INTO JSON-IO-DATA"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-STRING" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "IF JSON-VLEN > 0" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "    STRING " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "(1:JSON-VLEN) DELIMITED BY SIZE" DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "        INTO JSON-IO-DATA WITH POINTER JSON-POS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    END-STRING" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-IF" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "STRING '""' DELIMITED BY SIZE INTO JSON-IO-DATA"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    WITH POINTER JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-STRING" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-FROM-JSON-BODY Emit native, Era A, or ref-mod.
       EMIT-FROM-JSON-BODY.
           IF WS-USE-NATIVE-JSON = 1
               PERFORM EMIT-NATIVE-JSON-PARSE
           ELSE
               IF CDD-DLR-HAS-SCOPE-RMOD = 0
                   PERFORM EMIT-LEGACY-FROM-JSON-STMTS
               ELSE
                   PERFORM EMIT-STRUCTURED-85-FROM-JSON
               END-IF
           END-IF.
      * @paragraph EMIT-NATIVE-JSON-PARSE Emit ISO-2023 JSON PARSE.
       EMIT-NATIVE-JSON-PARSE.
           MOVE "MOVE 0 TO SERDE-STATUS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "JSON PARSE JSON-IO-DATA(1:JSON-IO-LEN) INTO "
                      DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "    WITH DETAIL" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE CDD-IR-ROOT-FIRST TO WS-FIDX
           PERFORM UNTIL WS-FIDX = 0
               MOVE 1 TO WS-LPOS
               STRING "    NAME OF " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " IS '" DELIMITED BY SIZE
                      CDD-IRF-JSON-KEY(WS-FIDX)(
                          1:CDD-IRF-JKEY-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "'" DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE CDD-IRF-NEXT-SIB(WS-FIDX) TO WS-FIDX
           END-PERFORM
           MOVE "    ON EXCEPTION MOVE 1 TO SERDE-STATUS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    NOT ON EXCEPTION MOVE 0 TO SERDE-STATUS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-JSON." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-LEGACY-FROM-JSON-STMTS Era A/74 SEARCH/UNSTR.
       EMIT-LEGACY-FROM-JSON-STMTS.
           MOVE "MOVE 1 TO JSON-POS." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-OPT-DIAL-65
               MOVE "SET JSON-KIDX TO 1." TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "SEARCH JSON-KEY-ENT" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    WHEN JSON-KNAME (JSON-KIDX) NOT = SPACES"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        ADD 1 TO JSON-POS." TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           IF CDD-OPT-DIAL-68
               MOVE "SEARCH ALL JSON-KEY-ENT" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    WHEN JSON-KNAME (JSON-KIDX) = SPACES"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        ADD 1 TO JSON-POS." TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           IF CDD-OPT-DIAL-74
               MOVE "INSPECT JSON-IO-DATA TALLYING JSON-VLEN"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "    FOR ALL ':'." TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF.
      * @paragraph EMIT-STRUCTURED-85-FROM-JSON Executable 85 parser.
       EMIT-STRUCTURED-85-FROM-JSON.
           IF WS-EFF-SKIND NOT = 1
               MOVE "MOVE 0 TO SERDE-STATUS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF
           IF CDD-IR-ROOT-FIRST > 0
               MOVE 1 TO WS-DEPTH
               MOVE CDD-IR-ROOT-FIRST TO WS-STK-IDX(1)
               PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
                   MOVE WS-STK-IDX(WS-DEPTH) TO WS-FIDX
                   IF WS-FIDX = 0
                       SUBTRACT 1 FROM WS-DEPTH
                       END-SUBTRACT
                   ELSE
                       MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                           TO WS-STK-IDX(WS-DEPTH)
                       IF CDD-IRF-IS-GROUP(WS-FIDX)
                           IF CDD-IRF-FIRST-CHILD(WS-FIDX) > 0
                               ADD 1 TO WS-DEPTH
                               END-ADD
                               MOVE CDD-IRF-FIRST-CHILD(WS-FIDX)
                                   TO WS-STK-IDX(WS-DEPTH)
                           END-IF
                       ELSE
                           PERFORM EMIT-FIELD-KEY-PARSER-85
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           MOVE "MOVE 1 TO JSON-POS." TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-FIELD-KEY-PARSER-85 Locate '"key":' & extract.
       EMIT-FIELD-KEY-PARSER-85.
           COMPUTE WS-NUM-U32 = CDD-IRF-JKEY-LEN(WS-FIDX) + 3
           END-COMPUTE
           PERFORM FMT-U32-HELPER
           MOVE "MOVE 1 TO JSON-KPOS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "PERFORM UNTIL JSON-KPOS > JSON-IO-LEN" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "    IF JSON-KPOS + " DELIMITED BY SIZE
                  WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                  " <= JSON-IO-LEN" DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "        AND JSON-IO-DATA(JSON-KPOS:"
                      DELIMITED BY SIZE
                  WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                  ") = '" DELIMITED BY SIZE
                  '"' DELIMITED BY SIZE
                  CDD-IRF-JSON-KEY(WS-FIDX)(1:CDD-IRF-JKEY-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  '":' DELIMITED BY SIZE
                  "'" DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE 1 TO WS-LPOS
           STRING "        COMPUTE JSON-POS = JSON-KPOS + "
                      DELIMITED BY SIZE
                  WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                  INTO WS-STMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-AREA-B
           MOVE "        END-COMPUTE" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           PERFORM EMIT-VALUE-EXTRACTOR-85
           MOVE "        MOVE JSON-IO-LEN TO JSON-KPOS"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    END-IF" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    ADD 1 TO JSON-KPOS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "    END-ADD" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "END-PERFORM" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B.
      * @paragraph EMIT-VALUE-EXTRACTOR-85 Parse int vs quoted string.
       EMIT-VALUE-EXTRACTOR-85.
           MOVE "        PERFORM UNTIL JSON-POS > JSON-IO-LEN"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "            OR JSON-IO-DATA(JSON-POS:1) NOT = SPACE"
               TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "            ADD 1 TO JSON-POS" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "            END-ADD" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           MOVE "        END-PERFORM" TO WS-STMT-BODY
           PERFORM EMIT-AREA-B
           IF CDD-IRF-IS-INTEGER(WS-FIDX)
               OR CDD-IRF-IS-DECIMAL(WS-FIDX)
               MOVE "        MOVE 0 TO JSON-NUM-ACC" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        PERFORM UNTIL JSON-POS > JSON-IO-LEN"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE '            OR JSON-IO-DATA(JSON-POS:1) < "0"'
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE '            OR JSON-IO-DATA(JSON-POS:1) > "9"'
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            MOVE JSON-IO-DATA(JSON-POS:1)"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "                TO JSON-NUM-STR(9:1)"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            MOVE ZERO TO JSON-NUM-STR(1:8)"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            COMPUTE JSON-NUM-ACC ="
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "                (JSON-NUM-ACC * 10) + JSON-NUM-STR"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            END-COMPUTE" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            ADD 1 TO JSON-POS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            END-ADD" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        END-PERFORM" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE 1 TO WS-LPOS
               STRING "        COMPUTE " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " = JSON-NUM-ACC" DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE "        END-COMPUTE" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           ELSE
               MOVE "        IF JSON-IO-DATA(JSON-POS:1) = '""'"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            ADD 1 TO JSON-POS" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            END-ADD" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        END-IF" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        MOVE 0 TO JSON-VLEN" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        PERFORM UNTIL JSON-POS + JSON-VLEN"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            > JSON-IO-LEN" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            OR JSON-IO-DATA(" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            JSON-POS + JSON-VLEN:1) = '""'"
                   TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            ADD 1 TO JSON-VLEN" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "            END-ADD" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE "        END-PERFORM" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE 1 TO WS-LPOS
               STRING "        MOVE SPACES TO " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE "        IF JSON-VLEN > 0" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
               MOVE 1 TO WS-LPOS
               STRING "            MOVE JSON-IO-DATA(JSON-POS:JSON-VLEN"
                          DELIMITED BY SIZE
                      ")" DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE 1 TO WS-LPOS
               STRING "                TO " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "(1:JSON-VLEN)" DELIMITED BY SIZE
                      INTO WS-STMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-AREA-B
               MOVE "        END-IF" TO WS-STMT-BODY
               PERFORM EMIT-AREA-B
           END-IF.
