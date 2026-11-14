      * @file CDDJEMT.cbl
      * @brief Deterministic JSON Schema, Sample JSON, and DOM emitter.
      * @module CDDJEMT
      * @details Emits Draft 2020-12 JSON Schema (with optional x-cobol
      *          metadata), default/sample JSON documents from CDD-IR,
      *          or canonical serialized JSON from CDD-JDOM in either
      *          2-space pretty or compact formatting with LF/CRLF.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDJEMT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
      * @record WS-BYTE-CONV Pure 2-byte big-endian LF/CR generator.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-EMT-STACK Non-recursive stack for IR/DOM traversal.
       01  WS-EMT-STACK.
      * @field WS-DEPTH Active traversal stack depth (0..64).
           05  WS-DEPTH                  PIC 9(4) COMP VALUE 0.
      * @field WS-FRAME Traversal stack frame.
           05  WS-FRAME                  OCCURS 64 TIMES.
      * @field WS-STK-IDX Active IR field or DOM node index.
               10  WS-STK-IDX            PIC 9(4) COMP.
      * @field WS-STK-PARENT Parent IR or DOM node index.
               10  WS-STK-PARENT         PIC 9(4) COMP.
      * @field WS-STK-PHASE Frame phase (1=children, 2=close).
               10  WS-STK-PHASE          PIC 9(2) COMP.
      * @record WS-EMT-STATE Formatting state and scratch variables.
       01  WS-EMT-STATE.
      * @field WS-INDENT Current 2-space indentation level (0..64).
           05  WS-INDENT                 PIC 9(4) COMP VALUE 0.
      * @field WS-LF Cached LF byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR Cached CR byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-FIDX Current IR field index being emitted.
           05  WS-FIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-DIDX Current DOM node index being emitted.
           05  WS-DIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-PIDX Parent node index.
           05  WS-PIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-REQ-ITER Sibling scan cursor for 'required' list.
           05  WS-REQ-ITER               PIC 9(4) COMP VALUE 0.
      * @field WS-REQ-CNT Count of required fields in current group.
           05  WS-REQ-CNT                PIC 9(4) COMP VALUE 0.
      * @field WS-REQ-EMITTED Number of required names emitted so far.
           05  WS-REQ-EMITTED            PIC 9(4) COMP VALUE 0.
      * @field WS-EIDX Enum index cursor.
           05  WS-EIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-ECNT Enum loop counter.
           05  WS-ECNT                   PIC 9(4) COMP VALUE 0.
      * @field WS-I Generic loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-SLEN Scratch byte length to append.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-JSON-TNAME Resolved JSON Schema scalar type name.
           05  WS-JSON-TNAME             PIC X(16) VALUE SPACES.
      * @field WS-JSON-TLEN Length of WS-JSON-TNAME.
           05  WS-JSON-TLEN              PIC 9(2) COMP VALUE 0.
      * @field WS-S64-VAL Scratch signed integer for formatting.
           05  WS-S64-VAL                PIC S9(12) COMP VALUE 0.
      * @record WS-STR-LIT Scratch buffer for literal slice emission.
       01  WS-STR-LIT                    PIC X(256) VALUE SPACES.
      * @record WS-ESC-SRC Scratch buffer for JSON string escaping.
       01  WS-ESC-SRC                    PIC X(256) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-jsch.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDJEMT Serialize IR Schema, Sample JSON, or DOM.
      * @param [in] CDD-JEMT-REQ Mode and formatting options.
      * @param [in] CDD-IR Canonical IR input (for SCHEMA/SAMPLE).
      * @param [in] CDD-JDOM JSON DOM input (for DOM mode).
      * @param [inout] CDD-BUF Output byte buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Serialized JSON text in CDD-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-GEN-BUF-OVERFLOW.
       PROCEDURE DIVISION USING CDD-JEMT-REQ CDD-IR CDD-JDOM CDD-BUF
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags and dispatch emit mode.
       MAIN-ENTRY.
           IF NOT CDD-JEM-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               OR NOT CDD-JDM-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDJEMT" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDJEMT"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-BUF-LEN
           MOVE 0 TO WS-INDENT
           MOVE 0 TO WS-DEPTH
           EVALUATE TRUE
               WHEN CDD-JEM-MODE-SCHEMA
                   PERFORM EMIT-IR-AS-SCHEMA
               WHEN CDD-JEM-MODE-SAMPLE
                   PERFORM EMIT-IR-AS-SAMPLE
               WHEN OTHER
                   PERFORM EMIT-DOM-TREE
           END-EVALUATE
           IF CDD-OK AND CDD-JEM-STYLE-PRETTY
               PERFORM APPEND-NL
           END-IF
           EXIT PROGRAM.
      * @paragraph APPEND-RAW Append WS-STR-LIT(1:WS-SLEN) to CDD-BUF.
       APPEND-RAW.
           IF CDD-OK AND WS-SLEN > 0
               IF CDD-BUF-LEN + WS-SLEN > CDD-BUF-CAP
                   MOVE 4001 TO CDD-ERR-CODE
                   MOVE "CDDJEMT" TO CDD-ERR-SUBPROG
                   MOVE "JSON emitter buffer overflow"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE WS-STR-LIT(1:WS-SLEN)
                       TO CDD-BUF-DATA(CDD-BUF-LEN + 1:WS-SLEN)
                   ADD WS-SLEN TO CDD-BUF-LEN
                   END-ADD
               END-IF
           END-IF.
      * @paragraph APPEND-NL Append LF or CRLF in pretty mode.
       APPEND-NL.
           IF CDD-JEM-STYLE-PRETTY
               IF CDD-BUF-NL-CRLF
                   MOVE WS-CR TO WS-STR-LIT(1:1)
                   MOVE WS-LF TO WS-STR-LIT(2:1)
                   MOVE 2 TO WS-SLEN
               ELSE
                   MOVE WS-LF TO WS-STR-LIT(1:1)
                   MOVE 1 TO WS-SLEN
               END-IF
               PERFORM APPEND-RAW
           END-IF.
      * @paragraph APPEND-INDENT Emit 2 * WS-INDENT spaces if pretty.
       APPEND-INDENT.
           IF CDD-JEM-STYLE-PRETTY AND WS-INDENT > 0
               MOVE SPACES TO WS-STR-LIT
               COMPUTE WS-SLEN = WS-INDENT * 2
               END-COMPUTE
               PERFORM APPEND-RAW
           END-IF.
      * @paragraph APPEND-COLON Emit ': ' (pretty) or ':' (compact).
       APPEND-COLON.
           IF CDD-JEM-STYLE-PRETTY
               MOVE ": " TO WS-STR-LIT(1:2)
               MOVE 2 TO WS-SLEN
           ELSE
               MOVE ":" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
           END-IF
           PERFORM APPEND-RAW.
      * @paragraph APPEND-QUOTED Escape WS-ESC-SRC(1:WS-SLEN) + quotes.
       APPEND-QUOTED.
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 4 TO CDD-SRQ-OP
           MOVE WS-SLEN TO CDD-SRQ-IN-LEN
           IF WS-SLEN > 0
               MOVE WS-ESC-SRC(1:WS-SLEN)
                   TO CDD-SRQ-IN-TEXT(1:WS-SLEN)
           END-IF
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           MOVE '"' TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           IF CDD-SRQ-OUT-LEN > 0
               MOVE CDD-SRQ-OUT-TEXT(1:CDD-SRQ-OUT-LEN)
                   TO WS-STR-LIT(1:CDD-SRQ-OUT-LEN)
               MOVE CDD-SRQ-OUT-LEN TO WS-SLEN
               PERFORM APPEND-RAW
           END-IF
           MOVE '"' TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW.
      * @paragraph APPEND-S64 Format signed WS-S64-VAL and append.
       APPEND-S64.
           IF WS-S64-VAL < 0
               MOVE "-" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               COMPUTE CDD-SRQ-NUM-VAL = 0 - WS-S64-VAL
               END-COMPUTE
           ELSE
               COMPUTE CDD-SRQ-NUM-VAL = WS-S64-VAL
               END-COMPUTE
           END-IF
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 7 TO CDD-SRQ-OP
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           MOVE CDD-SRQ-OUT-TEXT(1:CDD-SRQ-OUT-LEN)
               TO WS-STR-LIT(1:CDD-SRQ-OUT-LEN)
           MOVE CDD-SRQ-OUT-LEN TO WS-SLEN
           PERFORM APPEND-RAW.
      * @paragraph EMIT-KEY-PREFIX Emit indent + "key": prefix.
       EMIT-KEY-PREFIX.
           PERFORM APPEND-INDENT
           PERFORM APPEND-QUOTED
           PERFORM APPEND-COLON.
      * @paragraph EMIT-IR-AS-SCHEMA Emit Draft 2020-12 JSON Schema.
       EMIT-IR-AS-SCHEMA.
           MOVE "{" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           ADD 1 TO WS-INDENT
           END-ADD
           MOVE "$schema" TO WS-ESC-SRC(1:7)
           MOVE 7 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "https://json-schema.org/draft/2020-12/schema"
               TO WS-ESC-SRC(1:44)
           MOVE 44 TO WS-SLEN
           PERFORM APPEND-QUOTED
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           IF CDD-IR-TITLE-LEN > 0
               MOVE "title" TO WS-ESC-SRC(1:5)
               MOVE 5 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE CDD-IR-JSON-TITLE(1:CDD-IR-TITLE-LEN)
                   TO WS-ESC-SRC(1:CDD-IR-TITLE-LEN)
               MOVE CDD-IR-TITLE-LEN TO WS-SLEN
               PERFORM APPEND-QUOTED
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
           END-IF
           IF CDD-IR-DESC-LEN > 0
               MOVE "description" TO WS-ESC-SRC(1:11)
               MOVE 11 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE CDD-IR-DESC(1:CDD-IR-DESC-LEN)
                   TO WS-ESC-SRC(1:CDD-IR-DESC-LEN)
               MOVE CDD-IR-DESC-LEN TO WS-SLEN
               PERFORM APPEND-QUOTED
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
           END-IF
           MOVE "type" TO WS-ESC-SRC(1:4)
           MOVE 4 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "object" TO WS-ESC-SRC(1:6)
           MOVE 6 TO WS-SLEN
           PERFORM APPEND-QUOTED
           MOVE 0 TO WS-PIDX
           PERFORM OPEN-GROUP-PROPS-FRAME
           PERFORM DRIVE-SCHEMA-STACK
           SUBTRACT 1 FROM WS-INDENT
           END-SUBTRACT
           PERFORM APPEND-NL
           MOVE "}" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW.
      * @paragraph OPEN-GROUP-PROPS-FRAME Emit ,"properties":{ & push.
       OPEN-GROUP-PROPS-FRAME.
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "properties" TO WS-ESC-SRC(1:10)
           MOVE 10 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "{" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           ADD 1 TO WS-DEPTH
           END-ADD
           MOVE WS-PIDX TO WS-STK-PARENT(WS-DEPTH)
           IF WS-PIDX = 0
               MOVE CDD-IR-ROOT-FIRST TO WS-STK-IDX(WS-DEPTH)
           ELSE
               MOVE CDD-IRF-FIRST-CHILD(WS-PIDX)
                   TO WS-STK-IDX(WS-DEPTH)
           END-IF
           MOVE 1 TO WS-STK-PHASE(WS-DEPTH)
           IF WS-STK-IDX(WS-DEPTH) > 0
               PERFORM APPEND-NL
               ADD 1 TO WS-INDENT
               END-ADD
           END-IF.
      * @paragraph DRIVE-SCHEMA-STACK Non-recursively emit all levels.
       DRIVE-SCHEMA-STACK.
           PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
               MOVE WS-STK-IDX(WS-DEPTH) TO WS-FIDX
               MOVE WS-STK-PARENT(WS-DEPTH) TO WS-PIDX
               IF WS-STK-PHASE(WS-DEPTH) = 2
                   PERFORM CLOSE-NESTED-OBJ-SCHEMA
               ELSE
                   IF WS-FIDX = 0
                       PERFORM CLOSE-PROPS-AND-REQ
                   ELSE
                       MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                           TO WS-STK-IDX(WS-DEPTH)
                       PERFORM EMIT-FIELD-SCHEMA-BEGIN
                   END-IF
               END-IF
           END-PERFORM.
      * @paragraph CLOSE-PROPS-AND-REQ Close properties + emit required.
       CLOSE-PROPS-AND-REQ.
           IF WS-PIDX = 0
               MOVE CDD-IR-ROOT-FIRST TO WS-REQ-ITER
           ELSE
               MOVE CDD-IRF-FIRST-CHILD(WS-PIDX) TO WS-REQ-ITER
           END-IF
           IF WS-REQ-ITER > 0
               SUBTRACT 1 FROM WS-INDENT
               END-SUBTRACT
               PERFORM APPEND-NL
               PERFORM APPEND-INDENT
           END-IF
           MOVE "}" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM EMIT-REQUIRED-ARRAY
           SUBTRACT 1 FROM WS-DEPTH
           END-SUBTRACT.
      * @paragraph EMIT-REQUIRED-ARRAY Emit ,"required":[...] if >0.
       EMIT-REQUIRED-ARRAY.
           MOVE 0 TO WS-REQ-CNT
           IF WS-PIDX = 0
               MOVE CDD-IR-ROOT-FIRST TO WS-REQ-ITER
           ELSE
               MOVE CDD-IRF-FIRST-CHILD(WS-PIDX) TO WS-REQ-ITER
           END-IF
           PERFORM UNTIL WS-REQ-ITER = 0
               IF CDD-IRF-REQ(WS-REQ-ITER)
                   AND CDD-IRF-REDEF-LEN(WS-REQ-ITER) = 0
                   ADD 1 TO WS-REQ-CNT
                   END-ADD
               END-IF
               MOVE CDD-IRF-NEXT-SIB(WS-REQ-ITER) TO WS-REQ-ITER
           END-PERFORM
           IF WS-REQ-CNT > 0
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
               MOVE "required" TO WS-ESC-SRC(1:8)
               MOVE 8 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE "[" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               MOVE 0 TO WS-REQ-EMITTED
               IF WS-PIDX = 0
                   MOVE CDD-IR-ROOT-FIRST TO WS-REQ-ITER
               ELSE
                   MOVE CDD-IRF-FIRST-CHILD(WS-PIDX) TO WS-REQ-ITER
               END-IF
               PERFORM UNTIL WS-REQ-ITER = 0
                   IF CDD-IRF-REQ(WS-REQ-ITER)
                       AND CDD-IRF-REDEF-LEN(WS-REQ-ITER) = 0
                       IF WS-REQ-EMITTED > 0
                           MOVE "," TO WS-STR-LIT(1:1)
                           MOVE 1 TO WS-SLEN
                           PERFORM APPEND-RAW
                       END-IF
                       MOVE CDD-IRF-JSON-KEY(WS-REQ-ITER)(
                           1:CDD-IRF-JKEY-LEN(WS-REQ-ITER))
                           TO WS-ESC-SRC(
                           1:CDD-IRF-JKEY-LEN(WS-REQ-ITER))
                       MOVE CDD-IRF-JKEY-LEN(WS-REQ-ITER) TO WS-SLEN
                       PERFORM APPEND-QUOTED
                       ADD 1 TO WS-REQ-EMITTED
                       END-ADD
                   END-IF
                   MOVE CDD-IRF-NEXT-SIB(WS-REQ-ITER) TO WS-REQ-ITER
               END-PERFORM
               MOVE "]" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
           END-IF.
      * @paragraph EMIT-FIELD-SCHEMA-BEGIN Emit single property schema.
       EMIT-FIELD-SCHEMA-BEGIN.
           MOVE CDD-IRF-JSON-KEY(WS-FIDX)(1:CDD-IRF-JKEY-LEN(WS-FIDX))
               TO WS-ESC-SRC(1:CDD-IRF-JKEY-LEN(WS-FIDX))
           MOVE CDD-IRF-JKEY-LEN(WS-FIDX) TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "{" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           ADD 1 TO WS-INDENT
           END-ADD
           IF CDD-IRF-DESC-LEN(WS-FIDX) > 0
               MOVE "description" TO WS-ESC-SRC(1:11)
               MOVE 11 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE CDD-IRF-DESC(WS-FIDX)(1:CDD-IRF-DESC-LEN(WS-FIDX))
                   TO WS-ESC-SRC(1:CDD-IRF-DESC-LEN(WS-FIDX))
               MOVE CDD-IRF-DESC-LEN(WS-FIDX) TO WS-SLEN
               PERFORM APPEND-QUOTED
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
           END-IF
           IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
               PERFORM EMIT-ARRAY-WRAPPER-BEGIN
           ELSE
               PERFORM EMIT-INNER-FIELD-SCHEMA
           END-IF.
      * @paragraph EMIT-ARRAY-WRAPPER-BEGIN Emit type:array + items:{.
       EMIT-ARRAY-WRAPPER-BEGIN.
           MOVE "type" TO WS-ESC-SRC(1:4)
           MOVE 4 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "array" TO WS-ESC-SRC(1:5)
           MOVE 5 TO WS-SLEN
           PERFORM APPEND-QUOTED
           IF CDD-IRF-ARR-FIXED(WS-FIDX)
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
               MOVE "minItems" TO WS-ESC-SRC(1:8)
               MOVE 8 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE CDD-IRF-ARR-MAX(WS-FIDX) TO WS-S64-VAL
               PERFORM APPEND-S64
           ELSE
               IF CDD-IRF-ARR-MIN(WS-FIDX) > 0
                   MOVE "," TO WS-STR-LIT(1:1)
                   MOVE 1 TO WS-SLEN
                   PERFORM APPEND-RAW
                   PERFORM APPEND-NL
                   MOVE "minItems" TO WS-ESC-SRC(1:8)
                   MOVE 8 TO WS-SLEN
                   PERFORM EMIT-KEY-PREFIX
                   MOVE CDD-IRF-ARR-MIN(WS-FIDX) TO WS-S64-VAL
                   PERFORM APPEND-S64
               END-IF
           END-IF
           IF NOT CDD-IRF-ARR-DYNAMIC(WS-FIDX)
               AND CDD-IRF-ARR-MAX(WS-FIDX) > 0
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
               MOVE "maxItems" TO WS-ESC-SRC(1:8)
               MOVE 8 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE CDD-IRF-ARR-MAX(WS-FIDX) TO WS-S64-VAL
               PERFORM APPEND-S64
           END-IF
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "items" TO WS-ESC-SRC(1:5)
           MOVE 5 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "{" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           ADD 1 TO WS-INDENT
           END-ADD
           PERFORM EMIT-INNER-FIELD-SCHEMA.
      * @paragraph EMIT-INNER-FIELD-SCHEMA Emit scalar or push nested.
       EMIT-INNER-FIELD-SCHEMA.
           PERFORM RESOLVE-IR-JSON-TYPE
           MOVE "type" TO WS-ESC-SRC(1:4)
           MOVE 4 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           IF CDD-IRF-CAN-NULL(WS-FIDX)
               AND CDD-IRF-ARR-NONE(WS-FIDX)
               MOVE "[" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               MOVE WS-JSON-TNAME(1:WS-JSON-TLEN)
                   TO WS-ESC-SRC(1:WS-JSON-TLEN)
               MOVE WS-JSON-TLEN TO WS-SLEN
               PERFORM APPEND-QUOTED
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               MOVE "null" TO WS-ESC-SRC(1:4)
               MOVE 4 TO WS-SLEN
               PERFORM APPEND-QUOTED
               MOVE "]" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
           ELSE
               MOVE WS-JSON-TNAME(1:WS-JSON-TLEN)
                   TO WS-ESC-SRC(1:WS-JSON-TLEN)
               MOVE WS-JSON-TLEN TO WS-SLEN
               PERFORM APPEND-QUOTED
           END-IF
           IF CDD-IRF-IS-GROUP(WS-FIDX)
               ADD 1 TO WS-DEPTH
               END-ADD
               MOVE WS-FIDX TO WS-STK-IDX(WS-DEPTH)
               MOVE WS-PIDX TO WS-STK-PARENT(WS-DEPTH)
               MOVE 2 TO WS-STK-PHASE(WS-DEPTH)
               MOVE WS-FIDX TO WS-PIDX
               PERFORM OPEN-GROUP-PROPS-FRAME
           ELSE
               PERFORM EMIT-SCALAR-FACETS
               PERFORM FINISH-FIELD-CLOSING-BRACES
           END-IF.
      * @paragraph CLOSE-NESTED-OBJ-SCHEMA Pop phase-2 group wrapper.
       CLOSE-NESTED-OBJ-SCHEMA.
           SUBTRACT 1 FROM WS-DEPTH
           END-SUBTRACT
           PERFORM FINISH-FIELD-CLOSING-BRACES.
      * @paragraph FINISH-FIELD-CLOSING-BRACES Close items/prop + comma.
       FINISH-FIELD-CLOSING-BRACES.
           IF CDD-JEM-XCOB-YES
               PERFORM EMIT-X-COBOL-EXT
           END-IF
           IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
               SUBTRACT 1 FROM WS-INDENT
               END-SUBTRACT
               PERFORM APPEND-NL
               PERFORM APPEND-INDENT
               MOVE "}" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
           END-IF
           SUBTRACT 1 FROM WS-INDENT
           END-SUBTRACT
           PERFORM APPEND-NL
           PERFORM APPEND-INDENT
           MOVE "}" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           IF CDD-IRF-NEXT-SIB(WS-FIDX) > 0
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
           END-IF.
      * @paragraph RESOLVE-IR-JSON-TYPE Map CDD-IRF-KIND to JSON name.
       RESOLVE-IR-JSON-TYPE.
           EVALUATE TRUE
               WHEN CDD-IRF-IS-GROUP(WS-FIDX)
                   MOVE "object" TO WS-JSON-TNAME
                   MOVE 6 TO WS-JSON-TLEN
               WHEN CDD-IRF-IS-INTEGER(WS-FIDX)
                   MOVE "integer" TO WS-JSON-TNAME
                   MOVE 7 TO WS-JSON-TLEN
               WHEN CDD-IRF-IS-DECIMAL(WS-FIDX)
               WHEN CDD-IRF-IS-FLOAT(WS-FIDX)
               WHEN CDD-IRF-IS-DECFLOAT(WS-FIDX)
                   MOVE "number" TO WS-JSON-TNAME
                   MOVE 6 TO WS-JSON-TLEN
               WHEN CDD-IRF-IS-BOOL(WS-FIDX)
                   MOVE "boolean" TO WS-JSON-TNAME
                   MOVE 7 TO WS-JSON-TLEN
               WHEN OTHER
                   MOVE "string" TO WS-JSON-TNAME
                   MOVE 6 TO WS-JSON-TLEN
           END-EVALUATE.
      * @paragraph EMIT-SCALAR-FACETS Emit lengths, scale, bounds, enum.
       EMIT-SCALAR-FACETS.
           IF (CDD-IRF-IS-STRING(WS-FIDX)
               OR CDD-IRF-IS-UTF8(WS-FIDX)
               OR CDD-IRF-IS-NATIONAL(WS-FIDX))
               AND CDD-IRF-MAX-LEN(WS-FIDX) > 0
               IF CDD-IRF-MIN-LEN(WS-FIDX) > 0
                   MOVE "," TO WS-STR-LIT(1:1)
                   MOVE 1 TO WS-SLEN
                   PERFORM APPEND-RAW
                   PERFORM APPEND-NL
                   MOVE "minLength" TO WS-ESC-SRC(1:9)
                   MOVE 9 TO WS-SLEN
                   PERFORM EMIT-KEY-PREFIX
                   MOVE CDD-IRF-MIN-LEN(WS-FIDX) TO WS-S64-VAL
                   PERFORM APPEND-S64
               END-IF
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
               MOVE "maxLength" TO WS-ESC-SRC(1:9)
               MOVE 9 TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-S64-VAL
               PERFORM APPEND-S64
           END-IF
           IF CDD-IRF-FORMAT(WS-FIDX) NOT = SPACES
               PERFORM EMIT-FORMAT-FACET
           END-IF
           IF CDD-IRF-IS-DECIMAL(WS-FIDX)
               AND CDD-IRF-SCALE(WS-FIDX) > 0
               PERFORM EMIT-MULTIPLE-OF-FACET
           END-IF
           IF CDD-IRF-HAS-BOUNDS(WS-FIDX) = 1
               PERFORM EMIT-MIN-MAX-FACETS
           END-IF
           IF CDD-IRF-ENUM-COUNT(WS-FIDX) > 0
               PERFORM EMIT-ENUM-FACET
           END-IF.
      * @paragraph EMIT-FORMAT-FACET Emit ,"format":"...".
       EMIT-FORMAT-FACET.
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "format" TO WS-ESC-SRC(1:6)
           MOVE 6 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE 16 TO WS-I
           PERFORM UNTIL WS-I = 1
               OR CDD-IRF-FORMAT(WS-FIDX)(WS-I:1) NOT = SPACE
               SUBTRACT 1 FROM WS-I
               END-SUBTRACT
           END-PERFORM
           MOVE CDD-IRF-FORMAT(WS-FIDX)(1:WS-I) TO WS-ESC-SRC(1:WS-I)
           MOVE WS-I TO WS-SLEN
           PERFORM APPEND-QUOTED.
      * @paragraph EMIT-MULTIPLE-OF-FACET Emit ,"multipleOf":0.0...1.
       EMIT-MULTIPLE-OF-FACET.
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "multipleOf" TO WS-ESC-SRC(1:10)
           MOVE 10 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "0." TO WS-STR-LIT(1:2)
           MOVE 2 TO WS-SLEN
           PERFORM APPEND-RAW
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I >= CDD-IRF-SCALE(WS-FIDX)
               MOVE "0" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE "1" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW.
      * @paragraph EMIT-MIN-MAX-FACETS Emit minimum and maximum.
       EMIT-MIN-MAX-FACETS.
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "minimum" TO WS-ESC-SRC(1:7)
           MOVE 7 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE CDD-IRF-MIN-VAL(WS-FIDX) TO WS-S64-VAL
           PERFORM APPEND-S64
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "maximum" TO WS-ESC-SRC(1:7)
           MOVE 7 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE CDD-IRF-MAX-VAL(WS-FIDX) TO WS-S64-VAL
           PERFORM APPEND-S64.
      * @paragraph EMIT-ENUM-FACET Emit ,"enum":[...].
       EMIT-ENUM-FACET.
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "enum" TO WS-ESC-SRC(1:4)
           MOVE 4 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "[" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           MOVE CDD-IRF-ENUM-FIRST(WS-FIDX) TO WS-EIDX
           MOVE 0 TO WS-ECNT
           PERFORM UNTIL WS-ECNT >= CDD-IRF-ENUM-COUNT(WS-FIDX)
               IF WS-ECNT > 0
                   MOVE "," TO WS-STR-LIT(1:1)
                   MOVE 1 TO WS-SLEN
                   PERFORM APPEND-RAW
               END-IF
               IF CDD-IRE-IS-STR(WS-EIDX) = 1
                   MOVE CDD-IRE-VAL(WS-EIDX)(
                       1:CDD-IRE-VAL-LEN(WS-EIDX))
                       TO WS-ESC-SRC(1:CDD-IRE-VAL-LEN(WS-EIDX))
                   MOVE CDD-IRE-VAL-LEN(WS-EIDX) TO WS-SLEN
                   PERFORM APPEND-QUOTED
               ELSE
                   MOVE CDD-IRE-VAL(WS-EIDX)(
                       1:CDD-IRE-VAL-LEN(WS-EIDX))
                       TO WS-STR-LIT(1:CDD-IRE-VAL-LEN(WS-EIDX))
                   MOVE CDD-IRE-VAL-LEN(WS-EIDX) TO WS-SLEN
                   PERFORM APPEND-RAW
               END-IF
               ADD 1 TO WS-EIDX
               END-ADD
               ADD 1 TO WS-ECNT
               END-ADD
           END-PERFORM
           MOVE "]" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW.
      * @paragraph EMIT-X-COBOL-EXT Emit ,"x-cobol":{"digits":...}.
       EMIT-X-COBOL-EXT.
           MOVE "," TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           PERFORM APPEND-NL
           MOVE "x-cobol" TO WS-ESC-SRC(1:7)
           MOVE 7 TO WS-SLEN
           PERFORM EMIT-KEY-PREFIX
           MOVE "{" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           MOVE "usage" TO WS-ESC-SRC(1:5)
           MOVE 5 TO WS-SLEN
           PERFORM APPEND-QUOTED
           PERFORM APPEND-COLON
           MOVE CDD-IRF-USAGE(WS-FIDX) TO WS-S64-VAL
           PERFORM APPEND-S64
           IF CDD-IRF-PIC-LEN(WS-FIDX) > 0
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               MOVE "pic" TO WS-ESC-SRC(1:3)
               MOVE 3 TO WS-SLEN
               PERFORM APPEND-QUOTED
               PERFORM APPEND-COLON
               MOVE CDD-IRF-PIC-TEXT(WS-FIDX)(
                   1:CDD-IRF-PIC-LEN(WS-FIDX))
                   TO WS-ESC-SRC(1:CDD-IRF-PIC-LEN(WS-FIDX))
               MOVE CDD-IRF-PIC-LEN(WS-FIDX) TO WS-SLEN
               PERFORM APPEND-QUOTED
           END-IF
           MOVE "}" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW.
      * @paragraph EMIT-IR-AS-SAMPLE Emit sample JSON object from IR.
       EMIT-IR-AS-SAMPLE.
           MOVE "{" TO WS-STR-LIT(1:1)
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           MOVE 1 TO WS-DEPTH
           MOVE CDD-IR-ROOT-FIRST TO WS-STK-IDX(1)
           MOVE 0 TO WS-STK-PARENT(1)
           MOVE 1 TO WS-STK-PHASE(1)
           IF CDD-IR-ROOT-FIRST > 0
               PERFORM APPEND-NL
               ADD 1 TO WS-INDENT
               END-ADD
           END-IF
           PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
               MOVE WS-STK-IDX(WS-DEPTH) TO WS-FIDX
               MOVE WS-STK-PARENT(WS-DEPTH) TO WS-PIDX
               IF WS-FIDX = 0
                   PERFORM CLOSE-SAMPLE-FRAME
               ELSE
                   MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                       TO WS-STK-IDX(WS-DEPTH)
                   PERFORM EMIT-SAMPLE-FIELD
               END-IF
           END-PERFORM.
      * @paragraph CLOSE-SAMPLE-FRAME Emit closing brace/bracket.
       CLOSE-SAMPLE-FRAME.
           IF WS-PIDX = 0
               IF CDD-IR-ROOT-FIRST > 0
                   SUBTRACT 1 FROM WS-INDENT
                   END-SUBTRACT
                   PERFORM APPEND-NL
                   PERFORM APPEND-INDENT
               END-IF
               MOVE "}" TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
           ELSE
               IF CDD-IRF-FIRST-CHILD(WS-PIDX) > 0
                   SUBTRACT 1 FROM WS-INDENT
                   END-SUBTRACT
                   PERFORM APPEND-NL
                   PERFORM APPEND-INDENT
               END-IF
               IF NOT CDD-IRF-ARR-NONE(WS-PIDX)
                   MOVE "}]" TO WS-STR-LIT(1:2)
                   MOVE 2 TO WS-SLEN
               ELSE
                   MOVE "}" TO WS-STR-LIT(1:1)
                   MOVE 1 TO WS-SLEN
               END-IF
               PERFORM APPEND-RAW
               IF CDD-IRF-NEXT-SIB(WS-PIDX) > 0
                   MOVE "," TO WS-STR-LIT(1:1)
                   MOVE 1 TO WS-SLEN
                   PERFORM APPEND-RAW
                   PERFORM APPEND-NL
               END-IF
           END-IF
           SUBTRACT 1 FROM WS-DEPTH
           END-SUBTRACT.
      * @paragraph EMIT-SAMPLE-FIELD Emit key + default/nested sample.
       EMIT-SAMPLE-FIELD.
           IF CDD-IRF-REDEF-LEN(WS-FIDX) = 0
               MOVE CDD-IRF-JSON-KEY(WS-FIDX)(
                   1:CDD-IRF-JKEY-LEN(WS-FIDX))
                   TO WS-ESC-SRC(1:CDD-IRF-JKEY-LEN(WS-FIDX))
               MOVE CDD-IRF-JKEY-LEN(WS-FIDX) TO WS-SLEN
               PERFORM EMIT-KEY-PREFIX
               IF CDD-IRF-IS-GROUP(WS-FIDX)
                   IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
                       MOVE "[{" TO WS-STR-LIT(1:2)
                       MOVE 2 TO WS-SLEN
                   ELSE
                       MOVE "{" TO WS-STR-LIT(1:1)
                       MOVE 1 TO WS-SLEN
                   END-IF
                   PERFORM APPEND-RAW
                   ADD 1 TO WS-DEPTH
                   END-ADD
                   MOVE CDD-IRF-FIRST-CHILD(WS-FIDX)
                       TO WS-STK-IDX(WS-DEPTH)
                   MOVE WS-FIDX TO WS-STK-PARENT(WS-DEPTH)
                   MOVE 1 TO WS-STK-PHASE(WS-DEPTH)
                   IF CDD-IRF-FIRST-CHILD(WS-FIDX) > 0
                       PERFORM APPEND-NL
                       ADD 1 TO WS-INDENT
                       END-ADD
                   END-IF
               ELSE
                   IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
                       MOVE "[" TO WS-STR-LIT(1:1)
                       MOVE 1 TO WS-SLEN
                       PERFORM APPEND-RAW
                       PERFORM EMIT-SCALAR-SAMPLE-VAL
                       MOVE "]" TO WS-STR-LIT(1:1)
                       MOVE 1 TO WS-SLEN
                       PERFORM APPEND-RAW
                   ELSE
                       PERFORM EMIT-SCALAR-SAMPLE-VAL
                   END-IF
                   IF CDD-IRF-NEXT-SIB(WS-FIDX) > 0
                       MOVE "," TO WS-STR-LIT(1:1)
                       MOVE 1 TO WS-SLEN
                       PERFORM APPEND-RAW
                       PERFORM APPEND-NL
                   END-IF
               END-IF
           END-IF.
      * @paragraph EMIT-SCALAR-SAMPLE-VAL Emit VALUE or type default.
       EMIT-SCALAR-SAMPLE-VAL.
           EVALUATE TRUE
               WHEN CDD-IRF-IS-INTEGER(WS-FIDX)
                   IF CDD-IRF-DEF-LEN(WS-FIDX) > 0
                       MOVE CDD-IRF-DEF-VAL(WS-FIDX)(
                           1:CDD-IRF-DEF-LEN(WS-FIDX))
                           TO WS-STR-LIT(1:CDD-IRF-DEF-LEN(WS-FIDX))
                       MOVE CDD-IRF-DEF-LEN(WS-FIDX) TO WS-SLEN
                   ELSE
                       MOVE "0" TO WS-STR-LIT(1:1)
                       MOVE 1 TO WS-SLEN
                   END-IF
                   PERFORM APPEND-RAW
               WHEN CDD-IRF-IS-DECIMAL(WS-FIDX)
               WHEN CDD-IRF-IS-FLOAT(WS-FIDX)
               WHEN CDD-IRF-IS-DECFLOAT(WS-FIDX)
                   IF CDD-IRF-DEF-LEN(WS-FIDX) > 0
                       MOVE CDD-IRF-DEF-VAL(WS-FIDX)(
                           1:CDD-IRF-DEF-LEN(WS-FIDX))
                           TO WS-STR-LIT(1:CDD-IRF-DEF-LEN(WS-FIDX))
                       MOVE CDD-IRF-DEF-LEN(WS-FIDX) TO WS-SLEN
                   ELSE
                       MOVE "0.0" TO WS-STR-LIT(1:3)
                       MOVE 3 TO WS-SLEN
                   END-IF
                   PERFORM APPEND-RAW
               WHEN CDD-IRF-IS-BOOL(WS-FIDX)
                   IF CDD-IRF-DEF-LEN(WS-FIDX) = 4
                       AND CDD-IRF-DEF-VAL(WS-FIDX)(1:4) = "true"
                       MOVE "true" TO WS-STR-LIT(1:4)
                       MOVE 4 TO WS-SLEN
                   ELSE
                       MOVE "false" TO WS-STR-LIT(1:5)
                       MOVE 5 TO WS-SLEN
                   END-IF
                   PERFORM APPEND-RAW
               WHEN OTHER
                   IF CDD-IRF-DEF-LEN(WS-FIDX) > 0
                       MOVE CDD-IRF-DEF-VAL(WS-FIDX)(
                           1:CDD-IRF-DEF-LEN(WS-FIDX))
                           TO WS-ESC-SRC(1:CDD-IRF-DEF-LEN(WS-FIDX))
                       MOVE CDD-IRF-DEF-LEN(WS-FIDX) TO WS-SLEN
                   ELSE
                       MOVE 0 TO WS-SLEN
                   END-IF
                   PERFORM APPEND-QUOTED
           END-EVALUATE.
      * @paragraph EMIT-DOM-TREE Serialize CDD-JDOM iteratively.
       EMIT-DOM-TREE.
           IF CDD-JDM-ROOT > 0
               MOVE CDD-JDM-ROOT TO WS-DIDX
               PERFORM EMIT-DOM-NODE-START
               PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
                   MOVE WS-STK-IDX(WS-DEPTH) TO WS-DIDX
                   MOVE WS-STK-PARENT(WS-DEPTH) TO WS-PIDX
                   IF WS-DIDX = 0
                       PERFORM CLOSE-DOM-CONTAINER
                   ELSE
                       MOVE CDD-JDN-NEXT-SIB(WS-DIDX)
                           TO WS-STK-IDX(WS-DEPTH)
                       PERFORM APPEND-INDENT
                       IF CDD-JDN-IS-OBJECT(WS-PIDX)
                           MOVE CDD-JDN-KEY(WS-DIDX)(
                               1:CDD-JDN-KEY-LEN(WS-DIDX))
                               TO WS-ESC-SRC(
                               1:CDD-JDN-KEY-LEN(WS-DIDX))
                           MOVE CDD-JDN-KEY-LEN(WS-DIDX) TO WS-SLEN
                           PERFORM APPEND-QUOTED
                           PERFORM APPEND-COLON
                       END-IF
                       PERFORM EMIT-DOM-NODE-START
                   END-IF
               END-PERFORM
           END-IF.
      * @paragraph EMIT-DOM-NODE-START Emit scalar or open container.
       EMIT-DOM-NODE-START.
           EVALUATE TRUE
               WHEN CDD-JDN-IS-OBJECT(WS-DIDX)
               WHEN CDD-JDN-IS-ARRAY(WS-DIDX)
                   IF CDD-JDN-IS-OBJECT(WS-DIDX)
                       MOVE "{" TO WS-STR-LIT(1:1)
                   ELSE
                       MOVE "[" TO WS-STR-LIT(1:1)
                   END-IF
                   MOVE 1 TO WS-SLEN
                   PERFORM APPEND-RAW
                   ADD 1 TO WS-DEPTH
                   END-ADD
                   MOVE CDD-JDN-FIRST-CHILD(WS-DIDX)
                       TO WS-STK-IDX(WS-DEPTH)
                   MOVE WS-DIDX TO WS-STK-PARENT(WS-DEPTH)
                   IF CDD-JDN-FIRST-CHILD(WS-DIDX) > 0
                       PERFORM APPEND-NL
                       ADD 1 TO WS-INDENT
                       END-ADD
                   END-IF
               WHEN CDD-JDN-IS-STRING(WS-DIDX)
                   MOVE CDD-JDN-VAL-LEN(WS-DIDX) TO WS-SLEN
                   IF WS-SLEN > 0
                       MOVE CDD-JDN-VAL(WS-DIDX)(1:WS-SLEN)
                           TO WS-ESC-SRC(1:WS-SLEN)
                   END-IF
                   PERFORM APPEND-QUOTED
                   PERFORM EMIT-DOM-SIB-COMMA
               WHEN CDD-JDN-IS-INTEGER(WS-DIDX)
               WHEN CDD-JDN-IS-NUMBER(WS-DIDX)
                   MOVE CDD-JDN-VAL-LEN(WS-DIDX) TO WS-SLEN
                   MOVE CDD-JDN-VAL(WS-DIDX)(1:WS-SLEN)
                       TO WS-STR-LIT(1:WS-SLEN)
                   PERFORM APPEND-RAW
                   PERFORM EMIT-DOM-SIB-COMMA
               WHEN CDD-JDN-IS-TRUE(WS-DIDX)
                   MOVE "true" TO WS-STR-LIT(1:4)
                   MOVE 4 TO WS-SLEN
                   PERFORM APPEND-RAW
                   PERFORM EMIT-DOM-SIB-COMMA
               WHEN CDD-JDN-IS-FALSE(WS-DIDX)
                   MOVE "false" TO WS-STR-LIT(1:5)
                   MOVE 5 TO WS-SLEN
                   PERFORM APPEND-RAW
                   PERFORM EMIT-DOM-SIB-COMMA
               WHEN OTHER
                   MOVE "null" TO WS-STR-LIT(1:4)
                   MOVE 4 TO WS-SLEN
                   PERFORM APPEND-RAW
                   PERFORM EMIT-DOM-SIB-COMMA
           END-EVALUATE.
      * @paragraph CLOSE-DOM-CONTAINER Close '}'/']' and comma if sib.
       CLOSE-DOM-CONTAINER.
           IF CDD-JDN-FIRST-CHILD(WS-PIDX) > 0
               SUBTRACT 1 FROM WS-INDENT
               END-SUBTRACT
               PERFORM APPEND-NL
               PERFORM APPEND-INDENT
           END-IF
           IF CDD-JDN-IS-OBJECT(WS-PIDX)
               MOVE "}" TO WS-STR-LIT(1:1)
           ELSE
               MOVE "]" TO WS-STR-LIT(1:1)
           END-IF
           MOVE 1 TO WS-SLEN
           PERFORM APPEND-RAW
           SUBTRACT 1 FROM WS-DEPTH
           END-SUBTRACT
           IF WS-DEPTH > 0
               MOVE WS-PIDX TO WS-DIDX
               PERFORM EMIT-DOM-SIB-COMMA
           END-IF.
      * @paragraph EMIT-DOM-SIB-COMMA Emit ',' + NL if sibling follows.
       EMIT-DOM-SIB-COMMA.
           IF WS-DEPTH > 0 AND CDD-JDN-NEXT-SIB(WS-DIDX) > 0
               MOVE "," TO WS-STR-LIT(1:1)
               MOVE 1 TO WS-SLEN
               PERFORM APPEND-RAW
               PERFORM APPEND-NL
           END-IF.
