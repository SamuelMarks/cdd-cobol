      * @file CDDJSCH.cbl
      * @brief JSON Schema Draft 2020-12 & OpenAPI 3.x analyzer.
      * @module CDDJSCH
      * @details Auto-detects JSON Schema vs sample JSON payloads and
      *          analyzes JSON Schema DOM trees into Canonical IR
      *          (supporting primitives, nullable unions, string/number
      *          bounds, multipleOf scale, formats, enums, nested
      *          objects, arrays of scalars/objects, and local $ref
      *          resolution with cycle detection).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDJSCH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
      * @record WS-WORK-QUEUE Non-recursive object expansion queue.
       01  WS-WORK-QUEUE.
      * @field WS-WQ-HEAD Queue read index (1..256).
           05  WS-WQ-HEAD                PIC 9(4) COMP VALUE 1.
      * @field WS-WQ-TAIL Queue write index (0..256).
           05  WS-WQ-TAIL                PIC 9(4) COMP VALUE 0.
      * @field WS-WQ-ITEM Queued (DOM schema object, IR parent) pair.
           05  WS-WQ-ITEM                OCCURS 512 TIMES.
      * @field WS-WQ-DOM DOM node index of schema object.
               10  WS-WQ-DOM             PIC 9(4) COMP.
      * @field WS-WQ-PARENT Parent IR field index (0=root).
               10  WS-WQ-PARENT          PIC 9(4) COMP.
      * @record WS-REF-TRAIL Active $ref chain for cycle detection.
       01  WS-REF-TRAIL.
      * @field WS-REF-CNT Active $ref depth (0..16).
           05  WS-REF-CNT                PIC 9(2) COMP VALUE 0.
      * @field WS-REF-NODE Visited DOM node indices in $ref chain.
           05  WS-REF-NODE               PIC 9(4) COMP OCCURS 16 TIMES.
      * @record WS-SCH-STATE Working indices and parsed attribute vals.
       01  WS-SCH-STATE.
      * @field WS-CUR-DOM Current schema object DOM node index.
           05  WS-CUR-DOM                PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-PARENT Current IR parent field index.
           05  WS-CUR-PARENT             PIC 9(4) COMP VALUE 0.
      * @field WS-PROPS-DOM DOM node of 'properties' object.
           05  WS-PROPS-DOM              PIC 9(4) COMP VALUE 0.
      * @field WS-REQ-DOM DOM node of 'required' array.
           05  WS-REQ-DOM                PIC 9(4) COMP VALUE 0.
      * @field WS-PROP-ITER Loop cursor over children of 'properties'.
           05  WS-PROP-ITER              PIC 9(4) COMP VALUE 0.
      * @field WS-EFF-DOM Resolved schema DOM node after $ref hops.
           05  WS-EFF-DOM                PIC 9(4) COMP VALUE 0.
      * @field WS-ITEMS-DOM Resolved 'items' schema DOM node.
           05  WS-ITEMS-DOM              PIC 9(4) COMP VALUE 0.
      * @field WS-IR-IDX Newly allocated IR field index.
           05  WS-IR-IDX                 PIC 9(4) COMP VALUE 0.
      * @field WS-PREV-SIB Previous last-child IR index.
           05  WS-PREV-SIB               PIC 9(4) COMP VALUE 0.
      * @field WS-CHILD-ITER Generic child DOM scan index.
           05  WS-CHILD-ITER             PIC 9(4) COMP VALUE 0.
      * @field WS-SUB-ITER Nested child DOM scan index.
           05  WS-SUB-ITER               PIC 9(4) COMP VALUE 0.
      * @field WS-REF-TARGET-DOM Resolved target DOM node of $ref.
           05  WS-REF-TARGET-DOM         PIC 9(4) COMP VALUE 0.
      * @field WS-REF-IDX Loop index over WS-REF-NODE.
           05  WS-REF-IDX                PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-REF 1 if current node has a '$ref' string.
           05  WS-HAS-REF                PIC 9(2) COMP VALUE 0.
      * @field WS-REF-NAME Extracted target definition name.
           05  WS-REF-NAME               PIC X(128) VALUE SPACES.
      * @field WS-REF-NLEN Length of WS-REF-NAME.
           05  WS-REF-NLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-DEFS-DOM DOM node of '$defs' or 'definitions'.
           05  WS-DEFS-DOM               PIC 9(4) COMP VALUE 0.
      * @field WS-TYPE-STR Primary JSON Schema type name.
           05  WS-TYPE-STR               PIC X(32) VALUE SPACES.
      * @field WS-HAS-MINL 1 if minLength present.
           05  WS-HAS-MINL               PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-MAXL 1 if maxLength present.
           05  WS-HAS-MAXL               PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-MINV 1 if minimum/exclusiveMinimum present.
           05  WS-HAS-MINV               PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-MAXV 1 if maximum/exclusiveMaximum present.
           05  WS-HAS-MAXV               PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-MINI 1 if minItems present.
           05  WS-HAS-MINI               PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-MAXI 1 if maxItems present.
           05  WS-HAS-MAXI               PIC 9(2) COMP VALUE 0.
      * @field WS-MIN-S64 Parsed signed minimum value.
           05  WS-MIN-S64                PIC S9(12) COMP VALUE 0.
      * @field WS-MAX-S64 Parsed signed maximum value.
           05  WS-MAX-S64                PIC S9(12) COMP VALUE 0.
      * @field WS-PARSED-S64 Scratch signed integer result.
           05  WS-PARSED-S64             PIC S9(12) COMP VALUE 0.
      * @field WS-NUM-DOM DOM node whose integer/decimal is parsed.
           05  WS-NUM-DOM                PIC 9(4) COMP VALUE 0.
      * @field WS-SCAN-I Character loop index.
           05  WS-SCAN-I                 PIC 9(4) COMP VALUE 0.
      * @field WS-NEG-FLAG 1 if number starts with '-'.
           05  WS-NEG-FLAG               PIC 9(2) COMP VALUE 0.
      * @field WS-SCALE-CNT Fractional digits counted after '.'.
           05  WS-SCALE-CNT              PIC 9(4) COMP VALUE 0.
      * @field WS-SEEN-DOT 1 after '.' in multipleOf string.
           05  WS-SEEN-DOT               PIC 9(2) COMP VALUE 0.
      * @field WS-ENUM-DOM DOM node of 'enum' array.
           05  WS-ENUM-DOM               PIC 9(4) COMP VALUE 0.
      * @field WS-ENUM-ITER Child iterator over 'enum' array.
           05  WS-ENUM-ITER              PIC 9(4) COMP VALUE 0.
      * @field WS-EIDX Newly allocated CDD-IR-ENUM index.
           05  WS-EIDX                   PIC 9(4) COMP VALUE 0.
       LINKAGE SECTION.
       COPY "cdd-jsch.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDJSCH Detect or analyze JSON Schema DOM into IR.
      * @param [inout] CDD-JSCH-REQ Mode selector and detection output.
      * @param [in] CDD-JDOM Parsed JSON DOM tree.
      * @param [inout] CDD-IR Output Canonical IR container.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return CDD-JSQ-IS-SCHEMA or populated CDD-IR.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-SCHEMA-INVALID-ROOT,
      *         CDD-ERR-SCHEMA-BAD-TYPE, CDD-ERR-SCHEMA-UNRESOLVED-REF,
      *         CDD-ERR-SCHEMA-REF-CYCLE,
      *         CDD-ERR-SCHEMA-CONFLICT-BOUNDS, CDD-ERR-IR-CAPACITY.
       PROCEDURE DIVISION USING CDD-JSCH-REQ CDD-JDOM CDD-IR
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags and dispatch detect/analyze.
       MAIN-ENTRY.
           IF NOT CDD-JSQ-TAG-VALID
               OR NOT CDD-JDM-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDJSCH"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           IF CDD-JSQ-OP-DETECT
               PERFORM DO-AUTO-DETECT
           ELSE
               PERFORM DO-ANALYZE-SCHEMA
           END-IF
           EXIT PROGRAM.
      * @paragraph DO-AUTO-DETECT Inspect root keys for JSON Schema.
       DO-AUTO-DETECT.
           MOVE 0 TO CDD-JSQ-IS-SCHEMA
           IF CDD-JDM-ROOT > 0
               AND CDD-JDN-IS-OBJECT(CDD-JDM-ROOT)
               MOVE CDD-JDN-FIRST-CHILD(CDD-JDM-ROOT)
                   TO WS-CHILD-ITER
               PERFORM UNTIL WS-CHILD-ITER = 0
                   IF CDD-JDN-KEY(WS-CHILD-ITER) = "$schema"
                       OR CDD-JDN-KEY(WS-CHILD-ITER) = "properties"
                       OR CDD-JDN-KEY(WS-CHILD-ITER) = "$defs"
                       OR CDD-JDN-KEY(WS-CHILD-ITER) = "definitions"
                       OR CDD-JDN-KEY(WS-CHILD-ITER) = "$ref"
                       MOVE 1 TO CDD-JSQ-IS-SCHEMA
                   END-IF
                   MOVE CDD-JDN-NEXT-SIB(WS-CHILD-ITER)
                       TO WS-CHILD-ITER
               END-PERFORM
           END-IF.
      * @paragraph DO-ANALYZE-SCHEMA Convert root schema DOM into IR.
       DO-ANALYZE-SCHEMA.
           IF CDD-JDM-ROOT = 0
               OR NOT CDD-JDN-IS-OBJECT(CDD-JDM-ROOT)
               MOVE 1101 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "JSON Schema root must be a JSON object"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM RESET-IR-CONTAINER
               MOVE CDD-JDM-ROOT TO WS-EFF-DOM
               PERFORM EXTRACT-ROOT-META
               PERFORM RESOLVE-REF-CHAIN
               IF CDD-OK
                   PERFORM EXTRACT-ROOT-META
                   MOVE 1 TO WS-WQ-HEAD
                   MOVE 1 TO WS-WQ-TAIL
                   MOVE WS-EFF-DOM TO WS-WQ-DOM(1)
                   MOVE 0 TO WS-WQ-PARENT(1)
                   PERFORM PROCESS-WORK-QUEUE
               END-IF
           END-IF.
      * @paragraph RESET-IR-CONTAINER Clear CDD-IR fields and enums.
       RESET-IR-CONTAINER.
           MOVE 1 TO CDD-IR-UDT-KIND
           MOVE 0 TO CDD-IR-COB-NLEN
           MOVE SPACES TO CDD-IR-COB-NAME
           MOVE 0 TO CDD-IR-TITLE-LEN
           MOVE SPACES TO CDD-IR-JSON-TITLE
           MOVE 0 TO CDD-IR-DESC-LEN
           MOVE SPACES TO CDD-IR-DESC
           MOVE 0 TO CDD-IR-ROOT-FIRST
           MOVE 0 TO CDD-IR-ROOT-LAST
           MOVE 0 TO CDD-IR-FLD-COUNT
           MOVE 0 TO CDD-IR-ENUM-CNT.
      * @paragraph EXTRACT-ROOT-META Capture root title & description.
       EXTRACT-ROOT-META.
           MOVE CDD-JDN-FIRST-CHILD(WS-EFF-DOM) TO WS-CHILD-ITER
           PERFORM UNTIL WS-CHILD-ITER = 0
               IF CDD-JDN-KEY(WS-CHILD-ITER) = "title"
                   AND CDD-JDN-IS-STRING(WS-CHILD-ITER)
                   AND CDD-IR-TITLE-LEN = 0
                   IF CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 128
                       MOVE 128 TO CDD-IR-TITLE-LEN
                   ELSE
                       MOVE CDD-JDN-VAL-LEN(WS-CHILD-ITER)
                           TO CDD-IR-TITLE-LEN
                   END-IF
                   IF CDD-IR-TITLE-LEN > 0
                       MOVE CDD-JDN-VAL(WS-CHILD-ITER)(
                           1:CDD-IR-TITLE-LEN)
                           TO CDD-IR-JSON-TITLE(1:CDD-IR-TITLE-LEN)
                   END-IF
               END-IF
               IF CDD-JDN-KEY(WS-CHILD-ITER) = "description"
                   AND CDD-JDN-IS-STRING(WS-CHILD-ITER)
                   AND CDD-IR-DESC-LEN = 0
                   MOVE CDD-JDN-VAL-LEN(WS-CHILD-ITER)
                       TO CDD-IR-DESC-LEN
                   IF CDD-IR-DESC-LEN > 0
                       MOVE CDD-JDN-VAL(WS-CHILD-ITER)(
                           1:CDD-IR-DESC-LEN)
                           TO CDD-IR-DESC(1:CDD-IR-DESC-LEN)
                   END-IF
               END-IF
               MOVE CDD-JDN-NEXT-SIB(WS-CHILD-ITER) TO WS-CHILD-ITER
           END-PERFORM.
      * @paragraph PROCESS-WORK-QUEUE Expand queued object schemas.
       PROCESS-WORK-QUEUE.
           PERFORM UNTIL WS-WQ-HEAD > WS-WQ-TAIL OR NOT CDD-OK
               MOVE WS-WQ-DOM(WS-WQ-HEAD) TO WS-CUR-DOM
               MOVE WS-WQ-PARENT(WS-WQ-HEAD) TO WS-CUR-PARENT
               ADD 1 TO WS-WQ-HEAD
               END-ADD
               PERFORM EXPAND-OBJECT-PROPERTIES
           END-PERFORM.
      * @paragraph EXPAND-OBJECT-PROPERTIES Read properties & required.
       EXPAND-OBJECT-PROPERTIES.
           MOVE 0 TO WS-PROPS-DOM
           MOVE 0 TO WS-REQ-DOM
           MOVE CDD-JDN-FIRST-CHILD(WS-CUR-DOM) TO WS-CHILD-ITER
           PERFORM UNTIL WS-CHILD-ITER = 0
               IF CDD-JDN-KEY(WS-CHILD-ITER) = "properties"
                   AND CDD-JDN-IS-OBJECT(WS-CHILD-ITER)
                   MOVE WS-CHILD-ITER TO WS-PROPS-DOM
               END-IF
               IF CDD-JDN-KEY(WS-CHILD-ITER) = "required"
                   AND CDD-JDN-IS-ARRAY(WS-CHILD-ITER)
                   MOVE WS-CHILD-ITER TO WS-REQ-DOM
               END-IF
               MOVE CDD-JDN-NEXT-SIB(WS-CHILD-ITER) TO WS-CHILD-ITER
           END-PERFORM
           IF WS-PROPS-DOM > 0
               MOVE CDD-JDN-FIRST-CHILD(WS-PROPS-DOM) TO WS-PROP-ITER
               PERFORM UNTIL WS-PROP-ITER = 0 OR NOT CDD-OK
                   PERFORM PROCESS-ONE-PROPERTY
                   MOVE CDD-JDN-NEXT-SIB(WS-PROP-ITER) TO WS-PROP-ITER
               END-PERFORM
           END-IF.
      * @paragraph PROCESS-ONE-PROPERTY Allocate IR field & map schema.
       PROCESS-ONE-PROPERTY.
           IF NOT CDD-JDN-IS-OBJECT(WS-PROP-ITER)
               MOVE 1102 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "Property schema must be an object"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM ALLOC-IR-FIELD
               IF CDD-OK
                   PERFORM CHECK-IF-REQUIRED
                   MOVE WS-PROP-ITER TO WS-EFF-DOM
                   PERFORM RESOLVE-REF-CHAIN
                   IF CDD-OK
                       PERFORM APPLY-SCHEMA-TO-FIELD
                   END-IF
               END-IF
           END-IF.
      * @paragraph ALLOC-IR-FIELD Allocate & link slot in CDD-IR-FIELD.
       ALLOC-IR-FIELD.
           IF CDD-IR-FLD-COUNT >= CDD-IR-FLD-CAP
               MOVE 3001 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "IR field capacity exceeded" TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-IR-FLD-COUNT
               END-ADD
               MOVE CDD-IR-FLD-COUNT TO WS-IR-IDX
               MOVE WS-CUR-PARENT TO CDD-IRF-PARENT(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-FIRST-CHILD(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-LAST-CHILD(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-NEXT-SIB(WS-IR-IDX)
               MOVE CDD-JDN-KEY-LEN(WS-PROP-ITER)
                   TO CDD-IRF-JKEY-LEN(WS-IR-IDX)
               MOVE CDD-JDN-KEY(WS-PROP-ITER)
                   TO CDD-IRF-JSON-KEY(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-CNAM-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-COB-NAME(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-DESC-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-DESC(WS-IR-IDX)
               MOVE 2 TO CDD-IRF-KIND(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-REQUIRED(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-NULLABLE(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-SIGNED(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-MIN-LEN(WS-IR-IDX)
               MOVE 32 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
               MOVE 9 TO CDD-IRF-DIGITS(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-SCALE(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-HAS-BOUNDS(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-MIN-VAL(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-MAX-VAL(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-FORMAT(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-USAGE(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-PIC-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-PIC-TEXT(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-COB-LEVEL(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-REDEF-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-REDEFINES(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-SYNC-JUST(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-DEF-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-DEF-VAL(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-ARR-MODE(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-ARR-MIN(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-ARR-MAX(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-ENUM-FIRST(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-ENUM-COUNT(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-KEEP-HELPER(WS-IR-IDX)
               IF WS-CUR-PARENT = 0
                   IF CDD-IR-ROOT-FIRST = 0
                       MOVE WS-IR-IDX TO CDD-IR-ROOT-FIRST
                       MOVE WS-IR-IDX TO CDD-IR-ROOT-LAST
                   ELSE
                       MOVE CDD-IR-ROOT-LAST TO WS-PREV-SIB
                       MOVE WS-IR-IDX TO CDD-IRF-NEXT-SIB(WS-PREV-SIB)
                       MOVE WS-IR-IDX TO CDD-IR-ROOT-LAST
                   END-IF
               ELSE
                   IF CDD-IRF-FIRST-CHILD(WS-CUR-PARENT) = 0
                       MOVE WS-IR-IDX
                           TO CDD-IRF-FIRST-CHILD(WS-CUR-PARENT)
                       MOVE WS-IR-IDX
                           TO CDD-IRF-LAST-CHILD(WS-CUR-PARENT)
                   ELSE
                       MOVE CDD-IRF-LAST-CHILD(WS-CUR-PARENT)
                           TO WS-PREV-SIB
                       MOVE WS-IR-IDX
                           TO CDD-IRF-NEXT-SIB(WS-PREV-SIB)
                       MOVE WS-IR-IDX
                           TO CDD-IRF-LAST-CHILD(WS-CUR-PARENT)
                   END-IF
               END-IF
           END-IF.
      * @paragraph CHECK-IF-REQUIRED Mark CDD-IRF-REQUIRED from array.
       CHECK-IF-REQUIRED.
           IF WS-REQ-DOM > 0
               MOVE CDD-JDN-FIRST-CHILD(WS-REQ-DOM) TO WS-SUB-ITER
               PERFORM UNTIL WS-SUB-ITER = 0
                   IF CDD-JDN-IS-STRING(WS-SUB-ITER)
                       AND CDD-JDN-VAL-LEN(WS-SUB-ITER)
                           = CDD-IRF-JKEY-LEN(WS-IR-IDX)
                       AND CDD-JDN-VAL(WS-SUB-ITER)(1:128)
                           = CDD-IRF-JSON-KEY(WS-IR-IDX)
                       MOVE 1 TO CDD-IRF-REQUIRED(WS-IR-IDX)
                   END-IF
                   MOVE CDD-JDN-NEXT-SIB(WS-SUB-ITER) TO WS-SUB-ITER
               END-PERFORM
           END-IF.
      * @paragraph RESOLVE-REF-CHAIN Follow #/$defs/* or #/definitions/*.
       RESOLVE-REF-CHAIN.
           MOVE 1 TO WS-REF-CNT
           MOVE WS-EFF-DOM TO WS-REF-NODE(1)
           MOVE 1 TO WS-HAS-REF
           PERFORM UNTIL WS-HAS-REF = 0 OR NOT CDD-OK
               MOVE 0 TO WS-HAS-REF
               MOVE CDD-JDN-FIRST-CHILD(WS-EFF-DOM) TO WS-CHILD-ITER
               PERFORM UNTIL WS-CHILD-ITER = 0 OR WS-HAS-REF = 1
                   IF CDD-JDN-KEY(WS-CHILD-ITER) = "$ref"
                       AND CDD-JDN-IS-STRING(WS-CHILD-ITER)
                       MOVE 1 TO WS-HAS-REF
                       PERFORM LOOKUP-LOCAL-REF
                   END-IF
                   MOVE CDD-JDN-NEXT-SIB(WS-CHILD-ITER)
                       TO WS-CHILD-ITER
               END-PERFORM
           END-PERFORM.
      * @paragraph LOOKUP-LOCAL-REF Resolve WS-CHILD-ITER $ref string.
       LOOKUP-LOCAL-REF.
           MOVE 0 TO WS-REF-TARGET-DOM
           MOVE SPACES TO WS-REF-NAME
           MOVE 0 TO WS-REF-NLEN
           IF CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 8
               AND CDD-JDN-VAL(WS-CHILD-ITER)(1:8) = "#/$defs/"
               COMPUTE WS-REF-NLEN =
                   CDD-JDN-VAL-LEN(WS-CHILD-ITER) - 8
               END-COMPUTE
               MOVE CDD-JDN-VAL(WS-CHILD-ITER)(9:WS-REF-NLEN)
                   TO WS-REF-NAME(1:WS-REF-NLEN)
               MOVE "$defs" TO WS-TYPE-STR
               PERFORM FIND-IN-DEFS-OBJ
           ELSE
               IF CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 14
                   AND CDD-JDN-VAL(WS-CHILD-ITER)(1:14)
                       = "#/definitions/"
                   COMPUTE WS-REF-NLEN =
                       CDD-JDN-VAL-LEN(WS-CHILD-ITER) - 14
                   END-COMPUTE
                   MOVE CDD-JDN-VAL(WS-CHILD-ITER)(15:WS-REF-NLEN)
                       TO WS-REF-NAME(1:WS-REF-NLEN)
                   MOVE "definitions" TO WS-TYPE-STR
                   PERFORM FIND-IN-DEFS-OBJ
               END-IF
           END-IF
           IF WS-REF-TARGET-DOM = 0
               MOVE 1103 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "Unresolved local $ref target" TO CDD-ERR-MESSAGE
           ELSE
               PERFORM CHECK-REF-CYCLE
           END-IF.
      * @paragraph FIND-IN-DEFS-OBJ Search root $defs or definitions.
       FIND-IN-DEFS-OBJ.
           MOVE 0 TO WS-DEFS-DOM
           MOVE CDD-JDN-FIRST-CHILD(CDD-JDM-ROOT) TO WS-SUB-ITER
           PERFORM UNTIL WS-SUB-ITER = 0
               IF CDD-JDN-KEY(WS-SUB-ITER)(1:32) = WS-TYPE-STR
                   AND CDD-JDN-IS-OBJECT(WS-SUB-ITER)
                   MOVE WS-SUB-ITER TO WS-DEFS-DOM
               END-IF
               MOVE CDD-JDN-NEXT-SIB(WS-SUB-ITER) TO WS-SUB-ITER
           END-PERFORM
           IF WS-DEFS-DOM > 0
               MOVE CDD-JDN-FIRST-CHILD(WS-DEFS-DOM) TO WS-SUB-ITER
               PERFORM UNTIL WS-SUB-ITER = 0
                   IF CDD-JDN-KEY-LEN(WS-SUB-ITER) = WS-REF-NLEN
                       AND CDD-JDN-KEY(WS-SUB-ITER) = WS-REF-NAME
                       AND CDD-JDN-IS-OBJECT(WS-SUB-ITER)
                       MOVE WS-SUB-ITER TO WS-REF-TARGET-DOM
                   END-IF
                   MOVE CDD-JDN-NEXT-SIB(WS-SUB-ITER) TO WS-SUB-ITER
               END-PERFORM
           END-IF.
      * @paragraph CHECK-REF-CYCLE Reject circular $ref chains.
       CHECK-REF-CYCLE.
           MOVE 1 TO WS-REF-IDX
           PERFORM UNTIL WS-REF-IDX > WS-REF-CNT OR NOT CDD-OK
               IF WS-REF-NODE(WS-REF-IDX) = WS-REF-TARGET-DOM
                   MOVE 1104 TO CDD-ERR-CODE
                   MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
                   MOVE "Circular $ref cycle in JSON Schema"
                       TO CDD-ERR-MESSAGE
               END-IF
               ADD 1 TO WS-REF-IDX
               END-ADD
           END-PERFORM
           IF CDD-OK
               IF WS-REF-CNT >= 16
                   MOVE 1104 TO CDD-ERR-CODE
                   MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
                   MOVE "$ref chain depth exceeded"
                       TO CDD-ERR-MESSAGE
               ELSE
                   ADD 1 TO WS-REF-CNT
                   END-ADD
                   MOVE WS-REF-TARGET-DOM TO WS-REF-NODE(WS-REF-CNT)
                   MOVE WS-REF-TARGET-DOM TO WS-EFF-DOM
               END-IF
           END-IF.
      * @paragraph APPLY-SCHEMA-TO-FIELD Map WS-EFF-DOM onto WS-IR-IDX.
       APPLY-SCHEMA-TO-FIELD.
           PERFORM EXTRACT-FIELD-CONSTRAINTS
           IF CDD-OK
               IF WS-TYPE-STR = "array"
                   PERFORM APPLY-ARRAY-SCHEMA
               ELSE
                   PERFORM APPLY-SCALAR-OR-OBJ-TYPE
               END-IF
           END-IF.
      * @paragraph EXTRACT-FIELD-CONSTRAINTS Scan keywords on schema.
       EXTRACT-FIELD-CONSTRAINTS.
           MOVE SPACES TO WS-TYPE-STR
           MOVE 0 TO WS-HAS-MINL
           MOVE 0 TO WS-HAS-MAXL
           MOVE 0 TO WS-HAS-MINV
           MOVE 0 TO WS-HAS-MAXV
           MOVE 0 TO WS-HAS-MINI
           MOVE 0 TO WS-HAS-MAXI
           MOVE 0 TO WS-ITEMS-DOM
           MOVE 0 TO WS-ENUM-DOM
           MOVE CDD-JDN-FIRST-CHILD(WS-EFF-DOM) TO WS-CHILD-ITER
           PERFORM UNTIL WS-CHILD-ITER = 0 OR NOT CDD-OK
               PERFORM INSPECT-SCHEMA-KEYWORD
               MOVE CDD-JDN-NEXT-SIB(WS-CHILD-ITER) TO WS-CHILD-ITER
           END-PERFORM
           PERFORM VALIDATE-CONFLICT-BOUNDS.
      * @paragraph INSPECT-SCHEMA-KEYWORD Handle one keyword child.
       INSPECT-SCHEMA-KEYWORD.
           EVALUATE CDD-JDN-KEY(WS-CHILD-ITER)
               WHEN "type"
                   PERFORM PARSE-TYPE-KEYWORD
               WHEN "description"
                   IF CDD-JDN-IS-STRING(WS-CHILD-ITER)
                       IF CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 160
                           MOVE 160 TO CDD-IRF-DESC-LEN(WS-IR-IDX)
                       ELSE
                           MOVE CDD-JDN-VAL-LEN(WS-CHILD-ITER)
                               TO CDD-IRF-DESC-LEN(WS-IR-IDX)
                       END-IF
                       IF CDD-IRF-DESC-LEN(WS-IR-IDX) > 0
                           MOVE CDD-JDN-VAL(WS-CHILD-ITER)(
                               1:CDD-IRF-DESC-LEN(WS-IR-IDX))
                               TO CDD-IRF-DESC(WS-IR-IDX)(
                               1:CDD-IRF-DESC-LEN(WS-IR-IDX))
                       END-IF
                   END-IF
               WHEN "default"
                   PERFORM COPY-DEFAULT-VAL
               WHEN "format"
                   IF CDD-JDN-IS-STRING(WS-CHILD-ITER)
                       AND CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 0
                       AND CDD-JDN-VAL-LEN(WS-CHILD-ITER) <= 16
                       MOVE CDD-JDN-VAL(WS-CHILD-ITER)(
                           1:CDD-JDN-VAL-LEN(WS-CHILD-ITER))
                           TO CDD-IRF-FORMAT(WS-IR-IDX)
                   END-IF
               WHEN "minLength"
                   MOVE WS-CHILD-ITER TO WS-NUM-DOM
                   PERFORM PARSE-DOM-S64
                   IF WS-PARSED-S64 >= 0
                       MOVE 1 TO WS-HAS-MINL
                       COMPUTE CDD-IRF-MIN-LEN(WS-IR-IDX) =
                           WS-PARSED-S64
                       END-COMPUTE
                   END-IF
               WHEN "maxLength"
                   MOVE WS-CHILD-ITER TO WS-NUM-DOM
                   PERFORM PARSE-DOM-S64
                   IF WS-PARSED-S64 >= 0
                       MOVE 1 TO WS-HAS-MAXL
                       COMPUTE CDD-IRF-MAX-LEN(WS-IR-IDX) =
                           WS-PARSED-S64
                       END-COMPUTE
                   END-IF
               WHEN "minimum"
               WHEN "exclusiveMinimum"
                   MOVE WS-CHILD-ITER TO WS-NUM-DOM
                   PERFORM PARSE-DOM-S64
                   MOVE 1 TO WS-HAS-MINV
                   MOVE 1 TO CDD-IRF-HAS-BOUNDS(WS-IR-IDX)
                   MOVE WS-PARSED-S64 TO WS-MIN-S64
                   MOVE WS-PARSED-S64 TO CDD-IRF-MIN-VAL(WS-IR-IDX)
               WHEN "maximum"
               WHEN "exclusiveMaximum"
                   MOVE WS-CHILD-ITER TO WS-NUM-DOM
                   PERFORM PARSE-DOM-S64
                   MOVE 1 TO WS-HAS-MAXV
                   MOVE 1 TO CDD-IRF-HAS-BOUNDS(WS-IR-IDX)
                   MOVE WS-PARSED-S64 TO WS-MAX-S64
                   MOVE WS-PARSED-S64 TO CDD-IRF-MAX-VAL(WS-IR-IDX)
               WHEN "multipleOf"
                   PERFORM PARSE-MULTIPLE-OF
               WHEN "minItems"
                   MOVE WS-CHILD-ITER TO WS-NUM-DOM
                   PERFORM PARSE-DOM-S64
                   IF WS-PARSED-S64 >= 0
                       MOVE 1 TO WS-HAS-MINI
                       COMPUTE CDD-IRF-ARR-MIN(WS-IR-IDX) =
                           WS-PARSED-S64
                       END-COMPUTE
                   END-IF
               WHEN "maxItems"
                   MOVE WS-CHILD-ITER TO WS-NUM-DOM
                   PERFORM PARSE-DOM-S64
                   IF WS-PARSED-S64 >= 0
                       MOVE 1 TO WS-HAS-MAXI
                       COMPUTE CDD-IRF-ARR-MAX(WS-IR-IDX) =
                           WS-PARSED-S64
                       END-COMPUTE
                   END-IF
               WHEN "items"
                   IF CDD-JDN-IS-OBJECT(WS-CHILD-ITER)
                       MOVE WS-CHILD-ITER TO WS-ITEMS-DOM
                   END-IF
               WHEN "enum"
                   IF CDD-JDN-IS-ARRAY(WS-CHILD-ITER)
                       MOVE WS-CHILD-ITER TO WS-ENUM-DOM
                   END-IF
               WHEN "properties"
                   IF WS-TYPE-STR = SPACES
                       MOVE "object" TO WS-TYPE-STR
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph PARSE-TYPE-KEYWORD Handle string or union array type.
       PARSE-TYPE-KEYWORD.
           IF CDD-JDN-IS-STRING(WS-CHILD-ITER)
               AND CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 0
               AND CDD-JDN-VAL-LEN(WS-CHILD-ITER) <= 32
               MOVE CDD-JDN-VAL(WS-CHILD-ITER)(
                   1:CDD-JDN-VAL-LEN(WS-CHILD-ITER))
                   TO WS-TYPE-STR
           ELSE
               IF CDD-JDN-IS-ARRAY(WS-CHILD-ITER)
                   MOVE CDD-JDN-FIRST-CHILD(WS-CHILD-ITER)
                       TO WS-SUB-ITER
                   PERFORM UNTIL WS-SUB-ITER = 0
                       IF CDD-JDN-IS-STRING(WS-SUB-ITER)
                           IF CDD-JDN-VAL(WS-SUB-ITER)(1:4) = "null"
                               AND CDD-JDN-VAL-LEN(WS-SUB-ITER) = 4
                               MOVE 1 TO CDD-IRF-NULLABLE(WS-IR-IDX)
                           ELSE
                               IF CDD-JDN-VAL-LEN(WS-SUB-ITER) <= 32
                                   MOVE CDD-JDN-VAL(WS-SUB-ITER)(
                                       1:CDD-JDN-VAL-LEN(WS-SUB-ITER))
                                       TO WS-TYPE-STR
                               END-IF
                           END-IF
                       END-IF
                       MOVE CDD-JDN-NEXT-SIB(WS-SUB-ITER)
                           TO WS-SUB-ITER
                   END-PERFORM
               ELSE
                   MOVE 1102 TO CDD-ERR-CODE
                   MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
                   MOVE "Invalid JSON Schema type value"
                       TO CDD-ERR-MESSAGE
               END-IF
           END-IF.
      * @paragraph COPY-DEFAULT-VAL Copy default scalar text into IR.
       COPY-DEFAULT-VAL.
           IF CDD-JDN-IS-TRUE(WS-CHILD-ITER)
               MOVE "true" TO CDD-IRF-DEF-VAL(WS-IR-IDX)
               MOVE 4 TO CDD-IRF-DEF-LEN(WS-IR-IDX)
           ELSE
               IF CDD-JDN-IS-FALSE(WS-CHILD-ITER)
                   MOVE "false" TO CDD-IRF-DEF-VAL(WS-IR-IDX)
                   MOVE 5 TO CDD-IRF-DEF-LEN(WS-IR-IDX)
               ELSE
                   IF CDD-JDN-VAL-LEN(WS-CHILD-ITER) > 0
                       AND CDD-JDN-VAL-LEN(WS-CHILD-ITER) <= 64
                       MOVE CDD-JDN-VAL-LEN(WS-CHILD-ITER)
                           TO CDD-IRF-DEF-LEN(WS-IR-IDX)
                       MOVE CDD-JDN-VAL(WS-CHILD-ITER)(
                           1:CDD-IRF-DEF-LEN(WS-IR-IDX))
                           TO CDD-IRF-DEF-VAL(WS-IR-IDX)(
                           1:CDD-IRF-DEF-LEN(WS-IR-IDX))
                   END-IF
               END-IF
           END-IF.
      * @paragraph PARSE-DOM-S64 Parse integer portion of WS-NUM-DOM.
       PARSE-DOM-S64.
           MOVE 0 TO WS-PARSED-S64
           MOVE 0 TO WS-NEG-FLAG
           MOVE 1 TO WS-SCAN-I
           IF CDD-JDN-VAL-LEN(WS-NUM-DOM) > 0
               AND CDD-JDN-VAL(WS-NUM-DOM)(1:1) = "-"
               MOVE 1 TO WS-NEG-FLAG
               MOVE 2 TO WS-SCAN-I
           END-IF
           PERFORM UNTIL WS-SCAN-I > CDD-JDN-VAL-LEN(WS-NUM-DOM)
               OR WS-SCAN-I > 11
               IF CDD-JDN-VAL(WS-NUM-DOM)(WS-SCAN-I:1) >= "0"
                   AND CDD-JDN-VAL(WS-NUM-DOM)(WS-SCAN-I:1) <= "9"
                   MOVE "CDDSRQ01" TO CDD-SRQ-TAG
                   MOVE 6 TO CDD-SRQ-OP
                   MOVE 1 TO CDD-SRQ-IN-LEN
                   MOVE CDD-JDN-VAL(WS-NUM-DOM)(WS-SCAN-I:1)
                       TO CDD-SRQ-IN-TEXT(1:1)
                   CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
                   END-CALL
                   COMPUTE WS-PARSED-S64 =
                       (WS-PARSED-S64 * 10) + CDD-SRQ-NUM-VAL
                   END-COMPUTE
               ELSE
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-SCAN-I
               END-ADD
           END-PERFORM
           IF WS-NEG-FLAG = 1
               COMPUTE WS-PARSED-S64 = 0 - WS-PARSED-S64
               END-COMPUTE
           END-IF.
      * @paragraph PARSE-MULTIPLE-OF Count fractional scale digits.
       PARSE-MULTIPLE-OF.
           MOVE 0 TO WS-SCALE-CNT
           MOVE 0 TO WS-SEEN-DOT
           MOVE 1 TO WS-SCAN-I
           PERFORM UNTIL WS-SCAN-I > CDD-JDN-VAL-LEN(WS-CHILD-ITER)
               IF CDD-JDN-VAL(WS-CHILD-ITER)(WS-SCAN-I:1) = "."
                   MOVE 1 TO WS-SEEN-DOT
               ELSE
                   IF WS-SEEN-DOT = 1
                       AND CDD-JDN-VAL(WS-CHILD-ITER)(WS-SCAN-I:1)
                           >= "0"
                       AND CDD-JDN-VAL(WS-CHILD-ITER)(WS-SCAN-I:1)
                           <= "9"
                       ADD 1 TO WS-SCALE-CNT
                       END-ADD
                   END-IF
               END-IF
               ADD 1 TO WS-SCAN-I
               END-ADD
           END-PERFORM
           IF WS-SCALE-CNT > 0
               MOVE WS-SCALE-CNT TO CDD-IRF-SCALE(WS-IR-IDX)
           END-IF.
      * @paragraph VALIDATE-CONFLICT-BOUNDS Reject min > max bounds.
       VALIDATE-CONFLICT-BOUNDS.
           IF WS-HAS-MINL = 1 AND WS-HAS-MAXL = 1
               AND CDD-IRF-MIN-LEN(WS-IR-IDX)
                   > CDD-IRF-MAX-LEN(WS-IR-IDX)
               MOVE 1105 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "minLength exceeds maxLength" TO CDD-ERR-MESSAGE
           END-IF
           IF WS-HAS-MINV = 1 AND WS-HAS-MAXV = 1
               AND WS-MIN-S64 > WS-MAX-S64
               MOVE 1105 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "minimum exceeds maximum" TO CDD-ERR-MESSAGE
           END-IF
           IF WS-HAS-MINI = 1 AND WS-HAS-MAXI = 1
               AND CDD-IRF-ARR-MIN(WS-IR-IDX)
                   > CDD-IRF-ARR-MAX(WS-IR-IDX)
               MOVE 1105 TO CDD-ERR-CODE
               MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
               MOVE "minItems exceeds maxItems" TO CDD-ERR-MESSAGE
           END-IF.
      * @paragraph APPLY-ARRAY-SCHEMA Configure OCCURS & items type.
       APPLY-ARRAY-SCHEMA.
           IF WS-HAS-MAXI = 0
               MOVE 3 TO CDD-IRF-ARR-MODE(WS-IR-IDX)
               MOVE 10 TO CDD-IRF-ARR-MAX(WS-IR-IDX)
           ELSE
               IF WS-HAS-MINI = 1
                   AND CDD-IRF-ARR-MIN(WS-IR-IDX)
                       = CDD-IRF-ARR-MAX(WS-IR-IDX)
                   AND CDD-IRF-ARR-MAX(WS-IR-IDX) > 0
                   MOVE 1 TO CDD-IRF-ARR-MODE(WS-IR-IDX)
               ELSE
                   MOVE 2 TO CDD-IRF-ARR-MODE(WS-IR-IDX)
               END-IF
           END-IF
           IF WS-ITEMS-DOM > 0
               MOVE WS-ITEMS-DOM TO WS-EFF-DOM
               PERFORM RESOLVE-REF-CHAIN
               IF CDD-OK
                   PERFORM EXTRACT-ITEMS-KEYWORDS
                   IF CDD-OK
                       PERFORM APPLY-SCALAR-OR-OBJ-TYPE
                   END-IF
               END-IF
           END-IF.
      * @paragraph EXTRACT-ITEMS-KEYWORDS Read item type/bounds without wiping array mode.
       EXTRACT-ITEMS-KEYWORDS.
           MOVE SPACES TO WS-TYPE-STR
           MOVE 0 TO WS-HAS-MINL
           MOVE 0 TO WS-HAS-MAXL
           MOVE 0 TO WS-HAS-MINV
           MOVE 0 TO WS-HAS-MAXV
           MOVE 0 TO WS-ENUM-DOM
           MOVE CDD-JDN-FIRST-CHILD(WS-EFF-DOM) TO WS-CHILD-ITER
           PERFORM UNTIL WS-CHILD-ITER = 0 OR NOT CDD-OK
               IF CDD-JDN-KEY(WS-CHILD-ITER) NOT = "minItems"
                   AND CDD-JDN-KEY(WS-CHILD-ITER) NOT = "maxItems"
                   AND CDD-JDN-KEY(WS-CHILD-ITER) NOT = "items"
                   PERFORM INSPECT-SCHEMA-KEYWORD
               END-IF
               MOVE CDD-JDN-NEXT-SIB(WS-CHILD-ITER) TO WS-CHILD-ITER
           END-PERFORM
           PERFORM VALIDATE-CONFLICT-BOUNDS.
      * @paragraph APPLY-SCALAR-OR-OBJ-TYPE Finalize IR field type.
       APPLY-SCALAR-OR-OBJ-TYPE.
           IF WS-TYPE-STR = SPACES AND WS-ENUM-DOM > 0
               PERFORM INFER-TYPE-FROM-ENUM
           END-IF
           EVALUATE WS-TYPE-STR
               WHEN "object"
                   MOVE 1 TO CDD-IRF-KIND(WS-IR-IDX)
                   ADD 1 TO WS-WQ-TAIL
                   END-ADD
                   MOVE WS-EFF-DOM TO WS-WQ-DOM(WS-WQ-TAIL)
                   MOVE WS-IR-IDX TO WS-WQ-PARENT(WS-WQ-TAIL)
               WHEN "string"
                   PERFORM CONFIGURE-STRING-FIELD
               WHEN "integer"
                   PERFORM CONFIGURE-INTEGER-FIELD
               WHEN "number"
                   PERFORM CONFIGURE-NUMBER-FIELD
               WHEN "boolean"
                   MOVE 9 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 1 TO CDD-IRF-DIGITS(WS-IR-IDX)
               WHEN "null"
                   MOVE 2 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 1 TO CDD-IRF-NULLABLE(WS-IR-IDX)
               WHEN OTHER
                   MOVE 1102 TO CDD-ERR-CODE
                   MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
                   MOVE "Unsupported or missing JSON Schema type"
                       TO CDD-ERR-MESSAGE
           END-EVALUATE
           IF CDD-OK AND WS-ENUM-DOM > 0
               PERFORM RECORD-ENUM-VALUES
           END-IF.
      * @paragraph INFER-TYPE-FROM-ENUM Pick string vs integer enum.
       INFER-TYPE-FROM-ENUM.
           MOVE CDD-JDN-FIRST-CHILD(WS-ENUM-DOM) TO WS-SUB-ITER
           IF WS-SUB-ITER > 0 AND CDD-JDN-IS-INTEGER(WS-SUB-ITER)
               MOVE "integer" TO WS-TYPE-STR
           ELSE
               MOVE "string" TO WS-TYPE-STR
           END-IF.
      * @paragraph CONFIGURE-STRING-FIELD Apply string format lengths.
       CONFIGURE-STRING-FIELD.
           MOVE 2 TO CDD-IRF-KIND(WS-IR-IDX)
           IF WS-HAS-MAXL = 0
               EVALUATE CDD-IRF-FORMAT(WS-IR-IDX)
                   WHEN "date"
                       MOVE 10 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
                   WHEN "time"
                       MOVE 12 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
                   WHEN "date-time"
                       MOVE 30 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
                   WHEN "uuid"
                       MOVE 36 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
                   WHEN OTHER
                       MOVE 32 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
               END-EVALUATE
           END-IF.
      * @paragraph CONFIGURE-INTEGER-FIELD Apply int32/int64 & sign.
       CONFIGURE-INTEGER-FIELD.
           MOVE 5 TO CDD-IRF-KIND(WS-IR-IDX)
           MOVE 1 TO CDD-IRF-SIGNED(WS-IR-IDX)
           IF WS-HAS-MINV = 1 AND WS-MIN-S64 >= 0
               MOVE 0 TO CDD-IRF-SIGNED(WS-IR-IDX)
           END-IF
           EVALUATE CDD-IRF-FORMAT(WS-IR-IDX)
               WHEN "int32"
                   MOVE 9 TO CDD-IRF-DIGITS(WS-IR-IDX)
                   MOVE 5 TO CDD-IRF-USAGE(WS-IR-IDX)
               WHEN "int64"
                   MOVE 18 TO CDD-IRF-DIGITS(WS-IR-IDX)
                   MOVE 6 TO CDD-IRF-USAGE(WS-IR-IDX)
               WHEN OTHER
                   MOVE 9 TO CDD-IRF-DIGITS(WS-IR-IDX)
           END-EVALUATE.
      * @paragraph CONFIGURE-NUMBER-FIELD Apply float/double/decimal.
       CONFIGURE-NUMBER-FIELD.
           MOVE 1 TO CDD-IRF-SIGNED(WS-IR-IDX)
           IF WS-HAS-MINV = 1 AND WS-MIN-S64 >= 0
               MOVE 0 TO CDD-IRF-SIGNED(WS-IR-IDX)
           END-IF
           EVALUATE CDD-IRF-FORMAT(WS-IR-IDX)
               WHEN "float"
                   MOVE 7 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 3 TO CDD-IRF-USAGE(WS-IR-IDX)
               WHEN "double"
                   MOVE 7 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 4 TO CDD-IRF-USAGE(WS-IR-IDX)
               WHEN "decimal"
                   MOVE 8 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 7 TO CDD-IRF-USAGE(WS-IR-IDX)
               WHEN OTHER
                   MOVE 6 TO CDD-IRF-KIND(WS-IR-IDX)
                   IF CDD-IRF-SCALE(WS-IR-IDX) = 0
                       MOVE 2 TO CDD-IRF-SCALE(WS-IR-IDX)
                   END-IF
                   COMPUTE CDD-IRF-DIGITS(WS-IR-IDX) =
                       9 + CDD-IRF-SCALE(WS-IR-IDX)
                   END-COMPUTE
           END-EVALUATE.
      * @paragraph RECORD-ENUM-VALUES Store enum literals in CDD-IR.
       RECORD-ENUM-VALUES.
           MOVE CDD-JDN-FIRST-CHILD(WS-ENUM-DOM) TO WS-ENUM-ITER
           PERFORM UNTIL WS-ENUM-ITER = 0 OR NOT CDD-OK
               IF CDD-IR-ENUM-CNT >= CDD-IR-ENUM-CAP
                   MOVE 3001 TO CDD-ERR-CODE
                   MOVE "CDDJSCH" TO CDD-ERR-SUBPROG
                   MOVE "IR enum capacity exceeded"
                       TO CDD-ERR-MESSAGE
               ELSE
                   ADD 1 TO CDD-IR-ENUM-CNT
                   END-ADD
                   MOVE CDD-IR-ENUM-CNT TO WS-EIDX
                   IF CDD-IRF-ENUM-FIRST(WS-IR-IDX) = 0
                       MOVE WS-EIDX TO CDD-IRF-ENUM-FIRST(WS-IR-IDX)
                   END-IF
                   ADD 1 TO CDD-IRF-ENUM-COUNT(WS-IR-IDX)
                   END-ADD
                   MOVE 0 TO CDD-IRE-CNAM-LEN(WS-EIDX)
                   MOVE SPACES TO CDD-IRE-COB-NAME(WS-EIDX)
                   IF CDD-JDN-IS-STRING(WS-ENUM-ITER)
                       MOVE 1 TO CDD-IRE-IS-STR(WS-EIDX)
                       IF WS-HAS-MAXL = 0
                           AND CDD-JDN-VAL-LEN(WS-ENUM-ITER)
                               > CDD-IRF-MAX-LEN(WS-IR-IDX)
                           MOVE CDD-JDN-VAL-LEN(WS-ENUM-ITER)
                               TO CDD-IRF-MAX-LEN(WS-IR-IDX)
                       END-IF
                   ELSE
                       MOVE 0 TO CDD-IRE-IS-STR(WS-EIDX)
                   END-IF
                   IF CDD-JDN-VAL-LEN(WS-ENUM-ITER) > 64
                       MOVE 64 TO CDD-IRE-VAL-LEN(WS-EIDX)
                   ELSE
                       MOVE CDD-JDN-VAL-LEN(WS-ENUM-ITER)
                           TO CDD-IRE-VAL-LEN(WS-EIDX)
                   END-IF
                   MOVE SPACES TO CDD-IRE-VAL(WS-EIDX)
                   IF CDD-IRE-VAL-LEN(WS-EIDX) > 0
                       MOVE CDD-JDN-VAL(WS-ENUM-ITER)(
                           1:CDD-IRE-VAL-LEN(WS-EIDX))
                           TO CDD-IRE-VAL(WS-EIDX)(
                           1:CDD-IRE-VAL-LEN(WS-EIDX))
                   END-IF
               END-IF
               MOVE CDD-JDN-NEXT-SIB(WS-ENUM-ITER) TO WS-ENUM-ITER
           END-PERFORM.
