      * @file CDDJINF.cbl
      * @brief Raw JSON payload to COBOL IR type and precision inferrer.
      * @module CDDJINF
      * @details Infers COBOL-ready field structures, string lengths,
      *          date/UUID formats, signed/unsigned integer digits,
      *          decimal scale, floats, booleans, nullability, nested
      *          groups, and array bounds from sample JSON payloads.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDJINF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-MERGE-QUEUE Non-recursive object observation queue.
       01  WS-MERGE-QUEUE.
      * @field WS-MQ-HEAD Queue read index (1..256).
           05  WS-MQ-HEAD                PIC 9(4) COMP VALUE 1.
      * @field WS-MQ-TAIL Queue write index (0..256).
           05  WS-MQ-TAIL                PIC 9(4) COMP VALUE 0.
      * @field WS-MQ-ITEM Queued (DOM object, IR parent) observation.
           05  WS-MQ-ITEM                OCCURS 1024 TIMES.
      * @field WS-MQ-DOM DOM index of sample JSON object.
               10  WS-MQ-DOM             PIC 9(4) COMP.
      * @field WS-MQ-PARENT Target IR parent field index (0=root).
               10  WS-MQ-PARENT          PIC 9(4) COMP.
      * @record WS-INF-STATE Scratch indices and numeric observations.
       01  WS-INF-STATE.
      * @field WS-CUR-DOM Current sample object DOM index.
           05  WS-CUR-DOM                PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-PARENT Current IR parent index.
           05  WS-CUR-PARENT             PIC 9(4) COMP VALUE 0.
      * @field WS-PROP-DOM Current property DOM child index.
           05  WS-PROP-DOM               PIC 9(4) COMP VALUE 0.
      * @field WS-ELEM-DOM Current array element DOM index.
           05  WS-ELEM-DOM               PIC 9(4) COMP VALUE 0.
      * @field WS-OBS-DOM Active scalar/object observation DOM node.
           05  WS-OBS-DOM                PIC 9(4) COMP VALUE 0.
      * @field WS-IR-IDX Matched or newly allocated IR field index.
           05  WS-IR-IDX                 PIC 9(4) COMP VALUE 0.
      * @field WS-SEARCH-IDX Sibling search cursor in CDD-IR.
           05  WS-SEARCH-IDX             PIC 9(4) COMP VALUE 0.
      * @field WS-PREV-SIB Previous last sibling in CDD-IR.
           05  WS-PREV-SIB               PIC 9(4) COMP VALUE 0.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-OBS-SIGN 1 if negative number observed.
           05  WS-OBS-SIGN               PIC 9(2) COMP VALUE 0.
      * @field WS-OBS-INTD Observed digits before decimal point.
           05  WS-OBS-INTD               PIC 9(4) COMP VALUE 0.
      * @field WS-OBS-FRACD Observed digits after decimal point.
           05  WS-OBS-FRACD              PIC 9(4) COMP VALUE 0.
      * @field WS-OBS-EXP 1 if exponent 'e'/'E' observed.
           05  WS-OBS-EXP                PIC 9(2) COMP VALUE 0.
      * @field WS-SEEN-DOT 1 once '.' passed in number scan.
           05  WS-SEEN-DOT               PIC 9(2) COMP VALUE 0.
      * @field WS-OLD-INTD Previous integer digit width on field.
           05  WS-OLD-INTD               PIC 9(4) COMP VALUE 0.
      * @field WS-IS-DATE 1 if string matches YYYY-MM-DD.
           05  WS-IS-DATE                PIC 9(2) COMP VALUE 0.
      * @field WS-IS-UUID 1 if string matches 8-4-4-4-12 hex UUID.
           05  WS-IS-UUID                PIC 9(2) COMP VALUE 0.
      * @field WS-CH Character scratch.
           05  WS-CH                     PIC X(1) VALUE SPACE.
       LINKAGE SECTION.
       COPY "cdd-jdom.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDJINF Infer Canonical IR from sample JSON DOM.
      * @param [in] CDD-JDOM Parsed sample JSON DOM tree.
      * @param [inout] CDD-IR Output Canonical IR container.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-IR with inferred types and bounds.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH,
      *         CDD-ERR-INFER-EMPTY-CONFLICT, CDD-ERR-IR-CAPACITY.
       PROCEDURE DIVISION USING CDD-JDOM CDD-IR CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, seed queue, infer IR.
       MAIN-ENTRY.
           IF NOT CDD-JDM-TAG-VALID OR NOT CDD-IR-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDJINF" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-JDOM or CDD-IR tag"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           PERFORM INIT-INFER-IR
           PERFORM SEED-ROOT-SAMPLES
           IF CDD-OK
               PERFORM DRAIN-MERGE-QUEUE
           END-IF
           IF CDD-OK
               PERFORM FINALIZE-NULL-FIELDS
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-INFER-IR Clear CDD-IR and queue pointers.
       INIT-INFER-IR.
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
           MOVE 0 TO CDD-IR-ENUM-CNT
           MOVE 1 TO WS-MQ-HEAD
           MOVE 0 TO WS-MQ-TAIL.
      * @paragraph SEED-ROOT-SAMPLES Enqueue root object or obj array.
       SEED-ROOT-SAMPLES.
           IF CDD-JDM-ROOT = 0
               PERFORM FAIL-INFER-CONFLICT
           ELSE
               IF CDD-JDN-IS-OBJECT(CDD-JDM-ROOT)
                   MOVE 1 TO WS-MQ-TAIL
                   MOVE CDD-JDM-ROOT TO WS-MQ-DOM(1)
                   MOVE 0 TO WS-MQ-PARENT(1)
               ELSE
                   IF CDD-JDN-IS-ARRAY(CDD-JDM-ROOT)
                       AND CDD-JDN-CHILD-CNT(CDD-JDM-ROOT) > 0
                       MOVE CDD-JDN-FIRST-CHILD(CDD-JDM-ROOT)
                           TO WS-ELEM-DOM
                       PERFORM UNTIL WS-ELEM-DOM = 0 OR NOT CDD-OK
                           IF NOT CDD-JDN-IS-OBJECT(WS-ELEM-DOM)
                               PERFORM FAIL-INFER-CONFLICT
                           ELSE
                               ADD 1 TO WS-MQ-TAIL
                               END-ADD
                               MOVE WS-ELEM-DOM
                                   TO WS-MQ-DOM(WS-MQ-TAIL)
                               MOVE 0
                                   TO WS-MQ-PARENT(WS-MQ-TAIL)
                           END-IF
                           MOVE CDD-JDN-NEXT-SIB(WS-ELEM-DOM)
                               TO WS-ELEM-DOM
                       END-PERFORM
                   ELSE
                       PERFORM FAIL-INFER-CONFLICT
                   END-IF
               END-IF
           END-IF.
      * @paragraph DRAIN-MERGE-QUEUE Process all queued sample objects.
       DRAIN-MERGE-QUEUE.
           PERFORM UNTIL WS-MQ-HEAD > WS-MQ-TAIL OR NOT CDD-OK
               MOVE WS-MQ-DOM(WS-MQ-HEAD) TO WS-CUR-DOM
               MOVE WS-MQ-PARENT(WS-MQ-HEAD) TO WS-CUR-PARENT
               ADD 1 TO WS-MQ-HEAD
               END-ADD
               MOVE CDD-JDN-FIRST-CHILD(WS-CUR-DOM) TO WS-PROP-DOM
               PERFORM UNTIL WS-PROP-DOM = 0 OR NOT CDD-OK
                   PERFORM FIND-OR-ALLOC-FIELD
                   IF CDD-OK
                       PERFORM MERGE-PROP-OBSERVATION
                   END-IF
                   MOVE CDD-JDN-NEXT-SIB(WS-PROP-DOM) TO WS-PROP-DOM
               END-PERFORM
           END-PERFORM.
      * @paragraph FIND-OR-ALLOC-FIELD Match existing key or allocate.
       FIND-OR-ALLOC-FIELD.
           MOVE 0 TO WS-IR-IDX
           IF WS-CUR-PARENT = 0
               MOVE CDD-IR-ROOT-FIRST TO WS-SEARCH-IDX
           ELSE
               MOVE CDD-IRF-FIRST-CHILD(WS-CUR-PARENT)
                   TO WS-SEARCH-IDX
           END-IF
           PERFORM UNTIL WS-SEARCH-IDX = 0 OR WS-IR-IDX > 0
               IF CDD-IRF-JKEY-LEN(WS-SEARCH-IDX)
                   = CDD-JDN-KEY-LEN(WS-PROP-DOM)
                   AND CDD-IRF-JSON-KEY(WS-SEARCH-IDX)
                       = CDD-JDN-KEY(WS-PROP-DOM)
                   MOVE WS-SEARCH-IDX TO WS-IR-IDX
               END-IF
               MOVE CDD-IRF-NEXT-SIB(WS-SEARCH-IDX) TO WS-SEARCH-IDX
           END-PERFORM
           IF WS-IR-IDX = 0
               PERFORM ALLOC-NEW-INF-FIELD
           END-IF.
      * @paragraph ALLOC-NEW-INF-FIELD Create uninitialized IR field.
       ALLOC-NEW-INF-FIELD.
           IF CDD-IR-FLD-COUNT >= CDD-IR-FLD-CAP
               MOVE 3001 TO CDD-ERR-CODE
               MOVE "CDDJINF" TO CDD-ERR-SUBPROG
               MOVE "IR field pool exhausted in CDDJINF"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-IR-FLD-COUNT
               END-ADD
               MOVE CDD-IR-FLD-COUNT TO WS-IR-IDX
               MOVE WS-CUR-PARENT TO CDD-IRF-PARENT(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-FIRST-CHILD(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-LAST-CHILD(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-NEXT-SIB(WS-IR-IDX)
               MOVE CDD-JDN-KEY-LEN(WS-PROP-DOM)
                   TO CDD-IRF-JKEY-LEN(WS-IR-IDX)
               MOVE CDD-JDN-KEY(WS-PROP-DOM)
                   TO CDD-IRF-JSON-KEY(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-CNAM-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-COB-NAME(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-DESC-LEN(WS-IR-IDX)
               MOVE SPACES TO CDD-IRF-DESC(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-KIND(WS-IR-IDX)
               MOVE 1 TO CDD-IRF-REQUIRED(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-NULLABLE(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-SIGNED(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-MIN-LEN(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
               MOVE 0 TO CDD-IRF-DIGITS(WS-IR-IDX)
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
      * @paragraph MERGE-PROP-OBSERVATION Handle array vs single value.
       MERGE-PROP-OBSERVATION.
           IF CDD-JDN-IS-ARRAY(WS-PROP-DOM)
               MOVE 2 TO CDD-IRF-ARR-MODE(WS-IR-IDX)
               IF CDD-JDN-CHILD-CNT(WS-PROP-DOM)
                   > CDD-IRF-ARR-MAX(WS-IR-IDX)
                   MOVE CDD-JDN-CHILD-CNT(WS-PROP-DOM)
                       TO CDD-IRF-ARR-MAX(WS-IR-IDX)
               END-IF
               IF CDD-IRF-ARR-MAX(WS-IR-IDX) = 0
                   MOVE 1 TO CDD-IRF-ARR-MAX(WS-IR-IDX)
               END-IF
               MOVE CDD-JDN-FIRST-CHILD(WS-PROP-DOM) TO WS-ELEM-DOM
               PERFORM UNTIL WS-ELEM-DOM = 0 OR NOT CDD-OK
                   IF CDD-JDN-IS-ARRAY(WS-ELEM-DOM)
                       PERFORM FAIL-INFER-CONFLICT
                   ELSE
                       MOVE WS-ELEM-DOM TO WS-OBS-DOM
                       PERFORM MERGE-SINGLE-OBS
                   END-IF
                   MOVE CDD-JDN-NEXT-SIB(WS-ELEM-DOM) TO WS-ELEM-DOM
               END-PERFORM
           ELSE
               MOVE WS-PROP-DOM TO WS-OBS-DOM
               PERFORM MERGE-SINGLE-OBS
           END-IF.
      * @paragraph MERGE-SINGLE-OBS Merge WS-OBS-DOM into WS-IR-IDX.
       MERGE-SINGLE-OBS.
           EVALUATE TRUE
               WHEN CDD-JDN-IS-NULL(WS-OBS-DOM)
                   MOVE 1 TO CDD-IRF-NULLABLE(WS-IR-IDX)
                   MOVE 0 TO CDD-IRF-REQUIRED(WS-IR-IDX)
               WHEN CDD-JDN-IS-OBJECT(WS-OBS-DOM)
                   IF CDD-IRF-KIND(WS-IR-IDX) = 0
                       OR CDD-IRF-IS-GROUP(WS-IR-IDX)
                       MOVE 1 TO CDD-IRF-KIND(WS-IR-IDX)
                       ADD 1 TO WS-MQ-TAIL
                       END-ADD
                       MOVE WS-OBS-DOM TO WS-MQ-DOM(WS-MQ-TAIL)
                       MOVE WS-IR-IDX TO WS-MQ-PARENT(WS-MQ-TAIL)
                   ELSE
                       PERFORM FAIL-INFER-CONFLICT
                   END-IF
               WHEN CDD-JDN-IS-STRING(WS-OBS-DOM)
                   PERFORM MERGE-STRING-OBS
               WHEN CDD-JDN-IS-INTEGER(WS-OBS-DOM)
               WHEN CDD-JDN-IS-NUMBER(WS-OBS-DOM)
                   PERFORM MERGE-NUMERIC-OBS
               WHEN OTHER
                   PERFORM MERGE-BOOL-OBS
           END-EVALUATE.
      * @paragraph MERGE-STRING-OBS Infer length and date/uuid format.
       MERGE-STRING-OBS.
           IF CDD-IRF-KIND(WS-IR-IDX) NOT = 0
               AND NOT CDD-IRF-IS-STRING(WS-IR-IDX)
               PERFORM FAIL-INFER-CONFLICT
           ELSE
               MOVE 2 TO CDD-IRF-KIND(WS-IR-IDX)
               IF CDD-JDN-VAL-LEN(WS-OBS-DOM)
                   > CDD-IRF-MAX-LEN(WS-IR-IDX)
                   MOVE CDD-JDN-VAL-LEN(WS-OBS-DOM)
                       TO CDD-IRF-MAX-LEN(WS-IR-IDX)
               END-IF
               IF CDD-IRF-MAX-LEN(WS-IR-IDX) = 0
                   MOVE 1 TO CDD-IRF-MAX-LEN(WS-IR-IDX)
               END-IF
               PERFORM DETECT-STRING-FORMAT
               IF CDD-IRF-DEF-LEN(WS-IR-IDX) = 0
                   AND CDD-JDN-VAL-LEN(WS-OBS-DOM) > 0
                   AND CDD-JDN-VAL-LEN(WS-OBS-DOM) <= 64
                   MOVE CDD-JDN-VAL-LEN(WS-OBS-DOM)
                       TO CDD-IRF-DEF-LEN(WS-IR-IDX)
                   MOVE CDD-JDN-VAL(WS-OBS-DOM)(
                       1:CDD-IRF-DEF-LEN(WS-IR-IDX))
                       TO CDD-IRF-DEF-VAL(WS-IR-IDX)(
                       1:CDD-IRF-DEF-LEN(WS-IR-IDX))
               END-IF
           END-IF.
      * @paragraph DETECT-STRING-FORMAT Check YYYY-MM-DD and UUID.
       DETECT-STRING-FORMAT.
           MOVE 0 TO WS-IS-DATE
           MOVE 0 TO WS-IS-UUID
           IF CDD-JDN-VAL-LEN(WS-OBS-DOM) = 10
               AND CDD-JDN-VAL(WS-OBS-DOM)(5:1) = "-"
               AND CDD-JDN-VAL(WS-OBS-DOM)(8:1) = "-"
               MOVE 1 TO WS-IS-DATE
               MOVE 1 TO WS-I
               PERFORM UNTIL WS-I > 10
                   IF WS-I NOT = 5 AND WS-I NOT = 8
                       MOVE CDD-JDN-VAL(WS-OBS-DOM)(WS-I:1) TO WS-CH
                       IF WS-CH < "0" OR WS-CH > "9"
                           MOVE 0 TO WS-IS-DATE
                       END-IF
                   END-IF
                   ADD 1 TO WS-I
                   END-ADD
               END-PERFORM
               IF WS-IS-DATE = 1
                   MOVE "date" TO CDD-IRF-FORMAT(WS-IR-IDX)
               END-IF
           END-IF
           IF CDD-JDN-VAL-LEN(WS-OBS-DOM) = 36
               AND CDD-JDN-VAL(WS-OBS-DOM)(9:1) = "-"
               AND CDD-JDN-VAL(WS-OBS-DOM)(14:1) = "-"
               AND CDD-JDN-VAL(WS-OBS-DOM)(19:1) = "-"
               AND CDD-JDN-VAL(WS-OBS-DOM)(24:1) = "-"
               MOVE "uuid" TO CDD-IRF-FORMAT(WS-IR-IDX)
           END-IF.
      * @paragraph MERGE-NUMERIC-OBS Infer int/decimal/float precision.
       MERGE-NUMERIC-OBS.
           IF CDD-IRF-KIND(WS-IR-IDX) NOT = 0
               AND NOT CDD-IRF-IS-INTEGER(WS-IR-IDX)
               AND NOT CDD-IRF-IS-DECIMAL(WS-IR-IDX)
               AND NOT CDD-IRF-IS-FLOAT(WS-IR-IDX)
               PERFORM FAIL-INFER-CONFLICT
           ELSE
               PERFORM MEASURE-NUMBER-TEXT
               IF WS-OBS-SIGN = 1
                   MOVE 1 TO CDD-IRF-SIGNED(WS-IR-IDX)
               END-IF
               IF CDD-IRF-DEF-LEN(WS-IR-IDX) = 0
                   AND CDD-JDN-VAL-LEN(WS-OBS-DOM) <= 64
                   MOVE CDD-JDN-VAL-LEN(WS-OBS-DOM)
                       TO CDD-IRF-DEF-LEN(WS-IR-IDX)
                   MOVE CDD-JDN-VAL(WS-OBS-DOM)(
                       1:CDD-IRF-DEF-LEN(WS-IR-IDX))
                       TO CDD-IRF-DEF-VAL(WS-IR-IDX)(
                       1:CDD-IRF-DEF-LEN(WS-IR-IDX))
               END-IF
               IF WS-OBS-EXP = 1 OR CDD-IRF-IS-FLOAT(WS-IR-IDX)
                   MOVE 7 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 4 TO CDD-IRF-USAGE(WS-IR-IDX)
               ELSE
                   IF WS-OBS-FRACD > 0
                       OR CDD-IRF-IS-DECIMAL(WS-IR-IDX)
                       PERFORM WIDEN-DECIMAL-OBS
                   ELSE
                       MOVE 5 TO CDD-IRF-KIND(WS-IR-IDX)
                       IF WS-OBS-INTD > CDD-IRF-DIGITS(WS-IR-IDX)
                           MOVE WS-OBS-INTD TO CDD-IRF-DIGITS(WS-IR-IDX)
                       END-IF
                   END-IF
               END-IF
           END-IF.
      * @paragraph WIDEN-DECIMAL-OBS Widen integer and scale digits.
       WIDEN-DECIMAL-OBS.
           COMPUTE WS-OLD-INTD =
               CDD-IRF-DIGITS(WS-IR-IDX) - CDD-IRF-SCALE(WS-IR-IDX)
           END-COMPUTE
           IF WS-OBS-INTD > WS-OLD-INTD
               MOVE WS-OBS-INTD TO WS-OLD-INTD
           END-IF
           IF WS-OBS-FRACD > CDD-IRF-SCALE(WS-IR-IDX)
               MOVE WS-OBS-FRACD TO CDD-IRF-SCALE(WS-IR-IDX)
           END-IF
           MOVE 6 TO CDD-IRF-KIND(WS-IR-IDX)
           COMPUTE CDD-IRF-DIGITS(WS-IR-IDX) =
               WS-OLD-INTD + CDD-IRF-SCALE(WS-IR-IDX)
           END-COMPUTE.
      * @paragraph MEASURE-NUMBER-TEXT Count sign, int, frac, exp.
       MEASURE-NUMBER-TEXT.
           MOVE 0 TO WS-OBS-SIGN
           MOVE 0 TO WS-OBS-INTD
           MOVE 0 TO WS-OBS-FRACD
           MOVE 0 TO WS-OBS-EXP
           MOVE 0 TO WS-SEEN-DOT
           MOVE 1 TO WS-I
           IF CDD-JDN-VAL(WS-OBS-DOM)(1:1) = "-"
               MOVE 1 TO WS-OBS-SIGN
               MOVE 2 TO WS-I
           END-IF
           PERFORM UNTIL WS-I > CDD-JDN-VAL-LEN(WS-OBS-DOM)
               OR WS-OBS-EXP = 1
               MOVE CDD-JDN-VAL(WS-OBS-DOM)(WS-I:1) TO WS-CH
               IF WS-CH = "e" OR WS-CH = "E"
                   MOVE 1 TO WS-OBS-EXP
               ELSE
                   IF WS-CH = "."
                       MOVE 1 TO WS-SEEN-DOT
                   ELSE
                       IF WS-SEEN-DOT = 0
                           ADD 1 TO WS-OBS-INTD
                           END-ADD
                       ELSE
                           ADD 1 TO WS-OBS-FRACD
                           END-ADD
                       END-IF
                   END-IF
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM.
      * @paragraph MERGE-BOOL-OBS Merge true/false observation.
       MERGE-BOOL-OBS.
           IF CDD-IRF-KIND(WS-IR-IDX) NOT = 0
               AND NOT CDD-IRF-IS-BOOL(WS-IR-IDX)
               PERFORM FAIL-INFER-CONFLICT
           ELSE
               MOVE 9 TO CDD-IRF-KIND(WS-IR-IDX)
               MOVE 1 TO CDD-IRF-DIGITS(WS-IR-IDX)
               IF CDD-IRF-DEF-LEN(WS-IR-IDX) = 0
                   IF CDD-JDN-IS-TRUE(WS-OBS-DOM)
                       MOVE "true" TO CDD-IRF-DEF-VAL(WS-IR-IDX)
                       MOVE 4 TO CDD-IRF-DEF-LEN(WS-IR-IDX)
                   ELSE
                       MOVE "false" TO CDD-IRF-DEF-VAL(WS-IR-IDX)
                       MOVE 5 TO CDD-IRF-DEF-LEN(WS-IR-IDX)
                   END-IF
               END-IF
           END-IF.
      * @paragraph FINALIZE-NULL-FIELDS Default null-only fields to X16.
       FINALIZE-NULL-FIELDS.
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > CDD-IR-FLD-COUNT
               IF CDD-IRF-KIND(WS-I) = 0
                   MOVE 2 TO CDD-IRF-KIND(WS-I)
                   MOVE 16 TO CDD-IRF-MAX-LEN(WS-I)
               END-IF
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM.
      * @paragraph FAIL-INFER-CONFLICT Populate 1106 conflict error.
       FAIL-INFER-CONFLICT.
           MOVE 1106 TO CDD-ERR-CODE
           MOVE "CDDJINF" TO CDD-ERR-SUBPROG
           MOVE "Empty or conflicting sample JSON types"
               TO CDD-ERR-MESSAGE.
