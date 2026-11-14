      * @file CDDIRDF.cbl
      * @brief Ordered structural IR differ for surgical CST sync.
      * @module CDDIRDF
      * @details Computes an ordered patch plan (KEEP, UPDATE, INSERT,
      *          REMOVE, GROUP-RECURSE) between an existing COBOL IR
      *          (with CST node back-links) and a target JSON/Schema IR,
      *          preserving * @cdd-keep helper fields and tracking
      *          exact sibling CST insertion anchors.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDIRDF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-SCOPE-QUEUE Non-recursive group comparison queue.
       01  WS-SCOPE-QUEUE.
      * @field WS-SQ-HEAD Queue read index.
           05  WS-SQ-HEAD                PIC 9(4) COMP VALUE 1.
      * @field WS-SQ-TAIL Queue write index.
           05  WS-SQ-TAIL                PIC 9(4) COMP VALUE 0.
      * @field WS-SQ-ITEM Queued (old parent, new parent, CST group).
           05  WS-SQ-ITEM                OCCURS 256 TIMES.
      * @field WS-SQ-OLD-PAR Parent field index in OLD IR (0=root).
               10  WS-SQ-OLD-PAR         PIC 9(4) COMP.
      * @field WS-SQ-NEW-PAR Parent field index in NEW IR (0=root).
               10  WS-SQ-NEW-PAR         PIC 9(4) COMP.
      * @field WS-SQ-CST-PAR Enclosing CST group/record node index.
               10  WS-SQ-CST-PAR         PIC 9(4) COMP.
      * @record WS-DIFF-STATE Iteration cursors and match results.
       01  WS-DIFF-STATE.
      * @field WS-CUR-OLD-PAR Current OLD IR parent index.
           05  WS-CUR-OLD-PAR            PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-NEW-PAR Current NEW IR parent index.
           05  WS-CUR-NEW-PAR            PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-CST-PAR Current enclosing CST group node.
           05  WS-CUR-CST-PAR            PIC 9(4) COMP VALUE 0.
      * @field WS-OLD-ITER Sibling cursor in OLD IR.
           05  WS-OLD-ITER               PIC 9(4) COMP VALUE 0.
      * @field WS-NEW-ITER Sibling cursor in NEW IR.
           05  WS-NEW-ITER               PIC 9(4) COMP VALUE 0.
      * @field WS-MATCH-OLD Matched OLD IR index (0=not found).
           05  WS-MATCH-OLD              PIC 9(4) COMP VALUE 0.
      * @field WS-MATCH-NEW Matched NEW IR index (0=not found).
           05  WS-MATCH-NEW              PIC 9(4) COMP VALUE 0.
      * @field WS-ANCHOR-CST Last kept/updated CST sibling for INSERT.
           05  WS-ANCHOR-CST             PIC 9(4) COMP VALUE 0.
      * @field WS-OP-KIND Pending diff op kind (1..5).
           05  WS-OP-KIND                PIC 9(2) COMP VALUE 0.
      * @field WS-OP-OLD Pending OLD IR index.
           05  WS-OP-OLD                 PIC 9(4) COMP VALUE 0.
      * @field WS-OP-NEW Pending NEW IR index.
           05  WS-OP-NEW                 PIC 9(4) COMP VALUE 0.
      * @field WS-OP-CST Pending CST node or anchor index.
           05  WS-OP-CST                 PIC 9(4) COMP VALUE 0.
      * @field WS-TYPES-SAME 1 if OLD and NEW types/bounds match.
           05  WS-TYPES-SAME             PIC 9(2) COMP VALUE 0.
       LINKAGE SECTION.
      * @record CDD-OLD-IR Existing COBOL UDT Canonical IR.
       01  CDD-OLD-IR.
      * @field CDD-OIR-TAG Magic type-safety tag ('CDDIR001').
           05  CDD-OIR-TAG               PIC X(8).
      * @field CDD-OIR-TAG-VALID Condition confirming valid OIR tag.
               88  CDD-OIR-TAG-VALID              VALUE "CDDIR001".
      * @field CDD-OIR-UDT-KIND UDT kind.
           05  CDD-OIR-UDT-KIND          PIC 9(2) COMP.
      * @field CDD-OIR-COB-NLEN Name length.
           05  CDD-OIR-COB-NLEN          PIC 9(2) COMP.
      * @field CDD-OIR-COB-NAME COBOL name.
           05  CDD-OIR-COB-NAME          PIC X(64).
      * @field CDD-OIR-TITLE-LEN Title length.
           05  CDD-OIR-TITLE-LEN         PIC 9(4) COMP.
      * @field CDD-OIR-JSON-TITLE JSON title.
           05  CDD-OIR-JSON-TITLE        PIC X(128).
      * @field CDD-OIR-DESC-LEN Desc length.
           05  CDD-OIR-DESC-LEN          PIC 9(4) COMP.
      * @field CDD-OIR-DESC Description.
           05  CDD-OIR-DESC              PIC X(256).
      * @field CDD-OIR-ROOT-FIRST First root field.
           05  CDD-OIR-ROOT-FIRST        PIC 9(4) COMP.
      * @field CDD-OIR-ROOT-LAST Last root field.
           05  CDD-OIR-ROOT-LAST         PIC 9(4) COMP.
      * @field CDD-OIR-FLD-COUNT Field count.
           05  CDD-OIR-FLD-COUNT         PIC 9(4) COMP.
      * @field CDD-OIR-FLD-CAP Field capacity.
           05  CDD-OIR-FLD-CAP           PIC 9(4) COMP.
      * @field CDD-OIR-FIELD Old IR field pool.
           05  CDD-OIR-FIELD             OCCURS 256 TIMES.
      * @field CDD-OIF-PARENT Parent index.
               10  CDD-OIF-PARENT        PIC 9(4) COMP.
      * @field CDD-OIF-FIRST-CHILD First child index.
               10  CDD-OIF-FIRST-CHILD   PIC 9(4) COMP.
      * @field CDD-OIF-LAST-CHILD Last child index.
               10  CDD-OIF-LAST-CHILD    PIC 9(4) COMP.
      * @field CDD-OIF-NEXT-SIB Next sibling index.
               10  CDD-OIF-NEXT-SIB      PIC 9(4) COMP.
      * @field CDD-OIF-JKEY-LEN JSON key length.
               10  CDD-OIF-JKEY-LEN      PIC 9(4) COMP.
      * @field CDD-OIF-JSON-KEY JSON key text.
               10  CDD-OIF-JSON-KEY      PIC X(128).
      * @field CDD-OIF-CNAM-LEN COBOL name length.
               10  CDD-OIF-CNAM-LEN      PIC 9(2) COMP.
      * @field CDD-OIF-COB-NAME COBOL name text.
               10  CDD-OIF-COB-NAME      PIC X(64).
      * @field CDD-OIF-DESC-LEN Desc length.
               10  CDD-OIF-DESC-LEN      PIC 9(4) COMP.
      * @field CDD-OIF-DESC Description.
               10  CDD-OIF-DESC          PIC X(160).
      * @field CDD-OIF-KIND Field kind.
               10  CDD-OIF-KIND          PIC 9(2) COMP.
      * @field CDD-OIF-REQUIRED Required flag.
               10  CDD-OIF-REQUIRED      PIC 9(2) COMP.
      * @field CDD-OIF-NULLABLE Nullable flag.
               10  CDD-OIF-NULLABLE      PIC 9(2) COMP.
      * @field CDD-OIF-SIGNED Signed flag.
               10  CDD-OIF-SIGNED        PIC 9(2) COMP.
      * @field CDD-OIF-MIN-LEN Min length.
               10  CDD-OIF-MIN-LEN       PIC 9(8) COMP.
      * @field CDD-OIF-MAX-LEN Max length.
               10  CDD-OIF-MAX-LEN       PIC 9(8) COMP.
      * @field CDD-OIF-DIGITS Digits count.
               10  CDD-OIF-DIGITS        PIC 9(4) COMP.
      * @field CDD-OIF-SCALE Scale count.
               10  CDD-OIF-SCALE         PIC 9(4) COMP.
      * @field CDD-OIF-HAS-BOUNDS Has bounds flag.
               10  CDD-OIF-HAS-BOUNDS    PIC 9(2) COMP.
      * @field CDD-OIF-MIN-VAL Min value.
               10  CDD-OIF-MIN-VAL       PIC S9(12) COMP.
      * @field CDD-OIF-MAX-VAL Max value.
               10  CDD-OIF-MAX-VAL       PIC S9(12) COMP.
      * @field CDD-OIF-FORMAT Format text.
               10  CDD-OIF-FORMAT        PIC X(16).
      * @field CDD-OIF-USAGE Usage code.
               10  CDD-OIF-USAGE         PIC 9(2) COMP.
      * @field CDD-OIF-PIC-LEN PIC length.
               10  CDD-OIF-PIC-LEN       PIC 9(2) COMP.
      * @field CDD-OIF-PIC-TEXT PIC text.
               10  CDD-OIF-PIC-TEXT      PIC X(32).
      * @field CDD-OIF-COB-LEVEL COBOL level.
               10  CDD-OIF-COB-LEVEL     PIC 9(2) COMP.
      * @field CDD-OIF-REDEF-LEN Redefines length.
               10  CDD-OIF-REDEF-LEN     PIC 9(2) COMP.
      * @field CDD-OIF-REDEFINES Redefines target.
               10  CDD-OIF-REDEFINES     PIC X(64).
      * @field CDD-OIF-SYNC-JUST Sync/Just bitmask.
               10  CDD-OIF-SYNC-JUST     PIC 9(2) COMP.
      * @field CDD-OIF-DEF-LEN Default length.
               10  CDD-OIF-DEF-LEN       PIC 9(4) COMP.
      * @field CDD-OIF-DEF-VAL Default value.
               10  CDD-OIF-DEF-VAL       PIC X(64).
      * @field CDD-OIF-ARR-MODE Array mode.
               10  CDD-OIF-ARR-MODE      PIC 9(2) COMP.
      * @field CDD-OIF-ARR-MIN Array min.
               10  CDD-OIF-ARR-MIN       PIC 9(6) COMP.
      * @field CDD-OIF-ARR-MAX Array max.
               10  CDD-OIF-ARR-MAX       PIC 9(6) COMP.
      * @field CDD-OIF-ENUM-FIRST Enum start.
               10  CDD-OIF-ENUM-FIRST    PIC 9(4) COMP.
      * @field CDD-OIF-ENUM-COUNT Enum count.
               10  CDD-OIF-ENUM-COUNT    PIC 9(4) COMP.
      * @field CDD-OIF-KEEP-HELPER @cdd-keep flag.
               10  CDD-OIF-KEEP-HELPER   PIC 9(2) COMP.
      * @field CDD-OIR-ENUM-CNT Old enum count.
           05  CDD-OIR-ENUM-CNT          PIC 9(4) COMP.
      * @field CDD-OIR-ENUM-CAP Old enum cap.
           05  CDD-OIR-ENUM-CAP          PIC 9(4) COMP.
      * @field CDD-OIR-ENUM Old enum pool.
           05  CDD-OIR-ENUM              OCCURS 256 TIMES.
      * @field CDD-OIE-CNAM-LEN 88 name length.
               10  CDD-OIE-CNAM-LEN      PIC 9(2) COMP.
      * @field CDD-OIE-COB-NAME 88 name.
               10  CDD-OIE-COB-NAME      PIC X(64).
      * @field CDD-OIE-VAL-LEN 88 value length.
               10  CDD-OIE-VAL-LEN       PIC 9(4) COMP.
      * @field CDD-OIE-VAL 88 value.
               10  CDD-OIE-VAL           PIC X(64).
      * @field CDD-OIE-IS-STR 88 string flag.
               10  CDD-OIE-IS-STR        PIC 9(2) COMP.
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDIRDF Compute ordered sync diff plan OLD -> NEW.
      * @param [in] CDD-IR-MAP-REQ Contains CDD-MPR-MATCHED-CST root.
      * @param [in] CDD-OLD-IR Existing COBOL UDT Canonical IR.
      * @param [in] CDD-IR-FIELD-MAP Back-links from OLD IR to CST.
      * @param [in] CDD-IR Target JSON/Schema Canonical IR.
      * @param [inout] CDD-DIFF-PLAN Output ordered diff operations.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Ordered KEEP/UPDATE/INSERT/REMOVE/GROUP operations.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-IR-CAPACITY.
       PROCEDURE DIVISION USING CDD-IR-MAP-REQ CDD-OLD-IR
           CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags and diff scopes iteratively.
       MAIN-ENTRY.
           IF NOT CDD-MPR-TAG-VALID
               OR NOT CDD-OIR-TAG-VALID
               OR NOT CDD-FMP-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               OR NOT CDD-DIF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDIRDF" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDIRDF"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-DIF-COUNT
           MOVE 1 TO WS-SQ-HEAD
           MOVE 1 TO WS-SQ-TAIL
           MOVE 0 TO WS-SQ-OLD-PAR(1)
           MOVE 0 TO WS-SQ-NEW-PAR(1)
           MOVE CDD-MPR-MATCHED-CST TO WS-SQ-CST-PAR(1)
           PERFORM UNTIL WS-SQ-HEAD > WS-SQ-TAIL OR NOT CDD-OK
               MOVE WS-SQ-OLD-PAR(WS-SQ-HEAD) TO WS-CUR-OLD-PAR
               MOVE WS-SQ-NEW-PAR(WS-SQ-HEAD) TO WS-CUR-NEW-PAR
               MOVE WS-SQ-CST-PAR(WS-SQ-HEAD) TO WS-CUR-CST-PAR
               ADD 1 TO WS-SQ-HEAD
               END-ADD
               PERFORM DIFF-ONE-SCOPE
           END-PERFORM
           EXIT PROGRAM.
      * @paragraph PUSH-DIFF-OP Append operation to CDD-DIFF-PLAN.
       PUSH-DIFF-OP.
           IF CDD-DIF-COUNT >= CDD-DIF-CAP
               MOVE 3001 TO CDD-ERR-CODE
               MOVE "CDDIRDF" TO CDD-ERR-SUBPROG
               MOVE "Diff plan capacity exhausted" TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-DIF-COUNT
               END-ADD
               MOVE WS-OP-KIND TO CDD-DFO-KIND(CDD-DIF-COUNT)
               MOVE WS-OP-OLD TO CDD-DFO-OLD-IR(CDD-DIF-COUNT)
               MOVE WS-OP-NEW TO CDD-DFO-NEW-IR(CDD-DIF-COUNT)
               MOVE WS-OP-CST TO CDD-DFO-CST-NODE(CDD-DIF-COUNT)
               MOVE WS-CUR-CST-PAR
                   TO CDD-DFO-PARENT-CST(CDD-DIF-COUNT)
           END-IF.
      * @paragraph DIFF-ONE-SCOPE Emit removals then ordered keeps/adds.
       DIFF-ONE-SCOPE.
           PERFORM SCAN-OLD-FOR-REMOVALS
           IF CDD-OK
               PERFORM SCAN-NEW-SCOPE-ITEMS
           END-IF.
      * @paragraph SCAN-OLD-FOR-REMOVALS Remove absent non-@cdd-keep.
       SCAN-OLD-FOR-REMOVALS.
           IF WS-CUR-OLD-PAR = 0
               MOVE CDD-OIR-ROOT-FIRST TO WS-OLD-ITER
           ELSE
               MOVE CDD-OIF-FIRST-CHILD(WS-CUR-OLD-PAR) TO WS-OLD-ITER
           END-IF
           PERFORM UNTIL WS-OLD-ITER = 0 OR NOT CDD-OK
               IF CDD-OIF-KEEP-HELPER(WS-OLD-ITER) = 1
                   MOVE 1 TO WS-OP-KIND
                   MOVE WS-OLD-ITER TO WS-OP-OLD
                   MOVE 0 TO WS-OP-NEW
                   MOVE CDD-FMP-CST-NODE(WS-OLD-ITER) TO WS-OP-CST
                   PERFORM PUSH-DIFF-OP
               ELSE
                   PERFORM FIND-MATCH-IN-NEW
                   IF WS-MATCH-NEW = 0
                       IF CDD-FMP-COMP-CST(WS-OLD-ITER) > 0
                           MOVE 4 TO WS-OP-KIND
                           MOVE WS-OLD-ITER TO WS-OP-OLD
                           MOVE 0 TO WS-OP-NEW
                           MOVE CDD-FMP-COMP-CST(WS-OLD-ITER)
                               TO WS-OP-CST
                           PERFORM PUSH-DIFF-OP
                       END-IF
                       MOVE 4 TO WS-OP-KIND
                       MOVE WS-OLD-ITER TO WS-OP-OLD
                       MOVE 0 TO WS-OP-NEW
                       MOVE CDD-FMP-CST-NODE(WS-OLD-ITER) TO WS-OP-CST
                       PERFORM PUSH-DIFF-OP
                   END-IF
               END-IF
               MOVE CDD-OIF-NEXT-SIB(WS-OLD-ITER) TO WS-OLD-ITER
           END-PERFORM.
      * @paragraph FIND-MATCH-IN-NEW Find WS-OLD-ITER in NEW scope.
       FIND-MATCH-IN-NEW.
           MOVE 0 TO WS-MATCH-NEW
           IF WS-CUR-NEW-PAR = 0
               MOVE CDD-IR-ROOT-FIRST TO WS-NEW-ITER
           ELSE
               MOVE CDD-IRF-FIRST-CHILD(WS-CUR-NEW-PAR) TO WS-NEW-ITER
           END-IF
           PERFORM UNTIL WS-NEW-ITER = 0 OR WS-MATCH-NEW > 0
               IF (CDD-OIF-JKEY-LEN(WS-OLD-ITER)
                   = CDD-IRF-JKEY-LEN(WS-NEW-ITER)
                   AND CDD-OIF-JSON-KEY(WS-OLD-ITER)
                       = CDD-IRF-JSON-KEY(WS-NEW-ITER))
                   OR (CDD-OIF-CNAM-LEN(WS-OLD-ITER)
                       = CDD-IRF-CNAM-LEN(WS-NEW-ITER)
                   AND CDD-OIF-COB-NAME(WS-OLD-ITER)
                       = CDD-IRF-COB-NAME(WS-NEW-ITER))
                   MOVE WS-NEW-ITER TO WS-MATCH-NEW
               END-IF
               MOVE CDD-IRF-NEXT-SIB(WS-NEW-ITER) TO WS-NEW-ITER
           END-PERFORM.
      * @paragraph SCAN-NEW-SCOPE-ITEMS Walk NEW in order.
       SCAN-NEW-SCOPE-ITEMS.
           MOVE 0 TO WS-ANCHOR-CST
           IF WS-CUR-NEW-PAR = 0
               MOVE CDD-IR-ROOT-FIRST TO WS-NEW-ITER
           ELSE
               MOVE CDD-IRF-FIRST-CHILD(WS-CUR-NEW-PAR) TO WS-NEW-ITER
           END-IF
           PERFORM UNTIL WS-NEW-ITER = 0 OR NOT CDD-OK
               PERFORM FIND-MATCH-IN-OLD
               IF WS-MATCH-OLD = 0
                   MOVE 3 TO WS-OP-KIND
                   MOVE 0 TO WS-OP-OLD
                   MOVE WS-NEW-ITER TO WS-OP-NEW
                   MOVE WS-ANCHOR-CST TO WS-OP-CST
                   PERFORM PUSH-DIFF-OP
               ELSE
                   PERFORM COMPARE-MATCHED-PAIR
                   MOVE CDD-FMP-CST-NODE(WS-MATCH-OLD)
                       TO WS-ANCHOR-CST
               END-IF
               MOVE CDD-IRF-NEXT-SIB(WS-NEW-ITER) TO WS-NEW-ITER
           END-PERFORM.
      * @paragraph FIND-MATCH-IN-OLD Find WS-NEW-ITER in OLD scope.
       FIND-MATCH-IN-OLD.
           MOVE 0 TO WS-MATCH-OLD
           IF WS-CUR-OLD-PAR = 0
               MOVE CDD-OIR-ROOT-FIRST TO WS-OLD-ITER
           ELSE
               MOVE CDD-OIF-FIRST-CHILD(WS-CUR-OLD-PAR) TO WS-OLD-ITER
           END-IF
           PERFORM UNTIL WS-OLD-ITER = 0 OR WS-MATCH-OLD > 0
               IF CDD-OIF-KEEP-HELPER(WS-OLD-ITER) = 0
                   AND ((CDD-OIF-JKEY-LEN(WS-OLD-ITER)
                       = CDD-IRF-JKEY-LEN(WS-NEW-ITER)
                   AND CDD-OIF-JSON-KEY(WS-OLD-ITER)
                       = CDD-IRF-JSON-KEY(WS-NEW-ITER))
                   OR (CDD-OIF-CNAM-LEN(WS-OLD-ITER)
                       = CDD-IRF-CNAM-LEN(WS-NEW-ITER)
                   AND CDD-OIF-COB-NAME(WS-OLD-ITER)
                       = CDD-IRF-COB-NAME(WS-NEW-ITER)))
                   MOVE WS-OLD-ITER TO WS-MATCH-OLD
               END-IF
               MOVE CDD-OIF-NEXT-SIB(WS-OLD-ITER) TO WS-OLD-ITER
           END-PERFORM.
      * @paragraph COMPARE-MATCHED-PAIR Emit GROUP, KEEP, or UPDATE.
       COMPARE-MATCHED-PAIR.
           IF CDD-OIF-KIND(WS-MATCH-OLD) = 1
               AND CDD-IRF-IS-GROUP(WS-NEW-ITER)
               AND CDD-OIF-ARR-MODE(WS-MATCH-OLD)
                   = CDD-IRF-ARR-MODE(WS-NEW-ITER)
               AND CDD-OIF-ARR-MAX(WS-MATCH-OLD)
                   = CDD-IRF-ARR-MAX(WS-NEW-ITER)
               MOVE 5 TO WS-OP-KIND
               MOVE WS-MATCH-OLD TO WS-OP-OLD
               MOVE WS-NEW-ITER TO WS-OP-NEW
               MOVE CDD-FMP-CST-NODE(WS-MATCH-OLD) TO WS-OP-CST
               PERFORM PUSH-DIFF-OP
               ADD 1 TO WS-SQ-TAIL
               END-ADD
               MOVE WS-MATCH-OLD TO WS-SQ-OLD-PAR(WS-SQ-TAIL)
               MOVE WS-NEW-ITER TO WS-SQ-NEW-PAR(WS-SQ-TAIL)
               MOVE CDD-FMP-CST-NODE(WS-MATCH-OLD)
                   TO WS-SQ-CST-PAR(WS-SQ-TAIL)
           ELSE
               PERFORM CHECK-SCALAR-EQUIVALENCE
               IF WS-TYPES-SAME = 1
                   MOVE 1 TO WS-OP-KIND
               ELSE
                   MOVE 2 TO WS-OP-KIND
               END-IF
               MOVE WS-MATCH-OLD TO WS-OP-OLD
               MOVE WS-NEW-ITER TO WS-OP-NEW
               MOVE CDD-FMP-CST-NODE(WS-MATCH-OLD) TO WS-OP-CST
               PERFORM PUSH-DIFF-OP
           END-IF.
      * @paragraph CHECK-SCALAR-EQUIVALENCE Test type/len/digits/array.
       CHECK-SCALAR-EQUIVALENCE.
           MOVE 0 TO WS-TYPES-SAME
           IF CDD-OIF-ARR-MODE(WS-MATCH-OLD)
               = CDD-IRF-ARR-MODE(WS-NEW-ITER)
               AND CDD-OIF-ARR-MAX(WS-MATCH-OLD)
                   = CDD-IRF-ARR-MAX(WS-NEW-ITER)
               IF CDD-OIF-KIND(WS-MATCH-OLD) >= 2
                   AND CDD-OIF-KIND(WS-MATCH-OLD) <= 4
                   AND CDD-IRF-KIND(WS-NEW-ITER) >= 2
                   AND CDD-IRF-KIND(WS-NEW-ITER) <= 4
                   IF CDD-OIF-MAX-LEN(WS-MATCH-OLD)
                       = CDD-IRF-MAX-LEN(WS-NEW-ITER)
                       MOVE 1 TO WS-TYPES-SAME
                   END-IF
               ELSE
                   IF CDD-OIF-KIND(WS-MATCH-OLD)
                       = CDD-IRF-KIND(WS-NEW-ITER)
                       EVALUATE CDD-IRF-KIND(WS-NEW-ITER)
                           WHEN 5
                           WHEN 6
                               IF CDD-OIF-DIGITS(WS-MATCH-OLD)
                                   = CDD-IRF-DIGITS(WS-NEW-ITER)
                                   AND CDD-OIF-SCALE(WS-MATCH-OLD)
                                       = CDD-IRF-SCALE(WS-NEW-ITER)
                                   AND CDD-OIF-SIGNED(WS-MATCH-OLD)
                                       = CDD-IRF-SIGNED(WS-NEW-ITER)
                                   MOVE 1 TO WS-TYPES-SAME
                               END-IF
                           WHEN OTHER
                               MOVE 1 TO WS-TYPES-SAME
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF.
