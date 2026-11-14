      * @file CDDC2IR.cbl
      * @brief COBOL CST UDT to Canonical IR and CST-link extractor.
      * @module CDDC2IR
      * @details Extracts 01 records, COBOL-2002+ TYPEDEFs, and
      *          CLASS-ID storage from CDD-CST into CDD-IR and
      *          CDD-IR-FIELD-MAP, mapping PIC/SIZE/USAGE/OCCURS,
      *          88 enums/booleans, REDEFINES/66 RENAMES, companion
      *          ODO/null helpers, and @json-name/@field docblocks.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDC2IR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-PIC-PARM Local CDD-CPIC-INFO for clause extraction.
       01  WS-PIC-PARM.
      * @field WS-PPI-TAG Magic type-safety tag ('CDDCPI01').
           05  WS-PPI-TAG                PIC X(8) VALUE "CDDCPI01".
      * @field WS-PPI-OP Operation code (2=clauses).
           05  WS-PPI-OP                 PIC 9(2) COMP VALUE 2.
      * @field WS-PPI-T1 Start token.
           05  WS-PPI-T1                 PIC 9(8) COMP VALUE 0.
      * @field WS-PPI-T2 End token.
           05  WS-PPI-T2                 PIC 9(8) COMP VALUE 0.
      * @field WS-PPI-PLEN PIC length.
           05  WS-PPI-PLEN               PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-PIN PIC text.
           05  WS-PPI-PIN                PIC X(32) VALUE SPACES.
      * @field WS-PPI-HASPIC Has PIC flag.
           05  WS-PPI-HASPIC             PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-KIND Scalar kind.
           05  WS-PPI-KIND               PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-SIGNED Signed flag.
           05  WS-PPI-SIGNED             PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-MAXLEN Max length.
           05  WS-PPI-MAXLEN             PIC 9(8) COMP VALUE 0.
      * @field WS-PPI-DIGITS Digits count.
           05  WS-PPI-DIGITS             PIC 9(4) COMP VALUE 0.
      * @field WS-PPI-SCALE Scale count.
           05  WS-PPI-SCALE              PIC 9(4) COMP VALUE 0.
      * @field WS-PPI-USAGE Usage code.
           05  WS-PPI-USAGE              PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-ARRMODE Array mode.
           05  WS-PPI-ARRMODE            PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-ARRMIN Array min.
           05  WS-PPI-ARRMIN             PIC 9(6) COMP VALUE 0.
      * @field WS-PPI-ARRMAX Array max.
           05  WS-PPI-ARRMAX             PIC 9(6) COMP VALUE 0.
      * @field WS-PPI-ISTDEF Typedef flag.
           05  WS-PPI-ISTDEF             PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-SYNCJ Sync/Just bitmask.
           05  WS-PPI-SYNCJ              PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-RLEN Redefines length.
           05  WS-PPI-RLEN               PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-REDEF Redefines target.
           05  WS-PPI-REDEF              PIC X(64) VALUE SPACES.
      * @field WS-PPI-VLEN Value length.
           05  WS-PPI-VLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-PPI-VSTR Value is string.
           05  WS-PPI-VSTR               PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-VTXT Value text.
           05  WS-PPI-VTXT               PIC X(64) VALUE SPACES.
      * @field WS-PPI-TT1 Type span start.
           05  WS-PPI-TT1                PIC 9(8) COMP VALUE 0.
      * @field WS-PPI-TT2 Type span end.
           05  WS-PPI-TT2                PIC 9(8) COMP VALUE 0.
      * @record WS-NAM-PARM Local CDD-NAM-REQ for C2J conversion.
       01  WS-NAM-PARM.
      * @field WS-NMP-TAG Magic type-safety tag ('CDDNAM01').
           05  WS-NMP-TAG                PIC X(8) VALUE "CDDNAM01".
      * @field WS-NMP-OP Operation (2=C2J).
           05  WS-NMP-OP                 PIC 9(2) COMP VALUE 2.
      * @field WS-NMP-DIALECT Dialect ID.
           05  WS-NMP-DIALECT            PIC 9(2) COMP VALUE 7.
      * @field WS-NMP-CASE Target JSON key case.
           05  WS-NMP-CASE               PIC 9(2) COMP VALUE 1.
      * @field WS-NMP-ILEN Input length.
           05  WS-NMP-ILEN               PIC 9(4) COMP VALUE 0.
      * @field WS-NMP-IN Input string.
           05  WS-NMP-IN                 PIC X(128) VALUE SPACES.
      * @field WS-NMP-OLEN Output length.
           05  WS-NMP-OLEN               PIC 9(4) COMP VALUE 0.
      * @field WS-NMP-OUT Output string.
           05  WS-NMP-OUT                PIC X(128) VALUE SPACES.
      * @record WS-EXP-QUEUE Non-recursive CST group expansion queue.
       01  WS-EXP-QUEUE.
      * @field WS-EQ-HEAD Queue read index.
           05  WS-EQ-HEAD                PIC 9(4) COMP VALUE 1.
      * @field WS-EQ-TAIL Queue write index.
           05  WS-EQ-TAIL                PIC 9(4) COMP VALUE 0.
      * @field WS-EQ-ITEM Queued (CST group node, IR parent) pair.
           05  WS-EQ-ITEM                OCCURS 512 TIMES.
      * @field WS-EQ-CST CST group/record node index.
               10  WS-EQ-CST             PIC 9(4) COMP.
      * @field WS-EQ-IR-PAR Target IR parent index (0=root).
               10  WS-EQ-IR-PAR          PIC 9(4) COMP.
      * @record WS-C2IR-STATE Extraction cursors and helper trackers.
       01  WS-C2IR-STATE.
      * @field WS-UDT-CNT Count of top-level UDTs found.
           05  WS-UDT-CNT                PIC 9(4) COMP VALUE 0.
      * @field WS-SEL-CST Selected UDT CST node index.
           05  WS-SEL-CST                PIC 9(4) COMP VALUE 0.
      * @field WS-DATA-ROOT-CST CST node whose children hold fields.
           05  WS-DATA-ROOT-CST          PIC 9(4) COMP VALUE 0.
      * @field WS-ITER CST sibling iterator.
           05  WS-ITER                   PIC 9(4) COMP VALUE 0.
      * @field WS-SUB-ITER Nested CST child iterator.
           05  WS-SUB-ITER               PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-CST-GRP Current popped CST group node.
           05  WS-CUR-CST-GRP            PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-IR-PAR Current popped IR parent index.
           05  WS-CUR-IR-PAR             PIC 9(4) COMP VALUE 0.
      * @field WS-IR-IDX Newly allocated IR field index.
           05  WS-IR-IDX                 PIC 9(4) COMP VALUE 0.
      * @field WS-LAST-IR-IN-GRP Previous IR sibling in current group.
           05  WS-LAST-IR-IN-GRP         PIC 9(4) COMP VALUE 0.
      * @field WS-PEND-CNT-CST Pending *-COUNT/-CAP companion CST node.
           05  WS-PEND-CNT-CST           PIC 9(4) COMP VALUE 0.
      * @field WS-IS-COMPANION 1 if field is hidden ODO/null helper.
           05  WS-IS-COMPANION           PIC 9(2) COMP VALUE 0.
      * @field WS-HAS-SUB-FLD 1 if CST node has child CST-FIELD nodes.
           05  WS-HAS-SUB-FLD            PIC 9(2) COMP VALUE 0.
      * @field WS-EIDX Newly allocated CDD-IR-ENUM index.
           05  WS-EIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-NLEN Scratch name length.
           05  WS-NLEN                   PIC 9(2) COMP VALUE 0.
      * @field WS-MAX-ACC Computed integer max bound (9..999999999).
           05  WS-MAX-ACC                PIC S9(12) COMP VALUE 0.
      * @field WS-D-I Digit loop counter.
           05  WS-D-I                    PIC 9(2) COMP VALUE 0.
      * @record WS-UP-TARGET Uppercased target UDT name.
       01  WS-UP-TARGET                  PIC X(64) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-diff.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDC2IR Extract COBOL UDT from CDD-CST into CDD-IR.
      * @param [inout] CDD-IR-MAP-REQ Target UDT options & matched CST.
      * @param [in] CDD-BUF Raw COBOL source buffer.
      * @param [in] CDD-CTOK-STREAM Token + trivia stream.
      * @param [in] CDD-CST Lossless Concrete Syntax Tree.
      * @param [inout] CDD-IR Output Canonical IR container.
      * @param [inout] CDD-IR-FIELD-MAP Output IR->CST back-link map.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-IR and CDD-IR-FIELD-MAP.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-SYNC-UDT-NOT-FOUND,
      *         CDD-ERR-SYNC-AMBIGUOUS-UDT, CDD-ERR-IR-CAPACITY.
       PROCEDURE DIVISION USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
           CDD-CST CDD-IR CDD-IR-FIELD-MAP CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, locate UDT, extract tree.
       MAIN-ENTRY.
           IF NOT CDD-MPR-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               OR NOT CDD-CTK-TAG-VALID
               OR NOT CDD-CST-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               OR NOT CDD-FMP-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDC2IR" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDC2IR"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-MPR-MATCHED-CST
           PERFORM RESET-C2IR-OUTPUT
           PERFORM LOCATE-TARGET-UDT
           IF CDD-OK
               PERFORM POPULATE-ROOT-UDT-META
               PERFORM EXPAND-UDT-SUBTREE
           END-IF
           EXIT PROGRAM.
      * @paragraph RESET-C2IR-OUTPUT Clear CDD-IR before extraction.
       RESET-C2IR-OUTPUT.
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
      * @paragraph LOCATE-TARGET-UDT Match --udt or single UDT in CST.
       LOCATE-TARGET-UDT.
           MOVE 0 TO WS-UDT-CNT
           MOVE 0 TO WS-SEL-CST
           MOVE SPACES TO WS-UP-TARGET
           IF CDD-MPR-NAME-LEN > 0 AND CDD-MPR-NAME-LEN <= 64
               MOVE CDD-MPR-TARGET-NAME(1:CDD-MPR-NAME-LEN)
                   TO WS-UP-TARGET(1:CDD-MPR-NAME-LEN)
               INSPECT WS-UP-TARGET(1:CDD-MPR-NAME-LEN)
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-IF
           IF CDD-CST-ROOT > 0
               MOVE CDD-CSN-FIRST-CHILD(CDD-CST-ROOT) TO WS-ITER
               PERFORM UNTIL WS-ITER = 0
                   IF CDD-CSN-IS-RECORD(WS-ITER)
                       OR CDD-CSN-IS-TYPEDEF(WS-ITER)
                       OR CDD-CSN-IS-CLASS(WS-ITER)
                       ADD 1 TO WS-UDT-CNT
                       END-ADD
                       IF CDD-MPR-NAME-LEN > 0
                           IF CDD-CSN-NAME-LEN(WS-ITER)
                               = CDD-MPR-NAME-LEN
                               AND CDD-CSN-NAME(WS-ITER)
                                   = WS-UP-TARGET
                               MOVE WS-ITER TO WS-SEL-CST
                           END-IF
                       ELSE
                           IF WS-SEL-CST = 0
                               MOVE WS-ITER TO WS-SEL-CST
                           END-IF
                       END-IF
                   END-IF
                   MOVE CDD-CSN-NEXT-SIB(WS-ITER) TO WS-ITER
               END-PERFORM
           END-IF
           IF WS-SEL-CST = 0
               MOVE 4004 TO CDD-ERR-CODE
               MOVE "CDDC2IR" TO CDD-ERR-SUBPROG
               MOVE "Target COBOL UDT not found in source"
                   TO CDD-ERR-MESSAGE
           ELSE
               IF CDD-MPR-NAME-LEN = 0
                   AND CDD-MPR-STRICT-UDT = 1
                   AND WS-UDT-CNT > 1
                   MOVE 4005 TO CDD-ERR-CODE
                   MOVE "CDDC2IR" TO CDD-ERR-SUBPROG
                   MOVE "Multiple UDTs present; specify --udt <NAME>"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE WS-SEL-CST TO CDD-MPR-MATCHED-CST
               END-IF
           END-IF.
      * @paragraph POPULATE-ROOT-UDT-META Set UDT kind, name, and doc.
       POPULATE-ROOT-UDT-META.
           EVALUATE TRUE
               WHEN CDD-CSN-IS-TYPEDEF(WS-SEL-CST)
                   MOVE 2 TO CDD-IR-UDT-KIND
               WHEN CDD-CSN-IS-CLASS(WS-SEL-CST)
                   MOVE 3 TO CDD-IR-UDT-KIND
               WHEN OTHER
                   MOVE 1 TO CDD-IR-UDT-KIND
           END-EVALUATE
           MOVE CDD-CSN-NAME-LEN(WS-SEL-CST) TO CDD-IR-COB-NLEN
           MOVE CDD-CSN-NAME(WS-SEL-CST) TO CDD-IR-COB-NAME
           IF CDD-CSN-JKEY-LEN(WS-SEL-CST) > 0
               MOVE CDD-CSN-JKEY-LEN(WS-SEL-CST) TO CDD-IR-TITLE-LEN
               MOVE CDD-CSN-JSON-KEY(WS-SEL-CST) TO CDD-IR-JSON-TITLE
           ELSE
               MOVE CDD-CSN-NAME-LEN(WS-SEL-CST) TO CDD-IR-TITLE-LEN
               MOVE CDD-CSN-NAME(WS-SEL-CST)
                   TO CDD-IR-JSON-TITLE(1:64)
           END-IF
           IF CDD-CSN-DOC-LEN(WS-SEL-CST) > 0
               MOVE CDD-CSN-DOC-LEN(WS-SEL-CST) TO CDD-IR-DESC-LEN
               MOVE CDD-CSN-DOC-TEXT(WS-SEL-CST)
                   TO CDD-IR-DESC(1:160)
           END-IF
           MOVE WS-SEL-CST TO WS-DATA-ROOT-CST
           IF CDD-CSN-IS-CLASS(WS-SEL-CST)
               MOVE CDD-CSN-FIRST-CHILD(WS-SEL-CST) TO WS-ITER
               PERFORM UNTIL WS-ITER = 0
                   IF CDD-CSN-IS-RECORD(WS-ITER)
                       AND CDD-CSN-FIRST-CHILD(WS-ITER) > 0
                       MOVE WS-ITER TO WS-DATA-ROOT-CST
                       MOVE WS-ITER TO CDD-MPR-MATCHED-CST
                       EXIT PERFORM
                   END-IF
                   MOVE CDD-CSN-NEXT-SIB(WS-ITER) TO WS-ITER
               END-PERFORM
           END-IF.
      * @paragraph EXPAND-UDT-SUBTREE Walk CST field children -> IR.
       EXPAND-UDT-SUBTREE.
           IF CDD-CSN-FIRST-CHILD(WS-DATA-ROOT-CST) = 0
               AND NOT CDD-CSN-IS-CLASS(WS-DATA-ROOT-CST)
               MOVE WS-DATA-ROOT-CST TO WS-ITER
               MOVE 0 TO WS-CUR-IR-PAR
               MOVE 0 TO WS-LAST-IR-IN-GRP
               MOVE 0 TO WS-PEND-CNT-CST
               PERFORM EXTRACT-ONE-CST-FIELD
           ELSE
               MOVE 1 TO WS-EQ-HEAD
               MOVE 1 TO WS-EQ-TAIL
               MOVE WS-DATA-ROOT-CST TO WS-EQ-CST(1)
               MOVE 0 TO WS-EQ-IR-PAR(1)
               PERFORM UNTIL WS-EQ-HEAD > WS-EQ-TAIL OR NOT CDD-OK
                   MOVE WS-EQ-CST(WS-EQ-HEAD) TO WS-CUR-CST-GRP
                   MOVE WS-EQ-IR-PAR(WS-EQ-HEAD) TO WS-CUR-IR-PAR
                   ADD 1 TO WS-EQ-HEAD
                   END-ADD
                   MOVE 0 TO WS-LAST-IR-IN-GRP
                   MOVE 0 TO WS-PEND-CNT-CST
                   MOVE CDD-CSN-FIRST-CHILD(WS-CUR-CST-GRP) TO WS-ITER
                   PERFORM UNTIL WS-ITER = 0 OR NOT CDD-OK
                       IF CDD-CSN-IS-FIELD(WS-ITER)
                           OR CDD-CSN-IS-REN66(WS-ITER)
                           OR CDD-CSN-IS-RECORD(WS-ITER)
                           PERFORM EXTRACT-ONE-CST-FIELD
                       END-IF
                       MOVE CDD-CSN-NEXT-SIB(WS-ITER) TO WS-ITER
                   END-PERFORM
               END-PERFORM
           END-IF.
      * @paragraph EXTRACT-ONE-CST-FIELD Parse field clauses & map IR.
       EXTRACT-ONE-CST-FIELD.
           MOVE "CDDCPI01" TO WS-PPI-TAG
           MOVE 2 TO WS-PPI-OP
           MOVE CDD-CSN-CODE-START(WS-ITER) TO WS-PPI-T1
           MOVE CDD-CSN-CODE-END(WS-ITER) TO WS-PPI-T2
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM WS-PIC-PARM
               CDD-ERROR
           END-CALL
           IF CDD-OK
               PERFORM CHECK-COMPANION-FIELD
               IF WS-IS-COMPANION = 0
                   PERFORM ALLOC-AND-FILL-IR-FIELD
               END-IF
           END-IF.
      * @paragraph CHECK-COMPANION-FIELD Detect *-IS-NULL or ODO *-COUNT.
       CHECK-COMPANION-FIELD.
           MOVE 0 TO WS-IS-COMPANION
           MOVE CDD-CSN-NAME-LEN(WS-ITER) TO WS-NLEN
           IF CDD-CSN-KEEP-FLAG(WS-ITER) = 0
               AND CDD-CSN-JKEY-LEN(WS-ITER) = 0
               IF WS-NLEN > 8
                   AND CDD-CSN-NAME(WS-ITER)(WS-NLEN - 7:8)
                       = "-IS-NULL"
                   AND WS-LAST-IR-IN-GRP > 0
                   MOVE 1 TO CDD-IRF-NULLABLE(WS-LAST-IR-IN-GRP)
                   MOVE 0 TO CDD-IRF-REQUIRED(WS-LAST-IR-IN-GRP)
                   MOVE WS-ITER
                       TO CDD-FMP-COMP-CST(WS-LAST-IR-IN-GRP)
                   MOVE 1 TO WS-IS-COMPANION
               ELSE
                   IF WS-NLEN > 6
                       AND CDD-CSN-NAME(WS-ITER)(WS-NLEN - 5:6)
                           = "-COUNT"
                       AND CDD-CSN-NEXT-SIB(WS-ITER) > 0
                       MOVE WS-ITER TO WS-PEND-CNT-CST
                       MOVE 1 TO WS-IS-COMPANION
                   END-IF
               END-IF
           END-IF.
      * @paragraph ALLOC-AND-FILL-IR-FIELD Create IR field + FMP entry.
       ALLOC-AND-FILL-IR-FIELD.
           IF CDD-IR-FLD-COUNT >= CDD-IR-FLD-CAP
               MOVE 3001 TO CDD-ERR-CODE
               MOVE "CDDC2IR" TO CDD-ERR-SUBPROG
               MOVE "IR field pool exhausted in CDDC2IR"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-IR-FLD-COUNT
               END-ADD
               MOVE CDD-IR-FLD-COUNT TO WS-IR-IDX
               MOVE WS-ITER TO CDD-FMP-CST-NODE(WS-IR-IDX)
               MOVE WS-PPI-TT1 TO CDD-FMP-TYPE-TOK1(WS-IR-IDX)
               MOVE WS-PPI-TT2 TO CDD-FMP-TYPE-TOK2(WS-IR-IDX)
               MOVE 0 TO CDD-FMP-COMP-CST(WS-IR-IDX)
               IF WS-PEND-CNT-CST > 0 AND WS-PPI-ARRMODE > 0
                   MOVE WS-PEND-CNT-CST TO CDD-FMP-COMP-CST(WS-IR-IDX)
                   MOVE 0 TO WS-PEND-CNT-CST
               END-IF
               PERFORM LINK-IR-FIELD-TO-PARENT
               PERFORM POPULATE-IR-FIELD-ATTRS
               PERFORM EXTRACT-88-CHILDREN
               MOVE WS-IR-IDX TO WS-LAST-IR-IN-GRP
           END-IF.
      * @paragraph LINK-IR-FIELD-TO-PARENT Attach WS-IR-IDX in tree.
       LINK-IR-FIELD-TO-PARENT.
           MOVE WS-CUR-IR-PAR TO CDD-IRF-PARENT(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-FIRST-CHILD(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-LAST-CHILD(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-NEXT-SIB(WS-IR-IDX)
           IF WS-CUR-IR-PAR = 0
               IF CDD-IR-ROOT-FIRST = 0
                   MOVE WS-IR-IDX TO CDD-IR-ROOT-FIRST
                   MOVE WS-IR-IDX TO CDD-IR-ROOT-LAST
               ELSE
                   MOVE WS-IR-IDX
                       TO CDD-IRF-NEXT-SIB(CDD-IR-ROOT-LAST)
                   MOVE WS-IR-IDX TO CDD-IR-ROOT-LAST
               END-IF
           ELSE
               IF CDD-IRF-FIRST-CHILD(WS-CUR-IR-PAR) = 0
                   MOVE WS-IR-IDX
                       TO CDD-IRF-FIRST-CHILD(WS-CUR-IR-PAR)
                   MOVE WS-IR-IDX
                       TO CDD-IRF-LAST-CHILD(WS-CUR-IR-PAR)
               ELSE
                   MOVE WS-IR-IDX TO CDD-IRF-NEXT-SIB(
                       CDD-IRF-LAST-CHILD(WS-CUR-IR-PAR))
                   MOVE WS-IR-IDX
                       TO CDD-IRF-LAST-CHILD(WS-CUR-IR-PAR)
               END-IF
           END-IF.
      * @paragraph POPULATE-IR-FIELD-ATTRS Copy names, PIC, bounds, doc.
       POPULATE-IR-FIELD-ATTRS.
           MOVE CDD-CSN-NAME-LEN(WS-ITER) TO CDD-IRF-CNAM-LEN(WS-IR-IDX)
           MOVE CDD-CSN-NAME(WS-ITER) TO CDD-IRF-COB-NAME(WS-IR-IDX)
           IF CDD-CSN-JKEY-LEN(WS-ITER) > 0
               MOVE CDD-CSN-JKEY-LEN(WS-ITER)
                   TO CDD-IRF-JKEY-LEN(WS-IR-IDX)
               MOVE CDD-CSN-JSON-KEY(WS-ITER)
                   TO CDD-IRF-JSON-KEY(WS-IR-IDX)
           ELSE
               MOVE "CDDNAM01" TO WS-NMP-TAG
               MOVE 2 TO WS-NMP-OP
               MOVE CDD-MPR-DIALECT TO WS-NMP-DIALECT
               MOVE CDD-MPR-CASE TO WS-NMP-CASE
               MOVE CDD-CSN-NAME-LEN(WS-ITER) TO WS-NMP-ILEN
               MOVE CDD-CSN-NAME(WS-ITER) TO WS-NMP-IN
               CALL "CDDNAM" USING WS-NAM-PARM CDD-ERROR
               END-CALL
               MOVE WS-NMP-OLEN TO CDD-IRF-JKEY-LEN(WS-IR-IDX)
               MOVE WS-NMP-OUT TO CDD-IRF-JSON-KEY(WS-IR-IDX)
           END-IF
           MOVE CDD-CSN-DOC-LEN(WS-ITER) TO CDD-IRF-DESC-LEN(WS-IR-IDX)
           MOVE CDD-CSN-DOC-TEXT(WS-ITER) TO CDD-IRF-DESC(WS-IR-IDX)
           MOVE CDD-CSN-KEEP-FLAG(WS-ITER)
               TO CDD-IRF-KEEP-HELPER(WS-IR-IDX)
           MOVE CDD-CSN-LEVEL(WS-ITER) TO CDD-IRF-COB-LEVEL(WS-IR-IDX)
           MOVE 1 TO CDD-IRF-REQUIRED(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-NULLABLE(WS-IR-IDX)
           MOVE WS-PPI-SIGNED TO CDD-IRF-SIGNED(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-MIN-LEN(WS-IR-IDX)
           MOVE WS-PPI-MAXLEN TO CDD-IRF-MAX-LEN(WS-IR-IDX)
           MOVE WS-PPI-DIGITS TO CDD-IRF-DIGITS(WS-IR-IDX)
           MOVE WS-PPI-SCALE TO CDD-IRF-SCALE(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-HAS-BOUNDS(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-MIN-VAL(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-MAX-VAL(WS-IR-IDX)
           MOVE SPACES TO CDD-IRF-FORMAT(WS-IR-IDX)
           MOVE WS-PPI-USAGE TO CDD-IRF-USAGE(WS-IR-IDX)
           MOVE WS-PPI-PLEN TO CDD-IRF-PIC-LEN(WS-IR-IDX)
           MOVE WS-PPI-PIN TO CDD-IRF-PIC-TEXT(WS-IR-IDX)
           MOVE WS-PPI-RLEN TO CDD-IRF-REDEF-LEN(WS-IR-IDX)
           MOVE WS-PPI-REDEF TO CDD-IRF-REDEFINES(WS-IR-IDX)
           MOVE WS-PPI-SYNCJ TO CDD-IRF-SYNC-JUST(WS-IR-IDX)
           MOVE WS-PPI-VLEN TO CDD-IRF-DEF-LEN(WS-IR-IDX)
           MOVE WS-PPI-VTXT TO CDD-IRF-DEF-VAL(WS-IR-IDX)
           MOVE WS-PPI-ARRMODE TO CDD-IRF-ARR-MODE(WS-IR-IDX)
           MOVE WS-PPI-ARRMIN TO CDD-IRF-ARR-MIN(WS-IR-IDX)
           MOVE WS-PPI-ARRMAX TO CDD-IRF-ARR-MAX(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-ENUM-FIRST(WS-IR-IDX)
           MOVE 0 TO CDD-IRF-ENUM-COUNT(WS-IR-IDX)
           PERFORM CHECK-HAS-SUB-FIELDS
           IF WS-HAS-SUB-FLD = 1 AND WS-PPI-HASPIC = 0
               MOVE 1 TO CDD-IRF-KIND(WS-IR-IDX)
               ADD 1 TO WS-EQ-TAIL
               END-ADD
               MOVE WS-ITER TO WS-EQ-CST(WS-EQ-TAIL)
               MOVE WS-IR-IDX TO WS-EQ-IR-PAR(WS-EQ-TAIL)
           ELSE
               IF WS-PPI-HASPIC = 1
                   MOVE WS-PPI-KIND TO CDD-IRF-KIND(WS-IR-IDX)
               ELSE
                   MOVE 2 TO CDD-IRF-KIND(WS-IR-IDX)
               END-IF
               PERFORM COMPUTE-INT-BOUNDS
           END-IF.
      * @paragraph CHECK-HAS-SUB-FIELDS Check for child CST-FIELD nodes.
       CHECK-HAS-SUB-FIELDS.
           MOVE 0 TO WS-HAS-SUB-FLD
           MOVE CDD-CSN-FIRST-CHILD(WS-ITER) TO WS-SUB-ITER
           PERFORM UNTIL WS-SUB-ITER = 0
               IF CDD-CSN-IS-FIELD(WS-SUB-ITER)
                   MOVE 1 TO WS-HAS-SUB-FLD
               END-IF
               MOVE CDD-CSN-NEXT-SIB(WS-SUB-ITER) TO WS-SUB-ITER
           END-PERFORM.
      * @paragraph COMPUTE-INT-BOUNDS Populate min/max on integer PICs.
       COMPUTE-INT-BOUNDS.
           IF CDD-IRF-IS-INTEGER(WS-IR-IDX)
               AND CDD-IRF-DIGITS(WS-IR-IDX) >= 1
               AND CDD-IRF-DIGITS(WS-IR-IDX) <= 9
               MOVE 0 TO WS-MAX-ACC
               MOVE 1 TO WS-D-I
               PERFORM UNTIL WS-D-I > CDD-IRF-DIGITS(WS-IR-IDX)
                   COMPUTE WS-MAX-ACC = (WS-MAX-ACC * 10) + 9
                   END-COMPUTE
                   ADD 1 TO WS-D-I
                   END-ADD
               END-PERFORM
               MOVE 1 TO CDD-IRF-HAS-BOUNDS(WS-IR-IDX)
               MOVE WS-MAX-ACC TO CDD-IRF-MAX-VAL(WS-IR-IDX)
               IF CDD-IRF-IS-SIGNED(WS-IR-IDX)
                   COMPUTE CDD-IRF-MIN-VAL(WS-IR-IDX) =
                       0 - WS-MAX-ACC
                   END-COMPUTE
               ELSE
                   MOVE 0 TO CDD-IRF-MIN-VAL(WS-IR-IDX)
               END-IF
           END-IF.
      * @paragraph EXTRACT-88-CHILDREN Harvest 88 enums & detect bools.
       EXTRACT-88-CHILDREN.
           MOVE CDD-CSN-FIRST-CHILD(WS-ITER) TO WS-SUB-ITER
           PERFORM UNTIL WS-SUB-ITER = 0 OR NOT CDD-OK
               IF CDD-CSN-IS-COND88(WS-SUB-ITER)
                   MOVE "CDDCPI01" TO WS-PPI-TAG
                   MOVE 2 TO WS-PPI-OP
                   MOVE CDD-CSN-CODE-START(WS-SUB-ITER) TO WS-PPI-T1
                   MOVE CDD-CSN-CODE-END(WS-SUB-ITER) TO WS-PPI-T2
                   CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM
                       WS-PIC-PARM CDD-ERROR
                   END-CALL
                   IF CDD-IR-ENUM-CNT >= CDD-IR-ENUM-CAP
                       MOVE 3001 TO CDD-ERR-CODE
                       MOVE "CDDC2IR" TO CDD-ERR-SUBPROG
                       MOVE "IR enum pool exhausted in CDDC2IR"
                           TO CDD-ERR-MESSAGE
                   ELSE
                       ADD 1 TO CDD-IR-ENUM-CNT
                       END-ADD
                       MOVE CDD-IR-ENUM-CNT TO WS-EIDX
                       IF CDD-IRF-ENUM-FIRST(WS-IR-IDX) = 0
                           MOVE WS-EIDX
                               TO CDD-IRF-ENUM-FIRST(WS-IR-IDX)
                       END-IF
                       ADD 1 TO CDD-IRF-ENUM-COUNT(WS-IR-IDX)
                       END-ADD
                       MOVE CDD-CSN-NAME-LEN(WS-SUB-ITER)
                           TO CDD-IRE-CNAM-LEN(WS-EIDX)
                       MOVE CDD-CSN-NAME(WS-SUB-ITER)
                           TO CDD-IRE-COB-NAME(WS-EIDX)
                       MOVE WS-PPI-VLEN TO CDD-IRE-VAL-LEN(WS-EIDX)
                       MOVE WS-PPI-VTXT TO CDD-IRE-VAL(WS-EIDX)
                       MOVE WS-PPI-VSTR TO CDD-IRE-IS-STR(WS-EIDX)
                   END-IF
               END-IF
               MOVE CDD-CSN-NEXT-SIB(WS-SUB-ITER) TO WS-SUB-ITER
           END-PERFORM
           IF CDD-OK
               AND CDD-IRF-IS-STRING(WS-IR-IDX)
               AND CDD-IRF-MAX-LEN(WS-IR-IDX) = 1
               AND CDD-IRF-ENUM-COUNT(WS-IR-IDX) = 2
               MOVE CDD-IRF-ENUM-FIRST(WS-IR-IDX) TO WS-EIDX
               IF (CDD-IRE-VAL(WS-EIDX)(1:1) = "T"
                   AND CDD-IRE-VAL(WS-EIDX + 1)(1:1) = "F")
                   OR (CDD-IRE-VAL(WS-EIDX)(1:1) = "Y"
                   AND CDD-IRE-VAL(WS-EIDX + 1)(1:1) = "N")
                   MOVE 9 TO CDD-IRF-KIND(WS-IR-IDX)
                   MOVE 0 TO CDD-IRF-ENUM-COUNT(WS-IR-IDX)
               END-IF
           END-IF.
