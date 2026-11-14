      * @file CDDCSTB.cbl
      * @brief Lossless Red/Green COBOL CST builder across 10 dialects.
      * @module CDDCSTB
      * @details Partitions CDD-CTOK-STREAM into a lossless tree of
      *          opaque chunks, 01 records, COBOL-2002+ TYPEDEFs and
      *          CLASS-IDs, hierarchical 02-49/66/77/88 fields, and
      *          Procedure Division routines with exact leading and
      *          trailing trivia attachment.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCSTB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-PIC-PARM Local CDD-CPIC-INFO for TYPEDEF detection.
       01  WS-PIC-PARM.
      * @field WS-PPI-TAG Magic type-safety tag ('CDDCPI01').
           05  WS-PPI-TAG                PIC X(8) VALUE "CDDCPI01".
      * @field WS-PPI-OP Operation code (2=scan clauses).
           05  WS-PPI-OP                 PIC 9(2) COMP VALUE 2.
      * @field WS-PPI-T1 Start token index.
           05  WS-PPI-T1                 PIC 9(8) COMP VALUE 0.
      * @field WS-PPI-T2 End token index.
           05  WS-PPI-T2                 PIC 9(8) COMP VALUE 0.
      * @field WS-PPI-PLEN PIC input length.
           05  WS-PPI-PLEN               PIC 9(2) COMP VALUE 0.
      * @field WS-PPI-PIN PIC input string.
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
      * @field WS-PPI-ISTDEF 1 if IS TYPEDEF present.
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
      * @record WS-LVL-STACK Open Data Division hierarchy stack.
       01  WS-LVL-STACK.
      * @field WS-LSTK-DEPTH Active data hierarchy depth (0..16).
           05  WS-LSTK-DEPTH             PIC 9(2) COMP VALUE 0.
      * @field WS-LSTK-ENTRY Hierarchy stack slot (levels 01..49).
           05  WS-LSTK-ENTRY             OCCURS 50 TIMES.
      * @field WS-LSTK-NODE CST node index at this level.
               10  WS-LSTK-NODE          PIC 9(4) COMP.
      * @field WS-LSTK-LVL COBOL level number (01..49).
               10  WS-LSTK-LVL           PIC 9(2) COMP.
      * @record WS-BLD-STATE CST builder cursors and parser state.
       01  WS-BLD-STATE.
      * @field WS-UNCLAIMED First token index not yet owned by a node.
           05  WS-UNCLAIMED              PIC 9(8) COMP VALUE 1.
      * @field WS-TI Primary token scan cursor.
           05  WS-TI                     PIC 9(8) COMP VALUE 1.
      * @field WS-STMT-C1 First semantic token of current statement.
           05  WS-STMT-C1                PIC 9(8) COMP VALUE 0.
      * @field WS-STMT-C2 Last semantic token of current statement.
           05  WS-STMT-C2                PIC 9(8) COMP VALUE 0.
      * @field WS-STMT-OWN-END End of trailing trivia on C2's line.
           05  WS-STMT-OWN-END           PIC 9(8) COMP VALUE 0.
      * @field WS-LEAD-T1 Computed leading-trivia start for statement.
           05  WS-LEAD-T1                PIC 9(8) COMP VALUE 0.
      * @field WS-DIV-MODE Active division (1=ID,2=ENV,3=DATA,4=PROC).
           05  WS-DIV-MODE               PIC 9(2) COMP VALUE 3.
      * @field WS-DATA-SEC-OK 1 in WS/LOCAL/LINKAGE/CONSTANT/bare cpy.
           05  WS-DATA-SEC-OK            PIC 9(2) COMP VALUE 1.
      * @field WS-CLASS-NODE Open CLASS-ID CST node (0=none).
           05  WS-CLASS-NODE             PIC 9(4) COMP VALUE 0.
      * @field WS-RTN-NODE Open PROCEDURE routine CST node (0=none).
           05  WS-RTN-NODE               PIC 9(4) COMP VALUE 0.
      * @field WS-RTN-IS-SEC 1 if open routine is a SECTION.
           05  WS-RTN-IS-SEC             PIC 9(2) COMP VALUE 0.
      * @field WS-NEW-NODE Newly allocated CST node index.
           05  WS-NEW-NODE               PIC 9(4) COMP VALUE 0.
      * @field WS-PARENT-NODE Target parent CST node index.
           05  WS-PARENT-NODE            PIC 9(4) COMP VALUE 0.
      * @field WS-PREV-SIB Previous last-child CST node.
           05  WS-PREV-SIB               PIC 9(4) COMP VALUE 0.
      * @field WS-N2-IDX Second semantic token index in statement.
           05  WS-N2-IDX                 PIC 9(8) COMP VALUE 0.
      * @field WS-N3-IDX Third semantic token index in statement.
           05  WS-N3-IDX                 PIC 9(8) COMP VALUE 0.
      * @field WS-LVL-NUM Parsed numeric level on TOK-LEVEL.
           05  WS-LVL-NUM                PIC 9(2) COMP VALUE 0.
      * @field WS-WLEN Scratch token length.
           05  WS-WLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-BACK-I Backward trivia scan cursor.
           05  WS-BACK-I                 PIC 9(8) COMP VALUE 0.
      * @field WS-LINE-ANCHOR Line number during trivia scan.
           05  WS-LINE-ANCHOR            PIC 9(8) COMP VALUE 0.
      * @field WS-CMT-SCAN Cursor for doc tag extraction in trivia.
           05  WS-CMT-SCAN               PIC 9(8) COMP VALUE 0.
      * @field WS-BYTE-I Character cursor inside comment slice.
           05  WS-BYTE-I                 PIC 9(8) COMP VALUE 0.
      * @field WS-BYTE-END End offset of comment slice.
           05  WS-BYTE-END               PIC 9(8) COMP VALUE 0.
      * @record WS-W1 First semantic word of statement (uppercased).
       01  WS-W1                         PIC X(64) VALUE SPACES.
      * @record WS-W2 Second semantic word of statement (uppercased).
       01  WS-W2                         PIC X(64) VALUE SPACES.
      * @record WS-W3 Third semantic word of statement (uppercased).
       01  WS-W3                         PIC X(64) VALUE SPACES.
      * @record WS-CMT-LINE Extracted comment line scratch buffer.
       01  WS-CMT-LINE                   PIC X(256) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDCSTB Build lossless CST over CDD-CTOK-STREAM.
      * @param [in] CDD-BUF Raw COBOL source buffer.
      * @param [in] CDD-CTOK-STREAM Contiguous token + trivia stream.
      * @param [inout] CDD-CST Output lossless Concrete Syntax Tree.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-CST partitioning 1..CDD-CTK-COUNT.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-COB-LEVEL-HIERARCHY,
      *         CDD-ERR-COB-CST-CAPACITY.
       PROCEDURE DIVISION USING CDD-BUF CDD-CTOK-STREAM CDD-CST
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate envelopes and construct CST.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID
               OR NOT CDD-CTK-TAG-VALID
               OR NOT CDD-CST-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCSTB" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDCSTB"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-CST-ROOT
           IF CDD-OK AND CDD-CTK-COUNT > 0
               PERFORM PARSE-TOKEN-STREAM
           END-IF
           IF CDD-OK AND CDD-CTK-COUNT > 0
               PERFORM FINALIZE-OPEN-NODES
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-CST-ROOT Initialize root node 1 and state.
       INIT-CST-ROOT.
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-CST-COUNT
           MOVE 0 TO CDD-CST-ROOT
           MOVE 0 TO CDD-CST-PATCH-LEN
           MOVE 0 TO WS-LSTK-DEPTH
           MOVE 3 TO WS-DIV-MODE
           MOVE 1 TO WS-DATA-SEC-OK
           MOVE 0 TO WS-CLASS-NODE
           MOVE 0 TO WS-RTN-NODE
           MOVE 0 TO WS-RTN-IS-SEC
           MOVE 1 TO WS-UNCLAIMED
           MOVE 1 TO WS-TI
           IF CDD-CST-CAP < 1
               MOVE 2011 TO CDD-ERR-CODE
               MOVE "CDDCSTB" TO CDD-ERR-SUBPROG
               MOVE "CST node pool capacity is zero"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE 1 TO CDD-CST-COUNT
               MOVE 1 TO CDD-CST-ROOT
               MOVE 1 TO CDD-CSN-KIND(1)
               MOVE 0 TO CDD-CSN-PARENT(1)
               MOVE 0 TO CDD-CSN-FIRST-CHILD(1)
               MOVE 0 TO CDD-CSN-LAST-CHILD(1)
               MOVE 0 TO CDD-CSN-NEXT-SIB(1)
               MOVE 1 TO CDD-CSN-TOK-START(1)
               MOVE 0 TO CDD-CSN-LEAD-END(1)
               MOVE 0 TO CDD-CSN-CODE-START(1)
               MOVE 0 TO CDD-CSN-CODE-END(1)
               MOVE 0 TO CDD-CSN-OWN-END(1)
               MOVE 0 TO CDD-CSN-FOOT-START(1)
               MOVE CDD-CTK-COUNT TO CDD-CSN-TOK-END(1)
               MOVE 0 TO CDD-CSN-LEVEL(1)
               MOVE 0 TO CDD-CSN-NAME-LEN(1)
               MOVE SPACES TO CDD-CSN-NAME(1)
               MOVE 0 TO CDD-CSN-JKEY-LEN(1)
               MOVE SPACES TO CDD-CSN-JSON-KEY(1)
               MOVE 0 TO CDD-CSN-DOC-LEN(1)
               MOVE SPACES TO CDD-CSN-DOC-TEXT(1)
               MOVE 0 TO CDD-CSN-KEEP-FLAG(1)
               MOVE 0 TO CDD-CSN-PATCH-MODE(1)
               MOVE 0 TO CDD-CSN-SPLICE-T1(1)
               MOVE 0 TO CDD-CSN-SPLICE-T2(1)
               MOVE 0 TO CDD-CSN-PATCH-OFF(1)
               MOVE 0 TO CDD-CSN-PATCH-LEN(1)
           END-IF.
      * @paragraph ALLOC-CST-NODE Allocate node under WS-PARENT-NODE.
       ALLOC-CST-NODE.
           IF CDD-CST-COUNT >= CDD-CST-CAP
               MOVE 2011 TO CDD-ERR-CODE
               MOVE "CDDCSTB" TO CDD-ERR-SUBPROG
               MOVE "CST node pool capacity exhausted"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-CST-COUNT
               END-ADD
               MOVE CDD-CST-COUNT TO WS-NEW-NODE
               MOVE 2 TO CDD-CSN-KIND(WS-NEW-NODE)
               MOVE WS-PARENT-NODE TO CDD-CSN-PARENT(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-FIRST-CHILD(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-LAST-CHILD(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-NEXT-SIB(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-TOK-START(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-LEAD-END(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-CODE-START(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-CODE-END(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-OWN-END(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-FOOT-START(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-TOK-END(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-LEVEL(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-NAME-LEN(WS-NEW-NODE)
               MOVE SPACES TO CDD-CSN-NAME(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-JKEY-LEN(WS-NEW-NODE)
               MOVE SPACES TO CDD-CSN-JSON-KEY(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-DOC-LEN(WS-NEW-NODE)
               MOVE SPACES TO CDD-CSN-DOC-TEXT(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-KEEP-FLAG(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-PATCH-MODE(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-SPLICE-T1(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-SPLICE-T2(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-PATCH-OFF(WS-NEW-NODE)
               MOVE 0 TO CDD-CSN-PATCH-LEN(WS-NEW-NODE)
               IF CDD-CSN-FIRST-CHILD(WS-PARENT-NODE) = 0
                   MOVE WS-NEW-NODE
                       TO CDD-CSN-FIRST-CHILD(WS-PARENT-NODE)
                   MOVE WS-NEW-NODE
                       TO CDD-CSN-LAST-CHILD(WS-PARENT-NODE)
               ELSE
                   MOVE CDD-CSN-LAST-CHILD(WS-PARENT-NODE)
                       TO WS-PREV-SIB
                   MOVE WS-NEW-NODE TO CDD-CSN-NEXT-SIB(WS-PREV-SIB)
                   MOVE WS-NEW-NODE
                       TO CDD-CSN-LAST-CHILD(WS-PARENT-NODE)
               END-IF
           END-IF.
      * @paragraph ACTIVE-CONTAINER Choose WS-CLASS-NODE or root 1.
       ACTIVE-CONTAINER.
           IF WS-CLASS-NODE > 0
               MOVE WS-CLASS-NODE TO WS-PARENT-NODE
           ELSE
               MOVE 1 TO WS-PARENT-NODE
           END-IF.
      * @paragraph FLUSH-OPAQUE-BEFORE Emit opaque chunk < WS-LEAD-T1.
       FLUSH-OPAQUE-BEFORE.
           IF WS-UNCLAIMED < WS-LEAD-T1
               PERFORM ACTIVE-CONTAINER
               PERFORM ALLOC-CST-NODE
               IF CDD-OK
                   MOVE 2 TO CDD-CSN-KIND(WS-NEW-NODE)
                   MOVE WS-UNCLAIMED TO CDD-CSN-TOK-START(WS-NEW-NODE)
                   COMPUTE CDD-CSN-OWN-END(WS-NEW-NODE) =
                       WS-LEAD-T1 - 1
                   END-COMPUTE
                   COMPUTE CDD-CSN-TOK-END(WS-NEW-NODE) =
                       WS-LEAD-T1 - 1
                   END-COMPUTE
                   MOVE WS-LEAD-T1 TO WS-UNCLAIMED
               END-IF
           END-IF.
      * @paragraph COMPUTE-LEAD-TRIVIA Find start of attached comments.
       COMPUTE-LEAD-TRIVIA.
           MOVE WS-STMT-C1 TO WS-LEAD-T1
           PERFORM UNTIL WS-LEAD-T1 <= WS-UNCLAIMED
               COMPUTE WS-BACK-I = WS-LEAD-T1 - 1
               END-COMPUTE
               IF CDD-CTI-TRIVIA-TOK(WS-BACK-I)
                   MOVE WS-BACK-I TO WS-LEAD-T1
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           PERFORM TRIM-BLANK-PREFIX-LINES.
      * @paragraph TRIM-BLANK-PREFIX-LINES Leave prior blank gap opaque.
       TRIM-BLANK-PREFIX-LINES.
           PERFORM UNTIL WS-LEAD-T1 >= WS-STMT-C1
               IF CDD-CTI-TRV-NL(WS-LEAD-T1)
                   ADD 1 TO WS-LEAD-T1
                   END-ADD
               ELSE
                   IF CDD-CTI-LINE(WS-LEAD-T1)
                       < CDD-CTI-LINE(WS-STMT-C1)
                       PERFORM CHECK-LINE-HAS-COMMENT
                       IF WS-CMT-SCAN = 0
                           PERFORM ADVANCE-LEAD-PAST-NL
                       ELSE
                           EXIT PERFORM
                       END-IF
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.
      * @paragraph CHECK-LINE-HAS-COMMENT Set WS-CMT-SCAN=1 if comment.
       CHECK-LINE-HAS-COMMENT.
           MOVE 0 TO WS-CMT-SCAN
           MOVE CDD-CTI-LINE(WS-LEAD-T1) TO WS-LINE-ANCHOR
           MOVE WS-LEAD-T1 TO WS-BACK-I
           PERFORM UNTIL WS-BACK-I >= WS-STMT-C1
               OR CDD-CTI-LINE(WS-BACK-I) NOT = WS-LINE-ANCHOR
               IF CDD-CTI-TRV-CMTLINE(WS-BACK-I)
                   OR CDD-CTI-TRV-INLCMT(WS-BACK-I)
                   OR (CDD-CTI-TRV-IND(WS-BACK-I)
                   AND CDD-BUF-DATA(CDD-CTI-OFFSET(WS-BACK-I):1)
                       NOT = SPACE)
                   MOVE 1 TO WS-CMT-SCAN
               END-IF
               ADD 1 TO WS-BACK-I
               END-ADD
           END-PERFORM.
      * @paragraph ADVANCE-LEAD-PAST-NL Advance WS-LEAD-T1 to next line.
       ADVANCE-LEAD-PAST-NL.
           MOVE CDD-CTI-LINE(WS-LEAD-T1) TO WS-LINE-ANCHOR
           PERFORM UNTIL WS-LEAD-T1 >= WS-STMT-C1
               OR CDD-CTI-LINE(WS-LEAD-T1) NOT = WS-LINE-ANCHOR
               ADD 1 TO WS-LEAD-T1
               END-ADD
           END-PERFORM.
      * @paragraph FLUSH-INNER-OPAQUE Emit opaque gap under WS-PARENT.
       FLUSH-INNER-OPAQUE.
           IF WS-UNCLAIMED < WS-LEAD-T1
               PERFORM ALLOC-CST-NODE
               IF CDD-OK
                   MOVE 2 TO CDD-CSN-KIND(WS-NEW-NODE)
                   MOVE WS-UNCLAIMED TO CDD-CSN-TOK-START(WS-NEW-NODE)
                   COMPUTE CDD-CSN-OWN-END(WS-NEW-NODE) =
                       WS-LEAD-T1 - 1
                   END-COMPUTE
                   COMPUTE CDD-CSN-TOK-END(WS-NEW-NODE) =
                       WS-LEAD-T1 - 1
                   END-COMPUTE
                   MOVE WS-LEAD-T1 TO WS-UNCLAIMED
               END-IF
           END-IF.
      * @paragraph CLOSE-DATA-STACK Set TOK-END on open UDT groups.
       CLOSE-DATA-STACK.
           IF WS-LSTK-DEPTH > 0
               PERFORM UNTIL WS-LSTK-DEPTH = 0
                   IF CDD-CSN-FIRST-CHILD(
                       WS-LSTK-NODE(WS-LSTK-DEPTH)) > 0
                       COMPUTE CDD-CSN-TOK-END(
                           WS-LSTK-NODE(WS-LSTK-DEPTH)) =
                           WS-UNCLAIMED - 1
                       END-COMPUTE
                   END-IF
                   SUBTRACT 1 FROM WS-LSTK-DEPTH
                   END-SUBTRACT
               END-PERFORM
           END-IF.
      * @paragraph CLOSE-OPEN-ROUTINE Finalize open PROC routine span.
       CLOSE-OPEN-ROUTINE.
           IF WS-RTN-NODE > 0
               COMPUTE CDD-CSN-TOK-END(WS-RTN-NODE) = WS-LEAD-T1 - 1
               END-COMPUTE
               MOVE 0 TO WS-RTN-NODE
               MOVE 0 TO WS-RTN-IS-SEC
               MOVE WS-LEAD-T1 TO WS-UNCLAIMED
           END-IF.
      * @paragraph PARSE-TOKEN-STREAM Step sentence-by-sentence.
       PARSE-TOKEN-STREAM.
           PERFORM UNTIL WS-TI > CDD-CTK-COUNT OR NOT CDD-OK
               PERFORM FIND-NEXT-CODE-TOK
               IF WS-TI > CDD-CTK-COUNT
                   EXIT PERFORM
               END-IF
               PERFORM READ-STATEMENT-SPAN
               PERFORM INSPECT-STATEMENT
               COMPUTE WS-TI = WS-STMT-OWN-END + 1
               END-COMPUTE
           END-PERFORM.
      * @paragraph FIND-NEXT-CODE-TOK Advance WS-TI to semantic token.
       FIND-NEXT-CODE-TOK.
           PERFORM UNTIL WS-TI > CDD-CTK-COUNT
               IF CDD-CTI-CODE-TOK(WS-TI)
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM.
      * @paragraph READ-STATEMENT-SPAN Read tokens up to '.' & line NL.
       READ-STATEMENT-SPAN.
           MOVE WS-TI TO WS-STMT-C1
           MOVE WS-TI TO WS-STMT-C2
           MOVE 0 TO WS-N2-IDX
           MOVE 0 TO WS-N3-IDX
           MOVE WS-STMT-C1 TO WS-BACK-I
           PERFORM LOAD-UPPER-TOK
           MOVE WS-CMT-LINE(1:64) TO WS-W1
           PERFORM UNTIL WS-STMT-C2 >= CDD-CTK-COUNT
               IF CDD-CTI-CODE-TOK(WS-STMT-C2)
                   IF WS-STMT-C2 > WS-STMT-C1
                       AND NOT CDD-CTI-TOK-DOT(WS-STMT-C2)
                       IF WS-N2-IDX = 0
                           MOVE WS-STMT-C2 TO WS-N2-IDX
                       ELSE
                           IF WS-N3-IDX = 0
                               MOVE WS-STMT-C2 TO WS-N3-IDX
                           END-IF
                       END-IF
                   END-IF
                   IF CDD-CTI-TOK-DOT(WS-STMT-C2)
                       IF (WS-W1 = "CLASS-ID"
                           OR WS-W1 = "METHOD-ID"
                           OR WS-W1 = "FUNCTION-ID"
                           OR WS-W1 = "PROGRAM-ID")
                           AND WS-N2-IDX = 0
                           CONTINUE
                       ELSE
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-IF
               ADD 1 TO WS-STMT-C2
               END-ADD
           END-PERFORM
           MOVE WS-STMT-C2 TO WS-STMT-OWN-END
           MOVE CDD-CTI-LINE(WS-STMT-C2) TO WS-LINE-ANCHOR
           PERFORM UNTIL WS-STMT-OWN-END >= CDD-CTK-COUNT
               IF CDD-CTI-LINE(WS-STMT-OWN-END + 1) = WS-LINE-ANCHOR
                   AND CDD-CTI-TRIVIA-TOK(WS-STMT-OWN-END + 1)
                   ADD 1 TO WS-STMT-OWN-END
                   END-ADD
                   IF CDD-CTI-TRV-NL(WS-STMT-OWN-END)
                       EXIT PERFORM
                   END-IF
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           PERFORM EXTRACT-HEAD-WORDS.
      * @paragraph EXTRACT-HEAD-WORDS Load WS-W1, WS-W2, WS-W3.
       EXTRACT-HEAD-WORDS.
           MOVE SPACES TO WS-W1
           MOVE SPACES TO WS-W2
           MOVE SPACES TO WS-W3
           MOVE WS-STMT-C1 TO WS-BACK-I
           PERFORM LOAD-UPPER-TOK
           MOVE WS-CMT-LINE(1:64) TO WS-W1
           IF WS-N2-IDX > 0
               MOVE WS-N2-IDX TO WS-BACK-I
               PERFORM LOAD-UPPER-TOK
               MOVE WS-CMT-LINE(1:64) TO WS-W2
           END-IF
           IF WS-N3-IDX > 0
               MOVE WS-N3-IDX TO WS-BACK-I
               PERFORM LOAD-UPPER-TOK
               MOVE WS-CMT-LINE(1:64) TO WS-W3
           END-IF.
      * @paragraph LOAD-UPPER-TOK Load uppercased token WS-BACK-I.
       LOAD-UPPER-TOK.
           MOVE SPACES TO WS-CMT-LINE
           IF CDD-CTI-LEN(WS-BACK-I) > 64
               MOVE 64 TO WS-WLEN
           ELSE
               COMPUTE WS-WLEN = CDD-CTI-LEN(WS-BACK-I)
               END-COMPUTE
           END-IF
           MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-BACK-I):WS-WLEN)
               TO WS-CMT-LINE(1:WS-WLEN)
           INSPECT WS-CMT-LINE(1:WS-WLEN)
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
      * @paragraph INSPECT-STATEMENT Classify division, class, UDT, rtn.
       INSPECT-STATEMENT.
           EVALUATE TRUE
               WHEN (WS-W1 = "IDENTIFICATION" OR WS-W1 = "ID")
                   AND WS-W2 = "DIVISION"
                   PERFORM COMPUTE-LEAD-TRIVIA
                   PERFORM CLOSE-DATA-STACK
                   PERFORM CLOSE-OPEN-ROUTINE
                   MOVE 1 TO WS-DIV-MODE
               WHEN WS-W1 = "ENVIRONMENT" AND WS-W2 = "DIVISION"
                   PERFORM COMPUTE-LEAD-TRIVIA
                   PERFORM CLOSE-DATA-STACK
                   PERFORM CLOSE-OPEN-ROUTINE
                   MOVE 2 TO WS-DIV-MODE
               WHEN WS-W1 = "DATA" AND WS-W2 = "DIVISION"
                   PERFORM COMPUTE-LEAD-TRIVIA
                   PERFORM CLOSE-DATA-STACK
                   PERFORM CLOSE-OPEN-ROUTINE
                   MOVE 3 TO WS-DIV-MODE
                   MOVE 1 TO WS-DATA-SEC-OK
               WHEN WS-W1 = "PROCEDURE" AND WS-W2 = "DIVISION"
                   PERFORM COMPUTE-LEAD-TRIVIA
                   PERFORM CLOSE-DATA-STACK
                   PERFORM CLOSE-OPEN-ROUTINE
                   MOVE 4 TO WS-DIV-MODE
               WHEN WS-W1 = "CLASS-ID"
                   PERFORM BUILD-CLASS-NODE
               WHEN WS-W1 = "END" AND WS-W2 = "CLASS"
                   PERFORM FINISH-CLASS-NODE
               WHEN WS-DIV-MODE = 3 AND WS-W2 = "SECTION"
                   PERFORM HANDLE-DATA-SECTION-HDR
               WHEN WS-DIV-MODE = 3 AND WS-DATA-SEC-OK = 1
                   AND CDD-CTI-TOK-LEVEL(WS-STMT-C1)
                   PERFORM BUILD-DATA-ENTRY-NODE
               WHEN WS-W1 = "METHOD-ID" OR WS-W1 = "FUNCTION-ID"
                   PERFORM BUILD-MODULAR-ROUTINE
               WHEN WS-W1 = "END"
                   AND (WS-W2 = "METHOD" OR WS-W2 = "FUNCTION"
                   OR WS-W2 = "PROGRAM")
                   PERFORM FINISH-MODULAR-ROUTINE
               WHEN WS-DIV-MODE = 4
                   PERFORM CHECK-PROC-HEADER-STMT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph HANDLE-DATA-SECTION-HDR Track WS/LINKAGE vs FILE/RPT.
       HANDLE-DATA-SECTION-HDR.
           PERFORM COMPUTE-LEAD-TRIVIA
           PERFORM CLOSE-DATA-STACK
           IF WS-W1 = "FILE" OR WS-W1 = "REPORT"
               MOVE 0 TO WS-DATA-SEC-OK
           ELSE
               MOVE 1 TO WS-DATA-SEC-OK
           END-IF.
      * @paragraph BUILD-CLASS-NODE Open COBOL-2002+ CLASS-ID CST node.
       BUILD-CLASS-NODE.
           PERFORM COMPUTE-LEAD-TRIVIA
           PERFORM CLOSE-DATA-STACK
           PERFORM CLOSE-OPEN-ROUTINE
           PERFORM FLUSH-OPAQUE-BEFORE
           IF CDD-OK
               MOVE 1 TO WS-PARENT-NODE
               PERFORM ALLOC-CST-NODE
               IF CDD-OK
                   MOVE 5 TO CDD-CSN-KIND(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W2-NODE-NAME
                   MOVE WS-NEW-NODE TO WS-CLASS-NODE
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph FINISH-CLASS-NODE Close CLASS-ID with END CLASS span.
       FINISH-CLASS-NODE.
           IF WS-CLASS-NODE > 0
               PERFORM COMPUTE-LEAD-TRIVIA
               PERFORM CLOSE-DATA-STACK
               PERFORM CLOSE-OPEN-ROUTINE
               PERFORM FLUSH-OPAQUE-BEFORE
               IF CDD-OK
                   MOVE WS-LEAD-T1
                       TO CDD-CSN-FOOT-START(WS-CLASS-NODE)
                   MOVE WS-STMT-OWN-END
                       TO CDD-CSN-TOK-END(WS-CLASS-NODE)
                   MOVE 0 TO WS-CLASS-NODE
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph BUILD-DATA-ENTRY-NODE Build 01/TYPEDEF/field/88/66.
       BUILD-DATA-ENTRY-NODE.
           PERFORM COMPUTE-LEAD-TRIVIA
           MOVE CDD-BUF-DATA(
               CDD-CTI-OFFSET(WS-STMT-C1):CDD-CTI-LEN(WS-STMT-C1))
               TO WS-CMT-LINE(1:2)
           IF CDD-CTI-LEN(WS-STMT-C1) = 1
               MOVE 0 TO WS-LVL-NUM
               MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-STMT-C1):1)
                   TO WS-CMT-LINE(2:1)
               MOVE "0" TO WS-CMT-LINE(1:1)
           END-IF
           MOVE WS-CMT-LINE(1:2) TO WS-LVL-NUM
           EVALUATE TRUE
               WHEN WS-LVL-NUM = 1
                   PERFORM BUILD-LEVEL-01-NODE
               WHEN WS-LVL-NUM = 77
                   PERFORM BUILD-LEVEL-77-NODE
               WHEN WS-LVL-NUM = 88
                   PERFORM BUILD-LEVEL-88-NODE
               WHEN WS-LVL-NUM = 66
                   PERFORM BUILD-LEVEL-66-NODE
               WHEN OTHER
                   PERFORM BUILD-SUBORDINATE-FIELD
           END-EVALUATE.
      * @paragraph BUILD-LEVEL-01-NODE Create CST-RECORD or CST-TYPEDEF.
       BUILD-LEVEL-01-NODE.
           PERFORM CLOSE-DATA-STACK
           PERFORM FLUSH-OPAQUE-BEFORE
           IF CDD-OK
               MOVE "CDDCPI01" TO WS-PPI-TAG
               MOVE 2 TO WS-PPI-OP
               MOVE WS-STMT-C1 TO WS-PPI-T1
               MOVE WS-STMT-C2 TO WS-PPI-T2
               CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM
                   WS-PIC-PARM CDD-ERROR
               END-CALL
               IF CDD-OK
                   PERFORM ACTIVE-CONTAINER
                   PERFORM ALLOC-CST-NODE
                   IF CDD-OK
                       IF WS-PPI-ISTDEF = 1
                           MOVE 4 TO CDD-CSN-KIND(WS-NEW-NODE)
                       ELSE
                           MOVE 3 TO CDD-CSN-KIND(WS-NEW-NODE)
                       END-IF
                       MOVE 1 TO CDD-CSN-LEVEL(WS-NEW-NODE)
                       PERFORM POPULATE-NODE-SPANS-AND-DOCS
                       PERFORM ASSIGN-W2-NODE-NAME
                       MOVE 1 TO WS-LSTK-DEPTH
                       MOVE WS-NEW-NODE TO WS-LSTK-NODE(1)
                       MOVE 1 TO WS-LSTK-LVL(1)
                       COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                       END-COMPUTE
                   END-IF
               END-IF
           END-IF.
      * @paragraph BUILD-LEVEL-77-NODE Create standalone 77 field node.
       BUILD-LEVEL-77-NODE.
           PERFORM CLOSE-DATA-STACK
           PERFORM FLUSH-OPAQUE-BEFORE
           IF CDD-OK
               PERFORM ACTIVE-CONTAINER
               PERFORM ALLOC-CST-NODE
               IF CDD-OK
                   MOVE 6 TO CDD-CSN-KIND(WS-NEW-NODE)
                   MOVE 77 TO CDD-CSN-LEVEL(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W2-NODE-NAME
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph BUILD-SUBORDINATE-FIELD Create 02..49 child field.
       BUILD-SUBORDINATE-FIELD.
           PERFORM UNTIL WS-LSTK-DEPTH = 0
               OR WS-LSTK-LVL(WS-LSTK-DEPTH) < WS-LVL-NUM
               IF CDD-CSN-FIRST-CHILD(WS-LSTK-NODE(WS-LSTK-DEPTH)) > 0
                   COMPUTE CDD-CSN-TOK-END(
                       WS-LSTK-NODE(WS-LSTK-DEPTH)) = WS-UNCLAIMED - 1
                   END-COMPUTE
               END-IF
               SUBTRACT 1 FROM WS-LSTK-DEPTH
               END-SUBTRACT
           END-PERFORM
           IF WS-LSTK-DEPTH = 0
               MOVE 2007 TO CDD-ERR-CODE
               MOVE CDD-CTI-LINE(WS-STMT-C1) TO CDD-ERR-LINE
               MOVE CDD-CTI-COL(WS-STMT-C1) TO CDD-ERR-COL
               MOVE "CDDCSTB" TO CDD-ERR-SUBPROG
               MOVE "Subordinate level without enclosing 01 record"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE WS-LSTK-NODE(WS-LSTK-DEPTH) TO WS-PARENT-NODE
               PERFORM FLUSH-INNER-OPAQUE
               IF CDD-OK
                   MOVE WS-LSTK-NODE(WS-LSTK-DEPTH) TO WS-PARENT-NODE
                   PERFORM ALLOC-CST-NODE
               END-IF
               IF CDD-OK
                   MOVE 6 TO CDD-CSN-KIND(WS-NEW-NODE)
                   MOVE WS-LVL-NUM TO CDD-CSN-LEVEL(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W2-NODE-NAME
                   ADD 1 TO WS-LSTK-DEPTH
                   END-ADD
                   MOVE WS-NEW-NODE TO WS-LSTK-NODE(WS-LSTK-DEPTH)
                   MOVE WS-LVL-NUM TO WS-LSTK-LVL(WS-LSTK-DEPTH)
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph BUILD-LEVEL-88-NODE Attach 88 condition to top field.
       BUILD-LEVEL-88-NODE.
           IF WS-LSTK-DEPTH = 0
               MOVE 2007 TO CDD-ERR-CODE
               MOVE CDD-CTI-LINE(WS-STMT-C1) TO CDD-ERR-LINE
               MOVE CDD-CTI-COL(WS-STMT-C1) TO CDD-ERR-COL
               MOVE "CDDCSTB" TO CDD-ERR-SUBPROG
               MOVE "Level 88 condition without parent field"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE WS-LSTK-NODE(WS-LSTK-DEPTH) TO WS-PARENT-NODE
               PERFORM FLUSH-INNER-OPAQUE
               IF CDD-OK
                   MOVE WS-LSTK-NODE(WS-LSTK-DEPTH) TO WS-PARENT-NODE
                   PERFORM ALLOC-CST-NODE
               END-IF
               IF CDD-OK
                   MOVE 7 TO CDD-CSN-KIND(WS-NEW-NODE)
                   MOVE 88 TO CDD-CSN-LEVEL(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W2-NODE-NAME
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph BUILD-LEVEL-66-NODE Attach 66 RENAMES to 01 record.
       BUILD-LEVEL-66-NODE.
           IF WS-LSTK-DEPTH = 0
               MOVE 2007 TO CDD-ERR-CODE
               MOVE CDD-CTI-LINE(WS-STMT-C1) TO CDD-ERR-LINE
               MOVE CDD-CTI-COL(WS-STMT-C1) TO CDD-ERR-COL
               MOVE "CDDCSTB" TO CDD-ERR-SUBPROG
               MOVE "Level 66 RENAMES without enclosing 01 record"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM UNTIL WS-LSTK-DEPTH = 1
                   IF CDD-CSN-FIRST-CHILD(
                       WS-LSTK-NODE(WS-LSTK-DEPTH)) > 0
                       COMPUTE CDD-CSN-TOK-END(
                           WS-LSTK-NODE(WS-LSTK-DEPTH)) =
                           WS-UNCLAIMED - 1
                       END-COMPUTE
                   END-IF
                   SUBTRACT 1 FROM WS-LSTK-DEPTH
                   END-SUBTRACT
               END-PERFORM
               MOVE WS-LSTK-NODE(1) TO WS-PARENT-NODE
               PERFORM FLUSH-INNER-OPAQUE
               IF CDD-OK
                   MOVE WS-LSTK-NODE(1) TO WS-PARENT-NODE
                   PERFORM ALLOC-CST-NODE
               END-IF
               IF CDD-OK
                   MOVE 8 TO CDD-CSN-KIND(WS-NEW-NODE)
                   MOVE 66 TO CDD-CSN-LEVEL(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W2-NODE-NAME
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph BUILD-MODULAR-ROUTINE Open METHOD-ID or FUNCTION-ID.
       BUILD-MODULAR-ROUTINE.
           PERFORM COMPUTE-LEAD-TRIVIA
           PERFORM CLOSE-DATA-STACK
           PERFORM CLOSE-OPEN-ROUTINE
           PERFORM FLUSH-OPAQUE-BEFORE
           IF CDD-OK
               PERFORM ACTIVE-CONTAINER
               PERFORM ALLOC-CST-NODE
               IF CDD-OK
                   MOVE 9 TO CDD-CSN-KIND(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W2-NODE-NAME
                   MOVE WS-NEW-NODE TO WS-RTN-NODE
                   MOVE 2 TO WS-RTN-IS-SEC
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph FINISH-MODULAR-ROUTINE Close END METHOD/FUNCTION/PRG.
       FINISH-MODULAR-ROUTINE.
           IF WS-RTN-NODE > 0 AND WS-RTN-IS-SEC = 2
               PERFORM COMPUTE-LEAD-TRIVIA
               MOVE WS-LEAD-T1 TO CDD-CSN-FOOT-START(WS-RTN-NODE)
               MOVE WS-STMT-OWN-END TO CDD-CSN-TOK-END(WS-RTN-NODE)
               MOVE 0 TO WS-RTN-NODE
               MOVE 0 TO WS-RTN-IS-SEC
               COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
               END-COMPUTE
           END-IF.
      * @paragraph CHECK-PROC-HEADER-STMT Detect Area A section/para.
       CHECK-PROC-HEADER-STMT.
           IF (CDD-CTK-IS-FIXED AND CDD-CTI-COL(WS-STMT-C1) >= 8
               AND CDD-CTI-COL(WS-STMT-C1) <= 11)
               OR CDD-CTK-IS-FREE
               IF WS-W2 = "SECTION" AND WS-W1 NOT = "DECLARATIVES"
                   PERFORM OPEN-PROC-SEC-OR-PARA
                   MOVE 1 TO WS-RTN-IS-SEC
               ELSE
                   IF WS-N2-IDX = 0
                       AND CDD-CTI-TOK-DOT(WS-STMT-C2)
                       AND WS-W1 NOT = "EXIT"
                       AND WS-W1 NOT = "GOBACK"
                       AND WS-W1 NOT = "CONTINUE"
                       AND WS-RTN-IS-SEC = 0
                       PERFORM OPEN-PROC-SEC-OR-PARA
                   END-IF
               END-IF
           END-IF.
      * @paragraph OPEN-PROC-SEC-OR-PARA Create CST-ROUTINE for sec/par.
       OPEN-PROC-SEC-OR-PARA.
           PERFORM COMPUTE-LEAD-TRIVIA
           PERFORM CLOSE-OPEN-ROUTINE
           PERFORM FLUSH-OPAQUE-BEFORE
           IF CDD-OK
               PERFORM ACTIVE-CONTAINER
               PERFORM ALLOC-CST-NODE
               IF CDD-OK
                   MOVE 9 TO CDD-CSN-KIND(WS-NEW-NODE)
                   PERFORM POPULATE-NODE-SPANS-AND-DOCS
                   PERFORM ASSIGN-W1-NODE-NAME
                   MOVE WS-NEW-NODE TO WS-RTN-NODE
                   MOVE 0 TO WS-RTN-IS-SEC
                   COMPUTE WS-UNCLAIMED = WS-STMT-OWN-END + 1
                   END-COMPUTE
               END-IF
           END-IF.
      * @paragraph POPULATE-NODE-SPANS-AND-DOCS Set spans & scan docs.
       POPULATE-NODE-SPANS-AND-DOCS.
           MOVE WS-LEAD-T1 TO CDD-CSN-TOK-START(WS-NEW-NODE)
           COMPUTE CDD-CSN-LEAD-END(WS-NEW-NODE) = WS-STMT-C1 - 1
           END-COMPUTE
           MOVE WS-STMT-C1 TO CDD-CSN-CODE-START(WS-NEW-NODE)
           MOVE WS-STMT-C2 TO CDD-CSN-CODE-END(WS-NEW-NODE)
           MOVE WS-STMT-OWN-END TO CDD-CSN-OWN-END(WS-NEW-NODE)
           MOVE WS-STMT-OWN-END TO CDD-CSN-TOK-END(WS-NEW-NODE)
           PERFORM EXTRACT-TRIVIA-ANNOTATIONS.
      * @paragraph ASSIGN-W1-NODE-NAME Set CDD-CSN-NAME from WS-W1.
       ASSIGN-W1-NODE-NAME.
           MOVE 64 TO WS-WLEN
           PERFORM UNTIL WS-WLEN = 0 OR WS-W1(WS-WLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-WLEN
               END-SUBTRACT
           END-PERFORM
           COMPUTE CDD-CSN-NAME-LEN(WS-NEW-NODE) = WS-WLEN
           END-COMPUTE
           MOVE WS-W1 TO CDD-CSN-NAME(WS-NEW-NODE).
      * @paragraph ASSIGN-W2-NODE-NAME Set CDD-CSN-NAME from WS-W2.
       ASSIGN-W2-NODE-NAME.
           IF WS-W2 NOT = SPACES AND WS-W2 NOT = "."
               AND WS-W2 NOT = "PIC" AND WS-W2 NOT = "PICTURE"
               AND WS-W2 NOT = "IS"
               MOVE 64 TO WS-WLEN
               PERFORM UNTIL WS-WLEN = 0
                   OR WS-W2(WS-WLEN:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-WLEN
                   END-SUBTRACT
               END-PERFORM
               COMPUTE CDD-CSN-NAME-LEN(WS-NEW-NODE) = WS-WLEN
           END-COMPUTE
               MOVE WS-W2 TO CDD-CSN-NAME(WS-NEW-NODE)
           END-IF.
      * @paragraph EXTRACT-TRIVIA-ANNOTATIONS Parse @json-name/doc/keep.
       EXTRACT-TRIVIA-ANNOTATIONS.
           MOVE WS-LEAD-T1 TO WS-CMT-SCAN
           PERFORM UNTIL WS-CMT-SCAN > WS-STMT-OWN-END
               IF CDD-CTI-TRV-CMTLINE(WS-CMT-SCAN)
                   OR CDD-CTI-TRV-INLCMT(WS-CMT-SCAN)
                   PERFORM PARSE-COMMENT-SLICE
               END-IF
               ADD 1 TO WS-CMT-SCAN
               END-ADD
           END-PERFORM.
      * @paragraph PARSE-COMMENT-SLICE Extract @json-name/@cdd-keep/doc.
       PARSE-COMMENT-SLICE.
           MOVE SPACES TO WS-CMT-LINE
           IF CDD-CTI-LEN(WS-CMT-SCAN) > 256
               MOVE 256 TO WS-WLEN
           ELSE
               COMPUTE WS-WLEN = CDD-CTI-LEN(WS-CMT-SCAN)
               END-COMPUTE
           END-IF
           MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-CMT-SCAN):WS-WLEN)
               TO WS-CMT-LINE(1:WS-WLEN)
           MOVE 1 TO WS-BYTE-I
           IF WS-WLEN >= 2 AND WS-CMT-LINE(1:2) = "*>"
               MOVE 3 TO WS-BYTE-I
           END-IF
           PERFORM UNTIL WS-BYTE-I > WS-WLEN
               OR WS-CMT-LINE(WS-BYTE-I:1) NOT = SPACE
               ADD 1 TO WS-BYTE-I
               END-ADD
           END-PERFORM
           IF WS-BYTE-I <= WS-WLEN
               IF WS-BYTE-I + 9 <= 256
                   AND WS-CMT-LINE(WS-BYTE-I:10) = "@json-name"
                   ADD 10 TO WS-BYTE-I
                   END-ADD
                   PERFORM EXTRACT-JSON-NAME-ARG
               ELSE
                   IF WS-BYTE-I + 8 <= 256
                       AND WS-CMT-LINE(WS-BYTE-I:9) = "@cdd-keep"
                       MOVE 1 TO CDD-CSN-KEEP-FLAG(WS-NEW-NODE)
                   ELSE
                       PERFORM CAPTURE-COMMENT-DOC
                   END-IF
               END-IF
           END-IF.
      * @paragraph EXTRACT-JSON-NAME-ARG Store @json-name <key>.
       EXTRACT-JSON-NAME-ARG.
           PERFORM UNTIL WS-BYTE-I > WS-WLEN
               OR WS-CMT-LINE(WS-BYTE-I:1) NOT = SPACE
               ADD 1 TO WS-BYTE-I
               END-ADD
           END-PERFORM
           MOVE WS-BYTE-I TO WS-BYTE-END
           PERFORM UNTIL WS-BYTE-END > WS-WLEN
               OR WS-CMT-LINE(WS-BYTE-END:1) = SPACE
               ADD 1 TO WS-BYTE-END
               END-ADD
           END-PERFORM
           IF WS-BYTE-END > WS-BYTE-I
               COMPUTE WS-WLEN = WS-BYTE-END - WS-BYTE-I
               END-COMPUTE
               IF WS-WLEN > 128
                   MOVE 128 TO WS-WLEN
               END-IF
               MOVE WS-WLEN TO CDD-CSN-JKEY-LEN(WS-NEW-NODE)
               MOVE WS-CMT-LINE(WS-BYTE-I:WS-WLEN)
                   TO CDD-CSN-JSON-KEY(WS-NEW-NODE)(1:WS-WLEN)
           END-IF.
      * @paragraph CAPTURE-COMMENT-DOC Save human summary on CST node.
       CAPTURE-COMMENT-DOC.
           IF WS-CMT-LINE(WS-BYTE-I:1) = "@"
               PERFORM UNTIL WS-BYTE-I > WS-WLEN
                   OR WS-CMT-LINE(WS-BYTE-I:1) = SPACE
                   ADD 1 TO WS-BYTE-I
                   END-ADD
               END-PERFORM
               PERFORM UNTIL WS-BYTE-I > WS-WLEN
                   OR WS-CMT-LINE(WS-BYTE-I:1) NOT = SPACE
                   ADD 1 TO WS-BYTE-I
                   END-ADD
               END-PERFORM
               IF WS-CMT-LINE(1:8) = " @field "
                   OR WS-CMT-LINE(1:9) = " @record "
                   PERFORM UNTIL WS-BYTE-I > WS-WLEN
                       OR WS-CMT-LINE(WS-BYTE-I:1) = SPACE
                       ADD 1 TO WS-BYTE-I
                       END-ADD
                   END-PERFORM
                   PERFORM UNTIL WS-BYTE-I > WS-WLEN
                       OR WS-CMT-LINE(WS-BYTE-I:1) NOT = SPACE
                       ADD 1 TO WS-BYTE-I
                       END-ADD
                   END-PERFORM
               END-IF
           END-IF
           MOVE WS-WLEN TO WS-BYTE-END
           PERFORM UNTIL WS-BYTE-END < WS-BYTE-I
               OR WS-CMT-LINE(WS-BYTE-END:1) NOT = SPACE
               SUBTRACT 1 FROM WS-BYTE-END
               END-SUBTRACT
           END-PERFORM
           IF WS-BYTE-END >= WS-BYTE-I
               AND CDD-CSN-DOC-LEN(WS-NEW-NODE) = 0
               COMPUTE WS-WLEN = WS-BYTE-END - WS-BYTE-I + 1
               END-COMPUTE
               IF WS-WLEN > 160
                   MOVE 160 TO WS-WLEN
               END-IF
               MOVE WS-WLEN TO CDD-CSN-DOC-LEN(WS-NEW-NODE)
               MOVE WS-CMT-LINE(WS-BYTE-I:WS-WLEN)
                   TO CDD-CSN-DOC-TEXT(WS-NEW-NODE)(1:WS-WLEN)
           END-IF.
      * @paragraph FINALIZE-OPEN-NODES Close trailing UDT/rtn/opaque.
       FINALIZE-OPEN-NODES.
           COMPUTE WS-LEAD-T1 = CDD-CTK-COUNT + 1
           END-COMPUTE
           PERFORM CLOSE-DATA-STACK
           IF WS-RTN-NODE > 0
               MOVE CDD-CTK-COUNT TO CDD-CSN-TOK-END(WS-RTN-NODE)
               MOVE 0 TO WS-RTN-NODE
               MOVE 0 TO WS-RTN-IS-SEC
               COMPUTE WS-UNCLAIMED = CDD-CTK-COUNT + 1
               END-COMPUTE
           END-IF
           PERFORM FLUSH-OPAQUE-BEFORE
           IF WS-CLASS-NODE > 0
               MOVE CDD-CTK-COUNT TO CDD-CSN-TOK-END(WS-CLASS-NODE)
               MOVE 0 TO WS-CLASS-NODE
           END-IF.
