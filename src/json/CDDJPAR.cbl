      * @file CDDJPAR.cbl
      * @brief Non-recursive explicit-stack RFC 8259 JSON DOM parser.
      * @module CDDJPAR
      * @details Parses CDD-JTOK-STREAM into an ordered CDD-JDOM tree
      *          using an explicit state/container stack, enforcing max
      *          nesting depth, duplicate-key rejection, capacity
      *          bounds, and trailing-token rejection.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDJPAR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
      * @record WS-STACK Explicit container stack for objects/arrays.
       01  WS-STACK.
      * @field WS-DEPTH Active container nesting depth (0..64).
           05  WS-DEPTH                  PIC 9(4) COMP VALUE 0.
      * @field WS-FRAME Stack frame per open container.
           05  WS-FRAME                  OCCURS 64 TIMES.
      * @field WS-STK-NODE DOM node index of open OBJECT or ARRAY.
               10  WS-STK-NODE           PIC 9(4) COMP.
      * @field WS-STK-STATE Parser expectation state in container.
               10  WS-STK-STATE          PIC 9(2) COMP.
      * @record WS-PAR-STATE Token cursor and temporary key/val buffers.
       01  WS-PAR-STATE.
      * @field WS-TI 1-based token index in CDD-JTOK-STREAM.
           05  WS-TI                     PIC 9(8) COMP VALUE 1.
      * @field WS-CUR-KIND Active token kind.
           05  WS-CUR-KIND               PIC 9(2) COMP VALUE 0.
      * @field WS-CUR-OFF Active token byte offset in CDD-BUF.
           05  WS-CUR-OFF                PIC 9(8) COMP VALUE 0.
      * @field WS-CUR-LEN Active token byte length.
           05  WS-CUR-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-CUR-LINE Active token 1-based line.
           05  WS-CUR-LINE               PIC 9(8) COMP VALUE 1.
      * @field WS-CUR-COL Active token 1-based column.
           05  WS-CUR-COL                PIC 9(4) COMP VALUE 1.
      * @field WS-PEND-KEY-LEN Length of pending object member key.
           05  WS-PEND-KEY-LEN           PIC 9(4) COMP VALUE 0.
      * @field WS-PEND-KEY Unescaped pending object member key.
           05  WS-PEND-KEY               PIC X(128) VALUE SPACES.
      * @field WS-NEW-IDX Newly allocated DOM node index.
           05  WS-NEW-IDX                PIC 9(4) COMP VALUE 0.
      * @field WS-PARENT-IDX Parent container DOM node index.
           05  WS-PARENT-IDX             PIC 9(4) COMP VALUE 0.
      * @field WS-SIB-IDX Sibling walk index for duplicate-key check.
           05  WS-SIB-IDX                PIC 9(4) COMP VALUE 0.
      * @field WS-PREV-LAST Previous last-child index during attach.
           05  WS-PREV-LAST              PIC 9(4) COMP VALUE 0.
      * @field WS-ROOT-DONE 1 once top-level root value completed.
           05  WS-ROOT-DONE              PIC 9(2) COMP VALUE 0.
      * @field WS-EFF-MAXD Effective max nesting depth (1..64).
           05  WS-EFF-MAXD               PIC 9(4) COMP VALUE 32.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-jtok.cpy".
       COPY "cdd-jdom.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDJPAR Parse JSON token stream into CDD-JDOM tree.
      * @param [in] CDD-BUF Raw JSON buffer for string/number slices.
      * @param [in] CDD-JTOK-STREAM Tokenized JSON stream.
      * @param [inout] CDD-JDOM Output JSON DOM tree envelope.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Populated CDD-JDOM with CDD-JDM-ROOT index.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-JSON-EOF,
      *         CDD-ERR-JSON-BAD-TOKEN, CDD-ERR-JSON-TRAILING-DATA,
      *         CDD-ERR-JSON-DEPTH-LIMIT, CDD-ERR-JSON-DUP-KEY,
      *         CDD-ERR-JSON-CAPACITY.
       PROCEDURE DIVISION USING CDD-BUF CDD-JTOK-STREAM CDD-JDOM
           CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags and drive state machine.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID
               OR NOT CDD-JTK-TAG-VALID
               OR NOT CDD-JDM-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDJPAR"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-DOM-PARSER
           IF CDD-JTK-COUNT = 0
               MOVE 1001 TO CDD-ERR-CODE
               MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
               MOVE "Empty token stream" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM PARSE-LOOP
           IF CDD-OK
               PERFORM VERIFY-COMPLETE-AND-EOF
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-DOM-PARSER Reset DOM counts and stack state.
       INIT-DOM-PARSER.
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-JDM-ROOT
           MOVE 0 TO CDD-JDM-COUNT
           MOVE 0 TO WS-DEPTH
           MOVE 0 TO WS-ROOT-DONE
           MOVE 0 TO WS-PEND-KEY-LEN
           MOVE SPACES TO WS-PEND-KEY
           MOVE 1 TO WS-TI
           IF CDD-JDM-MAX-DEPTH = 0 OR CDD-JDM-MAX-DEPTH > 64
               MOVE 32 TO WS-EFF-MAXD
           ELSE
               MOVE CDD-JDM-MAX-DEPTH TO WS-EFF-MAXD
           END-IF.
      * @paragraph LOAD-CUR-TOK Copy token WS-TI fields into scratch.
       LOAD-CUR-TOK.
           MOVE CDD-JTK-KIND(WS-TI) TO WS-CUR-KIND
           MOVE CDD-JTK-OFFSET(WS-TI) TO WS-CUR-OFF
           MOVE CDD-JTK-LEN(WS-TI) TO WS-CUR-LEN
           MOVE CDD-JTK-LINE(WS-TI) TO WS-CUR-LINE
           MOVE CDD-JTK-COL(WS-TI) TO WS-CUR-COL.
      * @paragraph PARSE-LOOP Step through tokens until root complete.
       PARSE-LOOP.
           PERFORM UNTIL WS-TI > CDD-JTK-COUNT
               OR WS-ROOT-DONE = 1
               OR NOT CDD-OK
               PERFORM LOAD-CUR-TOK
               IF WS-CUR-KIND = 13
                   EXIT PERFORM
               END-IF
               IF WS-DEPTH = 0
                   PERFORM PARSE-VALUE-START
               ELSE
                   PERFORM STEP-CONTAINER-STATE
               END-IF
           END-PERFORM.
      * @paragraph VERIFY-COMPLETE-AND-EOF Check no open frame or extra.
       VERIFY-COMPLETE-AND-EOF.
           IF WS-ROOT-DONE = 0 OR WS-DEPTH > 0
               MOVE 1001 TO CDD-ERR-CODE
               MOVE WS-CUR-LINE TO CDD-ERR-LINE
               MOVE WS-CUR-COL TO CDD-ERR-COL
               MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
               MOVE "Unexpected end of JSON document"
                   TO CDD-ERR-MESSAGE
           ELSE
               IF WS-TI <= CDD-JTK-COUNT
                   PERFORM LOAD-CUR-TOK
                   IF WS-CUR-KIND NOT = 13
                       MOVE 1007 TO CDD-ERR-CODE
                       MOVE WS-CUR-LINE TO CDD-ERR-LINE
                       MOVE WS-CUR-COL TO CDD-ERR-COL
                       MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
                       MOVE "Trailing tokens after JSON root value"
                           TO CDD-ERR-MESSAGE
                   END-IF
               END-IF
           END-IF.
      * @paragraph STEP-CONTAINER-STATE Dispatch open OBJECT/ARRAY st.
       STEP-CONTAINER-STATE.
           EVALUATE WS-STK-STATE(WS-DEPTH)
               WHEN 1
                   IF WS-CUR-KIND = 2
                       SUBTRACT 1 FROM WS-DEPTH
                       END-SUBTRACT
                       ADD 1 TO WS-TI
                       END-ADD
                       PERFORM FINISH-IF-ROOT
                   ELSE
                       PERFORM EXPECT-MEMBER-KEY
                   END-IF
               WHEN 2
                   PERFORM EXPECT-MEMBER-KEY
               WHEN 3
                   IF WS-CUR-KIND = 5
                       MOVE 4 TO WS-STK-STATE(WS-DEPTH)
                       ADD 1 TO WS-TI
                       END-ADD
                   ELSE
                       PERFORM FAIL-BAD-SYNTAX
                   END-IF
               WHEN 4
                   MOVE 5 TO WS-STK-STATE(WS-DEPTH)
                   PERFORM PARSE-VALUE-START
               WHEN 5
                   IF WS-CUR-KIND = 6
                       MOVE 2 TO WS-STK-STATE(WS-DEPTH)
                       ADD 1 TO WS-TI
                       END-ADD
                   ELSE
                       IF WS-CUR-KIND = 2
                           SUBTRACT 1 FROM WS-DEPTH
                           END-SUBTRACT
                           ADD 1 TO WS-TI
                           END-ADD
                           PERFORM FINISH-IF-ROOT
                       ELSE
                           PERFORM FAIL-BAD-SYNTAX
                       END-IF
                   END-IF
               WHEN 6
                   IF WS-CUR-KIND = 4
                       SUBTRACT 1 FROM WS-DEPTH
                       END-SUBTRACT
                       ADD 1 TO WS-TI
                       END-ADD
                       PERFORM FINISH-IF-ROOT
                   ELSE
                       MOVE 8 TO WS-STK-STATE(WS-DEPTH)
                       PERFORM PARSE-VALUE-START
                   END-IF
               WHEN 7
                   MOVE 8 TO WS-STK-STATE(WS-DEPTH)
                   PERFORM PARSE-VALUE-START
               WHEN OTHER
                   IF WS-CUR-KIND = 6
                       MOVE 7 TO WS-STK-STATE(WS-DEPTH)
                       ADD 1 TO WS-TI
                       END-ADD
                   ELSE
                       IF WS-CUR-KIND = 4
                           SUBTRACT 1 FROM WS-DEPTH
                           END-SUBTRACT
                           ADD 1 TO WS-TI
                           END-ADD
                           PERFORM FINISH-IF-ROOT
                       ELSE
                           PERFORM FAIL-BAD-SYNTAX
                       END-IF
                   END-IF
           END-EVALUATE.
      * @paragraph FINISH-IF-ROOT Mark WS-ROOT-DONE if depth reached 0.
       FINISH-IF-ROOT.
           IF WS-DEPTH = 0
               MOVE 1 TO WS-ROOT-DONE
           END-IF.
      * @paragraph EXPECT-MEMBER-KEY Unescape key and check duplicates.
       EXPECT-MEMBER-KEY.
           IF WS-CUR-KIND NOT = 7
               PERFORM FAIL-BAD-SYNTAX
           ELSE
               PERFORM UNESCAPE-CUR-STRING
               IF CDD-OK
                   IF CDD-SRQ-OUT-LEN > 128
                       MOVE 128 TO WS-PEND-KEY-LEN
                   ELSE
                       MOVE CDD-SRQ-OUT-LEN TO WS-PEND-KEY-LEN
                   END-IF
                   MOVE SPACES TO WS-PEND-KEY
                   IF WS-PEND-KEY-LEN > 0
                       MOVE CDD-SRQ-OUT-TEXT(1:WS-PEND-KEY-LEN)
                           TO WS-PEND-KEY(1:WS-PEND-KEY-LEN)
                   END-IF
                   IF CDD-JDM-DUP-REJECT
                       PERFORM CHECK-DUPLICATE-KEY
                   END-IF
                   IF CDD-OK
                       MOVE 3 TO WS-STK-STATE(WS-DEPTH)
                       ADD 1 TO WS-TI
                       END-ADD
                   END-IF
               END-IF
           END-IF.
      * @paragraph CHECK-DUPLICATE-KEY Scan siblings of active OBJECT.
       CHECK-DUPLICATE-KEY.
           MOVE WS-STK-NODE(WS-DEPTH) TO WS-PARENT-IDX
           MOVE CDD-JDN-FIRST-CHILD(WS-PARENT-IDX) TO WS-SIB-IDX
           PERFORM UNTIL WS-SIB-IDX = 0 OR NOT CDD-OK
               IF CDD-JDN-KEY-LEN(WS-SIB-IDX) = WS-PEND-KEY-LEN
                   AND CDD-JDN-KEY(WS-SIB-IDX) = WS-PEND-KEY
                   MOVE 1009 TO CDD-ERR-CODE
                   MOVE WS-CUR-LINE TO CDD-ERR-LINE
                   MOVE WS-CUR-COL TO CDD-ERR-COL
                   MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
                   MOVE "Duplicate JSON object key"
                       TO CDD-ERR-MESSAGE
               END-IF
               MOVE CDD-JDN-NEXT-SIB(WS-SIB-IDX) TO WS-SIB-IDX
           END-PERFORM.
      * @paragraph UNESCAPE-CUR-STRING Decode string token via CDDSTR.
       UNESCAPE-CUR-STRING.
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 5 TO CDD-SRQ-OP
           MOVE SPACES TO CDD-SRQ-OUT-TEXT
           MOVE 0 TO CDD-SRQ-OUT-LEN
           IF WS-CUR-LEN > 0
               COMPUTE CDD-SRQ-IN-LEN = WS-CUR-LEN
               END-COMPUTE
               MOVE CDD-BUF-DATA(WS-CUR-OFF:WS-CUR-LEN)
                   TO CDD-SRQ-IN-TEXT(1:WS-CUR-LEN)
               CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
               END-CALL
           END-IF.
      * @paragraph ALLOC-DOM-NODE Allocate and link a new DOM node.
       ALLOC-DOM-NODE.
           IF CDD-JDM-COUNT >= CDD-JDM-CAP
               MOVE 1010 TO CDD-ERR-CODE
               MOVE WS-CUR-LINE TO CDD-ERR-LINE
               MOVE WS-CUR-COL TO CDD-ERR-COL
               MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
               MOVE "JSON DOM node capacity exhausted"
                   TO CDD-ERR-MESSAGE
           ELSE
               ADD 1 TO CDD-JDM-COUNT
               END-ADD
               MOVE CDD-JDM-COUNT TO WS-NEW-IDX
               MOVE 0 TO CDD-JDN-KIND(WS-NEW-IDX)
               MOVE WS-PEND-KEY-LEN TO CDD-JDN-KEY-LEN(WS-NEW-IDX)
               MOVE WS-PEND-KEY TO CDD-JDN-KEY(WS-NEW-IDX)
               MOVE 0 TO WS-PEND-KEY-LEN
               MOVE SPACES TO WS-PEND-KEY
               MOVE 0 TO CDD-JDN-VAL-LEN(WS-NEW-IDX)
               MOVE SPACES TO CDD-JDN-VAL(WS-NEW-IDX)
               MOVE 0 TO CDD-JDN-FIRST-CHILD(WS-NEW-IDX)
               MOVE 0 TO CDD-JDN-LAST-CHILD(WS-NEW-IDX)
               MOVE 0 TO CDD-JDN-NEXT-SIB(WS-NEW-IDX)
               MOVE 0 TO CDD-JDN-CHILD-CNT(WS-NEW-IDX)
               IF WS-DEPTH = 0
                   MOVE WS-NEW-IDX TO CDD-JDM-ROOT
               ELSE
                   MOVE WS-STK-NODE(WS-DEPTH) TO WS-PARENT-IDX
                   IF CDD-JDN-FIRST-CHILD(WS-PARENT-IDX) = 0
                       MOVE WS-NEW-IDX
                           TO CDD-JDN-FIRST-CHILD(WS-PARENT-IDX)
                       MOVE WS-NEW-IDX
                           TO CDD-JDN-LAST-CHILD(WS-PARENT-IDX)
                   ELSE
                       MOVE CDD-JDN-LAST-CHILD(WS-PARENT-IDX)
                           TO WS-PREV-LAST
                       MOVE WS-NEW-IDX
                           TO CDD-JDN-NEXT-SIB(WS-PREV-LAST)
                       MOVE WS-NEW-IDX
                           TO CDD-JDN-LAST-CHILD(WS-PARENT-IDX)
                   END-IF
                   ADD 1 TO CDD-JDN-CHILD-CNT(WS-PARENT-IDX)
                   END-ADD
               END-IF
           END-IF.
      * @paragraph PARSE-VALUE-START Create scalar or push container.
       PARSE-VALUE-START.
           EVALUATE WS-CUR-KIND
               WHEN 1
                   PERFORM OPEN-OBJECT-CONTAINER
               WHEN 3
                   PERFORM OPEN-ARRAY-CONTAINER
               WHEN 7
                   PERFORM BUILD-STRING-NODE
               WHEN 8
                   PERFORM BUILD-NUM-NODE-INT
               WHEN 9
                   PERFORM BUILD-NUM-NODE-DEC
               WHEN 10
                   PERFORM ALLOC-DOM-NODE
                   IF CDD-OK
                       MOVE 6 TO CDD-JDN-KIND(WS-NEW-IDX)
                       ADD 1 TO WS-TI
                       END-ADD
                       PERFORM FINISH-IF-ROOT
                   END-IF
               WHEN 11
                   PERFORM ALLOC-DOM-NODE
                   IF CDD-OK
                       MOVE 7 TO CDD-JDN-KIND(WS-NEW-IDX)
                       ADD 1 TO WS-TI
                       END-ADD
                       PERFORM FINISH-IF-ROOT
                   END-IF
               WHEN 12
                   PERFORM ALLOC-DOM-NODE
                   IF CDD-OK
                       MOVE 8 TO CDD-JDN-KIND(WS-NEW-IDX)
                       ADD 1 TO WS-TI
                       END-ADD
                       PERFORM FINISH-IF-ROOT
                   END-IF
               WHEN OTHER
                   PERFORM FAIL-BAD-SYNTAX
           END-EVALUATE.
      * @paragraph OPEN-OBJECT-CONTAINER Push new '{' frame on stack.
       OPEN-OBJECT-CONTAINER.
           IF WS-DEPTH >= WS-EFF-MAXD
               MOVE 1008 TO CDD-ERR-CODE
               MOVE WS-CUR-LINE TO CDD-ERR-LINE
               MOVE WS-CUR-COL TO CDD-ERR-COL
               MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
               MOVE "JSON nesting exceeds CDD-JDM-MAX-DEPTH"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM ALLOC-DOM-NODE
               IF CDD-OK
                   MOVE 1 TO CDD-JDN-KIND(WS-NEW-IDX)
                   ADD 1 TO WS-DEPTH
                   END-ADD
                   MOVE WS-NEW-IDX TO WS-STK-NODE(WS-DEPTH)
                   MOVE 1 TO WS-STK-STATE(WS-DEPTH)
                   ADD 1 TO WS-TI
                   END-ADD
               END-IF
           END-IF.
      * @paragraph OPEN-ARRAY-CONTAINER Push new '[' frame on stack.
       OPEN-ARRAY-CONTAINER.
           IF WS-DEPTH >= WS-EFF-MAXD
               MOVE 1008 TO CDD-ERR-CODE
               MOVE WS-CUR-LINE TO CDD-ERR-LINE
               MOVE WS-CUR-COL TO CDD-ERR-COL
               MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
               MOVE "JSON nesting exceeds CDD-JDM-MAX-DEPTH"
                   TO CDD-ERR-MESSAGE
           ELSE
               PERFORM ALLOC-DOM-NODE
               IF CDD-OK
                   MOVE 2 TO CDD-JDN-KIND(WS-NEW-IDX)
                   ADD 1 TO WS-DEPTH
                   END-ADD
                   MOVE WS-NEW-IDX TO WS-STK-NODE(WS-DEPTH)
                   MOVE 6 TO WS-STK-STATE(WS-DEPTH)
                   ADD 1 TO WS-TI
                   END-ADD
               END-IF
           END-IF.
      * @paragraph BUILD-STRING-NODE Unescape and store STRING node.
       BUILD-STRING-NODE.
           PERFORM UNESCAPE-CUR-STRING
           IF CDD-OK
               PERFORM ALLOC-DOM-NODE
               IF CDD-OK
                   MOVE 3 TO CDD-JDN-KIND(WS-NEW-IDX)
                   IF CDD-SRQ-OUT-LEN > 256
                       MOVE 256 TO CDD-JDN-VAL-LEN(WS-NEW-IDX)
                   ELSE
                       MOVE CDD-SRQ-OUT-LEN
                           TO CDD-JDN-VAL-LEN(WS-NEW-IDX)
                   END-IF
                   IF CDD-JDN-VAL-LEN(WS-NEW-IDX) > 0
                       MOVE CDD-SRQ-OUT-TEXT(
                           1:CDD-JDN-VAL-LEN(WS-NEW-IDX))
                           TO CDD-JDN-VAL(WS-NEW-IDX)(
                           1:CDD-JDN-VAL-LEN(WS-NEW-IDX))
                   END-IF
                   ADD 1 TO WS-TI
                   END-ADD
                   PERFORM FINISH-IF-ROOT
               END-IF
           END-IF.
      * @paragraph BUILD-NUM-NODE-INT Store exact INTEGER token slice.
       BUILD-NUM-NODE-INT.
           PERFORM ALLOC-DOM-NODE
           IF CDD-OK
               MOVE 4 TO CDD-JDN-KIND(WS-NEW-IDX)
               PERFORM COPY-NUM-SLICE
               ADD 1 TO WS-TI
               END-ADD
               PERFORM FINISH-IF-ROOT
           END-IF.
      * @paragraph BUILD-NUM-NODE-DEC Store exact NUMBER token slice.
       BUILD-NUM-NODE-DEC.
           PERFORM ALLOC-DOM-NODE
           IF CDD-OK
               MOVE 5 TO CDD-JDN-KIND(WS-NEW-IDX)
               PERFORM COPY-NUM-SLICE
               ADD 1 TO WS-TI
               END-ADD
               PERFORM FINISH-IF-ROOT
           END-IF.
      * @paragraph COPY-NUM-SLICE Copy raw number text into DOM node.
       COPY-NUM-SLICE.
           IF WS-CUR-LEN > 256
               MOVE 256 TO CDD-JDN-VAL-LEN(WS-NEW-IDX)
           ELSE
               COMPUTE CDD-JDN-VAL-LEN(WS-NEW-IDX) = WS-CUR-LEN
               END-COMPUTE
           END-IF
           MOVE CDD-BUF-DATA(WS-CUR-OFF:CDD-JDN-VAL-LEN(WS-NEW-IDX))
               TO CDD-JDN-VAL(WS-NEW-IDX)(
               1:CDD-JDN-VAL-LEN(WS-NEW-IDX)).
      * @paragraph FAIL-BAD-SYNTAX Populate CDD-ERR-JSON-BAD-TOKEN.
       FAIL-BAD-SYNTAX.
           MOVE 1002 TO CDD-ERR-CODE
           MOVE WS-CUR-LINE TO CDD-ERR-LINE
           MOVE WS-CUR-COL TO CDD-ERR-COL
           MOVE "CDDJPAR" TO CDD-ERR-SUBPROG
           MOVE "Unexpected JSON token" TO CDD-ERR-MESSAGE.
