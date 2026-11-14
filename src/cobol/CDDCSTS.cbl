      * @file CDDCSTS.cbl
      * @brief Lossless byte-exact and surgical-patch CST serializer.
      * @module CDDCSTS
      * @details Traverses CDD-CST iteratively in pre-order to serialize
      *          unmodified subtrees with 100% byte-for-byte fidelity
      *          while applying surgical KEEP, DELETE, SYNTH, and
      *          SPLICE patch modes for trivia-preserving sync updates.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCSTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-SER-STACK Explicit pre-order CST traversal stack.
       01  WS-SER-STACK.
      * @field WS-DEPTH Active traversal stack depth (0..32).
           05  WS-DEPTH                  PIC 9(2) COMP VALUE 0.
      * @field WS-FRAME Traversal stack frame.
           05  WS-FRAME                  OCCURS 32 TIMES.
      * @field WS-STK-PARENT Interior CST node whose children run.
               10  WS-STK-PARENT         PIC 9(4) COMP.
      * @field WS-STK-CHILD Next child CST node to visit (0=done).
               10  WS-STK-CHILD          PIC 9(4) COMP.
      * @record WS-SER-WORK Byte-range copy helpers.
       01  WS-SER-WORK.
      * @field WS-CUR Current CST node being visited.
           05  WS-CUR                    PIC 9(4) COMP VALUE 0.
      * @field WS-PAR Popped parent CST node on frame completion.
           05  WS-PAR                    PIC 9(4) COMP VALUE 0.
      * @field WS-T1 First token index of byte slice to emit.
           05  WS-T1                     PIC 9(8) COMP VALUE 0.
      * @field WS-T2 Last token index of byte slice to emit.
           05  WS-T2                     PIC 9(8) COMP VALUE 0.
      * @field WS-B-START 1-based start byte in CDD-BUF-DATA.
           05  WS-B-START                PIC 9(8) COMP VALUE 0.
      * @field WS-B-END 1-based inclusive end byte in CDD-BUF-DATA.
           05  WS-B-END                  PIC 9(8) COMP VALUE 0.
      * @field WS-B-LEN Byte count to copy into OUT-BUF.
           05  WS-B-LEN                  PIC 9(8) COMP VALUE 0.
      * @field WS-POFF Patch buffer start offset.
           05  WS-POFF                   PIC 9(8) COMP VALUE 0.
      * @field WS-PLEN Patch buffer byte length.
           05  WS-PLEN                   PIC 9(8) COMP VALUE 0.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
      * @record CDD-OUT-BUF Output serialized COBOL byte buffer.
       01  CDD-OUT-BUF.
      * @field CDD-OBF-TAG Magic type-safety tag ('CDDBUF01').
           05  CDD-OBF-TAG               PIC X(8).
      * @field CDD-OBF-TAG-VALID Condition confirming valid OBF tag.
               88  CDD-OBF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-OBF-LEN Active serialized byte length.
           05  CDD-OBF-LEN               PIC 9(8) COMP.
      * @field CDD-OBF-CAP Maximum capacity of CDD-OUT-BUF.
           05  CDD-OBF-CAP               PIC 9(8) COMP.
      * @field CDD-OBF-NL-KIND Line-ending kind (1=LF, 2=CRLF).
           05  CDD-OBF-NL-KIND           PIC 9(2) COMP.
      * @field CDD-OBF-DATA Serialized output COBOL bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDCSTS Serialize lossless or patched CST to bytes.
      * @param [in] CDD-BUF Original immutable COBOL source buffer.
      * @param [in] CDD-CTOK-STREAM Token + trivia slice table.
      * @param [in] CDD-CST Concrete Syntax Tree (with optional edits).
      * @param [inout] CDD-OUT-BUF Target output buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Exact serialized COBOL source in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-GEN-BUF-OVERFLOW.
       PROCEDURE DIVISION USING CDD-BUF CDD-CTOK-STREAM CDD-CST
           CDD-OUT-BUF CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags and traverse CST from root.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID
               OR NOT CDD-CTK-TAG-VALID
               OR NOT CDD-CST-TAG-VALID
               OR NOT CDD-OBF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCSTS" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDCSTS"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-OBF-LEN
           MOVE CDD-BUF-NL-KIND TO CDD-OBF-NL-KIND
           IF CDD-CST-ROOT > 0 AND CDD-CTK-COUNT > 0
               MOVE 1 TO WS-DEPTH
               MOVE CDD-CST-ROOT TO WS-STK-PARENT(1)
               MOVE CDD-CSN-FIRST-CHILD(CDD-CST-ROOT)
                   TO WS-STK-CHILD(1)
               PERFORM WALK-CST-STACK
           END-IF
           EXIT PROGRAM.
      * @paragraph EMIT-TOK-SLICE Copy original bytes [WS-T1..WS-T2].
       EMIT-TOK-SLICE.
           IF CDD-OK AND WS-T2 >= WS-T1
               MOVE CDD-CTI-OFFSET(WS-T1) TO WS-B-START
               COMPUTE WS-B-END =
                   CDD-CTI-OFFSET(WS-T2) + CDD-CTI-LEN(WS-T2) - 1
               END-COMPUTE
               COMPUTE WS-B-LEN = WS-B-END - WS-B-START + 1
               END-COMPUTE
               IF CDD-OBF-LEN + WS-B-LEN > CDD-OBF-CAP
                   MOVE 4001 TO CDD-ERR-CODE
                   MOVE "CDDCSTS" TO CDD-ERR-SUBPROG
                   MOVE "CST serializer output buffer overflow"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE CDD-BUF-DATA(WS-B-START:WS-B-LEN)
                       TO CDD-OBF-DATA(CDD-OBF-LEN + 1:WS-B-LEN)
                   ADD WS-B-LEN TO CDD-OBF-LEN
                   END-ADD
               END-IF
           END-IF.
      * @paragraph EMIT-PATCH-SLICE Copy synthesized patch text.
       EMIT-PATCH-SLICE.
           IF CDD-OK AND WS-PLEN > 0
               IF CDD-OBF-LEN + WS-PLEN > CDD-OBF-CAP
                   MOVE 4001 TO CDD-ERR-CODE
                   MOVE "CDDCSTS" TO CDD-ERR-SUBPROG
                   MOVE "CST serializer patch buffer overflow"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE CDD-CST-PATCH-BUF(WS-POFF:WS-PLEN)
                       TO CDD-OBF-DATA(CDD-OBF-LEN + 1:WS-PLEN)
                   ADD WS-PLEN TO CDD-OBF-LEN
                   END-ADD
               END-IF
           END-IF.
      * @paragraph WALK-CST-STACK Drive iterative pre-order traversal.
       WALK-CST-STACK.
           PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
               MOVE WS-STK-CHILD(WS-DEPTH) TO WS-CUR
               IF WS-CUR = 0
                   MOVE WS-STK-PARENT(WS-DEPTH) TO WS-PAR
                   IF CDD-CSN-FOOT-START(WS-PAR) > 0
                       MOVE CDD-CSN-FOOT-START(WS-PAR) TO WS-T1
                       MOVE CDD-CSN-TOK-END(WS-PAR) TO WS-T2
                       PERFORM EMIT-TOK-SLICE
                   END-IF
                   SUBTRACT 1 FROM WS-DEPTH
                   END-SUBTRACT
               ELSE
                   MOVE CDD-CSN-NEXT-SIB(WS-CUR)
                       TO WS-STK-CHILD(WS-DEPTH)
                   PERFORM VISIT-CST-NODE
               END-IF
           END-PERFORM.
      * @paragraph VISIT-CST-NODE Dispatch KEEP, DELETE, SYNTH, SPLICE.
       VISIT-CST-NODE.
           EVALUATE TRUE
               WHEN CDD-CSN-PM-DELETE(WS-CUR)
                   CONTINUE
               WHEN CDD-CSN-PM-SYNTH(WS-CUR)
                   MOVE CDD-CSN-PATCH-OFF(WS-CUR) TO WS-POFF
                   MOVE CDD-CSN-PATCH-LEN(WS-CUR) TO WS-PLEN
                   PERFORM EMIT-PATCH-SLICE
                   IF CDD-CSN-FIRST-CHILD(WS-CUR) > 0
                       ADD 1 TO WS-DEPTH
                       END-ADD
                       MOVE WS-CUR TO WS-STK-PARENT(WS-DEPTH)
                       MOVE CDD-CSN-FIRST-CHILD(WS-CUR)
                           TO WS-STK-CHILD(WS-DEPTH)
                   END-IF
               WHEN CDD-CSN-PM-SPLICE(WS-CUR)
                   PERFORM SERIALIZE-SPLICED-NODE
               WHEN OTHER
                   PERFORM SERIALIZE-KEEP-NODE
           END-EVALUATE.
      * @paragraph SERIALIZE-KEEP-NODE Emit unmodified leaf or interior.
       SERIALIZE-KEEP-NODE.
           IF CDD-CSN-FIRST-CHILD(WS-CUR) = 0
               MOVE CDD-CSN-TOK-START(WS-CUR) TO WS-T1
               MOVE CDD-CSN-TOK-END(WS-CUR) TO WS-T2
               PERFORM EMIT-TOK-SLICE
           ELSE
               MOVE CDD-CSN-TOK-START(WS-CUR) TO WS-T1
               MOVE CDD-CSN-OWN-END(WS-CUR) TO WS-T2
               PERFORM EMIT-TOK-SLICE
               ADD 1 TO WS-DEPTH
               END-ADD
               MOVE WS-CUR TO WS-STK-PARENT(WS-DEPTH)
               MOVE CDD-CSN-FIRST-CHILD(WS-CUR)
                   TO WS-STK-CHILD(WS-DEPTH)
           END-IF.
      * @paragraph SERIALIZE-SPLICED-NODE Emit prefix + patch + suffix.
       SERIALIZE-SPLICED-NODE.
           MOVE CDD-CSN-TOK-START(WS-CUR) TO WS-T1
           COMPUTE WS-T2 = CDD-CSN-SPLICE-T1(WS-CUR) - 1
           END-COMPUTE
           PERFORM EMIT-TOK-SLICE
           MOVE CDD-CSN-PATCH-OFF(WS-CUR) TO WS-POFF
           MOVE CDD-CSN-PATCH-LEN(WS-CUR) TO WS-PLEN
           PERFORM EMIT-PATCH-SLICE
           IF CDD-CSN-FIRST-CHILD(WS-CUR) = 0
               COMPUTE WS-T1 = CDD-CSN-SPLICE-T2(WS-CUR) + 1
               END-COMPUTE
               MOVE CDD-CSN-TOK-END(WS-CUR) TO WS-T2
               PERFORM EMIT-TOK-SLICE
           ELSE
               COMPUTE WS-T1 = CDD-CSN-SPLICE-T2(WS-CUR) + 1
               END-COMPUTE
               MOVE CDD-CSN-OWN-END(WS-CUR) TO WS-T2
               PERFORM EMIT-TOK-SLICE
               ADD 1 TO WS-DEPTH
               END-ADD
               MOVE WS-CUR TO WS-STK-PARENT(WS-DEPTH)
               MOVE CDD-CSN-FIRST-CHILD(WS-CUR)
                   TO WS-STK-CHILD(WS-DEPTH)
           END-IF.
