      * @file CDDAREN.cbl
      * @brief Deterministic fixed-capacity pool/arena slot manager.
      * @module CDDAREN
      * @details Manages initialization, reset, and safe bounds-checked
      *          slot allocation for DOM, CST, and IR node pools.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDAREN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDAREN Initialize, reset, or allocate pool slot.
      * @param [inout] CDD-ARENA-CTL Pool control state envelope.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Allocated 1-based slot index in CDD-ARN-OUT-IDX.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-UNINITIALIZED-ARENA.
       PROCEDURE DIVISION USING CDD-ARENA-CTL CDD-ERROR.
      * @paragraph MAIN-ENTRY Dispatch pool control operation.
       MAIN-ENTRY.
           IF NOT CDD-ARN-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDAREN" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-ARENA-CTL tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           EVALUATE TRUE
               WHEN CDD-ARN-OP-INIT
                   MOVE 1 TO CDD-ARN-READY
                   MOVE 0 TO CDD-ARN-USED
                   MOVE 0 TO CDD-ARN-OUT-IDX
               WHEN CDD-ARN-OP-RESET
                   PERFORM DO-RESET
               WHEN OTHER
                   PERFORM DO-ALLOC
           END-EVALUATE
           EXIT PROGRAM.
      * @paragraph DO-RESET Reset pool usage if initialized.
       DO-RESET.
           IF CDD-ARN-IS-UNINIT
               MOVE 9003 TO CDD-ERR-CODE
               MOVE "CDDAREN" TO CDD-ERR-SUBPROG
               MOVE "Pool reset before initialization"
                   TO CDD-ERR-MESSAGE
           ELSE
               MOVE 0 TO CDD-ARN-USED
               MOVE 0 TO CDD-ARN-OUT-IDX
           END-IF.
      * @paragraph DO-ALLOC Allocate next slot within capacity.
       DO-ALLOC.
           IF CDD-ARN-IS-UNINIT
               MOVE 9003 TO CDD-ERR-CODE
               MOVE "CDDAREN" TO CDD-ERR-SUBPROG
               MOVE "Pool alloc before initialization"
                   TO CDD-ERR-MESSAGE
           ELSE
               IF CDD-ARN-USED >= CDD-ARN-CAP
                   MOVE CDD-ARN-CAPACITY-CODE TO CDD-ERR-CODE
                   MOVE "CDDAREN" TO CDD-ERR-SUBPROG
                   MOVE "Pool capacity exhausted" TO CDD-ERR-MESSAGE
               ELSE
                   ADD 1 TO CDD-ARN-USED
                   END-ADD
                   MOVE CDD-ARN-USED TO CDD-ARN-OUT-IDX
               END-IF
           END-IF.
