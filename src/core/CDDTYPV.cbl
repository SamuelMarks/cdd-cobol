      * @file CDDTYPV.cbl
      * @brief Runtime envelope type-tag and table subscript validator.
      * @module CDDTYPV
      * @details Enforces strong typing across COBOL-85 subprogram CALL
      *          boundaries by verifying 8-character magic tags and
      *          optional 1-based table bounds prior to access.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDTYPV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDTYPV Validate envelope tag and optional bounds.
      * @param [in] CDD-TYPV-REQ Validation request parameters.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return CDD-OK when valid; error populated otherwise.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-BOUNDS-OUT-OF-RANGE.
       PROCEDURE DIVISION USING CDD-TYPV-REQ CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate request tag, target tag, bounds.
       MAIN-ENTRY.
           IF NOT CDD-TVR-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDTYPV" TO CDD-ERR-SUBPROG
               MOVE "Invalid CDD-TYPV-REQ tag" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           IF CDD-TVR-ACT-TAG NOT = CDD-TVR-EXP-TAG
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE CDD-TVR-CALLER TO CDD-ERR-SUBPROG
               MOVE "Envelope magic tag mismatch" TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           IF CDD-TVR-BOUNDS-YES
               IF CDD-TVR-IDX < CDD-TVR-MIN
                   OR CDD-TVR-IDX > CDD-TVR-MAX
                   MOVE "CDDERR01" TO CDD-ERR-TAG
                   MOVE 9002 TO CDD-ERR-CODE
                   MOVE CDD-TVR-CALLER TO CDD-ERR-SUBPROG
                   MOVE "Table index out of bounds"
                       TO CDD-ERR-MESSAGE
                   EXIT PROGRAM
               END-IF
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           EXIT PROGRAM.
