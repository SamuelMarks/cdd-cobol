      * @file fixture.cbl
      * @brief ANSI COBOL-68 golden fixture with D debug, LINKAGE, CALL.
      * @module ANSI68M
      * @details Standardized ANSI-68 LINKAGE SECTION and D col-7 line.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANSI68M.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
      * @record ANSI68-REC Linkage record.
       01  ANSI68-REC.
      * @field SEQ-ID Computational integer.
           05  SEQ-ID                PIC 9(9) USAGE COMPUTATIONAL.
      * @field LABEL-FLD Display string.
      * @json-name label
           05  LABEL-FLD             PIC X(12) USAGE DISPLAY.
      * @subprogram ANSI68M Subprogram entry.
      * @param [inout] ANSI68-REC Linkage record.
      * @return Updated ANSI68-REC.
      * @errors None.
       PROCEDURE DIVISION USING ANSI68-REC.
      * @paragraph MAIN-68 Debug line and return.
       MAIN-68.
      D    MOVE 1 TO SEQ-ID.
           EXIT PROGRAM.
