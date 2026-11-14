      * @file fixture.cbl
      * @brief COBOL-61 Extended fixture with 66 RENAMES & REPORT SEC.
      * @module PAY61EXT
      * @details Verifies 66 RENAMES and REPORT SECTION preservation.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAY61EXT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record PAY-REC Payroll record.
       01  PAY-REC.
      * @field EMP-ID Employee ID.
           05  EMP-ID                PICTURE 9(9).
      * @field DEPT Department code.
           05  DEPT                  PICTURE X(8).
      * @field EMP-KEY 66 RENAMES alias.
      * @cdd-keep
           66  EMP-KEY RENAMES EMP-ID.
       REPORT SECTION.
      * @subprogram PAY61EXT Entry point.
      * @return Status 0.
      * @errors None.
       PROCEDURE DIVISION.
      * @paragraph MAIN-P Exit cleanly.
       MAIN-P.
           EXIT PROGRAM.
