      * @file fixture.cpy
      * @brief COBOL-65 golden fixture with OCCURS ... INDEXED BY.
      * @record TABLE-REC Indexed table UDT for COBOL-65.
       01  TABLE-REC.
      * @field CODES Indexed code array.
           05  CODES                 PIC X(6) OCCURS 4 TIMES
                                     INDEXED BY CODES-IDX.
