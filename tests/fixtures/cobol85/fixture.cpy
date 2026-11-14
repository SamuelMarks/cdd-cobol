      * @file fixture.cpy
      * @brief ANSI COBOL-85 golden fixture with 88 boolean conditions.
      * @record ANSI85-REC Structured ANSI-85 record.
       01  ANSI85-REC.
      * @field CUST-ID Customer number.
           05  CUST-ID               PIC 9(9).
      * @field ACTIVE Boolean flag.
           05  ACTIVE                PIC X(1).
      * @field ACTIVE-TRUE True state.
               88  ACTIVE-TRUE                  VALUE 'T'.
      * @field ACTIVE-FALSE False state.
               88  ACTIVE-FALSE                 VALUE 'F'.
