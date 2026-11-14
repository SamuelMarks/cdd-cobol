      * @file fixture.cpy
      * @brief ANSI COBOL-74 golden fixture with '/' eject & ODO array.
      /
      * @record ANSI74-REC Variable-length ODO record for COBOL-74.
       01  ANSI74-REC.
      * @field LINES-COUNT Active line item count.
           05  LINES-COUNT           PIC 9(4) COMP.
      * @field LINES Variable ODO array.
           05  LINES                 PIC X(16)
                                     OCCURS 1 TO 5 TIMES
                                     DEPENDING ON LINES-COUNT.
