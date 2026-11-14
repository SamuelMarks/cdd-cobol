*> @file fixture.cpy
*> @brief ISO COBOL-2014 OCCURS DYNAMIC & FLOAT-DECIMAL-16 fixture.
>>SOURCE FORMAT IS FREE
*> @record ISO2014-REC COBOL-2014 dynamic capacity record.
       01  ISO2014-REC.
*> @field AMOUNT IEEE decimal floating-point field.
           05  AMOUNT USAGE FLOAT-DECIMAL-16.
*> @field SCORES Dynamic-capacity integer table.
           05  SCORES PIC 9(9) OCCURS DYNAMIC CAPACITY IN SCORES-CAP.
