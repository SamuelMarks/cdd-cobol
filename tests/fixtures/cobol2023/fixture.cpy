*> @file fixture.cpy
*> @brief ISO COBOL-2023 PIC U UTF-8 & DYNAMIC string fixture.
>>SOURCE FORMAT IS FREE
*> @record ISO2023-REC COBOL-2023 UTF-8 record.
       01  ISO2023-REC.
*> @field UTF8-TITLE Fixed UTF-8 string.
           05  UTF8-TITLE PIC U(24).
*> @field DYN-BODY Dynamic-length UTF-8 string.
           05  DYN-BODY PIC U DYNAMIC.
