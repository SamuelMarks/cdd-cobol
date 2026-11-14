*> @file fixture.cpy
*> @brief ISO COBOL-2002 free-format IS TYPEDEF STRONG fixture.
>>SOURCE FORMAT IS FREE
*> @typedef ISO2002-T Strongly-typed COBOL-2002 record.
       01  ISO2002-T IS TYPEDEF STRONG.
*> @field FLAG Bit boolean item.
           05  FLAG PIC 1 USAGE BIT.
*> @field NAT-NAME National string item.
           05  NAT-NAME PIC N(16).
