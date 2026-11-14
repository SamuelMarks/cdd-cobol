      * @file cdd-types.cpy
      * @brief Core strongly-typed buffer, span, and handle envelopes.
      * @record CDD-BUF Bounded byte buffer with newline metadata.
       01  CDD-BUF.
      * @field CDD-BUF-TAG Magic type-safety tag ('CDDBUF01').
           05  CDD-BUF-TAG               PIC X(8) VALUE "CDDBUF01".
      * @field CDD-BUF-TAG-VALID Condition confirming valid buffer tag.
               88  CDD-BUF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-BUF-LEN Active byte length stored in CDD-BUF-DATA.
           05  CDD-BUF-LEN               PIC 9(8) COMP VALUE 0.
      * @field CDD-BUF-CAP Maximum byte capacity (1048576 bytes).
           05  CDD-BUF-CAP               PIC 9(8) COMP VALUE 1048576.
      * @field CDD-BUF-NL-KIND Detected or target line-ending mode.
           05  CDD-BUF-NL-KIND           PIC 9(2) COMP VALUE 1.
      * @field CDD-BUF-NL-LF Unix LF (X'0A') line endings.
               88  CDD-BUF-NL-LF                  VALUE 1.
      * @field CDD-BUF-NL-CRLF DOS/Mainframe-PC CRLF (X'0D0A') endings.
               88  CDD-BUF-NL-CRLF                VALUE 2.
      * @field CDD-BUF-DATA Raw immutable or output byte storage.
           05  CDD-BUF-DATA              PIC X(1048576) VALUE SPACES.
      * @record CDD-SPAN Byte and line/column source range descriptor.
       01  CDD-SPAN.
      * @field CDD-SPAN-TAG Magic type-safety tag ('CDDSPN01').
           05  CDD-SPAN-TAG              PIC X(8) VALUE "CDDSPN01".
      * @field CDD-SPAN-TAG-VALID Condition confirming valid span tag.
               88  CDD-SPAN-TAG-VALID             VALUE "CDDSPN01".
      * @field CDD-SPAN-OFFSET 1-based starting byte index.
           05  CDD-SPAN-OFFSET           PIC 9(8) COMP VALUE 1.
      * @field CDD-SPAN-LEN Byte length of slice (0 for empty).
           05  CDD-SPAN-LEN              PIC 9(8) COMP VALUE 0.
      * @field CDD-SPAN-START-LINE 1-based start line number.
           05  CDD-SPAN-START-LINE       PIC 9(8) COMP VALUE 1.
      * @field CDD-SPAN-START-COL 1-based start column number.
           05  CDD-SPAN-START-COL        PIC 9(4) COMP VALUE 1.
      * @field CDD-SPAN-END-LINE 1-based end line number.
           05  CDD-SPAN-END-LINE         PIC 9(8) COMP VALUE 1.
      * @field CDD-SPAN-END-COL 1-based end column number.
           05  CDD-SPAN-END-COL          PIC 9(4) COMP VALUE 1.
      * @record CDD-PATH-REQ Typed file path envelope for CDDIO calls.
       01  CDD-PATH-REQ.
      * @field CDD-PATH-TAG Magic type-safety tag ('CDDPTH01').
           05  CDD-PATH-TAG              PIC X(8) VALUE "CDDPTH01".
      * @field CDD-PATH-TAG-VALID Condition confirming valid path tag.
               88  CDD-PATH-TAG-VALID             VALUE "CDDPTH01".
      * @field CDD-PATH-OP Requested I/O operation code.
           05  CDD-PATH-OP               PIC 9(2) COMP VALUE 1.
      * @field CDD-PATH-OP-READ Read entire file or stdin ('-').
               88  CDD-PATH-OP-READ               VALUE 1.
      * @field CDD-PATH-OP-WRITE Write entire buffer to file/stdout.
               88  CDD-PATH-OP-WRITE              VALUE 2.
      * @field CDD-PATH-OP-ATOMIC Atomic write via .cdd.tmp rename.
               88  CDD-PATH-OP-ATOMIC             VALUE 3.
      * @field CDD-PATH-OP-WRITE-FD Write buffer to pre-opened handle.
               88  CDD-PATH-OP-WRITE-FD           VALUE 4.
      * @field CDD-PATH-OP-READ-FD Read buffer from pre-opened handle.
               88  CDD-PATH-OP-READ-FD            VALUE 5.
      * @field CDD-PATH-HNDL Pre-opened 4-byte CBL file handle.
           05  CDD-PATH-HNDL             PIC X(4) VALUE SPACES.
      * @field CDD-PATH-LEN Active character length of path string.
           05  CDD-PATH-LEN              PIC 9(4) COMP VALUE 0.
      * @field CDD-PATH-VAL File filesystem path or '-' for stdio.
           05  CDD-PATH-VAL              PIC X(512) VALUE SPACES.
      * @record CDD-IDENT-30 Legacy COBOL-60..85 30-char identifier.
       01  CDD-IDENT-30.
      * @field CDD-ID30-TAG Magic type-safety tag ('CDDID30T').
           05  CDD-ID30-TAG              PIC X(8) VALUE "CDDID30T".
      * @field CDD-ID30-TAG-VALID Condition confirming ID30 tag.
               88  CDD-ID30-TAG-VALID             VALUE "CDDID30T".
      * @field CDD-ID30-LEN Active identifier length (1..30).
           05  CDD-ID30-LEN              PIC 9(2) COMP VALUE 0.
      * @field CDD-ID30-VAL Identifier characters.
           05  CDD-ID30-VAL              PIC X(30) VALUE SPACES.
      * @record CDD-IDENT-63 Modern COBOL-2002..2023 63-char ident.
       01  CDD-IDENT-63.
      * @field CDD-ID63-TAG Magic type-safety tag ('CDDID63T').
           05  CDD-ID63-TAG              PIC X(8) VALUE "CDDID63T".
      * @field CDD-ID63-TAG-VALID Condition confirming ID63 tag.
               88  CDD-ID63-TAG-VALID             VALUE "CDDID63T".
      * @field CDD-ID63-LEN Active identifier length (1..63).
           05  CDD-ID63-LEN              PIC 9(2) COMP VALUE 0.
      * @field CDD-ID63-VAL Identifier characters.
           05  CDD-ID63-VAL              PIC X(63) VALUE SPACES.
      * @record CDD-JSON-KEY Bounded JSON object property key.
       01  CDD-JSON-KEY.
      * @field CDD-JKEY-TAG Magic type-safety tag ('CDDJKEY1').
           05  CDD-JKEY-TAG              PIC X(8) VALUE "CDDJKEY1".
      * @field CDD-JKEY-TAG-VALID Condition confirming JKEY tag.
               88  CDD-JKEY-TAG-VALID             VALUE "CDDJKEY1".
      * @field CDD-JKEY-LEN Active property key byte length (0..128).
           05  CDD-JKEY-LEN              PIC 9(4) COMP VALUE 0.
      * @field CDD-JKEY-VAL Property key UTF-8 bytes.
           05  CDD-JKEY-VAL              PIC X(128) VALUE SPACES.
