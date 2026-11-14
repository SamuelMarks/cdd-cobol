      * @file cdd-cst.cpy
      * @brief Lossless Red/Green COBOL CST & PICTURE parser envelopes.
      * @record CDD-CPIC-INFO Parsed PICTURE, USAGE, and OCCURS facets.
       01  CDD-CPIC-INFO.
      * @field CDD-CPI-TAG Magic type-safety tag ('CDDCPI01').
           05  CDD-CPI-TAG               PIC X(8) VALUE "CDDCPI01".
      * @field CDD-CPI-TAG-VALID Condition confirming valid CPI tag.
               88  CDD-CPI-TAG-VALID              VALUE "CDDCPI01".
      * @field CDD-CPI-OP Parser operation (1=PIC str, 2=token span).
           05  CDD-CPI-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-CPI-OP-PIC-STR Parse standalone CDD-CPI-PIC-IN.
               88  CDD-CPI-OP-PIC-STR             VALUE 1.
      * @field CDD-CPI-OP-CLAUSES Scan token span [TOK-START..TOK-END].
               88  CDD-CPI-OP-CLAUSES             VALUE 2.
      * @field CDD-CPI-TOK-START First token index of field item.
           05  CDD-CPI-TOK-START         PIC 9(8) COMP VALUE 0.
      * @field CDD-CPI-TOK-END Last token index of field item.
           05  CDD-CPI-TOK-END           PIC 9(8) COMP VALUE 0.
      * @field CDD-CPI-PIC-IN-LEN Input PIC string length for OP 1.
           05  CDD-CPI-PIC-IN-LEN        PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-PIC-IN Raw PIC character-string input/extracted.
           05  CDD-CPI-PIC-IN            PIC X(32) VALUE SPACES.
      * @field CDD-CPI-HAS-PIC 1 if PIC/PICTURE or COBOL-60 SIZE seen.
           05  CDD-CPI-HAS-PIC           PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-KIND Resolved canonical IR scalar/group kind.
           05  CDD-CPI-KIND              PIC 9(2) COMP VALUE 1.
      * @field CDD-CPI-SIGNED 1 if signed ('S' or SIGNED or +/-).
           05  CDD-CPI-SIGNED            PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-MAX-LEN Resolved string length (0=ANY/DYNAMIC).
           05  CDD-CPI-MAX-LEN           PIC 9(8) COMP VALUE 0.
      * @field CDD-CPI-DIGITS Total integer + fractional digits.
           05  CDD-CPI-DIGITS            PIC 9(4) COMP VALUE 0.
      * @field CDD-CPI-SCALE Implied decimal scale digits.
           05  CDD-CPI-SCALE             PIC 9(4) COMP VALUE 0.
      * @field CDD-CPI-USAGE Resolved CDD-IRF-USAGE code (0..11).
           05  CDD-CPI-USAGE             PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-ARR-MODE Array mode (0=none,1=fix,2=odo,3=dyn).
           05  CDD-CPI-ARR-MODE          PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-ARR-MIN Minimum array bound.
           05  CDD-CPI-ARR-MIN           PIC 9(6) COMP VALUE 0.
      * @field CDD-CPI-ARR-MAX Maximum array bound.
           05  CDD-CPI-ARR-MAX           PIC 9(6) COMP VALUE 0.
      * @field CDD-CPI-IS-TYPEDEF 1 if IS TYPEDEF [STRONG] present.
           05  CDD-CPI-IS-TYPEDEF        PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-SYNC-JUST Bitmask (1=SYNC, 2=JUSTIFIED).
           05  CDD-CPI-SYNC-JUST         PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-REDEF-LEN Length of REDEFINES/RENAMES target.
           05  CDD-CPI-REDEF-LEN         PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-REDEFINES Target field of REDEFINES/RENAMES.
           05  CDD-CPI-REDEFINES         PIC X(64) VALUE SPACES.
      * @field CDD-CPI-VAL-LEN Length of extracted VALUE literal.
           05  CDD-CPI-VAL-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-CPI-VAL-IS-STR 1 if VALUE literal was quoted string.
           05  CDD-CPI-VAL-IS-STR        PIC 9(2) COMP VALUE 0.
      * @field CDD-CPI-VAL-TEXT Extracted unquoted VALUE literal.
           05  CDD-CPI-VAL-TEXT          PIC X(64) VALUE SPACES.
      * @field CDD-CPI-TYPE-TOK1 First token of PIC/SIZE/USAGE/OCCURS.
           05  CDD-CPI-TYPE-TOK1         PIC 9(8) COMP VALUE 0.
      * @field CDD-CPI-TYPE-TOK2 Last token of PIC/SIZE/USAGE/OCCURS.
           05  CDD-CPI-TYPE-TOK2         PIC 9(8) COMP VALUE 0.
      * @record CDD-CST Lossless COBOL Concrete Syntax Tree envelope.
       01  CDD-CST.
      * @field CDD-CST-TAG Magic type-safety tag ('CDDCST01').
           05  CDD-CST-TAG               PIC X(8) VALUE "CDDCST01".
      * @field CDD-CST-TAG-VALID Condition confirming valid CST tag.
               88  CDD-CST-TAG-VALID              VALUE "CDDCST01".
      * @field CDD-CST-ROOT Root compilation-unit node index (1).
           05  CDD-CST-ROOT              PIC 9(4) COMP VALUE 0.
      * @field CDD-CST-COUNT Active allocated CST node count.
           05  CDD-CST-COUNT             PIC 9(4) COMP VALUE 0.
      * @field CDD-CST-CAP Maximum CST node pool capacity (512).
           05  CDD-CST-CAP               PIC 9(4) COMP VALUE 512.
      * @field CDD-CST-PATCH-LEN Active bytes in CDD-CST-PATCH-BUF.
           05  CDD-CST-PATCH-LEN         PIC 9(8) COMP VALUE 0.
      * @field CDD-CST-PATCH-CAP Capacity of patch text pool (131072).
           05  CDD-CST-PATCH-CAP         PIC 9(8) COMP VALUE 131072.
      * @field CDD-CST-PATCH-BUF Synthesized text pool for sync edits.
           05  CDD-CST-PATCH-BUF         PIC X(131072) VALUE SPACES.
      * @field CDD-CST-NODE Pool of lossless CST interior & leaf nodes.
           05  CDD-CST-NODE              OCCURS 512 TIMES.
      * @field CDD-CSN-KIND Discriminated CST node kind.
               10  CDD-CSN-KIND          PIC 9(2) COMP.
      * @field CDD-CSN-IS-ROOT Compilation unit root.
                   88  CDD-CSN-IS-ROOT            VALUE 1.
      * @field CDD-CSN-IS-OPAQUE Verbatim untouched source chunk.
                   88  CDD-CSN-IS-OPAQUE          VALUE 2.
      * @field CDD-CSN-IS-RECORD 01-level COBOL record definition.
                   88  CDD-CSN-IS-RECORD          VALUE 3.
      * @field CDD-CSN-IS-TYPEDEF COBOL-2002+ 01 IS TYPEDEF STRONG.
                   88  CDD-CSN-IS-TYPEDEF         VALUE 4.
      * @field CDD-CSN-IS-CLASS COBOL-2002+ CLASS-ID definition.
                   88  CDD-CSN-IS-CLASS           VALUE 5.
      * @field CDD-CSN-IS-FIELD Subordinate 02..49 or 77 data field.
                   88  CDD-CSN-IS-FIELD           VALUE 6.
      * @field CDD-CSN-IS-COND88 88-level condition-name entry.
                   88  CDD-CSN-IS-COND88          VALUE 7.
      * @field CDD-CSN-IS-REN66 66-level RENAMES entry.
                   88  CDD-CSN-IS-REN66           VALUE 8.
      * @field CDD-CSN-IS-ROUTINE Procedure/method/function routine.
                   88  CDD-CSN-IS-ROUTINE         VALUE 9.
      * @field CDD-CSN-PARENT Parent CST node index (0=root).
               10  CDD-CSN-PARENT        PIC 9(4) COMP.
      * @field CDD-CSN-FIRST-CHILD First child CST node (0=leaf).
               10  CDD-CSN-FIRST-CHILD   PIC 9(4) COMP.
      * @field CDD-CSN-LAST-CHILD Last child CST node (0=leaf).
               10  CDD-CSN-LAST-CHILD    PIC 9(4) COMP.
      * @field CDD-CSN-NEXT-SIB Next ordered sibling CST node (0=none).
               10  CDD-CSN-NEXT-SIB      PIC 9(4) COMP.
      * @field CDD-CSN-TOK-START First token+trivia index of subtree.
               10  CDD-CSN-TOK-START     PIC 9(8) COMP.
      * @field CDD-CSN-LEAD-END Last leading-trivia token index.
               10  CDD-CSN-LEAD-END      PIC 9(8) COMP.
      * @field CDD-CSN-CODE-START First semantic code token of header.
               10  CDD-CSN-CODE-START    PIC 9(8) COMP.
      * @field CDD-CSN-CODE-END Last semantic code token of header.
               10  CDD-CSN-CODE-END      PIC 9(8) COMP.
      * @field CDD-CSN-OWN-END End of header trailing same-line trivia.
               10  CDD-CSN-OWN-END       PIC 9(8) COMP.
      * @field CDD-CSN-FOOT-START Start of footer (e.g. END CLASS) 0=no.
               10  CDD-CSN-FOOT-START    PIC 9(8) COMP.
      * @field CDD-CSN-TOK-END Final token+trivia index of subtree.
               10  CDD-CSN-TOK-END       PIC 9(8) COMP.
      * @field CDD-CSN-LEVEL COBOL level number (01..49/66/77/88).
               10  CDD-CSN-LEVEL         PIC 9(2) COMP.
      * @field CDD-CSN-NAME-LEN Length of CDD-CSN-NAME.
               10  CDD-CSN-NAME-LEN      PIC 9(2) COMP.
      * @field CDD-CSN-NAME Uppercased UDT, field, or routine name.
               10  CDD-CSN-NAME          PIC X(64).
      * @field CDD-CSN-JKEY-LEN Length of @json-name override if set.
               10  CDD-CSN-JKEY-LEN      PIC 9(4) COMP.
      * @field CDD-CSN-JSON-KEY Explicit * @json-name annotation.
               10  CDD-CSN-JSON-KEY      PIC X(128).
      * @field CDD-CSN-DOC-LEN Length of extracted doc comment text.
               10  CDD-CSN-DOC-LEN       PIC 9(4) COMP.
      * @field CDD-CSN-DOC-TEXT Extracted doc/inline comment summary.
               10  CDD-CSN-DOC-TEXT      PIC X(160).
      * @field CDD-CSN-KEEP-FLAG 1 if * @cdd-keep annotation present.
               10  CDD-CSN-KEEP-FLAG     PIC 9(2) COMP.
      * @field CDD-CSN-PATCH-MODE Serializer edit state (0..3).
               10  CDD-CSN-PATCH-MODE    PIC 9(2) COMP.
      * @field CDD-CSN-PM-KEEP Emit exact unmodified original bytes.
                   88  CDD-CSN-PM-KEEP            VALUE 0.
      * @field CDD-CSN-PM-DELETE Omit deleted node and its children.
                   88  CDD-CSN-PM-DELETE          VALUE 1.
      * @field CDD-CSN-PM-SYNTH Emit synthesized CDD-CST-PATCH-BUF.
                   88  CDD-CSN-PM-SYNTH           VALUE 2.
      * @field CDD-CSN-PM-SPLICE Splice [SPLICE-T1..T2] with patch.
                   88  CDD-CSN-PM-SPLICE          VALUE 3.
      * @field CDD-CSN-SPLICE-T1 First token replaced in PM-SPLICE.
               10  CDD-CSN-SPLICE-T1     PIC 9(8) COMP.
      * @field CDD-CSN-SPLICE-T2 Last token replaced in PM-SPLICE.
               10  CDD-CSN-SPLICE-T2     PIC 9(8) COMP.
      * @field CDD-CSN-PATCH-OFF 1-based offset in CDD-CST-PATCH-BUF.
               10  CDD-CSN-PATCH-OFF     PIC 9(8) COMP.
      * @field CDD-CSN-PATCH-LEN Byte length in CDD-CST-PATCH-BUF.
               10  CDD-CSN-PATCH-LEN     PIC 9(8) COMP.
      * @record CDD-CPRO-INFO Routine boundary locator query/result.
       01  CDD-CPRO-INFO.
      * @field CDD-CPR-TAG Magic type-safety tag ('CDDCPR01').
           05  CDD-CPR-TAG               PIC X(8) VALUE "CDDCPR01".
      * @field CDD-CPR-TAG-VALID Condition confirming valid CPR tag.
               88  CDD-CPR-TAG-VALID              VALUE "CDDCPR01".
      * @field CDD-CPR-UDT-NLEN Length of target UDT COBOL name.
           05  CDD-CPR-UDT-NLEN          PIC 9(2) COMP VALUE 0.
      * @field CDD-CPR-UDT-NAME Target UDT name (e.g. CUSTOMER-REC).
           05  CDD-CPR-UDT-NAME          PIC X(64) VALUE SPACES.
      * @field CDD-CPR-TO-NODE CST node of <UDT>-TO-JSON (0=absent).
           05  CDD-CPR-TO-NODE           PIC 9(4) COMP VALUE 0.
      * @field CDD-CPR-TO-B1 First body token inside TO-JSON routine.
           05  CDD-CPR-TO-B1             PIC 9(8) COMP VALUE 0.
      * @field CDD-CPR-TO-B2 Last body token inside TO-JSON routine.
           05  CDD-CPR-TO-B2             PIC 9(8) COMP VALUE 0.
      * @field CDD-CPR-FROM-NODE CST node of <UDT>-FROM-JSON (0=none).
           05  CDD-CPR-FROM-NODE         PIC 9(4) COMP VALUE 0.
      * @field CDD-CPR-FROM-B1 First body token inside FROM-JSON.
           05  CDD-CPR-FROM-B1           PIC 9(8) COMP VALUE 0.
      * @field CDD-CPR-FROM-B2 Last body token inside FROM-JSON.
           05  CDD-CPR-FROM-B2           PIC 9(8) COMP VALUE 0.
      * @field CDD-CPR-KIND Detected routine kind (1=sec,2=par,3=sub,4=m,5=fn).
           05  CDD-CPR-KIND              PIC 9(2) COMP VALUE 0.
