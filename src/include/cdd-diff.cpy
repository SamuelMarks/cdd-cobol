      * @file cdd-diff.cpy
      * @brief Naming, IR mapper, CST field-map, and diff envelopes.
      * @record CDD-NAM-REQ Request/response envelope for CDDNAM.
       01  CDD-NAM-REQ.
      * @field CDD-NAM-TAG Magic type-safety tag ('CDDNAM01').
           05  CDD-NAM-TAG               PIC X(8) VALUE "CDDNAM01".
      * @field CDD-NAM-TAG-VALID Condition confirming valid NAM tag.
               88  CDD-NAM-TAG-VALID              VALUE "CDDNAM01".
      * @field CDD-NAM-OP Direction (1=JSON->COBOL, 2=COBOL->JSON).
           05  CDD-NAM-OP                PIC 9(2) COMP VALUE 1.
      * @field CDD-NAM-OP-J2C Convert JSON key to COBOL identifier.
               88  CDD-NAM-OP-J2C                 VALUE 1.
      * @field CDD-NAM-OP-C2J Convert COBOL name to JSON key.
               88  CDD-NAM-OP-C2J                 VALUE 2.
      * @field CDD-NAM-DIALECT Target COBOL standard ID (1..10).
           05  CDD-NAM-DIALECT           PIC 9(2) COMP VALUE 7.
      * @field CDD-NAM-CASE JSON key case style for C2J (1..4).
           05  CDD-NAM-CASE              PIC 9(2) COMP VALUE 1.
      * @field CDD-NAM-CASE-CAMEL camelCase JSON keys.
               88  CDD-NAM-CASE-CAMEL             VALUE 1.
      * @field CDD-NAM-CASE-SNAKE snake_case JSON keys.
               88  CDD-NAM-CASE-SNAKE             VALUE 2.
      * @field CDD-NAM-CASE-KEBAB kebab-case JSON keys.
               88  CDD-NAM-CASE-KEBAB             VALUE 3.
      * @field CDD-NAM-CASE-PRESERVE Preserve verbatim COBOL name.
               88  CDD-NAM-CASE-PRESERVE          VALUE 4.
      * @field CDD-NAM-IN-LEN Active length of CDD-NAM-IN.
           05  CDD-NAM-IN-LEN            PIC 9(4) COMP VALUE 0.
      * @field CDD-NAM-IN Input JSON key or COBOL identifier.
           05  CDD-NAM-IN                PIC X(128) VALUE SPACES.
      * @field CDD-NAM-OUT-LEN Active length of CDD-NAM-OUT.
           05  CDD-NAM-OUT-LEN           PIC 9(4) COMP VALUE 0.
      * @field CDD-NAM-OUT Output COBOL identifier or JSON key.
           05  CDD-NAM-OUT               PIC X(128) VALUE SPACES.
      * @record CDD-IR-MAP-REQ Configuration for CDDJ2IR and CDDC2IR.
       01  CDD-IR-MAP-REQ.
      * @field CDD-MPR-TAG Magic type-safety tag ('CDDMPR01').
           05  CDD-MPR-TAG               PIC X(8) VALUE "CDDMPR01".
      * @field CDD-MPR-TAG-VALID Condition confirming valid MPR tag.
               88  CDD-MPR-TAG-VALID              VALUE "CDDMPR01".
      * @field CDD-MPR-DIALECT Target COBOL dialect (1..10).
           05  CDD-MPR-DIALECT           PIC 9(2) COMP VALUE 7.
      * @field CDD-MPR-UDT-KIND Target UDT construct (1=rec,2=td,3=cl).
           05  CDD-MPR-UDT-KIND          PIC 9(2) COMP VALUE 1.
      * @field CDD-MPR-JSON-MODE JSON input mode (0=auto,1=sch,2=smpl).
           05  CDD-MPR-JSON-MODE         PIC 9(2) COMP VALUE 0.
      * @field CDD-MPR-CASE JSON naming case (1=camel..4=preserve).
           05  CDD-MPR-CASE              PIC 9(2) COMP VALUE 1.
      * @field CDD-MPR-STRICT-UDT 1 if multiple UDTs require --udt.
           05  CDD-MPR-STRICT-UDT        PIC 9(2) COMP VALUE 0.
      * @field CDD-MPR-NAME-LEN Length of explicit target UDT name.
           05  CDD-MPR-NAME-LEN          PIC 9(2) COMP VALUE 0.
      * @field CDD-MPR-TARGET-NAME Explicit --name / --udt identifier.
           05  CDD-MPR-TARGET-NAME       PIC X(64) VALUE SPACES.
      * @field CDD-MPR-MATCHED-CST Matched UDT CST node index.
           05  CDD-MPR-MATCHED-CST       PIC 9(4) COMP VALUE 0.
      * @record CDD-IR-FIELD-MAP Back-links from CDD-IR to CDD-CST.
       01  CDD-IR-FIELD-MAP.
      * @field CDD-FMP-TAG Magic type-safety tag ('CDDFMP01').
           05  CDD-FMP-TAG               PIC X(8) VALUE "CDDFMP01".
      * @field CDD-FMP-TAG-VALID Condition confirming valid FMP tag.
               88  CDD-FMP-TAG-VALID              VALUE "CDDFMP01".
      * @field CDD-FMP-ENTRY Back-link slot per CDD-IR-FIELD (1..256).
           05  CDD-FMP-ENTRY             OCCURS 256 TIMES.
      * @field CDD-FMP-CST-NODE CST node index of parsed field.
               10  CDD-FMP-CST-NODE      PIC 9(4) COMP.
      * @field CDD-FMP-TYPE-TOK1 First token of field PIC/USAGE/OCCURS.
               10  CDD-FMP-TYPE-TOK1     PIC 9(8) COMP.
      * @field CDD-FMP-TYPE-TOK2 Last token of field PIC/USAGE/OCCURS.
               10  CDD-FMP-TYPE-TOK2     PIC 9(8) COMP.
      * @field CDD-FMP-COMP-CST Companion ODO/null CST node (0=none).
               10  CDD-FMP-COMP-CST      PIC 9(4) COMP.
      * @record CDD-DIFF-PLAN Ordered structural sync patch plan.
       01  CDD-DIFF-PLAN.
      * @field CDD-DIF-TAG Magic type-safety tag ('CDDDIF01').
           05  CDD-DIF-TAG               PIC X(8) VALUE "CDDDIF01".
      * @field CDD-DIF-TAG-VALID Condition confirming valid DIF tag.
               88  CDD-DIF-TAG-VALID              VALUE "CDDDIF01".
      * @field CDD-DIF-COUNT Active patch operation count.
           05  CDD-DIF-COUNT             PIC 9(4) COMP VALUE 0.
      * @field CDD-DIF-CAP Maximum patch operation capacity (512).
           05  CDD-DIF-CAP               PIC 9(4) COMP VALUE 512.
      * @field CDD-DIF-ITEM Ordered structural diff operation slot.
           05  CDD-DIF-ITEM              OCCURS 512 TIMES.
      * @field CDD-DFO-KIND Diff operation code (1..5).
               10  CDD-DFO-KIND          PIC 9(2) COMP.
      * @field CDD-DFO-KEEP Field identical -> touch zero CST bytes.
                   88  CDD-DFO-KEEP               VALUE 1.
      * @field CDD-DFO-UPDATE Field type changed -> splice clause.
                   88  CDD-DFO-UPDATE             VALUE 2.
      * @field CDD-DFO-INSERT New JSON field -> insert after anchor.
                   88  CDD-DFO-INSERT             VALUE 3.
      * @field CDD-DFO-REMOVE Removed JSON field -> delete CST subtree.
                   88  CDD-DFO-REMOVE             VALUE 4.
      * @field CDD-DFO-GROUP Nested group -> keep header, diff kids.
                   88  CDD-DFO-GROUP              VALUE 5.
      * @field CDD-DFO-OLD-IR Index in existing COBOL IR (0=insert).
               10  CDD-DFO-OLD-IR        PIC 9(4) COMP.
      * @field CDD-DFO-NEW-IR Index in target JSON IR (0=remove).
               10  CDD-DFO-NEW-IR        PIC 9(4) COMP.
      * @field CDD-DFO-CST-NODE Target CST node (or insert anchor).
               10  CDD-DFO-CST-NODE      PIC 9(4) COMP.
      * @field CDD-DFO-PARENT-CST Enclosing parent CST group/record.
               10  CDD-DFO-PARENT-CST    PIC 9(4) COMP.
