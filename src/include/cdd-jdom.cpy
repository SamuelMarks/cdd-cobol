      * @file cdd-jdom.cpy
      * @brief Arena-indexed JSON DOM tree preserving property order.
      * @record CDD-JDOM Non-recursive JSON DOM container envelope.
       01  CDD-JDOM.
      * @field CDD-JDM-TAG Magic type-safety tag ('CDDJDM01').
           05  CDD-JDM-TAG               PIC X(8) VALUE "CDDJDM01".
      * @field CDD-JDM-TAG-VALID Condition confirming valid JDM tag.
               88  CDD-JDM-TAG-VALID              VALUE "CDDJDM01".
      * @field CDD-JDM-MAX-DEPTH Max container nesting depth (1..64).
           05  CDD-JDM-MAX-DEPTH         PIC 9(4) COMP VALUE 32.
      * @field CDD-JDM-REJECT-DUP Duplicate object key policy flag.
           05  CDD-JDM-REJECT-DUP        PIC 9(2) COMP VALUE 1.
      * @field CDD-JDM-DUP-ALLOW Permit duplicate object keys.
               88  CDD-JDM-DUP-ALLOW              VALUE 0.
      * @field CDD-JDM-DUP-REJECT Reject duplicate keys with 1009.
               88  CDD-JDM-DUP-REJECT             VALUE 1.
      * @field CDD-JDM-ROOT 1-based index of root JSON DOM node.
           05  CDD-JDM-ROOT              PIC 9(4) COMP VALUE 0.
      * @field CDD-JDM-COUNT Active allocated DOM node count.
           05  CDD-JDM-COUNT             PIC 9(4) COMP VALUE 0.
      * @field CDD-JDM-CAP Maximum DOM node capacity (1024).
           05  CDD-JDM-CAP               PIC 9(4) COMP VALUE 1024.
      * @field CDD-JDM-NODE Indexed pool of JSON DOM value nodes.
           05  CDD-JDM-NODE              OCCURS 1024 TIMES.
      * @field CDD-JDN-KIND Discriminated JSON value node kind.
               10  CDD-JDN-KIND          PIC 9(2) COMP.
      * @field CDD-JDN-IS-OBJECT JSON object '{...}'.
                   88  CDD-JDN-IS-OBJECT          VALUE 1.
      * @field CDD-JDN-IS-ARRAY JSON array '[...]'.
                   88  CDD-JDN-IS-ARRAY           VALUE 2.
      * @field CDD-JDN-IS-STRING Unescaped UTF-8 string value.
                   88  CDD-JDN-IS-STRING          VALUE 3.
      * @field CDD-JDN-IS-INTEGER Exact integer literal text.
                   88  CDD-JDN-IS-INTEGER         VALUE 4.
      * @field CDD-JDN-IS-NUMBER Decimal/exponent number text.
                   88  CDD-JDN-IS-NUMBER          VALUE 5.
      * @field CDD-JDN-IS-TRUE Boolean true value.
                   88  CDD-JDN-IS-TRUE            VALUE 6.
      * @field CDD-JDN-IS-FALSE Boolean false value.
                   88  CDD-JDN-IS-FALSE           VALUE 7.
      * @field CDD-JDN-IS-NULL JSON null value.
                   88  CDD-JDN-IS-NULL            VALUE 8.
      * @field CDD-JDN-KEY-LEN Length of member key if parent=OBJECT.
               10  CDD-JDN-KEY-LEN       PIC 9(4) COMP.
      * @field CDD-JDN-KEY Unescaped member property key name.
               10  CDD-JDN-KEY           PIC X(128).
      * @field CDD-JDN-VAL-LEN Active byte length in CDD-JDN-VAL.
               10  CDD-JDN-VAL-LEN       PIC 9(4) COMP.
      * @field CDD-JDN-VAL Unescaped string or raw number text.
               10  CDD-JDN-VAL           PIC X(256).
      * @field CDD-JDN-FIRST-CHILD First child node index (0=none).
               10  CDD-JDN-FIRST-CHILD   PIC 9(4) COMP.
      * @field CDD-JDN-LAST-CHILD Last child node index (0=none).
               10  CDD-JDN-LAST-CHILD    PIC 9(4) COMP.
      * @field CDD-JDN-NEXT-SIB Next ordered sibling node (0=none).
               10  CDD-JDN-NEXT-SIB      PIC 9(4) COMP.
      * @field CDD-JDN-CHILD-CNT Direct member/element count.
               10  CDD-JDN-CHILD-CNT     PIC 9(4) COMP.
