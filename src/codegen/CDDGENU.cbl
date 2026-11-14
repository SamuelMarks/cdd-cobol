      * @file CDDGENU.cbl
      * @brief Feature 0 (emit): Canonical IR -> 10-dialect COBOL UDT.
      * @module CDDGENU
      * @details Emits 100% doc-covered fixed-format or free-format
      *          COBOL 01 records, COBOL-2002+ IS TYPEDEF STRONG types,
      *          and CLASS-ID classes across all 10 COBOL standards
      *          (COBOL-60 through COBOL-2023).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDGENU.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-dial.cpy".
      * @record WS-BYTE-CONV Big-endian LF/CR byte helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-GEN-STACK Non-recursive IR field tree traversal.
       01  WS-GEN-STACK.
      * @field WS-DEPTH Active field nesting depth (0..32).
           05  WS-DEPTH                  PIC 9(2) COMP VALUE 0.
      * @field WS-STK-IDX Next sibling IR field index at depth.
           05  WS-STK-IDX                PIC 9(4) COMP OCCURS 32 TIMES.
      * @record WS-GEN-STATE Formatting cursors and clause buffers.
       01  WS-GEN-STATE.
      * @field WS-LF Cached LF byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR Cached CR byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-IS-FREE 1 if emitting free format, 0 if fixed.
           05  WS-IS-FREE                PIC 9(2) COMP VALUE 0.
      * @field WS-FIDX Current IR field index being emitted.
           05  WS-FIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-LVL-NUM Computed COBOL level (05, 10, 15...).
           05  WS-LVL-NUM                PIC 9(2) COMP VALUE 5.
      * @field WS-LVL-DISP Two-digit display COBOL level ('05', '10').
           05  WS-LVL-DISP               PIC 9(2) VALUE 05.
      * @field WS-INDENT-COL 1-based starting column for field level.
           05  WS-INDENT-COL             PIC 9(4) COMP VALUE 12.
      * @field WS-LPOS Write cursor in WS-LINE-BUF.
           05  WS-LPOS                   PIC 9(4) COMP VALUE 1.
      * @field WS-CLEN Built clause string length.
           05  WS-CLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-SLEN Scratch slice length.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-INT-DIGITS Integer digits before V (DIGITS - SCALE).
           05  WS-INT-DIGITS             PIC 9(4) COMP VALUE 0.
      * @field WS-EIDX Enum cursor in CDD-IR-ENUM.
           05  WS-EIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-ECNT Enum counter for current field.
           05  WS-ECNT                   PIC 9(4) COMP VALUE 0.
      * @field WS-NUM-TXT Formatted unsigned integer scratch.
           05  WS-NUM-TXT                PIC X(16) VALUE SPACES.
      * @field WS-NUM-LEN Length of WS-NUM-TXT.
           05  WS-NUM-LEN                PIC 9(2) COMP VALUE 0.
      * @field WS-NUM-U32 Input integer to format via CDDSTR.
           05  WS-NUM-U32                PIC 9(8) COMP VALUE 0.
      * @record WS-LINE-BUF Single COBOL line builder.
       01  WS-LINE-BUF                   PIC X(256) VALUE SPACES.
      * @record WS-CLAUSE-BUF Built PIC/USAGE/OCCURS/VALUE clause.
       01  WS-CLAUSE-BUF                 PIC X(160) VALUE SPACES.
      * @record WS-CMT-BODY Comment payload after '* ' or '*> '.
       01  WS-CMT-BODY                   PIC X(160) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDGENU Emit dialect-specific COBOL UDT from CDD-IR.
      * @param [in] CDD-SDK-OPTS Dialect, format, UDT kind, verbose opt.
      * @param [in] CDD-IR Canonical IR definition to emit.
      * @param [inout] CDD-BUF Output COBOL source buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return 100% doc-covered COBOL UDT in CDD-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-COB-DIALECT-DISALLOWED,
      *         CDD-ERR-GEN-INVALID-UDT-KIND, CDD-ERR-GEN-BUF-OVERFLOW.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-IR CDD-BUF CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate options and emit COBOL UDT.
       MAIN-ENTRY.
           IF NOT CDD-OPT-TAG-VALID
               OR NOT CDD-IR-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDGENU" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDGENU"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-GENU-OPTIONS
           IF CDD-OK
               PERFORM EMIT-FILE-DOC-HEADER
               IF CDD-IR-UDT-CLASS
                   PERFORM EMIT-CLASS-WRAPPER
               ELSE
                   PERFORM EMIT-ROOT-RECORD-OR-TYPEDEF
               END-IF
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-GENU-OPTIONS Check dialect, format, & UDT kind.
       INIT-GENU-OPTIONS.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-BUF-LEN
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 1 TO CDD-DLR-OP
           MOVE CDD-OPT-DIALECT TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL
           IF CDD-OK
               IF CDD-OPT-FMT-FREE
                   IF CDD-DLR-ALLOW-FREE = 0
                       MOVE 2010 TO CDD-ERR-CODE
                       MOVE "CDDGENU" TO CDD-ERR-SUBPROG
                       MOVE "Free format requires COBOL-2002+"
                           TO CDD-ERR-MESSAGE
                   ELSE
                       MOVE 1 TO WS-IS-FREE
                   END-IF
               ELSE
                   MOVE 0 TO WS-IS-FREE
               END-IF
           END-IF
           IF CDD-OK
               IF (CDD-IR-UDT-TYPEDEF OR CDD-IR-UDT-CLASS)
                   AND CDD-DLR-HAS-TYPEDEF-OO = 0
                   MOVE 4002 TO CDD-ERR-CODE
                   MOVE "CDDGENU" TO CDD-ERR-SUBPROG
                   MOVE "TYPEDEF/CLASS disallowed before COBOL-2002"
                       TO CDD-ERR-MESSAGE
               END-IF
           END-IF.
      * @paragraph FLUSH-LINE Append trimmed WS-LINE-BUF + NL to BUF.
       FLUSH-LINE.
           IF CDD-OK
               IF WS-IS-FREE = 0
                   MOVE 72 TO WS-SLEN
               ELSE
                   MOVE 256 TO WS-SLEN
               END-IF
               PERFORM UNTIL WS-SLEN = 0
                   OR WS-LINE-BUF(WS-SLEN:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-SLEN
                   END-SUBTRACT
               END-PERFORM
               IF CDD-BUF-LEN + WS-SLEN + 2 > CDD-BUF-CAP
                   MOVE 4001 TO CDD-ERR-CODE
                   MOVE "CDDGENU" TO CDD-ERR-SUBPROG
                   MOVE "UDT generator output buffer overflow"
                       TO CDD-ERR-MESSAGE
               ELSE
                   MOVE WS-LINE-BUF(1:WS-SLEN)
                       TO CDD-BUF-DATA(CDD-BUF-LEN + 1:WS-SLEN)
                   ADD WS-SLEN TO CDD-BUF-LEN
                   END-ADD
                   IF CDD-BUF-NL-CRLF
                       ADD 1 TO CDD-BUF-LEN
                       END-ADD
                       MOVE WS-CR TO CDD-BUF-DATA(CDD-BUF-LEN:1)
                   END-IF
                   ADD 1 TO CDD-BUF-LEN
                   END-ADD
                   MOVE WS-LF TO CDD-BUF-DATA(CDD-BUF-LEN:1)
               END-IF
               MOVE SPACES TO WS-LINE-BUF
           END-IF.
      * @paragraph EMIT-COMMENT-LINE Format WS-CMT-BODY as * or *>.
       EMIT-COMMENT-LINE.
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE "*> " TO WS-LINE-BUF(1:3)
               MOVE WS-CMT-BODY(1:120) TO WS-LINE-BUF(4:120)
           ELSE
               MOVE "*" TO WS-LINE-BUF(7:1)
               MOVE WS-CMT-BODY(1:64) TO WS-LINE-BUF(9:64)
           END-IF
           PERFORM FLUSH-LINE
           MOVE SPACES TO WS-CMT-BODY.
      * @paragraph FMT-U32-HELPER Convert WS-NUM-U32 to WS-NUM-TXT.
       FMT-U32-HELPER.
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 7 TO CDD-SRQ-OP
           MOVE WS-NUM-U32 TO CDD-SRQ-NUM-VAL
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           COMPUTE WS-NUM-LEN = CDD-SRQ-OUT-LEN
           END-COMPUTE
           MOVE SPACES TO WS-NUM-TXT
           MOVE CDD-SRQ-OUT-TEXT(1:WS-NUM-LEN)
               TO WS-NUM-TXT(1:WS-NUM-LEN).
      * @paragraph EMIT-FILE-DOC-HEADER Emit @file/@brief (+ >>SOURCE).
       EMIT-FILE-DOC-HEADER.
           IF WS-IS-FREE = 1
               MOVE SPACES TO WS-LINE-BUF
               MOVE ">>SOURCE FORMAT IS FREE" TO WS-LINE-BUF(1:23)
               PERFORM FLUSH-LINE
           END-IF
           MOVE SPACES TO WS-CMT-BODY
           MOVE 1 TO WS-LPOS
           STRING "@file " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  ".cpy" DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-COMMENT-LINE
           IF CDD-IR-DESC-LEN > 0
               MOVE 1 TO WS-LPOS
               STRING "@brief " DELIMITED BY SIZE
                      CDD-IR-DESC(1:CDD-IR-DESC-LEN) DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
           ELSE
               MOVE "@brief Generated COBOL UDT definition."
                   TO WS-CMT-BODY
           END-IF
           PERFORM EMIT-COMMENT-LINE.
      * @paragraph EMIT-CLASS-WRAPPER Emit COBOL-2002+ CLASS-ID UDT.
       EMIT-CLASS-WRAPPER.
           MOVE 1 TO WS-LPOS
           STRING "@class " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  " Generated OO class." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-COMMENT-LINE
           MOVE "@details Encapsulates JSON-mapped instance storage."
               TO WS-CMT-BODY
           PERFORM EMIT-COMMENT-LINE
           PERFORM EMIT-AREA-A-STMT-IDENT
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE 1 TO WS-LPOS
           ELSE
               MOVE 8 TO WS-LPOS
           END-IF
           STRING "CLASS-ID. " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  "." DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           PERFORM FLUSH-LINE
           PERFORM EMIT-CLASS-OBJECT-HEADER
           PERFORM EMIT-ROOT-RECORD-OR-TYPEDEF
           PERFORM EMIT-CLASS-OBJECT-FOOTER.
      * @paragraph EMIT-AREA-A-STMT-IDENT Emit IDENTIFICATION DIVISION.
       EMIT-AREA-A-STMT-IDENT.
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE "IDENTIFICATION DIVISION." TO WS-LINE-BUF(1:24)
           ELSE
               MOVE "IDENTIFICATION DIVISION." TO WS-LINE-BUF(8:24)
           END-IF
           PERFORM FLUSH-LINE.
      * @paragraph EMIT-CLASS-OBJECT-HEADER Emit OBJECT..WS SECTION.
       EMIT-CLASS-OBJECT-HEADER.
           IF WS-IS-FREE = 1
               MOVE "OBJECT." TO WS-LINE-BUF(1:7)
               PERFORM FLUSH-LINE
               MOVE "DATA DIVISION." TO WS-LINE-BUF(1:14)
               PERFORM FLUSH-LINE
               MOVE "WORKING-STORAGE SECTION." TO WS-LINE-BUF(1:24)
               PERFORM FLUSH-LINE
           ELSE
               MOVE "OBJECT." TO WS-LINE-BUF(8:7)
               PERFORM FLUSH-LINE
               MOVE "DATA DIVISION." TO WS-LINE-BUF(8:14)
               PERFORM FLUSH-LINE
               MOVE "WORKING-STORAGE SECTION." TO WS-LINE-BUF(8:24)
               PERFORM FLUSH-LINE
           END-IF.
      * @paragraph EMIT-CLASS-OBJECT-FOOTER Emit END OBJECT & END CLASS.
       EMIT-CLASS-OBJECT-FOOTER.
           IF WS-IS-FREE = 1
               MOVE "END OBJECT." TO WS-LINE-BUF(1:11)
               PERFORM FLUSH-LINE
               MOVE 1 TO WS-LPOS
           ELSE
               MOVE "END OBJECT." TO WS-LINE-BUF(8:11)
               PERFORM FLUSH-LINE
               MOVE 8 TO WS-LPOS
           END-IF
           STRING "END CLASS " DELIMITED BY SIZE
                  CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN) DELIMITED BY SIZE
                  "." DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           PERFORM FLUSH-LINE.
      * @paragraph EMIT-ROOT-RECORD-OR-TYPEDEF Emit 01 header & fields.
       EMIT-ROOT-RECORD-OR-TYPEDEF.
           MOVE 1 TO WS-LPOS
           IF CDD-IR-UDT-TYPEDEF
               STRING "@typedef " DELIMITED BY SIZE
                      CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN)
                          DELIMITED BY SIZE
                      " Strongly-typed UDT." DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
           ELSE
               STRING "@record " DELIMITED BY SIZE
                      CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN)
                          DELIMITED BY SIZE
                      " Root JSON data record." DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
           END-IF
           PERFORM EMIT-COMMENT-LINE
           IF CDD-IR-TITLE-LEN > 0
               MOVE 1 TO WS-LPOS
               STRING "@json-name " DELIMITED BY SIZE
                      CDD-IR-JSON-TITLE(1:CDD-IR-TITLE-LEN)
                          DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-COMMENT-LINE
           END-IF
           MOVE SPACES TO WS-LINE-BUF
           IF WS-IS-FREE = 1
               MOVE 1 TO WS-LPOS
           ELSE
               MOVE 8 TO WS-LPOS
           END-IF
           IF CDD-IR-UDT-TYPEDEF
               STRING "01  " DELIMITED BY SIZE
                      CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN)
                          DELIMITED BY SIZE
                      " IS TYPEDEF STRONG." DELIMITED BY SIZE
                      INTO WS-LINE-BUF WITH POINTER WS-LPOS
               END-STRING
           ELSE
               IF CDD-IR-UDT-CLASS
                   STRING "01  " DELIMITED BY SIZE
                          CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN)
                              DELIMITED BY SIZE
                          "-DATA." DELIMITED BY SIZE
                          INTO WS-LINE-BUF WITH POINTER WS-LPOS
                   END-STRING
               ELSE
                   STRING "01  " DELIMITED BY SIZE
                          CDD-IR-COB-NAME(1:CDD-IR-COB-NLEN)
                              DELIMITED BY SIZE
                          "." DELIMITED BY SIZE
                          INTO WS-LINE-BUF WITH POINTER WS-LPOS
                   END-STRING
               END-IF
           END-IF
           PERFORM FLUSH-LINE
           IF CDD-IR-ROOT-FIRST > 0
               MOVE 1 TO WS-DEPTH
               MOVE CDD-IR-ROOT-FIRST TO WS-STK-IDX(1)
               PERFORM WALK-IR-FIELD-TREE
           END-IF.
      * @paragraph WALK-IR-FIELD-TREE Emit hierarchical fields & 88s.
       WALK-IR-FIELD-TREE.
           PERFORM UNTIL WS-DEPTH = 0 OR NOT CDD-OK
               MOVE WS-STK-IDX(WS-DEPTH) TO WS-FIDX
               IF WS-FIDX = 0
                   SUBTRACT 1 FROM WS-DEPTH
                   END-SUBTRACT
               ELSE
                   MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                       TO WS-STK-IDX(WS-DEPTH)
                   PERFORM EMIT-SINGLE-IR-FIELD
               END-IF
           END-PERFORM.
      * @paragraph COMPUTE-LEVEL-AND-INDENT Set WS-LVL-DISP & indent.
       COMPUTE-LEVEL-AND-INDENT.
           IF CDD-IRF-COB-LEVEL(WS-FIDX) = 66
               MOVE 66 TO WS-LVL-NUM
               MOVE 66 TO WS-LVL-DISP
           ELSE
               COMPUTE WS-LVL-NUM = WS-DEPTH * 5
               END-COMPUTE
               IF WS-LVL-NUM > 49
                   MOVE 49 TO WS-LVL-NUM
               END-IF
               MOVE WS-LVL-NUM TO WS-LVL-DISP
           END-IF
           IF WS-IS-FREE = 1
               COMPUTE WS-INDENT-COL = (WS-DEPTH * 2) + 1
               END-COMPUTE
           ELSE
               COMPUTE WS-INDENT-COL = 8 + (WS-DEPTH * 4)
               END-COMPUTE
               IF WS-INDENT-COL > 28
                   MOVE 28 TO WS-INDENT-COL
               END-IF
           END-IF.
      * @paragraph EMIT-SINGLE-IR-FIELD Emit ODO count, field, null, 88.
       EMIT-SINGLE-IR-FIELD.
           PERFORM COMPUTE-LEVEL-AND-INDENT
           IF CDD-IRF-ARR-ODO(WS-FIDX) AND CDD-DLR-HAS-ODO = 1
               PERFORM EMIT-ODO-COUNT-COMPANION
           END-IF
           PERFORM EMIT-FIELD-DOC-LINES
           IF CDD-IRF-COB-LEVEL(WS-FIDX) = 66
               AND CDD-DLR-HAS-RENAMES-66 = 1
               PERFORM EMIT-LEVEL-66-RENAMES
           ELSE
               IF CDD-IRF-IS-GROUP(WS-FIDX)
                   PERFORM EMIT-GROUP-FIELD-LINE
                   IF CDD-IRF-FIRST-CHILD(WS-FIDX) > 0
                       ADD 1 TO WS-DEPTH
                       END-ADD
                       MOVE CDD-IRF-FIRST-CHILD(WS-FIDX)
                           TO WS-STK-IDX(WS-DEPTH)
                   END-IF
               ELSE
                   PERFORM EMIT-ELEMENTARY-FIELD
                   PERFORM EMIT-FIELD-88-CONDITIONS
                   IF CDD-IRF-CAN-NULL(WS-FIDX)
                       AND CDD-OPT-DIALECT >= 7
                       PERFORM EMIT-NULL-COMPANION
                   END-IF
               END-IF
           END-IF.
      * @paragraph EMIT-ODO-COUNT-COMPANION Emit <ARR>-COUNT before ODO.
       EMIT-ODO-COUNT-COMPANION.
           MOVE 1 TO WS-LPOS
           STRING "@field " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "-COUNT Active array item count." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-COMMENT-LINE
           MOVE SPACES TO WS-LINE-BUF
           MOVE WS-INDENT-COL TO WS-LPOS
           STRING WS-LVL-DISP DELIMITED BY SIZE
                  "  " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "-COUNT PIC 9(4) COMP." DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           PERFORM FLUSH-LINE.
      * @paragraph EMIT-NULL-COMPANION Emit <FLD>-IS-NULL indicator.
       EMIT-NULL-COMPANION.
           MOVE 1 TO WS-LPOS
           STRING "@field " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "-IS-NULL Null indicator ('Y'/'N')." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-COMMENT-LINE
           MOVE SPACES TO WS-LINE-BUF
           MOVE WS-INDENT-COL TO WS-LPOS
           STRING WS-LVL-DISP DELIMITED BY SIZE
                  "  " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "-IS-NULL PIC X(1) VALUE 'N'." DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           PERFORM FLUSH-LINE.
      * @paragraph EMIT-FIELD-DOC-LINES Emit @field & @json-name docs.
       EMIT-FIELD-DOC-LINES.
           MOVE 1 TO WS-LPOS
           IF CDD-IRF-DESC-LEN(WS-FIDX) > 0
               STRING "@field " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      CDD-IRF-DESC(WS-FIDX)(1:CDD-IRF-DESC-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
           ELSE
               STRING "@field " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " Mapped JSON property." DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
           END-IF
           PERFORM EMIT-COMMENT-LINE
           IF CDD-IRF-JKEY-LEN(WS-FIDX) > 0
               MOVE 1 TO WS-LPOS
               STRING "@json-name " DELIMITED BY SIZE
                      CDD-IRF-JSON-KEY(WS-FIDX)(
                          1:CDD-IRF-JKEY-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-COMMENT-LINE
           END-IF.
      * @paragraph EMIT-LEVEL-66-RENAMES Emit 66 <NAME> RENAMES <TGT>.
       EMIT-LEVEL-66-RENAMES.
           MOVE SPACES TO WS-LINE-BUF
           MOVE WS-INDENT-COL TO WS-LPOS
           STRING "66  " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  " RENAMES " DELIMITED BY SIZE
                  CDD-IRF-REDEFINES(WS-FIDX)(
                      1:CDD-IRF-REDEF-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  "." DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           PERFORM FLUSH-LINE.
      * @paragraph EMIT-GROUP-FIELD-LINE Emit group level + OCCURS.
       EMIT-GROUP-FIELD-LINE.
           MOVE SPACES TO WS-CLAUSE-BUF
           MOVE 1 TO WS-CLEN
           IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
               PERFORM APPEND-OCCURS-CLAUSE
           END-IF
           SUBTRACT 1 FROM WS-CLEN
           END-SUBTRACT
           PERFORM EMIT-DECL-WITH-CLAUSE.
      * @paragraph EMIT-ELEMENTARY-FIELD Build PIC/USAGE/OCCURS/VALUE.
       EMIT-ELEMENTARY-FIELD.
           MOVE SPACES TO WS-CLAUSE-BUF
           MOVE 1 TO WS-CLEN
           IF CDD-IRF-REDEF-LEN(WS-FIDX) > 0
               AND CDD-DLR-HAS-REDEFINES = 1
               STRING "REDEFINES " DELIMITED BY SIZE
                      CDD-IRF-REDEFINES(WS-FIDX)(
                          1:CDD-IRF-REDEF-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           END-IF
           IF CDD-OPT-DIAL-60 AND CDD-OPT-C60-VERBOSE = 1
               PERFORM APPEND-C60-VERBOSE-CLAUSE
           ELSE
               PERFORM APPEND-DIALECT-TYPE-CLAUSE
           END-IF
           IF CDD-IRF-SYNC-JUST(WS-FIDX) = 1
               OR CDD-IRF-SYNC-JUST(WS-FIDX) = 3
               STRING " SYNCHRONIZED" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           END-IF
           IF CDD-IRF-SYNC-JUST(WS-FIDX) >= 2
               STRING " JUSTIFIED" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           END-IF
           IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
               STRING " " DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
               PERFORM APPEND-OCCURS-CLAUSE
           END-IF
           SUBTRACT 1 FROM WS-CLEN
           END-SUBTRACT
           PERFORM EMIT-DECL-WITH-CLAUSE.
      * @paragraph EMIT-DECL-WITH-CLAUSE Align or wrap col 72 cleanly.
       EMIT-DECL-WITH-CLAUSE.
           MOVE SPACES TO WS-LINE-BUF
           MOVE WS-INDENT-COL TO WS-LPOS
           STRING WS-LVL-DISP DELIMITED BY SIZE
                  "  " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           IF WS-CLEN = 0
               MOVE "." TO WS-LINE-BUF(WS-LPOS:1)
               PERFORM FLUSH-LINE
           ELSE
               IF WS-IS-FREE = 0 AND WS-LPOS < 40
                   MOVE 40 TO WS-LPOS
               ELSE
                   ADD 1 TO WS-LPOS
                   END-ADD
               END-IF
               IF WS-IS-FREE = 0 AND (WS-LPOS + WS-CLEN) > 72
                   PERFORM FLUSH-LINE
                   MOVE SPACES TO WS-LINE-BUF
                   COMPUTE WS-LPOS = WS-INDENT-COL + 4
                   END-COMPUTE
                   IF WS-LPOS + WS-CLEN > 72
                       MOVE 12 TO WS-LPOS
                   END-IF
               END-IF
               MOVE WS-CLAUSE-BUF(1:WS-CLEN)
                   TO WS-LINE-BUF(WS-LPOS:WS-CLEN)
               ADD WS-CLEN TO WS-LPOS
               END-ADD
               MOVE "." TO WS-LINE-BUF(WS-LPOS:1)
               PERFORM FLUSH-LINE
           END-IF.
      * @paragraph APPEND-C60-VERBOSE-CLAUSE Emit SIZE IS / CLASS IS.
       APPEND-C60-VERBOSE-CLAUSE.
           IF CDD-IRF-IS-INTEGER(WS-FIDX)
               OR CDD-IRF-IS-DECIMAL(WS-FIDX)
               MOVE CDD-IRF-DIGITS(WS-FIDX) TO WS-NUM-U32
               PERFORM FMT-U32-HELPER
               STRING "SIZE IS " DELIMITED BY SIZE
                      WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                      " DIGITS CLASS IS NUMERIC" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
               IF CDD-IRF-SCALE(WS-FIDX) > 0
                   MOVE CDD-IRF-SCALE(WS-FIDX) TO WS-NUM-U32
                   PERFORM FMT-U32-HELPER
                   STRING " POINT LOCATION IS LEFT " DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
               IF CDD-IRF-IS-SIGNED(WS-FIDX)
                   STRING " SIGNED" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
           ELSE
               IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
                   MOVE 32 TO WS-NUM-U32
               ELSE
                   MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
               END-IF
               PERFORM FMT-U32-HELPER
               STRING "SIZE IS " DELIMITED BY SIZE
                      WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                      " CHARACTERS CLASS IS ALPHANUMERIC"
                          DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           END-IF.
      * @paragraph APPEND-PIC-KW Emit 'PICTURE IS ', 'PICTURE ', 'PIC '.
       APPEND-PIC-KW.
           IF CDD-OPT-DIAL-60
               STRING "PICTURE IS " DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           ELSE
               IF CDD-OPT-DIAL-61 OR CDD-OPT-DIAL-61EXT
                   STRING "PICTURE " DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               ELSE
                   STRING "PIC " DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
           END-IF.
      * @paragraph APPEND-DIALECT-TYPE-CLAUSE Emit era PIC and USAGE.
       APPEND-DIALECT-TYPE-CLAUSE.
           EVALUATE TRUE
               WHEN CDD-IRF-IS-BOOL(WS-FIDX)
                   IF CDD-DLR-HAS-TYPEDEF-OO = 1
                       STRING "PIC 1 USAGE BIT" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   ELSE
                       PERFORM APPEND-PIC-KW
                       STRING "X(1)" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
               WHEN CDD-IRF-IS-FLOAT(WS-FIDX)
                   IF CDD-DLR-HAS-TYPEDEF-OO = 1
                       STRING "USAGE FLOAT-LONG" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   ELSE
                       STRING "USAGE COMP-2" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
               WHEN CDD-IRF-IS-DECFLOAT(WS-FIDX)
                   IF CDD-OPT-DIALECT >= 9
                       STRING "USAGE FLOAT-DECIMAL-16" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   ELSE
                       PERFORM APPEND-DECIMAL-PIC
                   END-IF
               WHEN CDD-IRF-IS-INTEGER(WS-FIDX)
                   PERFORM APPEND-INTEGER-PIC-OR-BIN
               WHEN CDD-IRF-IS-DECIMAL(WS-FIDX)
                   PERFORM APPEND-DECIMAL-PIC
               WHEN OTHER
                   PERFORM APPEND-STRING-PIC
           END-EVALUATE.
      * @paragraph APPEND-INTEGER-PIC-OR-BIN Emit int PIC or BINARY-*.
       APPEND-INTEGER-PIC-OR-BIN.
           IF CDD-DLR-HAS-TYPEDEF-OO = 1
               AND CDD-IRF-USG-BINLONG(WS-FIDX)
               STRING "USAGE BINARY-LONG" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           ELSE
               IF CDD-DLR-HAS-TYPEDEF-OO = 1
                   AND CDD-IRF-USG-BINDBL(WS-FIDX)
                   STRING "USAGE BINARY-DOUBLE" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               ELSE
                   PERFORM APPEND-PIC-KW
                   IF CDD-IRF-IS-SIGNED(WS-FIDX)
                       STRING "S" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
                   IF CDD-IRF-DIGITS(WS-FIDX) = 0
                       MOVE 9 TO WS-NUM-U32
                   ELSE
                       MOVE CDD-IRF-DIGITS(WS-FIDX) TO WS-NUM-U32
                   END-IF
                   PERFORM FMT-U32-HELPER
                   STRING "9(" DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
                   IF CDD-OPT-DIAL-68
                       STRING " USAGE COMPUTATIONAL" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
               END-IF
           END-IF.
      * @paragraph APPEND-DECIMAL-PIC Emit PIC [S]9(i)V9(s).
       APPEND-DECIMAL-PIC.
           PERFORM APPEND-PIC-KW
           IF CDD-IRF-IS-SIGNED(WS-FIDX)
               STRING "S" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           END-IF
           IF CDD-IRF-DIGITS(WS-FIDX) > CDD-IRF-SCALE(WS-FIDX)
               COMPUTE WS-INT-DIGITS =
                   CDD-IRF-DIGITS(WS-FIDX) - CDD-IRF-SCALE(WS-FIDX)
               END-COMPUTE
           ELSE
               MOVE 1 TO WS-INT-DIGITS
           END-IF
           MOVE WS-INT-DIGITS TO WS-NUM-U32
           PERFORM FMT-U32-HELPER
           STRING "9(" DELIMITED BY SIZE
                  WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                  ")" DELIMITED BY SIZE
                  INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
           END-STRING
           IF CDD-IRF-SCALE(WS-FIDX) > 0
               MOVE CDD-IRF-SCALE(WS-FIDX) TO WS-NUM-U32
               PERFORM FMT-U32-HELPER
               STRING "V9(" DELIMITED BY SIZE
                      WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                      ")" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           END-IF.
      * @paragraph APPEND-STRING-PIC Emit PIC X/N/U or DYNAMIC.
       APPEND-STRING-PIC.
           PERFORM APPEND-PIC-KW
           IF CDD-DLR-HAS-UTF8 = 1
               AND (CDD-IRF-IS-UTF8(WS-FIDX)
               OR CDD-IRF-MAX-LEN(WS-FIDX) = 0)
               IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
                   STRING "U DYNAMIC" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               ELSE
                   MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
                   PERFORM FMT-U32-HELPER
                   STRING "U(" DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
           ELSE
               IF CDD-DLR-HAS-TYPEDEF-OO = 1
                   AND CDD-IRF-IS-NATIONAL(WS-FIDX)
                   IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
                       MOVE 32 TO WS-NUM-U32
                   ELSE
                       MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
                   END-IF
                   PERFORM FMT-U32-HELPER
                   STRING "N(" DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               ELSE
                   IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
                       MOVE 32 TO WS-NUM-U32
                   ELSE
                       MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
                   END-IF
                   PERFORM FMT-U32-HELPER
                   STRING "X(" DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
           END-IF.
      * @paragraph APPEND-OCCURS-CLAUSE Emit era-idiomatic OCCURS.
       APPEND-OCCURS-CLAUSE.
           IF CDD-IRF-ARR-DYNAMIC(WS-FIDX)
               AND CDD-DLR-HAS-OCCURS-DYN = 1
               STRING "OCCURS DYNAMIC CAPACITY IN " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-CAP" DELIMITED BY SIZE
                      INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
               END-STRING
           ELSE
               IF CDD-IRF-ARR-ODO(WS-FIDX) AND CDD-DLR-HAS-ODO = 1
                   MOVE CDD-IRF-ARR-MIN(WS-FIDX) TO WS-NUM-U32
                   PERFORM FMT-U32-HELPER
                   STRING "OCCURS " DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          " TO " DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
                   IF CDD-IRF-ARR-MAX(WS-FIDX) = 0
                       MOVE 10 TO WS-NUM-U32
                   ELSE
                       MOVE CDD-IRF-ARR-MAX(WS-FIDX) TO WS-NUM-U32
                   END-IF
                   PERFORM FMT-U32-HELPER
                   STRING WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          " TIMES DEPENDING ON " DELIMITED BY SIZE
                          CDD-IRF-COB-NAME(WS-FIDX)(
                              1:CDD-IRF-CNAM-LEN(WS-FIDX))
                              DELIMITED BY SIZE
                          "-COUNT" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               ELSE
                   IF CDD-IRF-ARR-MAX(WS-FIDX) = 0
                       MOVE 10 TO WS-NUM-U32
                   ELSE
                       MOVE CDD-IRF-ARR-MAX(WS-FIDX) TO WS-NUM-U32
                   END-IF
                   PERFORM FMT-U32-HELPER
                   STRING "OCCURS " DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          " TIMES" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
                   IF CDD-OPT-DIAL-65 OR CDD-OPT-DIAL-68
                       STRING " INDEXED BY " DELIMITED BY SIZE
                              CDD-IRF-COB-NAME(WS-FIDX)(
                                  1:CDD-IRF-CNAM-LEN(WS-FIDX))
                                  DELIMITED BY SIZE
                              "-IDX" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
               END-IF
           END-IF.
      * @paragraph EMIT-FIELD-88-CONDITIONS Emit 88 bool or enum items.
       EMIT-FIELD-88-CONDITIONS.
           IF CDD-IRF-IS-BOOL(WS-FIDX) AND CDD-DLR-HAS-TYPEDEF-OO = 0
               MOVE 1 TO WS-LPOS
               STRING "@field " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-TRUE Boolean true condition." DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-COMMENT-LINE
               MOVE SPACES TO WS-LINE-BUF
               COMPUTE WS-LPOS = WS-INDENT-COL + 4
               END-COMPUTE
               STRING "88  " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-TRUE VALUE 'T'." DELIMITED BY SIZE
                      INTO WS-LINE-BUF WITH POINTER WS-LPOS
               END-STRING
               PERFORM FLUSH-LINE
               MOVE 1 TO WS-LPOS
               STRING "@field " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-FALSE Boolean false condition."
                          DELIMITED BY SIZE
                      INTO WS-CMT-BODY WITH POINTER WS-LPOS
               END-STRING
               PERFORM EMIT-COMMENT-LINE
               MOVE SPACES TO WS-LINE-BUF
               COMPUTE WS-LPOS = WS-INDENT-COL + 4
               END-COMPUTE
               STRING "88  " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-FALSE VALUE 'F'." DELIMITED BY SIZE
                      INTO WS-LINE-BUF WITH POINTER WS-LPOS
               END-STRING
               PERFORM FLUSH-LINE
           END-IF
           IF CDD-IRF-ENUM-COUNT(WS-FIDX) > 0
               MOVE CDD-IRF-ENUM-FIRST(WS-FIDX) TO WS-EIDX
               MOVE 0 TO WS-ECNT
               PERFORM UNTIL WS-ECNT >= CDD-IRF-ENUM-COUNT(WS-FIDX)
                   PERFORM EMIT-SINGLE-88-ENUM
                   ADD 1 TO WS-EIDX
                   END-ADD
                   ADD 1 TO WS-ECNT
                   END-ADD
               END-PERFORM
           END-IF.
      * @paragraph EMIT-SINGLE-88-ENUM Emit one 88 enum condition item.
       EMIT-SINGLE-88-ENUM.
           MOVE 1 TO WS-LPOS
           STRING "@field " DELIMITED BY SIZE
                  CDD-IRE-COB-NAME(WS-EIDX)(
                      1:CDD-IRE-CNAM-LEN(WS-EIDX)) DELIMITED BY SIZE
                  " Enum condition value." DELIMITED BY SIZE
                  INTO WS-CMT-BODY WITH POINTER WS-LPOS
           END-STRING
           PERFORM EMIT-COMMENT-LINE
           MOVE SPACES TO WS-LINE-BUF
           COMPUTE WS-LPOS = WS-INDENT-COL + 4
           END-COMPUTE
           IF CDD-IRE-IS-STR(WS-EIDX) = 1
               STRING "88  " DELIMITED BY SIZE
                      CDD-IRE-COB-NAME(WS-EIDX)(
                          1:CDD-IRE-CNAM-LEN(WS-EIDX))
                          DELIMITED BY SIZE
                      " VALUE '" DELIMITED BY SIZE
                      CDD-IRE-VAL(WS-EIDX)(1:CDD-IRE-VAL-LEN(WS-EIDX))
                          DELIMITED BY SIZE
                      "'." DELIMITED BY SIZE
                      INTO WS-LINE-BUF WITH POINTER WS-LPOS
               END-STRING
           ELSE
               STRING "88  " DELIMITED BY SIZE
                      CDD-IRE-COB-NAME(WS-EIDX)(
                          1:CDD-IRE-CNAM-LEN(WS-EIDX))
                          DELIMITED BY SIZE
                      " VALUE " DELIMITED BY SIZE
                      CDD-IRE-VAL(WS-EIDX)(1:CDD-IRE-VAL-LEN(WS-EIDX))
                          DELIMITED BY SIZE
                      "." DELIMITED BY SIZE
                      INTO WS-LINE-BUF WITH POINTER WS-LPOS
               END-STRING
           END-IF
           PERFORM FLUSH-LINE.
