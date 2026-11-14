      * @file CDDSYNC.cbl
      * @brief Feature 3 (sync): Trivia-preserving surgical CST patcher.
      * @module CDDSYNC
      * @details Updates an existing COBOL UDT (01 record, TYPEDEF, or
      *          CLASS-ID) and its optional to_json/from_json routines
      *          to match a JSON Schema or payload while preserving
      *          100% of whitespace, comments, sequence numbers, col-7
      *          indicators, PIC alignment, cols 73-80 tags, and code
      *          above, inside, between, and below routines.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDSYNC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-dial.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-diff.cpy".
      * @record WS-OLD-IR Parsed IR of existing COBOL UDT.
       01  WS-OLD-IR.
      * @field WS-OIR-TAG Magic tag ('CDDIR001').
           05  WS-OIR-TAG                PIC X(8) VALUE "CDDIR001".
      * @field WS-OIR-REST Storage matching CDD-IR layout.
           05  WS-OIR-REST               PIC X(260000) VALUE SPACES.
       COPY "cdd-ir.cpy".
      * @record WS-SUB-OPTS Copy of CDD-SDK-OPTS for CDDGENS calls.
       01  WS-SUB-OPTS.
      * @field WS-SOP-TAG Magic tag ('CDDOPT01').
           05  WS-SOP-TAG                PIC X(8) VALUE "CDDOPT01".
      * @field WS-SOP-DIALECT Dialect ID.
           05  WS-SOP-DIALECT            PIC 9(2) COMP VALUE 7.
      * @field WS-SOP-FORMAT Format code.
           05  WS-SOP-FORMAT             PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-UDT-KIND UDT kind.
           05  WS-SOP-UDT-KIND           PIC 9(2) COMP VALUE 1.
      * @field WS-SOP-SERDE-KIND Serde kind.
           05  WS-SOP-SERDE-KIND         PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-JSON-MODE JSON mode.
           05  WS-SOP-JSON-MODE          PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-CASE Naming case.
           05  WS-SOP-CASE               PIC 9(2) COMP VALUE 1.
      * @field WS-SOP-STYLE JSON style.
           05  WS-SOP-STYLE              PIC 9(2) COMP VALUE 1.
      * @field WS-SOP-PURE Pure COBOL flag.
           05  WS-SOP-PURE               PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-VENDOR Vendor JSON flag.
           05  WS-SOP-VENDOR             PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-XCOB x-cobol flag.
           05  WS-SOP-XCOB               PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-SYNC-SRD Body-only mode selector (8 or 9).
           05  WS-SOP-SYNC-SRD           PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-C60V Verbose C60 flag.
           05  WS-SOP-C60V               PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-NLEN Target name length.
           05  WS-SOP-NLEN               PIC 9(2) COMP VALUE 0.
      * @field WS-SOP-NAME Target name.
           05  WS-SOP-NAME               PIC X(64) VALUE SPACES.
      * @record WS-GEN-BUF Scratch buffer for synthesized routine body.
       01  WS-GEN-BUF.
      * @field WS-GBF-TAG Magic tag ('CDDBUF01').
           05  WS-GBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-GBF-LEN Active byte length.
           05  WS-GBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-GBF-CAP Buffer capacity.
           05  WS-GBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-GBF-NL Newline mode.
           05  WS-GBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-GBF-DATA Scratch output bytes.
           05  WS-GBF-DATA               PIC X(1048576) VALUE SPACES.
      * @record WS-BYTE-CONV Big-endian LF/CR helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 Unsigned 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STYLE-TPL Inferred local UDT sibling formatting.
       01  WS-STYLE-TPL.
      * @field WS-STY-FREE 1 if UDT is free-format, 0 if fixed.
           05  WS-STY-FREE               PIC 9(2) COMP VALUE 0.
      * @field WS-STY-HAS-SEQ 1 if cols 1-6 carry numeric seq numbers.
           05  WS-STY-HAS-SEQ            PIC 9(2) COMP VALUE 0.
      * @field WS-STY-SEQ-CUR Current interpolated 6-digit seq num.
           05  WS-STY-SEQ-CUR            PIC 9(6) COMP VALUE 100.
      * @field WS-STY-SEQ-DISP 6-digit display sequence number.
           05  WS-STY-SEQ-DISP           PIC 9(6) VALUE 000100.
      * @field WS-STY-BASE-LVL First child level (e.g. 05 or 02).
           05  WS-STY-BASE-LVL           PIC 9(2) COMP VALUE 5.
      * @field WS-STY-BASE-COL First child column (e.g. 12).
           05  WS-STY-BASE-COL           PIC 9(4) COMP VALUE 12.
      * @field WS-STY-PIC-COL Starting column of PIC clause (e.g. 40).
           05  WS-STY-PIC-COL            PIC 9(4) COMP VALUE 40.
      * @field WS-STY-PIC-KW-LEN Length of inferred PIC keyword.
           05  WS-STY-PIC-KW-LEN         PIC 9(2) COMP VALUE 4.
      * @field WS-STY-PIC-KW 'PIC ', 'PICTURE ', or 'PICTURE IS '.
           05  WS-STY-PIC-KW             PIC X(12) VALUE "PIC ".
      * @field WS-STY-HAS-IDENT 1 if cols 73-80 carry deck ident tag.
           05  WS-STY-HAS-IDENT          PIC 9(2) COMP VALUE 0.
      * @field WS-STY-IDENT-LEN Length of cols 73-80 deck tag (1..8).
           05  WS-STY-IDENT-LEN          PIC 9(2) COMP VALUE 0.
      * @field WS-STY-IDENT-TAG Deck tag copied onto synthesized lines.
           05  WS-STY-IDENT-TAG          PIC X(8) VALUE SPACES.
      * @record WS-INS-STACK Non-recursive stack for inserted groups.
       01  WS-INS-STACK.
      * @field WS-INS-DEPTH Nesting depth inside inserted group.
           05  WS-INS-DEPTH              PIC 9(2) COMP VALUE 0.
      * @field WS-INS-IDX Next sibling IR index at depth.
           05  WS-INS-IDX                PIC 9(4) COMP OCCURS 16 TIMES.
      * @record WS-SYNC-STATE Patching cursors and scratch fields.
       01  WS-SYNC-STATE.
      * @field WS-LF Cached LF byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-CR Cached CR byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-UDT-CST Matched root UDT CST node index.
           05  WS-UDT-CST                PIC 9(4) COMP VALUE 0.
      * @field WS-FIRST-FLD-CST First field child of WS-UDT-CST.
           05  WS-FIRST-FLD-CST          PIC 9(4) COMP VALUE 0.
      * @field WS-TI Token scan index.
           05  WS-TI                     PIC 9(8) COMP VALUE 0.
      * @field WS-DI Diff operation loop index.
           05  WS-DI                     PIC 9(4) COMP VALUE 0.
      * @field WS-HAS-MUTATION 1 if any UPDATE/INSERT/REMOVE occurred.
           05  WS-HAS-MUTATION           PIC 9(2) COMP VALUE 0.
      * @field WS-SRD-MODE Target serde body mode (8=to, 9=from).
           05  WS-SRD-MODE               PIC 9(2) COMP VALUE 0.
      * @field WS-LAST-ANCHOR-PAR Parent CST of previous insert op.
           05  WS-LAST-ANCHOR-PAR        PIC 9(4) COMP VALUE 0.
      * @field WS-LAST-ORIG-ANC Original anchor of previous insert op.
           05  WS-LAST-ORIG-ANC          PIC 9(4) COMP VALUE 0.
      * @field WS-CHAIN-ANC Chained CST node for consecutive inserts.
           05  WS-CHAIN-ANC              PIC 9(4) COMP VALUE 0.
      * @field WS-EFF-ANC Effective insertion anchor CST node.
           05  WS-EFF-ANC                PIC 9(4) COMP VALUE 0.
      * @field WS-PAR-CST Enclosing CST parent node for insert.
           05  WS-PAR-CST                PIC 9(4) COMP VALUE 0.
      * @field WS-NEW-CST Newly allocated synthesized CST node.
           05  WS-NEW-CST                PIC 9(4) COMP VALUE 0.
      * @field WS-FIDX Active NEW-IR field index being formatted.
           05  WS-FIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-OLD-FIDX Active OLD-IR field index on UPDATE.
           05  WS-OLD-FIDX               PIC 9(4) COMP VALUE 0.
      * @field WS-CST-TARGET Target CST node on UPDATE or REMOVE.
           05  WS-CST-TARGET             PIC 9(4) COMP VALUE 0.
      * @field WS-PATCH-START Starting offset in CDD-CST-PATCH-BUF.
           05  WS-PATCH-START            PIC 9(8) COMP VALUE 0.
      * @field WS-CLEN Built clause byte length.
           05  WS-CLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-LPOS Write cursor in WS-LINE-BUF.
           05  WS-LPOS                   PIC 9(4) COMP VALUE 1.
      * @field WS-SLEN Trimmed line length.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @field WS-CUR-DEPTH Effective hierarchy depth (1=top child).
           05  WS-CUR-DEPTH              PIC 9(2) COMP VALUE 1.
      * @field WS-LVL-NUM Computed COBOL level number.
           05  WS-LVL-NUM                PIC 9(2) COMP VALUE 5.
      * @field WS-LVL-DISP Two-digit display level number.
           05  WS-LVL-DISP               PIC 9(2) VALUE 05.
      * @field WS-COL-START Computed column of field level number.
           05  WS-COL-START              PIC 9(4) COMP VALUE 12.
      * @field WS-INT-DIGITS Integer digits before V.
           05  WS-INT-DIGITS             PIC 9(4) COMP VALUE 0.
      * @field WS-NUM-U32 Integer to format via CDDSTR.
           05  WS-NUM-U32                PIC 9(8) COMP VALUE 0.
      * @field WS-NUM-LEN Length of formatted integer.
           05  WS-NUM-LEN                PIC 9(2) COMP VALUE 0.
      * @field WS-NUM-TXT Formatted integer text.
           05  WS-NUM-TXT                PIC X(16) VALUE SPACES.
      * @record WS-CLAUSE-BUF Formatted type/occurs clause text.
       01  WS-CLAUSE-BUF                 PIC X(160) VALUE SPACES.
      * @record WS-LINE-BUF Formatted COBOL declaration line.
       01  WS-LINE-BUF                   PIC X(256) VALUE SPACES.
      * @record WS-TOK-PEEK Uppercased token text for style inference.
       01  WS-TOK-PEEK                   PIC X(32) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
      * @record CDD-JSON-BUF Input JSON Schema or sample JSON buffer.
       01  CDD-JSON-BUF.
      * @field CDD-JBF-TAG Magic tag ('CDDBUF01').
           05  CDD-JBF-TAG               PIC X(8).
      * @field CDD-JBF-TAG-VALID Condition confirming valid JBF tag.
               88  CDD-JBF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-JBF-LEN Active JSON byte length.
           05  CDD-JBF-LEN               PIC 9(8) COMP.
      * @field CDD-JBF-CAP JSON buffer capacity.
           05  CDD-JBF-CAP               PIC 9(8) COMP.
      * @field CDD-JBF-NL-KIND JSON buffer newline kind.
           05  CDD-JBF-NL-KIND           PIC 9(2) COMP.
      * @field CDD-JBF-DATA Raw JSON bytes.
           05  CDD-JBF-DATA              PIC X(1048576).
      * @record CDD-OUT-BUF Output synchronized COBOL source buffer.
       01  CDD-OUT-BUF.
      * @field CDD-OBF-TAG Magic tag ('CDDBUF01').
           05  CDD-OBF-TAG               PIC X(8).
      * @field CDD-OBF-TAG-VALID Condition confirming valid OBF tag.
               88  CDD-OBF-TAG-VALID              VALUE "CDDBUF01".
      * @field CDD-OBF-LEN Active output COBOL byte length.
           05  CDD-OBF-LEN               PIC 9(8) COMP.
      * @field CDD-OBF-CAP Output buffer capacity.
           05  CDD-OBF-CAP               PIC 9(8) COMP.
      * @field CDD-OBF-NL-KIND Output newline mode.
           05  CDD-OBF-NL-KIND           PIC 9(2) COMP.
      * @field CDD-OBF-DATA Synchronized COBOL output bytes.
           05  CDD-OBF-DATA              PIC X(1048576).
       COPY "cdd-err.cpy".
      * @subprogram CDDSYNC Surgically sync COBOL UDT & serde to JSON.
      * @param [in] CDD-SDK-OPTS Dialect, format, target UDT, & flags.
      * @param [in] CDD-BUF Existing COBOL source buffer to update.
      * @param [in] CDD-JSON-BUF Target JSON Schema or sample buffer.
      * @param [inout] CDD-OUT-BUF Output synchronized COBOL buffer.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Trivia-preserving updated COBOL source in CDD-OUT-BUF.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH, CDD-ERR-SYNC-UDT-NOT-FOUND,
      *         CDD-ERR-SYNC-AMBIGUOUS-UDT, CDD-ERR-GEN-BUF-OVERFLOW.
       PROCEDURE DIVISION USING CDD-SDK-OPTS CDD-BUF CDD-JSON-BUF
           CDD-OUT-BUF CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, parse, diff, patch, emit.
       MAIN-ENTRY.
           IF NOT CDD-OPT-TAG-VALID
               OR NOT CDD-BUF-TAG-VALID
               OR NOT CDD-JBF-TAG-VALID
               OR NOT CDD-OBF-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDSYNC" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDSYNC"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           PERFORM INIT-SYNC-PIPELINE
           IF CDD-OK
               PERFORM BUILD-NEW-AND-OLD-IR
           END-IF
           IF CDD-OK
               PERFORM INFER-LOCAL-UDT-STYLE
               PERFORM COMPUTE-AND-APPLY-DIFF
           END-IF
           IF CDD-OK
               PERFORM MAYBE-SYNC-SERDE-ROUTINES
           END-IF
           IF CDD-OK
               CALL "CDDCSTS" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   CDD-OUT-BUF CDD-ERROR
               END-CALL
           END-IF
           EXIT PROGRAM.
      * @paragraph INIT-SYNC-PIPELINE Initialize tags and capacities.
       INIT-SYNC-PIPELINE.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCST01" TO CDD-CST-TAG
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDFMP01" TO CDD-FMP-TAG
           MOVE "CDDDIF01" TO CDD-DIF-TAG
           MOVE "CDDCPR01" TO CDD-CPR-TAG
           MOVE "CDDDLR01" TO CDD-DLR-TAG
           MOVE 8192 TO CDD-CTK-CAP
           MOVE 512 TO CDD-CST-CAP
           MOVE 131072 TO CDD-CST-PATCH-CAP
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE 512 TO CDD-DIF-CAP
           MOVE 1 TO CDD-DLR-OP
           MOVE CDD-OPT-DIALECT TO CDD-DLR-DIALECT
           CALL "CDDDIAL" USING CDD-DIAL-REQ CDD-ERROR
           END-CALL.
      * @paragraph BUILD-NEW-AND-OLD-IR Parse JSON IR and COBOL CST/IR.
       BUILD-NEW-AND-OLD-IR.
           MOVE CDD-OPT-DIALECT TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           IF CDD-DLR-HAS-TYPEDEF-OO = 1
               MOVE CDD-OPT-UDT-KIND TO CDD-MPR-UDT-KIND
           END-IF
           MOVE CDD-OPT-JSON-MODE TO CDD-MPR-JSON-MODE
           MOVE CDD-OPT-CASE TO CDD-MPR-CASE
           MOVE CDD-OPT-NAME-LEN TO CDD-MPR-NAME-LEN
           MOVE CDD-OPT-TARGET-NAME TO CDD-MPR-TARGET-NAME
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-JSON-BUF CDD-IR
               CDD-ERROR
           END-CALL
           IF CDD-OK
               MOVE CDD-OPT-DIALECT TO CDD-CTK-DIALECT
               MOVE CDD-OPT-FORMAT TO CDD-CTK-FMT-REQ
               CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
               END-CALL
           END-IF
           IF CDD-OK
               CALL "CDDCSTB" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   CDD-ERROR
               END-CALL
           END-IF
           IF CDD-OK
               PERFORM EXTRACT-EXISTING-UDT-IR
           END-IF.
      * @paragraph EXTRACT-EXISTING-UDT-IR Resolve target UDT in CST.
       EXTRACT-EXISTING-UDT-IR.
           MOVE CDD-IR TO WS-OLD-IR
           MOVE 1 TO CDD-MPR-STRICT-UDT
           MOVE CDD-OPT-NAME-LEN TO CDD-MPR-NAME-LEN
           MOVE CDD-OPT-TARGET-NAME TO CDD-MPR-TARGET-NAME
           CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-CTOK-STREAM
               CDD-CST WS-OLD-IR CDD-IR-FIELD-MAP CDD-ERROR
           END-CALL
           IF CDD-ERR-SYNC-AMBIGUOUS-UDT AND CDD-IR-TITLE-LEN > 0
               MOVE 0 TO CDD-ERR-CODE
               COMPUTE CDD-MPR-NAME-LEN = CDD-IR-COB-NLEN
               END-COMPUTE
               MOVE CDD-IR-COB-NAME TO CDD-MPR-TARGET-NAME
               CALL "CDDC2IR" USING CDD-IR-MAP-REQ CDD-BUF
                   CDD-CTOK-STREAM CDD-CST WS-OLD-IR CDD-IR-FIELD-MAP
                   CDD-ERROR
               END-CALL
           END-IF
           IF CDD-OK
               MOVE CDD-MPR-MATCHED-CST TO WS-UDT-CST
           END-IF.
      * @paragraph INFER-LOCAL-UDT-STYLE Detect seq, cols, PIC, & tag.
       INFER-LOCAL-UDT-STYLE.
           IF CDD-CTK-IS-FREE
               MOVE 1 TO WS-STY-FREE
           ELSE
               MOVE 0 TO WS-STY-FREE
           END-IF
           MOVE 0 TO WS-STY-HAS-SEQ
           MOVE 100 TO WS-STY-SEQ-CUR
           MOVE 5 TO WS-STY-BASE-LVL
           MOVE 12 TO WS-STY-BASE-COL
           MOVE 40 TO WS-STY-PIC-COL
           MOVE "PIC " TO WS-STY-PIC-KW
           MOVE 4 TO WS-STY-PIC-KW-LEN
           IF CDD-OPT-DIAL-60
               MOVE "PICTURE IS " TO WS-STY-PIC-KW
               MOVE 11 TO WS-STY-PIC-KW-LEN
           ELSE
               IF CDD-OPT-DIAL-61 OR CDD-OPT-DIAL-61EXT
                   MOVE "PICTURE " TO WS-STY-PIC-KW
                   MOVE 8 TO WS-STY-PIC-KW-LEN
               END-IF
           END-IF
           MOVE 0 TO WS-STY-HAS-IDENT
           MOVE SPACES TO WS-STY-IDENT-TAG
           PERFORM INSPECT-UDT-HEADER-TRIVIA
           PERFORM INSPECT-FIRST-SIBLING-STYLE.
      * @paragraph INSPECT-UDT-HEADER-TRIVIA Check cols 1-6 & 73-80.
       INSPECT-UDT-HEADER-TRIVIA.
           MOVE CDD-CSN-TOK-START(WS-UDT-CST) TO WS-TI
           PERFORM UNTIL WS-TI > CDD-CSN-OWN-END(WS-UDT-CST)
               IF CDD-CTI-TRV-SEQ(WS-TI) AND CDD-CTI-LEN(WS-TI) = 6
                   PERFORM TRY-PARSE-SEQ-SLICE
               END-IF
               IF CDD-CTI-TRV-IDENT(WS-TI) AND CDD-CTI-LEN(WS-TI) >= 1
                   MOVE 1 TO WS-STY-HAS-IDENT
                   IF CDD-CTI-LEN(WS-TI) > 8
                       MOVE 8 TO WS-STY-IDENT-LEN
                   ELSE
                       COMPUTE WS-STY-IDENT-LEN = CDD-CTI-LEN(WS-TI)
                       END-COMPUTE
                   END-IF
                   MOVE CDD-BUF-DATA(
                       CDD-CTI-OFFSET(WS-TI):WS-STY-IDENT-LEN)
                       TO WS-STY-IDENT-TAG(1:WS-STY-IDENT-LEN)
               END-IF
               ADD 1 TO WS-TI
               END-ADD
           END-PERFORM.
      * @paragraph TRY-PARSE-SEQ-SLICE Check if 6-char seq is numeric.
       TRY-PARSE-SEQ-SLICE.
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 6 TO CDD-SRQ-OP
           MOVE 6 TO CDD-SRQ-IN-LEN
           MOVE CDD-BUF-DATA(CDD-CTI-OFFSET(WS-TI):6)
               TO CDD-SRQ-IN-TEXT(1:6)
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-OK
               MOVE 1 TO WS-STY-HAS-SEQ
               COMPUTE WS-STY-SEQ-CUR = CDD-SRQ-NUM-VAL
               END-COMPUTE
           ELSE
               MOVE 0 TO CDD-ERR-CODE
           END-IF.
      * @paragraph INSPECT-FIRST-SIBLING-STYLE Infer indent & PIC col.
       INSPECT-FIRST-SIBLING-STYLE.
           MOVE 0 TO WS-FIRST-FLD-CST
           MOVE CDD-CSN-FIRST-CHILD(WS-UDT-CST) TO WS-FIRST-FLD-CST
           PERFORM UNTIL WS-FIRST-FLD-CST = 0
               OR CDD-CSN-IS-FIELD(WS-FIRST-FLD-CST)
               MOVE CDD-CSN-NEXT-SIB(WS-FIRST-FLD-CST)
                   TO WS-FIRST-FLD-CST
           END-PERFORM
           IF WS-FIRST-FLD-CST > 0
               MOVE CDD-CSN-LEVEL(WS-FIRST-FLD-CST) TO WS-STY-BASE-LVL
               MOVE CDD-CTI-COL(CDD-CSN-CODE-START(WS-FIRST-FLD-CST))
                   TO WS-STY-BASE-COL
           END-IF
           IF CDD-FMP-TYPE-TOK1(1) > 0
               MOVE CDD-FMP-TYPE-TOK1(1) TO WS-TI
               MOVE CDD-CTI-COL(WS-TI) TO WS-STY-PIC-COL
               MOVE SPACES TO WS-TOK-PEEK
               MOVE CDD-BUF-DATA(
                   CDD-CTI-OFFSET(WS-TI):CDD-CTI-LEN(WS-TI))
                   TO WS-TOK-PEEK(1:CDD-CTI-LEN(WS-TI))
               INSPECT WS-TOK-PEEK
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               IF WS-TOK-PEEK(1:8) = "PICTURE "
                   IF WS-TI + 2 <= CDD-CTK-COUNT
                       AND CDD-BUF-DATA(
                           CDD-CTI-OFFSET(WS-TI + 2):2) = "IS"
                       MOVE "PICTURE IS " TO WS-STY-PIC-KW
                       MOVE 11 TO WS-STY-PIC-KW-LEN
                   ELSE
                       MOVE "PICTURE " TO WS-STY-PIC-KW
                       MOVE 8 TO WS-STY-PIC-KW-LEN
                   END-IF
               ELSE
                   IF WS-TOK-PEEK(1:4) = "PIC "
                       MOVE "PIC " TO WS-STY-PIC-KW
                       MOVE 4 TO WS-STY-PIC-KW-LEN
                   END-IF
               END-IF
           END-IF.
      * @paragraph COMPUTE-AND-APPLY-DIFF Run CDDIRDF and apply ops.
       COMPUTE-AND-APPLY-DIFF.
           CALL "CDDIRDF" USING CDD-IR-MAP-REQ WS-OLD-IR
               CDD-IR-FIELD-MAP CDD-IR CDD-DIFF-PLAN CDD-ERROR
           END-CALL
           MOVE 0 TO WS-HAS-MUTATION
           MOVE 0 TO WS-LAST-ANCHOR-PAR
           MOVE 0 TO WS-LAST-ORIG-ANC
           MOVE 0 TO WS-CHAIN-ANC
           MOVE 1 TO WS-DI
           PERFORM UNTIL WS-DI > CDD-DIF-COUNT
               PERFORM APPLY-ONE-DIFF-OP
               ADD 1 TO WS-DI
               END-ADD
           END-PERFORM.
      * @paragraph APPLY-ONE-DIFF-OP Dispatch KEEP/UPDATE/INSERT/REMOVE.
       APPLY-ONE-DIFF-OP.
           EVALUATE TRUE
               WHEN CDD-DFO-REMOVE(WS-DI)
                   MOVE 1 TO WS-HAS-MUTATION
                   MOVE CDD-DFO-CST-NODE(WS-DI) TO WS-CST-TARGET
                   MOVE 1 TO CDD-CSN-PATCH-MODE(WS-CST-TARGET)
               WHEN CDD-DFO-UPDATE(WS-DI)
                   MOVE 1 TO WS-HAS-MUTATION
                   PERFORM APPLY-SPLICE-UPDATE
               WHEN CDD-DFO-INSERT(WS-DI)
                   MOVE 1 TO WS-HAS-MUTATION
                   PERFORM APPLY-FIELD-INSERT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      * @paragraph APPLY-SPLICE-UPDATE Splice PIC/USAGE/OCCURS clause.
       APPLY-SPLICE-UPDATE.
           MOVE CDD-DFO-OLD-IR(WS-DI) TO WS-OLD-FIDX
           MOVE CDD-DFO-NEW-IR(WS-DI) TO WS-FIDX
           MOVE CDD-DFO-CST-NODE(WS-DI) TO WS-CST-TARGET
           PERFORM BUILD-FIELD-CLAUSE-TEXT
           COMPUTE WS-PATCH-START = CDD-CST-PATCH-LEN + 1
           END-COMPUTE
           IF WS-CLEN > 0
               MOVE WS-CLAUSE-BUF(1:WS-CLEN)
                   TO CDD-CST-PATCH-BUF(WS-PATCH-START:WS-CLEN)
               ADD WS-CLEN TO CDD-CST-PATCH-LEN
               END-ADD
           END-IF
           MOVE 3 TO CDD-CSN-PATCH-MODE(WS-CST-TARGET)
           IF CDD-FMP-TYPE-TOK1(WS-OLD-FIDX) > 0
               MOVE CDD-FMP-TYPE-TOK1(WS-OLD-FIDX)
                   TO CDD-CSN-SPLICE-T1(WS-CST-TARGET)
               MOVE CDD-FMP-TYPE-TOK2(WS-OLD-FIDX)
                   TO CDD-CSN-SPLICE-T2(WS-CST-TARGET)
           ELSE
               MOVE CDD-CSN-CODE-END(WS-CST-TARGET)
                   TO CDD-CSN-SPLICE-T1(WS-CST-TARGET)
               COMPUTE CDD-CSN-SPLICE-T2(WS-CST-TARGET) =
                   CDD-CSN-CODE-END(WS-CST-TARGET) - 1
               END-COMPUTE
           END-IF
           MOVE WS-PATCH-START
               TO CDD-CSN-PATCH-OFF(WS-CST-TARGET)
           MOVE WS-CLEN TO CDD-CSN-PATCH-LEN(WS-CST-TARGET).
      * @paragraph APPLY-FIELD-INSERT Synthesize & link new CST node.
       APPLY-FIELD-INSERT.
           MOVE CDD-DFO-NEW-IR(WS-DI) TO WS-FIDX
           MOVE CDD-DFO-PARENT-CST(WS-DI) TO WS-PAR-CST
           IF WS-PAR-CST = WS-LAST-ANCHOR-PAR
               AND CDD-DFO-CST-NODE(WS-DI) = WS-LAST-ORIG-ANC
               AND WS-CHAIN-ANC > 0
               MOVE WS-CHAIN-ANC TO WS-EFF-ANC
           ELSE
               MOVE CDD-DFO-CST-NODE(WS-DI) TO WS-EFF-ANC
               MOVE WS-PAR-CST TO WS-LAST-ANCHOR-PAR
               MOVE CDD-DFO-CST-NODE(WS-DI) TO WS-LAST-ORIG-ANC
           END-IF
           PERFORM INTERPOLATE-SEQ-FROM-ANCHOR
           COMPUTE WS-PATCH-START = CDD-CST-PATCH-LEN + 1
           END-COMPUTE
           PERFORM SYNTHESIZE-FIELD-SUBTREE
           ADD 1 TO CDD-CST-COUNT
           END-ADD
           MOVE CDD-CST-COUNT TO WS-NEW-CST
           MOVE 6 TO CDD-CSN-KIND(WS-NEW-CST)
           MOVE WS-PAR-CST TO CDD-CSN-PARENT(WS-NEW-CST)
           MOVE 0 TO CDD-CSN-FIRST-CHILD(WS-NEW-CST)
           MOVE 0 TO CDD-CSN-LAST-CHILD(WS-NEW-CST)
           MOVE 0 TO CDD-CSN-FOOT-START(WS-NEW-CST)
           MOVE 2 TO CDD-CSN-PATCH-MODE(WS-NEW-CST)
           MOVE WS-PATCH-START
               TO CDD-CSN-PATCH-OFF(WS-NEW-CST)
           COMPUTE CDD-CSN-PATCH-LEN(WS-NEW-CST) =
               CDD-CST-PATCH-LEN - WS-PATCH-START + 1
           END-COMPUTE
           PERFORM LINK-INSERTED-CST-NODE
           MOVE WS-NEW-CST TO WS-CHAIN-ANC.
      * @paragraph LINK-INSERTED-CST-NODE Splice after WS-EFF-ANC.
       LINK-INSERTED-CST-NODE.
           IF WS-EFF-ANC = 0
               MOVE CDD-CSN-FIRST-CHILD(WS-PAR-CST)
                   TO CDD-CSN-NEXT-SIB(WS-NEW-CST)
               MOVE WS-NEW-CST TO CDD-CSN-FIRST-CHILD(WS-PAR-CST)
               IF CDD-CSN-LAST-CHILD(WS-PAR-CST) = 0
                   MOVE WS-NEW-CST TO CDD-CSN-LAST-CHILD(WS-PAR-CST)
               END-IF
           ELSE
               MOVE CDD-CSN-NEXT-SIB(WS-EFF-ANC)
                   TO CDD-CSN-NEXT-SIB(WS-NEW-CST)
               MOVE WS-NEW-CST TO CDD-CSN-NEXT-SIB(WS-EFF-ANC)
               IF CDD-CSN-LAST-CHILD(WS-PAR-CST) = WS-EFF-ANC
                   MOVE WS-NEW-CST TO CDD-CSN-LAST-CHILD(WS-PAR-CST)
               END-IF
           END-IF.
      * @paragraph INTERPOLATE-SEQ-FROM-ANCHOR Advance seq +5.
       INTERPOLATE-SEQ-FROM-ANCHOR.
           IF WS-STY-HAS-SEQ = 1
               IF WS-EFF-ANC > 0
                   AND CDD-CSN-PM-KEEP(WS-EFF-ANC)
                   MOVE CDD-CSN-TOK-START(WS-EFF-ANC) TO WS-TI
                   PERFORM TRY-PARSE-SEQ-SLICE
               END-IF
               ADD 5 TO WS-STY-SEQ-CUR
               END-ADD
           END-IF.
      * @paragraph SYNTHESIZE-FIELD-SUBTREE Emit field (+ kids if grp).
       SYNTHESIZE-FIELD-SUBTREE.
           PERFORM COMPUTE-PARENT-DEPTH
           PERFORM APPEND-SYNTH-FIELD-LINES
           IF CDD-IRF-IS-GROUP(WS-FIDX)
               AND CDD-IRF-FIRST-CHILD(WS-FIDX) > 0
               MOVE 1 TO WS-INS-DEPTH
               MOVE CDD-IRF-FIRST-CHILD(WS-FIDX) TO WS-INS-IDX(1)
               PERFORM UNTIL WS-INS-DEPTH = 0 OR NOT CDD-OK
                   IF WS-INS-IDX(WS-INS-DEPTH) = 0
                       SUBTRACT 1 FROM WS-INS-DEPTH
                       END-SUBTRACT
                       SUBTRACT 1 FROM WS-CUR-DEPTH
                       END-SUBTRACT
                   ELSE
                       MOVE WS-INS-IDX(WS-INS-DEPTH) TO WS-FIDX
                       MOVE CDD-IRF-NEXT-SIB(WS-FIDX)
                           TO WS-INS-IDX(WS-INS-DEPTH)
                       ADD 1 TO WS-CUR-DEPTH
                       END-ADD
                       PERFORM APPEND-SYNTH-FIELD-LINES
                       IF CDD-IRF-IS-GROUP(WS-FIDX)
                           AND CDD-IRF-FIRST-CHILD(WS-FIDX) > 0
                           ADD 1 TO WS-INS-DEPTH
                           END-ADD
                           MOVE CDD-IRF-FIRST-CHILD(WS-FIDX)
                               TO WS-INS-IDX(WS-INS-DEPTH)
                       ELSE
                           SUBTRACT 1 FROM WS-CUR-DEPTH
                           END-SUBTRACT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.
      * @paragraph COMPUTE-PARENT-DEPTH Count CST ancestors to UDT.
       COMPUTE-PARENT-DEPTH.
           MOVE 1 TO WS-CUR-DEPTH
           MOVE WS-PAR-CST TO WS-CST-TARGET
           PERFORM UNTIL WS-CST-TARGET = 0
               OR WS-CST-TARGET = WS-UDT-CST
               ADD 1 TO WS-CUR-DEPTH
               END-ADD
               MOVE CDD-CSN-PARENT(WS-CST-TARGET) TO WS-CST-TARGET
           END-PERFORM.
      * @paragraph APPEND-SYNTH-FIELD-LINES Format ODO + doc + field.
       APPEND-SYNTH-FIELD-LINES.
           COMPUTE WS-LVL-NUM = WS-STY-BASE-LVL * WS-CUR-DEPTH
           END-COMPUTE
           IF WS-LVL-NUM > 45
               MOVE 49 TO WS-LVL-NUM
           END-IF
           MOVE WS-LVL-NUM TO WS-LVL-DISP
           IF WS-STY-FREE = 1
               COMPUTE WS-COL-START = (WS-CUR-DEPTH * 2) + 1
               END-COMPUTE
           ELSE
               COMPUTE WS-COL-START =
                   WS-STY-BASE-COL + ((WS-CUR-DEPTH - 1) * 4)
               END-COMPUTE
               IF WS-COL-START > 24
                   MOVE 28 TO WS-COL-START
               END-IF
           END-IF
           IF CDD-IRF-ARR-ODO(WS-FIDX) AND CDD-DLR-HAS-ODO = 1
               MOVE SPACES TO WS-LINE-BUF
               PERFORM WRITE-SEQ-PREFIX
               MOVE WS-COL-START TO WS-LPOS
               STRING WS-LVL-DISP DELIMITED BY SIZE
                      "  " DELIMITED BY SIZE
                      CDD-IRF-COB-NAME(WS-FIDX)(
                          1:CDD-IRF-CNAM-LEN(WS-FIDX))
                          DELIMITED BY SIZE
                      "-COUNT PIC 9(4) COMP." DELIMITED BY SIZE
                      INTO WS-LINE-BUF WITH POINTER WS-LPOS
               END-STRING
               PERFORM APPEND-LINE-TO-PATCH-BUF
               IF WS-STY-HAS-SEQ = 1
                   ADD 1 TO WS-STY-SEQ-CUR
                   END-ADD
               END-IF
           END-IF
           PERFORM BUILD-FIELD-CLAUSE-TEXT
           MOVE SPACES TO WS-LINE-BUF
           PERFORM WRITE-SEQ-PREFIX
           MOVE WS-COL-START TO WS-LPOS
           STRING WS-LVL-DISP DELIMITED BY SIZE
                  "  " DELIMITED BY SIZE
                  CDD-IRF-COB-NAME(WS-FIDX)(1:CDD-IRF-CNAM-LEN(WS-FIDX))
                      DELIMITED BY SIZE
                  INTO WS-LINE-BUF WITH POINTER WS-LPOS
           END-STRING
           IF WS-CLEN > 0
               IF WS-STY-FREE = 0 AND WS-LPOS < WS-STY-PIC-COL
                   MOVE WS-STY-PIC-COL TO WS-LPOS
               ELSE
                   ADD 1 TO WS-LPOS
                   END-ADD
               END-IF
               IF WS-STY-FREE = 0 AND (WS-LPOS + WS-CLEN) > 72
                   PERFORM APPEND-LINE-TO-PATCH-BUF
                   MOVE SPACES TO WS-LINE-BUF
                   IF WS-STY-HAS-SEQ = 1
                       ADD 1 TO WS-STY-SEQ-CUR
                       END-ADD
                   END-IF
                   PERFORM WRITE-SEQ-PREFIX
                   MOVE 16 TO WS-LPOS
               END-IF
               MOVE WS-CLAUSE-BUF(1:WS-CLEN)
                   TO WS-LINE-BUF(WS-LPOS:WS-CLEN)
               ADD WS-CLEN TO WS-LPOS
               END-ADD
           END-IF
           MOVE "." TO WS-LINE-BUF(WS-LPOS:1)
           PERFORM APPEND-LINE-TO-PATCH-BUF.
      * @paragraph WRITE-SEQ-PREFIX Write 6-digit seq if local has seq.
       WRITE-SEQ-PREFIX.
           IF WS-STY-FREE = 0 AND WS-STY-HAS-SEQ = 1
               MOVE WS-STY-SEQ-CUR TO WS-STY-SEQ-DISP
               MOVE WS-STY-SEQ-DISP TO WS-LINE-BUF(1:6)
           END-IF.
      * @paragraph APPEND-LINE-TO-PATCH-BUF Append line + tag + NL.
       APPEND-LINE-TO-PATCH-BUF.
           IF WS-STY-FREE = 0 AND WS-STY-HAS-IDENT = 1
               MOVE WS-STY-IDENT-TAG(1:WS-STY-IDENT-LEN)
                   TO WS-LINE-BUF(73:WS-STY-IDENT-LEN)
               COMPUTE WS-SLEN = 72 + WS-STY-IDENT-LEN
               END-COMPUTE
           ELSE
               MOVE 256 TO WS-SLEN
               PERFORM UNTIL WS-SLEN = 0
                   OR WS-LINE-BUF(WS-SLEN:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-SLEN
                   END-SUBTRACT
               END-PERFORM
           END-IF
           MOVE WS-LINE-BUF(1:WS-SLEN)
               TO CDD-CST-PATCH-BUF(CDD-CST-PATCH-LEN + 1:WS-SLEN)
           ADD WS-SLEN TO CDD-CST-PATCH-LEN
           END-ADD
           IF CDD-BUF-NL-CRLF
               ADD 1 TO CDD-CST-PATCH-LEN
               END-ADD
               MOVE WS-CR TO CDD-CST-PATCH-BUF(CDD-CST-PATCH-LEN:1)
           END-IF
           ADD 1 TO CDD-CST-PATCH-LEN
           END-ADD
           MOVE WS-LF TO CDD-CST-PATCH-BUF(CDD-CST-PATCH-LEN:1).
      * @paragraph FMT-U32-HELPER Format WS-NUM-U32 into WS-NUM-TXT.
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
      * @paragraph BUILD-FIELD-CLAUSE-TEXT Format PIC/USAGE/OCCURS.
       BUILD-FIELD-CLAUSE-TEXT.
           MOVE SPACES TO WS-CLAUSE-BUF
           MOVE 1 TO WS-CLEN
           IF NOT CDD-IRF-IS-GROUP(WS-FIDX)
               PERFORM APPEND-SYNC-TYPE-CLAUSE
           END-IF
           IF NOT CDD-IRF-ARR-NONE(WS-FIDX)
               IF WS-CLEN > 1
                   STRING " " DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
               PERFORM APPEND-SYNC-OCCURS-CLAUSE
           END-IF
           SUBTRACT 1 FROM WS-CLEN
           END-SUBTRACT.
      * @paragraph APPEND-SYNC-TYPE-CLAUSE Format scalar PIC/USAGE.
       APPEND-SYNC-TYPE-CLAUSE.
           EVALUATE TRUE
               WHEN CDD-IRF-IS-BOOL(WS-FIDX)
                   IF CDD-DLR-HAS-TYPEDEF-OO = 1
                       STRING "PIC 1 USAGE BIT" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   ELSE
                       STRING WS-STY-PIC-KW(1:WS-STY-PIC-KW-LEN)
                                  DELIMITED BY SIZE
                              "X(1)" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
               WHEN CDD-IRF-IS-FLOAT(WS-FIDX)
                   STRING "USAGE COMP-2" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               WHEN CDD-IRF-IS-INTEGER(WS-FIDX)
               WHEN CDD-IRF-IS-DECIMAL(WS-FIDX)
               WHEN CDD-IRF-IS-DECFLOAT(WS-FIDX)
                   STRING WS-STY-PIC-KW(1:WS-STY-PIC-KW-LEN)
                              DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
                   IF CDD-IRF-IS-SIGNED(WS-FIDX)
                       STRING "S" DELIMITED BY SIZE
                              INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                       END-STRING
                   END-IF
                   COMPUTE WS-INT-DIGITS =
                       CDD-IRF-DIGITS(WS-FIDX) - CDD-IRF-SCALE(WS-FIDX)
                   END-COMPUTE
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
                   END-IF
               WHEN OTHER
                   IF CDD-IRF-MAX-LEN(WS-FIDX) = 0
                       MOVE 32 TO WS-NUM-U32
                   ELSE
                       MOVE CDD-IRF-MAX-LEN(WS-FIDX) TO WS-NUM-U32
                   END-IF
                   PERFORM FMT-U32-HELPER
                   STRING WS-STY-PIC-KW(1:WS-STY-PIC-KW-LEN)
                              DELIMITED BY SIZE
                          "X(" DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
           END-EVALUATE.
      * @paragraph APPEND-SYNC-OCCURS-CLAUSE Format OCCURS clause.
       APPEND-SYNC-OCCURS-CLAUSE.
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
                   MOVE CDD-IRF-ARR-MAX(WS-FIDX) TO WS-NUM-U32
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
                   MOVE CDD-IRF-ARR-MAX(WS-FIDX) TO WS-NUM-U32
                   PERFORM FMT-U32-HELPER
                   STRING "OCCURS " DELIMITED BY SIZE
                          WS-NUM-TXT(1:WS-NUM-LEN) DELIMITED BY SIZE
                          " TIMES" DELIMITED BY SIZE
                          INTO WS-CLAUSE-BUF WITH POINTER WS-CLEN
                   END-STRING
               END-IF
           END-IF.
      * @paragraph MAYBE-SYNC-SERDE-ROUTINES Splice TO/FROM-JSON bodies.
       MAYBE-SYNC-SERDE-ROUTINES.
           IF CDD-OPT-SYNC-SERDE NOT = 2
               AND (WS-HAS-MUTATION = 1 OR CDD-OPT-SYNC-SERDE = 1)
               MOVE CDD-CSN-NAME-LEN(WS-UDT-CST) TO CDD-CPR-UDT-NLEN
               MOVE CDD-CSN-NAME(WS-UDT-CST) TO CDD-CPR-UDT-NAME
               CALL "CDDCPRO" USING CDD-BUF CDD-CTOK-STREAM CDD-CST
                   CDD-CPRO-INFO CDD-ERROR
               END-CALL
               IF CDD-CPR-TO-NODE > 0
                   AND CDD-CPR-TO-B2 >= CDD-CPR-TO-B1
                   MOVE 8 TO WS-SRD-MODE
                   MOVE CDD-CPR-TO-NODE TO WS-CST-TARGET
                   MOVE CDD-CPR-TO-B1
                       TO CDD-CSN-SPLICE-T1(WS-CST-TARGET)
                   MOVE CDD-CPR-TO-B2
                       TO CDD-CSN-SPLICE-T2(WS-CST-TARGET)
                   PERFORM SPLICE-SERDE-BODY
               END-IF
               IF CDD-OK
                   AND CDD-CPR-FROM-NODE > 0
                   AND CDD-CPR-FROM-B2 >= CDD-CPR-FROM-B1
                   MOVE 9 TO WS-SRD-MODE
                   MOVE CDD-CPR-FROM-NODE TO WS-CST-TARGET
                   MOVE CDD-CPR-FROM-B1
                       TO CDD-CSN-SPLICE-T1(WS-CST-TARGET)
                   MOVE CDD-CPR-FROM-B2
                       TO CDD-CSN-SPLICE-T2(WS-CST-TARGET)
                   PERFORM SPLICE-SERDE-BODY
               END-IF
           END-IF.
      * @paragraph SPLICE-SERDE-BODY Generate body & attach PM-SPLICE.
       SPLICE-SERDE-BODY.
           MOVE CDD-SDK-OPTS TO WS-SUB-OPTS
           MOVE WS-SRD-MODE TO WS-SOP-SYNC-SRD
           IF CDD-CPR-KIND = 1 OR CDD-CPR-KIND = 2
               MOVE 1 TO WS-SOP-SERDE-KIND
           END-IF
           IF WS-STY-FREE = 1
               MOVE 2 TO WS-SOP-FORMAT
           ELSE
               MOVE 1 TO WS-SOP-FORMAT
           END-IF
           MOVE "CDDBUF01" TO WS-GBF-TAG
           MOVE 1048576 TO WS-GBF-CAP
           MOVE CDD-BUF-NL-KIND TO WS-GBF-NL
           MOVE CDD-CSN-NAME-LEN(WS-UDT-CST) TO CDD-IR-COB-NLEN
           MOVE CDD-CSN-NAME(WS-UDT-CST) TO CDD-IR-COB-NAME
           CALL "CDDGENS" USING WS-SUB-OPTS CDD-IR WS-GEN-BUF
               CDD-ERROR
           END-CALL
           IF CDD-OK
               COMPUTE WS-PATCH-START = CDD-CST-PATCH-LEN + 1
               END-COMPUTE
               MOVE WS-GBF-DATA(1:WS-GBF-LEN)
                   TO CDD-CST-PATCH-BUF(WS-PATCH-START:WS-GBF-LEN)
               ADD WS-GBF-LEN TO CDD-CST-PATCH-LEN
               END-ADD
               MOVE 3 TO CDD-CSN-PATCH-MODE(WS-CST-TARGET)
               MOVE WS-PATCH-START
                   TO CDD-CSN-PATCH-OFF(WS-CST-TARGET)
               MOVE WS-GBF-LEN
                   TO CDD-CSN-PATCH-LEN(WS-CST-TARGET)
           END-IF.
