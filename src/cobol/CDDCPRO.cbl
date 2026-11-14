      * @file CDDCPRO.cbl
      * @brief Procedure Division serde routine & trivia boundary finder.
      * @module CDDCPRO
      * @details Locates <UDT>-TO-JSON / TO-JSON and <UDT>-FROM-JSON /
      *          FROM-JSON routines in CDD-CST while isolating leading
      *          trivia above each routine and trailing trivia/code
      *          below each routine terminator.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDDCPRO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * @record WS-PRO-STATE Search names and CST scan cursors.
       01  WS-PRO-STATE.
      * @field WS-BASE-LEN Length of stripped base UDT name.
           05  WS-BASE-LEN               PIC 9(2) COMP VALUE 0.
      * @field WS-TO-LEN1 Length of <UDT>-TO-JSON candidate.
           05  WS-TO-LEN1                PIC 9(2) COMP VALUE 0.
      * @field WS-TO-LEN2 Length of <BASE>-TO-JSON candidate.
           05  WS-TO-LEN2                PIC 9(2) COMP VALUE 0.
      * @field WS-FR-LEN1 Length of <UDT>-FROM-JSON candidate.
           05  WS-FR-LEN1                PIC 9(2) COMP VALUE 0.
      * @field WS-FR-LEN2 Length of <BASE>-FROM-JSON candidate.
           05  WS-FR-LEN2                PIC 9(2) COMP VALUE 0.
      * @field WS-NIDX CST node scan index.
           05  WS-NIDX                   PIC 9(4) COMP VALUE 0.
      * @field WS-B1 Computed first body token of routine.
           05  WS-B1                     PIC 9(8) COMP VALUE 0.
      * @field WS-B2 Computed last body token of routine.
           05  WS-B2                     PIC 9(8) COMP VALUE 0.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(4) COMP VALUE 1.
      * @record WS-BASE-NAME UDT name stripped of -REC/-T suffix.
       01  WS-BASE-NAME                  PIC X(64) VALUE SPACES.
      * @record WS-CAND-TO1 Full <UDT>-TO-JSON routine name.
       01  WS-CAND-TO1                   PIC X(64) VALUE SPACES.
      * @record WS-CAND-TO2 Stripped <BASE>-TO-JSON routine name.
       01  WS-CAND-TO2                   PIC X(64) VALUE SPACES.
      * @record WS-CAND-FR1 Full <UDT>-FROM-JSON routine name.
       01  WS-CAND-FR1                   PIC X(64) VALUE SPACES.
      * @record WS-CAND-FR2 Stripped <BASE>-FROM-JSON routine name.
       01  WS-CAND-FR2                   PIC X(64) VALUE SPACES.
       LINKAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-err.cpy".
      * @subprogram CDDCPRO Locate TO-JSON and FROM-JSON CST routines.
      * @param [in] CDD-BUF Source byte buffer.
      * @param [in] CDD-CTOK-STREAM Tokenized COBOL stream.
      * @param [in] CDD-CST Lossless Concrete Syntax Tree.
      * @param [inout] CDD-CPRO-INFO Target UDT name & output spans.
      * @param [inout] CDD-ERROR Unified error enum output.
      * @return Node indices and interior body token spans.
      * @errors CDD-ERR-TYPE-TAG-MISMATCH.
       PROCEDURE DIVISION USING CDD-BUF CDD-CTOK-STREAM CDD-CST
           CDD-CPRO-INFO CDD-ERROR.
      * @paragraph MAIN-ENTRY Validate tags, build names, scan CST.
       MAIN-ENTRY.
           IF NOT CDD-BUF-TAG-VALID
               OR NOT CDD-CTK-TAG-VALID
               OR NOT CDD-CST-TAG-VALID
               OR NOT CDD-CPR-TAG-VALID
               MOVE "CDDERR01" TO CDD-ERR-TAG
               MOVE 9001 TO CDD-ERR-CODE
               MOVE "CDDCPRO" TO CDD-ERR-SUBPROG
               MOVE "Invalid envelope tag in CDDCPRO"
                   TO CDD-ERR-MESSAGE
               EXIT PROGRAM
           END-IF
           MOVE "CDDERR01" TO CDD-ERR-TAG
           MOVE 0 TO CDD-ERR-CODE
           MOVE 0 TO CDD-CPR-TO-NODE
           MOVE 0 TO CDD-CPR-TO-B1
           MOVE 0 TO CDD-CPR-TO-B2
           MOVE 0 TO CDD-CPR-FROM-NODE
           MOVE 0 TO CDD-CPR-FROM-B1
           MOVE 0 TO CDD-CPR-FROM-B2
           MOVE 0 TO CDD-CPR-KIND
           IF CDD-CPR-UDT-NLEN > 0 AND CDD-CPR-UDT-NLEN <= 52
               PERFORM BUILD-CANDIDATE-NAMES
               PERFORM SCAN-CST-ROUTINES
           END-IF
           EXIT PROGRAM.
      * @paragraph BUILD-CANDIDATE-NAMES Derive <UDT>/<BASE> TO/FROM.
       BUILD-CANDIDATE-NAMES.
           MOVE SPACES TO WS-BASE-NAME
           MOVE CDD-CPR-UDT-NLEN TO WS-BASE-LEN
           MOVE CDD-CPR-UDT-NAME(1:WS-BASE-LEN)
               TO WS-BASE-NAME(1:WS-BASE-LEN)
           INSPECT WS-BASE-NAME(1:WS-BASE-LEN)
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           MOVE SPACES TO WS-CAND-TO1
           MOVE 1 TO WS-PTR
           STRING WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON" DELIMITED BY SIZE
                  INTO WS-CAND-TO1 WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-TO-LEN1
           END-SUBTRACT
           MOVE SPACES TO WS-CAND-FR1
           MOVE 1 TO WS-PTR
           STRING WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON" DELIMITED BY SIZE
                  INTO WS-CAND-FR1 WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-FR-LEN1
           END-SUBTRACT
           IF WS-BASE-LEN > 4
               AND WS-BASE-NAME(WS-BASE-LEN - 3:4) = "-REC"
               SUBTRACT 4 FROM WS-BASE-LEN
               END-SUBTRACT
           ELSE
               IF WS-BASE-LEN > 2
                   AND WS-BASE-NAME(WS-BASE-LEN - 1:2) = "-T"
                   SUBTRACT 2 FROM WS-BASE-LEN
                   END-SUBTRACT
               END-IF
           END-IF
           MOVE SPACES TO WS-CAND-TO2
           MOVE 1 TO WS-PTR
           STRING WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-TO-JSON" DELIMITED BY SIZE
                  INTO WS-CAND-TO2 WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-TO-LEN2
           END-SUBTRACT
           MOVE SPACES TO WS-CAND-FR2
           MOVE 1 TO WS-PTR
           STRING WS-BASE-NAME(1:WS-BASE-LEN) DELIMITED BY SIZE
                  "-FROM-JSON" DELIMITED BY SIZE
                  INTO WS-CAND-FR2 WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING WS-FR-LEN2
           END-SUBTRACT.
      * @paragraph SCAN-CST-ROUTINES Match routine CST nodes.
       SCAN-CST-ROUTINES.
           MOVE 1 TO WS-NIDX
           PERFORM UNTIL WS-NIDX > CDD-CST-COUNT
               IF CDD-CSN-IS-ROUTINE(WS-NIDX)
                   PERFORM COMPUTE-ROUTINE-BODY-SPAN
                   IF (CDD-CSN-NAME-LEN(WS-NIDX) = WS-TO-LEN1
                       AND CDD-CSN-NAME(WS-NIDX) = WS-CAND-TO1)
                       OR (CDD-CSN-NAME-LEN(WS-NIDX) = WS-TO-LEN2
                       AND CDD-CSN-NAME(WS-NIDX) = WS-CAND-TO2)
                       OR (CDD-CSN-NAME-LEN(WS-NIDX) = 7
                       AND CDD-CSN-NAME(WS-NIDX)(1:7) = "TO-JSON")
                       MOVE WS-NIDX TO CDD-CPR-TO-NODE
                       MOVE WS-B1 TO CDD-CPR-TO-B1
                       MOVE WS-B2 TO CDD-CPR-TO-B2
                       PERFORM CLASSIFY-ROUTINE-KIND
                   END-IF
                   IF (CDD-CSN-NAME-LEN(WS-NIDX) = WS-FR-LEN1
                       AND CDD-CSN-NAME(WS-NIDX) = WS-CAND-FR1)
                       OR (CDD-CSN-NAME-LEN(WS-NIDX) = WS-FR-LEN2
                       AND CDD-CSN-NAME(WS-NIDX) = WS-CAND-FR2)
                       OR (CDD-CSN-NAME-LEN(WS-NIDX) = 9
                       AND CDD-CSN-NAME(WS-NIDX)(1:9) = "FROM-JSON")
                       MOVE WS-NIDX TO CDD-CPR-FROM-NODE
                       MOVE WS-B1 TO CDD-CPR-FROM-B1
                       MOVE WS-B2 TO CDD-CPR-FROM-B2
                       PERFORM CLASSIFY-ROUTINE-KIND
                   END-IF
               END-IF
               ADD 1 TO WS-NIDX
               END-ADD
           END-PERFORM.
      * @paragraph COMPUTE-ROUTINE-BODY-SPAN Isolate interior body span.
       COMPUTE-ROUTINE-BODY-SPAN.
           COMPUTE WS-B1 = CDD-CSN-OWN-END(WS-NIDX) + 1
           END-COMPUTE
           IF CDD-CSN-FOOT-START(WS-NIDX) > 0
               COMPUTE WS-B2 = CDD-CSN-FOOT-START(WS-NIDX) - 1
               END-COMPUTE
           ELSE
               MOVE CDD-CSN-TOK-END(WS-NIDX) TO WS-B2
               PERFORM TRIM-TRAILING-BLANK-LINES
           END-IF.
      * @paragraph TRIM-TRAILING-BLANK-LINES Keep trailing gap below!
       TRIM-TRAILING-BLANK-LINES.
           PERFORM UNTIL WS-B2 < WS-B1
               IF CDD-CTI-CODE-TOK(WS-B2)
                   COMPUTE WS-B2 = WS-B2 + 1
                   END-COMPUTE
                   PERFORM UNTIL WS-B2 > CDD-CSN-TOK-END(WS-NIDX)
                       OR CDD-CTI-TRV-NL(WS-B2 - 1)
                       ADD 1 TO WS-B2
                       END-ADD
                   END-PERFORM
                   SUBTRACT 1 FROM WS-B2
                   END-SUBTRACT
                   EXIT PERFORM
               END-IF
               SUBTRACT 1 FROM WS-B2
               END-SUBTRACT
           END-PERFORM.
      * @paragraph CLASSIFY-ROUTINE-KIND Detect section/para/method/fn.
       CLASSIFY-ROUTINE-KIND.
           IF CDD-CSN-FOOT-START(WS-NIDX) > 0
               IF CDD-BUF-DATA(
                   CDD-CTI-OFFSET(CDD-CSN-CODE-START(WS-NIDX)):1) = "M"
                   OR CDD-BUF-DATA(
                   CDD-CTI-OFFSET(CDD-CSN-CODE-START(WS-NIDX)):1) = "m"
                   MOVE 4 TO CDD-CPR-KIND
               ELSE
                   MOVE 5 TO CDD-CPR-KIND
               END-IF
           ELSE
               IF CDD-CSN-CODE-END(WS-NIDX)
                   > CDD-CSN-CODE-START(WS-NIDX) + 1
                   MOVE 1 TO CDD-CPR-KIND
               ELSE
                   MOVE 2 TO CDD-CPR-KIND
               END-IF
           END-IF.
