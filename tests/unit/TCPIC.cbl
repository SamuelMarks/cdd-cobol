      * @file TCPIC.cbl
      * @brief Unit test suite for CDDCPIC PICTURE, USAGE & OCCURS.
      * @module TCPIC
      * @details Exercises standalone PIC parsing, COBOL-60 SIZE/CLASS/
      *          POINT/SIGNED, all 1960-2023 USAGE keywords, fixed/ODO/
      *          DYNAMIC OCCURS, dynamic elementary items, REDEFINES,
      *          SYNC/JUSTIFIED, TYPEDEF, VALUE literals, and errors.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCPIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-ctok.cpy".
       COPY "cdd-cst.cpy".
       COPY "cdd-err.cpy".
      * @record WS-STATE Test state and assertion counters.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-SLEN Snippet byte length.
           05  WS-SLEN                   PIC 9(4) COMP VALUE 0.
      * @record WS-SNIP COBOL field declaration snippet buffer.
       01  WS-SNIP                       PIC X(256) VALUE SPACES.
      * @subprogram TCPIC Verify 100% of CDDCPIC lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDCPIC unit test groups.
       MAIN-ENTRY.
           PERFORM TEST-ENVELOPE-GUARDS
           PERFORM TEST-STANDALONE-PIC-STRINGS
           PERFORM TEST-COBOL60-AND-USAGES
           PERFORM TEST-OCCURS-VALUES-AND-ERRORS
           IF WS-FAILS > 0
               DISPLAY "TCPIC FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph LEX-AND-SCAN-CLAUSES Lex WS-SNIP and run OP-CLAUSES.
       LEX-AND-SCAN-CLAUSES.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCPI01" TO CDD-CPI-TAG
           MOVE 10 TO CDD-CTK-DIALECT
           MOVE 2 TO CDD-CTK-FMT-REQ
           MOVE 8192 TO CDD-CTK-CAP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE WS-SNIP(1:WS-SLEN) TO CDD-BUF-DATA(1:WS-SLEN)
           MOVE WS-SLEN TO CDD-BUF-LEN
           CALL "CDDCLEX" USING CDD-BUF CDD-CTOK-STREAM CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-CPI-OP
           MOVE 1 TO CDD-CPI-TOK-START
           MOVE CDD-CTK-COUNT TO CDD-CPI-TOK-END
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL.
      * @paragraph TEST-ENVELOPE-GUARDS Test all 3 envelope tags.
       TEST-ENVELOPE-GUARDS.
           MOVE "BADBUF00" TO CDD-BUF-TAG
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "CDDCPI01" TO CDD-CPI-TAG
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "BADCTK00" TO CDD-CTK-TAG
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCTK01" TO CDD-CTK-TAG
           MOVE "BADCPI00" TO CDD-CPI-TAG
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDCPI01" TO CDD-CPI-TAG.
      * @paragraph TEST-STANDALONE-PIC-STRINGS Test OP-PIC-STR cases.
       TEST-STANDALONE-PIC-STRINGS.
           MOVE 1 TO CDD-CPI-OP
           MOVE 0 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 33 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "X(16)" TO CDD-CPI-PIC-IN
           MOVE 5 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-CPI-MAX-LEN NOT = 16
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "A(4)" TO CDD-CPI-PIC-IN
           MOVE 4 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "N(20)" TO CDD-CPI-PIC-IN
           MOVE 5 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "U(30)" TO CDD-CPI-PIC-IN
           MOVE 5 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "1" TO CDD-CPI-PIC-IN
           MOVE 1 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "s9(5)v99" TO CDD-CPI-PIC-IN
           MOVE 8 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "999PPP" TO CDD-CPI-PIC-IN
           MOVE 6 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "P(3)99" TO CDD-CPI-PIC-IN
           MOVE 6 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "+$,$$9.99B0/-" TO CDD-CPI-PIC-IN
           MOVE 13 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           MOVE "X(10" TO CDD-CPI-PIC-IN
           MOVE 4 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "X()" TO CDD-CPI-PIC-IN
           MOVE 3 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "X(0)" TO CDD-CPI-PIC-IN
           MOVE 4 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "@@@" TO CDD-CPI-PIC-IN
           MOVE 3 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "SV." TO CDD-CPI-PIC-IN
           MOVE 3 TO CDD-CPI-PIC-IN-LEN
           CALL "CDDCPIC" USING CDD-BUF CDD-CTOK-STREAM CDD-CPIC-INFO
               CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-COBOL60-AND-USAGES Test C60 verbose & 20 usages.
       TEST-COBOL60-AND-USAGES.
           MOVE "05 A SIZE IS 8 CLASS IS ALPHANUMERIC."
               TO WS-SNIP(1:37)
           MOVE 37 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-OK OR CDD-CPI-MAX-LEN NOT = 8
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 B SIZE 6 CLASS NUMERIC POINT LOCATION IS LEFT 2"
               TO WS-SNIP(1:50)
           MOVE " SIGNED." TO WS-SNIP(51:8)
           MOVE 58 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-OK OR CDD-CPI-SCALE NOT = 2
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 C SIZE 4 CLASS NUMERIC POINT RIGHT X."
               TO WS-SNIP(1:40)
           MOVE 40 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 D SIZE X CLASS OTHER POINT NOPE."
               TO WS-SNIP(1:35)
           MOVE 35 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 E SIZE" TO WS-SNIP(1:9)
           MOVE 9 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 F SIZE 4 CLASS NUMERIC POINT LEFT"
               TO WS-SNIP(1:36)
           MOVE 36 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           PERFORM TEST-ALL-USAGE-VARIANTS.
      * @paragraph TEST-ALL-USAGE-VARIANTS Exercise every USAGE arm.
       TEST-ALL-USAGE-VARIANTS.
           MOVE "05 U1 PIC X ANY LENGTH USAGE IS DISPLAY SYNC JUST."
               TO WS-SNIP(1:50)
           MOVE 50 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U2 PIC X DYNAMIC JUST SYNC." TO WS-SNIP(1:30)
           MOVE 30 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U3 ANY OTHER DYNAMIC." TO WS-SNIP(1:24)
           MOVE 24 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U4 USAGE COMP." TO WS-SNIP(1:17)
           MOVE 17 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U5 COMP-3." TO WS-SNIP(1:13)
           MOVE 13 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U6 COMP-1." TO WS-SNIP(1:13)
           MOVE 13 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U7 COMP-2." TO WS-SNIP(1:13)
           MOVE 13 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U8 BINARY-CHAR." TO WS-SNIP(1:18)
           MOVE 18 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 U9 BINARY-SHORT." TO WS-SNIP(1:19)
           MOVE 19 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UA BINARY-LONG." TO WS-SNIP(1:18)
           MOVE 18 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UB BINARY-DOUBLE." TO WS-SNIP(1:20)
           MOVE 20 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UC FLOAT-DECIMAL-16." TO WS-SNIP(1:23)
           MOVE 23 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UD FLOAT-DECIMAL-34." TO WS-SNIP(1:23)
           MOVE 23 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UE BIT." TO WS-SNIP(1:10)
           MOVE 10 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UF NATIONAL." TO WS-SNIP(1:15)
           MOVE 15 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UG UTF-8." TO WS-SNIP(1:12)
           MOVE 12 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UH USAGE TYPE CUST-T." TO WS-SNIP(1:24)
           MOVE 24 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UI USAGE INDEX." TO WS-SNIP(1:18)
           MOVE 18 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UJ USAGE" TO WS-SNIP(1:11)
           MOVE 11 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 UK USAGE TYPE" TO WS-SNIP(1:16)
           MOVE 16 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES.
      * @paragraph TEST-OCCURS-VALUES-AND-ERRORS Test OCCURS & VALUEs.
       TEST-OCCURS-VALUES-AND-ERRORS.
           MOVE "01 T-REC IS TYPEDEF STRONG." TO WS-SNIP(1:27)
           MOVE 27 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF CDD-CPI-IS-TYPEDEF NOT = 1
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 R1 REDEFINES A PIC IS X(4) VALUE IS 'AB'."
               TO WS-SNIP(1:44)
           MOVE 44 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 R2 REDEFINES" TO WS-SNIP(1:15)
           MOVE 15 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE '05 V1 PIC X VALUE "".' TO WS-SNIP(1:21)
           MOVE 21 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V2 PIC 9 VALUE ZERO." TO WS-SNIP(1:23)
           MOVE 23 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V3 PIC X VALUE SPACES." TO WS-SNIP(1:25)
           MOVE 25 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V4 PIC 1 VALUE TRUE." TO WS-SNIP(1:23)
           MOVE 23 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V5 PIC 1 VALUE FALSE." TO WS-SNIP(1:24)
           MOVE 24 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V6 PIC 99 VALUE 42." TO WS-SNIP(1:22)
           MOVE 22 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V7 VALUE." TO WS-SNIP(1:12)
           MOVE 12 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 V8 PIC X(4) VALUE '12345678901234567890123456789012"
               TO WS-SNIP(1:54)
           MOVE "34567890123456789012345678901234567890'."
               TO WS-SNIP(55:40)
           MOVE 94 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 A1 OCCURS 5 TIMES INDEXED BY IDX-1 PIC X."
               TO WS-SNIP(1:44)
           MOVE 44 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 A2 OCCURS 1 TO 10 DEPENDING ON CNT-1 PIC 9."
               TO WS-SNIP(1:46)
           MOVE 46 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 A3 OCCURS DYNAMIC CAPACITY IN CAP-1 INITIALIZED."
               TO WS-SNIP(1:51)
           MOVE 51 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 A4 OCCURS DYNAMIC BY." TO WS-SNIP(1:24)
           MOVE 24 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           MOVE "05 E1 PIC." TO WS-SNIP(1:10)
           MOVE 10 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 E2 PIC XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX."
               TO WS-SNIP(1:44)
           MOVE 44 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-ERR-COB-INVALID-PIC
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 E3 OCCURS" TO WS-SNIP(1:12)
           MOVE 12 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-ERR-COB-INVALID-OCCURS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 E4 OCCURS BAD." TO WS-SNIP(1:17)
           MOVE 17 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-ERR-COB-INVALID-OCCURS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 E5 OCCURS 1 TO" TO WS-SNIP(1:17)
           MOVE 17 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-ERR-COB-INVALID-OCCURS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 E6 OCCURS 5 TO 2." TO WS-SNIP(1:20)
           MOVE 20 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES
           IF NOT CDD-ERR-COB-INVALID-OCCURS
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "05 LONG12345678901234567890123456789012345678901234567"
               TO WS-SNIP(1:54)
           MOVE "890123456789012345 USAGE LONG1234567890123456789012345"
               TO WS-SNIP(55:54)
           MOVE "6789012345678901234567890." TO WS-SNIP(109:26)
           MOVE 134 TO WS-SLEN
           PERFORM LEX-AND-SCAN-CLAUSES.
