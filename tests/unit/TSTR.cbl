      * @file TSTR.cbl
      * @brief Unit tests for CDDSTR string, JSON escape, and U32 ops.
      * @module TSTR
      * @details Exercises 100% of branches in CDDSTR including tag/len
      *          guards, RTRIM, UPPER, LOWER, JSON escape (all control
      *          bytes and overflow), JSON unescape (all escapes, UTF-8
      *          1/2/3/4-byte codepoints, surrogate errors), PARSE-U32,
      *          and FMT-U32.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-core.cpy".
       COPY "cdd-err.cpy".
      * @record WS-BYTE-CONV Helper to construct control characters.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer value.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STATE Test state and counters.
       01  WS-STATE.
      * @field WS-FAILS Number of failed assertions.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-I Loop counter.
           05  WS-I                      PIC 9(4) COMP VALUE 0.
      * @field WS-BSLASH Backslash character (ASCII 92).
           05  WS-BSLASH                 PIC X(1) VALUE SPACE.
      * @subprogram TSTR Verify 100% of CDDSTR lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Displays failure diagnostic to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDSTR unit test groups.
       MAIN-ENTRY.
           MOVE 92 TO WS-U16
           MOVE WS-B-LO TO WS-BSLASH
           PERFORM TEST-GUARDS-AND-CASE
           PERFORM TEST-JSON-ESCAPE
           PERFORM TEST-JSON-UNESCAPE-BASIC
           PERFORM TEST-JSON-UNESCAPE-UTF8
           PERFORM TEST-U32-CONV
           IF WS-FAILS > 0
               DISPLAY "TSTR FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS-AND-CASE Test tags, bounds, RTRIM, case.
       TEST-GUARDS-AND-CASE.
           MOVE "BADTAG00" TO CDD-SRQ-TAG
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDSRQ01" TO CDD-SRQ-TAG
           MOVE 1025 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-SRQ-OP
           MOVE SPACES TO CDD-SRQ-IN-TEXT
           MOVE 5 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-SRQ-OUT-LEN NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "abc  " TO CDD-SRQ-IN-TEXT(1:5)
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-SRQ-OUT-LEN NOT = 3
               OR CDD-SRQ-OUT-TEXT(1:3) NOT = "abc"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-SRQ-OP
           MOVE 0 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           MOVE 4 TO CDD-SRQ-IN-LEN
           MOVE "aBc1" TO CDD-SRQ-IN-TEXT(1:4)
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-SRQ-OUT-TEXT(1:4) NOT = "ABC1"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-SRQ-OP
           MOVE 0 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           MOVE 4 TO CDD-SRQ-IN-LEN
           MOVE "aBc1" TO CDD-SRQ-IN-TEXT(1:4)
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-SRQ-OUT-TEXT(1:4) NOT = "abc1"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-JSON-ESCAPE Test all control escapes & overflow.
       TEST-JSON-ESCAPE.
           MOVE 4 TO CDD-SRQ-OP
           MOVE SPACES TO CDD-SRQ-IN-TEXT
           MOVE '"' TO CDD-SRQ-IN-TEXT(1:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(2:1)
           MOVE 8 TO WS-U16
           MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(3:1)
           MOVE 12 TO WS-U16
           MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(4:1)
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(5:1)
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(6:1)
           MOVE 9 TO WS-U16
           MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(7:1)
           MOVE 1 TO WS-U16
           MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(8:1)
           MOVE "A" TO CDD-SRQ-IN-TEXT(9:1)
           MOVE 9 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-SRQ-OUT-LEN NOT = 21
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO WS-I
           PERFORM UNTIL WS-I > 600
               MOVE 1 TO WS-U16
               MOVE WS-B-LO TO CDD-SRQ-IN-TEXT(WS-I:1)
               ADD 1 TO WS-I
               END-ADD
           END-PERFORM
           MOVE 600 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-JSON-UNESCAPE-BASIC Test simple escapes & errs.
       TEST-JSON-UNESCAPE-BASIC.
           MOVE 5 TO CDD-SRQ-OP
           MOVE SPACES TO CDD-SRQ-IN-TEXT
           MOVE "A" TO CDD-SRQ-IN-TEXT(1:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(2:1)
           MOVE '"' TO CDD-SRQ-IN-TEXT(3:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(4:1)
           MOVE "/" TO CDD-SRQ-IN-TEXT(5:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(6:1)
           MOVE "b" TO CDD-SRQ-IN-TEXT(7:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(8:1)
           MOVE "f" TO CDD-SRQ-IN-TEXT(9:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(10:1)
           MOVE "n" TO CDD-SRQ-IN-TEXT(11:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(12:1)
           MOVE "r" TO CDD-SRQ-IN-TEXT(13:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(14:1)
           MOVE "t" TO CDD-SRQ-IN-TEXT(15:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(16:1)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(17:1)
           MOVE 17 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-SRQ-OUT-LEN NOT = 9
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE 1 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "q" TO CDD-SRQ-IN-TEXT(2:1)
           MOVE 2 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-ESCAPE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-JSON-UNESCAPE-UTF8 Test \uXXXX and surrogates.
       TEST-JSON-UNESCAPE-UTF8.
           MOVE 5 TO CDD-SRQ-OP
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "u0041" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(7:1)
           MOVE "u00e9" TO CDD-SRQ-IN-TEXT(8:5)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(13:1)
           MOVE "u20AC" TO CDD-SRQ-IN-TEXT(14:5)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(19:1)
           MOVE "uD83D" TO CDD-SRQ-IN-TEXT(20:5)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(25:1)
           MOVE "uDE00" TO CDD-SRQ-IN-TEXT(26:5)
           MOVE 30 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-SRQ-OUT-LEN NOT = 10
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "u12" TO CDD-SRQ-IN-TEXT(2:3)
           MOVE 4 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "u00GZ" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE 6 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "uDC00" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE 6 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "uD83D" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE 6 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "uD83Dabcdef" TO CDD-SRQ-IN-TEXT(2:11)
           MOVE 12 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "uD83D" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE "X" TO CDD-SRQ-IN-TEXT(7:1)
           MOVE "uDE00" TO CDD-SRQ-IN-TEXT(8:5)
           MOVE 12 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "uD83D" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(7:1)
           MOVE "u0041" TO CDD-SRQ-IN-TEXT(8:5)
           MOVE 12 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(1:1)
           MOVE "uD83D" TO CDD-SRQ-IN-TEXT(2:5)
           MOVE WS-BSLASH TO CDD-SRQ-IN-TEXT(7:1)
           MOVE "uZZZZ" TO CDD-SRQ-IN-TEXT(8:5)
           MOVE 12 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-UNICODE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-U32-CONV Test PARSE-U32 and FMT-U32 branches.
       TEST-U32-CONV.
           MOVE 6 TO CDD-SRQ-OP
           MOVE 0 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "12a" TO CDD-SRQ-IN-TEXT(1:3)
           MOVE 3 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "/" TO CDD-SRQ-IN-TEXT(1:1)
           MOVE 1 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "100000000" TO CDD-SRQ-IN-TEXT(1:9)
           MOVE 9 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-JSON-BAD-NUMBER
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "42099" TO CDD-SRQ-IN-TEXT(1:5)
           MOVE 5 TO CDD-SRQ-IN-LEN
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-SRQ-NUM-VAL NOT = 42099
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 7 TO CDD-SRQ-OP
           MOVE 0 TO CDD-SRQ-NUM-VAL
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-SRQ-OUT-LEN NOT = 1
               OR CDD-SRQ-OUT-TEXT(1:1) NOT = "0"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 42099 TO CDD-SRQ-NUM-VAL
           CALL "CDDSTR" USING CDD-STR-REQ CDD-ERROR
           END-CALL
           IF CDD-SRQ-OUT-LEN NOT = 5
               OR CDD-SRQ-OUT-TEXT(1:5) NOT = "42099"
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
