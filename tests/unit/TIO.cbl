      * @file TIO.cbl
      * @brief Unit test suite for CDDIO raw byte, atomic, & stdio I/O.
      * @module TIO
      * @details Exercises tag/length validation, stdin reading and
      *          capacity overflow, stdout emission, LF/CRLF detection,
      *          direct file write/read, empty files, handle write
      *          failure, directory read failure, and atomic temp-file
      *          swap success and failure.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
      * @record WS-NL-CONV Helper for CR/LF byte generation.
       01  WS-NL-CONV.
      * @field WS-U16 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STATE Test state and assertion failure counter.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-CR Carriage return byte (13).
           05  WS-CR                     PIC X(1) VALUE SPACE.
      * @field WS-LF Line feed byte (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @subprogram TIO Verify 100% of CDDIO lines and branches.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failures to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Run all CDDIO test groups.
       MAIN-ENTRY.
           MOVE 13 TO WS-U16
           MOVE WS-B-LO TO WS-CR
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           PERFORM TEST-GUARDS
           PERFORM TEST-STDIO
           PERFORM TEST-FILES-AND-CRLF
           PERFORM TEST-ATOMIC-AND-ERRORS
           IF WS-FAILS > 0
               DISPLAY "TIO FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph TEST-GUARDS Verify tag and path-length checks.
       TEST-GUARDS.
           MOVE "BADPTH00" TO CDD-PATH-TAG
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE "BADBUF00" TO CDD-BUF-TAG
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-TYPE-TAG-MISMATCH
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE 0 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 513 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-BOUNDS-OUT-OF-RANGE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-STDIO Test '-' stdin overflow/read and stdout.
       TEST-STDIO.
           MOVE 1 TO CDD-PATH-OP
           MOVE "-" TO CDD-PATH-VAL
           MOVE 1 TO CDD-PATH-LEN
           MOVE 4 TO CDD-BUF-CAP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1048576 TO CDD-BUF-CAP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-PATH-OP
           MOVE 0 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "ok" TO CDD-BUF-DATA(1:2)
           MOVE WS-LF TO CDD-BUF-DATA(3:1)
           MOVE 3 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-FILES-AND-CRLF Test empty, LF, CRLF, cap check.
       TEST-FILES-AND-CRLF.
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/tio_empty.dat" TO CDD-PATH-VAL
           MOVE 19 TO CDD-PATH-LEN
           MOVE 0 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1 TO CDD-PATH-OP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR CDD-BUF-LEN NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/tio_crlf.dat" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE "AB" TO CDD-BUF-DATA(1:2)
           MOVE WS-CR TO CDD-BUF-DATA(3:1)
           MOVE WS-LF TO CDD-BUF-DATA(4:1)
           MOVE "CD" TO CDD-BUF-DATA(5:2)
           MOVE 6 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE 1 TO CDD-PATH-OP
           MOVE 3 TO CDD-BUF-CAP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-GEN-BUF-OVERFLOW
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 1048576 TO CDD-BUF-CAP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK OR NOT CDD-BUF-NL-CRLF OR CDD-BUF-LEN NOT = 6
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
      * @paragraph TEST-ATOMIC-AND-ERRORS Test open/read/write/rename.
       TEST-ATOMIC-AND-ERRORS.
           MOVE 1 TO CDD-PATH-OP
           MOVE "build/does_not_exist.dat" TO CDD-PATH-VAL
           MOVE 24 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IO-OPEN-READ
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build" TO CDD-PATH-VAL
           MOVE 5 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IO-READ-FAILURE AND NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 5 TO CDD-PATH-OP
           MOVE SPACES TO CDD-PATH-HNDL
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/no_dir/out.dat" TO CDD-PATH-VAL
           MOVE 20 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IO-OPEN-WRITE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 4 TO CDD-PATH-OP
           MOVE SPACES TO CDD-PATH-HNDL
           MOVE 0 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE 3 TO CDD-BUF-LEN
           MOVE "XYZ" TO CDD-BUF-DATA(1:3)
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IO-WRITE-FAILURE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 3 TO CDD-PATH-OP
           MOVE "build/no_dir/atom.dat" TO CDD-PATH-VAL
           MOVE 21 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IO-OPEN-WRITE
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build/tio_atom.dat" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-OK
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE "build" TO CDD-PATH-VAL
           MOVE 5 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           IF NOT CDD-ERR-IO-ATOMIC-RENAME
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
