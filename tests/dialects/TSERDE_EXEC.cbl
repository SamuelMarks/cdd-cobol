      * @file TSERDE_EXEC.cbl
      * @brief Live compile-and-execute round-trip test for CDDGENS.
      * @module TSRDEX
      * @details Generates a COBOL-85 UDT and its TO-JSON / FROM-JSON
      *          subprograms from a nested JSON Schema, verifies 100%
      *          doc coverage via MCDDDOCC, compiles the generated
      *          COBOL code with cobc, and executes it to prove exact
      *          runtime serialization and deserialization fidelity!
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSRDEX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "cdd-opts.cpy".
       COPY "cdd-diff.cpy".
       COPY "cdd-ir.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-argv.cpy".
      * @record WS-OUT-BUF Generated COBOL source buffer.
       01  WS-OUT-BUF.
      * @field WS-OBF-TAG Magic tag ('CDDBUF01').
           05  WS-OBF-TAG                PIC X(8) VALUE "CDDBUF01".
      * @field WS-OBF-LEN Active byte length.
           05  WS-OBF-LEN                PIC 9(8) COMP VALUE 0.
      * @field WS-OBF-CAP Buffer capacity.
           05  WS-OBF-CAP                PIC 9(8) COMP VALUE 1048576.
      * @field WS-OBF-NL Newline mode.
           05  WS-OBF-NL                 PIC 9(2) COMP VALUE 1.
      * @field WS-OBF-DATA Generated COBOL bytes.
           05  WS-OBF-DATA               PIC X(1048576) VALUE SPACES.
       COPY "cdd-err.cpy".
      * @record WS-SYS-CMD Shell command passed to SYSTEM call.
       01  WS-SYS-CMD                    PIC X(256) VALUE SPACES.
      * @record WS-BYTE-CONV LF/NUL byte helper.
       01  WS-BYTE-CONV.
      * @field WS-U16 16-bit integer.
           05  WS-U16                    PIC 9(4) COMP VALUE 0.
      * @field WS-U16-B Byte overlay.
           05  WS-U16-B REDEFINES WS-U16.
      * @field WS-B-HI High byte.
               10  WS-B-HI               PIC X(1).
      * @field WS-B-LO Low byte.
               10  WS-B-LO               PIC X(1).
      * @record WS-STATE Test state and return code.
       01  WS-STATE.
      * @field WS-FAILS Failed assertion count.
           05  WS-FAILS                  PIC 9(4) COMP VALUE 0.
      * @field WS-LF LF character (10).
           05  WS-LF                     PIC X(1) VALUE SPACE.
      * @field WS-NUL NUL byte (0) terminating SYSTEM command.
           05  WS-NUL                    PIC X(1) VALUE SPACE.
      * @field WS-PTR String builder pointer.
           05  WS-PTR                    PIC 9(8) COMP VALUE 1.
      * @field WS-RC Signed status from SYSTEM call.
           05  WS-RC                     PIC S9(9) COMP VALUE 0.
      * @subprogram TSRDEX Compile & execute generated serde routines.
      * @return Exit status 0 on pass; 1 on failure.
      * @errors Reports failure to SYSERR.
       PROCEDURE DIVISION.
      * @paragraph MAIN-ENTRY Generate, doc-check, compile, & run serde.
       MAIN-ENTRY.
           MOVE 10 TO WS-U16
           MOVE WS-B-LO TO WS-LF
           MOVE 0 TO WS-U16
           MOVE WS-B-LO TO WS-NUL
           PERFORM GENERATE-UDT-AND-SERDE
           PERFORM DOC-CHECK-GENERATED-SERDE
           PERFORM WRITE-AND-RUN-DRIVER
           IF WS-FAILS > 0
               DISPLAY "TSERDE_EXEC FAILED" UPON SYSERR
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF
           STOP RUN RETURNING 0.
      * @paragraph GENERATE-UDT-AND-SERDE Emit ORD-REC & serde routines.
       GENERATE-UDT-AND-SERDE.
           MOVE "CDDBUF01" TO CDD-BUF-TAG
           MOVE "CDDBUF01" TO WS-OBF-TAG
           MOVE "CDDOPT01" TO CDD-OPT-TAG
           MOVE "CDDMPR01" TO CDD-MPR-TAG
           MOVE "CDDIR001" TO CDD-IR-TAG
           MOVE "CDDPTH01" TO CDD-PATH-TAG
           MOVE 256 TO CDD-IR-FLD-CAP
           MOVE 256 TO CDD-IR-ENUM-CAP
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING '{"title":"Ord","properties":{' DELIMITED BY SIZE
                  '"ordId":{"type":"integer"},' DELIMITED BY SIZE
                  '"custName":{"type":"string","maxLength":16},'
                      DELIMITED BY SIZE
                  '"ship":{"type":"object","properties":{'
                      DELIMITED BY SIZE
                  '"city":{"type":"string","maxLength":12}}}}}'
                      DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE 7 TO CDD-MPR-DIALECT
           MOVE 1 TO CDD-MPR-UDT-KIND
           MOVE 1 TO CDD-MPR-JSON-MODE
           MOVE 7 TO CDD-MPR-NAME-LEN
           MOVE "ORD-REC" TO CDD-MPR-TARGET-NAME
           CALL "CDDJ2IR" USING CDD-IR-MAP-REQ CDD-BUF CDD-IR CDD-ERROR
           END-CALL
           MOVE 7 TO CDD-OPT-DIALECT
           MOVE 1 TO CDD-OPT-FORMAT
           MOVE 1 TO CDD-OPT-UDT-KIND
           MOVE 2 TO CDD-OPT-SERDE-KIND
           CALL "CDDGENU" USING CDD-SDK-OPTS CDD-IR WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE 2 TO CDD-PATH-OP
           MOVE "build/exec_ord.cpy" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ WS-OUT-BUF CDD-ERROR
           END-CALL
           CALL "CDDGENS" USING CDD-SDK-OPTS CDD-IR WS-OUT-BUF
               CDD-ERROR
           END-CALL
           MOVE "build/exec_srd.cbl" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           CALL "CDDIO" USING CDD-PATH-REQ WS-OUT-BUF CDD-ERROR
           END-CALL.
      * @paragraph DOC-CHECK-GENERATED-SERDE Verify 100% doc coverage.
       DOC-CHECK-GENERATED-SERDE.
           MOVE "build/exec_srd.lst" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE "build/exec_srd.cbl" TO CDD-BUF-DATA(1:18)
           MOVE 18 TO CDD-BUF-LEN
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE "CDDAGV01" TO CDD-AGV-TAG
           MOVE 1 TO CDD-AGV-MODE
           MOVE 1 TO CDD-AGV-QUIET
           MOVE 1 TO CDD-AGV-COUNT
           MOVE "build/exec_srd.lst" TO CDD-AGV-ITEM(1)
           CALL "MCDDDOCC"
           END-CALL
           IF CDD-AGV-EXIT-CODE NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF
           MOVE 0 TO CDD-AGV-MODE
           MOVE 0 TO CDD-AGV-QUIET.
      * @paragraph WRITE-AND-RUN-DRIVER Compile & execute round-trip.
       WRITE-AND-RUN-DRIVER.
           MOVE SPACES TO CDD-BUF-DATA
           MOVE 1 TO WS-PTR
           STRING "       IDENTIFICATION DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROGRAM-ID. EXDRV." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       DATA DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       WORKING-STORAGE SECTION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '       COPY "exec_ord.cpy".' DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  JBUF." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           05  JLEN  PIC 9(6) COMP VALUE 0."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           05  JDATA PIC X(4096) VALUE SPACES."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       01  JST PIC 9(4) COMP VALUE 0."
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       PROCEDURE DIVISION." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "       MAIN-P." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           MOVE 4207 TO ORD-ID" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '           MOVE "ALICE" TO CUST-NAME'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '           MOVE "BOSTON" TO CITY'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '           CALL "ORD-TO-JSON" USING ORD-REC'
                      DELIMITED BY SIZE
                  " JBUF JST" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           END-CALL" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           INITIALIZE ORD-REC" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '           CALL "ORD-FROM-JSON" USING ORD-REC'
                      DELIMITED BY SIZE
                  " JBUF JST" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           END-CALL" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           IF ORD-ID NOT = 4207" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '               OR CUST-NAME NOT = "ALICE"'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  '               OR CITY NOT = "BOSTON"'
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "               STOP RUN RETURNING 1"
                      DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           END-IF" DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  "           STOP RUN RETURNING 0." DELIMITED BY SIZE
                  WS-LF DELIMITED BY SIZE
                  INTO CDD-BUF-DATA WITH POINTER WS-PTR
           END-STRING
           SUBTRACT 1 FROM WS-PTR GIVING CDD-BUF-LEN
           END-SUBTRACT
           MOVE "build/exec_drv.cbl" TO CDD-PATH-VAL
           MOVE 18 TO CDD-PATH-LEN
           MOVE 2 TO CDD-PATH-OP
           CALL "CDDIO" USING CDD-PATH-REQ CDD-BUF CDD-ERROR
           END-CALL
           MOVE SPACES TO WS-SYS-CMD
           MOVE 1 TO WS-PTR
           STRING "cobc -x --fixed -std=cobol85 " DELIMITED BY SIZE
                  "-fstop-identifier-statement=ok " DELIMITED BY SIZE
                  "-freserved-words=default -I build "
                      DELIMITED BY SIZE
                  "build/exec_drv.cbl build/exec_srd.cbl "
                      DELIMITED BY SIZE
                  "-o build/exec_drv && ./build/exec_drv"
                      DELIMITED BY SIZE
                  WS-NUL DELIMITED BY SIZE
                  INTO WS-SYS-CMD WITH POINTER WS-PTR
           END-STRING
           CALL "SYSTEM" USING WS-SYS-CMD GIVING WS-RC
           END-CALL
           IF WS-RC NOT = 0
               ADD 1 TO WS-FAILS
               END-ADD
           END-IF.
