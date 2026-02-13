       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSABND.
      *
      * Test HANDLE ABEND invocation: register handler, trigger
      * ABEND, verify handler paragraph executes.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STATUS              PIC X(20) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'ABEND TEST STARTING'.
           EXEC CICS HANDLE ABEND
                LABEL(ABEND-HANDLER)
           END-EXEC.
           DISPLAY 'HANDLER REGISTERED'.
           EXEC CICS ABEND
                ABCODE('TEST')
           END-EXEC.
           DISPLAY 'SHOULD NOT REACH HERE'.
           STOP RUN.
      *
       ABEND-HANDLER.
           DISPLAY 'ABEND HANDLER INVOKED'.
           MOVE 'HANDLED' TO WS-STATUS.
           DISPLAY 'STATUS: HANDLED'.
           EXEC CICS RETURN
                TRANSID('MENU')
           END-EXEC.
           STOP RUN.
