       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSSIGN.
      *
      * Test program for CICS sign-on flow.
      * Issues SEND MAP, then RETURN TRANSID to wait for input.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05  WS-STATE           PIC X VALUE 'I'.
       01  WS-RESPONSE            PIC S9(8) COMP VALUE 0.
       01  COSGN0AI.
           05  FILLER             PIC X(12).
           05  USRIDIL             PIC S9(4) COMP.
           05  USRIDIF             PIC X.
           05  USRIDIA             PIC X.
           05  USRIDII             PIC X(8).
           05  PASSWIL             PIC S9(4) COMP.
           05  PASSWIF             PIC X.
           05  PASSWIA             PIC X.
           05  PASSWII             PIC X(8).
      *
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'SIGNON PROGRAM STARTING'.
      *
      * Check if this is a fresh start or return from input
           IF EIBCALEN = 0
               DISPLAY 'INITIAL START - SENDING MAP'
               EXEC CICS SEND
                    MAP('COSGN0A')
                    MAPSET('COSGN00')
                    ERASE
               END-EXEC
               DISPLAY 'MAP SENT - RETURNING WITH TRANSID'
               EXEC CICS RETURN
                    TRANSID('SIGN')
                    COMMAREA(WS-COMMAREA)
                    LENGTH(1)
               END-EXEC
           ELSE
               DISPLAY 'RETURNED FROM INPUT'
               EXEC CICS RECEIVE
                    MAP('COSGN0A')
                    MAPSET('COSGN00')
                    INTO(COSGN0AI)
               END-EXEC
               DISPLAY 'USER ID: ' USRIDII
               DISPLAY 'PASSWORD: ' PASSWII
               DISPLAY 'SIGNON COMPLETE'
               EXEC CICS RETURN
               END-EXEC
           END-IF.
           STOP RUN.
