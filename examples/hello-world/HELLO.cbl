       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. ZOS-CLONE.
      *
      * Simple "Hello, World!" program to demonstrate zOS-clone.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LINUX.
       OBJECT-COMPUTER. LINUX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGE         PIC X(20) VALUE "Hello, World!".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "================================".
           DISPLAY "     zOS-clone Hello World".
           DISPLAY "================================".
           DISPLAY WS-MESSAGE.
           DISPLAY "Goodbye!".
           STOP RUN.
