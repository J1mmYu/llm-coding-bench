       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUMN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N        PIC S9(18) COMP-5.
       01 S        PIC S9(18) COMP-5.
       01 OUTSTR   PIC Z(18)9.
       PROCEDURE DIVISION.
           ACCEPT N
           COMPUTE S = (N * (N + 1)) / 2
           MOVE S TO OUTSTR
           DISPLAY FUNCTION TRIM(OUTSTR)
           GOBACK.
       END PROGRAM SUMN.
