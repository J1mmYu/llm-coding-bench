       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSVPARSER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CSV-LINE            PIC X(1000).
       01  LINE-LENGTH         PIC 9999 VALUE 0.
       01  CURRENT-POS         PIC 9999 VALUE 1.
       01  FIELD-COUNT         PIC 9999 VALUE 0.
       01  IN-QUOTES           PIC 9 VALUE 0.
       01  CURRENT-CHAR        PIC X.
       01  NEXT-CHAR           PIC X.
       01  FIELD-COUNT-DISP    PIC Z(4).
       
       PROCEDURE DIVISION.
           ACCEPT CSV-LINE.
           
           INSPECT CSV-LINE TALLYING LINE-LENGTH 
               FOR CHARACTERS BEFORE INITIAL X'00'.
           
           IF LINE-LENGTH = 0
               INSPECT CSV-LINE TALLYING LINE-LENGTH
                   FOR CHARACTERS
           END-IF.
           
           MOVE 1 TO FIELD-COUNT.
           MOVE 0 TO IN-QUOTES.
           MOVE 1 TO CURRENT-POS.
           
           PERFORM UNTIL CURRENT-POS > LINE-LENGTH
               MOVE CSV-LINE(CURRENT-POS:1) TO CURRENT-CHAR
               
               IF CURRENT-CHAR = '"'
                   IF CURRENT-POS < LINE-LENGTH
                       COMPUTE CURRENT-POS = CURRENT-POS + 1
                       MOVE CSV-LINE(CURRENT-POS:1) TO NEXT-CHAR
                       IF NEXT-CHAR = '"' AND IN-QUOTES = 1
                           COMPUTE CURRENT-POS = CURRENT-POS + 1
                       ELSE
                           COMPUTE CURRENT-POS = CURRENT-POS - 1
                           IF IN-QUOTES = 0
                               MOVE 1 TO IN-QUOTES
                           ELSE
                               MOVE 0 TO IN-QUOTES
                           END-IF
                           COMPUTE CURRENT-POS = CURRENT-POS + 1
                       END-IF
                   ELSE
                       IF IN-QUOTES = 0
                           MOVE 1 TO IN-QUOTES
                       ELSE
                           MOVE 0 TO IN-QUOTES
                       END-IF
                       COMPUTE CURRENT-POS = CURRENT-POS + 1
                   END-IF
               ELSE IF CURRENT-CHAR = ',' AND IN-QUOTES = 0
                   ADD 1 TO FIELD-COUNT
                   COMPUTE CURRENT-POS = CURRENT-POS + 1
               ELSE
                   COMPUTE CURRENT-POS = CURRENT-POS + 1
               END-IF
               END-IF
           END-PERFORM.
           
           MOVE FIELD-COUNT TO FIELD-COUNT-DISP.
           DISPLAY FIELD-COUNT-DISP.
           
           STOP RUN.
