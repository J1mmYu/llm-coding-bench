       IDENTIFICATION DIVISION.
       PROGRAM-ID. RLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-LINE         PIC X(1000).
       01  INPUT-LENGTH       PIC 9999 VALUE 0.
       01  I                  PIC 9999 VALUE 1.
       01  J                  PIC 9999 VALUE 1.
       01  CURRENT-CHAR       PIC X.
       01  COUNT              PIC 9999 VALUE 0.
       01  COUNT-STR          PIC Z(3)9.
       01  OUTPUT-LINE        PIC X(4000).
       01  OUTPUT-POS         PIC 9999 VALUE 1.
       
       PROCEDURE DIVISION.
           ACCEPT INPUT-LINE.
           
           INSPECT INPUT-LINE TALLYING INPUT-LENGTH 
               FOR CHARACTERS BEFORE INITIAL SPACE.
           
           IF INPUT-LENGTH = 0
               INSPECT INPUT-LINE TALLYING INPUT-LENGTH
                   FOR ALL X'00' TO X'FF'
               IF INPUT-LENGTH = 0
                   MOVE 1000 TO INPUT-LENGTH
               END-IF
           END-IF.
           
           MOVE INPUT-LINE(1:1) TO CURRENT-CHAR.
           MOVE 1 TO COUNT.
           MOVE 2 TO I.
           
           PERFORM UNTIL I > INPUT-LENGTH
               IF INPUT-LINE(I:1) = CURRENT-CHAR
                   ADD 1 TO COUNT
               ELSE
                   MOVE COUNT TO COUNT-STR
                   STRING CURRENT-CHAR DELIMITED BY SIZE
                          FUNCTION TRIM(COUNT-STR) DELIMITED BY SIZE
                          INTO OUTPUT-LINE
                          WITH POINTER OUTPUT-POS
                   END-STRING
                   MOVE INPUT-LINE(I:1) TO CURRENT-CHAR
                   MOVE 1 TO COUNT
               END-IF
               ADD 1 TO I
           END-PERFORM.
           
           MOVE COUNT TO COUNT-STR.
           STRING CURRENT-CHAR DELIMITED BY SIZE
                  FUNCTION TRIM(COUNT-STR) DELIMITED BY SIZE
                  INTO OUTPUT-LINE
                  WITH POINTER OUTPUT-POS
           END-STRING.
           
           SUBTRACT 1 FROM OUTPUT-POS.
           DISPLAY OUTPUT-LINE(1:OUTPUT-POS).
           
           STOP RUN.
