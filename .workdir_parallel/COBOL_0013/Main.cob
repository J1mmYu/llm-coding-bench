       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIGINT-ADD.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM1            PIC X(501).
       01  NUM2            PIC X(501).
       01  RESULT          PIC X(502).
       01  LEN1            PIC 9(4).
       01  LEN2            PIC 9(4).
       01  MAXLEN          PIC 9(4).
       01  I               PIC 9(4).
       01  J               PIC 9(4).
       01  K               PIC 9(4).
       01  CARRY           PIC 9(1) VALUE 0.
       01  DIGIT1          PIC 9(1).
       01  DIGIT2          PIC 9(1).
       01  SUM-DIGIT       PIC 9(2).
       01  START-POS       PIC 9(4).
       01  END-FLAG        PIC 9(1) VALUE 0.
       
       PROCEDURE DIVISION.
           ACCEPT NUM1.
           ACCEPT NUM2.
           
           MOVE FUNCTION LENGTH(FUNCTION TRIM(NUM1)) TO LEN1.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(NUM2)) TO LEN2.
           
           IF LEN1 > LEN2
               MOVE LEN1 TO MAXLEN
           ELSE
               MOVE LEN2 TO MAXLEN
           END-IF.
           
           MOVE SPACES TO RESULT.
           MOVE 0 TO CARRY.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAXLEN + 1
               COMPUTE J = LEN1 - I + 1
               COMPUTE K = LEN2 - I + 1
               
               MOVE 0 TO DIGIT1
               MOVE 0 TO DIGIT2
               
               IF J > 0
                   MOVE NUM1(J:1) TO DIGIT1
               END-IF
               
               IF K > 0
                   MOVE NUM2(K:1) TO DIGIT2
               END-IF
               
               COMPUTE SUM-DIGIT = DIGIT1 + DIGIT2 + CARRY
               
               IF SUM-DIGIT > 9
                   COMPUTE CARRY = 1
                   COMPUTE SUM-DIGIT = SUM-DIGIT - 10
               ELSE
                   MOVE 0 TO CARRY
               END-IF
               
               COMPUTE J = MAXLEN - I + 2
               MOVE SUM-DIGIT TO RESULT(J:1)
           END-PERFORM.
           
           MOVE 1 TO START-POS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAXLEN + 1
               IF RESULT(I:1) NOT = '0' AND RESULT(I:1) NOT = ' '
                   MOVE I TO START-POS
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           DISPLAY RESULT(START-POS:MAXLEN - START-POS + 2).
           
           STOP RUN.
