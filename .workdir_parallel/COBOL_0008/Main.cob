IDENTIFICATION DIVISION.
PROGRAM-ID. MERGE-INTERVALS.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  N                   PIC 9(5).
01  I                   PIC 9(5).
01  J                   PIC 9(5).
01  INTERVALS.
    05  INTERVAL OCCURS 1000 TIMES.
        10  L-VAL       PIC S9(9).
        10  R-VAL       PIC S9(9).
01  TEMP-L              PIC S9(9).
01  TEMP-R              PIC S9(9).
01  CURRENT-START       PIC S9(9).
01  CURRENT-END         PIC S9(9).
01  TOTAL-LENGTH        PIC S9(10) VALUE 0.
01  OUTPUT-STR          PIC -(10)9.

PROCEDURE DIVISION.
    ACCEPT N.
    
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
        ACCEPT L-VAL(I)
        ACCEPT R-VAL(I)
    END-PERFORM.
    
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > N - 1
        PERFORM VARYING J FROM I BY 1 UNTIL J > N
            IF L-VAL(I) > L-VAL(J) OR 
               (L-VAL(I) = L-VAL(J) AND R-VAL(I) > R-VAL(J))
                MOVE L-VAL(I) TO TEMP-L
                MOVE R-VAL(I) TO TEMP-R
                MOVE L-VAL(J) TO L-VAL(I)
                MOVE R-VAL(J) TO R-VAL(I)
                MOVE TEMP-L TO L-VAL(J)
                MOVE TEMP-R TO R-VAL(J)
            END-IF
        END-PERFORM
    END-PERFORM.
    
    IF N > 0
        MOVE L-VAL(1) TO CURRENT-START
        MOVE R-VAL(1) TO CURRENT-END
        PERFORM VARYING I FROM 2 BY 1 UNTIL I > N
            IF L-VAL(I) <= CURRENT-END
                IF R-VAL(I) > CURRENT-END
                    MOVE R-VAL(I) TO CURRENT-END
                END-IF
            ELSE
                ADD CURRENT-END TO TOTAL-LENGTH
                SUBTRACT CURRENT-START FROM TOTAL-LENGTH
                MOVE L-VAL(I) TO CURRENT-START
                MOVE R-VAL(I) TO CURRENT-END
            END-IF
        END-PERFORM
        ADD CURRENT-END TO TOTAL-LENGTH
        SUBTRACT CURRENT-START FROM TOTAL-LENGTH
    END-IF.
    
    MOVE TOTAL-LENGTH TO OUTPUT-STR.
    DISPLAY FUNCTION TRIM(OUTPUT-STR).
    
    STOP RUN.
