       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 99 VALUE 0.
       01 WS-CATEGORY PIC X(15).
       01 WS-DICE PIC 9(5).
       01 WS-CNT1 PIC 9(5) VALUE 0.
       01 WS-CNT2 PIC 9(5) VALUE 0.
       01 WS-CNT3 PIC 9(5) VALUE 0.
       01 WS-CNT4 PIC 9(5) VALUE 0.
       01 WS-CNT5 PIC 9(5) VALUE 0.
       01 WS-CNT6 PIC 9(5) VALUE 0.
          PROCEDURE DIVISION.
          YACHT.
            INSPECT WS-DICE TALLYING WS-CNT1 FOR ALL '1'.
            INSPECT WS-DICE TALLYING WS-CNT2 FOR ALL '2'.
            INSPECT WS-DICE TALLYING WS-CNT3 FOR ALL '3'.
            INSPECT WS-DICE TALLYING WS-CNT4 FOR ALL '4'.
            INSPECT WS-DICE TALLYING WS-CNT5 FOR ALL '5'.
            INSPECT WS-DICE TALLYING WS-CNT6 FOR ALL '6'.

            EVALUATE WS-CATEGORY
               WHEN "yacht"
                  IF WS-CNT1=5 OR WS-CNT2=5 OR WS-CNT3=5 OR WS-CNT4=5 OR WS-CNT5=5 OR WS-CNT6=5 THEN
                     MOVE 50 TO WS-RESULT
                  ELSE
                     MOVE 0 TO WS-RESULT
                  END-IF
               WHEN "ones"
                  MOVE WS-CNT1 TO WS-RESULT
               WHEN "twos"
                  COMPUTE WS-CNT2 = (WS-CNT2 * 2)
                  MOVE WS-CNT2 TO WS-RESULT
               WHEN "threes"
                  COMPUTE WS-CNT3 = (WS-CNT3 * 3)
                  MOVE WS-CNT3 TO WS-RESULT
               WHEN "fours"
                  COMPUTE WS-CNT4 = (WS-CNT4 * 4)
                  MOVE WS-CNT4 TO WS-RESULT
               WHEN "fives"
                  COMPUTE WS-CNT5 = (WS-CNT5 * 5)
                  MOVE WS-CNT5 TO WS-RESULT
               WHEN "sixes"
                  COMPUTE WS-CNT6 = (WS-CNT6 * 6)
                  MOVE WS-CNT6 TO WS-RESULT
               WHEN "full house"
                  IF (WS-CNT1=3 OR WS-CNT2=3 OR WS-CNT3=3 OR WS-CNT4=3 OR WS-CNT5=3 OR WS-CNT6=3) THEN
                     IF (WS-CNT1=2 OR WS-CNT2=2 OR WS-CNT3=2 OR WS-CNT4=2 OR WS-CNT5=2 OR WS-CNT6=2) THEN
                        COMPUTE WS-RESULT = (WS-CNT1 * 1) + (WS-CNT2 * 2) + (WS-CNT3 * 3) + (WS-CNT4 * 4) + (WS-CNT5 * 5) + (WS-CNT6 * 6)
                     ELSE
                        MOVE 0 TO WS-RESULT
                     END-IF
                  ELSE
                     MOVE 0 TO WS-RESULT
                  END-IF
               WHEN "four of a kind"
                  MOVE 0 TO WS-RESULT
                  IF WS-CNT1>3 THEN
                     MOVE 4 TO WS-RESULT
                  END-IF
                  IF WS-CNT2>3 THEN
                     MOVE 8 TO WS-RESULT
                  END-IF
                  IF WS-CNT3>3 THEN
                     MOVE 12 TO WS-RESULT
                  END-IF
                  IF WS-CNT4>3 THEN
                     MOVE 16 TO WS-RESULT
                  END-IF
                  IF WS-CNT5>3 THEN
                     MOVE 20 TO WS-RESULT
                  END-IF
                  IF WS-CNT6>3 THEN
                     MOVE 24 TO WS-RESULT
                  END-IF
               WHEN "little straight"
                  IF (WS-CNT1=1 AND WS-CNT2=1 AND WS-CNT3=1 AND WS-CNT4=1 AND WS-CNT5=1) THEN
                     MOVE 30 TO WS-RESULT
                  ELSE
                     MOVE 0 TO WS-RESULT
                  END-IF
               WHEN "big straight"
                  IF (WS-CNT6=1 AND WS-CNT2=1 AND WS-CNT3=1 AND WS-CNT4=1 AND WS-CNT5=1) THEN
                     MOVE 30 TO WS-RESULT
                  ELSE
                     MOVE 0 TO WS-RESULT
                  END-IF
               WHEN "choice"
                  COMPUTE WS-RESULT = (WS-CNT1 * 1) + (WS-CNT2 * 2) + (WS-CNT3 * 3) + (WS-CNT4 * 4) + (WS-CNT5 * 5) + (WS-CNT6 * 6)
      
            END-EVALUATE.

            MOVE 0 to WS-CNT1.
            MOVE 0 to WS-CNT2.
            MOVE 0 to WS-CNT3.
            MOVE 0 to WS-CNT4.
            MOVE 0 to WS-CNT5.
            MOVE 0 to WS-CNT6.
      * STOP RUN.
