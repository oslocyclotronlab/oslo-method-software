
C=======================================================================

      SUBROUTINE LINEUP(NUP,NR)

C          NUP: no. of lines to go up
C          NR : no. of columns to move right

      INTEGER       NUP, NR

      CHARACTER*4   UP
      CHARACTER*5   RIGHT


      IF (NUP.GT.0) THEN
         JUP = NUP
         IF (JUP.GT.9) JUP = 9
         WRITE(UP,10) CHAR(27),JUP
10       FORMAT(A1,'[',I1.1,'A')
C                         esc[0A - move text cursor up....
         CALL QIO_PUT_TEXT(UP,4)
      ENDIF

      IF (NR.GT.0) THEN
         JR = NR
         IF (JR.GT.99) JR = 99
         WRITE(RIGHT,20) CHAR(27),JR
20       FORMAT(A1,'[',I2.2,'C')
C                         esc[00C - move text cursor right....
         CALL QIO_PUT_TEXT(RIGHT,5)
      ENDIF

      RETURN
      END
