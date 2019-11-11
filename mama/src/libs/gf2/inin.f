
C======================================================================

      SUBROUTINE ININ(CIN,NC,IOUT1,IOUT2,IOUT3,*)

C           free format integer input routine....
C           up to three integers (IOUT1, IOUT2, IOUT3) decoded from CIN....
C           input fields separated by commas or spaces....
C           CIN = input character string          (input, up to char*80)....
C           NC  = no. of valid characters in CIN  (input, integer)....
C           IOUT1, IOUT2, IOUT3 = decoded numbers (output, integer)....
C           RETURN 1: invalid character in CIN....
C                    D.C. Radford   July 1985

      CHARACTER*(*) CIN
      INTEGER       NC, IOUT1, IOUT2, IOUT3, IOUT(3)
      CHARACTER*81  IN


      IN(1:NC)=CIN(1:NC)
      DO 10 N=1,3
         IOUT(N)=0
10    CONTINUE
      IF (NC.LT.1) GO TO 150
      ILO=1
      DO 100 N=1,3
15       IF (IN(ILO:ILO).EQ.' ') THEN
            ILO=ILO+1
            IF (ILO.GT.NC) GO TO 150
            GO TO 15
         ENDIF
         DO 20 I=ILO,NC
20          IF (IN(I:I).EQ.' ' .OR. IN(I:I).EQ.',') GO TO 30
         I=NC+1
30       IN(I:I)=','
         IHI=I-1
         IF (IHI.LT.ILO) GO TO 60
         DO 40 I=ILO,IHI
            IF ((IN(I:I).LT.'0' .OR. IN(I:I).GT.'9')
     +                  .AND. IN(I:I).NE.'-') GO TO 200
40       CONTINUE
         READ(IN(ILO:IHI),*)IOUT(N)
c50       FORMAT(I)
60       ILO=IHI+2
         IF (ILO.GT.NC) GO TO 150
100   CONTINUE
150   IOUT1=IOUT(1)
      IOUT2=IOUT(2)
      IOUT3=IOUT(3)
      RETURN
200   IOUT1=IOUT(1)
      IOUT2=IOUT(2)
      IOUT3=IOUT(3)
      RETURN 1
      END
