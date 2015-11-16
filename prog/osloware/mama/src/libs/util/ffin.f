
C======================================================================

      SUBROUTINE FFIN(CIN,NC,OUT1,OUT2,OUT3,*)

C           free format floating point input routine....
C           up to three real numbers (OUT1, OUT2, OUT3) decoded from CIN....
C           input fields separated by commas or spaces....
C           CIN = input character string          (input, up to char*80)....
C           NC  = no. of valid characters in CIN  (input, integer)....
C           OUT1, OUT2, OUT3 = decoded numbers    (output, real)....
C           RETURN 1: invalid character in CIN....
C                    D.C. Radford   July 1985

      CHARACTER*(*) CIN
      INTEGER       NC
      REAL          OUT1, OUT2, OUT3, OUT(3)
      CHARACTER*81  IN


      IN(1:NC)=CIN(1:NC)
      DO 10 N=1,3
10       OUT(N)=0.0
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
         ITEST=0
         DO 40 I=ILO,IHI
            IF ((IN(I:I).LT.'0' .OR. IN(I:I).GT.'9')
     +          .AND. IN(I:I).NE.'-') THEN
               IF (IN(I:I).NE.'.') GO TO 200
               IF (ITEST.NE.0) GO TO 200
               ITEST=1
            ENDIF
40       CONTINUE
         READ(IN(ILO:IHI),50) OUT(N)
50       FORMAT(F40.0)
60       ILO=IHI+2
         IF (ILO.GT.NC) GO TO 150
100   CONTINUE
150   OUT1=OUT(1)
      OUT2=OUT(2)
      OUT3=OUT(3)
      RETURN
200   OUT1=OUT(1)
      OUT2=OUT(2)
      OUT3=OUT(3)
      RETURN 1
      END
