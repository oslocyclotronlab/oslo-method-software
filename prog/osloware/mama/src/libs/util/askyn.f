
C======================================================================

      SUBROUTINE ASKYN(MESAG,NCM,*)

C         mesag:    question to be asked (integer array)
C         ncm:      number of characters in question (integer)
C         return 1: answer = N/n/0/<return>
C         return:   answer = Y/y/1

      INTEGER*4    MESAG(20)
      INTEGER      NCM, NCA
      CHARACTER*40 ANS


10    CALL ASK2(MESAG,NCM,ANS,NCA,1)
      IF (NCA.EQ.0
     +   .OR.ANS(1:1).EQ.'N'
     +   .OR.ANS(1:1).EQ.'n'
     +   .OR.ANS(1:1).EQ.'0') RETURN 1
      IF (ANS(1:1).EQ.'Y'
     +   .OR.ANS(1:1).EQ.'y'
     +   .OR.ANS(1:1).EQ.'1') RETURN
      GO TO 10
      END
