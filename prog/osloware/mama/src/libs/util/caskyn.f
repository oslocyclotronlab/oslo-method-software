
C======================================================================

      SUBROUTINE CASKYN(MESAG,*)

C         mesag:    question to be asked (character string)
C         return 1: answer = N/n/0/<return>
C         return:   answer = Y/y/1

      CHARACTER*(*) MESAG
      CHARACTER*40  ANS
      INTEGER       NCA


10    CALL CASK2(MESAG,ANS,NCA,1)
      IF (NCA.EQ.0
     +   .OR.ANS(1:1).EQ.'N'
     +   .OR.ANS(1:1).EQ.'n'
     +   .OR.ANS(1:1).EQ.'0') RETURN 1
      IF (ANS(1:1).EQ.'Y'
     +   .OR.ANS(1:1).EQ.'y'
     +   .OR.ANS(1:1).EQ.'1') RETURN
      GO TO 10
      END
