      SUBROUTINE CASK(MESAG,ANS,NCA)

C         mesag: question to be asked (character string)
C         ans:   answer recieved (character*40)
C         nca:   number of characters received in answer (integer)
C         mca:   max. number of characters asked for in answer (integer)

      CHARACTER*(*) MESAG
      CHARACTER*40  ANS
      INTEGER       NCM, NCA
      CHARACTER*80  CMESAG
      INTEGER*4     IMESAG(20)
      EQUIVALENCE  (CMESAG,IMESAG)


      IMCA=40
      GO TO 10

           ENTRY CASK2(MESAG,ANS,NCA,MCA)

      IMCA=MCA
10    NCM = LEN(MESAG)
      IF (NCM.GT.80) NCM = 80
      CMESAG(1:NCM) = MESAG(1:NCM)
      CALL ASK2(IMESAG,NCM,ANS,NCA,IMCA)
      RETURN
      END
