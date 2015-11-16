      SUBROUTINE WRTSP(ANS,K)

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER*40 ANS,FN
      COMMON /LUS/ IR,IW,IP,IG

C            write spectrum in SPEC array to disk file....
C               default file extension = .spe....

      ANS(1:2)='  '
      FN=ANS
      IF (K.GE.3) GO TO 15

C          ask for output file name....

      CALL ASK(22H Name of output file=?,22,FN,K)
      IF (K.EQ.0) RETURN
15    CALL SETEXT(FN,'.spe',I)

C            open output file....
C            ....ask for spectrum name....

      OPEN(1,FILE=FN,FORM='UNFORMATTED',STATUS='NEW',ERR=800)
      FILNAM = FN

      FN(I:I+3)='    '
      CALL ASK(43HSpectrum name=? (rtn for same as file name),43,ANS,K)
      IF (K.GT.0) FN = ANS
      fname(2,IDEST) = FN(1:8)

C          write out spectrum....

      WRITE(1,ERR=800) fname(2,IDEST),MAXCH+1,1,1,1
      WRITE(1,ERR=800) (rSPEC(IDEST,I-1),I=1,MAXCH+1)
      CLOSE(1)
      RETURN

800   WRITE(IW,*) 'Open/write error - spectrum not written.'
      CLOSE(1,STATUS='DELETE',ERR=820)
820   RETURN
      END
