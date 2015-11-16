      SUBROUTINE GETSP(ANS,NC)

C          ask for spectrum file name and read spectrum from disk....

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON /LUS/ IR,IW,IP,IG
      CHARACTER*40 ANS
      REAL SPEC(8192)

      ANS(1:2)='  '
      IF (NC.GT.2) GO TO 30
10    CALL ASK(23HSpectrum file or ID = ?,23,ANS,K)
      IF (K.EQ.0) RETURN
30    CALL READSP(ANS,SPEC,fname(2,IDEST),NUMCH,8192,&10)
      WRITE(IW,40)fname(2,IDEST),NUMCH
40    FORMAT(' Sp. ',A,I8,' chs   read.')
      FILNAM=ANS
      DISP=.FALSE.
      MAXCH=NUMCH-1
      DO i=0,MAXCH
        rSPEC(IDEST,i)=SPEC(i+1)
      ENDDO
      comm(2,IDEST)='|SP:'//fname(2,IDEST)
      RETURN
      END
