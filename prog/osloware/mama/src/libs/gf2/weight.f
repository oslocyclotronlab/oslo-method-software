      SUBROUTINE WEIGHT(IDATA)

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER         IWMODE
      CHARACTER*8     NWTSP
      REAL            WTSP(8192)
      COMMON /WTMODE/ IWMODE,NWTSP,WTSP

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG

C          change weighting mode....

      IF (IDATA.LT.1 .OR. IDATA.GT.3) THEN
         WRITE(IW,*) 'IWMODE=1 : weight fit with results of fit'
         WRITE(IW,*) 'IWMODE=2 : weight fit with data'
         WRITE(IW,*) 'IWMODE=3 : weight fit with another spectrum'
250      CALL ASK2(8HIWMODE=?,8,ANS,K,1)
         IF (K.EQ.0) RETURN
         CALL ININ(ANS,K,IDATA,J1,J2,&250)
         IF (IDATA.LT.1 .OR. IDATA.GT.3) GO TO 250
      ENDIF

      IWMODE=IDATA-2
      IF (IWMODE.LT.1) RETURN
310   CALL ASK(33HWeighting spectrum file or ID = ?,33,ANS,K)
      CALL READSP(ANS,WTSP,NWTSP,NUMCH,8192,&310)
      IF (NUMCH.NE.MAXCH+1) WRITE(IW,*) 'Warning -- no. of chs in ',
     +   'weight spectrum different from no. of chs in fitted spectrum.'
      RETURN
      END
