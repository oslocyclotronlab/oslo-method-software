      SUBROUTINE DISPWIN(WINMOD,*)

C          display windows as they are presently defined....

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER WINMOD
C         WINMOD = 0 : no mode defined....
C         WINMOD = 1 : look-up file mode....
C         WINMOD = 2 : slice file mode....

      INTEGER*2 LOOKTAB(8192)
      INTEGER   NCLOOK,LOOKMIN,LOOKMAX
      COMMON /LOOK/ NCLOOK,LOOKMIN,LOOKMAX,LOOKTAB

      CHARACTER*8 CLUV
      COMMON /LUS/ IR,IW,IP,IG

      HICH=HDX
      LOCH=LDX
      LOCNT=LDY

      IF (WINMOD.EQ.0) THEN
         WRITE(IW,*) 'Bad command: No window file open...'
         RETURN 1
      ELSEIF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet displayed...'
         RETURN 1
      ENDIF
      ISAVE=MCH(1)

      IF (WINMOD.EQ.2) THEN

C           slice mode....

         REWIND(13)
         LUV=0

100      READ(13,120,ERR=100,END=900) ILO,IHI,PTOT
120      FORMAT(5X,I5,3X,I5,8X,F8.4)
         LUV=LUV+1
         IF (ILO.GT.HICH.OR.IHI.LT.LOCH) GO TO 100

C           display limits....

         MCH(1)=ILO
         CALL DSPMKR(1)
         MCH(1)=IHI
         CALL DSPMKR(1)

C        display background....

         SUM = 0.0
         DO 150 I=ILO,IHI
            SUM = SUM + rSPEC(IDEST,I)
150      CONTINUE
         BG = SUM*(1.0-PTOT)/FLOAT(IHI-ILO+1)

         CALL INITG(NX,NY)
         X=FLOAT(ILO)+0.5
         CALL PSPOT(X,BG)
         X=FLOAT(IHI)+0.5
         CALL VECT(X,BG)

C        display window number....

         Y = (rSPEC(IDEST,IHI) + LOCNT)/2
         CALL PSPOT(X,Y)
         WRITE (CLUV,160) LUV
160      FORMAT(I8)
         CALL PUTG(CLUV,8,9,1)
         CALL FINIG

         GO TO 100

      ELSE

C           look-up mode....

         ILO=0
         LUV=0
         DO 200 I=1,NCLOOK
            IF (LOOKTAB(I).EQ.LUV) GO TO 200
            IF (LUV.EQ.0) GO TO 180

            IHI=I-2
            IF (ILO.GT.HICH.OR.IHI.LT.LOCH) GO TO 180

C           display limits....

            MCH(1)=ILO
            CALL DSPMKR(1)
            MCH(1)=IHI
            CALL DSPMKR(1)

C           display background....

            CALL INITG(NX,NY)
            X=FLOAT(ILO)+0.5
            CALL PSPOT(X,rSPEC(IDEST,ILO))
            X=FLOAT(IHI)+0.5
            CALL VECT(X,rSPEC(IDEST,IHI))

C           display look-up value....

            Y = (rSPEC(IDEST,IHI) + LOCNT)/2
            CALL PSPOT(X,Y)
            WRITE (CLUV,160) LUV
            CALL PUTG(CLUV,8,9,1)
            CALL FINIG

180         LUV=LOOKTAB(I)
            ILO=I-1
200      CONTINUE

      ENDIF

900   MCH(1)=ISAVE
      RETURN
      END
