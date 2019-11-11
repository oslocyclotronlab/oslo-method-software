
      SUBROUTINE ADDWIN(WINMOD,*)

C          add window(s) to look-up file or slice input file....

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      LOGICAL         DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      REAL SPEC(8192)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER*40 ANS

      INTEGER WINMOD
C         WINMOD = 0 : no mode defined....
C         WINMOD = 1 : look-up file mode....
C         WINMOD = 2 : slice file mode....

      INTEGER*2 LOOKTAB(8192)
      INTEGER   NCLOOK,LOOKMIN,LOOKMAX
      COMMON /LOOK/ NCLOOK,LOOKMIN,LOOKMAX,LOOKTAB

      COMMON /LUS/ IR,IW,IP,IG

      DO i=1,maxch+1
        SPEC(i)=rSPEC(IDEST,i-1)
      ENDDO


      IF (WINMOD.EQ.0) THEN
         WRITE(IW,*) 'Bad command: No window file open...'
         RETURN 1
      ELSEIF (.NOT.DISP) THEN
         WRITE(IW,*) 'Bad command: New spectrum not yet displayed...'
         RETURN 1
      ENDIF

      WRITE (IW,*) 'Hit X to exit...'

C           get limits for integration, background....

100   CALL RETIC(X,Y,ANS)
      IF (ANS(1:1).EQ.'X'.OR.ANS(1:1).EQ.'x') GO TO 900
      ILO=X
      Y1=Y
      CALL RETIC(X,Y,ANS)
      IF (ANS(1:1).EQ.'X'.OR.ANS(1:1).EQ.'x') GO TO 900
      IHI=X
      Y2=Y
      IF (ILO.GT.IHI) THEN
         IHI=ILO
         ILO=X
         Y2=Y1
         Y1=Y
      ENDIF
      IF (WINMOD.EQ.1.AND.ILO.GE.NCLOOK) THEN
         WRITE (IW,120) NCLOOK
120      FORMAT(' Cannot - lower limit exceeds dimension of look-up',
     +          ' table (',I4,')')
         GO TO 100
      ENDIF

C           display limits....

      ISAVE=MCH(1)
      MCH(1)=ILO
      CALL DSPMKR(1)
      MCH(1)=IHI
      CALL DSPMKR(1)
      MCH(1)=ISAVE

      SUM=0.0
      AREA=0.0
      CENT=0.0
      FNC=IHI-ILO
      IF (FNC.EQ.0.0) FNC = 1.0

      IF (WINMOD.EQ.2) THEN

C           slice mode : background to be subtracted....
C           display background....

         CALL INITG(NX,NY)
         X=FLOAT(ILO)+0.5
         CALL PSPOT(X,Y1)
         X=FLOAT(IHI)+0.5
         CALL VECT(X,Y2)
         CALL FINIG

         DO 150 I=ILO,IHI
            SUM = SUM + SPEC(I+1)
            COU = SPEC(I+1) - (Y1+(Y2-Y1)*FLOAT(I-ILO)/FNC)
            AREA = AREA + COU
            CENT = CENT + COU*FLOAT(I-ILO)
150      CONTINUE

         IF (AREA.EQ.0.0) THEN
            CENT = 0.0
            GO TO 200
         ENDIF

         CENT = CENT/AREA + FLOAT(ILO)
         AREA = AREA/SUM
         DC = 0.0
         CALL ENERGY(CENT,DC,EG,DEG,*200)

C          write out results....

         WRITE (IW,190,ERR=180) ILO,IHI,AREA,EG
180      WRITE (13,190,ERR=100) ILO,IHI,AREA,EG
190      FORMAT('  Chs',I5,' to',I5,'   P/T =',F8.4,'   Energy =',F9.3)
         GO TO 100

C          no energy calibration defined or zero area....

200      WRITE (IW,230,ERR=220) ILO,IHI,AREA,CENT
220      WRITE (13,230,ERR=100) ILO,IHI,AREA,CENT
230      FORMAT('  Chs',I5,' to',I5,'   P/T =',F8.4,'    Cent. =',F9.3)

      ELSE

C           look-up mode : no background to be subtracted....

         DO 310 I=ILO,IHI
            AREA=AREA+SPEC(I+1)
            CENT=CENT+SPEC(I+1)*FLOAT(I-ILO)
310      CONTINUE

         IF (AREA.EQ.0.0) THEN
            CENT = 0.0
            GO TO 350
         ENDIF

         CENT = CENT/AREA + FLOAT(ILO)
         DC = 0.0
         CALL ENERGY(CENT,DC,EG,DEG,*350)

C          write out results....

         WRITE (IW,330,ERR=400) ILO,IHI,EG
330      FORMAT(' Chs',I5,' to',I5,'   Energy =',F9.3)
         GO TO 400

C          no energy calibration defined or zero area....

350      WRITE (IW,360,ERR=400) ILO,IHI,CENT
360      FORMAT(' Chs',I5,' to',I5,'    Cent. =',F9.3)

C          ask for look-up value....

400      CALL ASK(32H  Look-up value = ? (A to abort),32,ANS,NC)
         IF (NC.EQ.0) GO TO 400
         IF (ANS(1:1).EQ.'A'.OR.ANS(1:1).EQ.'a') GO TO 100
         CALL ININ(ANS,NC,LUV,J1,J2,*400)
         IF (LUV.LT.LOOKMIN) LOOKMIN=LUV
         IF (LUV.GT.LOOKMAX) LOOKMAX=LUV
         IF (IHI.GE.NCLOOK) IHI=NCLOOK-1
         DO 450 I=ILO+1,IHI+1
            LOOKTAB(I)=LUV
450      CONTINUE

      ENDIF
      GO TO 100

900   RETURN
      END
