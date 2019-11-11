      SUBROUTINE DSPFIT

      INTEGER       MCH(2),lo,hi
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      
      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      REAL         SAVE(8192)
      COMMON /SAV/ SAVE

      REAL B(51)
      LO=MCH(1)
      HI=MCH(2)
      IF (LO.LT.LOCH) LO=LOCH
      IF (HI.GT.HICH) HI=HICH
      LO=LO+1
      IF (HI.LE.LO) RETURN
      HI=HI+1
      IF(HI.GT.8192)HI=8192                        !Magne
C          display background....
      CALL EVAL(PARS,IFIXED(4),Y,NPKS,-9)
      CALL EVAL(PARS,LO,Y,NPKS,-1)
      SAVE(LO)=Y
      X1=FLOAT(LO)-0.5

      CALL INITG(NX,NY)
      CALL PSPOT(X1,Y)
      DO 20 I=LO+1,HI
         X1=X1+1.0
         CALL EVAL(PARS,I,Y,NPKS,-1)
         SAVE(I)=Y
         CALL VECT(X1,Y)
20    CONTINUE

C          display fit....

      CALL EVAL(PARS,LO,Y,NPKS,0)
      X1=FLOAT(LO)-0.5
      CALL PSPOT(X1,Y)
      DO 30 I=LO+1,HI
         X1=X1+1.0
         CALL EVAL(PARS,I,Y,NPKS,0)
         CALL VECT(X1,Y)
                  
30    CONTINUE

C          display difference....

      Y1=(Y+(LOCNT))/2.0
      X1=FLOAT(LO)-0.5
      CALL EVAL(PARS,LO,Y,NPKS,0)
      Y=rSPEC(IDEST,LO-1)-Y+Y1
      CALL PSPOT(X1,Y)
      DO 40 I=LO+1,HI
         X1=X1+1.0
         CALL EVAL(PARS,I,Y,NPKS,0)
         Y=rSPEC(IDEST,I-1)-Y+Y1
         CALL VECT(X1,Y)
40    CONTINUE

C          display each peak seperately....

      IF (NPKS.EQ.1) GO TO 90
      DO 45 I=1,6
45       B(I)=0.0
      B(4)=PARS(4)
      B(5)=PARS(5)
      DO 80 J=1,NPKS
         DO 50 I=4,6
50          B(I+3)=PARS(3*J+I)
         CALL EVAL(B,IFIXED(4),Y,1,-9)
         ILO=B(7)-3.0*B(8)
         IHI=B(7)+3.0*B(8)
         IF (ILO.LT.LO) ILO=LO
         IF (IHI.GT.HI) IHI=HI
         CALL EVAL(B,ILO,Y,1,0)
         X1=FLOAT(ILO)-0.5
             if(ILO.GT.8192)ILO=8192
         CALL PSPOT(X1,Y+SAVE(ILO))
         DO 60 I=ILO+1,IHI
            X1=X1+1.0
            CALL EVAL(B,I,Y,1,0)
            CALL VECT(X1,Y+SAVE(I))
60       CONTINUE
80    CONTINUE
90    CALL FINIG
      RETURN
      END
