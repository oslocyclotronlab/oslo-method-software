      SUBROUTINE DSPMKR(K)

      INTEGER       MCH(2)
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

      CHARACTER*15  MCHAR
      DATA          MCHAR /'123456789ABCDEF'/
      
      CALL INITG(NX,NY)
      I=K
      IF (K.EQ.99) I=1
20    IF (I.GT.2) THEN
         X=PPOS(I-2)+0.5
         ICH=X
      ELSE
         ICH=MCH(I)
      ENDIF

      IF (ICH.GE.LOCH.AND.ICH.LE.HICH) THEN
         IF (I.GT.2) THEN
            Y=rSPEC(IDEST,ICH)
            CALL CVXY(X,Y,IX,IY,1)
            CALL CVXY(X1,Y,IX,IY-10,2)
            CALL SYMBG(9,X,Y,7,90.0)
            CALL MSPOT(IX,IY-10)
            CALL IVECT(IX,IY-30)
            CALL MSPOT(IX,IY-45)
            CALL PUTG(MCHAR(I-2:I-2),1,5)
         ELSE
            X=FLOAT(ICH)+0.5
            Y=LOCNT
            CALL PSPOT(X,Y)
            CALL VECT(X,rSPEC(IDEST,ICH))
         ENDIF
      ENDIF

      I=I+1
      IF (K.EQ.99.AND.I.LE.NPKS+2) GO TO 20
      CALL FINIG      
      RETURN
      END
