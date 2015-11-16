
      SUBROUTINE SETCTS
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG
      IF(ITYPE.GT.1)THEN
        WRITE(6,*)'The command do not work on matrices (use instead PC)'
        WRITE(6,*)'If you want to set counts in a singles spectrum, you'
        WRITE(6,*)'should first select the spectrum with the IC or CR command'
        RETURN
      ENDIF
      iT=0
      Istatus=-7     !to flag that green button is active
      CALL CLEANUP
      CALL INITG(nx,ny)             !Pixel size of window
      is=18
      mx1=2
      mx2=mx1+2*is
      mx3=mx2+4
      mx4=mx3+2*is
      mx5=mx4+4
      mx6=mx5+3*is
      mx7=mx6+4
      mx8=mx7+3*is
      mx10=nx-1
      mx9=mx10-13
      my1=ny+42
      my2=my1-0.7*is
      my3=my2-0.3*is
      my33=my3-4    !An extra -4 because we never get iy > spec. area

      itest=0
      itry=0
10    IF (.NOT.DISP) GO TO 20
      WRITE(6,1)
1     FORMAT('Click on green button (or type X) to exit'
     +     /,'Click in spectrum to define counts (or type T)')
      CALL RETIC(X,Y,ANS)
      IF (ANS(1:1).EQ.'T' .OR. ANS(1:1).EQ.'t') GO TO 20
      IF (ANS(1:1).EQ.'X' .OR. ANS(1:1).EQ.'x') RETURN
      CALL CVXY(X,Y,ix,iy,1)
      IF(ix.GE.mx9.AND.ix.LE.mx10.AND.iy.GE.my33) RETURN

C            hit t for type, x to exit....
      ILO=X
      Y1=Y

30    CALL RETIC(X,Y,ANS)
      IF (ANS(1:1).EQ.'T' .OR. ANS(1:1).EQ.'t') GO TO 20
      IF (ANS(1:1).EQ.'X' .OR. ANS(1:1).EQ.'x') RETURN
      CALL CVXY(X,Y,ix,iy,1)
      IF(ix.GE.mx9.AND.ix.LE.mx10.AND.iy.GE.my33) RETURN

      IHI=X
      Y2=Y
      GO TO 80

C          ask for typed limits....
20    CALL ASK(17HType limits (chs),17,ANS,K)
      IF (K.EQ.0) RETURN
      CALL ININ(ANS,K,ILO,IHI,J2,&20)
      IF (ILO.GT.0.AND.IHI.EQ.0) IHI=ILO
      IF (ILO.GT.MAXCH.OR.IHI.GT.MAXCH.OR.ILO.LT.0.OR.IHI.LT.0) THEN
         WRITE(IW,*) 'Marker ch. outside spectrum - try again'
                  itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF

         GO TO 20
      ENDIF
      CALL ASK(19HType counts per ch.,19,ANS,K)
      CALL FFIN(ANS,K,Y,RJ1,RJ2,&20)
      Y1=Y
      Y2=Y
      iT=1

80    IF (ILO.GT.IHI) THEN
         ISAVE=IHI
         IHI=ILO
         ILO=ISAVE
         Y2=Y1
         Y1=Y
      ENDIF
C Updating comment in the heading of spectrum file
      itest=itest+1
      IF(itest.EQ.1)THEN
        xcomm='CT:'
        write(xcomm(4:7),991,ERR=997)ILO
991     FORMAT(I4)
        xcomm(8:8)='-'
        write(xcomm(9:12),991,ERR=997)IHI
997     CALL AddComment(xcomm,12)
      ENDIF
C        modify contents of spectrum....
      rSPEC(IDEST,ILO)=(Y1+Y2)/2.0
      IF (ILO.EQ.IHI) GO TO 10
      FNC=IHI-ILO
      DO 110 I=ILO,IHI
         rSPEC(IDEST,I)=Y1+(Y2-Y1)*FLOAT(I-ILO)/FNC
110   CONTINUE

C        display modified segment....
      CALL INITG(NX,NY)
      X=FLOAT(ILO)+0.5
      CALL PSPOT(X,Y1)
      X=FLOAT(IHI)+0.5
      CALL VECT(X,Y2)
      CALL FINIG
      IF(iT.EQ.1)RETURN

      ILO=IHI
      Y1=Y2
      GO TO 30

      END
