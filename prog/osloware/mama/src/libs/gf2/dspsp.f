      SUBROUTINE DSPSP(I1,I2,I3,*)

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER COLORMAP(20),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color
      REAL Limit(0:19)

      CHARACTER*28 HEADING
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      COMMON/OL/I3new,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh

      iblack=20
      I3new=I3
      Idistype=0                 !assume only one spectrum to be displayed
      LOY=0
      CALL INITG(NX,NY)
      IF (I1.GT.0 .AND. I2.GT.0) THEN
         Idistype=1
         IF (I1.GT.I2) I1=I2
         NY=NY/I2
         LOY=NY*(I1-1)
      ENDIF

      ICOL = COLORMAP(1)       !blue

      CALL LIMG(NX,0,NY,LOY)
 
      CALL Setmarker(0,0,0)
      IF((I1.EQ.1.AND.I2.EQ.0).OR.I3.EQ.1)THEN
        CALL SetMarker(1,1,2)                         !Full scale display
      ENDIF
      IF((I1.EQ.2.AND.I2.EQ.0).OR.I3.EQ.2)THEN
        CALL SetMarker(2,2,2)                         !Autoscaling
      ENDIF
       
      NCC =(HICH-LOCH)/NX+1
      NCHS=(HICH-LOCH+1)

      X0=LOCH
      DX=NCHS
      Y0=LOCNT
      DY0=HICNT-LOCNT
c      IF(DY0.LE.0.)DY0 = 1.
      DY=1.05*DY0

      CALL SETCOLOR(iblack)
      CALL TRAX(DX,X0,DY,Y0,IYAXIS)

      CALL SETCOLOR(ICOL)
      X=LOCH
      IF (NCC.EQ.1) THEN
        CALL PSPOT(X,rSPEC(IDEST,LOCH))
        DO 40 I=LOCH+1,HICH+1
           CALL VECT(X,rSPEC(IDEST,I-1))
           X=X+1.0
           CALL VECT(X,rSPEC(IDEST,I-1))
   40   CONTINUE
      ELSE
        Y=0.
        DO 50 I=1,NCC
   50      Y=Y+rSPEC(IDEST,LOCH+I-1)
        FNCC=FLOAT(NCC)
        Y=Y/FNCC
        CALL PSPOT(X,Y)
        DO 55 ICH=LOCH,LOCH+(NCHS/NCC-1)*NCC,NCC
           Y=0.
           DO 60 I=1,NCC
   60         Y=Y+rSPEC(IDEST,ICH+I-1)
           Y=Y/FNCC
           CALL VECT(X,Y)
           X=X+FNCC
           CALL VECT(X,Y)
   55   CONTINUE
      ENDIF
      CALL SETCOLOR(iblack)
      CALL DATETIME(HEADING)
      IF(itext.EQ.1)THEN
        CALL MSPOT(NX-60,LOY+NY+5)
        CALL PUTG(fname(2,IDEST)(1:8),8,1,1)
        CALL MSPOT(NX-60,LOY+NY-6)
        CALL PUTG(HEADING(1:11),11,1,1)
      ENDIF
      CALL FINIG
      DISP = .TRUE.


C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8,1)

      RETURN

999   CALL FINIG
      RETURN 1
      END

