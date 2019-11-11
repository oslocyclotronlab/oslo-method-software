      SUBROUTINE DSPSP(I1,I2,I3,*)

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER COLORMAP(20),Colorc(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      REAL Limit(0:19)

      CHARACTER*28 HEADING
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      COMMON/OL/I3new,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh
		
      COMMON/logauto/ylow
      
      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
	  
	  INTEGER ch1,ch2
	  COMMON/PMchan/ch1,ch2
	  CHARACTER PMchannels*9

      ylow = 10000000000.
      DO i=0,8191
		   IF(rSpec(IDEST,i).LT.ylow.AND.rSpec(IDEST,i).GT.0)ylow=rSpec(IDEST,i)
      ENDDO

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
      
C Gets window parameters for use for F77 routines through
C COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
      CALL GETGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)  
 
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
	  IF(IYAXIS.EQ.3.AND.Y0.LE.0)Y0=0.1
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
           Y=rSPEC(IDEST,I-1)
                 IF(IYAXIS.EQ.3.AND.Y.LT.Y0)Y=Y0
           CALL VECT(X,Y)
           X=X+1.0
           CALL VECT(X,Y)
   40   CONTINUE
      ELSE
        Y=0.
        DO 50 I=1,NCC
   50      Y=Y+rSPEC(IDEST,LOCH+I-1)
        FNCC=FLOAT(NCC)
        Y=Y/FNCC
                 IF(IYAXIS.EQ.3.AND.Y.LT.Y0)Y=Y0
        CALL PSPOT(X,Y)
        DO 55 ICH=LOCH,LOCH+(NCHS/NCC-1)*NCC,NCC
           Y=0.
           DO 60 I=1,NCC
   60         Y=Y+rSPEC(IDEST,ICH+I-1)
           Y=Y/FNCC
                  IF(IYAXIS.EQ.3.AND.Y.LT.Y0)Y=Y0
           CALL VECT(X,Y)
           X=X+FNCC
           CALL VECT(X,Y)
   55   CONTINUE
      ENDIF
      CALL SETCOLOR(iblack)
      CALL DATETIME(HEADING)
C Finding date text to write   
      n2=0   
      n3=0
      ic=0
      DO i=1,17
         IF(HEADING(i:i).EQ.'-'.OR.HEADING(i:i).EQ.' '.OR.HEADING(i:i).EQ.':')THEN
            ic=ic+1
            IF(ic.EQ.2)n1=i
            IF(ic.EQ.3)n2=i
            IF(ic.EQ.4)n3=i
         ENDIF
      ENDDO
      numb=n1+(n3-n2)+1
      IF(itext.EQ.1)THEN
        CALL MSPOT(NX-60,LOY+NY+5)
        CALL PUTG(fname(2,IDEST)(1:8),8,1)
        CALL MSPOT(NX-60,LOY+NY-6)
        CALL PUTG(HEADING(1:n1-1)//HEADING(n2+1:n3+3),numb,1)
      ENDIF
	  
	  IF(itext.EQ.1.AND.ch1.GT.-1)THEN
        CALL MSPOT(NX-60,LOY+NY-17)		
		WRITE(PMchannels(1:4),991)ch1
        PMchannels(5:5)='-'
        write(PMchannels(6:9),991)ch2
        CALL PUTG(PMchannels(1:9),9,1)
991     FORMAT(I4)
      ENDIF
	  ch1=-1
	  ch2=-1
      CALL FINIG
      DISP = .TRUE.


C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8)

      RETURN

 999  CALL FINIG
      RETURN 1
      END

