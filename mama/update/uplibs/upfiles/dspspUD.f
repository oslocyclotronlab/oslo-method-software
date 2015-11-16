      SUBROUTINE DSPSP(icount,iScaleChange)

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT

      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      INTEGER itext
      COMMON/OL/I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                         OLlow,OLhigh,OLlocnt,OLhicnt

      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER*28 HEADING
      INTEGER COLORMAP(20),Limit(0:19),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

      iblack=20

      LOY=0
      CALL INITG(NX,NY)
      NCC=(HICH-LOCH)/NX+1
      CALL LIMG(NX,0,NY,LOY)
      NCHS=(HICH-LOCH+1)

C Gets window parameters for use for G95 routines through
C COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
      CALL GETGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)  

      IF(iScaleChange.NE.0)THEN          !Autoscale of y-axis (meaning z-axis)
        LOCNT=0                          !Finding LOCNT,HICNT
        HICNT=4
        ii1=MAX(LOCH+2.0,LOCH+0.01*NCHS)
        ii2=MIN(HICH+0.0,HICH-0.01*NCHS)
        IF(ii2-ii1.lt.10)THEN
          ii1=LOCH+1
          ii2=HICH
        ENDIF

        IF (NCC.EQ.1) THEN
          DO I=ii1,ii2
            IF(HICNT.LT.MSPEC(I))HICNT=MSPEC(I)
          ENDDO
        ELSE
          Y=0.
          DO I=1,NCC
            Y=Y+MSPEC(LOCH+I-1)
          ENDDO
          FNCC=FLOAT(NCC)
          Y=Y/FNCC
          DO ICH=LOCH,LOCH+(NCHS/NCC-1)*NCC,NCC
            Y=0.
            DO I=1,NCC           
              Y=Y+MSPEC(ICH+I-1)
            ENDDO
            Y=Y/FNCC
            X=X+FNCC
            IF(HICNT.LT.Y.AND.ICH.GE.ii1.AND.ICH.LE.ii2)HICNT=Y
          ENDDO
        ENDIF
      ENDIF

      ICOL = COLORMAP(1)     !blue

      LOCNT=0
      X0=LOCH
      DX=NCHS
      Y0=LOCNT
      DY0=HICNT-FLOAT(LOCNT-1)
      DY=2.*DY0
      icount=0
      do i=LOCH,HICH
        icount=icount+MSPEC(i)
      enddo

      CALL ERASE                         !Clear window

      CALL SETCOLOR(iblack)
      CALL TRAX(DX,X0,DY,Y0,IYAXIS)

      CALL SETCOLOR(ICOL)
      X=LOCH
      IF (NCC.EQ.1) THEN
        CALL PSPOT(X,FLOAT(MSPEC(LOCH)))
        DO 40 I=LOCH+1,HICH+1
            Y=FLOAT(MSPEC(I-1))
                 IF(IYAXIS.EQ.3.AND.Y.LT.0.5)Y=Y0 + 0.1
           CALL VECT(X,Y)
           X=X+1.0
           CALL VECT(X,Y)
   40   CONTINUE
      ELSE
        Y=0.
        DO 50 I=1,NCC
   50      Y=Y+MSPEC(LOCH+I-1)
        FNCC=FLOAT(NCC)
        Y=Y/FNCC
                 IF(IYAXIS.EQ.3.AND.Y.LT.0.5)Y=Y0 + 0.1
        CALL PSPOT(X,Y)
        DO 55 ICH=LOCH,LOCH+(NCHS/NCC-1)*NCC,NCC
           Y=0.
           DO 60 I=1,NCC           
   60        Y=Y+MSPEC(ICH+I-1)
           Y=Y/FNCC
                 IF(IYAXIS.EQ.3.AND.Y.LT.0.5)Y=Y0 + 0.1
           CALL VECT(X,Y)
           X=X+FNCC
           CALL VECT(X,Y)
   55   CONTINUE
      ENDIF
      CALL SETCOLOR(iblack)
      CALL DATETIME(HEADING)

c      IF(itext.EQ.1)THEN
c        CALL MSPOT(NX-60,LOY+NY+5)
c        CALL PUTG(fname(2,IDEST)(1:8),8,1)
c        CALL MSPOT(NX-60,LOY+NY-6)
c        CALL PUTG(HEADING(1:11),11,1)
c      ENDIF
c      DISP = .TRUE.


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
      CALL FINIG
      DISP = .TRUE.

C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8)

      RETURN
999   CONTINUE
      RETURN
      END

