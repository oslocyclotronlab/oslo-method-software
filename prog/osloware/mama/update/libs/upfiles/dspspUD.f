      SUBROUTINE DSPSP(icount,iScaleChange)

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER COLORMAP(20),Limit(0:19),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color

      CHARACTER*28 HEADING
      COMMON/AXIS/iCE,itext,UNITx,UNITy
      INTEGER itext
      CHARACTER UNITx*3,UNITy*3
      COMMON/OL/I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                         OLlow,OLhigh,OLlocnt,OLhicnt

      iblack=20

      LOY=0
      CALL INITG(NX,NY)
      NCC=(HICH-LOCH)/NX+1
      CALL LIMG(NX,0,NY,LOY)
      NCHS=(HICH-LOCH+1)

      IF(iScaleChange.NE.0)THEN          !Autoscale of y-axis (meaning z-axis)
        LOCNT=0                          !Finding LOCNT,HICNT
        HICNT=4
        ii1=MAX0(LOCH+2,LOCH+0.01*NCHS)
        ii2=MIN0(HICH,HICH-0.01*NCHS)
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
           CALL VECT(X,FLOAT(MSPEC(I-1)))
           X=X+1.0
           CALL VECT(X,FLOAT(MSPEC(I-1)))
   40   CONTINUE
      ELSE
        Y=0.
        DO 50 I=1,NCC
   50      Y=Y+MSPEC(LOCH+I-1)
        FNCC=FLOAT(NCC)
        Y=Y/FNCC
        CALL PSPOT(X,Y)
        DO 55 ICH=LOCH,LOCH+(NCHS/NCC-1)*NCC,NCC
           Y=0.
           DO 60 I=1,NCC           
   60        Y=Y+MSPEC(ICH+I-1)
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
      DISP = .TRUE.

C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8,1)

      RETURN
999   CONTINUE
      RETURN
      END

