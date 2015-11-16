      SUBROUTINE OUTLAY(icount,iScaleChange)
C Routine to display part of a matrix as a number of singles spectra
      LOGICAL DISP
      INTEGER XDIM,YDIM
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/OL/I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                         OLlow,OLhigh,OLlocnt,OLhicnt
      REAL dex(14)
    
      CHARACTER*28 HEADING
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      INTEGER iSpec(4096)
      CHARACTER TEX1*3,TEX2*4,TEX*9
      COMMON/AXIS/iCE,itext,UNITx,UNITy
      CHARACTER UNITx*3,UNITy*3
      INTEGER COLORMAP(20),Limit(0:19),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color

      COMMON/SAVEOUTLAY/OLhi,OLlc,OLhc
      INTEGER OLhi(64),OLlc(64),OLhc(64)

      iblack=20

      IF(iScaleChange.EQ.-1)THEN      !first call
        I2=ABS(m1-m2)+1               !number of spectra
        maxdim=XDIM                   !max dimension according to rows or coloumns
        IF(iRC.EQ.2) maxdim=YDIM
        inx=1                         !Parting the screen into inx*iny parts
        iny=1      
        IF(I2.GT. 1.AND.I2.LE. 2)THEN !2*1 spectra
          inx=2
          iny=1
        ENDIF
        IF(I2.GT. 2.AND.I2.LE. 4)THEN !2*2 spectra
          inx=2
          iny=2
        ENDIF
        IF(I2.GT. 4.AND.I2.LE. 6)THEN !3*2 spectra
          inx=3
          iny=2
        ENDIF
        IF(I2.GT. 6.AND.I2.LE. 9)THEN !3*3 spectra
          inx=3
          iny=3
        ENDIF
        IF(I2.GT. 9.AND.I2.LE.12)THEN !4*3 spectra
          inx=4
          iny=3
        ENDIF
        IF(I2.GT.12.AND.I2.LE.16)THEN !4*4 spectra
          inx=4
          iny=4
        ENDIF
        IF(I2.GT.16.AND.I2.LE.20)THEN !5*4 spectra
          inx=5
          iny=4
        ENDIF
        IF(I2.GT.20.AND.I2.LE.25)THEN !5*5 spectra
          inx=5
          iny=5
        ENDIF
        IF(I2.GT.25.AND.I2.LE.30)THEN !6*5 spectra
          inx=6
          iny=5
        ENDIF
        IF(I2.GT.30.AND.I2.LE.36)THEN !6*6 spectra
          inx=6
          iny=6
        ENDIF           
        IF(I2.GT.36.AND.I2.LE.42)THEN !7*6 spectra
          inx=7
          iny=6
        ENDIF
        IF(I2.GT.42.AND.I2.LE.49)THEN !7*7 spectra
          inx=7
          iny=7
        ENDIF
        IF(I2.GT.49.AND.I2.LE.56)THEN !8*7 spectra
          inx=8
          iny=7
        ENDIF
        IF(I2.GT.56.AND.I2.LE.64)THEN !8*8 spectra
          inx=8
          iny=8
        ENDIF
      ENDIF

      CALL INITG(NX0,NY0) 

      CALL ERASE                    !Clear window

      NY=NY0/iny                    !Number of y-pixels pr. spectrum
      NX=NX0/inx                    !Number of x-pixels pr. spectrum

C Drawing grid and frame for inx*iny spectra
      CALL SETCOLOR(iblack)
      DO i=0,inx
        CALL KTRAS(i*NX,20    ,0)
        CALL KTRAS(i*NX,NY0+18,1)
      ENDDO
      DO i=0,iny-1
        CALL KTRAS(0  ,i*NY+20,0)
        CALL KTRAS(NX0,i*NY+20,1)
      ENDDO
                
C Starting the loop of drawing I2 spectra
      icount=0
      I1=0
      DO m=m1,m2
        I1=I1+1
        IF(iRC.EQ.1)THEN
          DO i=0,maxdim-1              
            iSpec(i+1)=MAT(i,m)
          ENDDO
        ENDIF
        IF(iRC.EQ.2)THEN
          DO i=0,maxdim-1              
            iSpec(i+1)=MAT(m,i)
          ENDDO
        ENDIF

        IF(iScaleChange.NE.-1)THEN
          OLlow  =OLlc(I1)                   !Using old values
          OLhigh =OLhc(I1)
          OLlocnt=0
          OLhicnt=OLhi(I1)  
        ENDIF

        IF(iScaleChange.NE.0.AND.I3.EQ.2)THEN
          DO i=maxdim,10,-1
            IF(iSpec(i).NE.0)GO TO 99
          ENDDO
 99       a=0
          b=i-1
          CALL GRAX(a,b,dex,nv,1)
          OLlow=a
          OLhigh=dex(nv)*(nv+1)/nv
        ENDIF

        IF(OLlow.LT.0)OLlow=0
        IF(OLhigh.GT.maxdim-1)OLhigh=maxdim-1
        NCC =(OLhigh-OLlow)/NX+1
        NCHS=OLhigh-OLlow+1
        DO i=OLlow+1,OLhigh+1
          icount=icount+iSpec(i)
        ENDDO 

        IF(iScaleChange.NE.0)THEN          !Autoscale of y-axis (meaning z-axis)
          OLhicnt=4
          ii1=MAX0(OLlow+2,OLlow+0.01*NCHS)
          ii2=MIN0(OLhigh,OLhigh-0.01*NCHS)
          IF(ii2-ii1.lt.10)THEN
            ii1=OLlow+1
            ii2=OLhigh+1
          ENDIF

          IF (NCC.EQ.1) THEN
            DO I=ii1,ii2
              IF(OLhicnt.LT.iSpec(I+1))OLhicnt=iSpec(I+1)
            ENDDO
          ELSE
            Y=0.
            DO I=1,NCC
              Y=Y+iSpec(OLlow+I)
            ENDDO
            FNCC=FLOAT(NCC)
            Y=Y/FNCC
            DO ICH=OLlow,OLlow+(NCHS/NCC-1)*NCC,NCC
              Y=0.
              DO I=1,NCC           
                Y=Y+iSpec(ICH+I)
              ENDDO
              Y=Y/FNCC
              X=X+FNCC
              IF(OLhicnt.LT.Y.AND.ICH.GE.ii1.AND.ICH.LE.ii2)OLhicnt=Y
            ENDDO
          ENDIF
        ENDIF

        OLhi(I1)=OLhicnt            !Storing for next display
        OLlc(I1)=OLlow
        OLhc(I1)=OLhigh


        
C Finding (x,y) pixel origo for spectrum I1
        LOY=NY*((I1-1)/inx)           !Pixelstart in y-direction
        II1=I1
        DO WHILE (II1.GT.inx)
          II1=II1-inx
        ENDDO
        LOX=NX*(II1-1)                !Pixelstart in x-direction
        CALL LIMG(NX,LOX,NY,LOY)      !Informing screen window

C Making axis
        X0=OLlow
        DX=NCHS
        Y0=OLlocnt
        DY0=OLhicnt-FLOAT(OLlocnt-1)
        DY=2.*DY0

        CALL SETCOLOR(iblack)
        CALL TRAX(DX,X0,DY,Y0,IYAXIS)

        ICOL = COLORMAP(1)

        CALL SETCOLOR(ICOL)
        X=OLlow
        IF(NCC.EQ.1)THEN
          s=iSpec(OLlow+1)
          CALL PSPOT(X,s)
          DO i=OLlow+1,OLhigh+1
             s=iSpec(i)
             CALL VECT(X,s)
             X=X+1.0
             CALL VECT(X,s)
          ENDDO 
        ELSE
          Y=0.
          DO i=1,NCC
            Y=Y+iSpec(OLlow+i)
          ENDDO
          FNCC=FLOAT(NCC)
          Y=Y/FNCC
          CALL PSPOT(X,Y)
          DO ICH=OLlow,OLlow+(NCHS/NCC-1)*NCC,NCC
            Y=0.
            DO i=1,NCC
              Y=Y+iSpec(ICH+i)
            ENDDO
            Y=Y/FNCC
            CALL VECT(X,Y)
            X=X+FNCC
            CALL VECT(X,Y)
          ENDDO
        ENDIF

        CALL SETCOLOR(iblack)
        CALL KTRAS(LOX+NX-10,LOY+NY-10,0)
        WRITE(TEX2,20)m
   20   FORMAT(I4)  
        TEX=TEX1//TEX2
        IF(itext.EQ.1)CALL PUTG(TEX,7,8,1)
      ENDDO
      CALL DATETIME(HEADING)
      IF(itext.EQ.1)THEN
        CALL MSPOT(NX0-60,LOY+NY+5)
        CALL PUTG(fname(1,1)(1:8),8,1,1)
        CALL MSPOT(NX0-60,LOY+NY-6)
        CALL PUTG(HEADING(1:11),11,1,1)
      ENDIF
c      CALL FINIG
      DISP=.TRUE.

C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8,1)

      RETURN

999   CALL FINIG
      RETURN
      END

