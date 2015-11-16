      SUBROUTINE OUTLAYSPY
C Routine to display part of a matrix as a number of singles spectra
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      COMMON/spy1/m1,m2,lx(0:23),hx(0:23),ly(0:23),hy(0:23),mon(0:23),specna(0:23),iD,sleeptime
      INTEGER lx,hx,ly,hy,mon,iD,sleeptime
      CHARACTER specna*8
      COMMON/spy2/dCounts(0:511,0:23)
      REAL dCounts
      COMMON/spy3/OLhi(24),OLlc(24),OLhc(24)
      REAL Olhi,OLlc,OLhc
      

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

      CHARACTER*28 HEADING
      REAL Spec(512)
      CHARACTER TEX*14

      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3

      itext=1
      iCE=1
      IYAXIS=1
      UNITx='hrs'
      UNITy='c/s'
      I2=ABS(m1-m2)+1               !number of spectra
      maxdim=512                    !max dimension for rows
      inx=1                         !Parting the screen into inx*iny parts
      iny=I2      

      IF(I2.GT. 8.AND.I2.LE.16)THEN 
        inx=2
        iny=(FLOAT(I2)/2.)+0.5
      ENDIF
      IF(I2.GT.16.AND.I2.LE.24)THEN
        inx=3
        iny=(FLOAT(I2)/3.)+0.5
      ENDIF

      CALL INITG(NX0,NY0) 

      CALL ERASE                    !Clear window

      NY=NY0/iny                    !Number of y-pixels pr. spectrum
      NX=NX0/inx                    !Number of x-pixels pr. spectrum

C Drawing grid and frame for inx*iny spectra
      CALL SETCOLOR(20)             !black
      DO i=0,inx
        CALL KTRAS(i*NX,20    ,0)
        CALL KTRAS(i*NX,NY0+18,1)
      ENDDO
      DO i=0,iny-1
        CALL KTRAS(0  ,i*NY+20,0)
        CALL KTRAS(NX0,i*NY+20,1)
      ENDDO
                
C Starting the loop of drawing I2 spectra
      I1=0
      DO m=m1,m2
        I1=I1+1
        DO i=0,maxdim-1              
          Spec(i+1)=dCounts(i,m)
        ENDDO

        OLlow  =OLlc(I1)                   !Using predefined values
        OLhigh =OLhc(I1)
        OLlocnt=0
        OLhicnt=OLhi(I1)  
        NCC =(OLhigh-OLlow)/NX+1
        NCHS=OLhigh-OLlow+1

C Finding (x,y) pixel origo for spectrum I1
        LOY=NY*((I1-1)/inx)           !Pixelstart in y-direction
        II1=I1
        DO WHILE (II1.GT.inx)
          II1=II1-inx
        ENDDO
        LOX=NX*(II1-1)                !Pixelstart in x-direction
        CALL LIMG(NX,LOX,NY,LOY)      !Informing screen window

C Gets window parameters for use for G95 routines through
C COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
       CALL GETGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)  


C Making axis
        X0=OLlow
        DX=NCHS
        Y0=OLlocnt
        DY0=OLhicnt-OLlocnt
        DY=1.*DY0
      
        CALL SETCOLOR(20)            !black
        ixAxis=0
        IF(LOY.EQ.0)ixAxis=1
        CALL TRAX(DX,X0,DY,Y0,IYAXIS,ixAxis)

        CALL SETCOLOR(10)           !red
        X=OLlow

        iUp=0                       !not yet first up
        IF(NCC.EQ.1)THEN
          s=Spec(INT(OLlow)+1)
          CALL PSPOT(X,s)
          DO i=INT(OLlow)+1,INT(OLhigh)+1
             s=Spec(i)
             IF(s.NE.0)iUp=iUp+1
             IF(iUp.GT.1)THEN
               CALL VECT(X,s)
             ELSE
               CALL PSPOT(X,s)
             ENDIF
             X=X+1.0
             IF(iUp.GT.1)CALL VECT(X,s)
          ENDDO 
        ELSE
          Y=0.
          DO i=1,NCC
            Y=Y+Spec(INT(OLlow)+i)
          ENDDO
          FNCC=FLOAT(NCC)
          Y=Y/FNCC
          CALL PSPOT(X,Y)
          DO ICH=INT(OLlow),INT(OLlow+(NCHS/NCC-1))*NCC,NCC
            Y=0.
            DO i=1,NCC
              Y=Y+Spec(ICH+i)
            ENDDO
            Y=Y/FNCC
            IF(Y.NE.0)iUp=iUp+1
            IF(iUp.GT.1)THEN
              CALL VECT(X,Y)
            ELSE
              CALL PSPOT(X,Y)
            ENDIF
            X=X+FNCC
            IF(iUp.GT.1)CALL VECT(X,Y)
          ENDDO
        ENDIF

        CALL SETCOLOR(1)              !blue
        CALL KTRAS(LOX+NX/2,LOY+NY+2,0)
        TEX=specna(m)
        lastno=lnblnk(TEX)
        IF(mon(m).EQ.1)TEX(1:lastno+9)=TEX(1:lastno)//'/ monitor'
c	IF(mon(m).EQ.0)TEX(1:lastno+9)=TEX(1:lastno)//'= monitor'

        CALL PUTG(TEX,14,5)
      ENDDO

C Putting on unit on axis
      CALL INITG(NX,NY)
      CALL SETCOLOR(20)                !chocolate
      CALL DATETIME(HEADING)
      CALL MSPOT(nx/2,ny+25)
      CALL PUTG('Oslo Cyclotron Laboratory (OCL) '//HEADING(1:26),47,5)

      CALL SETCOLOR(20)               !black
C Units on x-axis
      CALL MSPOT(NX-13,0)
      CALL PUTG(UNITx,3,8)
      CALL MSPOT(7,NY+8)
      CALL PUTG(UNITy,3,2)
      CALL FINIG
      RETURN

999   CALL FINIG
      RETURN
      END

