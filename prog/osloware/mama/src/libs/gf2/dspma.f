      SUBROUTINE DSPMA(I1,I2,I3)
C Routine to display 2-dimensional spectra (matrices)
C M.Guttormsen, November 1993, June 1998
C patched by Andreas Schiller, Sep. 10 2003
C reason: more standard repetitive format

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      INTEGER XDIM,YDIM
      CHARACTER APP*4, TEX*8
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      COMMON/REMEMBER/mlimit(0:19)
      COMMON/OL/I3new,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER*28 HEADING
      INTEGER COLORMAP(20),Color(0:19)
      REAL Limit(0:19), mlimit
      COMMON /COLORMAP/ COLORMAP,Limit,Color

C two new variables A.S.
      CHARACTER*6 F1STR
      CHARACTER*4 F2STR

      iwhite=19
      iblack=20
      I3new=I3
      IF(Idistype.NE.-3)Idistype=0   !assume only one matrix to display
                                     !=-3 is exception first time
C Testing that I's have right values
      IF(I1.LT.1.AND.I2.NE.0) Istatus=1
      IF(I2.GT.0.AND.I1.GT.I2)Istatus=1
      IF((I1.GT.2.AND.I2.EQ.0).OR.I3.GT.2)Istatus=1
      IF(Istatus.NE.0)THEN
        WRITE(6,*)'I1,I2,I3,IDEST,ITYPE',I1,I2,I3,IDEST,ITYPE
        RETURN
      ENDIF

C Testing if full scale values should be used      
      IF((I1.EQ.1.AND.I2.EQ.0).OR.I3.EQ.1)THEN
        CALL SetMarker(1,1,1)       
      ENDIF

C Testing if autoscale should be used      
      IF((I1.EQ.2.AND.I2.EQ.0).OR.I3.EQ.2)THEN
        CALL SetMarker(2,2,2)
      ENDIF

C Setting up 14 colors for 3D-plot
      nCol=14
      
C Logaritmic display
      IF(HDZ.GE.0.)THEN   !sign of HDZ used to determine log or manual plot 
        c1    = ABS(LDZ)
        ctop  = ABS(HDZ)
        f1    = log(c1)
        ftop  = log(ctop)
        df    = ABS(ftop-f1)/FLOAT(nCol-3)
        DO i=1,nCol-2
          fi=f1+df*FLOAT(i-1)
          Limit(i)=exp(fi)
        ENDDO       
        Limit(0)=-Limit(1)

      ELSE                     !ask for manual level-values as input

        WRITE(6,1)
 1      FORMAT(/'Type level-values for 2-dimensional plot.',/,
     +          'Each value must be > 0 and end with RETURN.',/,       
     +          'Type 0 and RETURN to terminate the input.',/,   
     +          'Example:',/,       
     +          '100',/,    
     +          '200',/,       
     +          '400',/,    
     +          '0',/,
     +          '(Maximum 16 levels)',/)
C Reading in levels-values:
        DO i=1,20
          Limit(i)=100*i
          IF(i.EQ.1)Limit(i)=1
          IF(mlimit(0).NE.0)Limit(i)=mlimit(i)
          WRITE(IW,29)Limit(i)
 29       FORMAT('Type level-value (0 for stop): <',F,'>:',$)
          CALL READF(5,Limit(i))
          IF(Istatus.NE.0.OR.Limit(i).LE.0)THEN
            Istatus=0
            GO TO 97
          ENDIF
        ENDDO
 97     Limit(0)=-Limit(1)
        ncol=i
        DO i=0,20
          mlimit(i)=Limit(i)   !just to remember for next time as default
          IF(i.GT.ncol-1)mlimit(i) = 0.
        ENDDO
      ENDIF

      CALL INITG(NX,NY)          ! Finding window size (nx,ny)

C Setting up color pallett of 'ncol' colors 
C Pixel basis is 'is'
C Left lower pixel-origo is (mx,my)
C Checks first if all colors have place 
      is   =10
      IF(ncol.GE.2)THEN    !Draw pallett - if more than 2 colors
        mx1  =nx-20-(ncol-1)*is-ncol*is !15 -> 20
        ihigh=3
        my1  =ny+43-ihigh 
        DO l=0,ncol-1
          CALL SETCOLOR(Color(l))
          mxx=mx1+l*2*is
          DO j=my1,my1+ihigh                !Box in case of color
            CALL KTRAS(mxx,j,0)            
            CALL KTRAS(mxx+is,j,1)     
          ENDDO  
          IF(Color(l).EQ.19)THEN            !Box in case of white color  
            CALL SETCOLOR(iblack)   
            CALL KTRAS(mxx   ,my1      ,0)
            CALL KTRAS(mxx+is,my1      ,1)
            CALL KTRAS(mxx+is,my1+ihigh,1)
            CALL KTRAS(mxx   ,my1+ihigh,1)
            CALL KTRAS(mxx   ,my1      ,1)
          ENDIF
          IF(l.LT.ncol-1)THEN               !Writing limits with numbers
            CALL SETCOLOR(iblack)
            IF(l/2*2.EQ.l)THEN              !Staggering output
              idown=12
            ELSE
              idown=21
            ENDIF
            xLim=Limit(l)
            ndecimal=ABS(log10(ABS(xLim)))
            IF(ndecimal.GT.3)ndecimal=3
C changed some of the following lines to make standard repetitive formats
C added the next two lines A.S.
            WRITE(F1STR,161)ndecimal+5,ndecimal+2
            WRITE(F2STR,162)ndecimal+2
            IF(ABS(xLim).LT.0.0001.AND.xLim.LT.0.)THEN
               CALL KTRAS(mxx+is-20,my1-idown,0) ! somewhat extra to the left
               WRITE(TEX,160)xLim
            ELSEIF(xLim.LT.0.0)THEN
               CALL KTRAS(mxx+is-20,my1-idown,0) ! somewhat extra to the left
C changed the following line A.S.
               WRITE(TEX,FMT=F1STR)xLim
            ELSEIF(ABS(xLim).LT.0.00001)THEN
               CALL KTRAS(mxx+is-10,my1-idown,0) ! somewhat extra to the left
               WRITE(TEX,160)xLim
            ELSEIF(ABS(xLim).LT.0.001)THEN
               CALL KTRAS(mxx+is-5,my1-idown,0) ! somewhat extra to the left
C changed the following line A.S.
               WRITE(TEX,FMT=F1STR)xLim
            ELSEIF(ABS(xLim).LT.10.)THEN
               CALL KTRAS(mxx+is,my1-idown,0)
C changed the following line A.S.
               WRITE(TEX,FMT=F1STR)xLim
            ELSEIF(ABS(xLIM).LT.9999999)THEN
               Lim=xLim+0.5
               mcorr=0
               IF(Lim.GT.999)mcorr=6
               CALL KTRAS(mxx+is-mcorr,my1-idown,0)
C changed the following line A.S.
               WRITE(TEX,FMT=F2STR)Lim
            ENDIF
  160       FORMAT(E8.2)
C changed the following two lines A.S.
 161        FORMAT('(F',I1.1,'.',I1.1,')')
 162        FORMAT('(I',I1.1,')')
            CALL PUTG(TEX,8,1,1)
          ENDIF
        ENDDO 
       ENDIF

C Want to display matrix no. I1 out of I2
C Parting up the screen in i.e. inx*iny spectra,
C and finding origo for I1
      inx=1
      iny=1
      LOY=0
      LOX=0
      IF(I1.GT.0.AND.I2.GT.0) THEN
        IF(I1.GT.I2)I1=I2
        Idistype=3
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
        
        IF(I2.GT.36)THEN
          WRITE(6,*)'Sorry, max. number of matrices is 36'
          Istatus=2
          GO TO 999
        ENDIF

        NX0=NX
        NY0=NY
        NX=NX/inx           !Number of x-pixels pr. spectrum
        NY=NY/iny           !Number of y-pixels pr. spectrum
        
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

        LOY=NY*((I1-1)/inx) !Pixelstart in y-direction
        II1=I1
        DO WHILE (II1.GT.inx)
          II1=II1-inx
        ENDDO
        LOX=NX*(II1-1)      !Pixelstart in x-direction
      ENDIF

      CALL LIMG(NX,LOX,NY,LOY) !Informing screen window

C Asures that everything is OK
      IF((LDX. LT.0).OR.(LDX. GT.HDX). OR.(HDX. GT.4095).
     +OR.(LDY.LT.0).OR.(LDY.GT.HDY).OR.(HDY.GT.511))THEN
      write(6,*)'LDX,HDX,LDY,HDY',LDX,HDX,LDY,HDY
        Istatus=1
        GO TO 999
      ENDIF

      X0=LDX
      DX=HDX-LDX+1
      Y0=LDY
      DY=HDY-LDY+1

      kkk=1
      IF(IYAXIS.LT.0)kkk=-1
 
      CALL SETCOLOR(iblack)
      CALL TRAX(DX,X0,DY,Y0,kkk)
      fracX=FLOAT(HDX-LDX+1)/FLOAT(NX+1)
      fracY=FLOAT(HDY-LDY+1)/FLOAT(NY+1)
      MX1=LOX
      MX2=MX1+NX-1
      MY1=LOY+20
      MY2=MY1+NY-1
      
      IF(Idistype.EQ.-3)GO TO 33 !Very first display when starting mama

C In order to make the drawing of the matrix efficient,
C it is drawn in 3 ways depending on:
C 1. chX < chY and chX < pixelX: loop chX, loop pixelY
C 2. chY < chX and chY < pixelY: loop chY, loop pixelX
C 3. ch > pixels in both x an y direction: loop pixels
C These things are tested with respect to fracX and fracY

C Case 1
      IF(fracX.LT.fracY.AND.fracX.LT.1.)THEN
        DO j=MY1,MY2 
          lOld=0
          jj=LDY+(j-MY1)*fracY
          IF(jj.GT.HDY)jj=HDY
          CALL KTRAS(MX1,j,0)      !Going to pixel (MX1,j)
          DO ii=LDX,HDX            !Looping X-channels
            xx=rMAT(IDEST,ii,jj)
            DO l=0,nCol-2
              IF(xx.LT.Limit(l))GO TO 11
            ENDDO
            l=nCoL-1
   11       IF(l.NE.lOld)THEN
              CALL SETCOLOR(Color(lOld))
              i=MX1+((ii-LDX)/fracX)+0.5 !Calculating pixel for x
              CALL KTRAS(i,j,1)
              lOld=l
            ENDIF        
          ENDDO
          xx=rMAT(IDEST,HDX,jj)   !Ensures to draw line to edge
          DO l=0,nCol-2
            IF(xx.LT.Limit(l))GO TO 12
          ENDDO
          l=nCol-1
  12      CALL SETCOLOR(Color(lOld))                
          CALL KTRAS(MX2,j,1)
        ENDDO

        GO TO 33
      ENDIF

C Case 2
      IF(fracY.LT.fracX.AND.fracY.LT.1.)THEN
        DO i=MX1,MX2
          lOld=0
          ii=LDX+(i-MX1)*fracX
          IF(ii.GT.HDX)ii=HDX
          CALL KTRAS(i,MY1,0)      !Going to pixel (i,MY1)
          DO jj=LDY,HDY
            xx=rMAT(IDEST,ii,jj)
            DO l=0,nCol-2
              IF(xx.LT.Limit(l))GO TO 13
            ENDDO
            l=nCoL-1
   13       IF(l.NE.lOld)THEN
              CALL SETCOLOR(Color(lOld))
              j=MY1+((jj-LDY)/fracY)+0.5 !Calculating pixel for y
              CALL KTRAS(i,j,1)
              lOld=l
            ENDIF        
          ENDDO
          xx=rMAT(IDEST,ii,HDY)   !Ensures to draw line to edge
          DO l=0,nCol-2
            IF(xx.LT.Limit(l))GO TO 14
          ENDDO
          l=nCol-1
  14      CALL SETCOLOR(Color(lOld))                
          CALL KTRAS(i,MY2,1)
        ENDDO

        GO TO 33
      ENDIF

C Case 3, no other way than looping through all pixels NX*NY
      y=LDY-fracY
      DO j=MY1,MY2            
        lOld=0
        y=y+fracY
        jj=y+0.5
        CALL KTRAS(MX1,j,0)      !Going to pixel (MY1,j)
        x=LDX-fracX
        DO i=MX1,MX2
          x=x+fracX
          ii=x+0.5
          xx=rMAT(IDEST,ii,jj)
          DO l=0,nCol-2
            IF(xx.LT.Limit(l))GO TO 15
          ENDDO
          l=nCoL-1
   15     IF(l.NE.lOld)THEN
            CALL SETCOLOR(Color(lOld))
            CALL KTRAS(i,j,1)
            lOld=l
          ENDIF        
        ENDDO
        xx=rMAT(IDEST,HDX,jj)   !Ensures to draw line to edge
        DO l=0,nCol-2
          IF(xx.LT.Limit(l))GO TO 16
        ENDDO
        l=nCol-1
  16    CALL SETCOLOR(Color(lOld))                
        CALL KTRAS(MX2,j,1)
      ENDDO

C Making axes and headings
  33  CALL SETCOLOR(iblack)
      CALL TRAX(DX,X0,DY,Y0,kkk)
      CALL DATETIME(HEADING)
      IF(itext.EQ.1)THEN
        CALL MSPOT(MX2-60,LOY+NY+5)
        CALL PUTG(fname(1,IDEST)(1:8),8,1,1)
        CALL MSPOT(MX2-60,LOY+NY-6)
        CALL PUTG(HEADING(1:11),11,1,1)
      ENDIF
      DISP = .TRUE.

      IF(itext.EQ.0)GO TO 999
C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-8,1)
      CALL PUTG(UNITx,3,8,1)
C Units on y-axis
      CALL SETCOLOR(iwhite)                  ! Clear area to write in
      DO j=NY+5,NY+14
        CALL KTRAS(6,j,0)            
        CALL KTRAS(6+21,j,1)     
      ENDDO  
      CALL SETCOLOR(iblack)
      CALL MSPOT(7,NY+3)
      CALL PUTG(UNITy,3,2,1)

999   CALL FINIG
      RETURN
      END

