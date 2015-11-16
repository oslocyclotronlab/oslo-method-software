      SUBROUTINE DSPMA(icount,iScaleChange)
C Routine to display 2-dimensional spectra (matrices)
C M.Guttormsen november 1993, october 1995 (for update)
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      INTEGER XDIM,YDIM
      CHARACTER APP*4, TEX*8
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      COMMON/OL/I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                         OLlow,OLhigh,OLlocnt,OLhicnt

      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER*28 HEADING
      INTEGER COLORMAP(20),Limit(0:19),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

      iwhite=19
      iblack=20

C Setting up 14 colors for 3D-plot
      nCol=14

      IF(iScaleChange.EQ.1)THEN     !first call or growing counts of 2 times
        IF(HDZ.GE.0)THEN            !sign of HDZ used to determine log or manual plot 
          LDZ=1                     !Finding LDZ,HDZ
          HDZ=10
          do j=LDY,HDY
            do i=LDX,HDX
              IF(MAT(i,j).GT.HDZ)HDZ=MAT(i,j)
            enddo
          enddo
          IF(HDZ.GT.50 )LDZ=2
          IF(HDZ.GT.500)LDZ=4
          Cmin = 1 !IABS(LDZ)           Logaritmic display
          Tot  = IABS(2*HDZ)
          DeltaTot=Tot-Cmin+0.5
          IF(DeltaTot.LT.1)DeltaTot=10
          basis=EXP((ALOG(DeltaTot))/(2.*(nCol-3)))    
          Limit(1)=Cmin
          DO i=2,ncol-2
            Limit(i)=Cmin+basis**(2.*(i-1.))
          ENDDO       
          Limit(0)=-Limit(1)
        ELSE                        !manual level-values as input
          do i=0,15
            Limit(i)=2*Limit(i)
          enddo
        ENDIF
      ENDIF

      CALL INITG(NX,NY)                          !Finding window size (nx,ny)
      IF(NX.NE.NXold.OR.NY.NE.NYold.OR.iScaleChange.NE.0)THEN
        CALL ERASE  !Clear window if new pixel witdhs
        NXold=NX
        NYold=NY
C Setting up color pallett of 'ncol' colors 
C Pixel basis is 'is'
C Left lower pixel-origo is (mx,my)
C Checks first if all colors have place  
        ibig =0              !If big numbers, only every second is written
        is   =13
        IF(ncol.GE.2)THEN    !Draw pallett - if more than 2 colors
          mx1  =nx-22-(ncol-1)*is-ncol*is
          ihigh=10
          my1  =ny+42-ihigh 
          DO l=0,ncol-1
            CALL SETCOLOR(Color(l))
            mxx=mx1+l*2*is
            DO j=my1,my1+ihigh
              CALL KTRAS(mxx,j,0)            
              CALL KTRAS(mxx+is,j,1)     
            ENDDO  
            IF(Color(l).EQ.19)THEN    !Case of white color  
              CALL SETCOLOR(iblack)   
              CALL KTRAS(mxx   ,my1      ,0)
              CALL KTRAS(mxx+is,my1      ,1)
              CALL KTRAS(mxx+is,my1+ihigh,1)
              CALL KTRAS(mxx   ,my1+ihigh,1)
              CALL KTRAS(mxx   ,my1      ,1)
            ENDIF
            IF(l.LT.ncol-1)THEN
              IF(Limit(l).GE.10000)ibig=ibig+1
              IF(ibig.EQ.0.OR.((ibig+1)/2)*2.EQ.ibig+1)THEN
                CALL SETCOLOR(iblack)
                CALL KTRAS(mxx+is,my1-ihigh-2,0)
                WRITE(TEX,160)Limit(l)
  160           FORMAT(I8)
                CALL PUTG(TEX,8,1)
              ENDIF
            ENDIF
          ENDDO 
        ENDIF
      ENDIF   

      LOY=0
      LOX=0
      CALL LIMG(NX,LOX,NY,LOY)                    !Informing screen window


C Gets window parameters for use for G95 routines through
C COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
      CALL GETGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)  

      X0=LDX
      DX=HDX-LDX+1
      Y0=LDY
      DY=HDY-LDY+1
      IYAXIS=1

      CALL SETCOLOR(iblack)
      CALL TRAX(DX,X0,DY,Y0,IYAXIS)
    
      fracX=FLOAT(HDX-LDX+1)/FLOAT(NX+1)
      fracY=FLOAT(HDY-LDY+1)/FLOAT(NY+1)

      MX1=LOX
      MX2=MX1+NX-1
      MY1=LOY+20
      MY2=MY1+NY-1
      icount=0
      do j=LDY,HDY
        do i=LDX,HDX
          icount=icount+MAT(i,j)
        enddo
      enddo

C In order to make the drawing of the matrix efficient,
C it is drawn in 3 ways depending on:
C Case: 1. chX < chY and chX < pixelX: loop chX, loop pixelY
C Case: 2. chY < chX and chY < pixelY: loop chY, loop pixelX
C Case: 3. ch > pixels in both x an y direction: loop pixels
C These things are tested with respect to fracX and fracY

C Case 1
      IF(fracX.LT.fracY.AND.fracX.LT.1.)THEN
        DO j=MY1,MY2 
          lOld=0
          jj=LDY+(j-MY1)*fracY
          IF(jj.GT.HDY)jj=HDY
          CALL KTRAS(MX1,j,0)            !Going to pixel (MX1,j)
          DO ii=LDX,HDX                  !Looping X-channels
            k=MAT(ii,jj)
            DO l=0,nCol-2
              IF(k.LT.Limit(l))GO TO 11
            ENDDO
            l=nCoL-1
  11        IF(l.NE.lOld)THEN
              CALL SETCOLOR(Color(lOld))
              i=MX1+((ii-LDX)/fracX)+0.5 !Calculating pixel for x
              CALL KTRAS(i,j,1)
              lOld=l
            ENDIF        
          ENDDO
          k=MAT(HDX,jj)                  !Ensures to draw line to edge
          DO l=0,nCol-2
            IF(k.LT.Limit(l))GO TO 12
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
          CALL KTRAS(i,MY1,0)            !Going to pixel (i,MY1)
          DO jj=LDY,HDY
            k=MAT(ii,jj)
            DO l=0,nCol-2
              IF(k.LT.Limit(l))GO TO 13
            ENDDO
            l=nCoL-1
  13        IF(l.NE.lOld)THEN
              CALL SETCOLOR(Color(lOld))
              j=MY1+((jj-LDY)/fracY)+0.5 !Calculating pixel for y
              CALL KTRAS(i,j,1)
              lOld=l
            ENDIF        
          ENDDO
          k=MAT(ii,HDY)                  !Ensures to draw line to edge
          DO l=0,nCol-2
            IF(k.LT.Limit(l))GO TO 14
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
        CALL KTRAS(MX1,j,0)               !Going to pixel (MY1,j)
        x=LDX-fracX
        DO i=MX1,MX2
          x=x+fracX
          ii=x+0.5
          k=MAT(ii,jj)
          DO l=0,nCol-2
            IF(k.LT.Limit(l))GO TO 15
          ENDDO
          l=nCoL-1
  15      IF(l.NE.lOld)THEN
            CALL SETCOLOR(Color(lOld))
            CALL KTRAS(i,j,1)
            lOld=l
          ENDIF        
        ENDDO
        k=MAT(HDX,jj)                      !Ensures to draw line to edge
        DO l=0,nCol-2
          IF(k.LT.Limit(l))GO TO 16
        ENDDO
        l=nCol-1
  16    CALL SETCOLOR(Color(lOld))                
        CALL KTRAS(MX2,j,1)
      ENDDO

C Making axes and headings
c  33  CALL SETCOLOR(iblack)
c      CALL TRAX(DX,X0,DY,Y0,IYAXIS)
c      CALL DATETIME(HEADING)
c      IF(itext.EQ.1)THEN
c        CALL MSPOT(MX2-60,LOY+NY+5)
c        CALL PUTG(fname(1,IDEST)(1:8),8,1,1)
c        CALL MSPOT(MX2-60,LOY+NY-6)
c        CALL PUTG(HEADING(1:11),11,1,1)
c      ENDIF
c      DISP = .TRUE.

C Making axes and headings
  33  CALL SETCOLOR(iblack)
      CALL TRAX(DX,X0,DY,Y0,IYAXIS)
      CALL DATETIME(HEADING)
C Finding date text to write   
      n1=0
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
        CALL MSPOT(MX2-60,LOY+NY+5)
        CALL PUTG(fname(1,IDEST)(1:8),8,1)
        CALL MSPOT(MX2-60,LOY+NY-6)
        CALL PUTG(HEADING(1:n1-1)//HEADING(n2+1:n3+3),numb,1)
      ENDIF
      DISP = .TRUE.



C Putting on unit on axis
      CALL INITG(NX,NY)
C Units on x-axis
      CALL MSPOT(NX-4,-2)
      CALL PUTG(UNITx,3,8)
C Units on y-axis
      CALL SETCOLOR(iwhite)                    !Clear area to write in
      DO j=NY+5,NY+14
        CALL KTRAS(6,j,0)            
        CALL KTRAS(6+21,j,1)     
      ENDDO  
      CALL SETCOLOR(iblack)
      CALL MSPOT(7,NY+3)
      CALL PUTG(UNITy,3,2)

      RETURN
      END

