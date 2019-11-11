      SUBROUTINE CLEANUP
C Displays icons in graphic window, with red boarder around active (=IDEST) spectrum
C Tests parameters from last command
  
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT

      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      REAL*8       GAIN(6)                !GF2
      INTEGER      ICAL,NTERMS            !GF2
      COMMON/CALIB/GAIN,ICAL,NTERMS       !GF2

      INTEGER COLORMAP(20),Colorc(0:19)
      REAL Limit(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      COMMON/memory/Lastdest,Lasttype,LastXDIM,LastYDIM,LastMAXCH,calLast(2,2,2,3)
      

C Tests first if something went wrong. 
C Then return to GFEXEC subroutine
C                   Istatus=0 :OK
C                   Istatus=1 :wrong type of input parameter
C                   Istatus=2 :unphysical input parameter
C                   Istatus=3 :illegal answer
C                   Istatus=-7:flags that green button is active

C Setting up colors that are not to be changed
      igreen     =4
      ilightgreen=6
      isand      =9
      ired       =10
      iwhite     =19
      iblack     =20

      Iactive=0                    ! passiv green button
      IF(Istatus.EQ.-7)THEN
        Iactive=1                  !green button is active
        Istatus=0
      ENDIF
       
      IF(IDEST.LT.1.OR.IDEST.GT.2)THEN
        WRITE(6,*)'Detected illegal value for destination',
     1' spectrum. Corrected to IDEST=2'
        IDEST=2
      ENDIF

      IF(ITYPE.LT.1.OR.ITYPE.GT.3)THEN
        WRITE(6,*)'Detected illegal value for spectrum',
     1' type. Corrected to ITYPE=3'
        ITYPE=3
      ENDIF

      IF(Istatus.NE.0)WRITE(6,*)CHAR(7)
      IF(Istatus.EQ.1)THEN
        WRITE(6,*)'wrong type of input parameter'
        Istatus=0
        IDEST=Lastdest
        ITYPE=Lasttype
        XDIM=LastXDIM
        YDIM=LastYDIM
        MAXCH=LastMAXCH
        DO i=1,2
          DO j=1,2
            DO k=1,2
              DO l=1,3
                cal(i,j,k,l)=calLast(i,j,k,l)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        RETURN
      ENDIF
      
      IF(Istatus.EQ.2)THEN
        WRITE(6,*)'unphysical input parameter'
        Istatus=0
        IDEST=Lastdest
        ITYPE=Lasttype
        XDIM=LastXDIM
        YDIM=LastYDIM
        MAXCH=LastMAXCH
        DO i=1,2
          DO j=1,2
            DO k=1,2
              DO l=1,3
                cal(i,j,k,l)=calLast(i,j,k,l)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        RETURN
      ENDIF
      
      IF(Istatus.EQ.3)THEN
        WRITE(6,*)'illegal answer'
        Istatus=0
        IDEST=Lastdest
        ITYPE=Lasttype
        RETURN
      ENDIF

      Istatus=0

C Tests if dimensions are OK
      IF(ITYPE.GT.1)THEN              !Matrix
        IF(XDIM.GT.4096)XDIM=4096
        IF(YDIM.GT.2048 )YDIM=2048
        Idim(1,IDEST,1)=XDIM
        Idim(1,IDEST,2)=YDIM
      ENDIF

      IF(ITYPE.EQ.1)THEN              !Singles spectrum
        IF(MAXCH.LE.0   )MAXCH=1
        IF(MAXCH.GT.8191)MAXCH=8191
        Idim(2,IDEST,1)=MAXCH+1
      ENDIF


C If the matr./spc. is changed we resets to new display markers
      Lastdest=IDEST !Remember the last IDEST and ITYPE 
      Lasttype=ITYPE
      LastXDIM=XDIM
      LastYDIM=YDIM
      LastMAXCH=MAXCH
      DO i=1,2
        DO j=1,2
          DO k=1,2
            DO l=1,3
              calLast(i,j,k,l)=cal(i,j,k,l)
            ENDDO
          ENDDO
        ENDDO
      ENDDO


C Test if crazy calibration of a2, and resets to zero
      IF(ITYPE.GT.1)THEN
        IF(cal(1,IDEST,1,3).GT.1.E+14)cal(1,IDEST,1,3)=0.
        IF(cal(1,IDEST,2,3).GT.1.E+14)cal(1,IDEST,2,3)=0.
      ELSE
        IF(cal(2,IDEST,1,3).GT.1.E+14)cal(2,IDEST,1,3)=0.
      ENDIF
C Test if a0=a1=a2=0, put a1=1.
      IF(ITYPE.GT.1)THEN
        xcal=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
        IF(xcal.EQ.0.)cal(1,IDEST,1,2)=1.
        ycal=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
        IF(ycal.EQ.0.)cal(1,IDEST,2,2)=1.
      ELSE
        xcal=cal(2,IDEST,1,1)+cal(2,IDEST,1,2)+cal(2,IDEST,1,3)
        IF(xcal.EQ.0.)cal(2,IDEST,1,2)=1.
      ENDIF

C Put into display-spectrum if singles spectrum
      IF(ITYPE.EQ.1)THEN
        GAIN(1)=cal(2,IDEST,1,1)
        GAIN(2)=cal(2,IDEST,1,2)
        GAIN(3)=cal(2,IDEST,1,3)
        ICAL=1
        NTERMS=3       
      ENDIF

C Setting up colors for 2-dim landscape
      nCol=14

      c1    = 0.001
      ctop  = 10000.
      f1    = log(c1)
      ftop  = log(ctop)
      df    = ABS(ftop-f1)/FLOAT(nCol-3)
      DO i=1,nCol-2
        fi=f1+df*FLOAT(i-1)
        Limit(i)=exp(fi)
      ENDDO       
      Limit(0)=-Limit(1)


C Making the drawing in left upper corner:
C  I-----I I-----I                             my1  my11 
C  I     I I     I I--------I I--------I       my2
C  I-----I I-----I I--------I I--------I       my3
C   area    area     area       area           my4
C
C mx1   mx2 mx3 mx4 mx5    mx6 mx7   mx8 mx9 mx10


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
      mx10=nx-4
      mx9=mx10-13

      my1=ny+42
      my2=my1-0.7*is
      my3=my2-0.3*is
      my11=my1-4


C Curser click exit point on green button
      ic1=igreen
      ic2=ilightgreen
      ic3=ilightgreen                  !passive green button
      IF(Iactive.EQ.1)THEN
        ic1=iblack
        ic2=igreen
        ic3=ilightgreen                !active green button
      ENDIF

      CALL SETCOLOR(ic2)
      DO j=my3+1,my11-3
        CALL KTRAS(mx9+4,j,0)
        CALL KTRAS(mx10+1,j,1)
      ENDDO

      CALL SETCOLOR(ic1)
      CALL KTRAS(mx9+2 ,my3   ,0)           !    -------    my11-2
      CALL KTRAS(mx10+2,my3   ,1)           !    |green |
      CALL KTRAS(mx10+2,my11-1,1)           !    |      |
                                            !    -------    my3+1
      CALL KTRAS(mx9+1 ,my3 -1,0)           !  mx9+3   mx10
      CALL KTRAS(mx10+3,my3 -1,1)              
      CALL KTRAS(mx10+3,my11  ,1)

      CALL SETCOLOR(ic3)
      CALL KTRAS(mx9+2 ,my3   ,0)            
      CALL KTRAS(mx9+2 ,my11-1,1)          
      CALL KTRAS(mx10+2,my11-1,1)  

      CALL KTRAS(mx9+1 ,my3 -1,0)
      CALL KTRAS(mx9+1 ,my11  ,1)
      CALL KTRAS(mx10+3,my11  ,1)

C Drawing the four matr./spec. icons
      LOY=0
      LOX=0

      CALL LIMG(NX,LOX,NY,LOY) !Informing screen window
    
C First matrix drawn
      fracX=FLOAT((Idim(1,1,1)-1))/FLOAT(mx2-mx1)
      fracY=FLOAT((Idim(1,1,2)-1))/FLOAT(my1-my3)
      y=-1.
      DO j=my3,my1        !Looping through all pixels NX*NY
        lOld=0
        CALL SETCOLOR(Colorc(lOld))        
        y=y+1.
        jj=y*fracY+0.5
        CALL KTRAS(mx1,j,0)      !Going to pixel (mx1,j)
        x=-1.
        DO i=mx1,mx2
          x=x+1.
          ii=x*fracX+0.5
          IF(ii.GT.4095.OR.jj.GT.2047)THEN
             write(6,*)'Warning from cleanup.f: ii,jj=',ii,jj
          ELSE
            xx=rMAT(1,ii,jj)
          ENDIF
          DO l=0,nCol-2
            IF(xx.LT.Limit(l))GO TO 11
          ENDDO
          l=nCol-1
          IF(ii.GE.Idim(1,1,1).AND.jj.GE.Idim(1,1,2))lOld=0
   11     IF(l.NE.lOld)THEN
            CALL SETCOLOR(Colorc(lOld))
            CALL KTRAS(i,j,1)     !Drawing line to pixel (i,j)
            lOld=l
          ENDIF
        ENDDO
        xx=rMAT(1,Idim(1,1,1)-1,jj)   !Ensures to draw line to edge
        DO l=0,nCol-2
          IF(xx.LT.Limit(l))GO TO 12
        ENDDO
        l=nCol-2
  12    CALL SETCOLOR(Colorc(lOld))                
        CALL KTRAS(mx2,j,1)      
      ENDDO

      ICOL=isand                     !Drawing frame
      IF(IDEST.EQ.1.AND.ITYPE.GT.1)ICOL=ired
      CALL SETCOLOR(ICOL)
      CALL KTRAS(mx1-1,my1+1,0)
      CALL KTRAS(mx2+1,my1+1,1)
      CALL KTRAS(mx2+1,my3-1,1)
      CALL KTRAS(mx1-1,my3-1,1)
      CALL KTRAS(mx1-1,my1+1,1)
      CALL KTRAS(mx1,my1,0)
      CALL KTRAS(mx2,my1,1)
      CALL KTRAS(mx2,my3,1)
      CALL KTRAS(mx1,my3,1)
      CALL KTRAS(mx1,my1,1)

C Showing the markers if active spectrum
      IF(ICOL.EQ.ired)THEN
        CALL SETCOLOR(iblack) 
        mmx1=mx1+((LDX)/fracX)+0.5 
        mmx2=mx1+((HDX)/fracX)+0.5
        mmy1=my3+((LDY)/fracY)+0.5
        mmy2=my3+((HDY)/fracY)+0.5
        IF(mmx1.GT.mx2.OR.mmy1.GT.my1)GO TO 21
        IF(mmx2.GT.mx2)mmx2=mx2+2
        IF(mmy2.GT.my1)mmy2=my1+2
        IF(mmx1.eq.mx1.and.mmx2.eq.mx2.and.mmy1.eq.my3.and.mmy2.eq.my1)THEN
          GO TO 21
        ELSE
          CALL KTRAS(mmx1,mmy1,0)
          CALL KTRAS(mmx1,mmy2,1)
          IF(mmy2.gt.my1)THEN
            CALL KTRAS(mmx2,mmy2,0)
          ELSE
            CALL KTRAS(mmx2,mmy2,1)
          ENDIF
          IF(mmx2.gt.mx2)THEN
            CALL KTRAS(mmx2,mmy1,0)
          ELSE
            CALL KTRAS(mmx2,mmy1,1)
          ENDIF
          CALL KTRAS(mmx1,mmy1,1)
        ENDIF
      ENDIF
 21   CONTINUE


C Second matrix drawn
      fracX=FLOAT((Idim(1,2,1)-1))/FLOAT(mx4-mx3)
      fracY=FLOAT((Idim(1,2,2)-1))/FLOAT(my1-my3)
      y=-1.
      DO j=my3,my1
        lOld=0
        CALL SETCOLOR(Colorc(lOld))                  
        y=y+1.
        jj=y*fracY+0.5
        CALL KTRAS(mx3,j,0)   
        x=-1.
        DO i=mx3,mx4
          x=x+1.
          ii=x*fracX+0.5
          xx=rMAT(2,ii,jj)
          DO l=0,nCol-2
            IF(xx.LT.Limit(l))GO TO 13
          ENDDO
          l=nCol-1
          IF(ii.GE.Idim(1,2,1).AND.jj.GE.Idim(1,2,2))lOld=0
   13     IF(l.NE.lOld)THEN
            CALL SETCOLOR(Colorc(lOld))
            CALL KTRAS(i,j,1)
            lOld=l
          ENDIF
        ENDDO         
        xx=rMAT(2,Idim(1,2,1)-1,jj)  !Ensures to draw line to edge
        DO l=0,nCol-2
          IF(xx.LT.Limit(l))GO TO 14
        ENDDO
        l=nCol-1
  14    CALL SETCOLOR(Colorc(lOld))                
        CALL KTRAS(mx4,j,1)      
      ENDDO

      ICOL=isand                !Drawing frame
      IF(IDEST.EQ.2.AND.ITYPE.GT.1)ICOL=ired
      CALL SETCOLOR(ICOL)
      CALL KTRAS(mx3-1,my1+1,0)
      CALL KTRAS(mx4+1,my1+1,1)
      CALL KTRAS(mx4+1,my3-1,1)
      CALL KTRAS(mx3-1,my3-1,1)
      CALL KTRAS(mx3-1,my1+1,1)
      CALL KTRAS(mx3,my1,0)
      CALL KTRAS(mx4,my1,1)
      CALL KTRAS(mx4,my3,1)
      CALL KTRAS(mx3,my3,1)
      CALL KTRAS(mx3,my1,1)

C Showing the markers if active spectrum
      IF(ICOL.EQ.ired)THEN
        CALL SETCOLOR(iblack)
        mmx1=mx3+((LDX)/fracX)+0.5 
        mmx2=mx3+((HDX)/fracX)+0.5
        mmy1=my3+((LDY)/fracY)+0.5
        mmy2=my3+((HDY)/fracY)+0.5
        IF(mmx1.GT.mx4.OR.mmy1.GT.my1)GO TO 22
        IF(mmx2.GT.mx4)mmx2=mx4+2
        IF(mmy2.GT.my1)mmy2=my1+2
        IF(mmx1.eq.mx3.and.mmx2.eq.mx4.and.mmy1.eq.my3.and.mmy2.eq.my1)THEN
          GO TO 22
        ELSE
          CALL KTRAS(mmx1,mmy1,0)
          CALL KTRAS(mmx1,mmy2,1)
          IF(mmy2.gt.my1)THEN
             CALL KTRAS(mmx2,mmy2,0)
          ELSE
            CALL KTRAS(mmx2,mmy2,1)
          ENDIF
          IF(mmx2.gt.mx4)THEN
            CALL KTRAS(mmx2,mmy1,0)
          ELSE
            CALL KTRAS(mmx2,mmy1,1)
          ENDIF
          CALL KTRAS(mmx1,mmy1,1)
        ENDIF
      ENDIF
 22   CONTINUE

C First spectrum
      fracX=FLOAT((Idim(2,1,1)-1))/FLOAT(mx6-mx5)
      DO j=my3,my2
        lOld=0
        CALL SETCOLOR(Colorc(lOld))       
        CALL KTRAS(mx5,j,0)  
        x=-1.
        DO i=mx5,mx6
          x=x+1.
          ii=x*fracX+0.5
          xx=rSPEC(1,ii)
          DO l=0,ncol-2
            IF(xx.LT.Limit(l))GO TO 15
          ENDDO
          l=nCol-1          
          IF(ii.GE.Idim(2,1,1)-1)lOld=0
   15     IF(l.NE.lOld)THEN
            CALL SETCOLOR(Colorc(lOld))
            CALL KTRAS(i,j,1)
            lOld=l
          ENDIF     
        ENDDO
        xx=rSPEC(1,Idim(2,1,1)-1)   !Ensures to draw line to edge
        DO l=0,nCol-2
          IF(xx.LT.Limit(l))GO TO 16
        ENDDO
        l=nCol-1
  16    CALL SETCOLOR(Colorc(lOld))                      
        CALL KTRAS(mx6,j,1)
      ENDDO
C Take away old black markers
      CALL SETCOLOR(iwhite)
      DO i=my3+8,my2+3
        CALL KTRAS(mx5-2,i,0)
        CALL KTRAS(mx6+3,i,1)
      ENDDO
C Frame around icon
      ICOL=isand      
      IF(IDEST.EQ.1.AND.ITYPE.EQ.1)ICOL=ired
      CALL SETCOLOR(ICOL)
      CALL KTRAS(mx5-1,my2+1,0)
      CALL KTRAS(mx6+1,my2+1,1)
      CALL KTRAS(mx6+1,my3-1,1)
      CALL KTRAS(mx5-1,my3-1,1)
      CALL KTRAS(mx5-1,my2+1,1)
      CALL KTRAS(mx5,my2,0)
      CALL KTRAS(mx6,my2,1)
      CALL KTRAS(mx6,my3,1)
      CALL KTRAS(mx5,my3,1)
      CALL KTRAS(mx5,my2,1)

C Showing the markers if active spectrum
      IF(ICOL.EQ.ired)THEN
        CALL SETCOLOR(iblack)
        mmx1=mx5+((LOCH)/fracX)+0.5 
        mmx2=mx5+((HICH)/fracX)+0.5
        mmy1=my2+5
        mmy2=my3+6
        IF(mmx1.gt.mx6.and.mmx2.gt.mx6)THEN
          GO TO 23
        ENDIF
        IF(mmx1.eq.mx5.and.mmx2.eq.mx6)THEN
          GO TO 23
        ELSE
          CALL KTRAS(mmx1,mmy2,0)
          CALL KTRAS(mmx1,mmy1,1)
          IF(mmx2.gt.mx6)THEN
            mmx2=mx6+2
            CALL KTRAS(mmx2,mmy1,1)
          ELSE
            CALL KTRAS(mmx2,mmy1,1)
            CALL KTRAS(mmx2,mmy2,1)
          ENDIF
        ENDIF
      ENDIF
  23  CONTINUE


C Second spectrum 
      fracX=FLOAT((Idim(2,2,1)-1))/FLOAT(mx8-mx7)
      DO j=my3,my2
        lOld=0
        CALL SETCOLOR(Colorc(lOld))                 
        CALL KTRAS(mx7,j,0)    
        x=-1
        DO i=mx7,mx8
          x=x+1.
          ii=x*fracX+0.5
          xx=rSPEC(2,ii)
          DO l=0,ncol-2
            IF(xx.LT.Limit(l))GO TO 17
          ENDDO
          l=nCol-1
          IF(ii.GE.Idim(2,2,1)-1)lOld=0
   17     IF(l.NE.lOld)THEN
            CALL SETCOLOR(Colorc(lOld))
            CALL KTRAS(i,j,1)
            lOld=l
          ENDIF
        ENDDO 
        xx=rSPEC(2,Idim(2,2,1)-1)   !Ensures to draw line to edge
        DO l=0,nCol-2
          IF(xx.LT.Limit(l))GO TO 18
        ENDDO
        l=nCol-1
  18    CALL SETCOLOR(Colorc(lOld))                      
        CALL KTRAS(mx8,j,1)   
      ENDDO
C Take away old black markers
      CALL SETCOLOR(iwhite)
      DO i=my3+8,my2+3
        CALL KTRAS(mx7-3,i,0)
        CALL KTRAS(mx8+2,i,1)
      ENDDO
C Frame around icon
      ICOL=isand          
      IF(IDEST.EQ.2.AND.ITYPE.EQ.1)ICOL=ired
      CALL SETCOLOR(ICOL)
      CALL KTRAS(mx7-1,my2+1,0)
      CALL KTRAS(mx8+1,my2+1,1)
      CALL KTRAS(mx8+1,my3-1,1)
      CALL KTRAS(mx7-1,my3-1,1)
      CALL KTRAS(mx7-1,my2+1,1)
      CALL KTRAS(mx7,my2,0)
      CALL KTRAS(mx8,my2,1)
      CALL KTRAS(mx8,my3,1)
      CALL KTRAS(mx7,my3,1)
      CALL KTRAS(mx7,my2,1)

C Showing the markers if active spectrum
      IF(ICOL.EQ.ired)THEN
        CALL SETCOLOR(iblack) 
        mmx1=mx7+((LOCH)/fracX)+0.5 
        mmx2=mx7+((HICH)/fracX)+0.5
        mmy1=my2+5
        mmy2=my3+6
        IF(mmx1.gt.mx8.and.mmx2.gt.mx8)THEN
          GO TO 24
        ENDIF
        IF(mmx1.eq.mx7.and.mmx2.eq.mx8)THEN
          GO TO 24
        ELSE
          CALL KTRAS(mmx1,mmy2,0)
          CALL KTRAS(mmx1,mmy1,1)
          IF(mmx2.gt.mx8)THEN
            mmx2=mx8+2
            CALL KTRAS(mmx2,mmy1,1)
          ELSE
            CALL KTRAS(mmx2,mmy1,1)
            CALL KTRAS(mmx2,mmy2,1)
          ENDIF
        ENDIF
      ENDIF
  24  CONTINUE

C Drawing black line below the icons

      CALL SETCOLOR(iblack)
      CALL KTRAS(mx10+5,my3-3,0)
      CALL KTRAS(mx1 -2,my3-3,1)
      CALL FINIG
      DISP = .TRUE.
      RETURN

      END
