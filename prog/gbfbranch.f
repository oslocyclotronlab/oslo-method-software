      PROGRAM GBFbranch
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,DIM
      CHARACTER fname*8,comm*60
      CHARACTER APP*4,outfile*20,comment*60
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      REAL eg(0:4095),ec(0:4095),pg(0:4095),pc(0:4095),esg(0:4095),psg(0:4095)
      REAL Fi(0:4095), Ff(0:4095), alpha(0:4095), salpha(0:4095)
      REAL x(0:4095), sx(0:4095), y(0:4095), sy(0:4095), xy(0:4095), sxy(0:4095)
                REAL gammexe(0:4095),gammexp(0:4095)  ! e and p means elastic and pick-up
                REAL relepg(0:4095), srelepg(0:4095)
      WRITE(6,*)'     _____________________________________________'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |   GroundBandFeedingBranch 1.1 (GBFbranch)   |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |  Program to compare feeding to ground band  |'
      WRITE(6,*)'    |   from first generation spectra of elastic  |'
      WRITE(6,*)'    |       scattering and pick-up reactions      |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |          Oslo Cyclotron Laboratory          |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Created:  20/05 - 1999                      |'
      WRITE(6,*)'    | Modified: 30/12 - 2006                      |'
      WRITE(6,*)'    | Magne Guttormsen                            |'
      WRITE(6,*)'    |_____________________________________________|'
      WRITE(6,*)' '


C Initializing arrays
      DO i=0,4095
                  gammexe(i)   = 0.
                  gammexp(i)   = 0.
        eg(i)     = 0.
        ec(i)     = 0.
        pg(i)     = 0.
        pc(i)     = 0.
        esg(i)    = 0.
        psg(i)    = 0.
        Fi(i)     = 0.
        Ff(i)     = 0.
        x(i)      = 0.
        sx(i)     = 0.
        y(i)      = 0.
        sy(i)     = 0.
        xy(i)     = 0.
        sxy(i)    = 0.
        alpha(i)  = 0.
        salpha(i) = 0.
      ENDDO

                OPEN(20,FILE='gammex3.gex',ACCESS='SEQUENTIAL',ERR=99)
                DO i=0,4095
                   READ(20,*,END=888,ERR=888)gammexe(i)
                ENDDO
                CLOSE(20)
888   CONTINUE

                g3a0=60.
                g3a1=120.
      WRITE(6,1)g3a0
 1    FORMAT('Give calibration constant a0(keV) for gammex3()      <',F9.2,'>:',$)
      CALL READF(5,g3a0)
      WRITE(6,2)g3a1
 2    FORMAT('Give calibration constant a1(keV/ch) for gammex3()    <',F9.2,'>:',$)
      CALL READF(5,g3a1)

                OPEN(20,FILE='gammex4.gex',ACCESS='SEQUENTIAL',ERR=99)
                DO i=0,4095
                   READ(20,*,END=887,ERR=887)gammexp(i)
                ENDDO
                CLOSE(20)
887   CONTINUE

                g4a0=50.
                g4a1=120.
      WRITE(6,3)g4a0
 3    FORMAT('Give calibration constant a0(keV) for gammex4()      <',F9.2,'>:',$)
      CALL READF(5,g4a0)
      WRITE(6,4)g4a1
 4    FORMAT('Give calibration constant a1(keV/ch) for gammex4()    <',F9.2,'>:',$)
      CALL READF(5,g4a1)

C Reading xxx.gbf matrix from gbf-program
C First reading elastic scattering
      IDEST=1
      ITYPE=3
      WRITE(6,*)'Please, answer 1 and the name of your input xxx.gbf'
      WRITE(6,*)'matrix for elastic scattering'
      WRITE(6,*)'(created by gbf) in the two next questions... '
      WRITE(6,*)' '

      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      IF(bx.EQ.1.)THEN
        ea0=0.               !Defaults in case of no calibration
        ea1=120.
      ELSE
        ea0=cal(1,IDEST,1,1) !Picks the spectrum calibration
        ea1=cal(1,IDEST,1,2)
      ENDIF

      WRITE(6,8)ea0
 8    FORMAT('Give calibration constant a0(keV)       <',F9.2,'>:',$)
      CALL READF(5,ea0)
      WRITE(6,10)ea1
 10   FORMAT('Give calibration constant a1(keV/ch)    <',F9.2,'>:',$)
      CALL READF(5,ea1)

      DO i=0,XDIM-1
        ec(i) = rMAT(IDEST,i,0)    !c-function for elast. sc.
        eg(i) = rMAT(IDEST,i,1)    !g-function for elast. sc.
      ENDDO
      DIM = XDIM

C Then reading pick-up reaction
      IDEST=1
      ITYPE=3
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 1 and the name of your input xxx.gbf'
      WRITE(6,*)'matrix for pick-up reaction'
      WRITE(6,*)'(created by gbf) in the two next questions... '
      WRITE(6,*)' '

      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      IF(bx.EQ.1.)THEN
        pa0=0.               !Defaults in case of no calibration
        pa1=120.
      ELSE
        pa0=cal(1,IDEST,1,1) !Picks the spectrum calibration
        pa1=cal(1,IDEST,1,2)
      ENDIF

      WRITE(6,12)pa0
 12   FORMAT('Give calibration constant a0(keV)       <',F9.2,'>:',$)
      CALL READF(5,pa0)
      WRITE(6,14)pa1
 14   FORMAT('Give calibration constant a1(keV/ch)    <',F9.2,'>:',$)
      CALL READF(5,pa1)

      DO i=0,XDIM-1
        pc(i) = rMAT(IDEST,i,0)    !c-function for pick-up
        pg(i) = rMAT(IDEST,i,1)    !g-function for pick-up
      ENDDO

      IF(XDIM.GT.DIM)DIM = XDIM

      WRITE(6,*)' '
      WRITE(6,*)'You have to choose a common calibration for the '
      WRITE(6,*)'three input spectra'
      WRITE(6,*)' '
      IF(ABS(pa1).GT.ABS(ea1))THEN
        a0 = pa0
        a1 = pa1
      ELSE
        a0 = ea0
        a1 = ea1
      ENDIF
                a0=0.
                a1=120.
      WRITE(6,16)a0
 16   FORMAT('Give calibration constant a0(keV)       <',F9.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,18)a1
 18   FORMAT('Give calibration constant a1(keV/ch)    <',F9.2,'>:',$)
      CALL READF(5,a1)

      DO i=0,4095
        Fi(i)= gammexe(i)
      ENDDO
      CALL ELASTIC(Fi,Ff,g3a0,g3a1,a0,a1,4095,4095) !Modifies spectrum to give
      DO i=0,4095                                 !calibration a0 and a1
        gammexe(i)=Ff(i)
        IF(gammexe(i).NE.0.)DIM=MAX(DIM,i)
        Fi(i)=0.
      ENDDO

      DO i=0,4095
        Fi(i)= gammexp(i)
      ENDDO
      CALL ELASTIC(Fi,Ff,g4a0,g4a1,a0,a1,4095,4095) !Modifies spectrum to give
      DO i=0,4095                                 !calibration a0 and a1
        gammexp(i)=Ff(i)
        IF(gammexp(i).NE.0.)DIM=MAX(DIM,i)
        Fi(i)=0.
      ENDDO

      DO i=0,4095
        Fi(i)= eg(i)
      ENDDO
      CALL ELASTIC(Fi,Ff,ea0,ea1,a0,a1,4095,4095) !Modifies spectrum to give
      DO i=0,4095                                 !calibration a0 and a1
        eg(i)=Ff(i)
        IF(eg(i).NE.0.)DIM=MAX(DIM,i)
        Fi(i)=0.
      ENDDO

      DO i=0,4095
        Fi(i)= ec(i)
      ENDDO
      CALL ELASTIC(Fi,Ff,ea0,ea1,a0,a1,4095,4095) !Modifies spectrum to give
      DO i=0,4095                                 !calibration a0 and a1
        ec(i)=Ff(i)
        IF(ec(i).NE.0.)DIM=MAX(DIM,i)
        Fi(i)=0.
      ENDDO

      DO i=0,4095
        Fi(i)= pg(i)
      ENDDO
      CALL ELASTIC(Fi,Ff,pa0,pa1,a0,a1,4095,4095) !Modifies spectrum to give
      DO i=0,4095                                 !calibration a0 and a1
        pg(i)=Ff(i)
        IF(pg(i).NE.0.)DIM=MAX(DIM,i)
        Fi(i)=0.
      ENDDO

      DO i=0,4095
        Fi(i)= pc(i)
      ENDDO
      CALL ELASTIC(Fi,Ff,pa0,pa1,a0,a1,4095,4095) !Modifies spectrum to give
      DO i=0,4095                                 !calibration a0 and a1
        pc(i)=Ff(i)
        IF(pc(i).NE.0.)DIM=MAX(DIM,i)
        Fi(i)=0.
      ENDDO

C Assuming uncertainties depend on direct spectra only
C takes 5 and 5 channels and see how much it deviates
      DO i=2,DIM-3
        eave   = (eg(i-2)+eg(i-1)+eg(i)+eg(i+1)+eg(i+2))/5.
        esg(i) = ABS(eg(i)-eave)  !st. dev. g-function for elast. sc.
        pave   = (pg(i-2)+pg(i-1)+pg(i)+pg(i+1)+pg(i+2))/5.
        psg(i) = ABS(pg(i)-pave)  !st. dev. g-function for pick-up
      ENDDO
      esg(0)     = esg(2)
      esg(1)     = esg(2)
      esg(DIM-2) = esg(DIM-3)
      esg(DIM-1) = esg(DIM-3)
      psg(0)     = psg(2)
      psg(1)     = psg(2)
      psg(DIM-2) = psg(DIM-3)
      psg(DIM-1) = psg(DIM-3)

C Assumes sqrt(2*N) for Eg = ex < 2.5 MeV (implemented 10. April 2007)
                DO i=0,DIM-1
                        ex = a0 + a1*i
                        IF(ex.LT.2500.)THEN
                                counts=eg(i)
                                IF(counts.LT.4)counts=4.
                                esg(i)=SQRT(4.*counts)
                                counts=pg(i)
                                IF(counts.LT.4)counts=4.
                                psg(i)=SQRT(4.*counts)
                        ENDIF
                ENDDO

      DO i=1,DIM-2
        esg(i) = (esg(i-1)+esg(i)+esg(i+1))/3. !average of three channels
        psg(i) = (psg(i-1)+psg(i)+psg(i+1))/3. !average of three channels
      ENDDO
      esg(0)     = esg(1)
      esg(DIM-1) = esg(DIM-2)
      psg(0)     = psg(1)
      psg(DIM-1) = psg(DIM-2)

C Now, fasten seatbelts
      DO i=0,DIM-1
        x(i)  = 0.
        y(i)  = 0.
        sx(i) = 0.
        sy(i) = 0.
        xy(i) = 0.
        sxy(i)= 0.
        IF((eg(i)+ec(i)).GT.0)THEN
          x(i)  = eg(i) /(eg(i)+ec(i))
          sx(i) = esg(i)/(eg(i)+ec(i))
        ENDIF
                  IF((pg(i)+pc(i)).GT.0)THEN
          y(i)  = pg(i) /(pg(i)+pc(i))
          sy(i) = psg(i)/(pg(i)+pc(i))
        ENDIF

        IF(x(i).GT.0.AND.y(i).GT.0.)THEN
          xy(i)  = x(i)/y(i)
          sxy(i) = xy(i)*SQRT( (sx(i)/x(i))**2. + (sy(i)/y(i))**2. )
        ENDIF
        IF(x(i).GT.0.AND.y(i).GT.0.AND.(x(i).NE.y(i)))THEN
          alpha(i)  = (1.-y(i))/(y(i)-x(i))
          salpha(i) = (1./(y(i)-x(i))**2) * SQRT( ((1-x(i))*sy(i))**2+((y(i)-1)*sx(i))**2 )
        ENDIF
      ENDDO

C Writing to terminal
      WRITE(6,*)' '
      WRITE(6,*)' ch  Ex(keV)       eg       ec      esg       pg       pc      psg'
      DO i=0,DIM-1
        ex = a0 + a1*i
        WRITE(6,40)i,ex,eg(i),ec(i),esg(i),pg(i),pc(i),psg(i)
 40     FORMAT(I4,7F9.1)
      ENDDO

      WRITE(6,*)' '
      WRITE(6,*)' ch  Ex(keV)   x     sx    y     sy     x/y   sx/y   alpha salpha'
      DO i=0,DIM-1
        ex = a0 + a1*i
        WRITE(6,42)i,ex,x(i),sx(i),y(i),sy(i),xy(i),sxy(i),alpha(i),salpha(i)
 42     FORMAT(I4,F9.1,4F6.3,4F7.2)
      ENDDO

C Writting spectra to matrices

      cal(IDEST,1,1,1)=a0
      cal(IDEST,1,1,2)=a1
      cal(IDEST,1,1,3)=0.
      cal(IDEST,1,2,1)=0.
      cal(IDEST,1,2,2)=1.
      cal(IDEST,1,2,3)=0.
      XDIM=DIM
      YDIM=14

      DO i=0,XDIM-1
        rMAT(IDEST,i,0)  = eg(i)
        rMAT(IDEST,i,1)  = ec(i)
        rMAT(IDEST,i,2)  = esg(i)
        rMAT(IDEST,i,3)  = pg(i)
        rMAT(IDEST,i,4)  = pc(i)
        rMAT(IDEST,i,5)  = psg(i)
        rMAT(IDEST,i,6)  = x(i)
        rMAT(IDEST,i,7)  = sx(i)
        rMAT(IDEST,i,8)  = y(i)
        rMAT(IDEST,i,9)  = sy(i)
        rMAT(IDEST,i,10) = xy(i)
        rMAT(IDEST,i,11) = sxy(i)
        rMAT(IDEST,i,12) = alpha(i)
        rMAT(IDEST,i,13) = salpha(i)
      ENDDO

      outfile=fname(1,1)
      LIN=8
      DO i=1,8
        IF(outfile(i:i).EQ.'.'.OR.outfile(i:i).EQ.' ')THEN
          LIN=i-1
          GO TO 55
        ENDIF
      ENDDO
55    outfile=outfile(1:LIN)//'.bch'
      comment='eg ec esg pg pc psg x sx y sy y/x sy/x alf salf'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,72)outfile
72    FORMAT(' eg ec esg pg pc psg x sx y sy y/x sy/x alpha salpha to matrix: ' , A)


C Writting table (this is implemented in X-mas 2006)
      WRITE(6,*)' ch  Ex(keV)  gammexe   gammexp     xeg        seg       xec       sec       xpg       spg       xpc       spc'
                OPEN(20,FILE='gammex.bch',ACCESS='SEQUENTIAL',ERR=99)
                DO i=0,DIM-1
                   ex = (a0 + a1*FLOAT(i))/1000.
                        xeg=gammexe(i)*x(i)
                        seg=gammexe(i)*sx(i)
                        xec=gammexe(i)*(1.-x(i))
                        sec=gammexe(i)*sx(i)

                        xpg=gammexp(i)*y(i)
                        spg=gammexp(i)*sy(i)
                        xpc=gammexp(i)*(1.-y(i))
                        spc=gammexp(i)*sy(i)

                   WRITE(20,44) i, ex, gammexe(i),gammexp(i), xeg,seg,xec,sec, xpg,spg,xpc,spc
                        WRITE(6,44)  i, ex, gammexe(i),gammexp(i), xeg,seg,xec,sec, xpg,spg,xpc,spc
44       FORMAT(I4,F9.4,10E10.3)
         IF(ex.GT.9.)GO TO 777    !stops at Ex = 9 MeV
                ENDDO
777   CONTINUE
      WRITE(6,*)' ch  Ex(keV)  gammexe   gammexp     xeg        seg       xec       sec       xpg       spg       xpc       spc'
      CLOSE(20)

                WRITE(6,74)'gammex.bch'
74    FORMAT(' i, ex, gammex, xeg,seg,xec,sec, xpg,spg,xpc,spc to matrix: ' , A)


C Writting table (this is implemented in X-mas 2006)
      WRITE(6,*)' ch  Ex(keV)  relepg   srelepg'
                OPEN(20,FILE='gamrel.bch',ACCESS='SEQUENTIAL',ERR=99)
                DO i=0,DIM-1
                   ex = (a0 + a1*FLOAT(i))/1000.
                        xeg=gammexe(i)*x(i)
                        seg=gammexe(i)*sx(i)
                        xpg=gammexp(i)*y(i)
                        spg=gammexp(i)*sy(i)
                        relepg(i)=0
                        srelepg(i)=0
                        IF(xpg.GT.0)               relepg(i)=xeg/xpg
                        IF(xeg.GT.0.AND.xpg.GT.0) srelepg(i)=relepg(i)*SQRT( (seg/xeg)**2 + (spg/xpg)**2 )

                   WRITE(20,45) i, ex, relepg(i), srelepg(i)
                        WRITE(6,45)  i, ex, relepg(i), srelepg(i)
45       FORMAT(I4,F9.4,2E10.3)
         IF(ex.GT.9.)GO TO 778    !stops at Ex = 9 MeV
                ENDDO
778   CONTINUE
      WRITE(6,*)' ch  Ex(keV)  relepg   srelepg'
      CLOSE(20)

                WRITE(6,75)'gamrel.bch'
75    FORMAT(' i, ex, relepg, srelepg to matrix: ' , A)


      STOP

99    WRITE(6,*)'Could not open file for results and spectra'
      END



      SUBROUTINE ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,Di,Df)
C The most magnificant stretch- and compress-routine
C ever created by a human beeing. It is complicated, but works!
C The routine streches or compresses spectrum from initial
C calibration (A0i,A1i) to final (A0f,A1f). The dimensions
C of the real spectra are Di and Df. First channel is 0, so
C that the spectra occupy (0:D-1) channels.
C August 1994, Oslo Cyclotron Laboratory, Magne Guttormsen
      INTEGER Di,Df
      DIMENSION Fi(0:Di-1),Ff(0:Df-1)
C Testing
      IF(A1i.EQ.0.OR.A1f.EQ.0)RETURN
C Zeroing final spectrum
      DO i=0,Df-1
        Ff(i)=0.
      ENDDO

C Case where no action is taken
      IF(A0i.EQ.A0f.AND.A1i.EQ.A1f)THEN
        DO i=0,MIN0(Di-1,Df-1)
          Ff(i)=Fi(i)
        ENDDO
        RETURN
      ENDIF

C Taking counts in initial spectrum and find where
C to put it in final spectrum. Then it is distributed on
C the channel(s). The loop goes through all the
C channels i of the initial spectrum

      IF(ABS(A1i/A1f).LT.2)THEN
        DO i=0,Di-1
          IF(Fi(i).EQ.0)GO TO 99
          EiL=A0i+A1i*(i-0.5)      !Step 1.0 chs left and right
          EiH=A0i+A1i*(i+0.5)
          CHf1=(EiL-A0f)/A1f       !CHf1 and CHf2 define limits where
          CHf2=(EiH-A0f)/A1f       !to put the counts in final spc.
          CHlength=ABS(CHf1-CHf2)  !Number of channels (float)
          CountCH=Fi(i)/CHlength   !Number of counts pr.ch unit
          CHfL=CHf1
          CHfH=CHf2
          IF(CHfL.GT.CHfH)THEN
            CHfL=CHf2
            CHfH=CHf1
          ENDIF
          j1=CHfL+0.5
          j2=CHfH+0.5              !filling with CHwidth*CountCH
          IF(j1.GE.Df.OR.j2.LT.0)GO TO 99
          nch=j2-j1+1
          IF(nch.EQ.1)THEN         !One channel to fill
            IF(j1.GE.0)Ff(j1)=Ff(j1)+Fi(i)
            GO TO 99
          ENDIF
          IF(nch.GT.1)THEN  !Two or three channels to fill
            Counts=CountCH*(j1+0.5-CHfL) !Fraction of left channel
            IF(j1.GE.0)Ff(j1)=Ff(j1)+Counts
            Counts=CountCH*(CHfH+0.5-j2) !Fraction of right channel
            IF(j2.LE.Df-1)Ff(j2)=Ff(j2)+Counts
            DO j=j1+1,j2-1           !Filling in for whole chs.
              IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+CountCH
            ENDDO
          ENDIF
  99      CONTINUE

        ENDDO
      ELSE

C The counts will be distributed in the streChing procedure as a triangle,
C which has its left and right tail overlaping with the center of the
C next triangle. The heighth and basis of the triangle is called h and b:
C               x         x          x
C             x   x     x   x      x   x
C           x       x x       x   x      x
C         x          0          0          x
C       x          x   x      x  x           x
C     x          x       x  x      x           x
C   x          x           0         x           x

        b=2.0*A1i/A1f   !basis of triangle
        h=2.0/b         !height of triangle in order to get area=1
        alpha=h/(b/2.)  !slope of triangle tails
        DO i=0,Di-1
          IF(Fi(i).EQ.0)GO TO 98
          EiL=A0i+A1i*(i-1.)      !Step 1.0 chs left and right
          EiH=A0i+A1i*(i+1.)
          CHf1=(EiL-A0f)/A1f      !CHf1 and CHf2 define limits where
          CHf2=(EiH-A0f)/A1f      !to put the counts in final spc.
          CHfL=CHf1
          CHfH=CHf2
          IF(CHfL.GT.CHfH)THEN
            CHfL=CHf2
            CHfH=CHf1
          ENDIF
          j1=CHfL+1
          j2=CHfH
          IF(j1.GE.Df.OR.j2.LT.0)GO TO 98
          w=0.
          DO j=j1,j2
            IF(j.LT.CHfL+(b/2.))THEN
              w=alpha*(j-CHfL)                !up going slope
            ELSE
              w=h-alpha*(j-(CHfL+(b/2.)))     !down going slope
            ENDIF
            IF(w.LT.-0.1)WRITE(6,*)'Warning, weight w < 0 : ',w
            IF(w.LT.0)w=0.
            IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+w*Fi(i)
          ENDDO
  98      CONTINUE
        ENDDO
      ENDIF
      END


