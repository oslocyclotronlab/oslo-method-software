      PROGRAM Tave1_0
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
      COMMON/iter/imin,imax,igmin,nit     
C Other stuff
      REAL bx,by,aEg0,aEg1,aEx0,aEx1,a0,a1,c0,c1,Emin,Emax,Egmin,Emr,Eold
      CHARACTER Qf,Qt
      REAL Sum,Delta,x,wf,wt,a,C,E
      DIMENSION Fi(0:4095),Ff(0:4095),Fg(0:511,0:511),Cn(0:4095,0:4)
      DIMENSION Tave(0:511),sTave(0:511)
      INTEGER multiple,i,j,iemr,fgdim,cndimx,cndimy,mass
      WRITE(6,*)' ________________________________________'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|              T A V E  1.0              |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|    Program to calculate the average    |'
      WRITE(6,*)'|   temperature at which the radiative   |'
      WRITE(6,*)'|  strength function has been extracted  |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|         Created: 27/05 - 2003          |'
      WRITE(6,*)'|          Undraa Agvaanluvsan           |'
      WRITE(6,*)'|   Magne Guttormsen, Andreas Schiller   |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Lawrence Livermore National Laboratory |'
      WRITE(6,*)'|        Oslo Cyclotron Laboratory       |'
      WRITE(6,*)'|________________________________________|'
      Emin=4000.0               ! Default lower Ex for 1.gen. spec.
      Emax=8000.0               ! Default higher Ex for 1.gen. spec.
      Egmin=1000.0              ! Default lowest gamma energy
      Qf='y'
      WRITE(6,10)Qf
 10   FORMAT('Weighting by first generation spectrum <',A1,'>:',$)
      CALL READA(5,Qf)
      IF(Qf.EQ.'y')THEN
C Reading first-generation mama-matrix
         IDEST=1
         ITYPE=3
         WRITE(6,'(A50)')'Please, answer 1 and the name of your input first-'
         WRITE(6,'(A46)')'generation matrix in the two next questions...'
         CALL READFILE
         IF(XDIM.GT.512)XDIM=512
         IF(YDIM.GT.512)YDIM=512
         bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
         by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
         IF(bx+by.EQ.2.)THEN     
            aEg0=11.            !Defaults in case of no calibration
            aEg1=20.
            aEx0=9660.
            aEx1=-120.
         ELSE
            aEg0=cal(1,IDEST,1,1) !Picks the spectrum calibration
            aEg1=cal(1,IDEST,1,2)
            aEx0=cal(1,IDEST,2,1)
            aEx1=cal(1,IDEST,2,2)
         ENDIF
         a1=ABS(aEx1)
         multiple=INT((120./a1)+0.5)   
         IF(a1.GT.150.)multiple=1
         a1=FLOAT(multiple)*a1
C An energy that was in the middle of a channel, shall still be in the 
C middle of a channel after change of calibration
         Eold=aEx0+aEx1*0       !Choosing old channel 0
         a0=(Eold/a1-INT(Eold/a1))*a1
         IF(a0.LT.0)a0=a0+a1
C Compressing (or stretching) along X and Y - axis
         DO j=0,YDIM-1          ! X-axis
            Sum=0.
            DO i=0,XDIM-1
               Fi(i)=rMAT(IDEST,i,j) ! Fi(i) and Ff(i) real type
               Sum=Sum+Fi(i)
            ENDDO
            IF(Sum.NE.0)THEN
               CALL ELASTIC(Fi,Ff,aEg0,aEg1,a0,a1,512,512) ! Modifies spectrum to give  
               DO i=0,XDIM-1    ! calibration a0 and a1
                  Fg(i,j)=Ff(i)
                  Fi(i)=0.
               ENDDO
            ENDIF
         ENDDO
         DO i=0,XDIM-1          ! Y-axis
            Sum=0.
            DO j=0,YDIM-1
               Fi(j)=Fg(i,j)
               Sum=Sum+Fi(j)
            ENDDO
            IF(Sum.NE.0)THEN
               CALL ELASTIC(Fi,Ff,aEx0,aEx1,a0,a1,512,512)
               DO j=0,YDIM-1
                  Fg(i,j)=Ff(j)
                  Fi(j)=0.
               ENDDO
            ENDIF
         ENDDO 
C Replacing negative counts with 0 and finding dimension of Fg matrix
         XDIM=INT(FLOAT(XDIM)*ABS(aEg1/a1)+0.5)
         YDIM=INT(FLOAT(YDIM)*ABS(aEx1/a1)+0.5)
         fgdim=10
         DO j=0,YDIM
            DO i=0,XDIM
               IF(Fg(i,j).GT.0.AND.i.GT.fgdim)fgdim=i
               IF(Fg(i,j).GT.0.AND.j.GT.fgdim)fgdim=j
               IF(Fg(i,j).LT.0.)Fg(i,j)=0 !Delete negative numbers
            ENDDO
         ENDDO
         fgdim=MIN0(fgdim,XDIM,YDIM)
         WRITE(6,11)a0,a1,fgdim,fgdim
 11      FORMAT('Common calibration is a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     +        /,'Dimension is',I3,' x',I3)
      ELSE
         a0=0.
         a1=120.
         fgdim=60
         WRITE(6,12)a0
 12      FORMAT('Calibration of first-generation spectrum (keV) a0=<',F7.1,'>:',$)
         CALL READF(5,a0)
         WRITE(6,13)a1
 13      FORMAT('Calibration of first-generation spectrum (keV/ch) a1=<',F7.1,'>:',$)
         CALL READF(5,a1)
         WRITE(6,14)fgdim
 14      FORMAT('Dimension of first-generation spectrum dim=<',I2,'>:',$)
         CALL READI(5,fgdim)
      END IF
C Input some parameters from keyboard
C Input lower limit for gammas used in the extraction
      igmin=INT(((Egmin-a0)/a1)+0.5)
      Egmin=a0+a1*igmin
      WRITE(6,15)Egmin
 15   FORMAT('Lower limit of gamma energy (keV)      <',F7.1,'>:',$)
      CALL READF(5,Egmin)
      igmin=INT(((Egmin-a0)/a1)+0.5)
      Egmin=a0+a1*igmin
      imin=INT(((Emin-a0)/a1)+0.5)
      imin=MAX(imin,igmin)
      Emin=a0+a1*imin
      WRITE(6,16)Emin
 16   FORMAT('Lower limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Emin)
      imin=INT(((Emin-a0)/a1)+0.5)
      Emin=a0+a1*imin
      IF(Emin.LT.Egmin)THEN
         WRITE(6,'(A29)')'Sorry, Emin<Egmin not allowed'
         STOP
      ENDIF
      imax=INT(((Emax-a0)/a1)+0.5)
      imax=MIN(imax,fgdim)
      Emax=a0+a1*imax
      WRITE(6,17)Emax
 17   FORMAT('Upper limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Emax)
      imax=INT(((Emax-a0)/a1)+0.5)
      imax=MIN(imax,fgdim)
      Emax=a0+a1*imax
      IF(imax.GT.511)THEN
         WRITE(6,18)Emax,a1
 18      FORMAT('Sorry, Emax/a1 = ',F7.1,'/',F5.1,' > 511 channels')
         STOP
      ENDIF
      Emr=Emax-Egmin
      iemr=INT(((Emr-a0)/a1)+0.5)
      Emr=a0+a1*iemr
      WRITE(6,19)Emin,Emax,a0,Emr,Egmin,Emax
 19   FORMAT('Excitation energy region is ',F5.0,'keV to ',F5.0,'keV',
     + /,'rho extracted from ',F5.0,'keV to ',F5.0,'keV',
     + /,'sig extracted from ',F5.0,'keV to ',F5.0,'keV')
      Qt='y'
      WRITE(6,20)Qt
 20   FORMAT('Using experimental caloric curve <',A1,'>:',$)
      CALL READA(5,Qt)
      IF(Qt.EQ.'y')THEN
C Reading canonical quantities as function of energy
         IDEST=1
         ITYPE=3
         WRITE(6,'(A48)')'Please, answer 1 and the name of your input file'
         WRITE(6,'(A47)')'with canonical quantities as function of energy'
         CALL READFILE
         bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
         by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
         IF(bx+by.EQ.2.)THEN     
            aEg0=0.             !Defaults in case of no calibration
            aEg1=120.
            aEx0=0.
            aEx1=1.
         ELSE
            aEg0=cal(1,IDEST,1,1) !Picks the spectrum calibration
            aEg1=cal(1,IDEST,1,2)
            aEx0=cal(1,IDEST,2,1)
            aEx1=cal(1,IDEST,2,2)
         ENDIF
         c1=ABS(aEg1)
         multiple=INT((120./c1)+0.5)   
         IF(c1.GT.150.)multiple=1
         c1=FLOAT(multiple)*c1
C An energy that was in the middle of a channel, shall still be in the 
C middle of a channel after change of calibration
         Eold=aEg0+aEg1*0       !Choosing old channel 0
         c0=(Eold/c1-INT(Eold/c1))*c1
         IF(c0.LT.0)c0=c0+c1
C Compressing (or stretching) along X and Y - axis
         DO j=0,YDIM-1          ! X-axis
            Sum=0.
            DO i=0,XDIM-1
               Fi(i)=rMAT(IDEST,i,j) ! Fi(i) and Ff(i) real type
               Sum=Sum+Fi(i)
            ENDDO
            IF(Sum.NE.0)THEN
               CALL ELASTIC(Fi,Ff,aEg0,aEg1,c0,c1,4096,4096) ! Modifies spectrum to give  
               DO i=0,XDIM-1    ! calibration c0 and c1
                  Cn(i,j)=Ff(i)
                  Fi(i)=0.
               ENDDO
            ENDIF
         ENDDO
C Replacing negative counts with 0 and finding dimension of canonical matrix
         XDIM=INT(FLOAT(XDIM)*ABS(aEg1/c1)+0.5)
         cndimx=10
         cndimy=10
         DO j=0,YDIM-1
            DO i=0,XDIM-1
               IF(Cn(i,j).GT.0.AND.i.GT.cndimx)cndimx=i
               IF(Cn(i,j).GT.0.AND.j.GT.cndimy)cndimy=j
               IF(Cn(i,j).LT.0.)Cn(i,j)=0 !Delete negative numbers
            ENDDO
         ENDDO
         cndimx=MIN0(cndimx+1,XDIM)
         cndimy=MIN0(cndimy+1,YDIM)
         WRITE(6,21)c0,c1,cndimx,cndimy
 21      FORMAT('Calibration is c0=',F7.2,'keV and c1=',F7.2,'keV/ch',
     + /,'Dimension is',I4,' x ',I4)
         IF((a0+c0).GT.1.E-5)THEN
            ac0=ABS(a0-c0)/ABS(a0+c0)
         ELSE
            ac0=ABS(a0-c0)
         ENDIF
         IF((a1+c1).GT.1.E-5)THEN
            ac1=ABS(a1-c1)/ABS(a1+c1)
         ELSE
            ac1=ABS(a1-c1)
         ENDIF
         IF((ac1.GT.1.E-5).OR.(ac0.GT.1.E-5))STOP'Calibration mismatch'
      ELSE
         WRITE(6,'(A37)')'We assume a caloric curve of the form'
         WRITE(6,'(A22)')'T=SQRT(U/a)+x/a, where'
         WRITE(6,'(A41)')'U=E-Delta-C is the backshifted energy and'
         WRITE(6,'(A33)')'a is the level density parameter.'
         WRITE(6,'(A32)')'C and a are taken from v. Egidy,'
         WRITE(6,'(A47)')'thus the only input parameters are Delta and x.'
         Delta=2000.
         x=0.
         mass=162
         WRITE(6,22)Delta
 22      FORMAT('Pairing energy of the nucleus (keV) Delta=<',F7.1,'>:',$)
         CALL READF(5,Delta)
         WRITE(6,23)x
 23      FORMAT('Next-to-leading order correction x=<',F7.1,'>:',$)
         CALL READF(5,x)
         WRITE(6,24)mass
 24      FORMAT('Mass number of the nucleus A=<',I3,'>:',$)
         CALL READI(5,mass)
         a=0.21*(mass**0.87)*0.001 ! in levels per keV
         C=-6.6*(mass**(-0.32))*1000. ! in keV
      ENDIF
C     Here: Do some calculation.
C For each j (i.e. Eg), calculate average T
      DO j=igmin,imax
C initialize the sum of Fg elements
         sumfg=0.0
C initialize the T weighted sum of Fg elements
C where weighting factor T comes from canonical ensemble average
         sumTfg=0.0
C intialize the T^2 weighted sum of fg elements
         sumT2fg=0.0
         DO i=MAX(j,imin),imax
            IF(Qf.EQ.'y')THEN
               wf=Fg(j,i)
            ELSE
               wf=1.
            ENDIF
            IF(Qt.EQ.'y')THEN
               wt=Cn(i-j,3)
            ELSE
               E=MAX(0.,(a1*REAL(i-j)+a0)-Delta-C)
               wt=(SQRT(E/a)-x/a)
            ENDIF
            sumfg=sumfg+wf
            sumTfg=sumTfg+wt*wf
            sumT2fg=sumT2fg+wt*wt*wf
         ENDDO
C calculate average T
         Tave(j-igmin)=sumTfg/sumfg   ! an average T as a function of Eg
         IF((sumT2fg/sumfg-Tave(j-igmin)*Tave(j-igmin)).GE.0.)THEN
            sTave(j-igmin)=sqrt(sumT2fg/sumfg-Tave(j-igmin)*Tave(j-igmin)) ! one standard deviation
         ELSE
            sTave(j-igmin)=0.
         ENDIF
         Tave(j-igmin)=Tave(j-igmin)*0.001   ! back to MeV
         sTave(j-igmin)=sTave(j-igmin)*0.001 ! back to MeV
      ENDDO
C Writing spectra out for mama
C Writting spectra to matrices
 151  IDEST=1
      cal(1,1,1,1)=Egmin
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      XDIM=fgdim-igmin
      YDIM=2
      DO i=0,XDIM-1
         rMAT(IDEST,i,0)=Tave(i)
         rMAT(IDEST,i,1)=sTave(i)
      ENDDO
      outfile='tavesp.tav'
      comment='Tave, dTave'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,25)outfile
 25   FORMAT('Tave, dTave written to file: ',A11)
      STOP
 99   WRITE(6,*)'Could not open file for results and spectra'
      STOP
      END

C -----------------------------------------------------------------------------
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
            IF(Fi(i).EQ.0)GO TO 97
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
            j2=CHfH+0.5                     !filling with CHwidth*CountCH 
            IF(j1.GE.Df.OR.j2.LT.0)GO TO 97
            nch=j2-j1+1
            IF(nch.EQ.1)THEN                !One channel to fill
               IF(j1.GE.0)Ff(j1)=Ff(j1)+Fi(i)
               GO TO 97
            ENDIF
            IF(nch.GT.1)THEN                !Two or three channels to fill
               Counts=CountCH*(j1+0.5-CHfL) !Fraction of left channel
               IF(j1.GE.0)Ff(j1)=Ff(j1)+Counts       
               Counts=CountCH*(CHfH+0.5-j2) !Fraction of right channel
               IF(j2.LE.Df-1)Ff(j2)=Ff(j2)+Counts
               DO j=j1+1,j2-1               !Filling in for whole chs.
                  IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+CountCH
               ENDDO
            ENDIF
 97         CONTINUE
         ENDDO
      ELSE
C The counts will be distributed in the streching procedure as a triangle,
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
 98         CONTINUE
         ENDDO
      ENDIF
      END
