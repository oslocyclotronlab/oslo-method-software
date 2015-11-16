      PROGRAM tradcc
      WRITE(6,*)' ___________________________________________'
      WRITE(6,*)'|                                           |'
      WRITE(6,*)'|             T R A D C C   1.0             |'
      WRITE(6,*)'|                                           |'
      WRITE(6,*)'| Program to calculate caloric curves from  |'
      WRITE(6,*)'| a theoretical multiplicity of states M(E) |'
      WRITE(6,*)'| using (microcanonical ensemble)  entropy: |'
      WRITE(6,*)'| S=LN(M), thus caloric curve: T=(dS/dE)^-1 |'
      WRITE(6,*)'| +(canonical ensemble) partition function: |'
      WRITE(6,*)'| Z(T)=INTEGRAL(0->infinity) EXP(-E/T)*M*dE |'
      WRITE(6,*)'| thus caloric curve: E=T^2*d(LN(Z))/dT.    |'
      WRITE(6,*)'|                                           |'
      WRITE(6,*)'|   Created: 12/08/03 (based on thermo.f)   |'
      WRITE(6,*)'|      Oslo Cyclotron Laboratory (OCL)      |'
      WRITE(6,*)'|             Magne  Guttormsen             |'
      WRITE(6,*)'|  Lawrence Livermore  National Laboratory  |'
      WRITE(6,*)'|             Andreas  Schiller             |'
      WRITE(6,*)'|___________________________________________|'
      OPEN(UNIT=11,FILE='result.tmo')
      CALL Microcanonical
      CALL Canonical
      WRITE(6,*)'___________________________________'
      WRITE(6,*)'Results written to file: result.tmo'
      WRITE(6,*)'and to matrices:'
      WRITE(6,*)'Microcanonical T(E):     micro.tmo'
      WRITE(6,*)'Canonical E(T):          canon.tmo'
      WRITE(6,*)'___________________________________'
      CLOSE(11)
      END

      SUBROUTINE Microcanonical
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
C Stuff for the microcanonical part 
      REAL E(0:8191),f(0:8191),S(0:8191),T(0:8191),dSdE(0:8191)
C Initializing arrays
      DO i=0,8191
         f(i)=0.
         S(i)=0.
         T(i)=0.
      ENDDO
C Reading singles level density
      WRITE(6,*)'Please, answer 1 and the name of your input'
      WRITE(6,*)'multiplicity spectrum in the two next questions... '
      WRITE(6,*)' '
      IDEST=1
      ITYPE=1
      CALL READFILE
      idime=MAXCH+1
      DO i=0,idime-1
         f(i)=rSPEC(1,i)
      ENDDO
      bx=cal(2,IDEST,1,1)+cal(2,IDEST,1,2)+cal(2,IDEST,1,3)
      IF(bx.EQ.1.)THEN     
         a0=100.                !Defaults in case of no calibration
         a1=200.
      ELSE
         a0=cal(2,IDEST,1,1)    !Picks the spectrum calibration
         a1=cal(2,IDEST,1,2)
      ENDIF
      WRITE(6,8)a0
 8    FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,10)a1
 10   FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)
C Finding mLow and mHigh markers
      mLow=0
      mHigh=MAXCH
      WRITE(6,12)a0,a1,idime,mLow,mHigh
 12   FORMAT(/,'Calibration: a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     + /,      'Dimension:',I4,', derivating between ch.: ',I4,' - ',I4)
      WRITE(6,*)' '
      WRITE(6,*)'The program steps through one by one channel in the'
      WRITE(6,*)'array and calculates the derivate at that channel. '
      WRITE(6,*)'You can choose between the following main strategies: '
      WRITE(6,*)' '
      WRITE(6,*)'Simple:   dy(i)/dx = (y(i) - y(i-1)) / (x(i) - x(i-1))'
      WRITE(6,*)'          dy(i)/dx = (y(i+1) - y(i)) / (x(i+1) - x(i))'
      WRITE(6,*)'          dy(i)/dx = (y(i+1)-y(i-1)) / (x(i+1)-x(i-1))'
      WRITE(6,*)'Advanced: A linear or quadratic fit for a given number'
      WRITE(6,*)'          of channels around the center-channel is'
      WRITE(6,*)'          performed. Thus, dy(i)/dx = a1 + 2*a2*x(i).'
      WRITE(6,*)' '
 777  method=2
      WRITE(6,14)method
 14   FORMAT('Simple (1) or advanced (2) method   <',I1,'>:',$)
      CALL READI(5,method)
      WRITE(6,*)' '
      IF(Istatus.NE.0.OR.method.LT.1.or.method.GT.2)THEN
         WRITE(6,*)'Illegal parameter, valid region:'
         WRITE(6,*)'0 < method < 3'
         GO TO 777
      ENDIF
      IF(method.EQ.1)THEN
         imax=2
 888     mode=1
         WRITE(6,*)'dy(i)/dx=(y(i)-y(i-1))/(x(i)-x(i-1))      (1)'
         WRITE(6,*)'dy(i)/dx=(y(i+1)-y(i))/(x(i+1)-x(i))      (2)'
         WRITE(6,*)'dy(i)/dx=(y(i+1)-y(i-1))/(x(i+1)-x(i-1))  (3)'
         WRITE(6,16)mode
 16      FORMAT('Choose your alternative                    <',I1,'>:',$)
         CALL READI(5,mode)
         IF(Istatus.NE.0.OR.mode.LT.1.or.mode.GT.3)THEN
            WRITE(6,*)'Illegal parameter, valid region:' 
            WRITE(6,*)'0 < mode < 4'
            GO TO 888
         ENDIF
      ELSE
 999     iPol=1
         Istatus=0
         imax=5
         WRITE(6,20)iPol
 20      FORMAT('Degree of polynom (lin=1, quad=2)   <',I1,'>:',$)
         CALL READI(5,iPol)
         WRITE(6,22)imax
 22      FORMAT('Number of datapoints               <',I2,'>:',$)
         CALL READI(5,imax)
         IF(Istatus.NE.0.OR.iPol.LT.1.OR.iPol.GT.2.
     +   OR.imax.GT.100.OR.imax.LT.2.OR.iPol.GE.imax)THEN
            WRITE(6,*)'Illegal parameters, valid region:' 
            WRITE(6,*)'0 < polynom <   3'
            WRITE(6,*)'1 < points  < 101'
            WRITE(6,*)'polynom < points'
            GO TO 999
         ENDIF
         IF(iPol.EQ.1)mode=4
         IF(iPol.EQ.2)mode=5
      ENDIF
      nu=1000
      DO i=mLow,mHigh
         E(i)=(a0+a1*i)/1000. ! Energy in MeV
         IF(f(i).GT.0)S(i)=log(f(i))
      ENDDO
      CALL Derivate(dSdE,S,E,mLow,mHigh,imax,mode)
      DO i=mLow,mHigh
         IF(dSdE(i).NE.0)T(i)=1./dSdE(i) ! Temperature in MeV
      ENDDO
      WRITE(6,*)' '
      WRITE(6,*)'Microcanonical ensemble:'
      WRITE(6,*)'  ch         E              T           dSdE      '  
      DO i=mLow,mHigh
         WRITE(6,30)i,E(i),T(i),dSdE(i)
 30      FORMAT(I5,X,E14.8,X,E14.8,X,E14.8)
      ENDDO
C Writting results to file result.tmo. Can be used for PAW-drawings
      WRITE(11,31)a0/1000.,a1/1000.,idime,mLow,mHigh
 31   FORMAT(/,'Calibration: a0=',F7.2,'MeV and a1=',F7.2,'MeV/ch',
     + /,      'Dimension:',I4,', derivating between ch.: ',I4,' - ',I4)
      WRITE(11,32)method,mode,imax
 32   FORMAT('Method = ',I2,', Mode = ',I2,', Number of interpolation points = ',I2)
      WRITE(11,34)fname(2,1)
 34   FORMAT('Input filename (only 8 first characters given): ',A8)
      WRITE(11,36)comm(2,1)
 36   FORMAT('Comments for input file: ',A60)
      WRITE(11,*)' '
      WRITE(11,*)'  ch         E              T           dSdE      '  
      DO i=mLow,mHigh
         WRITE(11,30)i,E(i),T(i),dSdE(i)
      ENDDO
      OPEN(UNIT=16,FILE='micro.tmo')
      DO i=mLow,mHigh
         WRITE(16,30)i,E(i),T(i),dSdE(i)
      ENDDO
      CLOSE(16)
      RETURN
      END

      SUBROUTINE Canonical
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4
C Stuff for the canonical part 
      COMMON/cant/Zt(0:8191),Ft(0:8191),St(0:8191),Et(0:8191),Ct(0:8191),aT0,aT1,idimt
      REAL rho(0:8191)
      REAL E,Eave,x,y,ww,Zz,T
C Reading in semiexperimental rho-function - if required
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 1 and the name of your input'
      WRITE(6,*)'multiplicity spectrum in the two next questions... '
      WRITE(6,*)' '
      IDEST=1
      ITYPE=1
      CALL READFILE
      idime=MAXCH+1
      DO i=0,idime-1
         rho(i)=rSPEC(1,i)
      ENDDO
      bx=cal(2,IDEST,1,1)+cal(2,IDEST,1,2)+cal(2,IDEST,1,3)
      IF(bx.EQ.1.)THEN     
         a0=100.                !Defaults in case of no calibration
         a1=200.
      ELSE
         a0=cal(2,IDEST,1,1)    !Picks the spectrum calibration
         a1=cal(2,IDEST,1,2)
      ENDIF
      WRITE(6,2)a0
 2    FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,4)a1
 4    FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)
      mLow=0
      mHigh=MAXCH
      idimt=100
      WRITE(6,22)idimt
 22   FORMAT('Number of temperature bins <',I3,'>:',$)
      CALL READI(5,idimt)
      IF(idimt.GT.8192)STOP'Too many channels must be less or equal 8k'      
      mhZ=mHigh
      mlZ=0
      WRITE(6,8)a0/1000.,a1/1000.,idime,mLow,mHigh
      WRITE(11,8)a0/1000.,a1/1000.,idime,mLow,mHigh
 8    FORMAT(/,'Energy calibration: a0=',F7.2,'MeV and a1=',F7.2,'MeV/ch',
     + /,      'Dimension:',I4,',  channels used: ',I4,' - ',I4)
      WRITE(11,12)fname(2,1)
 12   FORMAT('Input filename (only 8 first characters given): ',A8)
      WRITE(11,14)comm(2,1)
 14   FORMAT('Comments for input file: ',A60)
      WRITE(6,*)' '
      WRITE(6,*)'Canonical ensemble:'
      WRITE(6,*)'  ch         T              E      '  
      WRITE(11,*)' '
      WRITE(11,*)'Canonical ensemble:'
      WRITE(11,*)'  ch         T              E      '  
      OPEN(UNIT=17,FILE='canon.tmo')
      pi=ACOS(-1.)
      WRITE(6,16)0,0.,0.
      WRITE(11,16)0,0.,0.
      WRITE(17,16)0,0.,0.
      DO i=1,idimt-1
         T=TAN(pi*SQRT(1.-(FLOAT(i)/FLOAT(idimt)-1.)**2.)/2.) ! T in MeV 
         Eave=0.
         Zz=0.
         DO j=mlZ,mhZ
            E=(a0+a1*FLOAT(j))/1000. ! Energy in MeV
            x=E/T
            y=0.
            ww=rho(j)           ! Multiplicity in states per MeV 
            IF(x.LT.60)y=EXP(-x)*ww*a1/1000. ! a1 given in keV per channel
            Eave=Eave+E*y
            Zz=Zz+y
         ENDDO
         Eave=Eave/Zz
         WRITE(6,16)i,T,Eave
         WRITE(11,16)i,T,Eave
 16      FORMAT(I5,X,E14.8,X,E14.8)
         WRITE(17,16)i,T,Eave
      ENDDO
      Eave=0.
      Zz=0.
      DO j=mlZ,mhZ
         E=(a0+a1*FLOAT(j))/1000. ! Energy in MeV
         ww=rho(j)              ! Multiplicity in states per MeV 
         y=ww*a1/1000. ! a1 given in keV per channel
         Eave=Eave+E*y
         Zz=Zz+y
      ENDDO
      Eave=Eave/Zz
      WRITE(6,16)idimt,0.,Eave
      WRITE(11,16)idimt,0.,Eave
      WRITE(17,16)idimt,0.,Eave
      CLOSE(17)
      RETURN
      END

      SUBROUTINE Derivate(dydx,y,x,mLow,mHigh,imax,mode)
C Routine to derivate spectra. The program calls the very
C old and nice SUBROUTINE MATINV(). User can choose between
C 5 algorithms.
C Magne Guttormsen, Oslo Cyclotron Laboratory, April 1998
      DIMENSION dydx(0:8191),y(0:8191),x(0:8191)
      REAL*8 XMAT(3,3),sx1,sx2,sx3,sx4,sx0y,sx1y,sx2y
C Initializing
      DO i=0,8191
         dydx(i)=0.
      ENDDO
C The three simple methods
      IF(mode.EQ.1)THEN
         DO i=mLow+1,mHigh
            dx=x(i)-x(i-1)
            dy=y(i)-y(i-1)
            IF(dx.NE.0)dydx(i)=dy/dx
         ENDDO
         dydx(mLow)=dydx(mLow+1)
      ENDIF
      IF(mode.EQ.2)THEN
         DO i=mLow,mHigh-1
            dx=x(i+1)-x(i)
            dy=y(i+1)-y(i)
            IF(dx.NE.0)dydx(i)=dy/dx
         ENDDO
         dydx(mHigh)=dydx(mHigh-1)
      ENDIF
      IF(mode.EQ.3)THEN
         DO i=mLow+1,mHigh-1
            dx=x(i+1)-x(i-1)
            dy=y(i+1)-y(i-1)
            IF(dx.NE.0)dydx(i)=dy/dx
         ENDDO
         dydx(mLow) =dydx(mLow+1)
         dydx(mHigh)=dydx(mHigh-1)
      ENDIF
C The advanced method (see p.277 in M.R. Spiegel Prob & Stat)
C The number of points to fit is imax+1 
C Here is mode = 4 linear and mode = 5 quadratic
      IF(mode.EQ.4)THEN         ! Linear fit
         DO i=mLow,mHigh
            sx1=0.
            sx2=0.
            sx0y=0.
            sx1y=0.
            xi=(imax-1.)/2.
            xxi1=FLOAT(i)-xi
            xxi2=FLOAT(i)+xi
            i1=xxi1+0.5
            i2=xxi2+0.5
            IF(xxi1.LT.0)i1=xxi1-0.5
            IF(xxi2.LT.0)i2=xxi2-0.5
            IF(i1.LT.mLow)THEN
               i2=i2+(mLow-i1)
               i1=i1+(mLow-i1)
            ENDIF
            IF(i2.GT.mHigh)THEN
               i1=i1-(i2-mHigh)
               i2=i2-(i2-mHigh)
            ENDIF
            DO ii=i1,i2
               sx1=sx1+x(ii)
               sx2=sx2+x(ii)*x(ii)
               sx0y=sx0y+y(ii)
               sx1y=sx1y+x(ii)*y(ii)
            ENDDO
            XMAT(1,1)=imax
            XMAT(2,1)=sx1
            XMAT(1,2)=sx1
            XMAT(2,2)=sx2
            CALL MATINV(XMAT,2,3)
            a0=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y
            a1=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y
            dydx(i)=a1
         ENDDO
      ENDIF
      IF(mode.EQ.5)THEN         ! Quadratic fit
         DO i=mLow,mHigh
            sx1=0.
            sx2=0.
            sx3=0.
            sx4=0.
            sx0y=0.
            sx1y=0.
            sx2y=0.
            xi=(imax-1.)/2.
            xxi1=FLOAT(i)-xi
            xxi2=FLOAT(i)+xi
            i1=xxi1+0.5
            i2=xxi2+0.5
            IF(xxi1.LT.0)i1=xxi1-0.5
            IF(xxi2.LT.0)i2=xxi2-0.5
            IF(i1.LT.mLow)THEN
               i2=i2+(mLow-i1)
               i1=i1+(mLow-i1)
            ENDIF
            IF(i2.GT.mHigh)THEN
               i1=i1-(i2-mHigh)
               i2=i2-(i2-mHigh)
            ENDIF
            DO ii=i1,i2
               sx1=sx1+x(ii)
               sx2=sx2+x(ii)*x(ii)
               sx3=sx3+x(ii)*x(ii)*x(ii)
               sx4=sx4+x(ii)*x(ii)*x(ii)*x(ii)
               sx0y=sx0y+y(ii)
               sx1y=sx1y+x(ii)*y(ii)
               sx2y=sx2y+x(ii)*x(ii)*y(ii)
            ENDDO
            XMAT(1,1)=imax
            XMAT(2,1)=sx1
            XMAT(3,1)=sx2
            XMAT(1,2)=sx1
            XMAT(2,2)=sx2
            XMAT(3,2)=sx3
            XMAT(1,3)=sx2
            XMAT(2,3)=sx3
            XMAT(3,3)=sx4
            CALL MATINV(XMAT,3,3)
            a0=XMAT(1,1)*sx0y+XMAT(2,1)*sx1y+XMAT(3,1)*sx2y
            a1=XMAT(1,2)*sx0y+XMAT(2,2)*sx1y+XMAT(3,2)*sx2y
            a2=XMAT(1,3)*sx0y+XMAT(2,3)*sx1y+XMAT(3,3)*sx2y
            dydx(i)=a1+2.0*a2*x(i)
         ENDDO
      ENDIF
      RETURN
      END

      SUBROUTINE MATINV(ARRAY,NORDER,IDIM)
      REAL*8 ARRAY(IDIM,IDIM),AMAX,SAVE
      INTEGER IK(100),JK(100)
      DO 100 K=1,NORDER
c        find largest element array(i,j) in rest of matrix....
         AMAX=0.
  21     DO 31 I=K,NORDER
            DO 30 J=K,NORDER
               IF(DABS(AMAX)-DABS(ARRAY(I,J)))24,24,30
  24           AMAX=ARRAY(I,J)
               IK(K)=I
               JK(K)=J
  30        CONTINUE
  31     CONTINUE
C        interchange rows and columns to put amax in array(k,k)....
         IF(AMAX)41,140,41
  41     I=IK(K)
         IF(I-K)21,51,43
  43     DO 50 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=ARRAY(I,J)
            ARRAY(I,J)=-SAVE
  50     CONTINUE
  51     J=JK(K)
         IF(J-K)21,61,53
  53     DO 60 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=ARRAY(I,J)
            ARRAY(I,J)=-SAVE
  60     CONTINUE
C        accumulate elements of inverse matrix....
  61     DO 70 I=1,NORDER
            IF(I-K)63,70,63
  63        ARRAY(I,K)=-ARRAY(I,K)/AMAX
  70     CONTINUE
         DO 81 I=1,NORDER
            DO 80 J=1,NORDER
               IF(I-K)74,80,74
  74           IF(J-K)75,80,75
  75           ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
  80        CONTINUE
  81     CONTINUE
         DO 90 J=1,NORDER
            IF(J-K)83,90,83
  83        ARRAY(K,J)=ARRAY(K,J)/AMAX
  90     CONTINUE
         ARRAY(K,K)=1./AMAX
 100  CONTINUE
C        restore ordering of matrix....
      DO 130 L=1,NORDER
         K=NORDER-L+1
         J=IK(K)
         IF(J-K)111,111,105
 105     DO 110 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=-ARRAY(I,J)
            ARRAY(I,J)=SAVE
 110     CONTINUE
 111     I=JK(K)
         IF(I-K)130,130,113
 113     DO 120 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=-ARRAY(I,J)
            ARRAY(I,J)=SAVE
 120     CONTINUE
 130  CONTINUE
 140  RETURN
      END
