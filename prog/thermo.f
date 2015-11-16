      PROGRAM thermo

      WRITE(6,*)'     ________________________________________________'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |               T H E R M O  3.0                 |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |      Program to calculate thermodynamical      |'
      WRITE(6,*)'    |     quantities from the level density Rho      |'
      WRITE(6,*)'    | MICROCANONICAL ENSEMBLE:                       |'
      WRITE(6,*)'    | Entropy:           S(E)  = kln(Rho)            |'
      WRITE(6,*)'    | Temperature:       T(E)  = 1/(dS/dEx)          |'
      WRITE(6,*)'    | Heat capacity:     Cv(E) = 1/(dT/dEx)          |'
      WRITE(6,*)'    | CANONICAL ENSEMBLE:                            |'
      WRITE(6,*)'    | Partition function:Z(T)  = Sum(e**-E/T)Rho(E)dE|'
      WRITE(6,*)'    | Helholtz:          F(T)  = -kTln(Z)            |'
      WRITE(6,*)'    | Entropy:           S(T)  = dF/dT               |'
      WRITE(6,*)'    | Excitation energy: E(T)  = F + TS              |'
      WRITE(6,*)'    | Heat capacity:     Cv(T) = dE/dT               |'
      WRITE(6,*)'    | and Z(<E>), F(<E>), S(<E>), T(<E>), Cv(<E>)    |'
      WRITE(6,*)'    | Created:  November 1998                        |'
      WRITE(6,*)'    | Modified: 01/10/99, 23/3/07, 13/11/08          |'
      WRITE(6,*)'    |           Oslo Cyclotron Laboratory            |'
      WRITE(6,*)'    |                Magne Guttormsen                |'
      WRITE(6,*)'    | Modified: 07/06/02                             |'
      WRITE(6,*)'    |     Lawrence Livermore National Laboratory     |'
      WRITE(6,*)'    |                Andreas Schiller                |'
      WRITE(6,*)'    |________________________________________________|'

      IANS = 0
      WRITE(6,34)
 34   FORMAT(/'Use microcanonical and canonical ensemble (0)',/,
     1     'Use only microcanonical ensemble          (1)',/,
     2     'Use only canonical ensemble               (2)',/,
     3     'EXIT                                      (3)')
      WRITE(6,32)IANS
 32   FORMAT(/'Please, give your answer <',I1,'>:',$)
      CALL READI(5,IANS)
      IF(IANS.LT.0.OR.IANS.GT.2)STOP

      OPEN(UNIT=11,FILE='result.tmo')

      IF(IANS.EQ.0.OR.IANS.EQ.1)THEN
         WRITE(6,2)
 2       FORMAT(/,'Microcanonical ensemble:',/)
         CALL Microcanonical
      ENDIF
      IF(IANS.EQ.0.OR.IANS.EQ.2)THEN
         WRITE(6,3)
 3       FORMAT(/,'Canonical ensemble:')
         CALL Canonical 
      ENDIF
      WRITE(6,*)   '_________________________________________________________'
      WRITE(6,*)   'The results are written to file:            result.tmo'
      WRITE(6,*)   'and to matrices:'
      IF(IANS.EQ.0.OR.IANS.EQ.1)THEN
         WRITE(6,*)'Microcanonical S, dS, <T>, d<T>, Cv and dCv: micro.tmo'
      ENDIF      
      IF(IANS.EQ.0.OR.IANS.EQ.2)THEN
         WRITE(6,*)'Canonical Z(T), F(T), S(T), E(T) and Cv(T): canont.tmo'
         WRITE(6,*)'Canonical Z(E), F(E), S(E), T(E) and Cv(E): canone.tmo'
      ENDIF
      WRITE(6,*)   '_________________________________________________________'
      CLOSE(11)
      END


      SUBROUTINE Microcanonical

C     Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4

C     Stuff for the microcanonical part 
      REAL E(0:8191)
      REAL  f(0:8191), S(0:8191), T(0:8191), C(0:8191)
      REAL sf(0:8191),sS(0:8191),sT(0:8191),sC(0:8191)
      REAL fv(0:8191),Sv(0:8191),Tv(0:8191),Cv(0:8191)
      REAL dSdE(0:8191),dTdE(0:8191)
      INTEGER time

C     Initializing arrays
      DO i=0,8191
         f(i)  = 0.
         sf(i) = 0.
         S(i)  = 0.
         sS(i) = 0.
         T(i)  = 0.
         sT(i) = 0.
         C(i)  = 0.
         sC(i) = 0.
      ENDDO

C     Reading in experimental rho and errors from counting.kumac
      WRITE(6,*)' '
      WRITE(6,*)'You may read in the normalized Rho and dRho created'
      WRITE(6,*)'from counting.kumac. The a0 and a1 are given in rhosp.rsg'           
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer similar as shown below: '
      WRITE(6,*)' '
      WRITE(6,*)'Destination spectrum <1>:1 '
      WRITE(6,*)'Filename          <TEST>:rhopaw.cnt'
      WRITE(6,*)'Is it a singles spectrum (y/n)? <y>:n'           
      WRITE(6,*)'Give dimension on x-axis < 128>:64'
      WRITE(6,*)' '
      IDEST = 1
      ITYPE = 3
      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
      IF(bx+by.EQ.2.)THEN     
         a0=0.                  !Defaults in case of no calibration
         a1=120.
      ELSE
         a0=cal(1,IDEST,1,1)    !Picks the spectrum calibration
         a1=cal(1,IDEST,1,2)
      ENDIF
      IF(a1.EQ.0)a1=120.

      WRITE(6,8)a0
 8    FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,10)a1
 10   FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)

      WRITE(6,*)' '
      WRITE(6,*)'We have to normalize entropy S, so that S->0 when T->0. This'
      WRITE(6,*)'means that we should try to obtain S=0 in the ground band of'
      WRITE(6,*)'even-even nuclei. Please, try to find a proper value for your'
      WRITE(6,*)'case. The formula reads: S = ln(rho) + S0, where S0 is to be'
      WRITE(6,*)'adjusted. This may also be expressed by: S = ln[exp(S)] = '
      WRITE(6,*)'ln W = ln[rho*exp(S0)], meaning that we multiply rho by a'
      WRITE(6,*)'factor exp(S0). Remember, S0 might differ in microcanonical'
      WRITE(6,*)'and canonical ensemble.'
      S0m = -1.0 
      WRITE(6,2)S0m
 2    FORMAT('Shift microcanonical entropy with S0 <',F5.2,'>:',$)
      CALL READF(5,S0m)

      DO i=0,XDIM-1
         f(i) = rMAT(IDEST,i,0)*exp(S0m) !rho-function
         sf(i)= rMAT(IDEST,i,1)*exp(S0m) !standard dev. of rho-function
      ENDDO

C     Finding mLow and mHigh markers
      mLow =0
      mHigh=XDIM-1
      DO i=0,XDIM-1
         IF(f(i).NE.0.AND.mLow.EQ.0)mLow=i
      ENDDO
      DO i=XDIM-1,0,-1
         IF(f(i).NE.0.AND.mHigh.EQ.XDIM-1)mHigh=i
      ENDDO

      WRITE(6,12)a0,a1,XDIM,mLow,mHigh
 12   FORMAT(/,'Calibration: a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     +     /,      'Dimension:',I4,', derivating between ch.: ',I4,' - ',I4)

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
      WRITE(6,*) ' '
 777  method=2
      WRITE(6,14)method
 14   FORMAT('Simple (1) or advanced (2) method   <',I1,'>:',$)
      CALL READI(5,method)
      WRITE(6,*) ' '
      IF(Istatus.NE.0.OR.method.LT.1.or.method.GT.2)THEN
         WRITE(6,*) 'Illegal parameter, valid region:'
         WRITE(6,*) '0 < method < 3'
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
            WRITE(6,*) 'Illegal parameter, valid region:' 
            WRITE(6,*) '0 < mode < 4'
            GO TO 888
         ENDIF
      ELSE
 999     iPol=1
         Istatus=0
         imax=(600./a1) + 0.5
         IF(2*(imax/2).EQ.imax)imax=imax-1 ! wants odd number of points
         IF(imax.LT.3)imax=3
         IF(imax.GT.101)imax=3
         WRITE(6,20)iPol
 20      FORMAT('Degree of polynom (lin=1, quad=2)   <',I1,'>:',$)
         CALL READI(5,iPol)
         WRITE(6,22)imax
 22      FORMAT('Number of datapoints               <',I2,'>:',$)
         CALL READI(5,imax)
         IF(Istatus.NE.0.OR.iPol.LT.1.or.iPol.GT.2.
     +        OR.imax.GT.100.OR.imax.LT.2.OR.iPol.GE.imax)THEN
            WRITE(6,*) 'Illegal parameters, valid region:' 
            WRITE(6,*) '0 < polynom <   3'
            WRITE(6,*) '1 < points  < 101'
            WRITE(6,*) 'polynom < points'
            GO TO 999
         ENDIF
         IF(iPol.EQ.1)mode=4
         IF(iPol.EQ.2)mode=5
      ENDIF

      nu = 1000
C     
C     Ready to go, fasten your seatbelts**************************
C     
      DO i = mLow,mHigh
         E(i) = a0 + a1*i
         IF(f(i).GT.0)S(i)=log(f(i))
      ENDDO
      CALL Derivate(dSdE,S,E,mLow,mHigh,imax,mode)
      DO i=mLow,mHigh
         IF(dSdE(i).NE.0)T(i)=1./dSdE(i)
      ENDDO
      CALL Derivate(dTdE,T,E,mLow,mHigh,imax,mode)
      DO i=mLow,mHigh
         IF(dTdE(i).NE.0)C(i)=1./dTdE(i)
      ENDDO

C     Calculating standard deviation (error bars) of entropy
      DO i = mLow,mHigh
         sS(i)=0.
         IF(f(i).NE.0)sS(i) = ABS(sf(i)/f(i))
      ENDDO

C     Calculating errorbars, randomizing raw data 1000 times
      nStep = 100
      r=rand(time())            !seeding
      DO inu = 1,nu
         nTest=(inu/nStep)*nStep
         IF(nTest.EQ.inu)THEN
            WRITE(6,1111)
 1111       FORMAT('.',$)
c     ist=putc('.')
            call flush(6)
         ENDIF        
         DO i = mLow,mHigh
            Tv(i)=0.            !Initialize variabel vectors
            Cv(i)=0.
            Sv(i)=0.
            r = rand(0)         !Getting next random number
            z = Finvert(r)
            fv(i) = f(i) + z*sf(i) !Adding uncertainty to rho
            E(i) = a0 + a1*i
            IF(fv(i).GT.0)Sv(i)=log(fv(i))
         ENDDO

         CALL Derivate(dSdE,Sv,E,mLow,mHigh,imax,mode)
         DO i = mLow,mHigh
            IF(dSdE(i).NE.0)Tv(i)=ABS(1./dSdE(i))
            IF(Tv(i).GT.2.*ABS(T(i)))Tv(i)=2.*ABS(T(i))
            IF(T(i).NE.0)sT(i) = sT(i) + (ABS(T(i)) - Tv(i))**2.
         ENDDO

         CALL Derivate(dTdE,Tv,E,mLow,mHigh,imax,mode)
         DO i = mLow,mHigh
            IF(dTdE(i).NE.0)Cv(i)=ABS(1./dTdE(i))
            IF(Cv(i).GT.2.*ABS(C(i)))Cv(i)=2.*ABS(C(i))
            IF(C(i).NE.0)sC(i) = sC(i) + (ABS(C(i)) - Cv(i))**2.
         ENDDO
      ENDDO

      DO i = mLow,mHigh
         sT(i) = SQRT(sT(i)/nu)
         sC(i) = SQRT(sC(i)/nu)
      ENDDO
C     
C     Finished, take off your seatbelts***********************
C     
      WRITE(6,*)' '
      WRITE(6,*)'Microcanonical ensemble:'
      WRITE(6,*)'  ch    Ex       Rho        dRho       S       dS     <T>     d<T>      Cv       dCv'  
      DO i=mLow,mHigh
         WRITE(6,30)i,E(i),f(i),sf(i),S(i),sS(i),T(i),sT(i),C(i),sC(i)
 30      FORMAT(I5,F8.1,2E11.3, 2F7.2,2F8.1,2F10.3)
      ENDDO

C     Writting results to file result.tmo. Can be used for PAW-drawings
      WRITE(11,31)a0,a1,XDIM,mLow,mHigh
 31   FORMAT(/,'Calibration: a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     +     /,      'Dimension:',I4,', derivating between ch.: ',I4,' - ',I4)
      WRITE(11,32)method,mode,imax
 32   FORMAT('Method = ',I2,', Mode = ',I2,', Number of interpolation points = ',I2)
      WRITE(11,34)fname(1,1)
 34   FORMAT('Input filename (only 8 first characters given): ',A8)
      WRITE(11,36)comm(1,1)
 36   FORMAT('Comments for input file: ',A60)
      WRITE(11,*)' '
      WRITE(11,*)'  ch    Ex       Rho        dRho       S       dS     <T>     d<T>      Cv       dCv' 
      DO i=mLow,mHigh
         WRITE(11,30)i,E(i),f(i),sf(i),S(i),sS(i),T(i),sT(i),C(i),sC(i)
      ENDDO
      
C     Writting spectra to matrix
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.

      YDIM=6
      DO i=0,XDIM-1
         rMAT(IDEST,i,0)=  S(i)
         rMAT(IDEST,i,1)= sS(i)
         rMAT(IDEST,i,2)=  T(i)
         rMAT(IDEST,i,3)= sT(i)
         rMAT(IDEST,i,4)=  C(i)
         rMAT(IDEST,i,5)= sC(i)
      ENDDO
      outfile='micro.tmo'
      comment='S  dS  <T>  d<T>  Cv   dCv '
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
c     WRITE(6,40) outfile
c     40    FORMAT('Microcanonical S, dS, <T>, d<T>, Cv and dCv written to: ',A11)

      GO TO 98  
 99   WRITE(6,*)'Could not open file'
 98   CONTINUE
      END


      SUBROUTINE Canonical
C     Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4
      
C     Stuff for the canonical part 
      COMMON/cant/Zt(0:8191),Ft(0:8191),St(0:8191),Et(0:8191),Ct(0:8191),aT0,aT1,idimt
      COMMON/cane/Ze(0:8191),Fe(0:8191),Se(0:8191),Te(0:8191),Ce(0:8191), a0, a1,idime
      REAL rho(0:8191)
      DOUBLE PRECISION E, Eave, Eaveold, Eave2, dEave, x, y, ww, Zz, T, Cv

C     Reading in semiexperimental rho-function - if required
      WRITE(6,*)' '
      WRITE(6,*)'You may read in an extrapolated rho-function for the canonical'
      WRITE(6,*)'ensemble that should cover the energy region 0 - 30 MeV.'           
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer similar as shown below: '
      WRITE(6,*)' '
      WRITE(6,*)'Destination spectrum <1>:1 '
      WRITE(6,*)'Filename          <TEST>:rhotmopaw.cnt'
      WRITE(6,*)'Is it a singles spectrum (y/n)? <y>:y'
      WRITE(6,*)' '
      IDEST = 1
      ITYPE = 1
      CALL READFILE
      idime = MAXCH+1
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
      IF(a1.EQ.0)a1=120.
      
      WRITE(6,2)a0
 2    FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,4)a1
 4    FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)
      per     = 1000.
      WRITE(6,3)per
 3    FORMAT('The Rho values are per keV(1.) or per MeV(1000.) <',F5.0,'>:',$)
      CALL READF(5,per)

      WRITE(6,*)' '
      WRITE(6,*)'We have to normalize entropy S, so that S->0 when T->0. This'
      WRITE(6,*)'means that we should try to obtain S=0 in the ground band of'
      WRITE(6,*)'even-even nuclei. Please, try to find a proper value for your'
      WRITE(6,*)'case. The formula reads: S = ln(rho) + S0, where S0 is to be'
      WRITE(6,*)'adjusted. This may also be expressed by: S = ln[exp(S)] = '
      WRITE(6,*)'ln W =  ln[rho*exp(S0)], meaning that we multiply rho by a'
      WRITE(6,*)'factor exp(S0). Remember, S0 might differ in microcanonical'
      WRITE(6,*)'and canonical ensemble.'
      S0c = 1.3 
      WRITE(6,20)S0c
 20   FORMAT('Shift canonical entropy with S0 <',F5.2,'>:',$)
      CALL READF(5,S0c)

C     Finding mLow and mHigh markers
      mLow  = 0
      mHigh = MAXCH
      DO i=0,MAXCH
         IF(rho(i).NE.0.AND.mLow.EQ.0)mLow=i
      ENDDO
      DO i=MAXCH,0,-1
         IF(rho(i).NE.0.AND.mHigh.EQ.MAXCH)mHigh=i
      ENDDO

      aT0     = 0.              ! Calibration for temperatures
      aT1     = 10.

C     Modified by Andreas Schiller June 2002 (start)
      WRITE(6,22)aT1
 22   FORMAT('Width of temperature bins in keV <',F4.1,'>:',$)
      CALL READF(5,aT1)
C     Modified by Andreas Schiller (end)

      Tmax    = 3000.           ! Maximal temperature

C     Modified by Andreas Schiller June 2002 (start)
      WRITE(6,24)Tmax
 24   FORMAT('Maximum temperature in keV     <',F6.1,'>:',$)
      CALL READF(5,Tmax)
C     Modified by Andreas Schiller (end)

      idimt   = INT(((Tmax-aT0)/aT1) + 0.5) +1
      
C     Modified by Andreas Schiller June 2002 (start)
      IF(idimt.GT.8192)STOP'Too many channels must be less or equal 8k'
C     Modified by Andreas Schiller (end)

      Eaveold = 0.
      Ezmax   = a0 + mHigh*a1
      WRITE(6,*)' '
      WRITE(6,6)Ezmax
 6    FORMAT('Give upper energy for partition function Z (keV) <',F8.1,'>:',$)
      CALL READF(5,Ezmax)
      mhZ     = INT(((Ezmax - a0)/a1) + 0.5)
      mlZ     = 0

      WRITE(6 ,8)a0,a1,idime,mLow,mHigh
      WRITE(11,8)a0,a1,idime,mLow,mHigh
 8    FORMAT(/,'Energy calibration: a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     +     /,      'Dimension:',I4,',  channels used: ',I4,' - ',I4)
      WRITE(6 ,10)aT0,aT1,idimt,0,idimt-1
      WRITE(11,10)aT0,aT1,idimt,0,idimt-1
 10   FORMAT(/,'Temperature calibration: aT0=',F7.2,'keV and aT1=',F7.2,'keV/ch',
     +     /,      'Dimension:',I4,',  channels used: ',I4,' - ',I4)

      WRITE(11,12)fname(2,1)
 12   FORMAT('Input filename (only 8 first characters given): ',A8)
      WRITE(11,14)comm(2,1)
 14   FORMAT('Comments for input file: ',A60)

      WRITE(6,*)' '
      WRITE(6,*)'Canonical ensemble:'
      WRITE(6,*)'  ch      T        <Ex>    (ch)      <dEx>        <Cv>'
      WRITE(11,*)' '
      WRITE(11,*)'Canonical ensemble:'
      WRITE(11,*)'  ch      T        <Ex>    (ch)      <dEx>        <Cv>'
      
      Eaveold = 0.
      DO i = 1, idimt-1
         T     = aT0 + aT1*FLOAT(i)
         Eave  = 0.
         Eave2 = 0.
         Zz    = 0.
         DO j  = mlZ, mhZ
            E     = a0    + a1*FLOAT(j)
            x     = E/T
            y     = 0
            ww    = (rho(j)/per)*exp(S0c)
            IF(x.LT.200)y = DEXP(-x)*ww*a1
            Eave  = Eave  +   E*y
            Eave2 = Eave2 + E*E*y
            Zz    = Zz    +     y
            IF(Zz.GT.1.175494E-38.AND.Zz.LT.1.175494E+38)THEN
               Zt(i)=Zz         !Z is single precission
            ELSE
            ENDIF
         ENDDO



C     TESTING TO SUBTRACT Zrot = T/Rigid
c     Amass = 160.
c     Rigid=(0.0137/1000.)*(Amass**(5./3.))         !in 1/keV
c     IF(T.GT.0.02) Zt(i)=Zt(i)/(T/Rigid) 



         Eave   = Eave/Zz
         Eave2  = Eave2/Zz
         dEave  = SQRT(Eave2 - Eave**2.)
         Cv     = (Eave-Eaveold)/aT1 
         ich    = ((Eave-a0)/a1) + 0.5
         IF(i.GT.1)WRITE(6,16)i, T, Eave, ich, dEave, Cv
         IF(i.GT.1)WRITE(11,16)i, T, Eave, ich, dEave, Cv
 16      FORMAT(I5, F8.1, F12.2, I6, 2F12.2)
         Eaveold = Eave
      ENDDO
      
      CALL canont
      CALL canone 

      GO TO 98  
      WRITE(6,*)'Could not open file'
 98   CONTINUE

      END


      SUBROUTINE canont
C     We extract everything from Z(T)
C     This means we utilize the bridge equation: Helmholtz free energy F = -kTlnZ

C     Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4

C     Stuff for canonic
      COMMON/cant/Zt(0:8191),Ft(0:8191),St(0:8191),Et(0:8191),Ct(0:8191),aT0,aT1,idimt
      COMMON/cane/Ze(0:8191),Fe(0:8191),Se(0:8191),Te(0:8191),Ce(0:8191), a0, a1,idime

C     Stuff for spline part
      INTEGER NP
      REAL yp1,ypn,xa(4096),ya(4096),y2(4096)

C     First calculating F(T), the bridge equation
      DO i=0,idimt-1
         T     = aT0 + aT1*FLOAT(i)
         IF(Zt(i).GT.0)Ft(i) = -T*log(Zt(i))
      ENDDO
      
      diff0 = aT1 * 0.2         !Going +/- diff0 = 2 keV out from T
      
C     Interpolating with spline-method (From the book: Numerical Recepies)
      NP = idimt-1
      DO i=1,NP
         xa(i) = aT0 + aT1*FLOAT(i)
         ya(i) = Ft(i)          !Interpolating F
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=1,NP
         diff = diff0
         IF(xa(i).GT.2000)diff = diff0 * (1. + 10.* (xa(i) - 2000.)/3000.)
         xl = xa(i) - diff      !Going +/- diff keV out from T 
         CALL splint(xa,ya,y2,NP,xl,yl) !Interpolating
         xh = xa(i) + diff 
         CALL splint(xa,ya,y2,NP,xh,yh) !Interpolating
         St(i) = -(yh-yl)/(xh-xl) !Derivating S = -dF/dT
         Et(i) = Ft(i) + St(i)*xa(i) !E = F + T*S
      ENDDO

      DO i=1,NP
         xa(i) = aT0 + aT1*FLOAT(i)
         ya(i) = Et(i)          !Interpolating E
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=1,NP
         xl = xa(i) - diff      !Going +/- diff keV out from T 
         CALL splint(xa,ya,y2,NP,xl,yl) !Interpolating
         xh = xa(i) + diff 
         CALL splint(xa,ya,y2,NP,xh,yh) !Interpolating
         Ct(i) = (yh-yl)/(xh-xl) !Derivating Cv = dE/dT
      ENDDO

C     Writting spectra to matrices as function of T
      cal(1,1,1,1)=aT0
      cal(1,1,1,2)=aT1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.

      YDIM=5
      XDIM=NP
      DO i=0,XDIM-1
         rMAT(IDEST,i,0)= Zt(i)
         rMAT(IDEST,i,1)= Ft(i)/1000.
         rMAT(IDEST,i,2)= St(i)
         rMAT(IDEST,i,3)= Et(i)/1000.
         rMAT(IDEST,i,4)= Ct(i)
      ENDDO
      outfile='canont.tmo'
      comment='Z(T), F(T), S(T), E(T) and Cv(T)'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)

      GO TO 98  
 99   WRITE(6,*)'Could not open file'
 98   CONTINUE

      END


      SUBROUTINE canone

C     Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4

C     Stuff for canonic
      COMMON/cant/Zt(0:8191),Ft(0:8191),St(0:8191),Et(0:8191),Ct(0:8191),aT0,aT1,idimt
      COMMON/cane/Ze(0:8191),Fe(0:8191),Se(0:8191),Te(0:8191),Ce(0:8191), a0, a1,idime

C     Stuff for spline part
      INTEGER NP
      REAL xx,yy,yp1,ypn,xa(4096),ya(4096),y2(4096)

C     Interpolating with spline-method (From the book: Numerical Recepies)
      NP = idimt

      DO i=1,NP
         xa(i) = Et(i)
         ya(i) = Zt(i)          !Interpolating Z
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=0,idime-1
         xx = a0 + a1*FLOAT(i)  !Excitation energy bin of spectra
         CALL splint(xa,ya,y2,NP,xx,yy) !Interpolating
         Ze(i) = yy
      ENDDO

      DO i=1,NP
         xa(i) = Et(i)
         ya(i) = Ft(i)          !Interpolating F
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=0,idime-1
         xx = a0 + a1*FLOAT(i)  !Excitation energy bin of spectra
         CALL splint(xa,ya,y2,NP,xx,yy) !Interpolating
         Fe(i) = yy
      ENDDO

      DO i=1,NP
         xa(i) = Et(i)
         ya(i) = St(i)          !Interpolating S
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=0,idime-1
         xx = a0 + a1*FLOAT(i)  !Excitation energy bin of spectra
         CALL splint(xa,ya,y2,NP,xx,yy) !Interpolating
         Se(i) = yy
      ENDDO

      DO i=1,NP
         xa(i) = Et(i)
         ya(i) = aT0 + aT1*FLOAT(i) !Interpolating T
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=0,idime-1
         xx = a0 + a1*FLOAT(i)  !Excitation energy bin of spectr
         CALL splint(xa,ya,y2,NP,xx,yy) !Interpolating
         IF(yy.GT.0)Te(i) = yy
      ENDDO

      DO i=1,NP
         xa(i) = Et(i)
         ya(i) = Ct(i)          !Interpolating Cv
      ENDDO
      yp1 = 0                   !Calculates the derivatives at end-points
      ypn = 0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO i=0,idime-1
         xx = a0 + a1*FLOAT(i)  !Excitation energy bin of spectra
         CALL splint(xa,ya,y2,NP,xx,yy) !Interpolating
         Ce(i) = yy
      ENDDO

C     Writting spectra to matrices as function of E
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.

      YDIM=5
      XDIM=idime
      DO i=0,XDIM-1
         rMAT(IDEST,i,0)= Ze(i)
         rMAT(IDEST,i,1)= Fe(i)/1000.
         rMAT(IDEST,i,2)= Se(i)
         rMAT(IDEST,i,3)= Te(i)/1000.
         rMAT(IDEST,i,4)= Ce(i)
      ENDDO
      outfile='canone.tmo'
      comment='Z(E), F(E), S(E), T(E), Cv(E)'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)

      GO TO 98  
 99   WRITE(6,*)'Could not open file'
 98   CONTINUE

      END


      SUBROUTINE Derivate(dydx,y,x,mLow,mHigh,imax,mode)
C     Routine to derivate spectra. The program calls the very
C     old and nice SUBROUTINE MATINV(). User can choose between
C     5 algorithms.
C     Magne Guttormsen, Oslo Cyclotron Laboratory, April 1998
      DIMENSION dydx(0:8191),y(0:8191),x(0:8191)
      REAL*8 XMAT(3,3),sx1,sx2,sx3,sx4,sx0y,sx1y,sx2y

C     Initializing
      DO i=0,8191
         dydx(i)=0.
      ENDDO

C     The three simple methods
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

C     The advanced method (see p.277 in M.R. Spiegel Prob & Stat)
C     The number of points to fit is imax+1 
C     Here is mode = 4 linear and mode = 5 quadratic
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
            dydx(i) = a1
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
            dydx(i) = a1 + 2.0 * a2 * x(i)
         ENDDO
      ENDIF

      RETURN
      END


      SUBROUTINE MATINV(ARRAY,NORDER,IDIM)
      REAL*8 ARRAY(IDIM,IDIM),AMAX,SAVE
      INTEGER IK(100),JK(100)
      DO 100 K=1,NORDER

c     find largest element array(i,j) in rest of matrix....

         AMAX=0.
 21      DO 31 I=K,NORDER
            DO 30 J=K,NORDER
               IF (DABS(AMAX)-DABS(ARRAY(I,J))) 24,24,30
 24            AMAX=ARRAY(I,J)
               IK(K)=I
               JK(K)=J
 30         CONTINUE
 31      CONTINUE

C     interchange rows and columns to put amax in array(k,k)....

         IF (AMAX) 41,140,41
 41      I=IK(K)
         IF (I-K) 21,51,43
 43      DO 50 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=ARRAY(I,J)
            ARRAY(I,J)=-SAVE
 50      CONTINUE
 51      J=JK(K)
         IF (J-K) 21,61,53
 53      DO 60 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=ARRAY(I,J)
            ARRAY(I,J)=-SAVE
 60      CONTINUE

C     accumulate elements of inverse matrix....

 61      DO 70 I=1,NORDER
            IF (I-K) 63,70,63
 63         ARRAY(I,K)=-ARRAY(I,K)/AMAX
 70      CONTINUE
         DO 81 I=1,NORDER
            DO 80 J=1,NORDER
               IF (I-K) 74,80,74
 74            IF (J-K) 75,80,75
 75            ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
 80         CONTINUE
 81      CONTINUE
         DO 90 J=1,NORDER
            IF (J-K) 83,90,83
 83         ARRAY(K,J)=ARRAY(K,J)/AMAX
 90      CONTINUE
         ARRAY(K,K)=1./AMAX
 100  CONTINUE

C     restore ordering of matrix....

      DO 130 L=1,NORDER
         K=NORDER-L+1
         J=IK(K)
         IF (J-K) 111,111,105
 105     DO 110 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=-ARRAY(I,J)
            ARRAY(I,J)=SAVE
 110     CONTINUE
 111     I=JK(K)
         IF (I-K) 130,130,113
 113     DO 120 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=-ARRAY(I,J)
            ARRAY(I,J)=SAVE
 120     CONTINUE
 130  CONTINUE
 140  RETURN
      END


      FUNCTION Finvert(y0)
C     Inverting the monoton increasing function r=F(z) -> z=Finv(r) 
C     This means to find the z-value giving the value y0 
C     The function F is the cummulative Gauss function F=1/2(1+erf(z/sqrt(2)))
      REAL xl,xh,yl,yh,x,y,y0
      xl =-3.0
      xh = 3.0
      x  = 0.0
      yl = 0.0
      yh = 1.0
      y  = 0.5
      DO WHILE (ABS(y-y0).GT.0.001)
         x = xl + (xh-xl)*(y0-yl)/(yh-yl)
         y = 0.5*(1.+erf(x/1.414213562))
         IF(y.GT.y0)THEN
            yl=y
            xl=x
         ELSE
            yh=y
            xh=x
         ENDIF
      ENDDO
      Finvert=x
      RETURN
      END


      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(4096),y2a(4096),ya(4096)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
 1    if (khi-klo.gt.1) then
         k=(khi+klo)/2
         if(xa(k).gt.x)then
            khi=k
         else
            klo=k
         endif
         goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      END


      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n
      REAL yp1,ypn,x(4096),y(4096),y2(4096)
      INTEGER i,k
      REAL p,qn,sig,un,u(4096)
      if (yp1.gt..99e30) then
         y2(1)=0.
         u(1)=0.
      else
         y2(1)=-0.5
         u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
         sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
         p=sig*y2(i-1)+2.
         y2(i)=(sig-1.)/p
         u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
 11   continue
      if (ypn.gt..99e30) then
         qn=0.
         un=0.
      else
         qn=0.5
         un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
         y2(k)=y2(k)*y2(k+1)+u(k)
 12   continue
      return
      END
