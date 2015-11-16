        PROGRAM pet
        WRITE(6,*)' _________________________________________________________'
        WRITE(6,*)'|                                                          |'
        WRITE(6,*)'|                     M O M E N T S  1.0                   |'
        WRITE(6,*)'|                                                          |'
        WRITE(6,*)'|     Program to calculate thermodynamical quantities in   |'
        WRITE(6,*)'|       canonical ensemble from the level density Rho      |'
        WRITE(6,*)'|                                                          |'
        WRITE(6,*)'| Partition function: Z  =Sum(e^-E/T)W(E)dE                |'
        WRITE(6,*)'| First moment:       mu1=(1/Z)*Sum(e^-E/T)W(E)(E- 0 )^1dE |'
        WRITE(6,*)'| Second moment:      mu2=(1/Z)*Sum(e^-E/T)W(E)(E-<E>)^2dE |'
        WRITE(6,*)'| Third moment:       mu3=(1/Z)*Sum(e^-E/T)W(E)(E-<E>)^3dE |'
        WRITE(6,*)'| Energy centroid :   <E>=mu1                              |'
        WRITE(6,*)'| Heat capacity:      Cv =mu2/T^2                          |'
        WRITE(6,*)'| Skewness:           gam=mu3/mu2^3/2                      |'
        WRITE(6,*)'| Created:            January 2003                         |'
        WRITE(6,*)'| Modified:           07/01/03                             |'
        WRITE(6,*)'|                Oslo Cyclotron Laboratory                 |'
        WRITE(6,*)'|                     Magne Guttormsen                     |'
        WRITE(6,*)'|__________________________________________________________|'
        CALL Moments
        END


        SUBROUTINE Moments
C       Read/write stuff (mama)
        COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
        COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
        INTEGER XDIM,YDIM
        COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
        CHARACTER fname*8,comm*60
        CHARACTER APP*4
        
C       Stuff for the canonical part 
        REAL aT0,aT1
        REAL a0, a1
        INTEGER idime, idimt
        DOUBLE PRECISION rho(0:8191),pet(1:5),Zt(1:5)
        DOUBLE PRECISION E, Eave1, Eave2, Eave3, x, y, ww, Zz, T, Cv

C       Reading in semiexperimental rho-function - if required
        WRITE(6,*)' '
        WRITE(6,*)'You may read in an extrapolated rho-function for the canonical'
        WRITE(6,*)'ensemble that should cover the energy region 0 - 30 MeV.'           
        WRITE(6,*)' '
        WRITE(6,*)'Please, answer 1 and the name of your extrapolated singles '
        WRITE(6,*)'spectrum (rhotmopaw.cnt) in the two next questions... '
        WRITE(6,*)' '
        IDEST = 1
        ITYPE = 1
        CALL READFILE
        idime = MAXCH+1
        DO i=0, idime-1
           rho(i)=rSPEC(1,i)
        ENDDO
        bx=cal(2,IDEST,1,1)+cal(2,IDEST,1,2)+cal(2,IDEST,1,3)
        IF(bx.EQ.1.)THEN     
           a0=100.              !Defaults in case of no calibration
           a1=200.
        ELSE
           a0=cal(2,IDEST,1,1)  !Picks the spectrum calibration
           a1=cal(2,IDEST,1,2)
        ENDIF
        
        WRITE(6,2)a0
 2      FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
        CALL READF(5,a0)
        IF(a1.EQ.0.)a1=120.
        WRITE(6,4)a1
 4      FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
        CALL READF(5,a1)
        per     = 1000.
        WRITE(6,3)per
 3      FORMAT('The Rho values are per keV(1.) or per MeV(1000.) <',F5.0,'>:',$)
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
 20     FORMAT('Shift canonical entropy with S0 <',F5.2,'>:',$)
        CALL READF(5,S0c)

C       Finding mLow and mHigh markers
        mLow  = 0
        mHigh = MAXCH
        DO i=0,MAXCH
           IF(rho(i).NE.0.AND.mLow.EQ.0)mLow=i
        ENDDO
        DO i=MAXCH,0,-1
           IF(rho(i).NE.0.AND.mHigh.EQ.MAXCH)mHigh=i
        ENDDO

        aT0     = 0.            ! Calibration for temperatures
        aT1     = 10.
        Tmax    = 1500.         ! Maximal temperature
        idimt   = INT(((Tmax-aT0)/aT1) + 0.5) +1

        Ezmax   = a0 + mHigh*a1
        WRITE(6,*)' '
        WRITE(6,6)Ezmax
 6      FORMAT('Give upper energy for partition function Z (keV) <',F8.1,'>:',$)
        CALL READF(5,Ezmax)
        mhZ     = INT(((Ezmax - a0)/a1) + 0.5)
        mlZ     = 0

        OPEN(UNIT=11,FILE='tecg.mom')
        WRITE(6 ,8)a0,a1,idime,mLow,mHigh
        WRITE(11,8)a0,a1,idime,mLow,mHigh
 8      FORMAT(/,'Energy calibration: a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     x         /,      'Dimension:',I4,',  channels used: ',I4,' - ',I4)
        WRITE(6 ,10)aT0,aT1,idimt,0,idimt-1
        WRITE(11,10)aT0,aT1,idimt,0,idimt-1
 10     FORMAT(/,'Temperature calibration: aT0=',F7.2,'keV and aT1=',F7.2,'keV/ch',
     x         /,      'Dimension:',I4,',  channels used: ',I4,' - ',I4)

        WRITE(11,12)fname(2,1)
 12     FORMAT('Input filename (only 8 first characters given): ',A8)
        WRITE(11,14)comm(2,1)
 14     FORMAT('Comments for input file: ',A60)

        WRITE(6,*)' '
        WRITE(6,*) '  ch      T        <Ex>         (Cv)      <gamma>'
        WRITE(11,*)'  ch      T        <Ex>         (Cv)      <gamma>'
        
        DO i = 1, idimt-1
           T     = aT0 + aT1*FLOAT(i)
           Eave1 = 0.
           Eave2 = 0.
           Eave3 = 0.
           Zz    = 0.
           DO j  = mlZ, mhZ
              E     = a0    + a1*FLOAT(j)
              x     = E/T
              y     = 0
              ww    = (rho(j)/per)*exp(S0c)
              IF(x.LT.200)y = DEXP(-x)*ww*a1
              Eave1 = Eave1 +     E*y
              Eave2 = Eave2 +   E*E*y
              Eave3 = Eave3 + E*E*E*y
              Zz    = Zz    +       y
           ENDDO

           Eave1  = Eave1/Zz                           
           Eave2  = Eave2/Zz
           Eave3  = Eave3/Zz
           x1     = Eave1       ! first moment
           x2     = Eave2-Eave1**2 ! second moment
           x3     = Eave3-3.*Eave2*Eave1+2.0*Eave1**3 ! third momenr
           Cv     = x2/(T*T)
           gamma  = x3/(x2**(1.5))
           IF(i.GT.1)WRITE(6,16) i, T, Eave1, Cv, gamma 
           IF(i.GT.1)WRITE(11,16)i, T, Eave1, Cv, gamma
 16        FORMAT(I5, F8.1, 3F12.2)
        ENDDO
        CLOSE(11)

        OPEN(UNIT=12,FILE='pet.mom')
        WRITE(6,*)  '       E      T=0.5       T=1.0        T=1.2        T=1.5        T=2.0'
        WRITE(12,*) '       E      T=0.5       T=1.0        T=1.2        T=1.5        T=2.0'
        T = 0.
        DO i = 1, 5             ! Making Z(T) for 5 temperatures
           if(i.EQ.1)T =  500.
           if(i.EQ.2)T = 1000.
           if(i.EQ.3)T = 1200.
           if(i.EQ.4)T = 1500.
           if(i.EQ.5)T = 2000.
           Zz    = 0.
           DO j  = mlZ, mhZ
              E     = a0    + a1*FLOAT(j)
              x     = E/T
              y     = 0
              ww    = (rho(j)/per)*exp(S0c)
              IF(x.LT.200)y = DEXP(-x)*ww*a1
              Zz    = Zz    +       y
           ENDDO
           Zt(i)=Zz
        ENDDO

        DO j  = mlZ, mhZ
           E  = a0 + a1*FLOAT(j)
           T = 0
           DO i = 1, 5         
              if(i.EQ.1)T =  500.
              if(i.EQ.2)T = 1000.
              if(i.EQ.3)T = 1200.
              if(i.EQ.4)T = 1500.
              if(i.EQ.5)T = 2000.           
              x     = E/T
              y     = 0
              ww    = (rho(j)/per)*exp(S0c)
              IF(x.LT.200)y = DEXP(-x)*ww*a1*(1000./a1)
              pet(i)    = (y/Zt(i))
           ENDDO
           WRITE(6,17) E,pet(1),pet(2),pet(3),pet(4),pet(5)
           WRITE(12,17)E,pet(1),pet(2),pet(3),pet(4),pet(5)
 17        FORMAT(F12.1,5F12.8)
        ENDDO
        
        CLOSE(12)

        GO TO 98  
        WRITE(6,*)'Could not open file'
 98     CONTINUE
        RETURN
        END
