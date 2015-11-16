      PROGRAM caloric
      INTEGER n,i,j,run
      REAL pi,rho0,rho1,E0,delta,xn,S,xi,xj,dae,dat,da,ES,TS,E,T,P,se,st,dE,dT
      REAL angle
      LOGICAL Cr
      WRITE(6,*)' ______________________'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|     CALORIC  1.0     |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'| Program to calculate |'
      WRITE(6,*)'| a caloric curve from |'
      WRITE(6,*)'|  A_T*dE=A_E*dT with  |'
      WRITE(6,*)'|  A(E,T)=LN(P(E,T)),  |'
      WRITE(6,*)'| P=Omega*EXP(-E/T)/Z  |'
      WRITE(6,*)'|  and Omega(E), Z(T)  |'
      WRITE(6,*)'|  microcanonical and  |'
      WRITE(6,*)'| canonical  partition |'
      WRITE(6,*)'|  functions  derived  |'
      WRITE(6,*)'| from  a simple model |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|  Created:  06/05/03  |'
      WRITE(6,*)'|   Andreas Schiller   |'
      WRITE(6,*)'|  Lawrence Livermore  |'
      WRITE(6,*)'| National  Laboratory |'
      WRITE(6,*)'|______________________|'
      pi=ACOS(-1.)
      rho0=1.
      rho1=0.3
      E0=1.3
      delta=0.8
      n=1
      WRITE(6,101)n
 101  FORMAT('Energy dependence of level density E**n with n=<',I1,'>:',$)
      CALL READI(5,n)
      IF(n.LE.0)STOP 'n>0 required'
      xn=REAL(n)
      WRITE(6,102)rho0
 102  FORMAT('Level density rho_0*E**n with rho_0=<',F3.1,'> MeV**-(n+1):',$)
      CALL READF(5,rho0)
      IF(rho0.LE.0.)STOP 'rho_0>0 required'
      WRITE(6,103)E0
 103  FORMAT('Center of level density dip E0=<',F3.1,'> MeV:',$)
      CALL READF(5,E0)
      IF((E0.LE.0.).OR.(E0.GT.2.))STOP 'Value out of range 0-2 MeV'
      WRITE(6,104)delta
 104  FORMAT('Width of level density dip delta=<',F3.1,'> MeV:',$)
      CALL READF(5,delta)
      IF((delta.LE.0.).OR.(delta.GE.E0))STOP 'E0>delta>0 required'
      WRITE(6,105)rho1
 105  FORMAT('Depth of level density dip rho_1=<',F3.1,'> MeV**-1:',$)
      CALL READF(5,rho1)
      IF(rho1.LT.0.)STOP 'rho_1>=0 required'
      S=SOLUTION(n,xn)
      WRITE(6,'(A23,F10.8,A2)')'Asymptotic solution: E=',S,'*T'
      OPEN(11,ERR=196,FILE='direcx.dat')
      OPEN(12,ERR=196,FILE='direcy.dat')
      DO 107,i=2,38,2
         xi=i/10.
         DO 106,j=2,38,2
            xj=j/10.
            dae=dLNOE(xi,rho0,rho1,E0,delta,n,xn,pi)-1./xj
            dat=xi/(xj**2.)-dLNZT(xj,rho0,rho1,E0,delta,n,xn,pi)
            da=SQRT(dae**2.+dat**2.)
            dae=0.1*dae/da
            dat=0.1*dat/da
            WRITE(11,'(X,F10.7)',ERR=198)dae
            WRITE(12,'(X,F10.7)',ERR=198)dat
 106     CONTINUE
 107  CONTINUE
      CLOSE(11)
      CLOSE(12)
      Em=0.
      Tm=0.
      Cr=.TRUE.
      CALL SADDLE(rho0,rho1,E0,delta,n,xn,pi,Em,Tm,Ea,Ta,Cr)
      IF(Cr.EQ..TRUE.)THEN
         WRITE(6,'(A16)')'Maximum found at'
         WRITE(6,'(A2,F8.6,A8,F8.6,A4)')'E=',Em,' MeV, T=',Tm,' MeV'
         WRITE(6,'(A20)')'Saddlepoint found at'
         WRITE(6,'(A2,F8.6,A8,F8.6,A4)')'E=',Ea,' MeV, T=',Ta,' MeV'
      ELSE
         WRITE(6,'(A36)')'No crossings of caloric curves found'
      ENDIF
      ES=5.*E0
      TS=ES/S
      E=ES
      T=TS
      WRITE(6,108)E
 108  FORMAT('Starting point E=<',F4.1,'> MeV:',$)
      CALL READF(5,E)
      IF((E.LT.0.).OR.(E.GT.ES))STOP 'ES>=E>0 required'
      WRITE(6,109)T
 109  FORMAT('Starting point T=<',F4.1,'> MeV:',$)
      CALL READF(5,T)
      IF((T.LT.0.).OR.(T.GT.TS))STOP 'TS>=T>0 required'
      P=A(E,T,rho0,rho1,E0,delta,n,xn,pi)      
      OPEN(13,ERR=196,FILE='caloric.dat')
      run=0
 110  CONTINUE
      run=run+1
      WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      j=1
      i=0
 111  dae=dLNOE(E,rho0,rho1,E0,delta,n,xn,pi)-1./T
      dat=E/(T**2.)-dLNZT(T,rho0,rho1,E0,delta,n,xn,pi)
      da=SQRT(dae**2.+dat**2.)
      IF(da.LT.1.E-6)GOTO 112
      dae=dae/da
      dat=dat/da
      se=SIGN(1.,dae)
      st=SIGN(1.,dat)
      IF(ABS(dae).GT.ABS(dat))THEN
         dE=1.E-6*ES*se
         dT=dat*dE/dae
         E=E+dE
         T=T+dT
      ELSE
         dT=1.E-6*TS*st
         dE=dae*dT/dat
         T=T+dT
         E=E+dE
      ENDIF
      P=A(E,T,rho0,rho1,E0,delta,n,xn,pi)
      i=i+1
      IF(((1.E-2*ES).GE.E).AND.((1.E-2*TS).GE.T))GOTO 112
      IF(((1.E-4*ES).GE.ABS(E-Em)).AND.((1.E-4*TS).GE.ABS(T-Tm)))GOTO 112
      i=MOD(i,10000)
      IF(i.EQ.0)THEN
         j=j+1
         WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      ENDIF
      GOTO 111
 112  WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      j=j+1
      WRITE(6,'(A14,I5,A12)')'Stopped after ',j,' data points'
      WRITE(6,'(A4,I5,A12)')'and ',i,' minor steps'
      WRITE(6,'(A18)')'Final results are:'
      WRITE(6,'(A3,E14.8)')' E=',E
      WRITE(6,'(A3,E14.8)')' T=',T
      WRITE(6,'(A3,E14.8)')' A=',P
      WRITE(6,'(A3,E14.8)')'AE=',dae
      WRITE(6,'(A3,E14.8)')'AT=',dat
      IF((Cr.EQ..TRUE.).AND.(run.LT.3))THEN
         IF(((1.E-2*ES).GE.E).AND.((1.E-2*TS).GE.T))THEN
            WRITE(6,'(A45)')'Missed maximum, calculation stopped at origin'
            WRITE(6,'(A42)')'No calculations performed from saddlepoint'
            GOTO 114
         ELSE
            E=Ea
            T=Ta
            IF(run.EQ.1)THEN
               angle=340.
               WRITE(6,113)angle
 113           FORMAT('Angle in degrees <',F5.1,'>:',$)
               CALL READF(5,angle)
               IF((angle.LT.0.).OR.(angle.GE.360.))STOP '360>angle>=0 required'
               dE=1.E-6*COS(angle*pi/180.)
               dT=1.E-6*SIN(angle*pi/180.)
               WRITE(6,'(A32)')'Start first run from saddlepoint'
            ELSE
               dE=-1.E-6*COS(angle*pi/180.)
               dT=-1.E-6*SIN(angle*pi/180.)
               WRITE(6,'(A33)')'Start second run from saddlepoint'
            ENDIF
            E=E+dE
            T=T+dT
            GOTO 110
         ENDIF
      ENDIF
 114  CLOSE(13)
      STOP
 196  WRITE(6,197)
 197  FORMAT('Error during opening file')
      STOP
 198  WRITE(6,199)j,i
 199  FORMAT('Error during writing element beta=',I3,' tau=',I3)
      STOP
      END

C checking for saddlepoint
      
      SUBROUTINE SADDLE(rho0,rho1,E0,delta,n,xn,pi,Em,Tm,Ea,Ta,Cr)
      REAL E,dE,E1,T,rho0,rho1,E0,delta,xn,pi,s,sw,Em,Tm,Ea,Ta
      INTEGER n
      LOGICAL Cr
      sw=1.
      E=E0+1.1*delta
      E1=E+.1*delta
      dE=delta
 201  dE=-.1*dE
 202  s=SIGN(1.,E-E1)
      E=E+dE
      IF((1.E-2*5.*E0).GE.E)GOTO 207
      T=1./dLNOE(E,rho0,rho1,E0,delta,n,xn,pi)
      E1=T**2.*dLNZT(T,rho0,rho1,E0,delta,n,xn,pi)
      IF((ABS(E-E1)/(E+E1)).LT.1.E-6)sw=0.
      IF(sw*s*(E-E1))201,203,202
 203  Em=E
      Tm=T
      sw=1.
      dE=delta
 204  dE=-.1*dE
 205  s=SIGN(1.,E1-E)
      E=E+dE
      T=1./dLNOE(E,rho0,rho1,E0,delta,n,xn,pi)
      E1=T**2.*dLNZT(T,rho0,rho1,E0,delta,n,xn,pi)
      IF((ABS(E-E1)/(E+E1)).LT.1.E-6)sw=0.
      IF(sw*s*(E1-E))204,206,205
 206  Ea=E
      Ta=T
      RETURN
 207  Cr=.FALSE.
      RETURN
      END

C asymptotic solution 

      FUNCTION SOLUTION(n,xn)
      REAL xn,s1,s2,s3,s4
      INTEGER n,n1,n2
      n1=4*n**4+12*n**3+20*n**2-16*n+3
      n2=2*n**3+6*n**2+24*n-7
      n3=n+1
      n4=n**2+2*n-2
      s1=SQRT(REAL(n1)/108.)
      s2=(REAL(n2)/54.+s1)**(1./3.)
      s3=REAL(n3)/3.+s2
      s4=REAL(n4)/(9.*s2)
      SOLUTION=s3+s4
      RETURN
      END
      
C Gamma function

      FUNCTION GAMMA(n)
      INTEGER n,m
      GAMMA=1.
      DO 1001,m=1,n-1
         GAMMA=GAMMA*REAL(m)
 1001 CONTINUE
      RETURN
      END

C microcanonical partition function

      FUNCTION OMEGA(E,rho0,rho1,E0,delta,n,xn,pi)
      REAL E,rho0,rho1,E0,delta,xn,pi
      INTEGER n
      OMEGA=rho0*E**xn
      IF(ABS(E-E0).LT.delta)OMEGA=OMEGA-rho1*(1.+COS(pi*(E-E0)/delta))/2.
      RETURN
      END

C canonical partition function

      FUNCTION Z(T,rho0,rho1,E0,delta,n,xn,pi)
      REAL T,rho0,rho1,E0,delta,xn,pi
      INTEGER n
      Z=rho0*GAMMA(n+1)*T**(xn+1.)
      Z=Z-rho1*pi**2.*T**3.*EXP(-E0/T)*SINH(delta/T)/(delta**2.+pi**2.*T**2.)
      RETURN
      END

C logarithmic probability function

      FUNCTION A(E,T,rho0,rho1,E0,delta,n,xn,pi)
      REAL E,T,rho0,rho1,E0,delta,xn,pi
      INTEGER n
      A=OMEGA(E,rho0,rho1,E0,delta,n,xn,pi)*EXP(-E/T)
      A=A/Z(T,rho0,rho1,E0,delta,n,xn,pi)
      A=LOG(A)
      RETURN
      END

C temperature derivative of the logarithmic probability function

      FUNCTION dLNZT(T,rho0,rho1,E0,delta,n,xn,pi)
      REAL T,rho0,rho1,E0,delta,xn,pi
      INTEGER n
      REAL AT1,AT2,AT3
      AT1=(xn-2.)*T-E0+delta/TANH(delta/T)
      AT1=rho1*(AT1+2.*pi**2.*T**3./(delta**2.+pi**2.*T**2.))
      AT2=rho0*GAMMA(n+1)*T**xn*EXP(E0/T)*(delta**2.+pi**2.*T**2.)
      AT2=AT2/(pi**2.*SINH(delta/T))
      AT2=AT2-rho1*T**2.
      AT3=(xn+1.)/T
      dLNZT=AT3+AT1/AT2
      RETURN
      END

C energy derivative of the logarithmic probability function

      FUNCTION dLNOE(E,rho0,rho1,E0,delta,n,xn,pi)
      REAL E,rho0,rho1,E0,delta,xn,pi
      INTEGER n
      REAL AE1,AE2
      IF(ABS(E-E0).GE.delta)THEN
         dLNOE=xn/E
      ELSE
         AE1=rho0*xn*E**(xn-1.)+rho1*SIN(pi*(E-E0)/delta)*pi/(2.*delta)
         AE2=rho0*E**xn-rho1*(1.+COS(pi*(E-E0)/delta))/2.
         dLNOE=AE1/AE2
      ENDIF
      RETURN
      END
