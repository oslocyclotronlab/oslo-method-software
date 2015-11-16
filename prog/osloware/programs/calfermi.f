      PROGRAM caloric
      INTEGER i,j
      REAL pi,rho0,xi,xj,dae,dat,da,ES,TS,E,T,P,se,st,dE,dT
      WRITE(6,*)' ______________________'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|     CALFERMI 1.0     |'
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
      WRITE(6,*)'|   from a Fermi gas   |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|  Created:  06/06/03  |'
      WRITE(6,*)'|   Andreas Schiller   |'
      WRITE(6,*)'|  Lawrence Livermore  |'
      WRITE(6,*)'| National  Laboratory |'
      WRITE(6,*)'|______________________|'
      pi=ACOS(-1.)
      rho0=1.
      a=1.
      WRITE(6,101)a
 101  FORMAT('Level density parameter a=<',F3.1,'> MeV**-1:',$)
      CALL READF(5,a)
      IF(a.LE.0)STOP 'a>0 required'
      WRITE(6,102)rho0
 102  FORMAT('Level density rho_0*EXP(2*SQRT(aE)) with rho_0=<',F3.1,'> MeV**-1):',$)
      CALL READF(5,rho0)
      IF(rho0.LE.0.)STOP 'rho_0>0 required'
      OPEN(11,ERR=196,FILE='direcx.dat')
      OPEN(12,ERR=196,FILE='direcy.dat')
      DO 104,i=2,38,2
         xi=i/10.
         DO 103,j=2,38,2
            xj=j/10.
            dae=dLNOE(xi,a)-1./xj
            dat=xi/(xj**2.)-dLNZT(xj,a,pi)
            da=SQRT(dae**2.+dat**2.)
            dae=0.1*dae/da
            dat=0.1*dat/da
            WRITE(11,'(X,F10.7)',ERR=198)dae
            WRITE(12,'(X,F10.7)',ERR=198)dat
 103     CONTINUE
 104  CONTINUE
      CLOSE(11)
      CLOSE(12)
      ES=20./a
      TS=(SQRT(9.+16.*a*ES)-3.)/(4.*a)
      E=ES
      T=TS
      WRITE(6,105)E
 105  FORMAT('Starting point E=<',F4.1,'> MeV:',$)
      CALL READF(5,E)
      IF((E.LT.0.).OR.(E.GT.ES))STOP 'ES>=E>0 required'
      WRITE(6,106)T
 106  FORMAT('Starting point T=<',F4.1,'> MeV:',$)
      CALL READF(5,T)
      IF((T.LT.0.).OR.(T.GT.TS))STOP 'TS>=T>0 required'
      P=AN(E,T,a,pi)
      OPEN(13,ERR=196,FILE='caloric.dat')
      WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      j=1
      i=0
 107  dae=dLNOE(E,a)-1./T
      dat=E/(T**2.)-dLNZT(T,a,pi)
      da=SQRT(dae**2.+dat**2.)
      IF(da.LT.1.E-6)GOTO 108
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
      P=AN(E,T,a,pi)
      i=i+1
      IF(((1.E-2*ES).GE.E).AND.((1.E-2*TS).GE.T))GOTO 108
      i=MOD(i,10000)
      IF(i.EQ.0)THEN
         j=j+1
         WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      ENDIF
      GOTO 107
 108  WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      j=j+1
      WRITE(6,'(A14,I5,A12)')'Stopped after ',j,' data points'
      WRITE(6,'(A4,I5,A12)')'and ',i,' minor steps'
      WRITE(6,'(A18)')'Final results are:'
      WRITE(6,'(A3,E14.8)')' E=',E
      WRITE(6,'(A3,E14.8)')' T=',T
      WRITE(6,'(A3,E14.8)')' A=',P
      WRITE(6,'(A3,E14.8)')'AE=',dae
      WRITE(6,'(A3,E14.8)')'AT=',dat
      CLOSE(13)
      STOP
 196  WRITE(6,197)
 197  FORMAT('Error during opening file')
      STOP
 198  WRITE(6,199)j,i
 199  FORMAT('Error during writing element beta=',I3,' tau=',I3)
      STOP
      END

C microcanonical partition function

      FUNCTION OMEGA(E,rho0,a)
      REAL E,rho0,a
      OMEGA=rho0*EXP(2.*SQRT(a*E))
      RETURN
      END

C canonical partition function

      FUNCTION Z(T,rho0,a,pi)
      REAL T,rho0,a,pi,alpha
      alpha=SQRT(pi*a*T)*EXP(a*T)*(1.+ERF(SQRT(a*T)))
      Z=rho0*T*(1.+alpha)
      RETURN
      END

C logarithmic probability function

      FUNCTION AN(E,T,a,pi)
      REAL E,T,a,pi,alpha
      alpha=SQRT(pi*a*T)*EXP(a*T)*(1.+ERF(SQRT(a*T)))
      AN=2.*SQRT(a*E)-E/T-LOG(T*(1.+alpha))
      RETURN
      END

C temperature derivative of the logarithmic probability function

      FUNCTION dLNZT(T,a,pi)
      REAL T,a,pi,alpha
      alpha=SQRT(pi*a*T)*EXP(a*T)*(1.+ERF(SQRT(a*T)))
      dLNZT=a+(1.+1./(2.*(1.+1./alpha)))/T
      RETURN
      END

C energy derivative of the logarithmic probability function

      FUNCTION dLNOE(E,a)
      REAL E,a
      dLNOE=SQRT(a/E)
      RETURN
      END
