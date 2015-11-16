      PROGRAM caloric
      INTEGER i,j,run
      REAL pi,rho1,rho2,E1,E2,delta,xi,xj,dae,dat,da,Ea1,Ta1,Ea2,Ta2,Ea3,Ta3
      REAL Em,ES,TS,ES1,TS1,ES2,TS2,E,T,P,se,st,dE,dT
      WRITE(6,*)' ______________________'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|      CALTWO 1.0      |'
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
      WRITE(6,*)'|   from a two-level   |'
      WRITE(6,*)'|        scheme        |'
      WRITE(6,*)'|                      |'
      WRITE(6,*)'|  Created:  06/16/03  |'
      WRITE(6,*)'|   Andreas Schiller   |'
      WRITE(6,*)'|  Lawrence Livermore  |'
      WRITE(6,*)'| National  Laboratory |'
      WRITE(6,*)'|______________________|'
      pi=ACOS(-1.)
      rho1=2.
      WRITE(6,101)rho1
 101  FORMAT('Degeneracy of the lower level d1=<',F3.1,'>:',$)
      CALL READF(5,rho1)
      IF(rho1.LE.0.)STOP 'd1>0 required'
      rho2=2.*rho1
      WRITE(6,102)rho2
 102  FORMAT('Degeneracy of the higher level d2=<',F3.1,'>:',$)
      CALL READF(5,rho2)
      IF(rho2.LE.0.)STOP 'd2>0 required'
      delta=1.
      WRITE(6,103)delta
 103  FORMAT('Common half-width of levels delta=<',F3.1,'> MeV:',$)
      CALL READF(5,delta)
      IF(delta.LE.0.)STOP 'delta>0 required'
      E1=delta
      WRITE(6,104)E1
 104  FORMAT('Energy of the lower level E1=<',F3.1,'> MeV:',$)
      CALL READF(5,E1)
      IF(E1.LT.delta)STOP 'E1>=delta required'
      E2=E1+1.5*delta
      WRITE(6,105)E2
 105  FORMAT('Energy of the higher level E2=<',F3.1,'> MeV:',$)
      CALL READF(5,E2)
      IF(E2.LE.E1)STOP 'E2>E1 required'
      Em=(rho1*E1+rho2*E2)/(rho1+rho2)
      WRITE(6,'(A39,F10.6,A4)')'Maximum energy of the caloric curve Em=',Em,' MeV'
      OPEN(11,ERR=196,FILE='direcx.dat')
      OPEN(12,ERR=196,FILE='direcy.dat')
      DO 107,i=2,38,2
         xi=REAL(i)/10.
         DO 106,j=2,38,2
            xj=REAL(j)/10.
            dae=dLNOE(xi,rho1,rho2,E1,E2,delta,pi)-1./xj
            dat=xi/(xj**2.)-dLNZT(xj,rho1,rho2,E1,E2,delta,pi)
            da=SQRT(dae**2.+dat**2.)
            IF(da.GT.1.E-6)THEN
               dae=0.1*dae/da
               dat=0.1*dat/da
            ELSE
               dae=0.
               dat=0.
            ENDIF
            WRITE(11,'(X,F10.7)',ERR=198)dae
            WRITE(12,'(X,F10.7)',ERR=198)dat
 106     CONTINUE
 107  CONTINUE
      CLOSE(11)
      CLOSE(12)
      CALL SADDLE(rho1,rho2,E1,E2,delta,pi,Em,Ea1,Ta1,Ea2,Ta2,Ea3,Ta3)
      ES1=0.
      ES2=0.
      TS1=0.
      TS2=0.
      TS3=0.
      TS4=0.
      IF((E2-E1).GE.2.*delta)THEN
         WRITE(6,'(A26)')'No saddlepoint encountered'
         IF((Em.LT.(E1+delta)).OR.((ABS(Ea1-E1-delta)/(Ea1+E1+delta)).GT.1.E-6))THEN
            WRITE(6,'(A24)')'No crossings encountered'
            STOP'No further calculations performed'
         ELSE
            WRITE(6,'(A20,F8.5,A8,E12.6,A4)')'First crossing at E=',Ea1,' MeV, T=',Ta1,' MeV'
            ES1=Ea1
            TS1=Ta1
            IF((Em.LT.((E2+E1)/2.)).OR.((ABS(Ea2-(E2+E1)/2.)/(Ea2+(E2+E1)/2.)).GT.1.E-6))THEN
               WRITE(6,'(A32)')'No further crossings encountered'
            ELSE
               WRITE(6,'(A21,F8.5,A8,E12.6,A4)')'Second crossing at E=',Ea2,' MeV, T=',Ta2,' MeV'
               TS1=Ta2
               IF((Em.LT.(E2-delta)).OR.((ABS(Ea3-E2+delta)/(Ea3+E2-delta)).GT.1.E-6))THEN
                  WRITE(6,'(A32)')'No further crossings encountered'
               ELSE
                  WRITE(6,'(A20,F8.5,A8,E12.6,A4)')'Third crossing at E=',Ea3,' MeV, T=',Ta3,' MeV'
                  TS1=(Ta1+2.*Ta2+Ta3)/4.
                  ES2=Ea3
                  TS2=(Ta1+2.*Ta2+Ta3)/4.
                  TS3=MAX((Ta1+2.*Ta2+Ta3)/4.-ABS(Ta1-2.*Ta2+Ta3)/2.,Ta1)
                  TS4=MIN((Ta1+2.*Ta2+Ta3)/4.+ABS(Ta1-2.*Ta2+Ta3)/2.,Ta3)
               ENDIF
            ENDIF
         ENDIF
      ELSE
         WRITE(6,'(A17,F8.5,A8,E12.6,A4)')'Saddlepoint at E=',Ea1,' MeV, T=',Ta1,' MeV'
         ES1=Ea1
         ES2=Ea1
         TS1=Ta1
         TS2=Ta1
      ENDIF
      ES=ES1
      TS=TS1
      run=0
      OPEN(13,ERR=196,FILE='caloric.dat')
      E=ES-1.E-2
      T=TS
 108  run=run+1
      P=A(E,T,rho1,rho2,E1,E2,delta,pi)
      WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      j=1
      i=0
 109  dae=dLNOE(E,rho1,rho2,E1,E2,delta,pi)-1./T
      dat=E/(T**2.)-dLNZT(T,rho1,rho2,E1,E2,delta,pi)
      da=SQRT(dae**2.+dat**2.)
      IF(da.LT.1.E-6)GOTO 110
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
      P=A(E,T,rho1,rho2,E1,E2,delta,pi)
      i=i+1
      IF(((1.E-1*ES1).GE.(E-E1+delta)).AND.((1.E-1*TS1).GE.T))GOTO 110
      IF((1.E1*TS1).LE.T)GOTO 110
      i=MOD(i,10000)
      IF(i.EQ.0)THEN
         j=j+1
         WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      ENDIF
      GOTO 109
 110  WRITE(13,'(X,F10.7,X,F10.7,X,F10.7)',ERR=198)E,T,P
      j=j+1
      WRITE(6,'(A14,I5,A12)')'Stopped after ',j,' data points'
      WRITE(6,'(A4,I5,A12)')'and ',i,' minor steps'
      WRITE(6,'(A18)')'Final results are:'
      WRITE(6,'(A3,E14.8)')' E=',E
      WRITE(6,'(A3,E14.8)')' T=',T
      WRITE(6,'(A3,E14.8)')' A=',P
      WRITE(6,'(A3,E14.8)')'AE=',dae
      WRITE(6,'(A3,E14.8)')'AT=',dat
      IF((ES2.GT.0.).AND.(run.EQ.1))THEN
         ES=ES2
         TS=TS2
         E=ES+1.E-2
         T=TS
         WRITE(6,'(A16)')'Start second run'
         GOTO 108
      ENDIF
      IF(TS3.GT.0.)THEN
         IF(run.EQ.2)THEN
            ES=ES1
            TS=TS3
            E=ES-1.E-2
            T=TS
            WRITE(6,'(A15)')'Start third run'
            GOTO 108
         ENDIF
         IF(run.EQ.3)THEN
            ES=ES1
            TS=TS4
            E=ES-1.E-2
            T=TS
            WRITE(6,'(A15)')'Start forth run'
            GOTO 108
         ENDIF
         IF(run.EQ.4)THEN
            ES=ES2
            TS=TS3
            E=ES+1.E-2
            T=TS
            WRITE(6,'(A15)')'Start fifth run'
            GOTO 108
         ENDIF
         IF(run.EQ.5)THEN
            ES=ES2
            TS=TS4
            E=ES+1.E-2
            T=TS
            WRITE(6,'(A15)')'Start sixth run'
            GOTO 108
         ENDIF
      ENDIF
      CLOSE(13)
      STOP
 196  WRITE(6,197)
 197  FORMAT('Error during opening file')
      STOP
 198  WRITE(6,199)j,i
 199  FORMAT('Error during writing element beta=',I3,' tau=',I3)
      STOP
      END

C checking for saddlepoint
      
      SUBROUTINE SADDLE(rho1,rho2,E1,E2,delta,pi,Em,Ea1,Ta1,Ea2,Ta2,Ea3,Ta3)
      REAL rho1,rho2,E1,E2,delta,pi,Em,Ea1,Ta1,Ea2,Ta2,Ea3,Ta3
      REAL E,dE,EE,T,x,dx,s,sw
      Ea1=0.
      Ta1=0.
      Ea2=0.
      Ta2=0.
      Ea3=0.
      Ta3=0.
      IF((E2-E1).GE.2.*delta)THEN
         IF(Em.LE.(E2-delta))GOTO 204
         E=E2
         sw=1.
         x=4.
         dx=0.1
 201     dx=-.1*dx
         s=SIGN(1.,E-E2+delta)
 202     x=x+dx
         IF((x.LT.0.01).OR.(x.GT.3.99))GOTO 204
         T=TAN(pi*x/8.)
         E=T**2.*dLNZT(T,rho1,rho2,E1,E2,delta,pi)
         IF((ABS(E-E2+delta)/(E+E2-delta)).LT.1.E-6)sw=0.
         IF(sw*s*(E-E2+delta))201,203,202
 203     Ea3=E2-delta
         Ta3=T
 204     IF(Em.LE.((E2+E1)/2.))GOTO 208
         E=E2
         sw=1.
         x=4.
         dx=0.1
 205     dx=-.1*dx
         s=SIGN(1.,E-(E2+E1)/2.)
 206     x=x+dx
         IF((x.LT.0.01).OR.(x.GT.3.99))GOTO 208
         T=TAN(pi*x/8.)
         E=T**2.*dLNZT(T,rho1,rho2,E1,E2,delta,pi)
         IF((ABS(E-(E2+E1)/2.)/(E+(E2+E1)/2.)).LT.1.E-6)sw=0.
         IF(sw*s*(E-(E2+E1)/2.))205,207,206
 207     Ea2=(E2+E1)/2.
         Ta2=T
 208     IF(Em.LE.(E1+delta))GOTO 212
         E=E2
         sw=1.
         x=4.
         dx=0.1
 209     dx=-.1*dx
         s=SIGN(1.,E-E1-delta)
 210     x=x+dx
         IF((x.LT.0.01).OR.(x.GT.3.99))GOTO 212
         T=TAN(pi*x/8.)
         E=T**2.*dLNZT(T,rho1,rho2,E1,E2,delta,pi)
         IF((ABS(E-E1-delta)/(E+E1+delta)).LT.1.E-6)sw=0.
         IF(sw*s*(E-E1-delta))209,211,210
 211     Ea1=E1+delta
         Ta1=T
 212     CONTINUE
         ELSE
            sw=1.
            E=Em+1.1*delta
            EE=E-.1*delta
            dE=delta
 213        dE=-.1*dE
            s=SIGN(1.,E-EE)
 214        E=E+dE
            T=1./dLNOE(E,rho1,rho2,E1,E2,delta,pi)
            EE=T**2.*dLNZT(T,rho1,rho2,E1,E2,delta,pi)
            IF((ABS(E-EE)/(E+EE)).LT.1.E-6)sw=0.
            IF(sw*s*(E-EE))213,215,214
 215        Ea1=E
            Ta1=T
         ENDIF
      RETURN
      END

C microcanonical partition function

      FUNCTION OMEGA(E,rho1,rho2,E1,E2,delta,pi)
      REAL E,rho1,rho2,E1,E2,delta,pi
      OMEGA=0.
      IF(((E1-delta).LE.E).AND.(E.LE.(E1+delta)))
     +     OMEGA=OMEGA+rho1*(1.+COS(pi*(E-E1)/delta))/2.
      IF(((E2-delta).LE.E).AND.(E.LE.(E2+delta)))
     +     OMEGA=OMEGA+rho2*(1.+COS(pi*(E-E2)/delta))/2.
      RETURN
      END

C canonical partition function

      FUNCTION Z(T,rho1,rho2,E1,E2,delta,pi)
      REAL T,rho1,rho2,E1,E2,delta,pi
      Z=(rho1*EXP(-E1/T)+rho2*EXP(-E2/T))
      Z=Z*pi**2.*T**3.*SINH(delta/T)/(delta**2.+pi**2.*T**2.)
      RETURN
      END

C logarithmic probability function

      FUNCTION A(E,T,rho1,rho2,E1,E2,delta,pi)
      REAL E,T,rho1,rho2,E1,E2,delta,pi,om
      A=0.
      om=OMEGA(E,rho1,rho2,E1,E2,delta,pi)
      IF(om.GT.0.)THEN
         A=LOG(om)-E/T
         A=A-LOG(Z(T,rho1,rho2,E1,E2,delta,pi))
      ENDIF
      RETURN
      END

C temperature derivative of the logarithmic probability function

      FUNCTION dLNZT(T,rho1,rho2,E1,E2,delta,pi)
      REAL T,rho1,rho2,E1,E2,delta,pi
      REAL AT1,AT2,AT3
      AT1=rho1*E1*EXP(-E1/T)+rho2*E2*EXP(-E2/T)
      AT1=AT1/(T**2.*(rho1*EXP(-E1/T)+rho2*EXP(-E2/T)))
      AT2=3./T-delta/(T**2.*TANH(delta/T))
      AT3=2.*pi**2.*T/(delta**2.+pi**2.*T**2.)
      dLNZT=AT1+AT2-AT3
      RETURN
      END

C energy derivative of the logarithmic probability function

      FUNCTION dLNOE(E,rho1,rho2,E1,E2,delta,pi)
      REAL E,rho1,rho2,E1,E2,delta,pi
      dLNOE=0.
      IF(((E2-E1-2.*delta)/delta).GT.1.E-2)THEN
         IF(((delta-ABS(E-E1))/delta).GT.1.E-2)
     +        dLNOE=-pi*SIN(pi*(E-E1)/delta)/(delta*(1.+COS(pi*(E-E1)/delta)))
         IF(((delta-ABS(E-E2))/delta).GT.1.E-2)
     +        dLNOE=-pi*SIN(pi*(E-E2)/delta)/(delta*(1.+COS(pi*(E-E2)/delta)))
      ELSE
         IF((((E-E1+delta)/delta).GT.1.E-2).AND.(E.LT.(E2-delta)))
     +        dLNOE=-pi*SIN(pi*(E-E1)/delta)/(delta*(1.+COS(pi*(E-E1)/delta)))
         IF(((E1+delta).LT.E).AND.((E2+delta-E).GT.1.E-2))
     +        dLNOE=-pi*SIN(pi*(E-E2)/delta)/(delta*(1.+COS(pi*(E-E2)/delta)))
      IF(((E2-delta).LE.E).AND.(E.LE.(E1+delta)).AND.((E2-E1).LT.(2.*delta)))
     +dLNOE=-pi*(rho1*SIN(pi*(E-E1)/delta)+rho2*SIN(pi*(E-E2)/delta))/
     +(delta*(rho1*(1.+COS(pi*(E-E1)/delta))+rho2*(1.+COS(pi*(E-E2)/delta))))
      ENDIF
      RETURN
      END
