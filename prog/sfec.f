      PROGRAM sfec
      COMMON/Zfunc/D,T,epsilon,Arot,Avib,attn,xn,nupar
      DOUBLE PRECISION D,T,epsilon,Arot,Avib,attn,xn
      DOUBLE PRECISION A
      DOUBLE PRECISION S,F
      DOUBLE PRECISION Rf(1:1000),rho4
      REAL Starget(1:111),RhoE(1:1000,1:3)
      DOUBLE PRECISION x(1:111,1:3),y(1:111,1:3),ss
C Stuff for spline part
      INTEGER NP
      REAL yp1,ypn,xa(4096),ya(4096),y2(4096)

      Tstart = 50.
      Tstep  = 25.
      ExMin  = 200.    

      OPEN(23,FILE='sfecin.dat',STATUS='old',ERR=666)
      READ(23,*,ERR=666)epsilon4,Amass4,attn4,Arot4,Avib4,nupar,D4,T0,alpha,nuiter
      CLOSE(23)
      WRITE(6,*)'Default values taken from file: sfecin.dat'
      GO TO 777
 666  CONTINUE
      epsilon4 = 130.
      Amass4   = 162.
      attn4    = 1.0
      Avib4    = 900.
      nupar    = 10
      T0       = 0.
      alpha    = 0.
      nuiter   = 2
 777  CONTINUE

      WRITE(6,101) Amass4
 101  FORMAT(' Mass number                                            <',F6.0,'>:',$)
      CALL READF(5,Amass4)
      De4=1000.*12.*Amass4**(-0.5)
      IF(D4.EQ.0)D4=De4
      WRITE(6,102)De4,D4
 102  FORMAT(' Pairing gap parameter (Delta = 12A**-1/2 = ',F6.0,'keV) <',F7.1,'>:',$)
      CALL READF(5,D4)
      WRITE(6,*)'You may reduce Delta as function of T according to the equation:'
      WRITE(6,*)'1/{1+exp[(T-T0)/alpha]}. Use T0 = 0, if you want Delta = const.'

      WRITE(6,108)T0
 108  FORMAT(' Temperature at half Delta T0 (keV)                    <',F7.1,'>:',$)
      CALL READF(5,T0)      
      WRITE(6,109)alpha
 109  FORMAT(' Pairing gap decrease slope parameter alpha (keV)      <',F7.1,'>:',$)
      CALL READF(5,alpha)

      WRITE(6,105)attn4
 105  FORMAT(' Attenuation factor per breaking for Delta               <',F5.3,'>:',$)
      CALL READF(5,attn4)
      WRITE(6,103) epsilon4
 103  FORMAT(' Single particle level spacing (keV)                   <',F7.1,'>:',$)
      CALL READF(5,epsilon4)
      WRITE(6,107) nupar
 107  FORMAT(' Number of pairs to include (max=10)                        <',I2,'>:',$)
      CALL READI(5,nupar)

C Taking into account rotation, Erot=A*R(R+1), R=0,2,4,6
      Rigid=(0.0137/1000.)*(Amass4**(5./3.))  ! I henhold til Richter: 2/5*M*A*R**2, R0=1.25
      Arig4=1./(2.*Rigid)
      IF(Arot4.EQ.0)Arot4=Arig4

      WRITE(6,*)'If no rotational partitionfunction, answer 0 below:'
      WRITE(6,104)Arig4,Arot4
 104  FORMAT(' Rotational parameter (Arig =',F6.1,' keV)                <',F6.1,'>:',$)
      CALL READF(5,Arot4)

C Taking into account vibration, Erot=hw*v, v=0,1,2
      WRITE(6,*)'If no vibrational partitionfunction, answer 0 below:'
      WRITE(6,106)Avib4
 106  FORMAT(' Vibrational quantum energy hw (keV)                    <',F6.0,'>:',$)
      CALL READF(5,Avib4)

      WRITE(6,111) nuiter
 111  FORMAT(' Number of iterations for saddle point approximation (N > 0) <',I1,'>:',$)
      CALL READI(5,nuiter)

      epsilon = epsilon4
      Amass   = Amass4
      attn    = attn4
      D       = D4
      Arot    = Arot4
      Avib    = Avib4

      OPEN(23,FILE='sfecin.dat',ERR=555)
      WRITE(23,*,ERR=555)epsilon4,Amass4,attn4,Arot4,Avib4,nupar,D4,T0,alpha,nuiter
      CLOSE(23)
 555  CONTINUE

      OPEN(24,FILE='sfec.paw',ACCESS='SEQUENTIAL')
      OPEN(23,FILE='my.paw',ACCESS='SEQUENTIAL')

C Calculates See, Soe, Soo N =< 10
      WRITE(6,*)' T(keV)     S(ee)     F(ee)     E(ee)     Cv(ee)'
      T = Tstart
      DO i=1,111
         T4=T
         IF(T0.GT.0.AND.ABS(alpha).NE.0.)D=D4/(1+exp((T4-T0)/alpha))

         A=epsilon/T
         ii=1
         s1=S(A,ii)
         x1=xn
         f1=F(A,ii)
         e1=f1+T*s1
         T = T - 5.
         A =epsilon/T
         el=F(A,ii)+T*S(A,ii)
         fl=F(A,ii)
         xl=xn
         T = T + 10.
         A =epsilon/T
         eh=F(A,ii)+T*S(A,ii)
         fh=F(A,ii)
         xh=xn
         c1=(eh - el)/10.
         IF(ABS(T-300).LE.Tstep)THEN
            clow=c1
            tlow=T
         ENDIF
         IF(ABS(T-1000).LE.Tstep)THEN
            chigh=c1
            thigh=T
         ENDIF
         IF(xh.NE.xl)THEN
            u1=(fh-fl)/(xh-xl)
         ELSE
            u1=0
         ENDIF
         T = T - 5.
         
         A =epsilon/T
         ii=2
         s2=S(A,ii)
         x2=xn
         f2=F(A,ii)
         e2=f2+T*s2
         T = T - 5.
         A =epsilon/T
         el=F(A,ii)+T*S(A,ii)
         fl=F(A,ii)
         xl=xn
         T = T + 10.
         A =epsilon/T
         eh=F(A,ii)+T*S(A,ii)
         fh=F(A,ii)
         xh=xn
         c2=(eh - el)/10.
          IF(xh.NE.xl)THEN
            u2=(fh-fl)/(xh-xl)
         ELSE
            u2=0
         ENDIF
         T = T - 5.

         A =epsilon/T
         ii=3
         s3=S(A,ii)
         x3=xn
         f3=F(A,ii)         
         e3=f3+T*s3
         T = T - 5.
         A =epsilon/T
         el=F(A,ii)+T*S(A,ii)
         fl=F(A,ii)
         xl=xn
         T = T + 10.
         A =epsilon/T
         eh=F(A,ii)+T*S(A,ii)
         fh=F(A,ii)
         xh=xn
         c3=(eh - el)/10.         
          IF(xh.NE.xl)THEN
            u3=(fh-fl)/(xh-xl)
         ELSE
            u3=0
         ENDIF
         T = T - 5.

         iT = T 

         WRITE(23,*)  iT,u1,u2,u3
         WRITE(24,*)  s1,s2,s3,f1,f2,f3,e1,e2,e3,c1,c2,c3,x1,x2,x3
         IF(i-1.EQ.((i-1)/4)*4.OR.i.EQ.1.OR.i.EQ.111) WRITE(6,47)iT,s1,f1,e1,c1
  47     FORMAT(I8,4F10.1)
         T = T + Tstep
      ENDDO  
      CLOSE(24)
      CLOSE(23)


C Calculates Rho(<E>)
      OPEN(25,FILE='rhoe.paw',ACCESS='SEQUENTIAL')
      WRITE(6,*)' T(keV)    <Ex(ee)>    Rho(ee) '
      DO ii=1,3
         T = Tstart
         ibase = 0
         A = epsilon/T
         DO i = 1,111
            T4=T
            IF(T0.GT.0.AND.ABS(alpha).NE.0.)D=D4/(1+exp((T4-T0)/alpha))
            ss = S(A,ii)
            Starget(i) = ss
            x(i,ii) = F(A,ii) + T * ss
            T = T - 5.
            A = epsilon/T
            el = F(A,ii) + T * S(A,ii)
            T = T + 10.
            A = epsilon/T
            eh = F(A,ii) + T * S(A,ii)
            cc = (eh - el) / 10.
            T = T - 5.
            TT = (T/1000.)**2.
            y(i,ii) = dexp(ss) / sqrt(2.*3.14*cc*TT)
            iT = T


c         IF(i-1.EQ.((i-1)/4)*4.OR.i.EQ.1.OR.i.EQ.111) WRITE(6,47)iT,ss,f1,x(i,ii),cc


            IF(ii.EQ.1.AND.((i-1).EQ.((i-1)/4)*4.OR.i.EQ.1.OR.i.EQ.111)) WRITE(6,49)iT,x(i,ii),y(i,ii)
  49        FORMAT(I8,F10.1,E13.3)
            IF(x(i,ii).LE.ExMin)ibase=ibase+1
            T = T + Tstep
         ENDDO
C Interpolating x=<E> and y=Rho with spline-method (From the book: Numerical Recepies)
         NP=111-ibase     
         DO i=1,NP
           xa(i)=x(i+ibase,ii)
           ya(i)=dlog(y(i+ibase,ii))       !Better to interpolate ln      
         ENDDO
         yp1=0               !Calculates the derivatives at end-points
         ypn=0
         dx=xa(2)-xa(1)
         dy=ya(2)-ya(1)
         IF(dx.NE.0)yp1=dy/dx
         dx=xa(NP)-xa(NP-1)
         dy=ya(NP)-ya(NP-1)
         IF(dx.NE.0)ypn=dy/dx
         CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
         DO i=1,1000
            Ex = 0. + 100.*(i-1)
            CALL splint(xa,ya,y2,NP,Ex,rho)  !Interpolating
            rho4  = rho
            Rf(i) = dexp(rho4)               ! Initial Rho assumed 
            IF(Rf(i).GT.y(111,ii)) Rf(i) = y(111,ii)
         ENDDO

         CALL IterateRho(Starget,Rf,nuiter)

         DO i=1,1000
            RhoE(i,ii) = Rf(i)               ! Final Rho assumed 
         ENDDO
         WRITE(6,*)'Please, wait...'
      ENDDO 

      DO i=1,500
         WRITE(25,*)RhoE(i,1),RhoE(i,2),RhoE(i,3)
      ENDDO
      CLOSE(25)

      OPEN(26,FILE='usc.paw',ACCESS='SEQUENTIAL')
C Calculates See, Soe, Soo N =< 10
C Calculates Cv at T=300 and 900 keV, used to subtract linear background in next loops
      T=300.
      T4=T
      IF(T0.GT.0.AND.ABS(alpha).NE.0.)D=D4/(1+exp((T4-T0)/alpha))
      A=epsilon/T
      ii=1
      T = T - 5.
      A =epsilon/T
      el=F(A,ii)+T*S(A,ii)
      fl=F(A,ii)
      T = T + 10.
      A =epsilon/T
      eh=F(A,ii)+T*S(A,ii)
      fh=F(A,ii)
      xh=xn
      c1=(eh - el)/10.
      clow=c1
      tlow=T
      T=900.
      T4=T
      IF(T0.GT.0.AND.ABS(alpha).NE.0.)D=D4/(1+exp((T4-T0)/alpha))
      A=epsilon/T
      ii=1
      T = T - 5.
      A =epsilon/T
      el=F(A,ii)+T*S(A,ii)
      fl=F(A,ii)
      T = T + 10.
      A =epsilon/T
      eh=F(A,ii)+T*S(A,ii)
      fh=F(A,ii)
      xh=xn
      c1=(eh - el)/10.
      chigh=c1
      thigh=T


      WRITE(6,*)' T(keV)  f2-f0+2D   s2-s0   c2-c0   a(ee)   a(oo)'
      T = Tstart
      DO i=1,111
         T4=T
         IF(T0.GT.0.AND.ABS(alpha).NE.0.)D=D4/(1+exp((T4-T0)/alpha))
         A=epsilon/T
         ii=1
         s1=S(A,ii)
         f1=F(A,ii)
         e1=f1+T*s1
         T = T - 5.
         A =epsilon/T
         el=F(A,ii)+T*S(A,ii)
         sl=S(A,ii)
         T = T +10.
         A =epsilon/T
         eh=F(A,ii)+T*S(A,ii)
         sh=S(A,ii)
         c1=(eh - el)/10.
         a1=1000.*(1./2.)*( ((sh-sl)/10.) + (3./(2.*T)))
         T = T - 5.
         
         A =epsilon/T
         ii=3
         s3=S(A,ii)
         f3=F(A,ii)
         e3=f2+T*s3
         T = T - 5.
         A =epsilon/T
         el=F(A,ii)+T*S(A,ii)
         sl=S(A,ii)
         T = T +10.
         A =epsilon/T
         eh=F(A,ii)+T*S(A,ii)
         sh=S(A,ii)
         c3=(eh - el)/10.
         a3=1000.*(1./2.)*(((sh-sl)/10.) + (3./(2.*T)))
         T = T - 5.

         iT = T 
         cb=MAX(0.,clow+((T4-tlow)/(thigh-tlow))*(chigh-clow))
         cnetto=MAX(0.,c1-cb)
         IF(T.LT.300)cnetto=0
         WRITE(26,*)f3-f1+2.*D4,s3-s1,c3-c1,a1,a3,cnetto,dum,dum,dum,dum,dum,dum
         IF(i-1.EQ.((i-1)/4)*4.OR.i.EQ.1.OR.i.EQ.111)WRITE(6,48)iT,f3-f1+2.*D4,s3-s1,c3-c1,a1,a3
  48     FORMAT(I8,5F8.1)
         T = T + Tstep
      ENDDO  

      CLOSE(26)

      END


      FUNCTION Z(A,i)
      COMMON/Zfunc/D,T,epsilon,Arot,Avib,attn,xn,nupar
      DOUBLE PRECISION A,p(1:10)
      DOUBLE PRECISION Z,Zrot,Zvib
      DOUBLE PRECISION D,T,epsilon,Arot,Avib,attn,xn
      DOUBLE PRECISION r2,r3,r4,r5,r6,r7,r8,r9,r10
      DOUBLE PRECISION Z0a,Z1a,Z2a,Z3a,Z4a,Z5a,Z6a,Z7a,Z8a,Z9a,Z10a,Z11a,Z12a,Z13a,Z14a,Z15a,Z16a,Z17a,Z18a,Z19a,Z20a,Z21a
      DOUBLE PRECISION Z0b,Z1b,Z2b,Z3b,Z4b,Z5b,Z6b,Z7b,Z8b,Z9b,Z10b,Z11b,Z12b,Z13b,Z14b,Z15b,Z16b,Z17b,Z18b,Z19b,Z20b,Z21b
      DOUBLE PRECISION Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9,Z10,Z11,Z12,Z13,Z14,Z15,Z16,Z17,Z18,Z19,Z20,Z21
      DOUBLE PRECISION Z2bar,Z4bar,Z6bar,Z8bar,Z10bar,Z12bar,Z14bar,Z16bar,Z18bar,Z20bar,Z22bar

      DO j=1,10
         p(j) = 0.
         IF(j.LE.nupar)p(j) = 1.
      ENDDO

      Z0a =1
      Z1a =Z0a *1.          /(1.-dexp(-1 .*A))
      Z2a =Z1a *dexp(-1. *A)/(1.-dexp(-2. *A))
      Z3a =Z2a *dexp(-2. *A)/(1.-dexp(-3. *A))
      Z4a =Z3a *dexp(-3. *A)/(1.-dexp(-4. *A))
      Z5a =Z4a *dexp(-4. *A)/(1.-dexp(-5. *A))
      Z6a =Z5a *dexp(-5. *A)/(1.-dexp(-6. *A))
      Z7a =Z6a *dexp(-6. *A)/(1.-dexp(-7. *A))
      Z8a =Z7a *dexp(-7. *A)/(1.-dexp(-8. *A))
      Z9a =Z8a *dexp(-8. *A)/(1.-dexp(-9. *A))
      Z10a=Z9a *dexp(-9. *A)/(1.-dexp(-10.*A))
      Z11a=Z10a*dexp(-10.*A)/(1.-dexp(-11.*A))
      Z12a=Z11a*dexp(-11.*A)/(1.-dexp(-12.*A))
      Z13a=Z12a*dexp(-12.*A)/(1.-dexp(-13.*A))
      Z14a=Z13a*dexp(-13.*A)/(1.-dexp(-14.*A))
      Z15a=Z14a*dexp(-14.*A)/(1.-dexp(-15.*A))
      Z16a=Z15a*dexp(-15.*A)/(1.-dexp(-16.*A))
      Z17a=Z16a*dexp(-16.*A)/(1.-dexp(-17.*A))
      Z18a=Z17a*dexp(-17.*A)/(1.-dexp(-18.*A))
      Z19a=Z18a*dexp(-18.*A)/(1.-dexp(-19.*A))
      Z20a=Z19a*dexp(-19.*A)/(1.-dexp(-20.*A))
      Z21a=Z20a*dexp(-20.*A)/(1.-dexp(-21.*A))

      Z0b =1.
      Z1b =2.*Z1a
      Z2b =2.*Z2a +1.*Z1a*Z1a
      Z3b =2.*Z3a +2.*Z1a*Z2a
      Z4b =2.*Z4a +2.*Z1a*Z3a +1.*Z2a*Z2a
      Z5b =2.*Z5a +2.*Z1a*Z4a +2.*Z2a*Z3a
      Z6b =2.*Z6a +2.*Z1a*Z5a +2.*Z2a*Z4a +1.*Z3a*Z3a
      Z7b =2.*Z7a +2.*Z1a*Z6a +2.*Z2a*Z5a +2.*Z3a*Z4a
      Z8b =2.*Z8a +2.*Z1a*Z7a +2.*Z2a*Z6a +2.*Z3a*Z5a +1.*Z4a*Z4a
      Z9b =2.*Z9a +2.*Z1a*Z8a +2.*Z2a*Z7a +2.*Z3a*Z6a +2.*Z4a*Z5a
      Z10b=2.*Z10a+2.*Z1a*Z9a +2.*Z2a*Z8a +2.*Z3a*Z7a +2.*Z4a*Z6a +1.*Z5a*Z5a
      Z11b=2.*Z11a+2.*Z1a*Z10a+2.*Z2a*Z9a +2.*Z3a*Z8a +2.*Z4a*Z7a +2.*Z5a*Z6a
      Z12b=2.*Z12a+2.*Z1a*Z11a+2.*Z2a*Z10a+2.*Z3a*Z9a +2.*Z4a*Z8a +2.*Z5a*Z7a +1.*Z6a*Z6a
      Z13b=2.*Z13a+2.*Z1a*Z12a+2.*Z2a*Z11a+2.*Z3a*Z10a+2.*Z4a*Z9a +2.*Z5a*Z8a +2.*Z6a*Z7a
      Z14b=2.*Z14a+2.*Z1a*Z13a+2.*Z2a*Z12a+2.*Z3a*Z11a+2.*Z4a*Z10a+2.*Z5a*Z9a +2.*Z6a*Z8a +1.*Z7a*Z7a
      Z15b=2.*Z15a+2.*Z1a*Z14a+2.*Z2a*Z13a+2.*Z3a*Z12a+2.*Z4a*Z11a+2.*Z5a*Z10a+2.*Z6a*Z9a +2.*Z7a*Z8a
      Z16b=2.*Z16a+2.*Z1a*Z15a+2.*Z2a*Z14a+2.*Z3a*Z13a+2.*Z4a*Z12a+2.*Z5a*Z11a+2.*Z6a*Z10a+2.*Z7a*Z9a +1.*Z8a*Z8a
      Z17b=2.*Z17a+2.*Z1a*Z16a+2.*Z2a*Z15a+2.*Z3a*Z14a+2.*Z4a*Z13a+2.*Z5a*Z12a+2.*Z6a*Z11a+2.*Z7a*Z10a+2.*Z8a*Z9a
      Z18b=2.*Z18a+2.*Z1a*Z17a+2.*Z2a*Z16a+2.*Z3a*Z15a+2.*Z4a*Z14a+2.*Z5a*Z13a+2.*Z6a*Z12a+2.*Z7a*Z11a+2.*Z8a*Z10a+1.*Z9a* Z9a
      Z19b=2.*Z19a+2.*Z1a*Z18a+2.*Z2a*Z17a+2.*Z3a*Z16a+2.*Z4a*Z15a+2.*Z5a*Z14a+2.*Z6a*Z13a+2.*Z7a*Z12a+2.*Z8a*Z11a+2.*Z9a*Z10a
      Z20b=2.*Z20a+2.*Z1a*Z19a+2.*Z2a*Z18a+2.*Z3a*Z17a+2.*Z4a*Z16a+2.*Z5a*Z15a+2.*Z6a*Z14a+2.*Z7a*Z13a+2.*Z8a*Z12a+2.*Z9a*Z11a
     x   +1.*Z10a*Z10a
      Z21b=2.*Z21a+2.*Z1a*Z20a+2.*Z2a*Z19a+2.*Z3a*Z18a+2.*Z4a*Z17a+2.*Z5a*Z16a+2.*Z6a*Z15a+2.*Z7a*Z14a+2.*Z8a*Z13a+2.*Z9a*Z12a
     x   +2.*Z10a*Z11a

      Z1 =Z1b
      Z3 =Z3b +Z2b*Z1b
      Z5 =Z5b +Z2b*Z3b +Z1b*Z4b
      Z7 =Z7b +Z2b*Z5b +Z3b*Z4b +Z1b*Z6b
      Z9 =Z9b +Z2b*Z7b +Z5b*Z4b +Z3b*Z6b +Z1b*Z8b
      Z11=Z11b+Z2b*Z9b +Z7b*Z4b +Z5b*Z6b +Z3b*Z8b +Z1b*Z10b
      Z13=Z13b+Z2b*Z11b+Z9b*Z4b +Z7b*Z6b +Z5b*Z8b +Z3b*Z10b +Z1b*Z12b
      Z15=Z15b+Z2b*Z13b+Z11b*Z4b+Z9b*Z6b +Z7b*Z8b +Z5b*Z10b +Z3b*Z12b+Z1b*Z14b
      Z17=Z17b+Z2b*Z15b+Z13b*Z4b+Z11b*Z6b+Z9b*Z8b +Z7b*Z10b +Z5b*Z12b+Z3b*Z14b+Z1b*Z16b
      Z19=Z19b+Z2b*Z17b+Z15b*Z4b+Z13b*Z6b+Z11b*Z8b+Z9b*Z10b +Z7b*Z12b+Z5b*Z14b+Z3b*Z16b+Z1b*Z18b
      Z21=Z21b+Z2b*Z19b+Z17b*Z4b+Z15b*Z6b+Z13b*Z8b+Z11b*Z10b+Z9b*Z12b+Z7b*Z14b+Z5b*Z16b+Z3b*Z18b+Z1b*Z20b

      Z2 =2.*Z2b
      Z4 =2.*Z4b +1.*Z2b*Z2b
      Z6 =2.*Z6b +2.*Z2b*Z4b 
      Z8 =2.*Z8b +2.*Z2b*Z6b +1.*Z4b*Z4b
      Z10=2.*Z10b+2.*Z2b*Z8b +2.*Z4b*Z6b 
      Z12=2.*Z12b+2.*Z2b*Z10b+2.*Z4b*Z8b +1.*Z6b*Z6b 
      Z14=2.*Z14b+2.*Z2b*Z12b+2.*Z4b*Z10b+2.*Z6b*Z8b
      Z16=2.*Z16b+2.*Z2b*Z14b+2.*Z4b*Z12b+2.*Z6b*Z10b+1.*Z8b*Z8b
      Z18=2.*Z18b+2.*Z2b*Z16b+2.*Z4b*Z14b+2.*Z6b*Z12b+2.*Z8b*Z10b
      Z20=2.*Z20b+2.*Z2b*Z18b+2.*Z4b*Z16b+2.*Z6b*Z14b+2.*Z8b*Z12b+1.*Z10b*Z10b

      Z2bar =1.*Z1b *Z1b
      Z4bar =2.*Z3b *Z1b
      Z6bar =2.*Z5b *Z1b+1.*Z3b *Z3b
      Z8bar =2.*Z7b *Z1b+2.*Z5b *Z3b  
      Z10bar=2.*Z9b *Z1b+2.*Z7b *Z3b+1.*Z5b *Z5b
      Z12bar=2.*Z11b*Z1b+2.*Z9b *Z3b+2.*Z7b *Z5b
      Z14bar=2.*Z13b*Z1b+2.*Z11b*Z3b+2.*Z9b *Z5b+1.*Z7b *Z7b
      Z16bar=2.*Z15b*Z1b+2.*Z13b*Z3b+2.*Z11b*Z5b+2.*Z9b *Z7b
      Z18bar=2.*Z17b*Z1b+2.*Z15b*Z3b+2.*Z13b*Z5b+2.*Z11b*Z7b+1.*Z9b *Z9b
      Z20bar=2.*Z19b*Z1b+2.*Z17b*Z3b+2.*Z15b*Z5b+2.*Z13b*Z7b+2.*Z11b*Z9b
      Z22bar=2.*Z21b*Z1b+2.*Z19b*Z3b+2.*Z17b*Z5b+2.*Z15b*Z7b+2.*Z13b*Z9b+1.*Z11b*Z11b


C Attenuation (Andreas)

       r2 = 1. + (attn**1.)
       r3 = r2 + (attn**2.)
       r4 = r3 + (attn**3.)
       r5 = r4 + (attn**4.)
       r6 = r5 + (attn**5.)
       r7 = r6 + (attn**6.)
       r8 = r7 + (attn**7.)
       r9 = r8 + (attn**8.)
       r10= r9 + (attn**9.)

      IF(i.EQ.1)THEN
         Z=1.                           +p(1) *(dexp(-2.    *D/T))*Z2 +p(2)*(dexp(-2.*r2*D/T))*Z4  +
     1       p(3)*(dexp(-2.*r3*D/T))*Z6 +p(4) *(dexp(-2. *r4*D/T))*Z8 +p(5)*(dexp(-2.*r5*D/T))*Z10 +
     2       p(6)*(dexp(-2.*r6*D/T))*Z12+p(7) *(dexp(-2.*r7*D/T))*Z14 +p(8)*(dexp(-2.*r8*D/T))*Z16 +
     3       p(9)*(dexp(-2.*r9*D/T))*Z18+p(10)*(dexp(-2.*r10*D/T))*Z20 
         xn= 0                               +2.*p(1) *(dexp(-2.    *D/T))*Z2 + 4.*p(2)*(dexp(-2.*r2*D/T))*Z4  +
     1       6. *p(3)*(dexp(-2.*r3*D/T))*Z6  +8.*p(4) *(dexp(-2. *r4*D/T))*Z8 +10.*p(5)*(dexp(-2.*r5*D/T))*Z10 +
     2       12.*p(6)*(dexp(-2.*r6*D/T))*Z12+14.*p(7) *(dexp(-2.*r7*D/T))*Z14 +16.*p(8)*(dexp(-2.*r8*D/T))*Z16 +
     3       18.*p(9)*(dexp(-2.*r9*D/T))*Z18+20.*p(10)*(dexp(-2.*r10*D/T))*Z20
         xn = xn/Z
      ENDIF

      IF(i.EQ.2)THEN
         Z=Z1                           +p(1) *(dexp(-2.    *D/T))*Z3 +p(2)*(dexp(-2.*r2*D/T))*Z5  +
     1       p(3)*(dexp(-2.*r3*D/T))*Z7 +p(4) *(dexp(-2. *r4*D/T))*Z9 +p(5)*(dexp(-2.*r5*D/T))*Z11 +
     2       p(6)*(dexp(-2.*r6*D/T))*Z13+p(7) *(dexp(-2.*r7*D/T))*Z15 +p(8)*(dexp(-2.*r8*D/T))*Z17 +
     3       p(9)*(dexp(-2.*r9*D/T))*Z19+p(10)*(dexp(-2.*r10*D/T))*Z21
         xn= 1.*Z1                           +3.*p(1) *(dexp(-2.    *D/T))*Z3 + 5.*p(2)*(dexp(-2.*r2*D/T))*Z5  +
     1       7. *p(3)*(dexp(-2.*r3*D/T))*Z7  +9.*p(4) *(dexp(-2. *r4*D/T))*Z9 +11.*p(5)*(dexp(-2.*r5*D/T))*Z11 +
     2       13.*p(6)*(dexp(-2.*r6*D/T))*Z13+15.*p(7) *(dexp(-2.*r7*D/T))*Z15 +17.*p(8)*(dexp(-2.*r8*D/T))*Z17 +
     3       19.*p(9)*(dexp(-2.*r9*D/T))*Z19+21.*p(10)*(dexp(-2.*r10*D/T))*Z21
         xn = xn/Z
      ENDIF

      IF(i.EQ.3)THEN
         Z=Z2bar                           +p(1) *(dexp(-2.   *D/T))*Z4bar +p(2)*(dexp(-2.*r2*D/T))*Z6bar  +
     1       p(3)*(dexp(-2.*r3*D/T))*Z8bar +p(4) *(dexp(-2.*r4*D/T))*Z10bar+p(5)*(dexp(-2.*r5*D/T))*Z12bar +
     2       p(6)*(dexp(-2.*r6*D/T))*Z14bar+p(7) *(dexp(-2.*r7*D/T))*Z16bar+p(8)*(dexp(-2.*r8*D/T))*Z18bar +
     3       p(9)*(dexp(-2.*r9*D/T))*Z20bar+p(10)*(dexp(-2.*r10*D/T))*Z22bar
         xn= 2.*Z2bar                            +4.*p(1)*(dexp(-2.    *D/T))*Z4bar  +6.*p(2)*(dexp(-2.*r2*D/T))*Z6bar  +
     1       8. *p(3)*(dexp(-2.*r3*D/T))*Z8bar +10.*p(4) *(dexp(-2. *r4*D/T))*Z10bar+12.*p(5)*(dexp(-2.*r5*D/T))*Z12bar +
     2       14.*p(6)*(dexp(-2.*r6*D/T))*Z14bar+16.*p(7) *(dexp(-2. *r7*D/T))*Z16bar+18.*p(8)*(dexp(-2.*r8*D/T))*Z18bar +
     3       20.*p(9)*(dexp(-2.*r9*D/T))*Z20bar+22.*p(10)*(dexp(-2.*r10*D/T))*Z22bar
         xn = xn/Z
      ENDIF

      IF(i.EQ.4)THEN
         Z= (1.- (dexp(-12.*D/T))*(Z1)**12) /  (1.- (dexp(-2.*D/T))*(Z1)**2)
      ENDIF
      IF(i.EQ.5)THEN
         Z=((1.- (dexp(-12.*D/T))*(Z1)**12) /  (1.- (dexp(-2.*D/T))*(Z1)**2))*Z1
      ENDIF      
      IF(i.EQ.6)THEN
         Z=((1.- (dexp(-12.*D/T))*(Z1)**12) /  (1.- (dexp(-2.*D/T))*(Z1)**2))*Z1*Z1
      ENDIF

C Includes rotation, only R = 0,2,4,6,8,10,12
      Zrot = 1.+dexp(-Arot*2.*3./T)+dexp(-Arot*4.*5./T)+dexp(-Arot*6.*7./T) 
     1         +dexp(-Arot*8.*9./T)+dexp(-Arot*10.*11./T)+dexp(-Arot*12.*13./T)
c     Zrot = QSQRT(3.14*T/Arot)
      Z    = Z*Zrot

C Includes vibration, only v = 0,1,2
      Zvib = 1.+3.*dexp(-1.*Avib/T)     !+9.*dexp(-2.*Avib/T) +  3=beta,gamma,octupole
c     1         27.*dexp(-3.*Avib/T)+81.*dexp(-4.*Avib/T) 
      Z    = Z*Zvib

      RETURN
      END


      FUNCTION F(A,i)
      COMMON/Zfunc/D,T,epsilon,Arot,Avib,attn,xn,nupar
      DOUBLE PRECISION D,T,epsilon,Arot,Avib,attn,xn
      DOUBLE PRECISION A
      DOUBLE PRECISION F,Z
      F= - T * dlog(Z(A,i))
      RETURN
      END


      FUNCTION S(A,i)
      COMMON/Zfunc/D,T,epsilon,Arot,Avib,attn,xn,nupar
      DOUBLE PRECISION D,T,epsilon,Arot,Avib,attn,xn
      DOUBLE PRECISION A
      DOUBLE PRECISION dT,T1,T2,Tsave,A1,A2,F,F1,F2,S
      Tsave=T 
      dT=1.0    !Derivating F with temperatures 1 keV up and down
      T1=MAX(0.,T - dT)
      T2=T+dT
      A1=epsilon/T1
      A2=epsilon/T2
      T =T1
      F1=F(A1,i)
      T =T2
      F2=F(A2,i)
      S =-(F2-F1)/(T2-T1)
      T =Tsave
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


      SUBROUTINE IterateRho(Starget,Rout, nuiter)
C Transform rho to S by Laplace transformation
      DOUBLE PRECISION Rout(1:1000)
      REAL Starget(1:111), Sout(1:111), Sin(1:111)

      Tstart = 50.
      Tstep  = 25. 

      DO i = 1,111
         Sin(i) = Starget(i)
      ENDDO

      DO i = 1,1000
         Rout(i) = 0.
      ENDDO

      DO iter = 1,nuiter
         CALL S2R(Sin, Rout)
         CALL R2S(Rout,Sout)
         WRITE(6,*)'Iteration #',iter,':'
         WRITE(6,*)'-- T -- S(target) -- S(in) -- S(out)'
         DO i = 1, 111
            IF(i-1.EQ.((i-1)/4)*4.OR.i.EQ.1.OR.i.EQ.111)THEN
               TTT = Tstart+(i-1)*Tstep
               WRITE(6,20)TTT, Starget(i), Sin(i), Sout(i)
 20         FORMAT(F7.1,F8.1,F11.1,F9.1)
            ENDIF
            Sin(i) = Sin(i) + (Starget(i) - Sout(i))
         ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE S2R(Sin,Rout)
C Transform S to rho by saddle point approximation
      DOUBLE PRECISION Rout(1:1000), ss, cc, aa, y(1:111), rho4
      REAL Sin(1:111), F(1:111), x(1:111)
C Stuff for spline part
      INTEGER NP
      REAL yp1,ypn,xa(4096),ya(4096),y2(4096)

      Tstart =  50.
      Tstep  =  25. 
      Estart = 100.
      Estep  = 100.
      ExMin  = 200.

      DO iE = 1,1000
         Rout(iE) = 0.
      ENDDO

      DO i = 1,111
         F(i) = 0.
         x(i) = 0.
         y(i) = 0.
         xa(i)= 0.
         ya(i)= 0.
      ENDDO

C Interpolating x=T and y=S with spline-method
C From the book: Numerical Recepies
      NP=111 
      DO i=1,NP
        xa(i) = Tstart + (i-1)*Tstep
        ya(i) = Sin(i)
        IF(Sin(i).LE.0.AND.i.GT.10)GO TO 555
      ENDDO
 555  NP = i - 1
      yp1=0  !Calculates the derivatives at end-points
      ypn=0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding 2. deriv.

C Finding Cv(T) and Rho(T)
      DO it = 1,NP
         T  = Tstart + (it-1) * Tstep
         Tl = T - 5.
         CALL splint(xa,ya,y2,NP,Tl,Sl)  !Interpolating
         Th = T + 5.
         CALL splint(xa,ya,y2,NP,Th,Sh)  !Interpolating
         cc = MAX(0.01,T * (Sh-Sl)/10.)
         ss = Sin(it)
         TT = (T/1000.)**2.
         aa = 2. * 3.14 * cc * TT
         y(it) = DEXP(ss)/DSQRT(aa)
         IF(y(it).GT.1.E+23) GO TO 888
c         write(6,222)it,T,ss,cc,y(it)
c 222     format(I,3F8.1,E12.2)
      ENDDO
 
  888 NP = it-1

C Finding F(T), integrating in steps of 1 keV
      Tmax  = Tstart + NP * Tstep
      iitmax = Tmax/1.
      dF = 0.
      DO iit = 1,iitmax
         T  = 0. + (iit-1) * 1.
         CALL splint(xa,ya,y2,NP,T,S)  !Interpolating
         dF = dF + ( -S * 1.)
         it = iit / Tstep
         IF(it*Tstep.EQ.iit)F(it) = dF
      ENDDO

C Finding <E(T)>
      ibase = 0
      DO it = 1,NP
         T  = Tstart + (it-1)*Tstep
         x(it) = F(it) + T * Sin(it)
         IF(x(it).LT.ExMin)ibase = ibase + 1            ! Ex > ExMin
c         write(6,*) 'T,S(it),x(it) ', T,Sin(it),x(it)     
      ENDDO
c           write(6,*)'NP,ibase',NP,ibase

      NP = NP - ibase
      DO i=1, NP
        xa(i)=x(i+ibase)
        ya(i)=dlog(y(i+ibase))       !Better to interpolate ln    
c           write(6,*)xa(i),ya(i)  
      ENDDO
      yp1=0               !Calculates the derivatives at end-points
      ypn=0
      dx=xa(2)-xa(1)
      dy=ya(2)-ya(1)
      IF(dx.NE.0)yp1=dy/dx
      dx=xa(NP)-xa(NP-1)
      dy=ya(NP)-ya(NP-1)
      IF(dx.NE.0)ypn=dy/dx
      CALL spline(xa,ya,NP,yp1,ypn,y2) !Finding second derivatives
      DO iE = 1,1000
         Ex = Estart + Estep * (iE-1)
         IF(Ex.GT.x(NP-1))GO TO 999
         CALL splint(xa,ya,y2,NP,Ex,rho)  !Interpolating
         rho4 = rho
         Rout(iE)=dexp(rho4)
         IF(Rout(iE).GT.1.E+23) GO TO 999
c         write(6,*)'Ex Rho ',Ex,rho
      ENDDO
 999  CONTINUE

      RETURN
      END


      SUBROUTINE R2S(Rin,Sout)
C Transform rho to S by Laplace transformation
      DOUBLE PRECISION Rin(1:1000), xx, Z0, Zl, Zh
      REAL Sout(1:111),Sold

      Tstart = 50.
      Tstep  = 25. 
      Estart = 100.
      Estep  = 100.
      Sold   = 0.

      DO it = 1,111
         Sout(it) = 0.
      ENDDO
 
      DO it = 1,111
         T = Tstart + (it-1) * Tstep

         Z0 = 0.
         T0 = T
         DO iE = 1,1000
            E  = Estart + (iE-1) * Estep
            xx = E/T0
            IF(xx. LT. 0.001)xx = 0.001
            IF(xx. GT. 1000.)xx = 1000.
            Z0 = Z0 + Rin(iE) * dexp(-xx) * Estep/1000.
         ENDDO

         Zl = 0.
         Tl = T - 5.
         DO iE = 1,1000
            E  = Estart + (iE-1) * Estep
            xx = E/Tl
            IF(xx. LT. 0.001)xx = 0.001
            IF(xx. GT. 1000.)xx = 1000.
            Zl = Zl + Rin(iE) * dexp(-xx) * Estep/1000.
         ENDDO
         Fl = -Tl * DLOG(Zl)

         Zh = 0.
         Th = T + 5.
         DO iE = 1,1000
            E  = Estart + (iE-1) * Estep
            xx = E/Th
            IF(xx. LT. 0.001)xx = 0.001
            IF(xx. GT. 1000.)xx = 1000.
            Zh = Zh + Rin(iE) * dexp(-xx) * Estep/1000.
         ENDDO
         Fh = -Th * dlog(Zh)

         Sout(it) = MAX(0.01, -(Fh - Fl) / 10.)
         IF(Sout(it). LT. Sold. AND. it. GT. 10)  GO TO 9999
         Sold = Sout(it)
         
c          write(6,*)'T,Fh,Fl,Sout(it)', it*25,Fh,Fl,Sout(it)
      ENDDO

 9999 CONTINUE
      RETURN
      END
