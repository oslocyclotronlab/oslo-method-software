      PROGRAM Robin
      COMMON/mas03/Z(3200),A(3200),El(3200),Mexp(3200),dMexp(3200),ii(0:511,0:511)
      COMMON/rct2/Sn(3200),dSn(3200),Sp(3200),dSp(3200)
      COMMON/rct7/Pd(3200),dPd(3200),Pn(3200),dPn(3200),Pp(3200),dPp(3200)
      REAL Mexp, S_prime,Pa_prime
      CHARACTER El*3,dum1*1,dum3*3,dum4*4,dum,filnam*100
      INTEGER Z,A,ii,Z0,A0,N0,ieo,idum
      DOUBLE PRECISION S,dS
      WRITE(6,*)'    _____________________________________________'
      WRITE(6,*)'   |                                             |'
      WRITE(6,*)'   |                   ROBIN 1.92                |'
      WRITE(6,*)'   |                                             |'
      WRITE(6,*)'   |    Program to calculate the level density   |'
      WRITE(6,*)'   |           and spin cut-off parameter        |'
      WRITE(6,*)'   |         at Rho(Bn) for a nucleus with       |'
      WRITE(6,*)'   |   given proton number Z and mass number A   |'
      WRITE(6,*)'   |         See T. Egidy and Bucurescu:         |'
      WRITE(6,*)'   |                                             |'
      WRITE(6,*)'   |      (E&B2009): PRC 80, 054310 (2009)       |'
      WRITE(6,*)'   |      (E&B2006): PRC 72, 044311 (2005) and   |'
      WRITE(6,*)'   |                 PRC 73, 049901 (E) (2006)   |'
      WRITE(6,*)'   |                                             |'
      WRITE(6,*)'   |          Oslo Cyclotron Laboratory          |'
      WRITE(6,*)'   |               Magne Guttormsen              |'
      WRITE(6,*)'   | Created  13 Jun 2005                        |'
      WRITE(6,*)'   | Modified 14 Sep 2012                        |'
      WRITE(6,*)'   | Modified 25 Mar 2013                        |'
      WRITE(6,*)'   | Modified 16 Aug 2016:                       |'
      WRITE(6,*)'   | Opt(2) proposes better a and E1 values      |'
      WRITE(6,*)'   | Modified 19 Nov 2016:                       |'
      WRITE(6,*)'   | In Opt(1) you may choose reduct. of RMI     |'
      WRITE(6,*)'   | Temp. given at Bn and Bp for Fermi gas      |'
      WRITE(6,*)'   | Modified 03 Aug 2022:                       |'
      WRITE(6,*)'   | Removed sentences of confusion,lines 220,240|'
      WRITE(6,*)'   |_____________________________________________|'

      Z0     =  66
      A0     =  162
      isig   =  4
      itemp  =  1
      Exx    = -1.
      sig2   = -1.
      red    =  1.
      
      WRITE(6,*)' '
      call makepath("UIO_APPLICATIONS","prog/lib/egidy03/mass.mas03",filnam)
      WRITE(6,*)'Reading file '//filnam
      OPEN(20,ERR=98,FILE=filnam,STATUS='OLD')
      DO i=1,39
         READ(20,7,ERR=99)dum
 7       FORMAT(A1)
      ENDDO
      DO i=1,3200
         READ(20,11,END=12,ERR=99)dum1,idum,idum,Z(i),A(i),El(i),dum4,Mexp(i),dMexp(i)
 11      FORMAT(a1,i3,i5,i5,i5,1x,a3,a4,1x,f13.5,f11.5)
         ii(A(i),Z(i))=i 
      ENDDO
 12   CONTINUE

      WRITE(6,8)i-1,A(i-1),Z(i-1)
  8   FORMAT(' Number of nuclei read is',I5,' with last (A,Z) =(',I4,',',I4')')

      call makepath("UIO_APPLICATIONS","prog/lib/egidy03/rct2.mas03",filnam)
      WRITE(6,*)'Reading file '//filnam
      OPEN(21,ERR=98,FILE=filnam,STATUS='OLD')
      DO i=1,39
         READ(21,7,ERR=99)dum
      ENDDO
      DO i=1,3200
         READ(21,13,END=14,ERR=99)dum1,idum,dum3,idum,Sn(i),dSn(i),Sp(i),dSp(i)
 13      FORMAT(a1,i3,1x,a3,i3,1x,2(f10.2,f8.2))
      ENDDO
 14   CONTINUE
      WRITE(6,8)i-1,A(i-1),Z(i-1)

      call makepath("UIO_APPLICATIONS","prog/lib/egidy03/rct7.mas03",filnam)
      WRITE(6,*)'Reading file '//filnam
      OPEN(22,ERR=98,FILE=filnam,STATUS='OLD')
      DO i=1,39
         READ(22,7,ERR=99)dum
      ENDDO
      DO i=1,3200
         READ(22,15,END=16,ERR=99)dum1,idum,dum3,idum,Pd(i),dPd(i),Pn(i),dPn(i),Pp(i),dPp(i)
 15      FORMAT(a1,i3,1x,a3,i3,1x,3(f10.2,f8.2))
      ENDDO
 16   CONTINUE
      WRITE(6,8)i-1,A(i-1),Z(i-1)
      WRITE(6,*)'Please, check that the same number of nuclei is read.'
      WRITE(6,*)'(If not, mismatch of values will give wrong results.)'
      
      OPEN(23,FILE='input.rbn',STATUS='old',ERR=666)
      READ(23,*,END=666,ERR=666)Z0,A0,isig,itemp,Exx,sig2,red
      GO TO 777
 666  WRITE(6,*)'Could not read input.rbn, using default values'
 777  CLOSE(23)

      WRITE(6,*)' '
      WRITE(6,30)Z0
 30   FORMAT('Give proton number of the nucleus <',I3,'>:',$)
      CALL READI(5,Z0)
      IF(Z0.LT.0)STOP
      WRITE(6,31)A0
 31   FORMAT('Give mass number of the nucleus   <',I3,'>:',$)
      CALL READI(5,A0)
      IF(A0.LT.0)STOP
      IF(A0.LT.Z0)STOP 'Sorry, A < Z'

      WRITE(6,*)' '
      WRITE(6,*)'You should take care to use formulas from the same reference.'
      WRITE(6,*)'Example: If you use rigid moment of inertia from (E&B2006),'
      WRITE(6,*)'you should also use temperature T from (E&B2006), as well.'
      WRITE(6,*)'We recommend to use (E&B2006) (or maybe the old (G&C)) for nuclei heavier than A > 150,'
      WRITE(6,*)'and (E&B2009) for lighter nuclei with A < 150.'
      WRITE(6,*)' '
      WRITE(6,*)'You may choose between 4 spin cut-off formulas:'
      WRITE(6,*)'1 The rigid moment of inertia formula (RMI)(E&B2006)'
      WRITE(6,*)'2 The Gilbert and Cameron formula (G&C) Can. J. Phys 43(1965) 1446'
      WRITE(6,*)'3 The constant temperature (CT) formula (E&B2009) and NPA 481 (1988) 189'
      WRITE(6,*)'4 The Fermi gas formula with appropriate cut-off parameter (E&B2009)'

      WRITE(6,*)'Type 1 for RMI: sig**2=0.0146*(A**(5/3))*T for FG (E&B2006)'
      WRITE(6,*)'Type 2 for G&C: sig**2=0.0888*(A**(2/3))*a*T for FG'
      WRITE(6,*)'Type 3 for E&B: sig**2=(0.98*(A**(0.29)))**2 for CT'
      WRITE(6,*)'Type 4 for E&B: sig**2=0.391*A**0.675*(E-0.5*Pa_prime)**0.312 for FG'
 997  WRITE(6,17)isig
 17   FORMAT('Choose RMI(FG) (1), G&C(FG) (2), E&B(CT) (3) or E&B(FG) (4)  <',I1,'>:',$)
      CALL READI(5,isig)
      IF(isig.LT.1.OR.isig.GT.4)go to 997
      IF(isig.EQ.1.OR.isig.EQ.2)THEN
        itemp=1
        IF(isig.EQ.1)itemp=2
        WRITE(6,*)' '
        WRITE(6,*)'You may choose between 2 temperature formulas:'
        WRITE(6,*)'1 The common fermigas formula (CFG)'
        WRITE(6,*)'2 The somewhat more advanced fermigas formula (AFG) (E&B2006)'
        WRITE(6,*)'Type 1 for CFG: T = SQRT(U/a) (G&C)'
        WRITE(6,*)'Type 2 for AFG: T = (1+SQRT(1+4*a*U))/(2*a)(E&B2006)'
 998    WRITE(6,18)itemp
 18     FORMAT('Choose CFG (1) or AFG (2)           <',I1,'>:',$)
        CALL READI(5,itemp)
        IF(itemp.LT.0)STOP
        IF(itemp.LT.1.OR.itemp.GT.2)go to 998
      ENDIF
      WRITE(6,*)' '

      IF(isig.EQ.1)THEN
        WRITE(6,*)'You may choose a reduction factor of RMI at Sn'   ! new 1. Nov. 2016
        WRITE(6,*)'Typically, 0.8-1.0 is an appropriate reduction factor'
        WRITE(6,32)red
  32    FORMAT('Give reduction factor of RMI <',F4.2,'>:',$)
        CALL READF(5,red)
      ENDIF

      N0=A0-Z0
      IF((N0/2)*2.EQ.N0.AND.(Z0/2)*2.EQ.Z0)ieo=0 ! even-even nucleus
      IF((N0/2)*2.NE.N0.AND.(Z0/2)*2.EQ.Z0)ieo=1 ! even-Z odd-N
      IF((N0/2)*2.EQ.N0.AND.(Z0/2)*2.NE.Z0)ieo=2 ! odd-Z even-N
      IF((N0/2)*2.NE.N0.AND.(Z0/2)*2.NE.Z0)ieo=3 ! odd-odd nucleus
      i=ii(A0,Z0)
      CALL ShellCorr(S,A0,Z0)
      CALL dSdA(dS,A0,Z0)

c     OLD VALUES E&B2005 BEFORE ERRATUM
c     IF(ieo.EQ.0)THEN         
c     E1= -0.468 -0.5*(Pd(i)/1000.) + 0.438*dS
c     Sprime=S-0.5*(Pd(i)/1000.)
c     ENDIF 
c     IF(ieo.EQ.1)THEN
c     E1= -0.565 -0.5*(Pd(i)/1000.) + 0.438*dS
c     Sprime=S
c     ENDIF
c     IF(ieo.EQ.2)THEN
c     E1= -0.565 -0.5*(Pd(i)/1000.) + 0.438*dS
c     Sprime=S
c     ENDIF
c     IF(ieo.EQ.3)THEN
c     E1= -0.231 +0.5*(Pd(i)/1000.) + 0.438*dS
c     Sprime=S+0.5*(Pd(i)/1000.)
c     ENDIF

c     OLD VALUES E&B2006 AFTER ERRATUM
       IF(isig.EQ.1.OR.isig.EQ.2) THEN          ! FG model
        IF(ieo.EQ.0)THEN                       ! NEW VALUES FOR FERMIGAS
           E1= -0.48 -0.5*(Pd(i)/1000.) + 0.29*dS
           Sprime=S-0.5*(Pd(i)/1000.)
        ENDIF
        IF(ieo.EQ.1)THEN
           E1= -0.57 -0.5*(Pd(i)/1000.) + 0.70*dS
           Sprime=S
        ENDIF
        IF(ieo.EQ.2)THEN
           E1= -0.57 +0.5*(Pd(i)/1000.) - 0.70*dS
           Sprime=S
        ENDIF
        IF(ieo.EQ.3)THEN
           E1= -0.24 +0.5*(Pd(i)/1000.) + 0.29*dS
           Sprime=S+0.5*(Pd(i)/1000.)
        ENDIF
        aa=FLOAT(A0)*(0.127 + 4.98E-03*Sprime -8.95E-05*FLOAT(A0))
       ENDIF

C VALUES FROM E&B2009
       Pa_prime = Pd(i)/1000.
       IF(ieo.EQ.0.OR.ieo.EQ.1)Pa_prime = -Pd(i)/1000.       ! See page 3 of E&B2009
       S_prime = REAL(S) + 0.5 * Pa_prime

       IF(isig.EQ.1.OR.isig.EQ.2.OR.isig.EQ.4) THEN          ! FG models
        IF(isig.EQ.4)THEN
          aa = (0.199 + 0.0096*S_prime)*FLOAT(A0)**0.869
          E1 = -0.381 + 0.5   *Pa_prime
        ENDIF

        IF(isig.EQ.2)THEN  ! New 16. aug 2016
                WRITE(6,*)'In the T. von Egidy paper NPA A481, 189 (1988), they used the'
                WRITE(6,*)'Gilbert and Cameron formula (G&C) Can. J. Phys 43(1965) 1446'
                WRITE(6,*)'with U = E-C1-Epair, C1 = -6.6A**(-0.32)MeV, a= 0.21A**(0.87) MeV**(-1),'
                WRITE(6,*)'and Dp and Dn from mass differences. The same procedure was used by us'
                WRITE(6,*)'in Guttormsen et al., PRC 68, 064306 (2003).'
                WRITE(6,*)'Below we calculate these values for you.You may use them'
c                 WRITE(6,*)'or the default a and E1 values from E&B 2009.'

                pair_n = Pn(i)/1000.
                pair_p = Pp(i)/1000.
                a_gc = 0.21*A0**( 0.87)
                C1_gc=-6.6 *A0**(-0.32)
                IF(ieo.EQ.0)pair_gc = pair_n + pair_p
                IF(ieo.EQ.1)pair_gc = 0      + pair_p
                IF(ieo.EQ.2)pair_gc = pair_n + 0
                IF(ieo.EQ.3)pair_gc = 0      + 0
                E1_gc = pair_gc + C1_gc
                WRITE(6,80)pair_n, pair_p
 80             FORMAT('Mass difference pairing, neutron =',F7.3,' ,proton =',F7.3,' MeV')
                WRITE(6,81)pair_gc, C1_gc
 81             FORMAT('Pairing for this nucleus =',F7.3,' and C1 =',f7.3' MeV')
                WRITE(6,82)a_gc, E1_gc
 82             FORMAT('a =',F7.3,' MeV**(-1) and energy shift E1 =',F7.3,' MeV')
                WRITE(6,*)' '
        ENDIF

c        WRITE(6,*)'You may choose another a and/or E1 than proposed by E&B2009:'
        WRITE(6,*)'You may choose another a and/or E1 than calculated:'

        WRITE(6,25)aa
 25     FORMAT('Level density parameter a    <',F7.3,'>:',$)
        CALL READF(5,aa)
        WRITE(6,26)E1
 26     FORMAT('Total backshift parameter E1 <',F7.3,'>:',$)
        CALL READF(5,E1)
        WRITE(6,*)' '                     

        CALL FermiGas(rho_n,A0,Sn(i)/1000.,aa,E1,sig_n,isig,itemp,red)
        CALL FermiGas(rho_p,A0,Sp(i)/1000.,aa,E1,sig_p,isig,itemp,red)

        IF(isig.EQ.1.AND.itemp.EQ.1) WRITE(6,20)A0,El(i)
        IF(isig.EQ.1.AND.itemp.EQ.2) WRITE(6,21)A0,El(i)
        IF(isig.EQ.2.AND.itemp.EQ.1) WRITE(6,22)A0,El(i)
        IF(isig.EQ.2.AND.itemp.EQ.2) WRITE(6,23)A0,El(i)

        WRITE(6,41)S,dS
        WRITE(6,42)Pn(i)/1000.,Pp(i)/1000.,Pd(i)/1000.,Pa_prime
        WRITE(6,43)Sn(i)/1000.,Sp(i)/1000.
        WRITE(6,44)aa,E1
        WRITE(6,45)SQRT(sig_n),SQRT(sig_p)
        WRITE(6,46)rho_n,rho_p

C Calculating the temperature at Bn
        IF(itemp.EQ.1)tempBn = SQRT(((Sn(i)/1000)-E1)/aa)
        IF(itemp.EQ.1)tempBp = SQRT(((Sp(i)/1000)-E1)/aa)
        IF(itemp.EQ.2)tempBn = (1.+SQRT(1.+4.*aa*((Sn(i)/1000)-E1)))/(2.*aa)
        IF(itemp.EQ.2)tempBp = (1.+SQRT(1.+4.*aa*((Sp(i)/1000)-E1)))/(2.*aa)
        WRITE(6,49)tempBn,tempBp


      ELSE

        sig= (0.98*(FLOAT(A0)**(0.29)))    ! CONSTANT TEMPERATURE
        sig2 = sig*sig
        tt = FLOAT(A0)**(-0.66666)/(0.0597 + 0.00198 * S_prime)
        E0 = -1.004 + 0.5 * Pa_prime

        WRITE(6,*)'You may choose another T and/or E0 than proposed by CT(E&B):'
        WRITE(6,27)tt
 27     FORMAT('Constant temperature T(MeV)  <',F7.3,'>:',$)
        CALL READF(5,tt)
        WRITE(6,28)E0
 28     FORMAT('Total backshift parameter E0 <',F7.3,'>:',$)
        CALL READF(5,E0)
        WRITE(6,*)' '

        CALL ConstT(rho_n,Sn(i)/1000.,tt,E0)
        CALL ConstT(rho_p,Sp(i)/1000.,tt,E0)

        WRITE(6,24)A0,El(i)
        WRITE(6,41)S,dS
        WRITE(6,42)Pn(i)/1000.,Pp(i)/1000.,Pd(i)/1000.,Pa_prime
        WRITE(6,43)Sn(i)/1000.,Sp(i)/1000.
        WRITE(6,40)tt,E0
        WRITE(6,45)sig, sig
        WRITE(6,46)rho_n,rho_p

      ENDIF
      
      IF(Exx.EQ.-1.)Exx = Sn(i)/1000.
      WRITE(6,50)Exx
 50   FORMAT('Calculate rho and sigma at Ex (MeV)  <',F7.3,'>:',$)
      CALL READF(5,Exx)
      IF(isig.EQ.1.OR.isig.EQ.2.OR.isig.EQ.4)THEN
         CALL FermiGas(rhox,A0,Exx,aa,E1,sigx,isig,itemp,red)
      ELSE
         CALL ConstT(rhox,Exx,tt,E0)
         sigx = sig2
      ENDIF
      
      WRITE(6,51)Exx,rhox,SQRT(sigx)
      
 20   FORMAT('Results for',I4,A2,' with sig**2=0.0146*(A**(5/3))*SQRT(U/a):')
 21   FORMAT('Results for',I4,A2,' with sig**2=0.0146*(A**(5/3))*(1+SQRT(1+4*a*U))/(2*a):')
 22   FORMAT('Results for',I4,A2,' with sig**2=0.0888*(A**(2/3))*a*SQRT(U/a):')
 23   FORMAT('Results for',I4,A2,' with sig**2=0.0888*(A**(2/3))*a*(1+SQRT(1+4*a*U))/(2*a):')
 24   FORMAT('Results for',I4,A2,' with sig**2=(0.98*(A**(0.29)))**2.:')

 41   FORMAT('Shell values            : S=    ',F7.3,' MeV,   dS/dA=',F7.3,' MeV')
 42   FORMAT('Pairing energies        : P_n=  ',F7.3,' MeV,   P_p=  ',F7.3,' MeV, P_d= ',F7.3,' MeV, Pa_prime= ',F7.3,' MeV')
 43   FORMAT('Binding energies        : B_n=  ',F7.3,' MeV,   B_p=  ',F7.3,' MeV')
 44   FORMAT('Fermi gas parameters    : a=    ',F7.3,' 1/MeV, E1=   ',F7.3,' MeV')
 45   FORMAT('Spin cut-off parameters : sig_n=',F7.3,'        sig_p=',F7.3)
 46   FORMAT('Level densities         : rho_n=',E12.5,' 1/MeV, rho_p=',E12.5,' 1/MeV')
 40   FORMAT('Constant temp. param.   : T=    ',F7.3,' MeV,   E0=   ',F7.3,' MeV')
 49   FORMAT('Temperature (FG)        : T(Bn)=',E12.5,' MeV,   T(Bp)=',E12.5,' MeV')


 51   FORMAT('For Ex =',F7.3,' MeV     : rho  =',E12.5,' 1/MeV,',' sig =',F7.3)
      
      IF(sig2.EQ.-1.) sig2 = sigx
      sig = SQRT(sig2)
      WRITE(6,47)sig
 47   FORMAT('Write file with spin distribution for spin cut-off sig = <',F7.3,'>:',$)
      CALL READF(5,sig)
      sig2 = sig*sig
      OPEN(UNIT=12,FILE='spindis.rbn')
      xI = 0.                   !Assumes even-mass nucleus with integer spins
      IF(ieo.EQ.1. OR. ieo.EQ.2) xI = 0.5
      xI_ave = 0.
      Sum_g  = 0.
      P_max  = 0.
      xI_max = 0.
      DO i = 0,40
         CALL SinDistribution(Pspin,xI,sig2)
         WRITE(12,48, ERR=663) xI, Pspin
 48      FORMAT(F5.1,F12.5)
         xI_ave = xI_ave + xI*Pspin
         Sum_g = Sum_g +    Pspin
         IF(Pspin.GT.P_max)THEN
           P_max = Pspin
           xI_max= xI
         ENDIF
         xI = xI + 1.
      ENDDO
      xI_ave = xI_ave/Sum_g
      WRITE(6,*)' '
      WRITE(6,52)xI_ave,sig-0.5,Sum_g
 52   FORMAT('File spindis.rbn written with <I> =',F5.2,', Pmax @ I = sigma-0.5 =',F4.1,' and sum_P =',F5.3)
      GO TO 773
 663  WRITE(6,*)'Could not write to spindis.rbn'
 773  CLOSE(12)
      
      OPEN(UNIT=13,FILE='input.rbn')
      WRITE(13,*,ERR=668)Z0,A0,isig,itemp,Exx,sig2,red
      GO TO 778
 668  WRITE(6,*)'Could not write to input.rbn'
 778  CLOSE(13)
      STOP
      
 98   WRITE(6,*)'Error during opening file'
      STOP
 99   WRITE(6,101)i
 101  FORMAT('Error during reading line: ',I4)
      STOP
      END
      
      SUBROUTINE Shellcorr(S,A0,Z0)
      COMMON/mas03/Z(3200),A(3200),El(3200),Mexp(3200),dMexp(3200),ii(0:511,0:511)
      REAL Mexp
      DOUBLE PRECISION Mn,Mp,u,eps0,e2,rZ,rA,rN,r0,avol,asf,asym,ass,Eb_on_A,pi,S,Mtheo
      CHARACTER El*3
      INTEGER Z,A,Z0,A0
      i=ii(A0,Z0)
      rZ=FLOAT(Z0)
      rA=FLOAT(A0)
      rN=rA-rZ
      u=  931.494043
      Mn= (8071.323/1000.)+u
      Mp= (7288.969/1000.)+u
      pi=ACOS(-1.)
      avol=-15.65
      asf=17.63
      asym=27.72
      ass=-25.60
      r0=1.233
      e2=(1.60217653E-19)**2    ! e**2 in units of C**2
      eps0=8.854187817E-12      ! epsilon_0 in units of F/m
      e2=e2/(4.*pi*eps0)
      e2=e2*6.24150947E+12
      e2=e2*(1.E+15) 
      Eb_on_A=-(avol+(asf*(rA**(-1./3.)))+((3.*e2)/(5.*r0))*(rZ**2)*(rA**(-4./3.))+(asym+ass*(rA**(-1./3.)))*((rN-rZ)/rA)**2)
      Mtheo = rN*Mn + rZ*Mp - rA*(Eb_on_A +u)
      S = (Mexp(i)/1000.)-Mtheo
      END
      
      SUBROUTINE dSdA(dS,A0,Z0)
      INTEGER A0,Z0
      DOUBLE PRECISION S1,S2,dS
      CALL ShellCorr(S1,A0+2,Z0+1)
      CALL ShellCorr(S2,A0-2,Z0-1)
      dS=(1./4.)*(S1-S2)
      RETURN
      END
      
      SUBROUTINE FermiGas(rho,A0,U,aa,E1,sig2,isig,itemp,red)
      INTEGER A0
      REAL T1, T2, T
      T = 0.
      rho = 0.
      rA=FLOAT(A0)
      uu=U-E1
      T1 = SQRT(uu/aa)          ! FG1
      T2 = (1.+SQRT(1.+4.*aa*uu))/(2.*aa) ! FG2
      IF(uu.LE.0.010)uu=0.0010
      IF(itemp.EQ.1)T = T1
      IF(itemp.EQ.2)T = T2
      IF(isig.EQ.1)sig2=red*0.0146*(rA**(5./3))*T ! Rigid body
      IF(isig.EQ.2)sig2=0.0888*(rA**(2./3.))*aa*T ! G&C
      IF(isig.EQ.4)sig2=0.391 *(rA**(0.675))*(U-(E1+0.381))**0.312  !Last factor is Ex-0.5Pa_prime, with 0.5Pa_prime=E1+0.381
      IF(sig2.LE.0) RETURN
      rho=EXP(2.*SQRT(aa*uu))/(12.*SQRT(2.*sig2)*(aa**(1./4.))*(uu**(5./4.)))
      RETURN
      END

      SUBROUTINE ConstT(rho,U,tt,E0)
      rho = 0.
      uu = U-E0
      IF(tt.GT.0.01.AND.uu.GT.0.)rho = (1./tt)*EXP(uu/tt)
      RETURN
      END

      SUBROUTINE SinDistribution(Pspin,xI,sig2)
      Pspin = ((2.*xI+1)/(2.*sig2))*EXP((-(xI+0.5)**2.)/(2.*sig2)) 
      RETURN
      END
      
