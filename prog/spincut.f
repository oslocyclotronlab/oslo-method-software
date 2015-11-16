      PROGRAM spincut
      WRITE(6,*)'     ________________________________________________'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |               S P I N C U T  1.0               |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |        Program to calculate spin cut-off       |'
      WRITE(6,*)'    |       parameter sigma(E) based on Gilbert      |'
      WRITE(6,*)'    |       and Cameron with Egidys parameters       |'
      WRITE(6,*)'    |           Written to file spincut.paw          |'     
      WRITE(6,*)'    |     NB Spincut is now included in counting.c   |'
      WRITE(6,*)'    |            Oslo Cyclotron Laboratory           |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    | Created:  28/08-2002                           |'
      WRITE(6,*)'    | Modified:                                      |'
      WRITE(6,*)'    | Magne Guttormsen                               |'
      WRITE(6,*)'    |________________________________________________|'
      WRITE(6,*)' '
      
      ieo  = 1
      Mass = 161
      a0   = 0.
      a1   = 120.
      Emax = 11800.0

      WRITE(6,1)
 1    FORMAT(/'Nuclear system may be of type: ',/,
     1     '         even Z even N (ee) <0>',/,
     2     '         odd  Z even N (oe) <1>',/,
     3     '         even Z odd  N (eo) <2>',/,
     4     '         odd  Z odd  N (oo) <3>')
      WRITE(6,2)ieo
 2    FORMAT(/'Define your system and type 0, 1, 2 or 3: <',I1,'>:',$)
      CALL READI(5,ieo)

      IF(ieo.EQ.0.OR.ieo.EQ.3)Mass=160

      WRITE(6,3)Mass
 3    FORMAT('Give mass number A of your system:      <',I3,'>:',$)
      CALL READI(5,Mass)
      Amass=Mass

      Delta  = 12.*Amass**(-0.5)
      IF(ieo.EQ.0)delta = 2.*Delta
      IF(ieo.EQ.1)delta = 1.*Delta
      IF(ieo.EQ.2)delta = 1.*Delta
      IF(ieo.EQ.3)delta = 0.*Delta
      WRITE(6,31)delta
 31   FORMAT('Give total pairing gap Epair (MeV) of your system: <',F7.3,'>:',$)
      CALL READF(5,delta)
      
      WRITE(6,4)a0
 4    FORMAT('Spectrum calibration offset a0 (keV)        <',F5.1,'>:',$)
      CALL READF(5,a0)
      WRITE(6,5)a1
 5    FORMAT('Spectrum calibration dispersion a1 (keV/ch) <',F5.1,'>:',$)
      CALL READF(5,a1)

      WRITE(6,6)Emax
 6    FORMAT('Give upper excitation energy Emax (keV)   <',F7.1,'>:',$)
      CALL READF(5,Emax)

      a0   = a0/1000.           ! in MeV
      a1   = a1/1000.
      Emax = Emax/1000.
      N = INT(((Emax-a0)/a1) + 0.5) + 1
      Emin = a0
      Emax = a0 + N*a1
      OPEN(13,FILE='spincut.paw') 
      DO iE = 0,N
         energy = a0 + a1*iE
         aLev   = 0.21*Amass**(0.87)
         C1     = -6.6*Amass**(-0.32)
         E1     = Delta + C1

c     U      = energy - E1
c     test   = MAX(0.001,(33+16.*aLeV*U)) !Andreas har 33 i stedet for 9
c     t      = (3.+sqrt(test))/(4.*aLev) !Andreas, canonical
c     sigma  = SQRT(0.0888*(Amass**(2./3.))*aLev*t)
c     IF(test.EQ.0.001)sigma=1.0

c     U      = energy - E1
c     test   = MAX(0.001,(9+16.*aLeV*U)) !Andreas har 33 i stedet for 9
c     t      = (3.+sqrt(test))/(4.*aLev) !Andreas, canonical
c     sigma  = SQRT(0.0888*(Amass**(2./3.))*aLev*t)
c     IF(test.EQ.0.001)sigma=1.0

         U      = energy - E1
         test   = MAX(0.01,U)
         t      = SQRT(test/aLev)
         sigma  = SQRT(0.0888*(Amass**(2./3.))*aLev*t)
         IF(test.EQ.0.01)sigma=1.0

c     U      = energy - E1
c     test   = MAX(0.001,U)
c     t      = 1/(SQRT(aLev/test)-(3./(2.*test)))
c     sigma  = SQRT(0.0888*(Amass**(2./3.))*aLev*t)
c     IF(test.EQ.0.001)sigma=1.0


c     U      = MAX(0.01,energy - E1)     ! U never less than 100 keV
c     sigma  = SQRT(0.0888*(Amass**(2./3.))*SQRT(aLev*U)) !G&C


         WRITE(13,*)sigma
      ENDDO
      CLOSE(13)
      WRITE(6,*)' '
      WRITE(6,*)'File spincut.paw written to file'
      WRITE(6,*)'Number of channels are:',N+1
      WRITE(6,*)'First energy is (MeV): ',Emin
      WRITE(6,*)'Last energy is (MeV):',Emax
      WRITE(6,*)'Egidy parameters:  a(MeV**-1)   D(MeV)    C1(MeV)    E1(MeV)'
      WRITE(6,*)'                ',aLev,Delta,C1,E1
      END
