      PROGRAM Neutron 
      INTEGER Z,A,I
      CHARACTER *2 Nuc
      CHARACTER *1 Q
      REAL D,DD,sigma,Dsigma,alpha,Dalpha,s
      REAL x1,x2,y1a,y1b,y2a,y2b,z1,z2,u1,u2,u3,rho,Drho
      WRITE(6,*)' _____________________________'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|    N E U T R O N E B 1.0    |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'| Program to  calculate level |'
      WRITE(6,*)'| density Rho, at the neutron |'
      WRITE(6,*)'| binding energy from neutron |'
      WRITE(6,*)'|   resonance  spacing data   |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|  Oslo Cyclotron Laboratory  |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|    Created: 04/08 - 1999    |'
      WRITE(6,*)'|      Andreas  Schiller      |'
      WRITE(6,*)'|Modified for NLD E&B 16/08/05|'
      WRITE(6,*)'|       Magne Guttormsen      |'
      WRITE(6,*)'|_____________________________|'

      WRITE(6,*)''
      WRITE(6,*)'Run first robin to get spin cut-off'
      WRITE(6,*)'parameter sigma with option (2) E&B'
      WRITE(6,*)'(you need this values as input in neutroneb).'
      WRITE(6,*)''

      Z=66
      WRITE(6,10)Z
 10   FORMAT('Proton number of the nucleus <',I3,'>:',$)
      CALL READI(5,Z)
      IF(Z.LE.1)STOP 'Sorry, Z<=1'
      IF(Z.EQ.60)THEN
         Nuc='Nd'
      ELSE IF(Z.EQ.62)THEN
         Nuc='Sm'
      ELSE IF(Z.EQ.64)THEN
         Nuc='Gd'
      ELSE IF(Z.EQ.66)THEN
         Nuc='Dy'
      ELSE IF(Z.EQ.68)THEN
         Nuc='Er'
      ELSE IF(Z.EQ.70)THEN
         Nuc='Yb'
      ELSE 
         Nuc='??'
         WRITE(6,11)Nuc
 11      FORMAT('Chemical symbol of the nucleus <',A2,'>:',$)
         CALL READA(5,Nuc)
      ENDIF
      A=162
      WRITE(6,12)A
 12   FORMAT('Mass number of the final nucleus <',I3,'>:',$)
      CALL READI(5,A)
      IF(A.LE.Z)STOP 'Sorry, A<=Z'
      eo=MOD(A,2)
      WRITE(6,13)(A-1),Nuc,A,Nuc
 13   FORMAT('Now, give details of the reaction ',I3,'-',A2,'+n->',I3,'-',A2)
      WRITE(6,'(A55)')'Take Tables I or II (pages 1448-1450) from the article:'
      WRITE(6,'(A57)')'A. Gilbert, A.G.W. Cameron, Can. Jour. Phys. 43(1965)1446'
      WRITE(6,14)(A-1),Nuc
 14   FORMAT('Note: Take the row with ',I3,'-',A2,' on the left')
      WRITE(6,15)(A-1),Nuc
 15   FORMAT('If the nucleus ',I3,'-',A2,' is not listed there, find another reference') 
      D=2.67
      WRITE(6,16)D
 16   FORMAT('Neutron resonance spacing parameter D in eV <',F8.2,'>:',$)
      CALL READF(5,D)
      IF(D.LE.0.)STOP 'Sorry, D<=0'
      DD=0.13
      WRITE(6,17)DD
 17   FORMAT('Error of this value in eV <',F8.2,'>:',$)
      CALL READF(5,DD)
      IF(DD.LE.0.)STOP 'Sorry, error<=0'
      Q='y'
c      WRITE(6,18)Q
c 18   FORMAT('You want to give the spin-dependence parameter sigma ? <',A1,'>:',$)
c      CALL READA(5,Q)
      IF((Q.EQ.'y').OR.(Q.EQ.'Y'))THEN
         sigma=5.55
         WRITE(6,19)sigma
 19      FORMAT('Give spin cut-off parameter sigma <',F4.2,'>:',$)
         CALL READF(5,sigma)
         IF(sigma.LE.0.)STOP 'Sorry, sigma<=0'
         alpha=2.*sigma*sigma
         Q='n'
         WRITE(6,20)Q
 20      FORMAT('You want to give the error of this value ? <',A1,'>:',$)
         CALL READA(5,Q)
         IF((Q.EQ.'y').OR.(Q.EQ.'Y'))THEN
            Dsigma=0.05*sigma
            WRITE(6,21)Dsigma
 21         FORMAT('Error of this value <',F4.2,'>:',$)
            CALL READF(5,Dsigma)
            IF(Dsigma.LE.0.)STOP 'Sorry, error<=0'
            Dalpha=4.*sigma*Dsigma
         ELSE
            WRITE(6,'(A52)')'An error of 10% is assumed for the value of sigma^2,'
            WRITE(6,'(A47)')'according to Equation 10 of Gilbert and Cameron'
            Dalpha=0.1*alpha
         ENDIF
      ENDIF

      WRITE(6,'(A45)')'For the last question check any isotope table'
      IF(eo.EQ.0)THEN
         I=5
         WRITE(6,28)(A-1),Nuc,I
 28      FORMAT('Ground state spin of ',I3,'-',A2,' (*2 since odd nucleus) <',I2,'>:',$)
         CALL READI(5,I)
         IF(I.LT.0.)STOP 'Sorry, spin<0'
         IF(MOD(I,2).NE.1)STOP 'Sorry, 2*spin not odd'
         s=real(I)/2.
      ELSE
         I=0 
         WRITE(6,29)(A-1),Nuc,I
 29      FORMAT('Ground state spin of ',I3,'-',A2,' <',I2,'>:',$)
         CALL READI(5,I)
         IF(I.LT.0.)STOP 'Sorry, spin<0'
         s=real(I)
      ENDIF
      WRITE(6,30)(A-1),Nuc,a,Nuc,s,D,DD,(alpha/2.),(Dalpha/2.)
 30   FORMAT('Details of the reaction ',I3,'-',A2,'+n->',I3,'-',A2,
     + /,'Target spin=',F3.1,' with spacing D=(',F8.2,'+-',F8.2,') eV',
     + /,'and spin dependence parameter sigma^2=(',F5.2,'+-',F5.2,')')
C Here starts the calculation
      x1=EXP(-(s+1.)*(s+1.)/alpha)
      x2=EXP(-s*s/alpha)
      y1a=(s+1.)*x1
      y1b=(s+1.)*(s+1.)*y1a
      y2a=s*x2
      y2b=s*s*y2a
      z1=y1b+y2b
      z2=y1a+y2a
      u1=DD/D
      u2=Dalpha/alpha
      u3=1-z1/(alpha*z2)
      rho=1.E6*alpha/(D*z2)
      Drho=rho*SQRT(u1*u1+u2*u2*u3*u3)
C This was the calculation
      WRITE(6,31)rho,Drho
 31   FORMAT('Level density at the neutron binding energy is:',
     + /,'rho=(',E9.3,'+-',E9.3,') 1/MeV')
      STOP
      END
