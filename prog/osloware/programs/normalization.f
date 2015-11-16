      PROGRAM Normalization 
      INTEGER IT,N,i
      CHARACTER *20 rhofile,Ffile,sigfile
      CHARACTER *1 Q1
      REAL Bn,a0,a1,rho(100),F(100),sig(100),h(100),I1,I2,I3,I4,IS,s,D,G,k
      WRITE(6,*)' ____________________________'
      WRITE(6,*)'|                            |'
      WRITE(6,*)'|     Normalization  1.0     |'
      WRITE(6,*)'|                            |'
      WRITE(6,*)'|  Program to normalize the  |'
      WRITE(6,*)'| gamma-ray energy-dependent |'
      WRITE(6,*)'|  factor F to experimental  |'
      WRITE(6,*)'|  total average  radiation  |'
      WRITE(6,*)'|         width data         |'
      WRITE(6,*)'|                            |'
      WRITE(6,*)'| Oslo Cyclotron Laboratory  |'
      WRITE(6,*)'|                            |'
      WRITE(6,*)'|   Created: 29/08 - 2000    |'
      WRITE(6,*)'|      Andreas Schiller      |'
      WRITE(6,*)'|____________________________|'
      WRITE(6,*)''
      Q1='i'
      WRITE(6,10)Q1
 10   FORMAT('Integer (i) or half-integer (h) target spin <',A1,'>:',$)
      CALL READA(5,Q1)
      IF(Q1.EQ.'i')THEN
         IT=0
         WRITE(6,11)IT
 11      FORMAT('Target spin in (n,gamma) reaction <',I1,'>:',$)
         CALL READI(5,IT)
         IF(IT.LT.0)STOP 'Sorry, target spin<0'
      ELSE IF(Q1.EQ.'h')THEN
         IT=1
         WRITE(6,12)IT
 12      FORMAT('2*(Target spin in (n,gamma) reaction <',I1,'>:',$)
         CALL READI(5,IT)
         IF(IT.LT.0)STOP 'Sorry, target spin<0'
         IF(MOD(IT,2).NE.1)STOP 'Sorry, 2*(target spin) is not odd'
      ELSE 
         STOP 'Sorry, wrong input'
      ENDIF
      Bn=8.
      WRITE(6,13)Bn
 13   FORMAT('Neutron binding energy of the nucleus in MeV <',F10.7,'>:',$)
      CALL READF(5,Bn)
      IF(Bn.LE.0.)STOP 'Sorry, Bn<=0' 
      N=60
      WRITE(6,14)N
 14   FORMAT('Number of channels in spectra <',I2,'>:',$)
      CALL READI(5,N)
      IF(N.LE.0)STOP 'Sorry, N<=0'
      a0=0.
      WRITE(6,15)a0
 15   Format('Spectrum calibration offset a0 in keV <',F4.1,'>:',$)
      CALL READF(5,a0)
      a1=120.0
      WRITE(6,16)a1
 16   FORMAT('Spectrum calibration dispersion a1 in keV/channel <',F6.1,'>:',$)
      CALL READF(5,a1)
      IF(a1.LE.0.)STOP 'Sorry, a1<=0'
      IF(ABS((a0+a1*REAL(N-1))-Bn*1.E3).GT.(0.5*a1))
     +STOP 'Sorry, a0+a1*(N-1) is not close enough to Bn'
      rhofile='rhotmopaw.cnt'
      WRITE(6,17)rhofile
 17   FORMAT('Name of the extrapolated level density file <',A13,'>:',$)
      CALL READA(5,rhofile)
      OPEN(11,ERR=98,FILE=rhofile,STATUS='OLD')
      Ffile='sigextpaw.cnt'
      WRITE(6,18)Ffile
 18   FORMAT('Name of the extrapolated F-function file <',A13,'>:',$)
      CALL READA(5,Ffile)
      OPEN(12,ERR=98,FILE=Ffile,STATUS='OLD')
      sigfile='spincut.paw'
      WRITE(6,19)sigfile
 19   FORMAT('Name of the spin cut-off parameter file <',A11,'>:',$)
      CALL READA(5,sigfile)
      OPEN(13,ERR=98,FILE=sigfile,STATUS='OLD')
      DO 21,i=1,N
 20      FORMAT(X,E14.0)
         READ(11,20,ERR=96)rho(N+1-i)
         READ(12,20,ERR=96)F(i)
         READ(13,20,ERR=96)sig(i)
         sig(i)=2.*sig(i)*sig(i)
 21   CONTINUE
      DO 22,i=1,N
         h(i)=a1*F(i)*rho(i)/sig(i)         
 22   CONTINUE
      I1=0.
      I2=0.
      I3=0.
      I4=0.
      IF(Q1.EQ.'i')THEN
         IF(IT.EQ.0)THEN
            DO 23,i=1,N
               I1=I1+h(i)*EXP(-1./sig(i))
               I2=I2+h(i)*2.*EXP(-4./sig(i))
 23         CONTINUE
            IS=I1+I2
         ELSE IF(IT.EQ.1) THEN
            DO 24,i=1,N
               I1=I1+h(i)*EXP(-1./sig(i))
               I2=I2+h(i)*2.*EXP(-4./sig(i))
               I3=I3+h(i)*3.*EXP(-9./sig(i))
 24         CONTINUE
            IS=2.*I1+2.*I2+I3
         ELSE
            s=REAL(IT)
            DO 25,i=1,N
               I1=I1+h(i)*(s-1.)*EXP(-(s-1.)*(s-1.)/sig(i))
               I2=I2+h(i)*s*EXP(-s*s/sig(i))
               I3=I3+h(i)*(s+1.)*EXP(-(s+1.)*(s+1.)/sig(i))
               I4=I4+h(i)*(s+2.)*EXP(-(s+2.)*(s+2.)/sig(i))
 25         CONTINUE
            IS=I1+2.*I2+2.*I3+I4
         ENDIF
      ELSE
         IF(IT.EQ.1)THEN
            DO 26,i=1,N
               I1=I1+h(i)*.5*EXP(-.5*.5/sig(i))
               I2=I2+h(i)*1.5*EXP(-1.5*1.5/sig(i))
               I3=I3+h(i)*2.5*EXP(-2.5*2.5/sig(i))
 26         CONTINUE
            IS=I1+2.*I2+I3
         ELSE
            s=.5*REAL(IT)
            DO 27,i=1,N
               I1=I1+h(i)*(s-1.)*EXP(-(s-1.)*(s-1.)/sig(i))
               I2=I2+h(i)*s*EXP(-s*s/sig(i))
               I3=I3+h(i)*(s+1.)*EXP(-(s+1.)*(s+1.)/sig(i))
               I4=I4+h(i)*(s+2.)*EXP(-(s+2.)*(s+2.)/sig(i))               
 27         CONTINUE
            IS=I1+2.*I2+2.*I3+I4
         ENDIF
      ENDIF
      WRITE(6,28)IS
 28   FORMAT('The normalization integrals yield ',E14.7)
      D=2.
      WRITE(6,29)D
 29   FORMAT('Neutron resonance spacing parameter D in eV <',F3.1,'>:',$)
      CALL READF(5,D)
      IF(D.LE.0.)STOP 'Sorry, D<=0'
      G=2.
      WRITE(6,30)G
 30   FORMAT('Average total radiative width of neutron resonances G in meV <',F3.1,'>:',$)
      CALL READF(5,G)
      IF(G.LE.0.)STOP 'Sorry, D<=0'
      k=IS*D/G
      WRITE(6,31)k
 31   FORMAT('The normalization factor yields ',E14.7)
      STOP
 96   WRITE(6,97)i
 97   FORMAT('Error during reading line: ',I3)
      STOP
 98   WRITE(6,99)
 99   FORMAT('Error during opening file')
      STOP
      END



