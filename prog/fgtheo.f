      PROGRAM fgtheo 
C Read/write stuff (mama)
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      INTEGER XDIM,YDIM,IDEST
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
C Stuff for the calculation
      REAL Rho(0:511),Sig(0:511),FgSum(0:511)
      REAL Fg(0:511,0:511)
      REAL a0,a1,mass,density,Bn,factor,E,n,Cn,Elow,Ehigh,f,PZ,PN,Delta
      INTEGER iBn,i,i1,i2,iElow,iEhigh,low
      WRITE(6,*)' ______________________________________'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|          F G T H E O    1.1          |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|  Program to calculate a normalized,  |'
      WRITE(6,*)'| theoretical first-generation spectra |'
      WRITE(6,*)'|   using Fg(Ex,Eg)=Rho(Ex-Eg)*F(Eg)   |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|   Oslo Cyclotron Laboratory  (OCL)   |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|        Created:  13/08 - 1999        |'
      WRITE(6,*)'|           Andreas Schiller           |'
      WRITE(6,*)'|______________________________________|'
C Input calibration
      a0=0.
      a1=100.
C Input data of the nucleus
      mass=162.
      density=20.47
      Bn=8196.95
      PZ=0.92
      PN=0.70
      Delta=PZ+PN
      low=INT((1000.*(Delta+9./(4.*density))-a0)/a1+0.5)
      iBn=INT((Bn-a0)/a1+0.5)
      Bn=FLOAT(iBn)*a1+a0
      WRITE(6,'(A51)')'The level density formula is taken from the article'
      WRITE(6,'(A58)')'A. Gilbert, A.G.W. Cameron, Can. Jour. Phys. 43(1965)1446,'
      WRITE(6,'(A54)')'Equation 5, using Eqs 9 and 11 for the spin-dependence'
      WRITE(6,'(A52)')'parameter sigma^2. The parameters used are those for'
      WRITE(6,'(A56)')'162-Dy, including backshift. From the minimum at 1.7 MeV'
      WRITE(6,'(A55)')'and down to 0 MeV, a constant level density is assumed.'
C Level density formula from Gilbert Cameron Eq 5 using Eqs 9 and 11 for sigma
      factor=1./(12.*SQRT(0.1776)*(mass**(1./3.)))
      DO i=0,iBn
         E=a1*REAL(i)+a0
         E=E/1000.
         E=E-Delta
         IF(i.GT.low)THEN
            Rho(i)=factor*EXP(2.*SQRT(density*E))/(E*SQRT(E*density))
         ELSE 
            Rho(i)=factor*EXP(3.)*8.*density/27.
         ENDIF
      ENDDO
      WRITE(6,'(A52)')'The level density can be changed by making two boxes'
      WRITE(6,'(A54)')'Type now a E_low, E_high and f, in order to change the'
      WRITE(6,'(A55)')'level density between E_low and E_high by a factor of f'
      Elow=2.5
      WRITE(6,10)Elow
 10   FORMAT('E_low in MeV (first box) <',F3.1,'>:',$)
      CALL READF(5,Elow)
      Elow=Elow*1000.
      IF(Elow.LE.0.)STOP 'Sorry, E_low<=0'
      Ehigh=3.5
      WRITE(6,11)Ehigh
 11   FORMAT('E_high in MeV (first box) <',F3.1,'>:',$)
      CALL READF(5,Ehigh)
      Ehigh=Ehigh*1000.
      IF(Ehigh.GT.Bn)STOP 'Sorry, E_high>Bn'
      f=2.
      WRITE(6,12)f
 12   FORMAT('f (first box) <',F3.1,'>:',$)
      CALL READF(5,f)
      IF(f.LE.0.)STOP 'Sorry, f<=0'
      iElow=INT((Elow-a0)/a1+0.5)
      Elow=FLOAT(iElow)*a1+a0
      iEhigh=INT((Ehigh-a0)/a1+0.5)
      Ehigh=FLOAT(iEhigh)*a1+a0
      WRITE(6,13)Elow,Ehigh,f
 13   FORMAT('E_low=',F6.1,' keV, E_high=',F6.1,' keV, factor=',E16.6)
      DO i=iElow,iEhigh
         Rho(i)=Rho(i)*f
      ENDDO
      Elow=7.5
      WRITE(6,14)Elow
 14   FORMAT('E_low in MeV (second box) <',F3.1,'>:',$)
      CALL READF(5,Elow)
      Elow=Elow*1000.
      IF(Elow.LE.0.)STOP 'Sorry, E_low<=0'
      Ehigh=8.2
      WRITE(6,15)Ehigh
 15   FORMAT('E_high in MeV (second box) <',F3.1,'>:',$)
      CALL READF(5,Ehigh)
      Ehigh=Ehigh*1000.
      IF(Ehigh.GT.Bn)STOP 'Sorry, E_high>Bn'
      f=2.
      WRITE(6,16)f
 16   FORMAT('f (second box) <',F3.1,'>:',$)
      CALL READF(5,f)
      IF(f.LE.0.)STOP 'Sorry, f<=0'
      iElow=INT((Elow-a0)/a1+0.5)
      Elow=FLOAT(iElow)*a1+a0
      iEhigh=INT((Ehigh-a0)/a1+0.5)
      Ehigh=FLOAT(iEhigh)*a1+a0
      WRITE(6,13)Elow,Ehigh,f
      DO i=iElow,iEhigh
         Rho(i)=Rho(i)*f
      ENDDO
      WRITE(6,'(A56)')'The strength function is just F(Eg)=Cn*(Eg)^n with n=4.2'
      WRITE(6,'(A51)')'and normalized at Bn/2=4.1 MeV i.e. F(Eg=4.1 MeV)=1'
C Now, calculating a strength function 
C Input data of the nucleus
      n=4.2
      Cn=(Bn/2000.)**n
      DO i=0,iBn
         E=a1*REAL(i)+a0
         E=E/1000.
         Sig(i)=(E**n)/Cn
      ENDDO
      WRITE(6,'(A56)')'The strength function can be changed by making two boxes'
      WRITE(6,'(A54)')'Type now a E_low, E_high and f, in order to change the'
      WRITE(6,'(A59)')'strength function between E_low and E_high by a factor of f'
      Elow=2.
      WRITE(6,10)Elow
      CALL READF(5,Elow)
      Elow=Elow*1000.
      IF(Elow.LE.0.)STOP 'Sorry, E_low<=0'
      Ehigh=3.
      WRITE(6,11)Ehigh
      CALL READF(5,Ehigh)
      Ehigh=Ehigh*1000.
      IF(Ehigh.GT.Bn)STOP 'Sorry, E_high>Bn'
      f=2.
      WRITE(6,12)f
      CALL READF(5,f)
      IF(f.LE.0.)STOP 'Sorry, f<=0'
      iElow=INT((Elow-a0)/a1+0.5)
      Elow=FLOAT(iElow)*a1+a0
      iEhigh=INT((Ehigh-a0)/a1+0.5)
      Ehigh=FLOAT(iEhigh)*a1+a0
      WRITE(6,13)Elow,Ehigh,f
      DO i=iElow,iEhigh
         Sig(i)=Sig(i)*f
      ENDDO
      Elow=7.5
      WRITE(6,14)Elow
      CALL READF(5,Elow)
      Elow=Elow*1000.
      IF(Elow.LE.0.)STOP 'Sorry, E_low<=0'
      Ehigh=8.2
      WRITE(6,15)Ehigh
      CALL READF(5,Ehigh)
      Ehigh=Ehigh*1000.
      IF(Ehigh.GT.Bn)STOP 'Sorry, E_high>Bn'
      f=2.
      WRITE(6,16)f
      CALL READF(5,f)
      IF(f.LE.0.)STOP 'Sorry, f<=0'
      iElow=INT((Elow-a0)/a1+0.5)
      Elow=FLOAT(iElow)*a1+a0
      iEhigh=INT((Ehigh-a0)/a1+0.5)
      Ehigh=FLOAT(iEhigh)*a1+a0
      WRITE(6,13)Elow,Ehigh,f
      DO i=iElow,iEhigh
         Sig(i)=Sig(i)*f
      ENDDO
C Now, calculating first generation spectra
      DO i1=0,iBn
         FgSum(i1)=0.
         DO i2=0,i1
            Fg(i2,i1)=Rho(i1-i2)*Sig(i2)
            FgSum(i1)=FgSum(i1)+Fg(i2,i1)
         ENDDO
C Now, normalizing first generation spectra
         DO i2=0,i1
            IF(FgSum(i1).GT.0.)Fg(i2,i1)=Fg(i2,i1)/FgSum(i1)
         ENDDO
      ENDDO
C Writing matrices  out for mama
C Writing matrices to files
      IDEST=1
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=a0
      cal(1,1,2,2)=a1
      cal(1,1,2,3)=0.
      XDIM=iBn+1
      YDIM=iBn+1
      DO i1=0,YDIM-1
         DO i2=0,XDIM-1
               rMAT(IDEST,i2,i1)=Fg(i2,i1)
         ENDDO
      ENDDO
      outfile='fgtheo.dat'
      comment='Normalized first generation matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,17)outfile
 17   FORMAT('Normalized first generation matrix written to file: ',A11)
      IDEST=1
      cal(1,1,1,1)=0.
      cal(1,1,1,2)=1.
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=a0
      cal(1,1,2,2)=a1
      cal(1,1,2,3)=0.
      XDIM=iBn+1
      YDIM=2
      DO i=0,XDIM-1
         rMAT(IDEST,i,0)=Rho(i)
         rMAT(IDEST,i,1)=Sig(i)
      ENDDO
      outfile='rstheo.dat'
      comment='Level density, gamma strength function'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,18)outfile
 18   FORMAT('Level density and gamma strength function written to file: ',A11)
      STOP
 99   WRITE(6,*)'Could not open file for results and spectra'
      STOP
      END
