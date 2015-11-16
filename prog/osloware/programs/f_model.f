      PROGRAM MODEL
      DIMENSION YFIT(100),X(100),Y(100),SIGMAY(100),T(100)
      DIMENSION A(5),DELTAA(5),SIGMAA(5),P(12),AA(5),SIGMAAA(5)
      DIMENSION CH(1001)
      CHARACTER *20 FILENAME
      CHARACTER *1 Q,STEPS,WP
      INTEGER NPTS,I,CHLO,CHHI,MASS,Z,BUMP,BA,NA,NFREE,MBEST,ITER,MODE,M,NBAD
      REAL ELO,EHI,STEP,DEF,VAL,FE1A,FE1B,E17M,M17M,XX,TT,CHISQR,BEST,PROG,CHA
      WRITE(6,*)' _____________________________'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|        M O D E L 1.1        |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'| Program to fit experimental |'
      WRITE(6,*)'| strength functions to a sum |'
      WRITE(6,*)'|        of models of         |'
      WRITE(6,*)'| the GDR (Kadmenskii et al.) |'
      WRITE(6,*)'|  the M1 spin-flip resonance |'
      WRITE(6,*)'|   and the pygmy resonance   |'
      WRITE(6,*)'|      (both Lorentzian)      |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|    Created: 06/09 - 2000    |'
      WRITE(6,*)'|  Oslo Cyclotron Laboratory  |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|   Improved:  27/05 - 2003   |'
      WRITE(6,*)'|     Lawrence  Livermore     |'
      WRITE(6,*)'|     National Laboratory     |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|      Andreas  Schiller      |'
      WRITE(6,*)'|_____________________________|'
      NPTS=60
      WRITE(6,10)NPTS
 10   FORMAT('Number of data points in the spectrum <',I2,'>:',$)
      CALL READI(5,NPTS)
      IF(NPTS.LE.6)STOP 'Too few data points'
      FILENAME='strength.cnt'
      WRITE(6,11)FILENAME
 11   FORMAT('Name of the strength function file <',A12,'>:',$)
      CALL READA(5,FILENAME)
      OPEN(11,ERR=98,FILE=FILENAME,STATUS='OLD')
      DO 13,I=1,2*NPTS
 12      FORMAT(X,E14.7E2)
         IF(I.LE.NPTS)THEN
            READ(11,12,ERR=96)Y(I)
         ELSE
            READ(11,12,ERR=96)SIGMAY(I-NPTS)
         ENDIF
 13   CONTINUE
      CLOSE(11)
      ELO=1.
      WRITE(6,14)ELO
 14   FORMAT('Energy of the lowest data point <',F3.1,'>:',$)
      CALL READF(5,ELO)
      IF(ELO.LT.0.)STOP 'No negative energy allowed'
      EHI=7.
      WRITE(6,15)EHI
 15   FORMAT('Energy of the highest data point <',F3.1,'>:',$)
      CALL READF(5,EHI)
      IF(EHI.LE.ELO)STOP 'Energy must be increasing'
      STEP=1.E3*(EHI-ELO)/REAL(NPTS-1)
      WRITE(6,16)STEP
 16   FORMAT('Dispersion is ',F5.1,' keV per channel')
      DO 17,I=1,NPTS
         X(I)=ELO+1.E-3*STEP*REAL(I-1)
 17   CONTINUE
      CHLO=1
      WRITE(6,18)CHLO
 18   FORMAT('Fit from channel <',I1,'>:',$)
      CALL READI(5,CHLO)
      IF((CHLO.LT.1).OR.(CHLO.GT.NPTS))STOP 'Channel outside range'
      CHHI=NPTS
      WRITE(6,19)CHHI
 19   FORMAT('Fit to channel <',I2,'>:',$)
      CALL READI(5,CHHI)
      IF(CHHI.LE.(CHLO+6).OR.(CHHI.GT.NPTS))STOP 'Channel outside range'
      MASS=170
      WRITE(6,20)MASS
 20   FORMAT('Mass number of nucleus <',I3,'>:',$)
      CALL READI(5,MASS)
      IF((MASS.LT.1).OR.(MASS.GT.300))STOP 'Mass outside range'
      Z=70
      WRITE(6,21)Z
 21   FORMAT('Proton number of nucleus <',I2,'>:',$)
      CALL READI(5,Z)
      IF((Z.LT.1).OR.(Z.GT.MASS))STOP 'Proton number outside range'
      BUMP=2
      WRITE(6,22)BUMP
 22   FORMAT('One or two bumps in GDR cross-section <',I1,'>:',$)
      CALL READI(5,BUMP)
      IF(.NOT.((BUMP.EQ.1).OR.(BUMP.EQ.2)))STOP 'Wrong number of bumps'
      DEF=.3
      WRITE(6,23)DEF
 23   FORMAT('Quadrupole deformation parameter beta <',F5.3,'>:',$)
      CALL READF(5,DEF)
      IF(BUMP.EQ.1)THEN
C Values taken from RIPL page 102-103 (and erratum page 1)
         P(2)=(49.336+7.34*DEF)*(REAL(MASS)**(-.2409))
         P(3)=.3*P(2)
         P(1)=10.6*REAL(MASS)/P(3)
         VAL=P(2)
         WRITE(6,24)VAL
 24      FORMAT('Centroid of the bump in the GDR in MeV <',F5.2,'>:',$)
         CALL READF(5,VAL)
         P(2)=VAL
         VAL=P(3)
         WRITE(6,25)VAL
 25      FORMAT('Width of the bump in the GDR in MeV <',F5.2,'>:',$)
         CALL READF(5,VAL)
         P(3)=VAL
         VAL=P(1)
         WRITE(6,26)VAL
 26      FORMAT('Cross-section of the bump in the GDR in mbarn <',F6.1,'>:',$)
         CALL READF(5,VAL)
         P(1)=VAL
      ELSE
C Values taken from RIPL page 102-103 (and erratum page 1)
         P(5)=50.*(REAL(MASS)**(-.232))
         P(2)=P(5)*EXP(-.946*DEF)
         P(3)=(.283-.263*DEF)*P(2)
         P(6)=(.35-.14*DEF)*P(5)
         P(1)=3.48*REAL(MASS)/P(3)
         P(4)=1.464*(REAL(MASS)**(4./3.))/P(6)
         VAL=P(2)
         WRITE(6,27)VAL
 27      FORMAT('Centroid of the first bump in the GDR in MeV <',F5.2,'>:',$)
         CALL READF(5,VAL)
         P(2)=VAL
         VAL=P(3)
         WRITE(6,28)VAL
 28      FORMAT('Width of the first bump in the GDR in MeV <',F5.2,'>:',$)
         CALL READF(5,VAL)
         P(3)=VAL
         VAL=P(1)
         WRITE(6,29)VAL
 29      FORMAT('Cross-section of the first bump in the GDR in mbarn <',F6.1,'>:',$)
         CALL READF(5,VAL)
         P(1)=VAL
         VAL=P(5)
         WRITE(6,30)VAL
 30      FORMAT('Centroid of the second bump in the GDR in MeV <',F5.2,'>:',$)
         CALL READF(5,VAL)
         P(5)=VAL
         VAL=P(6)
         WRITE(6,31)VAL
 31      FORMAT('Width of the second bump in the GDR in MeV <',F5.2,'>:',$)
         CALL READF(5,VAL)
         P(6)=VAL
         VAL=P(4)
         WRITE(6,32)VAL
 32      FORMAT('Cross-section of the second bump in the GDR in mbarn <',F6.1,'>:',$)
         CALL READF(5,VAL)
         P(4)=VAL
      ENDIF
C Now, calculate f_E1 at 7 MeV and zero temprature, for systematics, using the
C Kadmenskii, Markushev, Furman model
      IF(BUMP.EQ.1)THEN
         E17M=(49.-P(2)*P(2))**2.*P(2)
         E17M=0.7*P(1)*P(3)*P(3)*49./E17M
      ELSE
         FE1A=(49.-P(2)*P(2))**2.*P(2)
         FE1A=0.7*P(1)*P(3)*P(3)*49./FE1A
         FE1B=(49.-P(5)*P(5))**2.*P(5)
         FE1B=0.7*P(4)*P(6)*P(6)*49./FE1B
         E17M=FE1A+FE1B
      ENDIF
C Values taken from RIPL page 102-103 (and erratum page 1)
      P(8)=41.*(REAL(MASS)**(-1./3.))
      P(9)=4.
      M17M=1./(((49.-P(8)**2.)/(7.*P(9)))**2.+1.)
      M17M=M17M/7.
C Two estimates for sigma(M1), the first one is absolute, the second relative
C 0.018216=(3*(pi*hbar*c)**2) * (1.58E-9 MeV**-3)
      P(7)=.018216*(REAL(MASS)**.47)/M17M
      P(7)=E17M/(M17M*.0588*REAL(MASS)**.878)
      VAL=P(8)
      WRITE(6,33)VAL
 33   FORMAT('Centroid of the spin-flip resonance in MeV <',F5.2,'>:',$)
      CALL READF(5,VAL)
      P(8)=VAL
      VAL=P(9)
      WRITE(6,34)VAL
 34   FORMAT('Width of the spin-flip resonance in MeV <',F5.2,'>:',$)
      CALL READF(5,VAL)
      P(9)=VAL
      VAL=P(7)
      WRITE(6,35)VAL
 35   FORMAT('Cross-section of the spin-flip resonance in mbarn <',F6.1,'>:',$)
      CALL READF(5,VAL)
      P(7)=VAL
C Now, isoscalar E2 model. Values taken from RIPL page 103 with corresctions:
C 0.00014->0.00015=2*pi*alpha*r**2/(5*mc**2), alpha=fine structure constant,
C r=1.233 fm, m=nucleon mass (=1u). Value given in MeV/mb. Further, it should 
C be Eo**2 in the formula for sigma.
      P(11)=63.*(REAL(MASS)**(-1./3.))
      P(12)=6.11-0.021*REAL(MASS)
      P(10)=0.00015*(P(11)*REAL(Z))**2./(P(12)*(REAL(MASS)**(1./3.)))
      VAL=P(11)
      WRITE(6,36)VAL
 36   FORMAT('Centroid of the isoscalar E2 resonance in MeV <',F5.2,'>:',$)
      CALL READF(5,VAL)
      P(11)=VAL
      VAL=P(12)
      WRITE(6,37)VAL
 37   FORMAT('Width of the isoscalar E2 resonance in MeV <',F5.2,'>:',$)
      CALL READF(5,VAL)
      P(12)=VAL
      VAL=P(10)
      WRITE(6,38)VAL
 38   FORMAT('Cross-section of the isoscalar E2 resonance in mbarn <',F6.1,'>:',$)
      CALL READF(5,VAL)
      P(10)=VAL


      WRITE(6,'(A28)')'Start parameters for the fit'
      STEPS='y'
      WRITE(6,39)STEPS
 39   FORMAT('Steps in iteration equals 10% of the value <',A1,'>:',$)
      CALL READA(5,STEPS)
      IF(.NOT.((STEPS.EQ.'n').OR.(STEPS.EQ.'y')))STOP 'Wrong answer'
      BA=1
      WP='y'
      WRITE(6,40)WP
 40   FORMAT('Including a pygmy resonance in the fit <',A1,'>:',$)
      CALL READA(5,WP)
      IF(.NOT.((WP.EQ.'n').OR.(WP.EQ.'y')))STOP 'Wrong answer'
      IF(WP.EQ.'y')THEN
C Rescale deformation from beta to delta
C delta=(3/4)*SQRT(5/pi)*beta-45/(32*pi)*beta**2+675/(512*pi**2)*beta**4
C Formula taken from Nuclear Data Tables A7, 495-564 (1970) page 507
         DEF=.946*DEF-.448*DEF**2.
C Systematic of scissors mode taken from Phys. Rev. C 58, 184 (1998)
C Equation (11) for the centroid,
         A(2)=13.4*SQRT(1+9.*DEF**2.)*(REAL(MASS)**(-1./3.))
C Table I for the width,
         A(3)=1.
         IF(Z.GE.68)A(3)=1.3
C Equation (14) for the summed B(M1) strength
C pi/2*sigma*Gamma=16*pi**3/(9*hbar*c)*E_scissors*Summed B(M1) strength
C For the calculation of the coefficient, a bare g-factor was assumed (cg=1)
         A(1)=.987*DEF**3.*REAL(Z*Z)/(SQRT(1.+9.*DEF**2.)*REAL(MASS)*A(3))
         VAL=A(2)
         WRITE(6,41)VAL
 41      FORMAT('Centroid of the pygmy resonance in MeV <',F4.2,'>:',$)
         CALL READF(5,VAL)
         A(2)=VAL
         VAL=.1*A(2)
         IF(STEPS.EQ.'n')THEN
            WRITE(6,42)VAL
 42         FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
            CALL READF(5,VAL)
         ENDIF
         DELTAA(2)=VAL
         VAL=A(3)
         WRITE(6,43)VAL
 43      FORMAT('Width of the pygmy resonance in MeV <',F4.2,'>:',$)
         CALL READF(5,VAL)
         A(3)=VAL
         VAL=.1*A(3)
         IF(STEPS.EQ.'n')THEN
            WRITE(6,44)VAL
 44         FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
            CALL READF(5,VAL)
         ENDIF
         DELTAA(3)=VAL
         VAL=A(1)
         WRITE(6,45)VAL
 45      FORMAT('Cross-section of the pygmy resonance in mbarn <',F4.2,'>:',$)
         CALL READF(5,VAL)
         A(1)=VAL
         VAL=.1*A(1)
         IF(STEPS.EQ.'n')THEN
            WRITE(6,46)VAL
 46         FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
            CALL READF(5,VAL)
         ENDIF
      ELSE
         BA=4
         WRITE(6,'(A52)')'No pygmy resonance taken into account during the fit'
      ENDIF
      DELTAA(1)=VAL
C Hopefully little renormalization necessary
      VAL=1.
      WRITE(6,47)VAL
 47   FORMAT('Overall normalization constant <',F4.2,'>:',$)
      CALL READF(5,VAL)
      A(4)=VAL
      VAL=.1*A(4)
      IF(STEPS.EQ.'n')THEN
         WRITE(6,48)VAL
 48      FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
         CALL READF(5,VAL)
      ENDIF
      DELTAA(4)=VAL
      NA=4
      Q='v'
      WRITE(6,49)Q
 49   FORMAT('(v) variable, (c) constant, or (e) external Temperature <',A1,'>:',$)
      CALL READA(5,Q)
      IF(.NOT.((Q.EQ.'v').OR.(Q.EQ.'c').OR.(Q.EQ.'e')))STOP 'Wrong answer'
      IF((Q.EQ.'v').OR.(Q.EQ.'c'))THEN
C Temperature parameter taken from experience for rare earth nuclei
         VAL=.3
         WRITE(6,50)VAL
 50      FORMAT('Temperature in MeV <',F5.3,'>:',$)
         CALL READF(5,VAL)
         A(5)=VAL
         DO 51,I=1,NPTS
            T(I)=A(5)
 51      CONTINUE
         IF(Q.EQ.'v')THEN
            NA=5
            VAL=.1*A(5)
            IF(STEPS.EQ.'n')THEN
               WRITE(6,52)VAL
 52            FORMAT('Steps in fit for this parameter <',F6.4,'>:',$)
               CALL READF(5,VAL)
            ENDIF
            DELTAA(5)=VAL
         ELSE
            WRITE(6,'(A47)')'Keeping the temperature constant during the fit'
         ENDIF
      ELSE
         FILENAME='tave.paw'
         WRITE(6,'(A50)')'The following input file must have the same number' 
         WRITE(6,'(A52)')'of points and the same calibration as the input file'
         WRITE(6,'(A36)')'for the radiative strength function.'
         WRITE(6,53)FILENAME
 53      FORMAT('Name of external temperature data file <',A12,'>:',$)
         CALL READA(5,FILENAME)
         OPEN(11,ERR=98,FILE=FILENAME,STATUS='OLD')
         DO 55,I=1,NPTS
 54         FORMAT(X,F20.7)
            READ(11,54,ERR=96)T(I)
 55      CONTINUE
         CLOSE(11)
      ENDIF
      PROG=1.E-7
      MBEST=0
      ITER=0
      MODE=1
      NFREE=CHHI-CHLO+1-(NA-BA+1)
      WRITE(6,'(A40)')'**********  STARTING THE FIT  **********'
      WRITE(6,'(A27)')'Estimate from start values:'
 56   CONTINUE
      DO 57,I=CHLO,CHHI
         XX=X(I)
         TT=T(I)
         YFIT(I)=FUNC(XX,A,P,BUMP,BA,TT)
 57   CONTINUE
      ITER=ITER+1
      M=0
      CHISQR=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
      CH(M+1)=CHISQR
      BEST=CHISQR
      NBAD=0
      IF(((ITER.LE.3).AND.(STEPS.EQ.'y')).OR.(ITER.EQ.1))THEN
         IF((NA.EQ.5).AND.(BA.EQ.1))WRITE(6,58)
         IF((NA.EQ.4).AND.(BA.EQ.1))WRITE(6,59)
         IF((NA.EQ.5).AND.(BA.EQ.4))WRITE(6,60)
         IF((NA.EQ.4).AND.(BA.EQ.4))WRITE(6,61)
 58      FORMAT(' Loop    Chi**2    E-py G-py sigma  K   Temp ')
 59      FORMAT(' Loop    Chi**2    E-py G-py sigma  K  ')
 60      FORMAT(' Loop    Chi**2     K   Temp ')
 61      FORMAT(' Loop    Chi**2     K  ')
         IF(M.EQ.0)THEN
            IF((NA.EQ.5).AND.(BA.EQ.1))WRITE(6,62)MBEST,BEST,A(2),A(3),A(1),A(4),A(5)
            IF((NA.EQ.4).AND.(BA.EQ.1))WRITE(6,63)MBEST,BEST,A(2),A(3),A(1),A(4)
            IF((NA.EQ.5).AND.(BA.EQ.4))WRITE(6,64)MBEST,BEST,A(4),A(5)
            IF((NA.EQ.4).AND.(BA.EQ.4))WRITE(6,65)MBEST,BEST,A(4)
 62         FORMAT(X,I4,X,E12.5,X,F4.2,X,F4.2,X,F5.3,X,F4.2,X,F5.3)
 63         FORMAT(X,I4,X,E12.5,X,F4.2,X,F4.2,X,F5.3,X,F4.2)
 64         FORMAT(X,I4,X,E12.5,X,F4.2,X,F5.3)
 65         FORMAT(X,I4,X,E12.5,X,F4.2)
         ENDIF
      ENDIF
      IF(ITER.GT.1)WRITE(6,'(A34)')'Decreasing steps by a factor of 10'
      WRITE(6,'(A31)')'**********  FITTING  **********'
      MBEST=0
 66   CONTINUE
      M=M+1
      CALL GRIDLS(X,Y,SIGMAY,CHLO,CHHI,NFREE,MODE,A,DELTAA,
     +SIGMAA,NA,YFIT,CHISQR,P,BUMP,BA,T)
      CH(M+1)=CHISQR
      CHA=ABS(CH(M+1)-CH(M))/CH(M)
      IF(CHISQR.LE.BEST)THEN
         MBEST=M
         BEST=CHISQR
         DO 67,I=BA,NA
            AA(I)=A(I)
            SIGMAAA(I)=SIGMAA(I)
 67      CONTINUE
      ENDIF
      IF(CH(M+1).GT.(CH(M)+PROG))NBAD=NBAD+1
      IF(NBAD.EQ.7)THEN
         WRITE(6,'(A30)')'Running out of chi^2 minimum !'
         GO TO 68
      ENDIF
      IF(M.EQ.1000)THEN
         WRITE(6,'(A37)')'1000 Iterations without convergence !'
         GO TO 68
      ENDIF
      IF(CHA.GT.PROG)GO TO 66
      WRITE(6,'(A23)')'Iteration has converged'
C Severals problems here:
C 1.) Let us assume the iteration went out of some first minimum less than 7 
C     times, then it converges at a second (local) minimum with a chi^2 value 
C     larger than the value before it went out of the first minimum. The 
C     program will say that the iteration converged, while it did not really
C     converge in the deeper minimum.
C 2.) If it did not converge after 1000 iterations, why should decreasing step 
C     size help? (except if oscillating between 2 or more points in the 
C     parameter space)
 68   CONTINUE
      WRITE(6,69)M
 69   FORMAT('Gridls was running ',I4,' loops, with best result from')
      IF((ITER.GT.2).OR.(STEPS.EQ.'n'))THEN
         IF((NA.EQ.5).AND.(BA.EQ.1))WRITE(6,58)
         IF((NA.EQ.4).AND.(BA.EQ.1))WRITE(6,59)
         IF((NA.EQ.5).AND.(BA.EQ.1))WRITE(6,62)MBEST,BEST,AA(2),AA(3),AA(1),AA(4),AA(5)
         IF((NA.EQ.4).AND.(BA.EQ.1))WRITE(6,63)MBEST,BEST,AA(2),AA(3),AA(1),AA(4)
         IF((NA.EQ.5).AND.(BA.EQ.4))WRITE(6,60)
         IF((NA.EQ.4).AND.(BA.EQ.4))WRITE(6,61)
         IF((NA.EQ.5).AND.(BA.EQ.4))WRITE(6,64)MBEST,BEST,AA(4),AA(5)
         IF((NA.EQ.4).AND.(BA.EQ.4))WRITE(6,65)MBEST,BEST,AA(4)
         Q='n'
         WRITE(6,70)Q
 70      FORMAT('New calulation with smaller steps <',A1,'>:',$)
         CALL READA(5,Q)
         IF(.NOT.((Q.EQ.'n').OR.(Q.EQ.'y')))STOP 'Wrong answer'
         IF(Q.EQ.'n')GO TO 73
      ENDIF
      DO 71,I=BA,NA
         A(I)=AA(I)
         SIGMAA(I)=SIGMAAA(I)
         DELTAA(I)=.1*DELTAA(I)
 71   CONTINUE
      IF(NA.EQ.5)THEN
         DO 72,I=CHLO,CHHI
            T(I)=A(5)
 72      CONTINUE
      ENDIF
      GO TO 56
 73   CONTINUE
      IF(NA.EQ.5)THEN
         DO 74,I=1,NPTS
            T(I)=AA(5)
 74      CONTINUE
      ENDIF
      DO 75,I=1,NPTS
         XX=X(I)
         TT=T(I)
         YFIT(I)=FUNC(XX,AA,P,BUMP,BA,TT)
 75   CONTINUE
      OPEN(11,ERR=98,FILE='sigfit.cnt')
      DO 77,I=1,NPTS
         WRITE(11,76,ERR=96)X(I),Y(I),SIGMAY(I),YFIT(I)
 76      FORMAT(4(X,E12.5))
 77   CONTINUE
      CLOSE(11)
      WRITE(6,'(A17)')'Final result with'
      IF(BA.EQ.1)THEN
         WRITE(6,78)AA(2),SIGMAAA(2)
         WRITE(6,79)AA(3),SIGMAAA(3)
         WRITE(6,80)AA(1),SIGMAAA(1)
      ENDIF
      WRITE(6,81)AA(4),SIGMAAA(4)
      IF(NA.EQ.5)WRITE(6,82)AA(5),SIGMAAA(5)
 78   FORMAT('E-py=',F4.2,'+/-',F4.2)
 79   FORMAT('G-py=',F4.2,'+/-',F4.2)
 80   FORMAT('sigma=',F5.3,'+/-',F5.3)
 81   FORMAT('K=',F4.2,'+/-',F4.2)
 82   FORMAT('Temp=',F5.3,'+/-',F5.3)
      STOP
 96   WRITE(6,97)I
 97   FORMAT('Error during reading/writing line: ',I3)
      STOP
 98   WRITE(6,99)FILENAME
 99   FORMAT('Error during opening file: ',A12)
      STOP
      END

      SUBROUTINE GRIDLS(X,Y,SIGMAY,CHLO,CHHI,NFREE,MODE,A,DELTAA,
     +SIGMAA,NA,YFIT,CHISQR,P,BUMP,BA,T)
      DIMENSION X(100),Y(100),SIGMAY(100),YFIT(100)
      DIMENSION A(5),DELTAA(5),SIGMAA(5),P(12),T(100)
      REAL CHISQR,XX,TT,CHISQ1,DELTA,CHISQ2,DUMMY,CHISQ3
      INTEGER CHLO,CHHI,NFREE,MODE,NA,BUMP,BA,J
      CHISQR=0.
      IF(NFREE)115,115,100
 100  DO 113 J=BA,NA
C EVALUATE CHI SQUARE AT FIRST TWO SEARCH POINTS
         DO 101 I=CHLO,CHHI
            XX=X(I)
            TT=T(I)
            YFIT(I)=FUNC(XX,A,P,BUMP,BA,TT)
 101     CONTINUE
         CHISQ1=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
         DELTA=.5*DELTAA(J)
 102     DELTA=DELTA*2.
         A(J)=A(J)+DELTA
         IF(J.EQ.5)THEN
            DO 103 I=CHLO,CHHI
               T(I)=A(J)
 103        CONTINUE
         ENDIF
         DO 104 I=CHLO,CHHI
            XX=X(I)
            TT=T(I)
            YFIT(I)=FUNC(XX,A,P,BUMP,BA,TT)
 104     CONTINUE
         CHISQ2=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
         IF(CHISQ1-CHISQ2)105,102,107
C REVERSE DIRECTION OF SEARCH IF CHI SQUARE IS INCREASING
 105     DELTA=-DELTA
         A(J)=A(J)+DELTA
         IF(J.EQ.5)THEN
            DO 106 I=CHLO,CHHI
               T(I)=A(J)
 106        CONTINUE
         ENDIF
         DUMMY=CHISQ1
         CHISQ1=CHISQ2
         CHISQ2=DUMMY
C INCREMENT A(J) UNTIL CHI SQUARE INCREASES
 107     A(J)=A(J)+DELTA
         IF(J.EQ.5)THEN
            DO 108 I=CHLO,CHHI
               T(I)=A(J)
 108        CONTINUE
         ENDIF
         DO 109 I=CHLO,CHHI
            XX=X(I)
            TT=T(I)
            YFIT(I)=FUNC(XX,A,P,BUMP,BA,TT)
 109     CONTINUE
         CHISQ3=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
         IF(CHISQ3-CHISQ2)110,111,111
 110     CHISQ1=CHISQ2
         CHISQ2=CHISQ3
         GO TO 107
C FIND MIN OF PARABOLA DEFINED BY LAST THREE POINTS
 111     DELTA=DELTA*(1./(1.+(CHISQ1-CHISQ2)/(CHISQ3-CHISQ2))+0.5)
         A(J)=A(J)-DELTA
         IF(J.EQ.5)THEN
            DO 112 I=CHLO,CHHI
               T(I)=A(J)
 112        CONTINUE
         ENDIF
C Must be wrong to use NFREE here, taken away, Magne, June 23. 2002
C Implemented by Andreas Schiller, June 26 2002
C         SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
         SIGMAA(J)=DELTAA(J)*SQRT(2./(CHISQ3-2.*CHISQ2+CHISQ1))
 113  CONTINUE
C EVALUATE FIT AND CHI SQUARE FOR FINAL PARAMETERS
      DO 114 I=CHLO,CHHI
         XX=X(I)
         TT=T(I)
         YFIT(I)=FUNC(XX,A,P,BUMP,BA,TT)
 114  CONTINUE
      CHISQR=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
 115  RETURN
      END

C FUNCTION TO CALCULATE CHI SQUARE

      FUNCTION FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
      DIMENSION Y(100),SIGMAY(100),YFIT(100)
      REAL CHISQ,WEIGHT
      INTEGER CHLO,CHHI,NFREE,MODE,I
      CHISQ=0.
      IF(NFREE)200,200,201
 200  FCHISQ=0.
      GO TO 209
 201  DO 208 I=CHLO,CHHI
         IF(MODE)202,205,206
 202     IF(Y(I))204,205,203
 203     WEIGHT=1./Y(I)
         GO TO 207
 204     WEIGHT=1./(-Y(I))
         GO TO 207
 205     WEIGHT=1.
         GO TO 207
 206     WEIGHT=1./SIGMAY(I)**2.
 207     CHISQ=CHISQ+WEIGHT*(Y(I)-YFIT(I))**2.
 208  CONTINUE
      FCHISQ=CHISQ/REAL(NFREE)
 209  RETURN
      END

C FIT FUNCTION

      FUNCTION FUNC(X,A,P,BUMP,BA,T)
      DIMENSION A(5),P(12)
      REAL X,X2,T,E1O,E11O,E11U,E12O,E12U,E1,P92,M1O,M1U,M1,P122,E2O,E2U,E2
      REAL A32,PY1O,PY1U,PY
      INTEGER BUMP,BA
      X2=X*X
      E1O=X2+39.47841760*T*T
      E11O=.7*P(1)*P(3)*P(3)/P(2)
      E11U=(X2-P(2)*P(2))**2.
      IF(BUMP.EQ.2)THEN
         E12O=.7*P(4)*P(6)*P(6)/P(5)
         E12U=(X2-P(5)*P(5))**2.
         E1=(E11O/E11U+E12O/E12U)*E1O
      ELSE
         E1=(E11O/E11U)*E1O
      ENDIF
      P92=P(9)*P(9)
      M1O=P(7)*P92*X
      M1U=(X2-P(8)*P(8))**2.+X2*P92
      M1=M1O/M1U
      P122=P(12)*P(12)
      E2O=P(10)*P122*X
      E2U=(X2-P(11)*P(11))**2.+X2*P122
      E2=3.*E2O/(5.*E2U)
      IF(BA.EQ.1)THEN
         A32=A(3)*A(3)
         PY1O=A(1)*A32*X
         PY1U=(X2-A(2)*A(2))**2.+X2*A32
         PY=PY1O/PY1U
         FUNC=8.673723898E-8*(A(4)*(E1+M1)+E2+PY)
      ELSE
         FUNC=8.673723898E-8*(A(4)*(E1+M1)+E2)
      ENDIF
      RETURN          
      END

