      PROGRAM MODEL
      DIMENSION YFIT(100),X(100),Y(100),SIGMAY(100)
      DIMENSION A(5),DELTAA(5),SIGMAA(5),P(9),AA(5),SIGMAAA(5)
      DIMENSION CH(1001)
      CHARACTER *20 FILENAME
      CHARACTER *1 Q,STEPS
      INTEGER NPTS,I,CHLO,CHHI,MASS,Z,BUMP,NA,NFREE,MBEST,ITER,MODE,M,NBAD
      REAL ELO,EHI,STEP,DEF,VAL,M17M,XX,CHISQR,BEST,PROG,CHA
      WRITE(6,*)' _____________________________'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|        M O D E L 1.0        |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'| Program to fit experimental |'
      WRITE(6,*)'| strength functions to a sum |'
      WRITE(6,*)'|        of models of         |'
      WRITE(6,*)'| the GDR (Kadmenskii et al.) |'
      WRITE(6,*)'|  the M1 spin-flip resonance |'
      WRITE(6,*)'|   and the pygmy resonance   |'
      WRITE(6,*)'|      (both Lorentzian)      |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|  Oslo Cyclotron Laboratory  |'
      WRITE(6,*)'|                             |'
      WRITE(6,*)'|    Created: 06/09 - 2000    |'
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
 12      FORMAT(1X,E14.7E2)
         IF(I.LE.NPTS)THEN
            READ(11,12,ERR=96)Y(I)
         ELSE
            READ(11,12,ERR=96)SIGMAY(I-NPTS)
         ENDIF
 13   CONTINUE
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
      P(8)=41.*(REAL(MASS)**(-1./3.))
      P(9)=4.
      M17M=1./(((49.-P(8)**2.)/(7.*P(9)))**2.+1.)
      P(7)=.1275*(REAL(MASS)**.47)/M17M
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
      WRITE(6,'(A28)')'Start parameters for the fit'
      STEPS='y'
      WRITE(6,36)STEPS
 36   FORMAT('Steps in iteration equals 10% of the value <',A1,'>:',$)
      CALL READA(5,STEPS)
      IF(.NOT.((STEPS.EQ.'n').OR.(STEPS.EQ.'y')))STOP 'Wrong answer'
      DEF=.946*DEF-.448*DEF**2.
      A(2)=13.4*SQRT(1+9.*DEF**2.)*(REAL(MASS)**(-1./3.))
      A(3)=1.
      A(1)=.987*DEF**3.*REAL(Z*Z)/(SQRT(1.+9.*DEF**2.)*REAL(MASS)*A(3))
      VAL=A(2)
      WRITE(6,37)VAL
 37   FORMAT('Centroid of the pygmy resonance in MeV <',F4.2,'>:',$)
      CALL READF(5,VAL)
      A(2)=VAL
      VAL=.1*A(2)
      IF(STEPS.EQ.'n')THEN
         WRITE(6,38)VAL
 38      FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
         CALL READF(5,VAL)
      ENDIF
      DELTAA(2)=VAL
      VAL=A(3)
      WRITE(6,39)VAL
 39   FORMAT('Width of the pygmy resonance in MeV <',F4.2,'>:',$)
      CALL READF(5,VAL)
      A(3)=VAL
      VAL=.1*A(3)
      IF(STEPS.EQ.'n')THEN
         WRITE(6,40)VAL
 40      FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
         CALL READF(5,VAL)
      ENDIF
      DELTAA(3)=VAL
      VAL=A(1)
      WRITE(6,41)VAL
 41   FORMAT('Cross-section of the pygmy resonance in mbarn <',F4.2,'>:',$)
      CALL READF(5,VAL)
      A(1)=VAL
      VAL=.1*A(1)
      IF(STEPS.EQ.'n')THEN
         WRITE(6,42)VAL
 42      FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
         CALL READF(5,VAL)
      ENDIF
      DELTAA(1)=VAL
      VAL=1.
      WRITE(6,43)VAL
 43   FORMAT('Overall normalization constant <',F4.2,'>:',$)
      CALL READF(5,VAL)
      A(4)=VAL
      VAL=.1*A(4)
      IF(STEPS.EQ.'n')THEN
         WRITE(6,44)VAL
 44      FORMAT('Steps in iteration for this parameter <',F5.3,'>:',$)
         CALL READF(5,VAL)
      ENDIF
      DELTAA(4)=VAL
      VAL=.3
      WRITE(6,45)VAL
 45   FORMAT('Temperature <',F5.3,'>:',$)
      CALL READF(5,VAL)
      A(5)=VAL
      Q='n'
      WRITE(6,46)Q
 46   FORMAT('Keeping the temperature constant in the fit <',A1,'>:',$)
      CALL READA(5,Q)
      IF(.NOT.((Q.EQ.'n').OR.(Q.EQ.'y')))STOP 'Wrong answer'
      IF(Q.EQ.'n')THEN
         NA=5
         VAL=.1*A(5)
         IF(STEPS.EQ.'n')THEN
            WRITE(6,47)VAL
 47         FORMAT('Steps in fit for this parameter <',F6.4,'>:',$)
            CALL READF(5,VAL)
         ENDIF
         DELTAA(5)=VAL
      ELSE
         NA=4
         WRITE(6,'(A47)')'Keeping the temperature constant during the fit'
      ENDIF
      PROG=1.E-7
      MBEST=0
      ITER=0
      MODE=1
      NFREE=CHHI-CHLO+1-NA
      WRITE(6,'(A40)')'**********  STARTING THE FIT  **********'
      WRITE(6,'(A27)')'Estimate from start values:'
 48   CONTINUE
      DO 49,I=CHLO,CHHI
         XX=X(I)
         YFIT(I)=FUNC(XX,A,P,BUMP)
 49   CONTINUE
      ITER=ITER+1
      M=0
      CHISQR=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
      CH(M+1)=CHISQR
      BEST=CHISQR
      NBAD=0
      IF(((ITER.LE.3).AND.(STEPS.EQ.'y')).OR.(ITER.EQ.1))THEN
         IF(NA.EQ.5)WRITE(6,50)
         IF(NA.EQ.4)WRITE(6,51)
 50      FORMAT(' Loop    Chi**2    E-py G-py sigma  K   Temp ')
 51      FORMAT(' Loop    Chi**2    E-py G-py sigma  K  ')
         IF(M.EQ.0)THEN
            IF(NA.EQ.5)WRITE(6,52)MBEST,BEST,A(2),A(3),A(1),A(4),A(5)
            IF(NA.EQ.4)WRITE(6,53)MBEST,BEST,A(2),A(3),A(1),A(4)
 52         FORMAT(1X,I4,1X,E12.5,1X,F4.2,1X,F4.2,1X,F5.3,1X,F4.2,1X,F5.3)
 53         FORMAT(1X,I4,1X,E12.5,1X,F4.2,1X,F4.2,1X,F5.3,1X,F4.2)
         ENDIF
      ENDIF
      IF(ITER.GT.1)WRITE(6,'(A34)')'Decreasing steps by a factor of 10'
      WRITE(6,'(A31)')'**********  FITTING  **********'
      MBEST=0
 54   CONTINUE
      M=M+1
      CALL GRIDLS(X,Y,SIGMAY,CHLO,CHHI,NFREE,MODE,A,DELTAA,
     +SIGMAA,NA,YFIT,CHISQR,P,BUMP)
      CH(M+1)=CHISQR
      CHA=ABS(CH(M+1)-CH(M))/CH(M)
      IF(CHISQR.LE.BEST)THEN
         MBEST=M
         BEST=CHISQR
         DO 55,I=1,NA
            AA(I)=A(I)
            SIGMAAA(I)=SIGMAA(I)
 55      CONTINUE
      ENDIF
      IF(CH(M+1).GT.(CH(M)+PROG))NBAD=NBAD+1
      IF(NBAD.EQ.7)THEN
         WRITE(6,'(A30)')'Running out of chi^2 minimum !'
         GO TO 56
      ENDIF
      IF(M.EQ.1000)THEN
         WRITE(6,'(A37)')'1000 Iterations without convergence !'
         GO TO 56
      ENDIF
      IF(CHA.GT.PROG)GO TO 54
      WRITE(6,'(A23)')'Iteration has converged'
 56   CONTINUE   
      WRITE(6,57)M
 57   FORMAT('Gridls was running ',I4,' loops, with best result from')
      IF((ITER.GT.2).OR.(STEPS.EQ.'n'))THEN
         IF(NA.EQ.5)WRITE(6,50)
         IF(NA.EQ.4)WRITE(6,51)
         IF(NA.EQ.5)WRITE(6,52)MBEST,BEST,AA(2),AA(3),AA(1),AA(4),AA(5)
         IF(NA.EQ.4)WRITE(6,53)MBEST,BEST,AA(2),AA(3),AA(1),AA(4)
         Q='n'
         WRITE(6,58)Q
 58      FORMAT('New calulation with smaller steps <',A1,'>:',$)
         CALL READA(5,Q)
         IF(.NOT.((Q.EQ.'n').OR.(Q.EQ.'y')))STOP 'Wrong answer'
         IF(Q.EQ.'n')GO TO 60
      ENDIF
      DO 59,I=1,NA
         A(I)=AA(I)
         SIGMAA(I)=SIGMAAA(I)
         DELTAA(I)=.1*DELTAA(I)
 59   CONTINUE
      GO TO 48
 60   CONTINUE
      DO 61,I=1,NPTS
         XX=X(I)
         YFIT(I)=FUNC(XX,AA,P,BUMP)
 61   CONTINUE
      OPEN(11,ERR=98,FILE='sigfit.cnt')
      DO 63,I=1,NPTS
         WRITE(11,62,ERR=96)X(I),Y(I),SIGMAY(I),YFIT(I)
 62      FORMAT(4(1X,E12.5))
 63   CONTINUE
      CLOSE(11)
      WRITE(6,'(A17)')'Final result with'
      WRITE(6,64)AA(2),SIGMAAA(2)
      WRITE(6,65)AA(3),SIGMAAA(3)
      WRITE(6,66)AA(1),SIGMAAA(1)
      WRITE(6,67)AA(4),SIGMAAA(4)
      IF(NA.EQ.5)WRITE(6,68)AA(5),SIGMAAA(5)
 64   FORMAT('E-py=',F4.2,'+/-',F4.2)
 65   FORMAT('G-py=',F4.2,'+/-',F4.2)
 66   FORMAT('sigma=',F5.3,'+/-',F5.3)
 67   FORMAT('K=',F4.2,'+/-',F4.2)
 68   FORMAT('Temp=',F5.3,'+/-',F5.3)
      STOP
 96   WRITE(6,97)I
 97   FORMAT('Error during reading/writing line: ',I3)
      STOP
 98   WRITE(6,99)FILENAME
 99   FORMAT('Error during opening file: ',A12)
      STOP
      END

      SUBROUTINE GRIDLS(X,Y,SIGMAY,CHLO,CHHI,NFREE,MODE,A,DELTAA,
     +SIGMAA,NA,YFIT,CHISQR,P,BUMP)
      DIMENSION X(100),Y(100),SIGMAY(100),YFIT(100)
      DIMENSION A(5),DELTAA(5),SIGMAA(5),P(9)
      REAL CHISQR,XX,CHISQ1,DELTA,CHISQ2,DUMMY,CHISQ3
      INTEGER CHLO,CHHI,NFREE,MODE,NA,BUMP,J
      CHISQR=0.
      IF(NFREE)111,111,100
 100  DO 109 J=1,NA
C EVALUATE CHI SQUARE AT FIRST TWO SEARCH POINTS
         DO 101 I=CHLO,CHHI
            XX=X(I)
            YFIT(I)=FUNC(XX,A,P,BUMP)
 101     CONTINUE
         CHISQ1=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
         DELTA=.5*DELTAA(J)
 102     DELTA=DELTA*2.
         A(J)=A(J)+DELTA
         DO 103 I=CHLO,CHHI
            XX=X(I)
            YFIT(I)=FUNC(XX,A,P,BUMP)
 103     CONTINUE
         CHISQ2=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
         IF(CHISQ1-CHISQ2)104,102,105
C REVERSE DIRECTION OF SEARCH IF CHI SQUARE IS INCREASING
 104     DELTA=-DELTA
         A(J)=A(J)+DELTA
         DUMMY=CHISQ1
         CHISQ1=CHISQ2
         CHISQ2=DUMMY
C INCREMENT A(J) UNTIL CHI SQUARE INCREASES
 105     A(J)=A(J)+DELTA
         DO 106 I=CHLO,CHHI
            XX=X(I)
            YFIT(I)=FUNC(XX,A,P,BUMP)
 106     CONTINUE
         CHISQ3=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
         IF(CHISQ3-CHISQ2)107,108,108
 107     CHISQ1=CHISQ2
         CHISQ2=CHISQ3
         GO TO 105
C FIND MIN OF PARABOLA DEFINED BY LAST THREE POINTS
 108     DELTA=DELTA*(1./(1.+(CHISQ1-CHISQ2)/(CHISQ3-CHISQ2))+0.5)
         A(J)=A(J)-DELTA
         SIGMAA(J)=DELTAA(J)*SQRT(2./(REAL(NFREE)*(CHISQ3-2.*CHISQ2+CHISQ1)))
 109  CONTINUE
C EVALUATE FIT AND CHI SQUARE FOR FINAL PARAMETERS
      DO 110 I=CHLO,CHHI
         XX=X(I)
         YFIT(I)=FUNC(XX,A,P,BUMP)
 110  CONTINUE
      CHISQR=FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
 111  RETURN
      END

C FUNCTION TO CALCULATE CHI SQUARE

      FUNCTION FCHISQ(Y,SIGMAY,CHLO,CHHI,NFREE,MODE,YFIT)
      DIMENSION Y(100),SIGMAY(100),YFIT(100)
      REAL CHISQ,WEIGHT
      INTEGER CHLO,CHHI,NFREE,MODE,I
      CHISQ=0.
      IF(NFREE)200,200,201
 200  FCHISQ=0.
      GO TO 208
 201  DO 207 I=CHLO,CHHI
         IF(MODE)202,205,206
 202     IF(Y(I))204,205,203
 203     WEIGHT=1./Y(I)
         GO TO 207
 204     WEIGHT=1./(-Y(I))
         GO TO 207
 205     WEIGHT=1.
         GO TO 207
 206     WEIGHT=1./SIGMAY(I)**2.
         CHISQ=CHISQ+WEIGHT*(Y(I)-YFIT(I))**2.
 207  CONTINUE
      FCHISQ=CHISQ/REAL(NFREE)
 208  RETURN
      END

C FIT FUNCTION

      FUNCTION FUNC(X,A,P,BUMP)
      DIMENSION A(5),P(9)
      REAL X,X2,EO,E1O,E1U,E2O,E2U,P92,MO,MU,A32,PO,PU,E1,M1,PY
      INTEGER BUMP
      X2=X*X
      EO=X2+39.47841760*A(5)*A(5)
      E1O=.7*P(1)*P(3)*P(3)/P(2)
      E1U=(X2-P(2)*P(2))**2.
      IF(BUMP.EQ.2)THEN
         E2O=.7*P(4)*P(6)*P(6)/P(5)
         E2U=(X2-P(5)*P(5))**2.
      ENDIF
      P92=P(9)*P(9)
      MO=P(7)*P92*X
      MU=(X2-P(8)*P(8))**2.+X2*P92
      A32=A(3)*A(3)
      PO=A(1)*A32*X
      PU=(X2-A(2)*A(2))**2.+X2*A32
      IF(BUMP.EQ.2)THEN
         E1=(E1O/E1U+E2O/E2U)*EO
      ELSE
         E1=(E1O/E1U)*EO
      ENDIF
      M1=MO/MU
      PY=PO/PU
      FUNC=8.673723898E-8*(A(4)*(E1+M1)+PY)
      RETURN          
      END

