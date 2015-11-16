      PROGRAM POLAR

*     This program calculates the direction-polarization correlation of gammas 
*     for a given Spin cascade:
*
*                   L1+delta1*(L1+1)                       L2+delta2*(L2+1)
*     J(initial)---------------------->J(intermediate)----------------------->J(final)
*
*     The multipolarities L1 and L2 can be adjusted to the users wishes, although in
*     most cases the lowest possible multipolarities, also taken as defaults, might apply.
*     In addition, mixing parameters delta1 and delta2 are taken into account. These 
*     mixing parameters reflect the ammount of the next higher multipole transition,
*     e.g. M1+delta*E2. Since mixing parameters are phase sensitive, also negative
*     mixing parameters can occur, even mixing parameters greater than 1 are possible.
*     So mixing parameters range from -infinity to +infinity. It is also worth to notice,
*     that this program uses mixing parameters of the Biedenharn-Rose type.
*
*     The polarization is calculated either for the first or the second gamma 
*     transition. The angle between the two gamma rays has to be specified by the user.
*     It is also possible to calculate the direction-polarization correlation for a set
*     of different angles between the two gamma rays. 
*
*     As output, this program calculates coefficients of Legendre polynomials, called 
*     A0, A2, A4, and E2, E4 etc. The angular distribution then, can be written as:
*     W(theta,phi)*dOmega=(A0+A2*P2+E2*P22*COS(2*phi)+A4*P4+E4*P42*COS(2*phi)+...)*dOmega, 
*     whereas theta is the angle between the first and the second gamma, P2, P4, etc. are 
*     Legendre polynomials, P22, P42, etc. are associated Legendre polynomials of the first
*     kind, and phi is the angle between the plane of the directions of motion of the two
*     gamma rays and the plane of Compton scattering of the polarized photon.
*
*     In the case of a given angle theta, the direction-polarization correlation can be
*     written as W(phi)=A0+A2*COS(2*phi), with phi as defined above. 
*
*     The output is a table, showing the A and E coefficients if neccessary, and several 
*     files, which are customized to be used by PAW.

      IMPLICIT LOGICAL(P)
      IMPLICIT CHARACTER (Q)
      REAL PI
      DIMENSION AE(0:180),EE(0:180),T(0:180),TC(0:180),TS(0:180),
     +R0M(0:180),R0O(0:180),R0P(0:180),R1M(0:180),R1O(0:180),R1P(0:180)

*     Initializing 
      PI=ABS(ACOS(-1.0))
      CONV=PI/180.0

*     Printig header
      WRITE(6,'(A50)')'     ___________________________________________  '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |              P O L A R   1.0              | '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |Program to calculate direction-polarization| '
      WRITE(6,'(A50)')'    | correlation of a given gamma ray cascade. | '
      WRITE(6,'(A50)')'    |   Formulae based on Landolt-Boernstein,   | '
      WRITE(6,'(A50)')'    | Numerical Tables  for Angular Correlation | '
      WRITE(6,'(A50)')'    | Computations in alpha-, beta-  and gamma- | '
      WRITE(6,'(A50)')'    |   Spectroscopy. Springer, Berlin, 1968    | '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |   Oslo Cyclotron Laboratory, July 1997    | '
      WRITE(6,'(A50)')'    |___________________________________________| '
      WRITE(6,'(X)')

*     Input some data
      Q='e'
      WRITE(6,1)Q
 1    FORMAT('Calculation for odd (o) or even (e) nucleus  <',A1,'>:',$)
      CALL READA(5,Q)
      LI=1
      WRITE(6,2)LI
 2    FORMAT('Initial Spin           (*2 for odd nucleus) <',I2,'>:',$)  
      CALL READI(5,LI)
      QI='+'
      WRITE(6,3)QI
 3    FORMAT('Initial Parity                               <',A1,'>:',$)
      CALL READA(5,QI)
      IF (.NOT.((QI.EQ.'+').OR.(QI.EQ.'-'))) STOP 'Initial parity out of range'
      LM=1
      WRITE(6,4)LM
 4    FORMAT('Intermediate Spin      (*2 for odd nucleus) <',I2,'>:',$)
      CALL READI(5,LM)
      QM='+'
      WRITE(6,5)QM
 5    FORMAT('Intermediate Parity                          <',A1,'>:',$)
      CALL READA(5,QM)
      IF (.NOT.((QM.EQ.'+').OR.(QM.EQ.'-'))) STOP 'Intermediate parity out of range'
      LF=1
      WRITE(6,6)LF
 6    FORMAT('Final Spin             (*2 for odd nucleus) <',I2,'>:',$)
      CALL READI(5,LF)
      QF='+'
      WRITE(6,7)QF
 7    FORMAT('Final Parity                                 <',A1,'>:',$)
      CALL READA(5,QF)
      IF (.NOT.((QF.EQ.'+').OR.(QF.EQ.'-'))) STOP 'Final parity out of range'
      IF ((LM.EQ.0).AND.((LI.EQ.0).OR.(LF.EQ.0))) STOP '0-->0 transition not allowed'
     
*     Now, calculating multipolarity of transitions
      IF (Q.EQ.'o') THEN
         L1=ABS(LI-LM)/2
         L2=ABS(LF-LM)/2
         L1P=(LI+LM)/2
         L2P=(LF+LM)/2
      ELSE 
         L1=ABS(LI-LM)
         L2=ABS(LF-LM)
         L1P=LI+LM
         L2P=LF+LM
      END IF
*     E0 or M0 transitions are not taken into account
      IF (L1.EQ.0) L1=1
      IF (L2.EQ.0) L2=1

*     Checking if calculated multipolarity is according to users wishes
      IF (L1P.GT.L1) THEN
         WRITE(6,'(A48,I2)')'Calculated  multipolarity for first transition: ',L1
         WRITE(6,'(A48,I2)')'Maximum  multipolarity  for  first  transition: ',L1P
         Q1='n'
         WRITE(6,8)Q1
 8       FORMAT('Do you want to increase multipolarity <',A1,'>:',$)
         CALL READA(5,Q1)
         IF (Q1.EQ.'y') THEN
            L1N=L1+1
            WRITE(6,9)L1N
 9          FORMAT('Type  new  multipolarity for first transition <',I2,'>:',$)
            CALL READI(5,L1N)
            IF ((L1N.LT.L1).OR.(L1N.GT.L1P)) THEN 
               STOP 'Multipolarity out of range'
            ELSE 
               L1=L1N
            END IF
         END IF
      END IF
      IF (L2P.GT.L2) THEN
         WRITE(6,'(A48,I2)')'Calculated multipolarity for second transition: ',L2
         WRITE(6,'(A48,I2)')'Maximum  multipolarity  for  second transition: ',L2P
         Q2='n'
         WRITE(6,10)Q2
 10      FORMAT('Do you want to increase multipolarity <',A1,'>:',$)
         CALL READA(5,Q2)
         IF (Q2.EQ.'y') THEN
            L2N=L2+1
            WRITE(6,11)L2N
 11         FORMAT('Type  new multipolarity for second transition <',I2,'>:',$)
            CALL READI(5,L2N)
            IF ((L2N.LT.L2).OR.(L2N.GT.L2P)) THEN 
               STOP 'Multipolarity out of range'
            ELSE 
               L2=L2N
            END IF
         END IF
      END IF

*     Calculating type of transition
      S1=1.0
      IF (((QI.EQ.QM).AND.(MOD(L1,2).EQ.0)).OR.((QI.NE.QM).AND.(MOD(L1,2).EQ.1))) S1=-1.0
      S2=1.0
      IF (((QF.EQ.QM).AND.(MOD(L2,2).EQ.0)).OR.((QF.NE.QM).AND.(MOD(L2,2).EQ.1))) S2=-1.0

*     Input of mixing parameters if required
      D1=0.0
      Q1='n'
      IF (L1P.GT.L1) THEN
         WRITE(6,12)Q1
 12      FORMAT('Mixing  for first transition <',A1,'>:',$)
         CALL READA(5,Q1)
         IF (Q1.EQ.'y') THEN
            D1=1.0
            WRITE(6,13)D1
 13         FORMAT('Type  mixing parameter for first transition <',SP,F4.1,'>:',$)
            CALL READF(5,D1)
            IF (D1.EQ.0.0) Q1='n'
         ELSE 
            Q1='n'
         END IF
      END IF
      D2=0.0
      Q2='n'
      IF (L2P.GT.L2) THEN
         WRITE(6,14)Q2
 14      FORMAT('Mixing for second transition <',A1,'>:',$)
         CALL READA(5,Q2)
         IF (Q2.EQ.'y') THEN
            D2=1.0
            WRITE(6,15)D2
 15         FORMAT('Type mixing parameter for second transition <',SP,F4.1,'>:',$)
            CALL READF(5,D2)
            IF (D2.EQ.0.0) Q2='n'
         ELSE 
            Q2='n'
         END IF
      END IF

*     Input which gamma ray is the polarized one
      IP=2
      WRITE(6,'(A39)')'Which of the two gamma rays forming the'
      WRITE(6,'(A41)')'cascade, is observed in the polarimeter ?'
      WRITE(6,16)IP
 16   FORMAT('Number of the polarized gamma ray <',I1,'>:',$)
      CALL READI(5,IP)
      IF (.NOT.((IP.EQ.1).OR.(IP.EQ.2))) STOP 'Gamma ray number out of range'

*     Input of angle between the two gamma rays
      THETA=90.0
      WRITE(6,'(A42)')'Angle between the two gamma rays in degree'
      WRITE(6,'(A41)')'If you type 0.0, all angles between 0 and'
      WRITE(6,'(A39)')'180 degrees will be taken into account.'
      WRITE(6,17)THETA
 17   FORMAT('Angle between the two gammas <',F7.3,'>:',$)
      CALL READF(5,THETA)
      IF ((THETA.LT.0.0).OR.(THETA.GT.180.0)) STOP 'Angle out of range'

*     Calculating a set of angles
      NA=0
      IF (THETA.EQ.0.0) NA=180
      DO 18,J=0,NA,1
         T(J)=(REAL(J)+THETA)*CONV
         TS(J)=SIN(T(J))
         TC(J)=COS(T(J))
         R0M(J)=TC(J)
         R0O(J)=1.5*TC(J)*R0M(J)-0.5
         R1M(J)=0.0
         R1O(J)=3.0*TS(J)*TS(J)
         AE(J)=1.0
         EE(J)=0.0
 18   CONTINUE

*     Calculating maximal loop value
      IF ((Q1.EQ.'n').AND.(Q2.EQ.'n')) M=MIN(2*L1,2*L2)
      IF ((Q1.EQ.'y').AND.(Q2.EQ.'n')) M=MIN(2*L1+2,2*L2)
      IF ((Q1.EQ.'n').AND.(Q2.EQ.'y')) M=MIN(2*L1,2*L2+2)
      IF ((Q1.EQ.'y').AND.(Q2.EQ.'y')) M=MIN(2*L1+2,2*L2+2)
      IF (Q.EQ.'o') THEN
         M=MIN(LM,M)
      ELSE
         M=MIN(2*LM,M)
      END IF
      IF (MOD(M,2).EQ.1) M=M-1

*     Type conversion to real
      IF (Q.EQ.'o') THEN
         BI=REAL(LI)/2.0
         BM=REAL(LM)/2.0
         BF=REAL(LF)/2.0
      ELSE
         BI=REAL(LI)
         BM=REAL(LM)
         BF=REAL(LF)
      END IF
      B1=REAL(L1)
      B2=REAL(L2)

*     Opening file for use with PAW
      OPEN(10,FILE='polar.kumac',STATUS='UNKNOWN')
      WRITE(10,'(A11)')'macro polar'
      WRITE(10,'(A18)')'histogram/delete *'
      WRITE(10,'(A15)')'vector/delete *'
      WRITE(10,'(A14)')'graphics/set *'
      WRITE(10,'(A17)')'graphics/option *'
      WRITE(10,'(A19)')'graphics/option zfl'
      WRITE(10,'(A20)')'graphics/option nbox'
      WRITE(10,'(A18)')'graphics/misc/next'

*     The first coefficient is trivial
      WRITE(6,'(X)')
      WRITE(6,'(A2,I3,X,A2,SP,E10.3)')'k=',0,'A=',1.0

*     Calculating the other coefficients
      IF (M.GT.0) THEN
*     Opening data file for output
         OPEN(11,FILE='polar.out',STATUS='UNKNOWN')
         DO 20,I=2,M,2
*     Set a control flag
            P1=(I.LE.(2*L1))
*     Calculating the first part of the other coefficients
            A1=A((.FALSE.),P1,I,B1,BI,BM,D1)
            E1=S1*A((.TRUE.),P1,I,B1,BI,BM,D1)
*     Set a control flag
            P1=(I.LE.(2*L2))
*     Calculating the second part of the other coefficients
            A2=A((.FALSE.),P1,I,B2,BF,BM,D2)
            E2=S2*A((.TRUE.),P1,I,B2,BF,BM,D2)        
*     Calculate values
            AA=A1*A2
            IF (IP.EQ.1) THEN
               EA=E1*A2
            ELSE
               EA=A1*E2
            END IF
*     Write the result to file
            WRITE(11,'(X,SP,E10.3,X,E10.3)')AA,EA
*     Write the result on screen
            WRITE(6,'(A2,I3,X,A2,SP,E10.3,X,A2,E10.3)')'k=',I,'A=',AA,'E=',EA
*     Calculate coefficients for given theta
            DO 19,J=0,NA,1
*     Calculate Legendre polynomials
               IF (I.NE.2) THEN
                  R0P(J)=(REAL(2*I-3)*TC(J)*R0O(J)-REAL(I-2)*R0M(J))/REAL(I-1)
                  R0M(J)=R0O(J)
                  R0O(J)=R0P(J)
                  R0P(J)=(REAL(2*I-1)*TC(J)*R0O(J)-REAL(I-1)*R0M(J))/REAL(I)
                  R0M(J)=R0O(J)
                  R0O(J)=R0P(J)
                  R1P(J)=(REAL(2*I-3)*TC(J)*R1O(J)-REAL(I)*R1M(J))/REAL(I-3)
                  R1M(J)=R1O(J)
                  R1O(J)=R1P(J)
                  R1P(J)=(REAL(2*I-1)*TC(J)*R1O(J)-REAL(I+1)*R1M(J))/REAL(I-2)
                  R1M(J)=R1O(J)
                  R1O(J)=R1P(J)
               END IF
*     Add contributions of the order I
               AE(J)=AE(J)+AA*R0O(J)
               EE(J)=EE(J)+EA*R1O(J)
 19         CONTINUE
 20      CONTINUE
         CLOSE(11)
         WRITE(6,'(X)')
      END IF

*     Calculating explicit direction-polarization correlation
      OPEN(11,FILE='polar.dat',STATUS='UNKNOWN')
      WRITE(10,'(A25)')'vector/create polar(181) R'
      WRITE(10,'(A35)')'vector/read polar polar.dat (F10.7)'
      WRITE(10,'(A49)')'label1=[g]-[g]-Direction-Polarization Correlation'
      IF (NA.EQ.0) THEN
*     Write read instructions to PAW file
         WRITE(10,'(A75)')
     +'label2=[f]=Angle between the [g]-[g]-Plane and the Compton Scattering Plane'
         WRITE(10,'(A51)')'label3=Normalized Ideal Scattering Intensity N([f])'
         YM=EE(0)/AE(0)
         DO 21,J=0,180,1
            Y=1.0+EE(0)*COS(REAL(2*J)*CONV)/AE(0)
            WRITE(11,'(F10.7)')Y        
 21      CONTINUE
         WRITE(10,'(A33,SP,E11.4)')'label4=(N?0!-N?90!)/(N?0!+N?90!)=',YM
      ELSE
         WRITE(10,'(A41)')'label2=[q]=Angle between the two [g]-Rays'
         WRITE(10,'(A52)')'label3=Intensity Asymmetry=(N?0!-N?90!)/(N?0!+N?90!)'
         YM=0.0
         JM=0
         DO 22,J=0,180,1
            Y=EE(J)/AE(J)
            WRITE(11,'(F10.7)')Y
            IF (ABS(Y).GT.ABS(YM)) THEN
               YM=Y
               JM=J
            END IF
 22      CONTINUE
         WRITE(10,'(A38,SP,E11.4,SS,A8,I3.3,A7)')
     +'label4=(N?0!-N?90!)/(N?0!+N?90!)?MAX!=',YM,' at [q]=',JM,' degree'
      END IF
      CLOSE(11)
      WRITE(10,'(A38)')'histogram/create/title_global [label4]'
      WRITE(10,'(A45)')'histogram/create/1dhisto 1 [label1] 181 0 180'
      WRITE(10,'(A35)')'histogram/put_vect/contents 1 polar'
      WRITE(10,'(A18)')'histogram/plot 1 C'
      WRITE(10,'(A39)')'graphics/hplot/atitle [label2] [label3]'
      WRITE(10,'(A24)')'picture/print ''polar.ps'''
      WRITE(10,'(A6)')'return'
      CLOSE(10)
      WRITE(6,'(A49)')'Hint: If you do not like the screen output, type:'
      WRITE(6,'(A9)')'''paw'' and'
      WRITE(6,'(A18)')'''exec polar.kumac'''
      END
      
*     Calculating A for (ABS(mixing).LE.1).AND.(I.LT.M) 
      REAL FUNCTION A(P2,P3,J1,C1,C2,C3,DE)
      LOGICAL P2,P3
      Z1=1.0
      Z2=1.0
      Z3=1.0
      Z4=1.0
      IF (P2) THEN
         W0=REAL(J1)
         Z1=2.0*W0*(W0+1.0)*C1*(C1+1.0)/(W0*(W0+1.0)-2.0*C1*(C1+1.0))
         Z2=-2.0*(C1+1.0)
         Z3=-2.0*W0*(W0+1.0)*(C1+1.0)*(C1+2.0)/(W0*(W0+1.0)-2.0*(C1+1.0)*(C1+2.0))
         Z4=REAL(N(J1-2))/REAL(N(J1+2))
      END IF
      IF (P3) THEN
         IF (ABS(DE).LE.1.0) THEN
            X1=F(J1,C1,C1,C2,C3)*Z1
            X2=2.0*DE*F(J1,C1,C1+1.0,C2,C3)*Z2
            X3=DE*DE*F(J1,C1+1.0,C1+1.0,C2,C3)*Z3
            X4=(1.0+DE*DE)/Z4
         ELSE
            X1=F(J1,C1,C1,C2,C3)*Z1/DE/DE
            X2=2.0*F(J1,C1,C1+1.0,C2,C3)*Z2/DE
            X3=F(J1,C1+1.0,C1+1.0,C2,C3)*Z3
            X4=(1.0+1.0/DE/DE)/Z4
         END IF
         A=(X1+X2+X3)/X4
      ELSE
         IF (DE.EQ.0.0) THEN
            X3=0.0
            X4=1.0
         ELSE IF (ABS(DE).LE.1.0) THEN
            X3=DE*DE*F(J1,C1+1.0,C1+1.0,C2,C3)*Z3
            X4=(1.0+DE*DE)/Z4
         ELSE IF (ABS(DE).GT.1.0) THEN
            X3=F(J1,C1+1.0,C1+1.0,C2,C3)*Z3
            X4=(1.0+1.0/DE/DE)/Z4
         END IF
         A=X3/X4
      END IF
      RETURN
      END

*     Calculating the F-coefficients
      REAL FUNCTION F(J2,D1,D2,D3,D4)
      D0=REAL(J2)
      Y0=D3+D4-0.5
      IF (MOD(INT(Y0),2).EQ.1) THEN
         Y0=(-1.0)
      ELSE 
         Y0=1.0
      END IF
      Y1=SQRT((2.0*D1+1.0)*(2.0*D2+1.0)*(2.0*D4+1.0)*(2.0*D0+1.0))
      Y2=RWIG3J(D1,D2,D0,1.0,(-1.0),0.0)
      Y3=RWIG6J(D4,D4,D0,D1,D2,D3)
      F=Y0*Y1*Y2*Y3
      RETURN
      END

*     Calculating the faculty
      INTEGER FUNCTION N(K)
      N=1
      IF (K.NE.0) THEN
         DO 23,N1=1,K,1
            N=N*N1
 23      CONTINUE
      END IF
      RETURN
      END

