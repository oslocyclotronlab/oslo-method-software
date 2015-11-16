      PROGRAM ANGLERAND

*     This program calculates the angular distribution of gammas for a given Spin cascade
*     and a given (gaussian) population of the magnetic substates of J(initial):
*
*                    L+delta*(L+1)              
*     J(initial)----------------------->J(final)
*
*     The multipolarity L and can be adjusted to the users wishes, although in most 
*     cases the lowest possible multipolarity, also taken as default, might apply.
*     In addition, a mixing parameters delta is taken into account. This mixing 
*     parameter reflects the ammount of the next higher multipole transition, f. ex. 
*     M1+delta*E2. Since mixing parameters are phase sensitive, also a negative mixing 
*     parameter can occur, even a mixing parameters greater than 1 is possible. Thus  
*     mixing parameters range from -infinity to +infinity. It is also worth to notice,
*     that this program uses mixing parameters of the Biedenharn-Rose type.
 
*     In the case of an unknown mixing parameter, it is possible to calculate the angular 
*     distribution for a set of different mixing parameters. This set is calculated as
*     follows:      delta=+-exp(+-i/step), whereas i=0..(N-1),infinity. 'N' and 'step' 
*     can be customized within this program.

*     As output, this program calculates coefficients of Legendre polynomials, called 
*     A0, A2, A4, etc. The angular distribution then, can be written as:
*     W(theta)*dOmega=(A0+A2*P2(cos(theta))+A4*P4(cos(theta))+...)*dOmega, whereas
*     theta is the angle between the first and the second gamma respectively, and P2,
*     P4, etc. are Legendre polynomials.
*
*     The output is a table, showing the A coefficients, and several files 
*     which are customized to be used by PAW.

      IMPLICIT LOGICAL(P)
      IMPLICIT CHARACTER (Q)
      CHARACTER *46 FORM
      CHARACTER *11 INFINITY
      CHARACTER *16 CORNER
      CHARACTER *9 FILENAME
      CHARACTER *3 G1,G2
      PARAMETER (INFINITY='+-Infinity ',CORNER='   delta       A')
      DIMENSION AA(0:400),AD(0:399),AI(0:399),AF(0:100),S(0:100)

*     Printig header
      WRITE(6,'(A50)')'     ___________________________________________  '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |          A N G L E R A N D  1.0           | '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    | Program to calculate angular distribution | '
      WRITE(6,'(A50)')'    | of a given gamma ray cascade, and a given | '
      WRITE(6,'(A50)')'    |   (gaussian) population of the magnetic   | '
      WRITE(6,'(A50)')'    |          substates of J(initial).         | '
      WRITE(6,'(A50)')'    | Formulae based on T. Yamazaki, Tables of  | '
      WRITE(6,'(A50)')'    | coefficients for angular distribution of  | '
      WRITE(6,'(A50)')'    |      gamma rays from aligned nuclei.      | '
      WRITE(6,'(A50)')'    |    Nuclear Data, Section A, Volume 3,     | '
      WRITE(6,'(A50)')'    |           Number 1, August 1967           | '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |  Oslo Cyclotron Laboratory, January 1999  | '
      WRITE(6,'(A50)')'    |___________________________________________| '
      WRITE(6,'(X)')

*     Input some data
      Q='e'
      WRITE(6,1)Q
 1    FORMAT('Calculation for odd (o) or even (e) nucleus  <',A1,'>:',$)
      CALL READA(5,Q)
      LI=1
      WRITE(6,2)LI
 2    FORMAT('Initial Spin (max 50)  (*2 for odd nucleus) <',I2,'>:',$)  
      CALL READI(5,LI)
      IF (((Q.EQ.'e').AND.(LI.GE.50)).OR.((Q.EQ.'o').AND.(LI.GT.99))) STOP 'Spin out of range'
      LF=1
      WRITE(6,3)LF
 3    FORMAT('Final Spin             (*2 for odd nucleus) <',I2,'>:',$)
      CALL READI(5,LF)
      IF ((LI.EQ.0).AND.(LF.EQ.0)) STOP '0-->0 transition not allowed'
     
*     Now, calculating multipolarity of transition
      IF (Q.EQ.'o') THEN
         L=ABS(LI-LF)/2
         LP=(LI+LF)/2
      ELSE 
         L=ABS(LI-LF)
         LP=LI+LF
      END IF
*     E0 or M0 transitions are not taken into account
      IF (L.EQ.0) L=1

*     Checking if calculated multipolarity is according to users wishes
      IF (LP.GT.L) THEN
         WRITE(6,'(A48,I2)')'Calculated  multipolarity for gamma transition: ',L
         WRITE(6,'(A48,I2)')'Maximum  multipolarity  for  gamma  transition: ',LP
         QM='n'
         WRITE(6,4)QM
 4       FORMAT('Do you want to increase multipolarity <',A1,'>:',$)
         CALL READA(5,QM)
         IF (QM.EQ.'y') THEN
            LN=L+1
            WRITE(6,5)LN
 5          FORMAT('Type  new  multipolarity for gamma transition <',I2,'>:',$)
            CALL READI(5,LN)
            IF ((LN.LT.L).OR.(LN.GT.LP)) THEN 
               STOP 'Multipolarity out of range'
            ELSE 
               L=LN
            END IF
         END IF
      END IF
         
*     Input of mixing parameter if required
      D=0.0
      P=(.FALSE.)
      QD='n'
      IF (LP.GT.L) THEN
         WRITE(6,6)QD
 6       FORMAT('Mixing  for gamma transition <',A1,'>:',$)
         CALL READA(5,QD)
         IF (QD.EQ.'y') THEN
            WRITE(6,'(A44)')'If you type 0.0 , angular distribution for a'
            WRITE(6,'(A44)')'set of mixing parameters will be calculated.'
            WRITE(6,7)D
 7          FORMAT('Type  mixing parameter for gamma transition <',SP,F4.1,'>:',$)
            CALL READF(5,D)
            IF (D.EQ.0.0) P=(.TRUE.)
         ELSE 
            QD='n'
         END IF
      END IF
      
*     Specify the set of mixing parameters
      N=1
      AD(0)=0.0
      IF (P) THEN
         WRITE(6,'(A52)')'How many different mixing parameters should be taken'
         WRITE(6,'(A52)')'into account. Maximum=4*100.  Be careful, the number'
         WRITE(6,8)N
 8       FORMAT('you type here, will be taken times 4. <4*',I1,'>',$)
         CALL READI(5,N)
         IF ((N.LT.1).OR.(N.GT.100)) STOP 'Number out of range'
         WRITE(6,'(A52)')'Here, you can specify a stepwidth. The set of mixing'
         WRITE(6,'(A52)')'parameters is calculated,  according to the formula'
         IF (N.GE.10) THEN
            WRITE(6,'(A36,I2,A14)')'delta=+-exp(+-i/step), whereas i=0..',(N-1),',infinity. So,'
         ELSE
            WRITE(6,'(A36,I1,A15)')'delta=+-exp(+-i/step), whereas i=0..',(N-1),',infinity.  So,'
         END IF 
         B=1.0
         WRITE(6,9)B
 9       FORMAT('you can type here the stepwidth you want to apply. <',F3.1,'>',$)
         CALL READF(5,B)
         IF (B.LE.0.0) STOP 'Stepwidth out of range'

*     Calculating a format
         N1=2*N
         G1=CHAR(MOD(N1,10)+48)
         IF (N1.GE.10) G1=CHAR(MOD(INT(REAL(N1)/10.0),10)+48)//G1
         IF (N1.GE.100) G1=CHAR(MOD(INT(REAL(N1)/100.0),10)+48)//G1
         N1=2*N-1
         G2=CHAR(MOD(N1,10)+48)
         IF (N1.GE.10) G2=CHAR(MOD(INT(REAL(N1)/10.0),10)+48)//G2
         IF (N1.GE.100) G2=CHAR(MOD(INT(REAL(N1)/100.0),10)+48)//G2
         FORM='(A28,I3,X,SP,'//G1//'(E10.3,X),A11,SP,'//G2//'(E10.3,X))'

*     Calculating a set of mixing parameters (0+)-->(+-infinity)-->(0-)
         DO 100,J=-(N-1),(N-1),1
         AD(N+J)=EXP(REAL(J)/B)
         AD(3*N-J)=(-EXP(REAL(J)/B))
 100     CONTINUE
  
      END IF

*     Specify the gaussian population of the magnetic substates
      WRITE(6,'(A53)')'You should now specify the population of the magnetic'
      WRITE(6,'(A53)')'substates of J_(initial). The population distribution'
      WRITE(6,'(A53)')'is calculated, according to: P(m)=exp(-m^2/2*sigma^2)'
      WRITE(6,'(A53)')'and automatically normalized. You can now specify the' 
      WRITE(6,'(A53)')'parameter sigma, if you type 0.0 , complete alignment'
      WRITE(6,'(A16)')'will be assumed.'
      SIG=0.0
      WRITE(6,10)SIG
 10   FORMAT('Type population distribution parameter sigma <',F5.2,'>:',$)
      CALL READF(5,SIG)

*     Calculating the population distribution
      SUM=0.0
      IF (Q.EQ.'o') THEN
         IF (SIG.EQ.0.0) THEN
            S((LI-1)/2)=0.5
            S((LI+1)/2)=0.5
            SUM=1.0
         ELSE
            DO 110,J=0,(2*LI),2
            S(J/2)=EXP(-REAL((J-LI)*(J-LI))/(8.0*SIG*SIG))
            SUM=SUM+S(J/2)
 110        CONTINUE
         END IF 
      ELSE
         IF (SIG.EQ.0.0) THEN
            S(LI)=1.0
            SUM=1.0
         ELSE
            DO 120,J=(-LI),LI,1
            S(LI+J)=EXP(-REAL(J*J)/(2.0*SIG*SIG))
            SUM=SUM+S(LI+J)
 120        CONTINUE
         END IF
      END IF

*     Calculating maximal loop value
      IF (QD.EQ.'n') M=2*L
      IF (QD.EQ.'y') M=2*L+2
      IF (Q.EQ.'o') THEN
         M=MIN(LI,M)
      ELSE
         M=MIN(2*LI,M)
      END IF
      IF (MOD(M,2).EQ.1) M=M-1

*     Type conversion to real
      IF (Q.EQ.'o') THEN
         BI=REAL(LI)/2.0
         BF=REAL(LF)/2.0
      ELSE
         BI=REAL(LI)
         BF=REAL(LF)
      END IF
      B=REAL(L)

*     Opening file for use with PAW
      OPEN(10,FILE='angle.kumac',STATUS='UNKNOWN')
      WRITE(10,'(A11)')'macro angle'
      WRITE(10,'(A18)')'histogram/delete *'
      WRITE(10,'(A15)')'vector/delete *'
      WRITE(10,'(A14)')'graphics/set *'
      WRITE(10,'(A17)')'graphics/option *'
      WRITE(10,'(A19)')'graphics/option zfl'
      WRITE(10,'(A20)')'graphics/option nbox'
      WRITE(10,'(A18)')'graphics/misc/next'

*     The first coefficient is trivial
      WRITE(6,'(X)')
      WRITE(6,'(A2)')'A0'
      WRITE(6,'(SP,F10.7)')1.0
      WRITE(6,'(X)')

*     Calculating the other coefficients
      IF (M.GT.0) THEN
         DO 260,I=2,M,2
*     Opening data file for output
         IF (P) THEN
            II=MOD(I,10)
            FILENAME=CHAR(II+48)
            IF (I.GE.10) FILENAME=CHAR(MOD(INT(REAL(I)/10.0),10)+48)//FILENAME
            FILENAME='angle.a'//FILENAME
            OPEN(11,FILE=FILENAME,STATUS='UNKNOWN')
         END IF
*     Set two control flags
         IC=0
         PS=(I.LE.(2*L))
*     Check if mixing was enabled
         IF (QD.EQ.'n') THEN
            AI(0)=A(PS,I,B,BF,BI,0.0)
         ELSE 
*     Check if calculation for a set of mixing parameters was required
            IF (.NOT.P) THEN
               AI(0)=A(PS,I,B,BF,BI,D)
            ELSE
*     Calculation for mixing parameters equal 0 or infinity
               AI(0)=A(PS,I,B,BF,BI,0.0)
               AI(2*N)=A((.TRUE.),I,(B+1.0),BF,BI,0.0)
               IC=4*N-1
*     Calculating for other mixing parameters (0+)-->(+-infinity)-->(0-)
               DO 210,J=-(N-1),(N-1),1
               AI(N+J)=A(PS,I,B,BF,BI,AD(N+J))
               AI(3*N-J)=A(PS,I,B,BF,BI,AD(3*N-J))
 210           CONTINUE
            END IF
         END IF

*     Calculating the statistical tensor
         E0=REAL(I)
         SUM1=0.0
*     Calculating the sum over all magnetic substates
         DO 220,J=0,INT(2.0*BI+0.5),1
         E1=REAL(J)-BI
*     Calculating sign of each term
         Z0=BI-E1+0.5
         IF (MOD(INT(Z0),2).EQ.1) THEN
            Z0=(-1.0)
         ELSE 
            Z0=1.0
         END IF
*     Calculating each term
         Z1=RWIG3J(BI,BI,E0,E1,(-E1),0.0)
         Z2=S(J)
         SUM1=SUM1+Z0*Z1*Z2      
 220     CONTINUE
*     Calculating the factor
         Z3=SQRT((2.0*E0+1.0)*(2.0*E1+1.0))
*     Calculating the result for the statistical tensor
         R=Z3*SUM1/SUM

*     Write read instructions to PAW file
         IF (P) THEN
            WRITE(10,'(A15,I2.2,A1,I3.3,A3)')'vector/create a',I,'(',4*N,') R'
            WRITE(10,'(A13,I2.2,X,A9,A8)')'vector/read a',I,FILENAME,' (F10.7)'
         END IF
*     Write first line on screen
         IF (QD.EQ.'y') THEN
            WRITE(6,'(A16,I2.2)')CORNER,I
         ELSE 
            WRITE(6,'(A1,I2.2)')'A',I
         END IF
*     Calculate values
         DO 230,J=0,IC,1
         AI(J)=AI(J)*R
 230     CONTINUE
         AF(I)=AI(0)
*     Write the result to file     
         IF (P) THEN
            DO 240,J=0,IC,1
            WRITE(11,'(F10.7)')AI(J)
 240        CONTINUE
         END IF
*     Write the result on screen
         IF (P) THEN
            DO 250,J=0,IC,1
            IF (J.EQ.(2*N)) THEN 
               WRITE(6,'(A11,SP,F10.7,X)')INFINITY,AI(J)
            ELSE IF (D.NE.0.0) THEN
               WRITE(6,'(SP,E10.3,X,F10.7,X)')D,AI(J)
            ELSE 
               WRITE(6,'(SP,E10.3,X,F10.7,X)')AD(J),AI(J)
            END IF
 250        CONTINUE
         ELSE
            WRITE(6,'(SP,F10.7,X)')AI(0)
         END IF
         CLOSE(11)
         WRITE(6,'(X)')
 260     CONTINUE
      END IF

*     Calculating explicit angular distribution
      IF (.NOT.(P)) THEN
*     Write read instructions to PAW file
         WRITE(10,'(A28)')'vector/create angle(2,401) R'
         WRITE(10,'(A43)')'vector/read angle angle.out (F10.7,X,F10.7)'
         WRITE(10,'(A26)')'label=Angular Distribution'
         WRITE(10,'(A37)')'histogram/create/title_global [label]'
         WRITE(10,'(A29)')'graphics/hplot/null -3 3 -3 3'
         WRITE(10,'(A61)')'graphics/primitives/graph 401 angle(1,1:401) angle(2,1:401) C'
         WRITE(10,'(A42)')'graphics/hplot/atitle ''x-y-Plane'' ''z-Axis'''
         OPEN(11,FILE='angle.out',STATUS='UNKNOWN')
         DO 320,J1=0,400,1
*     Calculating direction (X,Y) of vector
         Y=COS(REAL(J1)*3.141592654/200.0)
         X=-SIN(REAL(J1)*3.141592654/200.0)
         AA(0)=1
         AA(1)=Y
         R=1
         IF (M.GT.0) THEN
            DO 310,J2=2,M,2
*     Calculating length (R) of vector, using Legendre polynomials
               AA(J2)=AA(J2-1)*Y*REAL(2*J2-1)/REAL(J2)-AA(J2-2)*REAL(J2-1)/REAL(J2)
               AA(J2+1)=AA(J2)*Y*REAL(2*J2+1)/REAL(J2+1)-AA(J2-1)*REAL(J2)/REAL(J2+1)
               R=R+AF(J2)*AA(J2)
 310        CONTINUE
         END IF
*     Write position to file
         Y=R*Y
         X=R*X
         WRITE(11,'(F10.7,X,F10.7)')X,Y        
 320     CONTINUE
         CLOSE(11)
*     Write graph instructions to PAW file
      ELSE
         IF (M.EQ.2) THEN
            WRITE(10,'(A27)')'label=A?22! Plot versus [d]'
            WRITE(10,'(A37)')'histogram/create/title_global [label]'
            WRITE(10,'(A30,I3,A3,I3)')'histogram/create/1dhisto 1 '''' ',4*N,' 1 ',4*N
            WRITE(10,'(A33)')'histogram/put_vect/contents 1 a02'
            WRITE(10,FORM)'graphics/primitives/label 1 ',4*N,(AD(II),II=0,(2*N-1)),INFINITY,(AD(II),II=(2*N+1),(4*N-1))
            WRITE(10,'(A18,I3,A3)')'graphics/set ndvx ',4*N,'.18'
            WRITE(10,'(A22)')'graphics/set vsiz 0.15'
            WRITE(10,'(A18)')'histogram/plot 1 C'
         ELSE IF (M.GT.2) THEN
            WRITE(10,'(A22)')'label=A?22!-A?44! Plot'
            WRITE(10,'(A37)')'histogram/create/title_global [label]'
            WRITE(10,'(A29)')'graphics/hplot/null -2 2 -2 2'
            WRITE(10,'(A26,I3,A10)')'graphics/primitives/graph ',4*N,' a02 a04 C'
            WRITE(10,'(A33)')'graphics/hplot/atitle A?22! A?44!'
         END IF
      END IF
      WRITE(10,'(A24)')'picture/print ''angle.ps'''
      WRITE(10,'(A6)')'return'
      CLOSE(10)
      WRITE(6,'(A49)')'Hint: If you do not like the screen output, type:'
      WRITE(6,'(A9)')'''paw'' and'
      WRITE(6,'(A18)')'''exec angle.kumac'''
      END
      
*     Calculating A for (ABS(mixing).LE.1).AND.(I.LT.M) 
      REAL FUNCTION A(P3,J1,C1,C2,C3,DE)
      IF (P3) THEN
         IF (ABS(DE).LE.1.0) THEN
            X1=F(J1,C1,C1,C2,C3)
            X2=2.0*DE*F(J1,C1,C1+1.0,C2,C3)
            X3=DE*DE*F(J1,C1+1.0,C1+1.0,C2,C3)
            X4=1.0+DE*DE
         ELSE
            X1=F(J1,C1,C1,C2,C3)/DE/DE
            X2=2.0*F(J1,C1,C1+1.0,C2,C3)/DE
            X3=F(J1,C1+1.0,C1+1.0,C2,C3)
            X4=1.0+1.0/DE/DE
         END IF
         A=(X1+X2+X3)/X4
      ELSE
         IF (ABS(DE).LE.1.0) THEN
            X1=DE*DE*F(J1,C1+1.0,C1+1.0,C2,C3)
            X4=1.0+DE*DE
         ELSE 
            X1=F(J1,C1+1.0,C1+1.0,C2,C3)
            X4=1.0+1.0/DE/DE
         END IF
         A=X1/X4
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
