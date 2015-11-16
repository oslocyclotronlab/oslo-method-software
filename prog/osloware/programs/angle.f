      PROGRAM ANGLE

*     This program calculates the angular distribution of gammas for a given Spin cascade:
*
*                   L1+delta1*(L1+1)                       L2+delta2*(L2+1)
*     J(initial)---------------------->J(intermediate)----------------------->J(final)
*
*     The multipolarities L1 and L2 can be adjusted to the users wishes, although in
*     most cases the lowest possible multipolarities, also taken as defaults, might apply.
*     In addition, mixing parameters delta1 and delta2 are taken into account. These 
*     mixing parameters reflect the ammount of the next higher multipole transition,
*     f. ex. M1+delta*E2. Since mixing parameters are phase sensitive, also negative
*     mixing parameters can occur, even mixing parameters greater than 1 are possible.
*     So mixing parameters range from -infinity to +infinity. It is also worth to notice,
*     that this program uses mixing parameters of the Biedenharn-Rose type.
 
*     In the case of unknown mixing parameters, it is possible to calculate the angular 
*     distribution for a set of different mixing parameters. This set is calculated as
*     follows:      delta=+-exp(+-i/step), whereas i=0..(N-1),infinity.
*     'N' and 'step' can be customized within this program.

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
      CHARACTER *41 FORM1
      CHARACTER *25 FORM2
      CHARACTER *21 FORM3
      CHARACTER *46 FORM4
      CHARACTER *11 INFINITY,CORNER
      CHARACTER *9 FILENAME
      CHARACTER *3 G1,G2,G3
      PARAMETER (INFINITY='+-Infinity ',CORNER=' d-i / d-f ')
      DIMENSION AA(0:399),AD(0:399),AI(0:399),AF(0:399)

*     Printig header
      WRITE(6,'(A50)')'     ___________________________________________  '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |              A N G L E   1.1              | '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    | Program to calculate angular distribution | '
      WRITE(6,'(A50)')'    |       of a given gamma ray cascade.       | '
      WRITE(6,'(A50)')'    |   Formulae based on Landolt-Boernstein,   | '
      WRITE(6,'(A50)')'    | Numerical Tables  for Angular Correlation | '
      WRITE(6,'(A50)')'    | Computations in alpha-, beta-  and gamma- | '
      WRITE(6,'(A50)')'    | Spectroscopy. Springer, Berlin, 1968      | '
      WRITE(6,'(A50)')'    |                                           | '
      WRITE(6,'(A50)')'    |  Oslo Cyclotron Laboratory, October 1996  | '
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
      LM=1
      WRITE(6,3)LM
 3    FORMAT('Intermediate Spin      (*2 for odd nucleus) <',I2,'>:',$)
      CALL READI(5,LM)
      LF=1
      WRITE(6,4)LF
 4    FORMAT('Final Spin             (*2 for odd nucleus) <',I2,'>:',$)
      CALL READI(5,LF)
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
         WRITE(6,5)Q1
 5       FORMAT('Do you want to increase multipolarity <',A1,'>:',$)
         CALL READA(5,Q1)
         IF (Q1.EQ.'y') THEN
            L1N=L1+1
            WRITE(6,6)L1N
 6          FORMAT('Type  new  multipolarity for first transition <',I2,'>:',$)
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
         WRITE(6,7)Q2
 7       FORMAT('Do you want to increase multipolarity <',A1,'>:',$)
         CALL READA(5,Q2)
         IF (Q2.EQ.'y') THEN
            L2N=L2+1
            WRITE(6,8)L2N
 8          FORMAT('Type  new multipolarity for second transition <',I2,'>:',$)
            CALL READI(5,L2N)
            IF ((L2N.LT.L2).OR.(L2N.GT.L2P)) THEN 
               STOP 'Multipolarity out of range'
            ELSE 
               L2=L2N
            END IF
         END IF
      END IF
         
*     Input of mixing parameters if required
      D1=0.0
      P1=(.FALSE.)
      Q1='n'
      IF (L1P.GT.L1) THEN
         WRITE(6,9)Q1
 9       FORMAT('Mixing  for first transition <',A1,'>:',$)
         CALL READA(5,Q1)
         IF (Q1.EQ.'y') THEN
            WRITE(6,'(A44)')'If you type 0.0 , angular distribution for a'
            WRITE(6,'(A44)')'set of mixing parameters will be calculated.'
            D1=0.0
            WRITE(6,10)D1
 10         FORMAT('Type  mixing parameter for first transition <',SP,F4.1,'>:',$)
            CALL READF(5,D1)
            IF (D1.EQ.0.0) P1=(.TRUE.)
         ELSE 
            Q1='n'
         END IF
      END IF
      D2=0.0
      P2=(.FALSE.)
      Q2='n'
      IF (L2P.GT.L2) THEN
         WRITE(6,11)Q2
 11      FORMAT('Mixing for second transition <',A1,'>:',$)
         CALL READA(5,Q2)
         IF (Q2.EQ.'y') THEN
            WRITE(6,'(A44)')'If you type 0.0 , angular distribution for a'
            WRITE(6,'(A44)')'set of mixing parameters will be calculated.'
            D2=0.0
            WRITE(6,12)D2
 12         FORMAT('Type mixing parameter for second transition <',SP,F4.1,'>',$)
            CALL READF(5,D2)
            IF (D2.EQ.0.0) P2=(.TRUE.)
         ELSE 
            Q2='n'
         END IF
      END IF
      
*     Specify the set of mixing parameters
      N=1
      AD(0)=0.0
      IF (P1.OR.P2) THEN
         WRITE(6,'(A52)')'How many different mixing parameters should be taken'
         WRITE(6,'(A52)')'into account. Maximum=4*100.  Be careful, the number'
         WRITE(6,13)N
 13      FORMAT('you type here, will be taken times 4. <4*',I1,'>',$)
         CALL READI(5,N)
         IF ((N.LT.1).OR.(N.GT.100)) STOP 'Number out of range'
         WRITE(6,'(A52)')'Here, you can specify a stepwidth. The set of mixing'
         WRITE(6,'(A52)')'parameters is calculated,  according to the formular'
         IF (N.GE.10) THEN
            WRITE(6,'(A36,I2,A14)')'delta=+-exp(+-i/step), whereas i=0..',(N-1),',infinity. So,'
         ELSE
            WRITE(6,'(A36,I1,A15)')'delta=+-exp(+-i/step), whereas i=0..',(N-1),',infinity.  So,'
         END IF 
         B=1.0
         WRITE(6,14)B
 14      FORMAT('you can type here the stepwidth you want to apply. <',F3.1,'>',$)
         CALL READF(5,B)
         IF (B.LE.0.0) STOP 'Stepwidth out of range'

*     Calculating some formats
         N1=4*N
         G1=CHAR(MOD(N1,10)+48)
         IF (N1.GE.10) G1=CHAR(MOD(INT(REAL(N1)/10.0),10)+48)//G1
         IF (N1.GE.100) G1=CHAR(MOD(INT(REAL(N1)/100.0),10)+48)//G1
         N1=2*N
         G2=CHAR(MOD(N1,10)+48)
         IF (N1.GE.10) G2=CHAR(MOD(INT(REAL(N1)/10.0),10)+48)//G2
         IF (N1.GE.100) G2=CHAR(MOD(INT(REAL(N1)/100.0),10)+48)//G2
         N1=2*N-1
         G3=CHAR(MOD(N1,10)+48)
         IF (N1.GE.10) G3=CHAR(MOD(INT(REAL(N1)/10.0),10)+48)//G3
         IF (N1.GE.100) G3=CHAR(MOD(INT(REAL(N1)/100.0),10)+48)//G3
         FORM1='(A11,SP,'//G2//'(E10.3,X),A11,SP,'//G3//'(E10.3,X))'    
         FORM2='(SP,E10.3,X,'//G1//'(F10.7,X))' 
         FORM3='(A11,SP,'//G1//'(F10.7,X))'
         FORM4='(A28,I3,X,SP,'//G2//'(E10.3,X),A11,SP,'//G3//'(E10.3,X))'
 101     FORMAT(A11,SP,E10.3,X)
 102     FORMAT(A11,SP,F10.7,X)
 103     FORMAT(SP,E10.3,X,F10.7,X)

*     Calculating a set of mixing parameters (0+)-->(+-infinity)-->(0-)
         DO 110,J=-(N-1),(N-1),1
         AD(N+J)=EXP(REAL(J)/B)
         AD(3*N-J)=(-EXP(REAL(J)/B))
 110     CONTINUE
      END IF

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
      WRITE(6,'(SP,E10.3)')1.0
      WRITE(6,'(X)')

*     Calculating the other coefficients
      IF (M.GT.0) THEN
         DO 260,I=2,M,2
*     Opening data file for output
         IF (P1.OR.P2) THEN
            II=MOD(I,10)
            FILENAME=CHAR(II+48)
            IF (I.GE.10) FILENAME=CHAR(MOD(INT(REAL(I)/10.0),10)+48)//FILENAME
            FILENAME='angle.a'//FILENAME
            OPEN(11,FILE=FILENAME,STATUS='UNKNOWN')
         END IF
*     Set two control flags
         I1=0
         P=(I.LE.(2*L1))
*     Check if mixing was enabled
         IF (Q1.EQ.'n') THEN
            AI(0)=A(P,I,B1,BI,BM,0.0)
         ELSE 
*     Check if calculation for a set of mixing parameters was required
            IF (.NOT.P1) THEN
               AI(0)=A(P,I,B1,BI,BM,D1)
            ELSE
*     Calculation for mixing parameters equal 0 or infinity
               AI(0)=A(P,I,B1,BI,BM,0.0)
               AI(2*N)=A((.TRUE.),I,(B1+1.0),BI,BM,0.0)
               I1=4*N-1
*     Calculating for other mixing parameters (0+)-->(+-infinity)-->(0-)
               DO 210,J=-(N-1),(N-1),1
               AI(N+J)=A(P,I,B1,BI,BM,AD(N+J))
               AI(3*N-J)=A(P,I,B1,BI,BM,AD(3*N-J))
 210           CONTINUE
            END IF
         END IF
*     Set two control flags
         I2=0
         P=(I.LE.(2*L2))
*     Check if mixing was enabled
         IF (Q2.EQ.'n') THEN
            AF(0)=A(P,I,B2,BF,BM,0.0)
         ELSE 
*     Check if calculation for a set of mixing parameters was required
            IF (.NOT.P2) THEN
               AF(0)=A(P,I,B2,BF,BM,D2)
            ELSE
*     Calculation for mixing parameters equal 0 or infinity
               AF(0)=A(P,I,B2,BF,BM,0.0)
               AF(2*N)=A((.TRUE.),I,(B2+1.0),BF,BM,0.0)
               I2=4*N-1
*     Calculating a for other mixing parameters (0+)-->(+-infinity)-->(0-) 
               DO 220,J=-(N-1),(N-1),1
               AF(N+J)=A(P,I,B2,BF,BM,AD(N+J))
               AF(3*N-J)=A(P,I,B2,BF,BM,AD(3*N-J))
 220           CONTINUE
            END IF
         END IF

*     Write read instructions to PAW file
         IF (P1.AND.P2) THEN
            WRITE(10,'(A15,I2.2,A1,I3.3,A1,I3.3,A3)')'vector/create a',I,'(',4*N,',',4*N,') R'
            WRITE(10,'(A13,I2.2,X,A10,A8)')'vector/read a',I,FILENAME,' (F10.7)'
         ELSE IF (P1.OR.P2) THEN
            WRITE(10,'(A15,I2.2,A1,I3.3,A3)')'vector/create a',I,'(',4*N,') R'
            WRITE(10,'(A13,I2.2,X,A10,A8)')'vector/read a',I,FILENAME,' (F10.7)'
         END IF
*     Write first line on screen
         WRITE(6,'(A1,I2.2)')'A',I
         IF (P2) THEN
            WRITE(6,FORM1)CORNER,(AD(II),II=0,(2*N-1)),INFINITY,(AD(II),II=(2*N+1),(4*N-1))
         ELSE IF (Q2.EQ.'y') THEN
            WRITE(6,101)CORNER,D2
         ELSE 
            WRITE(6,101),CORNER,0.0
         END IF
*     Calculate values
         DO 250,J1=0,I1,1
         DO 230,J2=0,I2,1
         AA(J2)=AI(J1)*AF(J2)
 230     CONTINUE
*     Write the result to file     
         IF (P2) THEN
            DO 240,J2=0,I2,1
            WRITE(11,'(F10.7)')AA(J2)
 240        CONTINUE
         ELSE IF (P1) THEN
            WRITE(11,'(F10.7)')AA(0)
         ELSE
            AD(I)=AA(0)
         END IF
*     Write the result on screen
         IF (P2) THEN
            IF (J1.EQ.(2*N)) THEN 
               WRITE(6,FORM3)INFINITY,(AA(II),II=0,(4*N-1))
            ELSE IF (D1.NE.0.0) THEN
               WRITE(6,FORM2)D1,(AA(II),II=0,(4*N-1))
            ELSE 
               WRITE(6,FORM2)AD(J1),(AA(II),II=0,(4*N-1))
            END IF
         ELSE 
            IF (J1.EQ.(2*N)) THEN
               WRITE(6,102)INFINITY,AA(0)
            ELSE IF (D1.NE.0.0) THEN
               WRITE(6,103)D1,AA(0)
            ELSE
               WRITE(6,103)AD(J1),AA(0)
            END IF
         END IF
 250     CONTINUE
         CLOSE(11)
         WRITE(6,'(X)')
 260     CONTINUE
      END IF

*     Calculating explicit angular distribution
      IF (.NOT.(P1.OR.P2)) THEN
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
               R=R+AD(J2)*AA(J2)
 310        CONTINUE
         END IF
*     Write position to file
         Y=R*Y
         X=R*X
         WRITE(11,'(F10.7,X,F10.7)')X,Y        
 320     CONTINUE
         CLOSE(11)
*     Write graph instructions to PAW file
      ELSE IF (P1.AND.P2) THEN
         IF (M.EQ.2) THEN
            WRITE(10,'(A41)')'label=A?22! Plot versus [d]?1! and [d]?2!'
            WRITE(10,'(A37)')'histogram/create/title_global [label]'
            WRITE(10,'(A30,I3,A3,I3,X,I3,A3,I3)')'histogram/create/2dhisto 1 '''' ',4*N,' 1 ',4*N,4*N,' 1 ',4*N
            WRITE(10,'(A33)')'histogram/put_vect/contents 1 a02'
            WRITE(10,FORM4)'graphics/primitives/label 1 ',4*N,(AD(II),II=0,(2*N-1)),INFINITY,(AD(II),II=(2*N+1),(4*N-1))
            WRITE(10,'(A18,I3,A3)')'graphics/set ndvx ',4*N,'.18'
            WRITE(10,'(A18,I3,A3)')'graphics/set ndvy ',4*N,'.16'
            WRITE(10,'(A22)')'graphics/set vsiz 0.15'
            WRITE(10,'(A32)')'histogram/2d_plot/contour 1 10 1'
         ELSE IF (M.GT.2) THEN
            WRITE(10,'(A25)')'graphics/viewing/zone 1 2'
            WRITE(10,'(A52)')'label=A?22! and A?44! Plots versus [d]?1! and [d]?2!'
            WRITE(10,'(A37)')'histogram/create/title_global [label]'
            WRITE(10,FORM4)'graphics/primitives/label 1 ',4*N,(AD(II),II=0,(2*N-1)),INFINITY,(AD(II),II=(2*N+1),(4*N-1))
            WRITE(10,'(A18,I3,A3)')'graphics/set ndvx ',4*N,'.18'
            WRITE(10,'(A18,I3,A3)')'graphics/set ndvy ',4*N,'.16'
            WRITE(10,'(A22)')'graphics/set vsiz 0.15'
            WRITE(10,'(A30,I3,A3,I3,X,I3,A3,I3)')'histogram/create/2dhisto 1 '''' ',4*N,' 1 ',4*N,4*N,' 1 ',4*N
            WRITE(10,'(A33)')'histogram/put_vect/contents 1 a02'
            WRITE(10,'(A30,I3,A3,I3,X,I3,A3,I3)')'histogram/create/2dhisto 2 '''' ',4*N,' 1 ',4*N,4*N,' 1 ',4*N
            WRITE(10,'(A33)')'histogram/put_vect/contents 2 a04'
            WRITE(10,'(A32)')'histogram/2d_plot/contour 1 10 1'
            WRITE(10,'(A32)')'histogram/2d_plot/contour 2 10 1'
         END IF
      ELSE
         IF (M.EQ.2) THEN
            WRITE(10,'(A27)')'label=A?22! Plot versus [d]'
            WRITE(10,'(A37)')'histogram/create/title_global [label]'
            WRITE(10,'(A30,I3,A3,I3)')'histogram/create/1dhisto 1 '''' ',4*N,' 1 ',4*N
            WRITE(10,'(A33)')'histogram/put_vect/contents 1 a02'
            WRITE(10,FORM4)'graphics/primitives/label 1 ',4*N,(AD(II),II=0,(2*N-1)),INFINITY,(AD(II),II=(2*N+1),(4*N-1))
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
         IF (DE.EQ.0.0) THEN
            X1=0.0
            X4=1.0
         ELSE IF (ABS(DE).LE.1.0) THEN
            X1=DE*DE*F(J1,C1+1.0,C1+1.0,C2,C3)
            X4=1.0+DE*DE
         ELSE IF (ABS(DE).GT.1.0) THEN
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

