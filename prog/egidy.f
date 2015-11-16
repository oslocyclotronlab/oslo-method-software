      PROGRAM egidy

      WRITE(6,*)'    __________________________________________'
      WRITE(6,*)'   |                                          |'
      WRITE(6,*)'   |                 EGIDY 1.1                |'
      WRITE(6,*)'   |                                          |'
      WRITE(6,*)'   |    Program to calculate level density.   |'
      WRITE(6,*)'   |   You may produce the level density of   |'
      WRITE(6,*)'   |     Egidy or of Gilbert and Cameron.     |'
      WRITE(6,*)'   |       Both approaches are based on       |'
      WRITE(6,*)'   |   Rho = exp[2sqrt(a(E-E1))]/(E-E1)**1.5  |'
      WRITE(6,*)'   |  + a lot of factors in front according   |'
      WRITE(6,*)'   |    to eq. (5) of Gilbert and Cameron     |'
      WRITE(6,*)'   |        Oslo Cyclotron Laboratory         |'
      WRITE(6,*)'   |             Magne Guttormsen             |'
      WRITE(6,*)'   |          Created: 04/11 - 1999           |'
      WRITE(6,*)'   |        Last change: 22/12 - 1999         |'
      WRITE(6,*)'   |__________________________________________|'
      
      WRITE(6,*)' '
      WRITE(6,*)'EGIDY:'
      WRITE(6,*)'Ref: T. von Egidy et al., Nucl.Phys. A481,189(1988).'
      WRITE(6,*)'You need mass number A and pairing gap Delta.'

      WRITE(6,*)' '
      WRITE(6,*)'GILBERT:'
      WRITE(6,*)'Ref: Gilbert and Cameron, Can.J.Phys. 43,1446 (1965).'
      WRITE(6,*)'You need P(N), P(Z), a, T, E0 and Ex form this paper.'
      WRITE(6,*)' '

      IANS = 1
      WRITE(6,34)
   34 FORMAT(/'Use Egidy et al. formula        (1)',/,
     1        'Use Gilbert and Cameron formula (2)',/,
     2        'EXIT                            (3)')
      WRITE(6,32)IANS
  32  FORMAT(/'Please, give your answer <',I1,'>:',$)
      CALL READI(5,IANS)
      IF(IANS.LT.1.OR.IANS.GT.2)STOP
      IF(IANS.EQ.1)CALL EGIDY1
      IF(IANS.EQ.2)CALL GILBERT
      END


      SUBROUTINE EGIDY1
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
       
      REAL*8 x, a, U, s, f 
      REAL rho(0:8191),Calib(6)

      a0 = 0.
      a1 = 120.
      Emax = 100000.
      M  = 162

      WRITE(6,2)a0
 2    FORMAT('Give excitation calibration constant a0(keV)    <',F7.2,'>:',$)
      CALL READF(5,a0)

      WRITE(6,4)a1
 4    FORMAT('Give excitation calibration constant a1(keV/ch) <',F7.2,'>:',$)
      CALL READF(5,a1)

      WRITE(6,8)Emax
 8    FORMAT('Give higher energy limit Emax for Rho (keV)       <',F7.0,'>:',$)
      CALL READF(5,Emax)

      WRITE(6,10)M
 10   FORMAT('Give mass number A for nucleus                      <',I3,'>:',$)
      CALL READI(5,M)
      aM = M

      D = 1000.*12./sqrt(aM)
      IF((M/2).EQ.(aM/2.))D=2.*D
      WRITE(6,12)D
 12   FORMAT('Give pairing gap (0, Delta, or 2*Delta) (keV)   <',F7.1,'>:',$)
      CALL READF(5,D)

C Calculating parameters
c      a  = 0.110*(aM**(0.975))/1000.
c      C1 = (-5.6*aM**(-0.28))*1000.
      a  = 0.210*(aM**(0.87))/1000.
      C1 = (-6.6*aM**(-0.32))*1000.
      E1 = C1 + D

C Calculating rho
      MAXCH = INT(((Emax-a0)/a1)+0.5)
      IF(MAXCH.GT.8191)MAXCH = 8191   
      WRITE(6,*)
      WRITE(6,*)' ch      Ex         U        Rho        sigma'
      DO i = 0,MAXCH
         rho(i) = 0.
         U = (a0 + a1*i)-E1
         IF(U.GT.(2.*a1).AND.U.LE.Emax)THEN
            x =DEXP(2.*DSQRT(a*U))
            s =DSQRT(0.0888*(aM**(2./3.))*DSQRT(a*U))
            f =1/(12.*1.414213562*(a**0.25)*(U**(5./4.))*s)
            IF(x*f*1000.LT.3.1E+38)THEN
               rho(i) = f*x*1000.
               WRITE(6,24) i, a0+a1*i, U, rho(i),s
 24            FORMAT(I4,F10.0,F10.0,E12.3,F10.2)
            ENDIF
         ENDIF
      ENDDO
 

      WRITE(6,20)M,D,a,C1,E1
 20   FORMAT('Mass number             A  = ',I3,/,    
     1       'Pairing gap             D  = ',F6.1,' keV',/,
     2       'Level density parameter a  = 0.210A**0.870/1000  = ',F8.5,'(1/keV)',/,  
     3       'Correction to gap       C1 = -6.6A**(-0.32)*1000 = ',F7.1,' keV',/,
     4       'Back-shift parameter    E1 = C1 + D = ',F7.1,'keV')


C Writing spectrum out for mama
      ITYPE=1
      Calib(1)=a0
      Calib(2)=a1
      Calib(3)=0.0
      outfile='egidy.egi'
      comment='Emperical level density Rho(1/MeV) from Egidy'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL')
      CALL norw1dim(20,comment,MAXCH,rho,Calib)
      CLOSE(20)
      WRITE(6,30)outfile
 30   FORMAT('Egidy level density Rho(1/MeV) written to file: ',A13)
 
      END


      SUBROUTINE GILBERT
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
       
      REAL*8 x, a, U, s, f ,E, E0, T
      REAL rho(0:8191),Calib(6)

      a0 = 0.
      a1 = 120.
      E1 = 0.
      Emax = 100000.
      M  = 162

      WRITE(6,2)a0
 2    FORMAT('Give excitation calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)

      WRITE(6,4)a1
 4    FORMAT('Give excitation calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)

      WRITE(6,8)Emax
 8    FORMAT('Give higher energy limit Emax for Rho (keV)        <',F7.0,'>:',$)
      CALL READF(5,Emax)

      WRITE(6,9)M
 9    FORMAT('Give mass number A for nucleus                         <',I3,'>:',$)
      CALL READI(5,M)
      aM = M

      D  = 1000.*12./sqrt(aM)
      PN = 0.
      PZ = D
      IF((M/2).EQ.(aM/2.))PN = D


      WRITE(6,10)PN
 10   FORMAT('Give neutron pairing energy P(N) (keV)             <',F7.1,'>:',$)
      CALL READF(5,PN)

      WRITE(6,11)PZ
 11   FORMAT('Give proton pairing energy P(Z)  (keV)             <',F7.1,'>:',$)
      CALL READF(5,PZ)
    
      D = PZ + PN
      aa = (aM/8.)/1000.
      TT  = 525.
      EE0 = 430.
      Ex = 2500. + (150./aM)*1000. + D

      WRITE(6,12)aa
 12   FORMAT('Give level density parameter a (1/keV)             <',F7.3,'>:',$)
      CALL READF(5,aa)
      a = aa

      WRITE(6,14)TT
 14   FORMAT('Give constant nuclear temperature T (keV)          <',F7.1,'>:',$)
      CALL READF(5,TT)
      T = TT

      WRITE(6,16)EE0
 16   FORMAT('Give energy-shift for const. temp. region E0 (keV) <',F7.1,'>:',$)
      CALL READF(5,EE0)
      E0 = EE0

      WRITE(6,18)Ex
 18   FORMAT('Give energy of tangency point Ex (keV)             <',F7.1,'>:',$)
      CALL READF(5,Ex)

C Calculating rho
      MAXCH = INT(((Emax-a0)/a1)+0.5)
      IF(MAXCH.GT.8191)MAXCH = 8191   
      WRITE(6,*)
      WRITE(6,*)' ch      Ex         U        Rho        sigma'
      DO i = 0,MAXCH
         rho(i) = 0.
         E = (a0 + a1*i)
         IF(E.LE.Ex)THEN
            rho(i) = 1000.*(1./T)*DEXP((E-E0)/T)
            WRITE(6,22) i, a0+a1*i, rho(i)
 22         FORMAT(I4,F10.0,E22.3)
         ELSE
            U = E - D
            IF(U.GE.(2.*a1).AND.U.LE.Emax)THEN
               x =DEXP(2.*DSQRT(a*U))
               s =DSQRT(0.0888*(aM**(2./3.))*DSQRT(a*U))
               f =1/(12.*1.414213562*(a**0.25)*(U**(5./4.))*s)
               IF(x*f*1000.LT.3.1E+38)THEN
                  rho(i) = f*x*1000.
                  WRITE(6,24) i, a0+a1*i, U, rho(i),s
 24               FORMAT(I4,F10.0,F10.0,E12.3,F10.2)
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      ss =DSQRT(0.0888*(aM**(2./3.))*DSQRT(a*(Ex-D)))
      WRITE(6,20)M,D,a,T,E0,ss,Ex
 20   FORMAT('Mass number             A  = ',I3,/,    
     1       'Total pairing gap       D  = ',F6.1,' keV',/,
     2       'Level density parameter a  = ',F8.5,' (1/keV)',/,  
     3       'Nuclear temperature     T  = ',F7.1,' keV',/,
     4       'Energy shift            E0 = ',F7.1,' keV',/,
     5       'Sigma at Ex             s  = ',F7.1,' hbar',/,
     6       'Energy tangency point   Ex = ',F7.1,' keV')

C Writing spectrum out for mama
      ITYPE=1
      Calib(1)=a0
      Calib(2)=a1
      Calib(3)=0.0
      outfile='gilbert.egi'
      comment='Emperical level density Rho(1/MeV) from Gilbert and Cameron'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL')
      CALL norw1dim(20,comment,MAXCH,rho,Calib)
      CLOSE(20)
      WRITE(6,30)outfile
 30   FORMAT('Gilbert and Cameron level density Rho(1/MeV) written to file: ',A13)
 
      END
