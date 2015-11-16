      PROGRAM toro 
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
       
      REAL*8 xx, eeta, aa, EE0, TT0, EE
      REAL rho(0:8191),Calib(6)

      WRITE(6,*)'    ______________________________________'
      WRITE(6,*)'   |                                      |'
      WRITE(6,*)'   |               TORO 1.0               |'
      WRITE(6,*)'   | (Constant Temperature -> Fermi gas)  |'
      WRITE(6,*)'   |                                      |'
      WRITE(6,*)'   |  Program to calculate level density  |'
      WRITE(6,*)'   |  Rho of two components:              |'
      WRITE(6,*)'   |      exp(E/T0) and exp[2sqrt(aE)]    |'
      WRITE(6,*)'   |  The functions are fitted at E=E0,   |'
      WRITE(6,*)'   |          and a0 = E0/T0**2           |'
      WRITE(6,*)'   |      Oslo Cyclotron Laboratory       |'
      WRITE(6,*)'   |           Magne Guttormsen           |'
      WRITE(6,*)'   |        Created: 09/08 - 1999         |'
      WRITE(6,*)'   |    Last change: 09/08 - 1999         |'
      WRITE(6,*)'   |    TORO betasuppe is a trademark     |'
      WRITE(6,*)'   |______________________________________|'

      a0 = 0.
      a1 = 100.
      E1 = 0.
      E2 = 100000.
      T0 = 530.
      E0 = 7000.

      WRITE(6,2)a0
 2    FORMAT('Give calibration constant a0(keV)                  <',F7.2,'>:',$)
      CALL READF(5,a0)

      WRITE(6,3)a1
 3    FORMAT('Give calibration constant a1(keV/ch)               <',F7.2,'>:',$)
      CALL READF(5,a1)

      WRITE(6,4)E0
 4    FORMAT('Give excitation energy E0 for change',/,
     +       'from lower to upper Rho (keV)                      <',F7.2,'>:',$)
      CALL READF(5,E0)

      WRITE(6,6)T0
 6    FORMAT('Give constant temperature T0 for lower Rho (keV)   <',F7.2,'>:',$)
      CALL READF(5,T0)
     
      a = E0/(T0*T0)

      WRITE(6,8)a
 8    FORMAT('The default level density parameter is calculated',/,
     1       'to make the derivatives of Rho continoues at E0',/,
     2       'Give level density parameter for upper Rho (keV)   <',F7.4,'>:',$)
      CALL READF(5,a)

      WRITE(6,10)E1
 10   FORMAT('Give lower energy limit E1 for total Rho (keV)     <',F7.2,'>:',$)
      CALL READF(5,E1)
      WRITE(6,12)E2
 12   FORMAT('Give higher energy limit E2 for total Rho (keV)    <',F7.0,'>:',$)
      CALL READF(5,E2)

C Introdusing double precsision
      aa = a
      EE0=E0
      TT0=T0

C Calculating factor to math the two functions at E0
      eeta = DEXP((EE0/TT0) - 2.*DSQRT(aa*EE0))

      MAXCH = INT(((E2-a0)/a1)+0.5)
      IF(MAXCH.GT.8191)MAXCH = 8191   

      DO i = 0,MAXCH
         rho(i) = 0.
         x      = 0.
         EE = a0 + a1*i
         IF(E.GE.E1.AND.E.LE.E2)THEN
            IF(E.LE.E0)THEN
               xx=EXP(EE/TT0)
            ENDIF
            IF(EE.GT.E0)THEN
               xx=eeta*DEXP(2.*DSQRT(aa*EE))
            ENDIF
            IF(xx.LT.3.1E+38)THEN
               rho(i) = xx
            ENDIF
         ENDIF
      ENDDO

C Writing spectrum out for mama
      ITYPE=1
      Calib(1)=a0
      Calib(2)=a1
      Calib(3)=0.0
       outfile='rho.tro'
      comment='Semiexperimental level density'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL')
      CALL norw1dim(20,comment,MAXCH,rho,Calib)
      CLOSE(20)

      WRITE(6,44)outfile
 44   FORMAT('Two component level density written to file: ',A13)
 
      END
