      PROGRAM normrhof
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      CHARACTER fname*8,comm*60
      CHARACTER APP*4,outfile*20,comment*60
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)

      WRITE(6,*)'     _____________________________________________'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |       Normalizing rho and F (normrhof)      |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Program to determine A and alpha from known |'
      WRITE(6,*)'    |   level density based on counting at 2      |'
      WRITE(6,*)'    |    anchor points (E1,r1) and (E1,r2). No    |'
      WRITE(6,*)'    |      extrapolations are performed here.     |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |          Oslo Cyclotron Laboratory          |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Created:  12/06 - 2001                      |'
      WRITE(6,*)'    | Modified: 12/06 - 2001                      |'
      WRITE(6,*)'    | Magne Guttormsen                            |'
      WRITE(6,*)'    |_____________________________________________|'
      WRITE(6,*)' '

C History:
C 12 June  2001:
  
      OPEN(23,FILE='normrhofin.dat',STATUS='old',ERR=666)
      READ(23,*,ERR=666)Ex1,Ex2,rhonew1,rhonew2,B
      CLOSE(23)
      WRITE(6,*)'Default values taken from file: normrhofin.dat'
      GO TO 777
 666  CONTINUE
C Initializing parameter values
      Ex1  = 1000. !Lower excitation energy E1
      Ex2  = 9000. !Upper excitation energy E2
      rho1 =  430. !Lower target value rho1
      rho2 =  400. !Upper target value rho2
      B    =    1. !Factor to multiply new F
 777  CONTINUE

C Reading rhosp.rsg from rhosigchi 
      IDEST=1
      ITYPE=3
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 1 and the name of your rho from the'
      WRITE(6,*)'rhosigchi program: rhosp.rsg in the two next questions... '
      WRITE(6,*)' '
      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
      IF(bx+by.EQ.2.)THEN
        a0Ex=0.
        a1Ex=120.
      ELSE
        a0Ex=cal(1,IDEST,1,1)
        a1Ex=cal(1,IDEST,1,2)
      ENDIF
             
C Giving energy calibrations for rhosp.rsg
 21   CONTINUE
      WRITE(6,26) a0Ex
 26   FORMAT(/'Cal. coeff. a0Ex (keV) on x-axis         <',F7.1,'>:',$)
      CALL READF(5, a0Ex)
      WRITE(6,27) a1Ex
 27   FORMAT( 'Cal. coeff. a1Ex (keV/ch) on x-axis      <',F7.2,'>:',$)
      CALL READF(5, a1Ex)

      WRITE(6,*)' '
      WRITE(6,28) Ex1
 28   FORMAT( 'Lower target excitation energy Ex1 (keV) <',F7.1,'>:',$)
      CALL READF(5,Ex1)
      WRITE(6,29) Ex2
 29   FORMAT( 'Upper target excitation energy Ex2 (keV) <',F7.1,'>:',$)
      CALL READF(5,Ex2)
      i1=((Ex1-a0Ex)/a1Ex)+0.5 
      i2=((Ex2-a0Ex)/a1Ex)+0.5
      IF(i1.LT.0.OR.i1.GT.XDIM-1.OR.i2.LT.0.OR.i2.GT.XDIM-1)THEN
        WRITE(6,*)'Ex1 and Ex2 give channels outside the rho-spectrum, try again...'
        GO TO 21
      ENDIF

      WRITE(6,*)' '
      WRITE(6,30) rhonew1
 30   FORMAT( 'Lower target level density rho1 (1/MeV) <',F8.2,'>:',$)
      CALL READF(5,rhonew1)
      WRITE(6,31) rhonew2
 31   FORMAT( 'Upper target level density rho2 (1/MeV) <',F8.2,'>:',$)
      CALL READF(5,rhonew2)

C Reading sigsp.rsg from rhosigchi 
      IDEST=2
      ITYPE=3
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 2 and the name of your F from the'
      WRITE(6,*)'rhosigchi program: sigsp.rsg in the two next questions... '
      WRITE(6,*)' '
      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
      IF(bx+by.EQ.2.)THEN
        a0Eg=0.               !Defaults in case of no calibration
        a1Eg=120.
        ELSE
        a0Eg=cal(1,IDEST,1,1)  !Picks the spectrum calibration
        a1Eg=cal(1,IDEST,1,2)
      ENDIF

C Giving energy calibrations for sigsp.rsg
 22   CONTINUE
      WRITE(6,23) a0Eg
 23   FORMAT(/'Cal. coeff. a0Eg (keV) on x-axis    <',F8.1,'>:',$)
      CALL READF(5, a0Eg)
      WRITE(6,24) a1Eg
 24   FORMAT( 'Cal. coeff. a1Eg (keV/ch) on x-axis <',F8.3,'>:',$)
      CALL READF(5, a1Eg)

      WRITE(6,32) B
 32   FORMAT( 'Give factor B to multiply new F     <',F8.3,'>:',$)
      CALL READF(5, B)

      OPEN(23,FILE='normrhofin.dat')
      WRITE(23,*)Ex1,Ex2,rhonew1,rhonew2,B
      CLOSE(23)

C Calculating A, alpha
      xil = (Ex1 - a0Ex)/a1Ex
      il  = xil
      ih  = il + 1
      fl  = xil - il
      fh  = 1. - fl
      rhoold1 = fl * rMAT(1,il,0) + fh * rMAT(1,ih,0)
c      write(6,*)'il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold1',il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold1
      xil = (Ex2 - a0Ex)/a1Ex
      il  = xil
      ih  = il + 1
      fl  = xil - il
      fh  = 1. - fl
      rhoold2 = fl * rMAT(1,il,0) + fh * rMAT(1,ih,0)
c      write(6,*)'il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold2',il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold2

      alpha = (1./(Ex1-Ex2))*log((rhonew1*rhoold2)/(rhonew2*rhoold1))
      A     = (exp(-alpha*Ex1))*rhonew1/rhoold1

      WRITE(6,*)' '
      WRITE(6,*)'A = ',A,' alpha = ',alpha
      WRITE(6,*)'B = ',B
      WRITE(6,*)'(Assumes the alpha-correction is 1. at Eg = 5 MeV)'
      WRITE(6,*)' '

C Now, fasten seatbelts
      OPEN(27,FILE='normrhof.paw')
      WRITE(6,*)'       Rho        dRho          F          dF'
      DO i=0,200 
         Ex   = a0Ex + a1Ex * i
         Eg   = a0Eg + a1Eg * i
         rho  = A*rMAT(1,i,0)*(exp(alpha*Ex))
         drho = A*rMAT(1,i,1)*(exp(alpha*Ex))
         F    = B*rMAT(2,i,0)*(exp(alpha*(Eg-5.)))
         dF   = B*rMAT(2,i,1)*(exp(alpha*(Eg-5.)))
         WRITE( 6,40)rho,drho,F,dF
         WRITE(27,40)rho,drho,F,dF
 40      FORMAT(4E13.5)
      ENDDO
      CLOSE(27)

      END
