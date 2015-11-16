      PROGRAM normrhof
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      CHARACTER fname*8,comm*60
      CHARACTER APP*4
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
      WRITE(6,*)'    | Modified: 15/02 - 2002  10/05 -2002         |'
      WRITE(6,*)'    | Magne Guttormsen                            |'
      WRITE(6,*)'    |_____________________________________________|'
      WRITE(6,*)' '

C History:
C 5 May  2002: Interpolation was before wrong, corrected 
C              Corrected fl  = xil - il  -> fh  = xil - il etc.

  
      OPEN(23,FILE='normrhofin.dat',STATUS='old',ERR=666)
      READ(23,*,ERR=666)comm
      READ(23,*,ERR=666)Ex1,Ex2,dum,rhonew1,dum,rhonew2,B
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
      IF(IDEST.NE.1)stop
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


C Reading sigsp.rsg from rhosigchi 
      IDEST=2
      ITYPE=3
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 2 and the name of your F from the'
      WRITE(6,*)'rhosigchi program: sigsp.rsg in the two next questions... '
      WRITE(6,*)' '
      CALL READFILE
      IF(IDEST.NE.2)stop
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
      CONTINUE
      WRITE(6,23) a0Eg
 23   FORMAT(/'Cal. coeff. a0Eg (keV) on x-axis    <',F8.1,'>:',$)
      CALL READF(5, a0Eg)
      WRITE(6,24) a1Eg
 24   FORMAT( 'Cal. coeff. a1Eg (keV/ch) on x-axis <',F8.3,'>:',$)
      CALL READF(5, a1Eg)


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
 30   FORMAT( 'Lower target level density rho1 (1/MeV) <',E12.6,'>:',$)
      CALL READF(5,rhonew1)
      WRITE(6,31) rhonew2
 31   FORMAT( 'Upper target level density rho2 (1/MeV) <',E12.6,'>:',$)
      CALL READF(5,rhonew2)


C Calculating A, alpha
      xil = (Ex1 - a0Ex)/a1Ex
      il  = xil
      ih  = il + 1
      fh  = xil - il
      fl  = 1. - fh
      rhoold1 = fl * rMAT(1,il,0) + fh * rMAT(1,ih,0)
C      write(6,*)'il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold1',il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold1
      xil = (Ex2 - a0Ex)/a1Ex
      il  = xil
      ih  = il + 1
      fh  = xil - il
      fl  = 1. - fh
      write(6,*)
      rhoold2 = fl * rMAT(1,il,0) + fh * rMAT(1,ih,0)
C      write(6,*)'il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold2',il,ih,rMAT(1,il,0),rMAT(1,ih,0),rhoold2

      alpha = (1./(Ex1-Ex2))*log((rhonew1*rhoold2)/(rhonew2*rhoold1))
      A     = (exp(-alpha*Ex1))*rhonew1/rhoold1

      WRITE(6,*)' '
      WRITE(6,*)'A = ',A,' alpha (1/MeV) = ',alpha*1000.

      WRITE(6,*)' '




      WRITE(6,32) B
 32   FORMAT( 'Give factor B to multiply new F     <',F8.3,'>:',$)
      CALL READF(5, B)

      OPEN(23,FILE='normrhofin.dat')
      WRITE(23,*)'Ex1(keV) Ex2(keV)   Rhoold1     Rhonew1     Rhoold2     Rhonew2'
      WRITE(23,34)Ex1,Ex2,rhoold1,rhonew1,rhoold2,rhonew2
 34   FORMAT(F7.2,F10.2,5E12.5)
      WRITE(23,*)' '
      WRITE(23,*)'A = ',A,' alpha (1/MeV) = ',alpha*1000,' B = ',B


      CLOSE(23)

      WRITE(6,*)'B = ',B
      WRITE(6,*)'(Assumes the alpha-correction is 1. at Eg = 5 MeV)'
      WRITE(6,*)' '

C Now, fasten seatbelts
      OPEN(27,FILE='normrho.paw')
      WRITE(27,*)'  No    Ex(MeV)    Rho(1/MeV)    dRho(1/MeV)'
      DO i=0,200 
         Ex   = a0Ex + a1Ex * i
         rho  = A*rMAT(1,i,0)*(exp(alpha*Ex))
         drho = A*rMAT(1,i,1)*(exp(alpha*Ex))
         WRITE(27,50)i+1,Ex/1000.,rho,drho 
  50     FORMAT(I4,F11.3,'',2E15.3)
      ENDDO
      CLOSE(27)

      OPEN(28,FILE='normf.paw')
      WRITE(28,*)'  No    Eg(MeV)    f(MeV**-3)    df(MeV**-3)'
      DO i=0,200 
         Eg   = a0Eg + a1Eg * i
         F    = B*rMAT(2,i,0)*(exp(alpha*(Eg-5.)))
         dF   = B*rMAT(2,i,1)*(exp(alpha*(Eg-5.)))
         WRITE(28,50)i+1,Eg/1000.,F,dF  
      ENDDO
      CLOSE(28)


      END
