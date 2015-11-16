      SUBROUTINE DIVEFF(ANS,K)

C         correct spectrum for detector efficiency....
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60

      REAL*8         GAIN(6)
      INTEGER        ICAL, NTERMS
      COMMON /CALIB/ GAIN, ICAL, NTERMS

      CHARACTER*40 ANS
      REAL         PARS(9)
      REAL*8       X1,X2,F1,F2,U3,G
      COMMON /LUS/ IR,IW,IP,IG

      IF (ICAL.EQ.0) GO TO 950
      ANS(1:2)='  '
      IF (K.GE.3) GO TO 15

C         ask for efficiency parameter data file....

10    CALL ASK(28HEff. parameter data file = ?,28,ANS,K)
      IF (K.EQ.0) RETURN
15    CALL SETEXT(ANS,'.eff',J)

C          read parameters from disk file....

      OPEN(1,FILE=ANS,FORM='UNFORMATTED',STATUS='OLD',ERR=320)
      READ(1,ERR=340)JUNK
      READ(1,ERR=340)PARS
      CLOSE(1)

C          divide sp. by calculated efficiency....

      G=PARS(7)
      DCH=0.0
      DO 200,ICH=0,MAXCH
         CH=ICH
         CALL ENERGY(CH,DCH,X,DX,&950)
         IF (X.LE.0.0) GO TO 200
         X1=LOG(X/PARS(8))
         X2=LOG(X/PARS(9))
         F1=PARS(1)+PARS(2)*X1+PARS(3)*X1*X1
         F2=PARS(4)+PARS(5)*X2+PARS(6)*X2*X2
         IF (F1.LE.0.0D0 .OR. F2.LE.0.0D0) GO TO 200
         U3=EXP(-G*LOG(F1))+EXP(-G*LOG(F2))
         IF (U3.LE.0.0D0) GO TO 200
         EFF=EXP(EXP(-LOG(U3)/G))
         rSPEC(IDEST,ICH)=rSPEC(IDEST,ICH)/EFF
200   CONTINUE

      fname(2,IDEST)(5:8) = '.MOD'
      xcomm(1:3)='EF:'
      CALL AddComment(xcomm,3)
      RETURN

C            error messages....

320   WRITE(IW,*) 'File does not exist.'
      GO TO 10
340   WRITE(IW,*) 'Cannot read file.'
      CLOSE(1)
      GO TO 10
950   WRITE(IW,*) 'Cannot - no energy calibration.'
      RETURN
      END

