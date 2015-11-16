
C=======================================================================

      SUBROUTINE STARTWID(ANS,NC)

      REAL           FINEST(5), SWPARS(3)
      INTEGER        INFIX(3), INFIXRW, INFIXW
      COMMON /INEST/ FINEST,INFIX,SWPARS,INFIXRW,INFIXW

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG


      IF (NC.GT.2) THEN
          ANS(1:38)=ANS(3:40)
          NC=NC-2
      ELSE
10        WRITE(IW,20) SQRT(SWPARS(1)),SQRT(1000.0*SWPARS(2))
     +                 ,1000.0*SQRT(SWPARS(3))
20        FORMAT(
     + ' Initial estimates for the fitted peak widths are taken as:'/
     + '       FWHM = SQRT(F*F + G*G*x + H*H*x*x)  (x = ch. no. /1000)'/
     + '  Default values are:  F =', F7.2,', G =',F7.2,', H=',F7.2)
          CALL ASK(36HEnter F,G,H (rtn for default values),36,ANS,NC)
          IF (NC.EQ.0) GO TO 50
      ENDIF

      CALL FFIN(ANS,NC,SW1,SW2,SW3,&10)
      IF (SW1.LE.0.0.OR.SW2.LT.0.0) THEN
         WRITE(IW,*) 'Bad values. F must be .gt. 0, G must be .ge. 0.'
         GO TO 10
      ENDIF
      SWPARS(1)=SW1*SW1
      SWPARS(2)=SW2*SW2/1000.0
      SWPARS(3)=SW3*SW3/1000000.0

50    INFIXW = 1
      ans='y'
      WRITE(6,52)ans
52    FORMAT('Free all widths by default (y/n) <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')GO TO 60
      INFIXW  = 0

60    INFIXRW = 1
      ans='y'
      WRITE(6,62)ans
62    FORMAT('Free all relative widths by default (y/n) <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')GO TO 70
      INFIXRW = 0

70    RETURN
      END
