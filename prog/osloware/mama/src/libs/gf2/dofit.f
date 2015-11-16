
C=======================================================================

      SUBROUTINE DOFIT(IDATA,READY)

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      LOGICAL READY
c      CHARACTER*40 ANS

C          get limits etc. and/or do fit....

      IF (IDATA.GT.0) THEN
C          get limits, peak positions and fixed parameters....
         NPKS=IDATA
         CALL GFSET
         IF (NPKS.LE.0) THEN
            READY = .FALSE.
            RETURN
         ENDIF
         READY=.TRUE.
      ELSEIF (IDATA.LT.0) THEN
C          reset initial parameter estimates....
         CALL PARSET(0)
      ENDIF

c130   CALL ASK(37H Max. no. of iterations=?(rtn for 50),37,ANS,K)
      MAXITS=100
c      IF (K.NE.0) THEN
c         CALL ININ(ANS,K,MAXITS,J1,J2,&130)
c         IF (MAXITS.LE.0) RETURN
c      ENDIF

C          do fit....
      CALL FITTER(MAXITS,&160,&150)

C          display fit and difference and list parameters....
      CALL GFFIN(0)
      RETURN
150   CALL GFFIN(1)
160   RETURN
      END
