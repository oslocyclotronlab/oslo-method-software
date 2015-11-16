
C=======================================================================

      SUBROUTINE ENERGY(X,DX,EG,DEG,*)

      REAL*8         GAIN(6)
      INTEGER        ICAL, NTERMS
      COMMON /CALIB/ GAIN, ICAL, NTERMS


      IF (ICAL.EQ.0) RETURN 1
      DEG=0.0
      EG=GAIN(NTERMS)
      DO 15 JJ=NTERMS-1,1,-1
         DEG=FLOAT(JJ)*GAIN(JJ+1)+DEG*X
         EG=GAIN(JJ)+EG*X
15    CONTINUE
      DEG=DEG*DX
      RETURN
      END
