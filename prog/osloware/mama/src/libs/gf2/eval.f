
C=======================================================================

      SUBROUTINE EVAL(PARS,II,FIT,NPKS,MODE)

C       this eval is for use with 'GF2' version V....
C          D. C. Radford     July 1985

      REAL    PARS(51),Y(15),Y1(15),Y2(15)
      LOGICAL NOTAIL

      REAL           DERIVS(51)
      COMMON /DERIV/ DERIVS

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS
      IF(NPKS.GT.15)THEN
        WRITE(6,*)'Too many peaks'
        RETURN
      ENDIF
C       calculate the fit using present values of the pars....

      IF (MODE.EQ.-9) THEN

C         mode = -9 ; initialise, i.e calculate  IX0,NOTAIL,Y,Y1,Y2....

         IX0=(MCH(1)+MCH(2)+2)/2
         NOTAIL=.TRUE.
         IF (II.EQ.0 .AND. PARS(4).EQ.0.0) RETURN
         NOTAIL=.FALSE.
         DO 10 I=1,NPKS
            Y(I)=PARS(3*I+5)/(3.33021838*PARS(5))
            Y1(I)=ERFC(Y(I))
            IF (Y1(I).EQ.0.0) THEN
               NOTAIL=.TRUE.
            ELSE
               Y2(I)=EXP(-Y(I)*Y(I))*1.12837917/Y1(I)
            ENDIF
10       CONTINUE
         RETURN
      ENDIF

      X=II-IX0
      FIT=PARS(1)+PARS(2)*X+PARS(3)*X*X
      IF (MODE.GE.1) THEN
         DERIVS(2)=X
         DERIVS(3)=X*X
         DERIVS(4)=0.
         DERIVS(5)=0.
         DERIVS(6)=0.
      ENDIF
      X1=II-1

      DO 50 I=1,NPKS
         WIDTH=PARS(3*I+5)/2.35482
         H=PARS(3*I+6)
         X=X1-PARS(3*I+4)

         W=X/(1.41421356*WIDTH)
         IF (ABS(W).GT.4.0) THEN
            U1=0.0
            U3=0.0
            IF (X.LT.0.0) U3=2.0
         ELSE
            U1=EXP(-W*W)
            U3=ERFC(W)
         ENDIF

         IF (MODE.EQ.-1) THEN
C               mode = -1; calculate background only....
            FIT=FIT+H*PARS(6)*U3/200.0
            GO TO 50
         ENDIF

         IF (NOTAIL) THEN
C               notail = true; pur gaussians only....
            U=U1+PARS(6)*U3/200.0
            FIT=FIT+H*U
C             calculate derivs only for mode.ge.1....
            IF (MODE.GE.1) THEN
               DERIVS(6)=DERIVS(6)+H*U3/200.0
               A=U1*(W+PARS(6)/354.49077)*2.0
               DERIVS(3*I+4)=H*A/(1.41421356*WIDTH)
               DERIVS(3*I+5)=H*W*A/WIDTH
               DERIVS(3*I+6)=U
            ENDIF
            GO TO 50
         ENDIF

         R=PARS(4)/100.0
         R1=1.0-R
         BETA=PARS(5)
         Z=W+Y(I)
         IF (ABS(X/BETA).GT.12.0) THEN
            U5=0.0
            U6=0.0
            U7=0.0
         ELSE
            U7=EXP(X/BETA)/Y1(I)
            IF (ABS(Z).GT.4.0) THEN
               U5=0.0
               IF (Z.LT.0.0) U5=2.0
               U6=0.0
            ELSE
               U5=ERFC(Z)
               U6=EXP(-Z*Z)*1.12837917
            ENDIF
         ENDIF
         U2=U7*U5

         U=R1*U1+R*U2+PARS(6)*U3/200.0
         FIT=FIT+H*U

C          calculate derivs only for mode.ge.1....
         IF (MODE.GE.1) THEN
            U8=U5*Y2(I)
            DERIVS(4)=DERIVS(4)+H*(U2-U1)/100.0
            DERIVS(5)=DERIVS(5)+R*H*U7*(Y(I)*(U6-U8)-U5*X/BETA)/BETA
            DERIVS(6)=DERIVS(6)+H*U3/200.0
            A=U1*(R1*W+PARS(6)/354.49077)*2.0
            DERIVS(3*I+4)=H*(A+R*U7*(U6-2.0*U5*Y(I)))/(1.41421356*WIDTH)
            DERIVS(3*I+5)=H*(W*A+R*U7*(U6*(W-Y(I))+U8*Y(I)))/WIDTH
            DERIVS(3*I+6)=U
         ENDIF

50    CONTINUE
      RETURN
      END
