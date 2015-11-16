
C=======================================================================

          SUBROUTINE CVXY(X,Y,IX,IY,I)

C            i = 1: convert x,y to ix,iy  (axes to screen pixels)
C            i = 2: convert ix,iy to x,y  (screen pixels to axes)

      REAL        X, Y
      INTEGER     IX, IY, I

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM


      IF (I.EQ.1) THEN
C           conversion from axes to virtual display coords....

         IF (IYFLAG.EQ.1) THEN
C              linear y-axis....
            YC = Y-FY0
         ELSEIF (IYFLAG.EQ.2) THEN
C              sqrt y-axis....
            YC = 0.0
            IF (Y.GT.0.0) YC = SQRT(Y)-SQRT(FY0)
         ELSE
C              logarithmic y-axis....
            YC = ALOG(AMAX1(Y,1.0))-ALOG(FY0)
         ENDIF

         IX = IX0 + NINT((X-FX0)/FDX * FLOAT(IDX))
         IY = IY0 + NINT(   YC  /FDY * FLOAT(IDY))

         IF (IX.LT.0)       IX=0
         IF (IX.GT.IDX+IX0) IX=IDX+IX0
         IF (IY.LT.0)       IY=0
         IF (IY.GT.IDY+IY0) IY=IDY+IY0

      ELSE
C           conversion from virtual display to axes coords....

         X = FX0 + FLOAT(IX-IX0) * FDX/FLOAT(IDX)
         A =       FLOAT(IY-IY0) * FDY/FLOAT(IDY)

         IF (IYFLAG.EQ.1) THEN
C              linear y-axis....
            Y = FY0 + A
         ELSEIF (IYFLAG.EQ.2) THEN
C              sqrt y-axis....
            Y = FY0 + A*(A + 2.0*SQRT(FY0))
         ELSE
C              logarithmic y-axis....
            Y = FY0 * 2.718282**A
         ENDIF

      ENDIF

      RETURN
      END
