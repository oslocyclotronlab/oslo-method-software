
C=======================================================================

      SUBROUTINE INITG(NX,NY)

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

C             FDX = range of channels on x-axis
C             FX0 = starting channel on x-axis
C             FDY = range of counts on y-axis
C             FY0 = starting counts on y-axis
C             IDX = no. of x-pixels in display region
C             IX0 = low x-pixel in display region
C             IDY = no. of y-pixels in display region
C             IY0 = low y-pixel in display region
C             IYFLAG = 1 for linear y-axis
C                      2 for sqrt y-axis
C                      3 for log y-axis
C             ITERM : not used....

      INTEGER*4       WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY
      COMMON /GW_DAT/ WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY

      LOGICAL      FIRST /.TRUE./
      EXTERNAL     OPEN_GWINDOW
      EXTERNAL     GET_GW_GEOMETRY

      IF (FIRST) THEN
         FIRST = .FALSE.

C         create new graphics window on first entry....
         CALL OPEN_GWINDOW
         CALL FINIG         !denne har jeg satt inn igjen 1.nov. 1997,magne
         CALL TXTCLR(0)
      ENDIF

      CALL GET_GW_GEOMETRY(WIN_WIDTH,WIN_HEIGHT)
      NX=WIN_WIDTH - 5
      NY=WIN_HEIGHT - 44  !before it was -40
      RETURN
      END
