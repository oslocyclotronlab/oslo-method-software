
C=======================================================================

          SUBROUTINE RETIC(X,Y,COUT)

C            call Graphics Input (GIN) mode - i.e. call up cursor....
C            X,Y  = position w.r.to axes when GIN terminated...
C            COUT = character used to terminate GIN....

      REAL            X, Y
      CHARACTER*(*)   COUT

      INTEGER*4       WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY
      COMMON /GW_DAT/ WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY

      INTEGER         IXI,IYI
      CHARACTER*40    CIN

C           ring bell....
c      CALL BELL

      CALL SET_UP_CURSOR

C           wait for mouse button or text window keyboard button
C              to be pressed....
C           COUT(1:1) returns pressed key, or G for mouse button....
      CALL GETCUR(CIN,IXI,IYI)

      CALL DONE_CURSOR

C           convert coordinates and exit....
      X = 0.0
      Y = 0.0
      IF (IXI.GE.0 .AND. IXI.LE.WIN_WIDTH .AND.
     +    IYI.GE.0 .AND. IYI.LE.WIN_HEIGHT)
     +                            CALL CVXY(X,Y,IXI,WIN_HEIGHT-IYI,2)

      COUT(1:1) = CIN(1:1)

      RETURN
      END
