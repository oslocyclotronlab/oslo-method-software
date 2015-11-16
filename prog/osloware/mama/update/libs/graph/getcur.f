
C=======================================================================

          SUBROUTINE GETCUR(COUT,IXI,IYI)

C           check to see if mouse button has been pressed....

      CHARACTER*(*) COUT
      INTEGER*4     IXI,IYI

      INTEGER       INCR /8/
      BYTE          BIN(4)
      CHARACTER*4   CIN
      EQUIVALENCE  (BIN,CIN)

      CHARACTER*92 KEY_LOOKUP
      DATA          KEY_LOOKUP(1:50)
     +        /'1234567890-= ***********QWERTYUIOP[]***********ASD'/
C               12345678911234567892123456789312345678941234567895
      DATA          KEY_LOOKUP(51:92)
     +        /'FGHJKL;  ***********ZXCVBNM,. *********** '/
C               123456789112345678921234567893123456789412
      INTEGER*4    BUTTON, KEY


C           first check for button press on graphics window....
10    CALL CHECK_GW_BP(BUTTON,IXI,IYI)
      IF (BUTTON.GT.0) THEN
         COUT(1:1) = 'G'
         IF (BUTTON.EQ.2) COUT(1:1) = 'X'
         IF (BUTTON.EQ.3) COUT(1:1) = 'X'
         RETURN
      ENDIF

C           next check for key press on graphics window....
      CALL CHECK_GW_KP(KEY,IXI,IYI)
      IF (KEY.GT.0) THEN
         COUT(1:1) = ' '
C***
C***         WRITE (6,*) ' Keycode = ', KEY
C***
C              decode alphanumeric keystrokes....
         ILU = KEY - 36
         IF (ILU.GT.0 .AND. ILU.LE.92) THEN
            COUT(1:1) = KEY_LOOKUP(ILU:ILU)
            IF (COUT(1:1).NE.'*') THEN
               RETURN
            ENDIF
         ENDIF

C                 check for <keypad 5> key....
         IF (KEY.EQ.99) THEN
            IF (INCR.EQ.1) THEN
               INCR = 8
            ELSE
               INCR = 1
            ENDIF
            GO TO 10

C                 check for arrow keys and move cursor appropriately....
         ELSEIF (KEY.EQ.76  .OR.
     +           KEY.EQ.98  .OR.
     +           KEY.EQ.100 .OR.
     +           KEY.EQ.120) THEN
            IXI = 0
            IYI = 0
            IF (KEY.EQ.76 ) IYI = -INCR
            IF (KEY.EQ.120) IYI = INCR
            IF (KEY.EQ.100) IXI = INCR
            IF (KEY.EQ.98 ) IXI = -INCR

C              put pointer at new location....
            CALL MOVE_GW_POINTER(IXI,IYI)
            GO TO 10

         ENDIF

      ENDIF

C           next check for pointer motion on graphics window....
      CALL CHECK_GW_PM

C           next, if no response on graphics window, try text window....
      CALL CHK_READ(COUT,NC,IXI,IYI)

C        if no response on text window, try graphics window again....
      IF (NC.LE.0) GO TO 10

      IF (COUT(1:1).NE.CHAR(27)) RETURN

C        check for <keypad 5> key....
      IF (COUT(2:6).EQ.'[218z') THEN
         IF (INCR.EQ.1) THEN
            INCR = 8
         ELSE
            INCR = 1
         ENDIF
         GO TO 10
         
C        check for arrow keys and move cursor appropriately....
      ELSEIF (COUT(2:3).EQ.'[A'  .OR.
     +        COUT(2:3).EQ.'[B'  .OR.
     +        COUT(2:3).EQ.'[C'  .OR.
     +        COUT(2:3).EQ.'[D') THEN

         IXI = 0
         IYI = 0
         IF (COUT(2:3).EQ.'[A') IYI = -INCR
         IF (COUT(2:3).EQ.'[B') IYI =  INCR
         IF (COUT(2:3).EQ.'[C') IXI =  INCR
         IF (COUT(2:3).EQ.'[D') IXI = -INCR

C           put pointer at new location....
         CALL MOVE_GW_POINTER(IXI,IYI)
         GO TO 10
         
      ENDIF

      RETURN
      END
