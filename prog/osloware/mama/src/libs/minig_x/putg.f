
C=======================================================================

          SUBROUTINE PUTG(TEXT,NC,KAD,IDI)

C          write nc characters from text to terminal as graphtext....
C          if nc < 0, the characters are stored in the graphics buffer;
C             otherwise the buffer is emptied and the chars written....
C          kad = 1-3/4-6/7-9 for text to the right/centered/to the left
C                    of the current graphics cursor position....
C           if kad = 3,6 or 9 the characters are written below the
C                    current position....
C          idi = not used....

      CHARACTER*(*) TEXT
      INTEGER       NC,KAD,IDI,IX,IY,NCHARS

      INTEGER*4       WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY
      COMMON /GW_DAT/ WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY

      CHARACTER*100 NYTEXT

      NCHARS = IABS(NC)

      ILO = 1
      DO WHILE(ILO.LT.NCHARS .AND. TEXT(ILO:ILO).EQ.' ')
         ILO = ILO + 1
      ENDDO
      IF (NCHARS-ILO+1.EQ.0) RETURN

      IX = NOWATX
      IY = NOWATY - 2
      IF (KAD.EQ.3 .OR. KAD.EQ.6 .OR. KAD.EQ.9)
     +         IY = IY + 12
      IF (KAD.GT.6) THEN
         IX = IX - (NCHARS-ILO+1)*5
      ELSEIF (KAD.GT.3) THEN
         IX = IX - (NCHARS-ILO+1)*5/2
      ENDIF
      IF (IX.LT.0) IX = 0
      IF (IY.LT.0) IY = 0

C sometimes TEXT is short by one character Andreas Schiller September 10 2003
      NYTEXT=TEXT//' '
      
      CALL DRAW_GW_STRING(IX,IY,NCHARS-ILO+1,NYTEXT(ILO:NCHARS))
      RETURN
      END





