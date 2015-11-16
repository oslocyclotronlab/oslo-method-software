
C=======================================================================

          SUBROUTINE KTRAS(IX,IY,I)

C             IX, IY  = destination in screen pixels
C             I       = 0/1/2 for move/draw/point

      INTEGER  IX,IY,I,NEWX,NEWY,NSTORED /0/

      INTEGER*4       WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY
      COMMON /GW_DAT/ WIN_WIDTH,WIN_HEIGHT,NOWATX,NOWATY

      INTEGER*4 POINTS(2,256)

      NEWX = IX
      NEWY = WIN_HEIGHT-IY

      IF (I.NE.1 .AND. NSTORED.GE.2) THEN
C           empty buffer of stored x,y values....
         CALL DRAW_GW_LINES(POINTS,NSTORED)
         NSTORED = 0
      ENDIF

      IF (I.EQ.2) THEN
C           point mode.....
         CALL DRAW_GW_POINT(NEWX,NEWY)
      ELSEIF (I.EQ.1) THEN
C           draw mode.....
C           ....store newx,newx inbuffer of stored x,y values....
         IF (NSTORED.EQ.0) THEN
            POINTS(1,1) = NOWATX
            POINTS(2,1) = NOWATY
            NSTORED = 1
         ENDIF
         NSTORED = NSTORED + 1
         POINTS(1,NSTORED) = NEWX
         POINTS(2,NSTORED) = NEWY
         IF (NSTORED.EQ.256) THEN
C              empty buffer of stored x,y values....
            CALL DRAW_GW_LINES(POINTS,NSTORED)
            NSTORED = 0
         ENDIF
      ENDIF

      NOWATX = NEWX
      NOWATY = NEWY
      RETURN


          ENTRY FINIG

C        empty buffer of stored x,y values....
      IF (NSTORED.GE.2) THEN
         CALL DRAW_GW_LINES(POINTS,NSTORED)
         NSTORED = 0
      ENDIF
      CALL FLUSH_GW
      RETURN

      END
