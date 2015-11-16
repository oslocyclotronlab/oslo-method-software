
C=======================================================================

          SUBROUTINE MSPOT(NX,NY)

      INTEGER  NX, NY, K

C            move graphics cursor to (mx,my) in screen pixels....
      K=0
      GO TO 10

          ENTRY IVECT(NX,NY)
C            draw from current position to (mx,my) in screen pixels....
      K=1
10    CALL KTRAS(NX,NY,K)
      RETURN
      END
