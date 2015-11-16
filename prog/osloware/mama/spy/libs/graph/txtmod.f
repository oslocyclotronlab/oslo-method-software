
C=======================================================================

      SUBROUTINE TXTMOD


      ENTRY GRAMOD
      RETURN

      ENTRY SETCOLOR(ICOL)

      J = ICOL
      IF (J.LT.1 .OR. J.GT.20) RETURN
C        call finig to dump stored plot array
C           before changing attribute block....
      CALL FINIG
      CALL SET_GW_FOREGROUND(J)
      RETURN
      END
