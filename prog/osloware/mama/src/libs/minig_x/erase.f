
C=======================================================================

          SUBROUTINE ERASE

      CALL CLEAR_GW
      RETURN

          ENTRY BELL

      CALL QIO_PUT_TEXT(CHAR(7),1)
      RETURN

      END
