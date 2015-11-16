C=======================================================================

      SUBROUTINE OPEN_PR_FILE(ILU,FILENAME)

C        open new file on logical unit ILU....
C        FILENAME = default file name....   

      INTEGER       ILU
      CHARACTER*(*) FILENAME

      OPEN(ILU,FILE=FILENAME,STATUS='NEW')
      RETURN
      END
