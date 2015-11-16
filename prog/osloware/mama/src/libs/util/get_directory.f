
C=======================================================================

      SUBROUTINE GET_DIRECTORY(LOGICAL_NAME,DIR_NAME,IOFFSET)
C           translate logical name to give directory name....

      CHARACTER*(*) LOGICAL_NAME, DIR_NAME
      INTEGER       IOFFSET

      CALL GETENV(LOGICAL_NAME,DIR_NAME)
      IOFFSET = LNBLNK(DIR_NAME)
      IOFFSET  = IOFFSET + 1
      DIR_NAME(IOFFSET:IOFFSET) = '/'
      IOFFSET  = IOFFSET + 1

      RETURN
      END
