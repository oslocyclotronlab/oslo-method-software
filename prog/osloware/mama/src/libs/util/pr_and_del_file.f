
C=======================================================================

      SUBROUTINE PR_AND_DEL_FILE(LOG_UNIT)
C           print and delete file attached to log_unit....

      INTEGER LOG_UNIT

C        variables for handling PRINT/DELETE on unix w/ std f77....
      LOGICAL      FILE_EXISTS
      INTEGER      LPR_FILE_DEL,IEND
      EXTERNAL     LPR_FILE_DEL
      CHARACTER*50 PR_FILE
      CHARACTER*1  TEST

      INTEGER IW,IR,IP,IG
      COMMON/LUS/IW,IR,IP,IG


      INQUIRE(LOG_UNIT,EXIST=FILE_EXISTS,NAME=PR_FILE)
      IF(FILE_EXISTS) THEN
         REWIND(LOG_UNIT)
         READ(LOG_UNIT,'(A)',IOSTAT=IOS) TEST
         IF (IOS.NE.0) THEN
C              file is empty....
            CLOSE(LOG_UNIT,STATUS='DELETE')
         ELSE
            IEND = 50
C              strip off trailing blanks from file name....
C            DO WHILE (PR_FILE(IEND:IEND).NE." ")
            DO WHILE (PR_FILE(IEND:IEND).NE.' ')
               IEND = IEND - 1
            ENDDO
            CLOSE(LOG_UNIT,STATUS='KEEP')
C              call C subroutine to print and delete file....
            ISTAT = LPR_FILE_DEL(PR_FILE(1:IEND))
            IF(ISTAT.NE.0) THEN
               WRITE(IW,*)'Your file was saved since printing failed.'
               CLOSE(LOG_UNIT,STATUS='KEEP')
            ENDIF
         ENDIF
      ELSE
         WRITE(IW,*) 'There is no file to print.'
      ENDIF

      RETURN
      END
