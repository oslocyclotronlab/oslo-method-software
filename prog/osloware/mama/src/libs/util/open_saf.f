
C========================================================================

      SUBROUTINE OPEN_SAF(FILENAME, F_EXT, LOG_UNIT)

*         filename: name of file to be opened (character string)
*         ext:      default extension of file (character*4)
*         log_unit: logical unit for file     (integer*4)

      CHARACTER*40  FILENAME
      CHARACTER*4   F_EXT
      INTEGER*4     LOG_UNIT

      INTEGER*4     IOS, NC
      CHARACTER*45  MESSAGE

      INTEGER       IR,IW,IP,IG
      COMMON /LUS/  IR,IW,IP,IG


5     OPEN(LOG_UNIT, FILE = FILENAME, STATUS='OLD', IOSTAT=IOS)
      IF (IOS.EQ.0) THEN
         WRITE(IW,*) 'File ', FILENAME,' already exists...'
         CALL CASKYN('  ...delete? (Y/N)', &99)
         CLOSE(LOG_UNIT, STATUS='DELETE', IOSTAT=IOS)
         IF (IOS.NE.0) THEN
            WRITE(IW, *) 'Cannot delete file...'
            GO TO 99
         ENDIF
      ENDIF
      OPEN(LOG_UNIT, FILE = FILENAME, STATUS='NEW', IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE(IW, *) 'Cannot open new file ', FILENAME
         GO TO 99
      ENDIF

      RETURN

99    CLOSE(LOG_UNIT, IOSTAT=IOS)
      WRITE(MESSAGE, 199)
     $     '-- New file name = ? (default .EXT = ', F_EXT(1:4), '). '
 199  FORMAT( 38A, 4A, 3A)
      CALL CASK( MESSAGE,
*     +     '-- New file name = ? ,default .EXT = ', F_EXT,
     +        FILENAME, NC)
      CALL SETEXT(FILENAME, F_EXT(1:4), NC)
      GO TO 5

      END
