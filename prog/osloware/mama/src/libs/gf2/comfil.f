
C=======================================================================

      SUBROUTINE COMFIL(ANS,K)

      CHARACTER*40 ANS
      INTEGER      K

      LOGICAL        CFLOG
      COMMON /CFLOG/ CFLOG

      LOGICAL        CFOPEN /.FALSE./
      INTEGER        ICF /4/, ICON /5/

      COMMON /LUS/ IR,IW,IP,IG


      ANS(1:2) = '  '
      IF (K.GE.3) GO TO 15

C         ask for command file name....
10    IR = ICON
      CALL CASK('Command file name = ? (default .EXT = .cmd)',ANS,K)
      IF (K.EQ.0) RETURN

15    CALL SETEXT(ANS,'.cmd',J)

      IF (ANS(1:3).EQ.'END' .OR. ANS(1:3).EQ.'end') THEN
C           ....close command file, lu IR = console....
         CFOPEN = .FALSE.
         CFLOG = .FALSE.
         IR = ICON
         CLOSE(ICF,ERR=900)

      ELSEIF (ANS(1:3).EQ.'CON' .OR. ANS(1:3).EQ.'con') THEN
         IF (CFOPEN) THEN
            IR = ICF
         ELSE
            WRITE(IW,*) 'Command file not open.'
         ENDIF

      ELSEIF (ANS(1:3).EQ.'CHK' .OR. ANS(1:3).EQ.'chk') THEN
         IF (CFOPEN) THEN
            IR = ICON
            CALL CASKYN('Proceed? (Y/N)',&900)
            IR = ICF
         ELSEIF (.NOT.CFLOG) THEN
            WRITE(IW,*) 'Command file not open.'
         ENDIF

      ELSEIF (ANS(1:3).EQ.'ERA' .OR. ANS(1:3).EQ.'era') THEN
         CALL ERASE
c         CALL TXTMOD

      ELSEIF (ANS(1:3).EQ.'LOG' .OR. ANS(1:3).EQ.'log') THEN
         IF (CFOPEN.OR.CFLOG) CLOSE(ICF,ERR=50)
50       CFOPEN = .FALSE.
         CFLOG = .FALSE.
         IR = ICON
         CALL CASK(
     +        'File name for command logging = ? (default .EXT = .cmd)',
     +        ANS,K)
         IF (K.EQ.0) RETURN

         CALL SETEXT(ANS,'.cmd',J)
         IF (ANS(1:3).EQ.'END' .OR. ANS(1:3).EQ.'end' .OR.
     +       ANS(1:3).EQ.'CON' .OR. ANS(1:3).EQ.'con' .OR.
     +       ANS(1:3).EQ.'CHK' .OR. ANS(1:3).EQ.'chk' .OR.
     +       ANS(1:3).EQ.'ERA' .OR. ANS(1:3).EQ.'era' .OR.
     +       ANS(1:3).EQ.'LOG' .OR. ANS(1:3).EQ.'log') THEN
            WRITE(IW,*) '*** That is an illegal command file name. ***'
            GO TO 50
         ENDIF

C          ....open log command file for output....
C          ....all input from lu IR to be copied to lu ICF....
         OPEN(ICF,FILE=ANS,STATUS='NEW',ERR=990)
         CFLOG = .TRUE.

      ELSE
C         CF filename....
C          ....open command file for input on lu IR....
         IF (CFOPEN.OR.CFLOG) CLOSE(ICF,ERR=80)
80       CFLOG = .FALSE.
         OPEN(ICF,FILE=ANS,STATUS='OLD',ERR=920)
         CFOPEN = .TRUE.
         IR = ICF
      ENDIF

900   RETURN

920   WRITE(IW,*) 'File does not exist.'
      CFOPEN = .FALSE.
      CLOSE(ICF,ERR=10)
      GO TO 10

990   WRITE(IW,*) '*** Could not open new command file. ***'
      RETURN
      END
