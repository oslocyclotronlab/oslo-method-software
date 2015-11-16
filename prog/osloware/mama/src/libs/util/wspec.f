
C=======================================================================

      SUBROUTINE WSPEC(FILNAM,SPEC,IDIM)

C         subroutine to write spectra in GF2 format....
C            FILNAM = name of file to be created and written....
C            SPEC = REAL spectrum of dimension IDIM....

      CHARACTER*40 FILNAM
      REAL         SPEC(IDIM)
      INTEGER      IDIM
      CHARACTER*8  NAMESP
      LOGICAL      FIL_EXIST

      COMMON /LUS/ IR,IW,IP,IG


      CALL SETEXT(FILNAM,'.spe',J)
      NAMESP = FILNAM(1:8)
      IF (J.LE.8) NAMESP(J:8) = ' '

C           check to see if required disk file already exists...,
      INQUIRE(FILE=FILNAM,EXIST=FIL_EXIST)
      IF (FIL_EXIST) THEN
         WRITE(IW,*) 'WARNING - file ',FILNAM,' already exists....'
         WRITE(IW,*) '  - it will be deleted and replaced ',
     +                             'if you continue!'
         WRITE(IW,*) '  - the spectrum will be lost ',
     +                             'if you do not continue!'
         CALL CASKYN('  - Continue ? (Y/N)',&700)
      ENDIF

      OPEN(1,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',ERR=600)
      CLOSE(1,STATUS='DELETE')
600   OPEN(1,FILE=FILNAM,FORM='UNFORMATTED',STATUS='NEW')

      WRITE(1) NAMESP,IDIM,1,1,1
      WRITE(1) SPEC
      CLOSE(1)

700   RETURN
      END
