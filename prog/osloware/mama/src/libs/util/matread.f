
C======================================================================

      SUBROUTINE MATREAD(FN,SP,NAMESP,NUMCH,IDIMSP,SPMODE,*)

C           subroutine to read spectrum from MATRIX file
C              into array SP, of dimension IDIMSP....
C              FN = file name, or contains range of channels to read....
C              NUMCH = number of channels read....
C              NAMESP = name of spectrum (char*8, set to ILO IHI)....
C              SPMODE = 2/3 for .mat/.spn files...
C              return1 = open/read error....
C              file extension must be .mat

C           D. Radford      Oct. 1986....

C           modified to include reading from .spn files....
C              SPMODE = 3 for .spn files....
C           .mat files have 2 bytes/ch., .spn files have 4 bytes/ch....
C           D. Radford      Oct. 1987....

C           modified to add integer*2 matrix rows using integer*4 addition....
C           this prevents overflows for larger counts/ch....
C           D.C. Radford    Jan. 1992....

      CHARACTER*40 FN
      REAL         SP(*)
      CHARACTER*8  NAMESP
      INTEGER      NUMCH, IDIMSP, SPMODE

      CHARACTER*40   ANS, FILNAM /' '/
      LOGICAL        NOT_INT, FIL_EXIST
      INTEGER*2      I2MAT(8192)
      INTEGER*4      I4SPN(8192)
      INTEGER*4      I4SUM(8192)
      EQUIVALENCE   (I4SPN,I2MAT)

      COMMON /LUS/ IR,IW,IP,IG


C              look for integer chan. no. limits....
      NC=40
      CALL ININ_FLG(FN,NC,ILO,IHI,IN3,NOT_INT)
      IF (NOT_INT) THEN
C                 not integers; FN = file name, check matrix file exists....
         INQUIRE(FILE=FN,EXIST=FIL_EXIST)
         IF (.NOT.FIL_EXIST) THEN
            WRITE(IW,*) 'File does not exist.'
            RETURN 1
         ENDIF
         FILNAM=FN
C                 ask for range of Y-channels to be added together....
80       CALL ASK(34HType range of y-channels (lo,hi) ?,34,ANS,NC)
         IF (NC.EQ.0) RETURN
         CALL ININ(ANS,NC,ILO,IHI,IN3,&80)
      ENDIF

      IF (IHI.EQ.0) IHI=ILO
      IF (IHI.LT.ILO) THEN
         I=ILO
         ILO=IHI
         IHI=I
      ENDIF

      OPEN(1,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',
     +           RECL=2048*(SPMODE-1),ACCESS='DIRECT',ERR=800)
      NUMCH = 4096
      IF (ILO.LT.0 .OR. IHI.GT.NUMCH-1) THEN
         WRITE(IW,*) 'Bad y-gate channel limits:',ILO,IHI
         GO TO 900
      ENDIF
      IF (IHI-ILO.GT.256) THEN
         WRITE(IW,*) 'Large range of channels to be read; will take',
     +               ' a very long time.'
         CALL CASKYN('...Proceed? (Y/N)',&80)
      ENDIF

C           initialize I4SUM....
      DO 120 I=1,NUMCH
         I4SUM(I) = 0
120   CONTINUE

      IF (SPMODE.EQ.2) THEN

C          read matrix rows into I2MAT and add into I4SUM....
         DO 140 IY=ILO+1,IHI+1
            READ(1,REC=IY,ERR=900) (I2MAT(J),J=1,NUMCH)
            DO 130 I=1,NUMCH
               I4SUM(I) = I4SUM(I) + I2MAT(I)
130         CONTINUE
140      CONTINUE

      ELSE

C          read matrix rows into I4SPN and add into I4SUM....
         DO 200 IY=ILO+1,IHI+1
            READ(1,REC=IY,ERR=900) (I4SPN(J),J=1,NUMCH)
            DO 180 I=1,NUMCH
               I4SUM(I) = I4SUM(I) + I4SPN(I)
180         CONTINUE
200      CONTINUE

      ENDIF

C           convert to real format....
      DO 250 I=1,NUMCH
         SP(I)=FLOAT(I4SUM(I))
250   CONTINUE

      CLOSE(1)
      FN=FILNAM
      WRITE (NAMESP,'(2I4)') ILO,IHI
      RETURN

C             error messages....

800   WRITE(IW,*) 'Cannot open file ',FILNAM
      GO TO 910
900   WRITE(IW,*) 'Cannot read file ',FILNAM
910   CLOSE(1,ERR=920)
920   RETURN 1
      END
