
C=======================================================================

      SUBROUTINE READSP(FILNAM,SP,NAMESP,NUMCH,IDIMSP,*)

C           subroutine to read spectrum from file (or ID) = FILNAM,
C              into array SP, of dimension IDIMSP....
C              NUMCH = number of channels read....
C              NAMESP = name of spectrum (character*8)....
C              return1 = open/read error....
C              default file extension = .spe
C           D. Radford      Apr. 1989....

C           modified to include reading from .spk and .mat files....
C              SPMODE = 0/1/2 for .spe/.spk/.mat files....

C           modified to include reading from .spn files....
C              SPMODE = 3 for .spn files....
C           D. Radford      Oct. 1987....

C           modified to read from .mat files using cursor....
C           D. Radford      Feb. 1991....
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      CHARACTER*40 FILNAM
      REAL         SP(*)
      CHARACTER*8  NAMESP
      INTEGER      NUMCH, IDIMSP, SPMODE /0/
      LOGICAL      NOT_INT

      COMMON /LUS/ IR,IW,IP,IG

C         if in .spk, .mat or .spn mode, and FILNAM has integers,
C            call SPKREAD or MATREAD....

      IF   (INDEX(FILNAM,'/C').NE.0
     + .OR. INDEX(FILNAM,'/c').NE.0 ) THEN
         IF (.NOT.DISP) THEN
            WRITE(IW,*) 'Bad command: New spectrum not yet displayed...'
            RETURN
         ENDIF
         CALL MATREAD(FILNAM,SP,NAMESP,NUMCH,IDIMSP,
     +                SPMODE,&120)
         SPMODE = 2
         RETURN
      ENDIF

      IF (SPMODE.NE.0) THEN
         NC=40
         CALL ININ_FLG(FILNAM,NC,IN1,IN2,IN3,NOT_INT)
         IF (.NOT. NOT_INT) THEN
            IF (SPMODE.EQ.1) THEN
               CALL SPKREAD(FILNAM,SP,NAMESP,NUMCH,IDIMSP,&120)
               RETURN
            ENDIF
            IF (SPMODE.GE.2) THEN
               CALL MATREAD(FILNAM,SP,NAMESP,NUMCH,IDIMSP,SPMODE,&120)
               RETURN
            ENDIF
         ENDIF
      ENDIF

C       remove leading spaces from FILNAM....
C       look for file extension in FILNAM....
C            ....if there is none, put it to .spe....
      CALL SETEXT(FILNAM,'.spe',I)

C         ....if it is .spk, .mat or .spn, call SPKREAD or MATREAD....
      IF (FILNAM(I:I+3).EQ.'.SPK'.OR.FILNAM(I:I+3).EQ.'.spk') THEN
C###         IF (FILNAM(I:I+3).EQ.'.spk') FILNAM(I:I+3) = '.SPK'
         CALL SPKREAD(FILNAM,SP,NAMESP,NUMCH,IDIMSP,&120)
         SPMODE=1
         RETURN
      ENDIF
      IF (FILNAM(I:I+3).EQ.'.MAT'.OR.FILNAM(I:I+3).EQ.'.mat') THEN
         SPMODE=2
         CALL MATREAD(FILNAM,SP,NAMESP,NUMCH,IDIMSP,SPMODE,&120)
         RETURN
      ENDIF
      IF (FILNAM(I:I+3).EQ.'.SPN'.OR.FILNAM(I:I+3).EQ.'.spn') THEN
         SPMODE=3
         CALL MATREAD(FILNAM,SP,NAMESP,NUMCH,IDIMSP,SPMODE,&120)
         RETURN
      ENDIF

C        read spectrum in standard GF2 format....

      OPEN(1,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD',ERR=70)
      READ(1,ERR=90) NAMESP,IDIM1,IDIM2,IRED1,IRED2
      NUMCH=IDIM1*IDIM2
      IF (NUMCH.GT.IDIMSP) THEN
        NUMCH=IDIMSP
        WRITE(IW,60)IDIMSP
60      FORMAT(' First',I6,' chs only taken.')
      ENDIF
      READ(1,ERR=90)(SP(I),I=1,NUMCH)
      CLOSE(1)
      SPMODE=0
      RETURN

C             error messages....

70    WRITE(IW,*) 'File does not exist.'
      GO TO 110
90    WRITE(IW,*) 'Cannot read file.'
110   CLOSE(1,ERR=120)
120   RETURN 1
      END
