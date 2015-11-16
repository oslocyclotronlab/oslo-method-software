
C=======================================================================

      SUBROUTINE SPKREAD(FN,SP,NAMESP,NUMCH,IDIMSP,*)

C           subroutine to read spectrum from .spk file
C              into array SP, of dimension IDIMSP....
C              FN = file name, or ID of spectrum to be read....
C              NUMCH = number of channels read....
C              NAMESP = name of spectrum (char*8, set to ID)....
C              return1 = open/read error....
C              file extension must be .spk (ORNL format)....

C           D.C. Radford    Oct. 1986....

      CHARACTER*40 FN
      REAL         SP(*)
      CHARACTER*8  NAMESP
      INTEGER      NUMCH, IDIMSP

      REAL         SAVE(8192)
      COMMON /SAV/ SAVE
      INTEGER*4    ISAV(8192)
      EQUIVALENCE (ISAV,SAVE)

      CHARACTER*40 ANS, FILNAM /' '/
      LOGICAL      NOT_INT
      INTEGER      IHED(32), NDX(4)/1,3*0/, MAXH/64/
      CHARACTER*80 TMPFN /' '/
      INTEGER*4    ITMPFN(20)
      EQUIVALENCE (TMPFN,ITMPFN)

      COMMON /LUS/ IR,IW,IP,IG

      COMMON /LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF


C        SPKIO error handling initialization....
      DO I=1,28            ! blank message buffer....
         MSSG(I) = '   '
      ENDDO
      NAMPROG(1)='GF2 '    ! 1st half of program name....
      NAMPROG(2)='    '    ! 2nd half of program name....
      LOGUT = IW           ! logical unit for: terminal....
      LOGUP = IP           !                  log/print file....
      LISFLG = 'LOF'       ! no log file output....
      LUS = 1              ! SPK logical unit....

      NC=40
      CALL ININ_FLG(FN,NC,ID,IN2,IN3,NOT_INT)
      IF (.NOT. NOT_INT) THEN
         TMPFN(1:40) = FILNAM
         CALL SPKMAN('OPEN',ITMPFN,LUS,'RO  ',IERR)
         IF (IERR.NE.0) GO TO 800
         GO TO 100
      ENDIF

C         FN = file name....
C         ....open .spk file and ask "display directory?"....

      TMPFN(1:40) = FN
      CALL SPKMAN('OPEN',ITMPFN,LUS,'RO  ',IERR)
      IF (IERR.NE.0) GO TO 800
      FILNAM = FN

      CALL ASKYN(24HDisplay directory? (Y/N),24,&80)

C         list file directory....
      READ(UNIT=1,REC=1,ERR=850) (ISAV(JJ),JJ=1,512)
      NS=ISAV(3)
      IF (NS.GT.254) NS=254
      DO 50 I1=1,NS,10
         I2=I1+9
         IF (I2.GT.NS) I2=NS
         WRITE(IW,40) (ISAV(2*I+3),I=I1,I2)
40       FORMAT(' ',10I8)
50    CONTINUE

C         ask for spectrum ID....

80    CALL ASK(15HSpectrum ID = ?,15,ANS,NC)
      IF (NC.EQ.0) GO TO 900
      CALL ININ(ANS,NC,ID,IN2,IN3,&80)
100   IF (ID.LT.1) GO TO 850

C        get number of chs in spectrum....

      LU=1
      CALL SPKIO(1,LU,ID,IHED,MAXH,ISAV,NDX,0,IERR)
      IF (IERR.NE.0) GO TO 850
      NUMCH=IHED(12)
      IF (NUMCH.GT.IDIMSP) THEN
        NUMCH=IDIMSP
        WRITE(IW,60)IDIMSP
60      FORMAT(' First',I6,' chs only taken.')
      ENDIF

C          read spectrum....

      CALL SPKIO(1,LU,ID,IHED,MAXH,ISAV,NDX,NUMCH,IERR)
      IF (IERR.NE.0) GO TO 850

      CLOSE(1)

C       convert to real format....

      DO 250 I=1,NUMCH
         SP(I)=FLOAT(ISAV(I))
250   CONTINUE

      FN=FILNAM
      WRITE (NAMESP,'(I8)')ID
      RETURN

C             error messages....

800   WRITE(IW,*) 'File does not exist.'
      GO TO 900
850   WRITE(IW,*) 'Cannot read file.'
900   CLOSE(1,ERR=910)
910   RETURN 1
      END
