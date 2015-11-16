      SUBROUTINE LOOKUP(ANS,K,WINMOD)

C          open old or create new look-up table file....
C                   (file name stored in ANS)....
C          default file name = .tab....

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      INTEGER WINMOD
C         WINMOD = 0 : no mode defined....
C         WINMOD = 1 : look-up file mode....
C         WINMOD = 2 : slice file mode....

      INTEGER*2 LOOKTAB(8192)
      INTEGER   NCLOOK,LOOKMIN,LOOKMAX
      COMMON /LOOK/ NCLOOK,LOOKMIN,LOOKMAX,LOOKTAB

      CHARACTER*40 ANS
      CHARACTER*45 CMESS
      COMMON /LUS/ IR,IW,IP,IG


      ANS(1:2)='  '
      IF (K.GE.3) GO TO 15

C          ask for output file name....

10    CALL ASK(45HName of lookup file = ? (default .EXT = .tab),45,
     +         ANS,K)
      IF (K.EQ.0) RETURN

C         save any files already created....

15    IF (WINMOD.EQ.1) CALL WRTLOOK
      IF (WINMOD.NE.0) CLOSE(13,ERR=20)
20    WINMOD=1

      CALL SETEXT(ANS,'.tab',J)

C            try to open OLD output file....

      OPEN (13,FILE=ANS,FORM='UNFORMATTED',STATUS='OLD',ERR=200)
      READ (13,ERR=800) NCLOOK,LOOKMIN,LOOKMAX
      IF (NCLOOK.LT.2.OR.NCLOOK.GT.8192) GO TO 800
      READ (13,ERR=800) (LOOKTAB(I),I=1,NCLOOK)

      CALL ASKYN(27HModify existing file? (Y/N),27,&100)
      RETURN

100   CLOSE (13)
      GO TO 10

C        open NEW output file....

200   OPEN (13,FILE=ANS,FORM='UNFORMATTED',STATUS='NEW',ERR=800)

C        ask for dimension of look-up table....
C           ....(default = spectrum dimension)....

      NCLOOK=MAXCH+1
      WRITE(CMESS,220) NCLOOK
220   FORMAT('Dimension of look-up table = ? (rtn for',I5,')')
250   CALL CASK(CMESS,ANS,K)
      IF (K.NE.0) THEN
         CALL ININ(ANS,K,N,J1,J2,&250)
         IF (N.GT.8192.OR.N.LT.2) GO TO 250
         NCLOOK=N
      ENDIF

C         initialize look-up table....

      LOOKMIN=0
      LOOKMAX=0
      DO 280 I=1,NCLOOK
         LOOKTAB(I)=0
280   CONTINUE

      RETURN

C           error messages....

800   WRITE(IW,*) 'Error: cannot open file.'
      WINMOD=0
      RETURN
      END
