
C=======================================================================

      SUBROUTINE SLICE(ANS,K,WINMOD)

C          open old or create new slice input file....
C                   (file name stored in ANS)....
C          default file name = .win....

      INTEGER WINMOD
C         WINMOD = 0 : no mode defined....
C         WINMOD = 1 : look-up file mode....
C         WINMOD = 2 : slice file mode....

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG


      ANS(1:2)='  '
      IF (K.GE.3) GO TO 15

C          ask for output file name....

10    CALL ASK(45HName of window file = ? (default .EXT = .win),45,
     +         ANS,K)
      IF (K.EQ.0) RETURN

C         save any files already created....

15    IF (WINMOD.EQ.1) CALL WRTLOOK
      IF (WINMOD.NE.0) CLOSE(13,ERR=20)
20    WINMOD=2
      CALL SETEXT(ANS,'.win',J)

C            try to open OLD output file....

      OPEN (13,FILE=ANS,STATUS='OLD',ERR=200)

      CALL ASKYN(28H Add to existing file? (Y/N),28,&100)
50    READ (13,'(A1)',END=70) J
      GO TO 50
70    RETURN

100   CLOSE (13)
      GO TO 10

C        open NEW output file....

200   OPEN (13,FILE=ANS,STATUS='NEW',ERR=800)
      RETURN

C           error messages....

800   WRITE(IW,*) 'Error: cannot open file.'
      WINMOD=0
      RETURN
      END
