
C======================================================================

      SUBROUTINE SETEXT(FILNAM,CEXT,IEXT)

C           set default extension of filename FILNAM to CEXT...
C           leading spaces are first removed from FILNAM....
C           if extension is present, it is left unchanged....
C           if no extension is present, CEXT is used....
C           IEXT is a returned pointer to the dot of the .EXT....
C           CEXT should include the dot plus a three-letter extension....

      CHARACTER*40 FILNAM,FN
      CHARACTER*4  CEXT
      INTEGER      IEXT

C       remove leading spaces from FILNAM....

      FN=FILNAM
      DO 10 I=1,40
         IF (FN(1:1).NE.' ') GO TO 20
         FN(1:39)=FILNAM(2:40)
         FN(40:40)=' '
         FILNAM=FN
10    CONTINUE

C       look for file extension in FILNAM....
C            ....if there is none, put it to CEXT....

20    DO 30 ISP=1,36
         IF (FILNAM(ISP:ISP).EQ.' ') GO TO 40
30    CONTINUE
      ISP=37
40    DO 50 I=ISP-1,1,-1
         IF (FILNAM(I:I).EQ.']' .OR. FILNAM(I:I).EQ.':') GO TO 60
         IF (FILNAM(I:I).EQ.'.') GO TO 70
50    CONTINUE
60    I=ISP
      FILNAM(I:I+3)=CEXT
70    IEXT=I

      RETURN
      END
