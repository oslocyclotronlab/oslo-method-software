
C=======================================================================

      SUBROUTINE PARA2NUM(ANS,PARAM,ERR_FLG)

C     change an alphanumeric ANSwer into PARAM (parameter number)....
C     ERR_FLG = true for unrecognized parameter....
C     called by FIXORFREE.... 

      CHARACTER*40 ANS,TMP_ANS
      INTEGER      PARAM
      LOGICAL      ERR_FLG

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      COMMON /LUS/ IR,IW,IP,IG

C        local variables....
      CHARACTER*3    PARC(51)
     +           /' A ',' B ',' C ',' R ','BTA','STP','P1 ','W1 ','H1 ',
     +            'P2 ','W2 ','H2 ','P3 ','W3 ','H3 ','P4 ','W4 ','H4 ',
     +            'P5 ','W5 ','H5 ','P6 ','W6 ','H6 ','P7 ','W7 ','H7 ',
     +            'P8 ','W8 ','H8 ','P9 ','W9 ','H9 ','PA ','WA ','HA ',
     +            'PB ','WB ','HB ','PC ','WC ','HC ','PD ','WD ','HD ',
     +            'PE ','WE ','HE ','PF ','WF ','HF '/


      TMP_ANS = ANS
C        remove leading spaces....
140   IF (TMP_ANS(1:1).EQ.' ') THEN
         TMP_ANS(1:39) = TMP_ANS(2:40)
         TMP_ANS(40:40) = ' '
         GO TO 140
      ENDIF

C         convert lower case to upper case characters....
      DO 150 I = 1,4
         IC = ICHAR(TMP_ANS(I:I))
         IF (IC.GE.97.AND.IC.LE.122) TMP_ANS(I:I) = CHAR(IC-32)
150   CONTINUE

      ERR_FLG = .FALSE.
      DO 160 PARAM = 1, 4
C           put a space in front of possible one letter answer....
         IF (' '//TMP_ANS(1:2).EQ.PARC(PARAM)) RETURN
160   CONTINUE

      DO 170 PARAM = 5, NPARS
         IF (TMP_ANS(1:3).EQ.PARC(PARAM)) RETURN
170   CONTINUE

      IF (TMP_ANS(1:4).EQ.'BETA') THEN
         PARAM = 5
      ELSEIF (TMP_ANS(1:4).EQ.'STEP') THEN
         PARAM = 6
      ELSEIF (TMP_ANS(1:3).EQ.'RP ') THEN
         PARAM = 101
      ELSEIF (TMP_ANS(1:3).EQ.'RW ') THEN
         PARAM = 102
      ELSE
C           no match so ....
         ERR_FLG = .TRUE.
      ENDIF

      RETURN
      END
