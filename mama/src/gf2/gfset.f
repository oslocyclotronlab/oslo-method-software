
C=======================================================================

      SUBROUTINE GFSET

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG

C       ask for limits for fit....
      itry=0
10    WRITE(IW,*) '  Limits for fit?  (hit T to type)'
      DO 70 N=1,2
         IF (.NOT.DISP) GO TO 30
         CALL RETIC(X,Y,ANS)
         IF (ANS(1:1).NE.'T' .AND. ANS(1:1).NE.'t') GO TO 60
30       CALL CASK('Limit = ?',ANS,K)
         CALL FFIN(ANS,K,CH,RJ1,RJ2,IALT_RET)
         IF (IALT_RET.EQ.1) GO TO 30
         MCH(N)=CH+0.5
         IF (MCH(N).GT.MAXCH) GO TO 40
         IF (MCH(N).GE.0) GO TO 70
40       WRITE(IW,*) 'Marker ch. outside spectrum - try again.'
         GO TO 30
60       MCH(N)=X
70    CONTINUE
      IF (MCH(2).GT.MCH(1)) GO TO 80
      LO=MCH(2)
      MCH(2)=MCH(1)
      MCH(1)=LO
80    IF (.NOT.DISP) GO TO 90
      CALL DSPMKR(1)
      CALL DSPMKR(2)

C       ask for peak positions....

90    IF (NPKS.LE.15) THEN
         WRITE(IW,*)' Peak positions? (hit T to type, R to restart)'
         DO 200 N=1,NPKS
110         IF (DISP) THEN
               CALL RETIC(X,Y,ANS)
               IF (ANS(1:1).EQ.'R' .OR. ANS(1:1).EQ.'r') GO TO 10
               PPOS(N)=X-0.5
            ENDIF
            IF (.NOT.DISP .OR.
     +          ANS(1:1).EQ.'T' .OR. ANS(1:1).EQ.'t') THEN
C               ....hit t for type....
120            CALL CASK('Peak position = ?',ANS,K)
               IF (ANS(1:1).EQ.'R' .OR. ANS(1:1).EQ.'r') GO TO 10
               CALL FFIN(ANS,K,PPOS(N),RJ1,RJ2,IALT_RET)
               IF (IALT_RET.EQ.1) GO TO 120
            ENDIF

            IF (IFIX(PPOS(N)).LT.MCH(1) .OR. 
     +          IFIX(PPOS(N)).GE.MCH(2)) THEN
               WRITE(IW,*)' Peaks must be within limits - try again.'
               itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF
               GO TO 110
            ENDIF
            IF (DISP) CALL DSPMKR(N+2)
200      CONTINUE

      ELSE
         WRITE(IW,*)
     +     ' Peak positions? (hit X when done, T to type, R to restart)'
         DO 300 N=1,15
210         IF (DISP) THEN
               CALL RETIC(X,Y,ANS)
               IF (ANS(1:1).EQ.'X' .OR. ANS(1:1).EQ.'x') GO TO 310
               IF (ANS(1:1).EQ.'R' .OR. ANS(1:1).EQ.'r') GO TO 10
               PPOS(N)=X-0.5
            ENDIF
            IF (.NOT.DISP .OR.
     +          ANS(1:1).EQ.'T' .OR. ANS(1:1).EQ.'t') THEN
C               ....hit t for type....
220            CALL CASK('Peak position=? (rtn when done)',ANS,K)
               IF (K.EQ.0) GO TO 310
               CALL FFIN(ANS,K,PPOS(N),RJ1,RJ2,IALT_RET)
               IF (IALT_RET.EQ.1) GO TO 220
            ENDIF

            IF (IFIX(PPOS(N)).LT.MCH(1) .OR. 
     +          IFIX(PPOS(N)).GE.MCH(2)) THEN
               WRITE(IW,*)' Peaks must be within limits - try again.'
               itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF
               GO TO 210
            ENDIF
            IF (DISP) CALL DSPMKR(N+2)
300      CONTINUE
310      NPKS = N - 1
         IF (NPKS.LE.0) RETURN
      ENDIF

C       ask for fixed pars....

      NPARS=3*(NPKS+2)
      DO I=1,NPARS
         IFIXED(I)=1
      ENDDO
      IRELPOS=1
      NFP=0
      CALL PARSET(-1)
      ANS = 'FX'
      NC=2
      CALL FIXORFREE(ANS,NC)
      RETURN
      END
