      SUBROUTINE ADDDELPK(MODE,NPEAK,READY)

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      REAL          AREAS(15),DAREAS(15),CENTS(15)
      COMMON /AREA/ AREAS,DAREAS,CENTS

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT

      REAL           FINEST(5), SWPARS(3)
      INTEGER        INFIX(3), INFIXRW, INFIXW
      COMMON /INEST/ FINEST,INFIX,SWPARS,INFIXRW,INFIXW

      LOGICAL      READY
      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG

      itry=0

      IF ((.NOT.READY) .OR. NPKS.LE.0) THEN
         WRITE(IW,*) 'Cannot - no fit defined.'
         RETURN
      ENDIF

      IF (MODE.EQ.1) THEN

C                  AP; add peak to fit....
         IF (NPKS.GE.15) THEN
            WRITE(IW,*) 'Cannot - too many peaks.'
            RETURN
         ENDIF

C            ask for peak position....
30       N=NPKS+1
         IF (.NOT.DISP) GO TO 50
         WRITE(IW,*) ' New peak position?  (hit T to type, A to abort)'
         CALL RETIC(X,Y,ANS)
         IF (ANS(1:1).EQ.'A' .OR. ANS(1:1).EQ.'a') RETURN
         IF (ANS(1:1).NE.'T' .AND. ANS(1:1).NE.'t') GO TO 60
C            ....hit t for type....
c50       CALL ASK(15HPeak position=?,15,ANS,K)
50       CALL CASK('Peak position = ?',ANS,K)

         CALL FFIN(ANS,K,PPOS(N),RJ1,RJ2,*50)
         GO TO 70
60       PPOS(N)=X-0.5
70       IF (IFIX(PPOS(N)).LT.MCH(1)) GO TO 80
         IF (IFIX(PPOS(N)).LT.MCH(2)) GO TO 90
80       WRITE(IW,*) 'Peaks must be within limits - try again'
                  itry=itry+1
                  IF(itry.GE.3)THEN
                    WRITE(6,*)'Sorry, not your day today'
                    RETURN
                  ENDIF
         GO TO 30
90       IF (DISP) CALL DSPMKR(N+2)

         NPKS=NPKS+1
         NPARS=NPARS+3
         AREAS(NPKS)=0.0
         DAREAS(NPKS)=0.0
         CENTS(NPKS)=PPOS(NPKS)
         DO 120 I=NPARS-2,NPARS
            ERRS(I)=0.0
            IFIXED(I)=1
120      CONTINUE
         IFIXED(NPARS-1)=INFIXW
         NFP=NFP+1-INFIXW

         ILO=MCH(1)+1
         IHI=MCH(2)+1
         X0=(IHI+ILO)/2
         PARS(NPARS-2)=PPOS(NPKS)
         PARS(NPARS-1)=SQRT( SWPARS(1)
     +                     + SWPARS(2)*PPOS(NPKS)
     +                     + SWPARS(3)*PPOS(NPKS)*PPOS(NPKS) )
         X=PPOS(NPKS)-X0+1.0
         Y=PARS(1)+PARS(2)*X+PARS(3)*X*X
         IPP=PPOS(NPKS)+1.5
         PARS(NPARS)=rSPEC(IDEST,IPP-1)-Y

         IF (INFIXW.EQ.1.AND.IRELW.EQ.0) THEN
c             CALL ASKYN(38HReset all non-fixed peak widths? (Y/N)
c     +                 ,38,*140)
             CALL CASKYN('Reset all non-fixed peak widths? (Y/N)',
     +                    IALT_RET)
             IF (IALT_RET.EQ.1) GO TO 140
             DO 130 I=1,NPKS-1
                J=3*I+5
                IF (IFIXED(J).EQ.1) PARS(J) =
     +                  SQRT( SWPARS(1)
     +                     + SWPARS(2)*PPOS(I)
     +                     + SWPARS(3)*PPOS(I)*PPOS(I) )
130          CONTINUE
         ENDIF

140      IF (CENTS(NPKS).GE.CENTS(NPKS-1)) GO TO 1000
c         CALL ASKYN(40HRe-order peaks in order of energy? (Y/N)
c     +             ,40,*1000)
         CALL CASKYN('Re-order peaks in order of energy? (Y/N)',
     +               IALT_RET)
         IF (IALT_RET.EQ.1) GO TO 1000

         DO 180 I=1,NPKS-1
            DO 170 J=I+1,NPKS
               IF (CENTS(I).GT.CENTS(J)) THEN
                  S=PPOS(I)
                  PPOS(I)=PPOS(J)
                  PPOS(J)=S
                  S=AREAS(I)
                  AREAS(I)=AREAS(J)
                  AREAS(J)=S
                  S=DAREAS(I)
                  DAREAS(I)=DAREAS(J)
                  DAREAS(J)=S
                  S=CENTS(I)
                  CENTS(I)=CENTS(J)
                  CENTS(J)=S
                  DO 150 K=3*I+4,3*I+6
                     L=3*(J-I)+K
                     S=PARS(K)
                     PARS(K)=PARS(L)
                     PARS(L)=S
                     S=ERRS(K)
                     ERRS(K)=ERRS(L)
                     ERRS(L)=S
                     JS=IFIXED(K)
                     IFIXED(K)=IFIXED(L)
                     IFIXED(L)=JS
150               CONTINUE
               ENDIF
170         CONTINUE
180      CONTINUE

      ELSE

C                  DP; delete peak from fit....
         IF (NPKS.LE.1) THEN
            WRITE(IW,*) 'Cannot - too few peaks.'
            RETURN
         ENDIF
230      IF (NPEAK.LE.0.OR.NPEAK.GT.NPKS) THEN
c240         CALL ASK(32HNumber of peak to be deleted = ?,32,ANS,NC)
240         CALL CASK('Number of peak to be deleted = ?',ANS,NC)
            IF (NC.EQ.0) RETURN
c            CALL ININ(ANS,NC,NPEAK,J,J2,*240)
            CALL ININ(ANS,NC,NPEAK,J,J2,IALT_RET)
            IF (IALT_RET.EQ.1) GO TO 240
            GO TO 230
         ENDIF

         NPARS=NPARS-3
         J=3*NPEAK+4
         NFP=NFP-3+IFIXED(J)+IFIXED(J+1)+IFIXED(J+2)
         IF (NPEAK.NE.NPKS) THEN
            DO 260 I=NPEAK,NPKS-1
               PPOS(I)=PPOS(I+1)
               AREAS(I)=AREAS(I+1)
               DAREAS(I)=DAREAS(I+1)
               CENTS(I)=CENTS(I+1)
260         CONTINUE
            DO 280 I=J,NPARS
               PARS(I)=PARS(I+3)
               ERRS(I)=ERRS(I+3)
               IFIXED(I)=IFIXED(I+3)
280         CONTINUE
         ENDIF
         NPKS=NPKS-1
      ENDIF

1000  RETURN
      END
