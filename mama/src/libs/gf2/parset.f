      SUBROUTINE PARSET(MODE)

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      REAL          AREAS(15),DAREAS(15),CENTS(15)
      COMMON /AREA/ AREAS,DAREAS,CENTS

      REAL           FINEST(5), SWPARS(3)
      INTEGER        INFIX(3), INFIXRW, INFIXW
      COMMON /INEST/ FINEST,INFIX,SWPARS,INFIXRW,INFIXW

      LOGICAL RESETP,RESETW

      ILO=MCH(1)+1
      IHI=MCH(2)+1
      X0=(IHI+ILO)/2
      DO 5 I=1,NPARS
    5    ERRS(I)=0.0
      X=0.0
      DO 20 I=1,NPKS
20       X=X+PPOS(I)
      X=X/FLOAT(NPKS)
      IF (IFIXED(1).EQ.1) PARS(1)=(rSPEC(IDEST,IHI-1)+rSPEC(IDEST,ILO-1))/2.0
      IF (IFIXED(2).EQ.1) PARS(2)=(rSPEC(IDEST,IHI-1)-rSPEC(IDEST,ILO-1))/FLOAT(IHI-ILO)
      IF (IFIXED(3).EQ.1) PARS(3)=0.0
      IF (IFIXED(4).EQ.1) PARS(4)=FINEST(1)+FINEST(2)*X
      IF (IFIXED(5).EQ.1) PARS(5)=FINEST(3)+FINEST(4)*X
      IF (PARS(5).EQ.0.0) PARS(5)=0.5*SQRT
     +                       ( SWPARS(1)
     +                       + SWPARS(2)*X
     +                       + SWPARS(3)*X*X )
      IF (IFIXED(6).EQ.1) PARS(6)=FINEST(5)
      IF (MODE.LT.0) THEN

C            come here only during set-up....
         RESETP=.TRUE.
         RESETW=.TRUE.
         NFP=3-INFIX(1)-INFIX(2)-INFIX(3)
         IFIXED(4)=INFIX(1)
         IFIXED(5)=INFIX(2)
         IFIXED(6)=INFIX(3)
         IF (IFIXED(4).EQ.0.AND.PARS(4).EQ.0.0) THEN
            NFP=NFP+IFIXED(5)
            IFIXED(5)=0
         ENDIF
         IRELW=INFIXRW
         IF (INFIXW.EQ.0) THEN
            DO I=1,NPKS
              IFIXED(3*I+5)=0         !magne
            ENDDO
            NFP=NFP+NPKS
         ENDIF
      ELSE
         IF (IRELPOS.EQ.0) THEN
            RESETP=.FALSE.
            CALL ASKYN(
     +       54HRelative positions fixed - reset peak positions? (Y/N)
     +      ,54,*120)
         ENDIF
         RESETP=.TRUE.
120      IF (IRELW.EQ.0) THEN
            RESETW=.FALSE.
            CALL ASKYN(
     +         43HRelative widths fixed - reset widths? (Y/N),43,*140)
         ENDIF
         RESETW=.TRUE.
      ENDIF
140   DO 10 I=1,NPKS
         AREAS(I)=0.0
         DAREAS(I)=0.0
         J=3*I+6
         IF (RESETP.AND.IFIXED(J-2).EQ.1) PARS(J-2)=PPOS(I)
         IF (MODE.LT.0.OR.(RESETW.AND.IFIXED(J-1).EQ.1))
     +              PARS(J-1)=SQRT( SWPARS(1)
     +                            + SWPARS(2)*PPOS(I)
     +                            + SWPARS(3)*PPOS(I)*PPOS(I) )
         IF (IFIXED(J).EQ.0) GO TO 10
         X=PPOS(I)-X0+1.0
         Y=PARS(1)+PARS(2)*X
         IPP=PPOS(I)+1.5
         PARS(J)=rSPEC(IDEST,IPP-1)-Y
         CENTS(I)=PARS(J-2)
	 
c	 write(6,*)"values of fit",i, ipp, pars(j), cents(i)
	 
10    CONTINUE

      RETURN
      END
