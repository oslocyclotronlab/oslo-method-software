      SUBROUTINE TYPEIT(IMODE)

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      REAL          PARS(51), ERRS(51)
      INTEGER       IFIXED(51), NPARS, NFP, NPKS, IRELW, IRELPOS
      COMMON /PARA/ IRELW,PARS,IFIXED,NPARS,NFP,ERRS,NPKS,IRELPOS

      REAL          AREAS(15),DAREAS(15),CENTS(15)
      COMMON /AREA/ AREAS,DAREAS,CENTS

      REAL*8         GAIN(6)
      INTEGER        ICAL, NTERMS
      COMMON /CALIB/ GAIN, ICAL, NTERMS

      CHARACTER*40 ANS
      COMMON /LUS/ IR,IW,IP,IG
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER*20 DATTIM

      IF (IMODE.GE.3) GO TO 55

      IF (IMODE.LE.1) THEN
         WRITE(IW,10,ERR=25)(MCH(I),I=1,2),(PPOS(I),I=1,NPKS)
10       FORMAT(' Mkr chs: limits ',2I5/12X,'Peaks ',8F8.2/18X,7F8.2)
         IPAGE = 4 - (NPKS)/8
      ELSE
         IPAGE = 6
      ENDIF

25    WRITE(IW,30,ERR=31)(PARS(I),ERRS(I),I=1,6)
30    FORMAT('Background and shape parameters:',/,
     +'A=',F10.2,'+-',F8.2,' B=',F9.4,'+-',F8.4,
     +' C=',F9.4,'+-',F8.4,/,
     +'R=',F9.2,'+-',F8.2,' Beta=',F9.3,'+-',F8.3,
     +' Step=',F6.2,'+-',F6.2)

31    IF (ICAL.EQ.0) WRITE(IW,35)
      IF (ICAL.NE.0) WRITE(IW,36)
35    FORMAT('Peak     Width           Area     Centroid')
36    FORMAT('Peak     Width           Area     Centroid',
     +'   Energy(keV)  Fwhm(keV)')
      DO 50 I=1,NPKS
         K=3*I+3
         CALL ENERGY(CENTS(I),ERRS(K+1),EG,DEG,&47)
         EFW=PARS(2+K)*(GAIN(2)+2.0*GAIN(3)*CENTS(I))
         WRITE(IW,45,ERR=49) I,(PARS(J+K),ERRS(J+K),J=2,2),
     +   IFIX(AREAS(I)),IFIX(DAREAS(I)),CENTS(I),EG,ABS(DEG),ABS(EFW)
45       FORMAT(I2,F7.2,'(',F6.2,')',
     +   I9,'(',I5,')',F8.2,F9.2,'(',F6.2,')',F7.1)
         GO TO 49
47       WRITE(IW,45,ERR=49) I,(PARS(J+K),ERRS(J+K),J=2,2),
     +              IFIX(AREAS(I)),IFIX(DAREAS(I)),CENTS(I)
49       IF (I.EQ.IPAGE.AND.I.LT.NPKS) THEN
c            CALL ASK2(28HPress any key to continue...,28,ANS,K,1)
            IPAGE = IPAGE + 9
         ENDIF
50    CONTINUE
      RETURN
C Open file to write results of peak fits     
55    OPEN(IP,FILE='peakfit.out',ACCESS='APPEND',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE(IW,*) 'Cannot open new file ',ANS
         CLOSE(IP,IOSTAT=IOS)
         RETURN       
      ENDIF
      CALL DATETIME(DATTIM)
      WRITE(IP,56)
56    FORMAT(' ')
      WRITE(IP,33,ERR=57),fname(2,IDEST),DATTIM(1:12),(PARS(I),ERRS(I),I=1,6)
33    FORMAT('Background and shape parameters for ',A,' at ',A,/,
     +'A=',F10.2,'+-',F8.2,' B=',F9.4,'+-',F8.4,
     +' C=',F9.4,'+-',F8.4,/,
     +'R=',F9.2,'+-',F8.2,' Beta=',F9.3,'+-',F8.3,
     +' Step=',F6.2,'+-',F6.2)

57    IF (ICAL.EQ.0) WRITE(IP,35)
      IF (ICAL.NE.0) WRITE(IP,36)
      DO 60 I=1,NPKS
         K=3*I+3
         EFW=PARS(2+K)*(GAIN(2)+2.0*GAIN(3)*CENTS(I))
         CALL ENERGY(CENTS(I),ERRS(K+1),EG,DEG,&59)
         WRITE(IP,45,ERR=60) I,(PARS(J+K),ERRS(J+K),J=2,2),
     +           IFIX(AREAS(I)),IFIX(DAREAS(I)),CENTS(I),EG,DEG,EFW
         GO TO 60
59       WRITE(IP,45,ERR=60) I,(PARS(J+K),ERRS(J+K),J=2,2),
     +           IFIX(AREAS(I)),IFIX(DAREAS(I)),CENTS(I)
60    CONTINUE
      CLOSE(IP)
      RETURN
      END
