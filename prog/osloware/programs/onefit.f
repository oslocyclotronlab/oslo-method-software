      PROGRAM FUNCFIT
      EXTERNAL FUNC
      COMMON/nucleus/xN,xZ,xA,beta
      DIMENSION YFIT(1000)
      DIMENSION X(1000),Y(1000),SIGMAY(1000),
     1A(12),DELTAA(12),SIGMAA(12),CH(1010), Abest(12),SIGMAAbest(12)

      OPEN(23,FILE='strffit.dat',STATUS='old',ERR=666)
      READ(23,*,ERR=666)NA,NPTS
      NA=5
      DO i=1,NPTS
        READ(23,*,ERR=666)X(i),Y(i),SIGMAY(i)
      ENDDO
      CLOSE(23)
      WRITE(6,*)'Default values taken from file: strffit.dat'

C NB, made to fit these data. Making log, and then back again alog
      DO i=1,NPTS
        SIGMAY(i)=SIGMAY(i)/Y(i)
        Y(i)=log(Y(i))
      ENDDO


      GO TO 777
 666  NA=2
      NPTS=9
      DO i=1,NPTS
        X(i)=10.+(I-1.)*20.
        Y(i)=1000.
        SIGMAY(i)=SQRT(1000.)
      ENDDO


      WRITE(6,*)''
      WRITE(6,*)'Give the experimental X-, Y- and Ysig-values'
      WRITE(6,*)'for the number of datapoints chosen'

      WRITE(6,*)' '
      WRITE(6,*)' '
      WRITE(6,*)'Type the X-values:'
      DO i=1,NPTS
        WRITE(6,41)i,X(i)
 41     FORMAT(' Type X(',I2,')      <',F12.4,'>:',$)
        CALL READF(5,X(i))
      ENDDO
      WRITE(6,*)CHAR(7)

      WRITE(6,*)' '
      WRITE(6,*)'Type the Y-values:'
      DO i=1,NPTS
        WRITE(6,42)i,Y(i)
 42     FORMAT(' Type Y(',I2,')      <',F12.4,'>:',$)
        CALL READF(5,Y(i))
      ENDDO
      WRITE(6,*)CHAR(7)

      WRITE(6,*)' '
      WRITE(6,*)'Type the Ysig-values:'
      DO i=1,NPTS
        WRITE(6,43)i,SIGMAY(i)
 43     FORMAT(' Type Ysig(',I2,')   <',F12.4,'>:',$)
        CALL READF(5,SIGMAY(i))
      ENDDO
      WRITE(6,*)CHAR(7)
      WRITE(6,*)' '

c      OPEN(23,FILE='onefit.dat',ERR=555)
c      WRITE(23,*,ERR=555)NA,NPTS
c      DO i=1,NPTS
c        WRITE(23,*,ERR=555)X(i),Y(i),SIGMAY(i)
c      ENDDO
c     CLOSE(23)
c 555  CONTINUE



 777  CONTINUE

      WRITE(6,62)NA
  62  FORMAT(/' Number of parameters to be fitted (1,2,..,12) <',I3,'>:',$)
      CALL READI(5,NA)

      WRITE(6,63)NPTS
  63  FORMAT( ' Number of datapoints                          <',I3,'>:',$)
      CALL READI(5,NPTS)

C Calculates resonance parameters from systematics of RIPL, p.102
      xN=31
      xZ=26
      xA=57
      pi=3.14159
      ee1=31.2*xA**(-0.333)+20.6*xA**(-0.167)
      ge1=0.026*ee1**1.91
      se1=1.2*120.*xN*xZ/(xA*pi*ge1)
      em1=41*xA**(-0.33333)
      gm1=4.0
      sm1=1.0E-09*1.58*xA**(0.47)*((7.0**2-em1**2)**2+7.0**2*gm1**2)/(8.6E-08*7.0*gm1**2)


C     ESTIMATE PARAMETERS

      A(1)=1.
      A(2)=1.33

      DO I=1,12
         DELTAA(I)=ABS(A(I)/10.)
      ENDDO
C
C     PREPARATION FOR FIT
C
      PROG=0.000002
      MODE=1
      M=0

      CHISQR=999999.
      BEST  =999999.
      NBAD  =0
C
C     STARTING FIT  ***********************************************
C
      IF(NA.EQ.1)WRITE(6,78)
      IF(NA.EQ.2)WRITE(6,79)
      IF(NA.EQ.3)WRITE(6,80)

  78  FORMAT(' Loop    Chi**2        A(1)')
  79  FORMAT(' Loop    Chi**2        A(1)         A(2)')
  80  FORMAT(' Loop    Chi**2        A(1)         A(2)     ...')

      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
 505  CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A
     1,DELTAA,SIGMAA,NA,YFIT,CHISQR)
      M=M+1
      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
  81  FORMAT(1X,I3,F10.4,8(1X,E12.5))
      CH(M)=CHISQR
      IF(M.LT.2) GO TO 505
      VER=ABS(CH(M)-CH(M-1))/CH(M-1)
      IF(CHISQR.LT.BEST)THEN
        Mbest=M
        BEST=CHISQR
        DO i=1,12
           Abest(i)=A(i)
           SIGMAAbest(i)=SIGMAA(i)
        ENDDO
      ENDIF
      IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
      IF(NBAD.GT.6.OR.M.GE.500)GO TO 506
      IF(M.GT.1000)GO TO 506
      IF(VER.GT.PROG) GO TO 505
 506  CONTINUE   
C
C     FIT IS FINISHED *********************************************
C
      DO i=1,12
         A(i)=Abest(i)
         SIGMAA(i)=SIGMAAbest(i)
      ENDDO

      WRITE(6,*)' '
      WRITE(6,*)'          X(i)            Y(i)          Yfit(i)'
      DO I=1,NPTS
        WRITE(6,44)X(I),Y(I),YFIT(I)
44      FORMAT(3F16.4)
      ENDDO


      WRITE(6,82)Mbest,BEST
  82  FORMAT(' Final result from fitting loop',I4,' with Chi**2 = ',E10.4)
      WRITE(6,83)  A(1),SIGMAA(1)
      WRITE(6,84)  A(2),SIGMAA(2)


  83  FORMAT(' A(1) K  = ',E12.5,' +/- ',E12.5)
  84  FORMAT(' A(2) T  = ',E12.5,' +/- ',E12.5)


      OPEN(24,FILE='oneresults.dat',ACCESS='SEQUENTIAL',ERR=555)
      WRITE(24,*)'Resonance parameters (RIPL p.102) for A=',INT(xA),'  Z=',INT(xZ),'  with beta=',beta
      WRITE(24,*)'GEDR #1 parameters: E=',ee1,'MeV  S=',se1,'mb  G=',ge1,'MeV'
      WRITE(24,*)'GEDR #2 parameters: E=',ee2,'MeV  S=',se2,'mb  G=',ge2,'MeV'
      WRITE(24,*)'GMDR #1 parameters: E=',em1,'MeV  S=',sm1,'mb  G=',gm1,'MeV'
      WRITE(24,*)' '
      WRITE(24,*)'Fitting', NPTS,' data points with', NA,' parameters'
      WRITE(24,82)Mbest,BEST
      WRITE(24,83)  A(1),SIGMAA(1)
      WRITE(24,84)  A(2),SIGMAA(2)

      CLOSE(24)

C Writing to paw file for plotting
      OPEN(24,FILE='fitted.paw',ACCESS='SEQUENTIAL',ERR=555)

      pi=3.14159
     
      xK =A(1)
      T  =A(2)

      DO i=1,400
         e=i*0.1            ! up to 40 MeV

         xlow = ABS(15.1**2 - 15.2**2)
         diff1 = MAX(xlow,ABS(e**2 - ee1**2))
         diff2 = MAX(xlow,ABS(e**2 - ee2**2))
         fe1=(8.6E-08) * xK * (
     1   (0.7*se1*ge1**2*(e**2+4*pi**2*T**2))/(ee1*(diff1)**2) +
     2   (0.7*se2*ge2**2*(e**2+4*pi**2*T**2))/(ee2*(diff2)**2)  )

         fm1=(8.6E-08) * xK *
     3   (sm1*e*gm1**2)/((e**2-em1**2)**2+e**2*gm1**2)

         fpy1=(8.6E-08) * (   
     4   (sp1*e*gp1**2)/((e**2-ep1**2)**2+e**2*gp1**2) )

         fpy2=(8.6E-08) * (   
     5   (sp2*e*gp2**2)/((e**2-ep2**2)**2+e**2*gp2**2) )

         WRITE(24,*) e, EXP(FUNC(e,A)),fe1,fm1,fpy1,fpy2
      ENDDO


      CLOSE(24)

 555  CONTINUE

      WRITE(6,98)IANS
  98  FORMAT(/' New calulation (yes =1, no = 0) <',I1,'>:',$)
      CALL READI(5,IANS)
      IF(IANS.EQ.0) GO TO 507
      GO TO 777
 508  WRITE(6,*) '  ERROR TERMINATION'
 507  CONTINUE
      END

      SUBROUTINE GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A,DELTAA,
     +SIGMAA,NA,YFIT,CHISQR)
      DIMENSION X(1000),Y(1000),SIGMAY(1000),A(12),DELTAA(12),
     +SIGMAA(12),YFIT(1000)
      INTEGER try

      NFREE=NPTS-NA
      IF(NFREE.EQ.0)NFREE=1
      FREE=NFREE
      CHISQR=0.
      IF(NFREE.LT.0)RETURN

      DO J=1,NA
C  EVALUATE CHI SQUARE AT FIRST TWO SEARCH POINTS
        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        CHISQ1=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)

        try=0
        DELTA=DELTAA(J)
   41   A(J)=A(J)+DELTA
        try=try+1
c        ntry=try/1000
c        IF(1000*ntry.EQ.try)THEN
c          ist=PUTC('.')
c          CALL FLUSH(6)
c        ENDIF
        IF(try.GT.5)THEN
          WRITE(6,*)'Warning, iteration stopped after 5 tries'
          GO TO 81
        ENDIF

        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        CHISQ2=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
        IF(CHISQ1-CHISQ2) 51,41,61

C  REVERSE DIRECTION OF SEARCH IF CHI SQUARE IS INCREASING
   51   DELTA=-DELTA
        A(J)=A(J)+DELTA
        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        SAVE=CHISQ1
        CHISQ1=CHISQ2
        CHISQ2=SAVE

C  INCREMENT A(J) UNTIL CHI SQUARE INCREASES
   61   try=try+1
c        ntry=try/1000
c        IF(1000*ntry.EQ.try)THEN
c          ist=PUTC('.')
c          CALL FLUSH(6)
c        ENDIF
        IF(try.GT.5)THEN
          WRITE(6,*)'Warning, iteration stopped after 5 tries'
          GO TO 81
        ENDIF
        A(J)=A(J)+DELTA
        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        CHISQ3=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
        IF(CHISQ3-CHISQ2) 71,81,81
   71   CHISQ1=CHISQ2
        CHISQ2=CHISQ3
        GO TO 61

C  FIND MIN OF PARABOLA DEFINED BY LAST THREE POINTS
   81   DELTA=DELTA*(1./(1.+(CHISQ1-CHISQ2)/(CHISQ3-CHISQ2))+0.5)
        A(J)=A(J)-DELTA
c Must be wrong to use FREE here, taken away, Magne, June 23. 2002
c       SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
        SIGMAA(J)=DELTAA(J)*SQRT(2./(      (CHISQ3-2.*CHISQ2+CHISQ1)))
      ENDDO

C  EVALUATE FIT AND CHI SQUARE FOR FINAL PARAMETERS
      DO I=1,NPTS
        XX=X(I)
        YFIT(I)=FUNC(XX,A)
      ENDDO
      CHISQR=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      RETURN
      END


      FUNCTION FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      DIMENSION Y(1000),SIGMAY(1000),YFIT(1000)
      CHISQ=0.
      IF(NFREE) 13,13,20
   13 FCHISQ=0.
      GO TO 40

   20  continue
       DO i=1,npts
         w=1.
         yy=ABS(Y(i))
         if(MODE.LT.0.AND.yy.NE.0.)        w=1./yy
         if(MODE.GT.0.AND.SIGMAY(i).NE.0.) w=1./SIGMAY(I)**2
         CHISQ=CHISQ+w*(Y(I)-YFIT(I))**2
       ENDDO
C  DIVIDE BY NUMBER OF DEGREES OF FREEDOM
      FREE=NFREE
      FCHISQ=CHISQ/FREE
   40 RETURN
      END


      FUNCTION FUNC(X,A)
      REAL A(12)
      COMMON/nucleus/xN,xZ,xA,beta

C Calculates resonance parameters from systematics of RIPL, p.102
      pi=3.14159
      ee1=31.2*xA**(-0.333)+20.6*xA**(-0.167)
      ge1=0.026*ee1**1.91
      se1=1.2*120.*xN*xZ/(xA*pi*ge1)
      em1=41*xA**(-0.33333)
      gm1=4.0
      sm1=1.0E-09*1.58*xA**(0.47)*((7.0**2-em1**2)**2+7.0**2*gm1**2)/(8.6E-08*7.0*gm1**2)

      xK =A(1)
      T  =A(2)
  
      xlow = ABS(15.1**2 - 15.2**2)
      diff1 = MAX(xlow,ABS(x**2 - ee1**2))
      diff2 = MAX(xlow,ABS(x**2 - ee2**2))

      fe1=(8.6E-08) * (
     1   (0.7*se1*ge1**2*(x**2+4*pi**2*T**2))/(ee1*(diff1)**2) )

      fm1=(8.6E-08) *
     3    (sm1*x*gm1**2)/((x**2-em1**2)**2+x**2*gm1**2)


      FUNC=LOG( xK*(fe1+fm1))

      RETURN          
      END
