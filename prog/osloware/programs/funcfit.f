      PROGRAM FUNCFIT
      EXTERNAL FUNC
      DIMENSION YFIT(100)
      DIMENSION X(100),Y(100),SIGMAY(100),
     1A(3),DELTAA(3),SIGMAA(3),CH(1010)

      OPEN(23,FILE='funcfit.dat',STATUS='old',ERR=666)
      READ(23,*,ERR=666)NA,NPTS
      DO i=1,NPTS
        READ(23,*,ERR=666)X(i),Y(i),SIGMAY(i)
      ENDDO
      CLOSE(23)
      WRITE(6,*)'Default values taken from file: funcfit.dat'
      GO TO 777
 666  NA=2
      NPTS=9
      DO i=1,NPTS
        X(i)=10.+(I-1.)*20.
        Y(i)=1000.
        SIGMAY(i)=SQRT(1000.)
      ENDDO

 777  CONTINUE

      WRITE(6,62)NA
  62  FORMAT(/' Number of parameters to be fitted (1, 2 or 3) <',I3,'>:',$)
      CALL READI(5,NA)

      WRITE(6,63)NPTS
  63  FORMAT( ' Number of datapoints                          <',I3,'>:',$)
      CALL READI(5,NPTS)

C     ESTIMATE PARAMETERS TO BE FITTED
      A(1)=1000.
      A(2)=200.
      A(3)=40.
      IF(NA.LT.3)A(3)=0.
      IF(NA.LT.2)A(2)=0.
      DO I=1,3
        SIGMAA(I)=ABS(A(I)/10.)
        DELTAA(I)=ABS(A(I)/10.)
      ENDDO
C
C     PREPARATION FOR FIT
C
      PROG=0.000002
      MODE=1
      M=0

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

      OPEN(23,FILE='funcfit.dat',ERR=555)
      WRITE(23,*,ERR=555)NA,NPTS
      DO i=1,NPTS
        WRITE(23,*,ERR=555)X(i),Y(i),SIGMAY(i)
      ENDDO
      CLOSE(23)
 555  CONTINUE

      CHISQR=999999.
      BEST=999999.
      NBAD=0
C
C     STARTING FIT  ***********************************************
C
      IF(NA.EQ.1)WRITE(6,78)
      IF(NA.EQ.2)WRITE(6,79)
      IF(NA.EQ.3)WRITE(6,80)

  78  FORMAT(' Loop    Chi**2        A(1)')
  79  FORMAT(' Loop    Chi**2        A(1)         A(2)')
  80  FORMAT(' Loop    Chi**2        A(1)         A(2)         A(3)')

      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
 505  CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A
     1,DELTAA,SIGMAA,NA,YFIT,CHISQR,N)
      M=M+1
      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
  81  FORMAT(1X,I3,4(1X,E12.5))
      CH(M)=CHISQR
      IF(M.LT.2) GO TO 505
      VER=ABS(CH(M)-CH(M-1))/CH(M-1)
      IF(CHISQR.LT.BEST)THEN
        BEST=CHISQR
        AA1=A(1)
        AA2=A(2)
        AA3=A(3)
        SIGAA1=SIGMAA(1)
        SIGAA2=SIGMAA(2)
        SIGAA3=SIGMAA(3)
      ENDIF
      IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
      IF(NBAD.GT.6)GO TO 506
      IF(M.GT.1000)GO TO 506
      IF(VER.GT.PROG) GO TO 505
 506  CONTINUE   
C
C     FIT IS FINISHED *********************************************
C
      A(1)=AA1
      SIGMAA(1)=SIGAA1
      A(2)=AA2
      SIGMAA(2)=SIGAA2
      A(3)=AA3
      SIGMAA(3)=SIGAA3

      WRITE(6,*)' '
      WRITE(6,*)'       X(i)        Y(i)     Yfit(i)'
      DO I=1,NPTS
        WRITE(6,44)X(I),Y(I),YFIT(I)
44      FORMAT(3F12.4)
      ENDDO


      WRITE(6,86)M,BEST
  86  FORMAT(/' Final result from loop',I4,' with Chi**2 = ',E10.4)
      WRITE(6,83)  A(1),SIGMAA(1)
      WRITE(6,84)  A(2),SIGMAA(2)
      WRITE(6,85)  A(3),SIGMAA(3)
  83  FORMAT(' A(1) = ',E12.5,' +/- ',E12.5)
  84  FORMAT(' A(2) = ',E12.5,' +/- ',E12.5)
  85  FORMAT(' A(3) = ',E12.5,' +/- ',E12.5)

      WRITE(6,98)IANS
  98  FORMAT(/' New calulation (yes =1, no = 0) <',I1,'>:',$)
      CALL READI(5,IANS)
      IF(IANS.EQ.0) GO TO 507
      GO TO 777
 508  WRITE(6,*) '  ERROR TERMINATION'
 507  CONTINUE
      END


      SUBROUTINE GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A,DELTAA,
     1SIGMAA,NA,YFIT,CHISQR,N)
      DIMENSION X(100),Y(100),SIGMAY(100),A(3),DELTAA(3),
     1SIGMAA(3),YFIT(100)
      REAL N(0:100)
      NTERMS=NA
      NFREE=NPTS-NTERMS
      FREE=NFREE
      CHISQR=0.
      IF(NFREE)10,10,20
   20 DO 90 J=1,NTERMS
C
C  EVALUATE CHI SQUARE AT FIRST TWO SEARCH POINTS
C
      DO 22 I=1,NPTS
      XX=X(I)
   22 YFIT(I)=FUNC(XX,A)
      CHISQ1=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      FN=0.
      DELTA=DELTAA(J)
   41 A(J)=A(J)+DELTA
      DO 43 I=1,NPTS
      XX=X(I)
   43 YFIT(I)=FUNC(XX,A)
      CHISQ2=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      IF(CHISQ1-CHISQ2) 51,41,61
C
C  REVERSE DIRECTION OF SEARCH IF CHI SQUARE IS INCREASING
C
   51 DELTA=-DELTA
      A(J)=A(J)+DELTA
      DO 54 I=1,NPTS
      XX=X(I)
   54 YFIT(I)=FUNC(XX,A)
      SAVE=CHISQ1
      CHISQ1=CHISQ2
      CHISQ2=SAVE
C
C  INCREMENT A(J) UNTIL CHI SQUARE INCREASES
C
   61 FN=FN+1
      A(J)=A(J)+DELTA
      DO 64 I=1,NPTS
      XX=X(I)
   64 YFIT(I)=FUNC(XX,A)
      CHISQ3=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      IF(CHISQ3-CHISQ2) 71,81,81
   71 CHISQ1=CHISQ2
      CHISQ2=CHISQ3
      GO TO 61
C
C  FIND MIN OF PARABOLA DEFINED BY LAST THREE POINTS
C
   81 DELTA=DELTA*(1./(1.+(CHISQ1-CHISQ2)/(CHISQ3-CHISQ2))+0.5)
      A(J)=A(J)-DELTA
C Must be wrong to use FREE here, taken away, Magne, June 23. 2002
C Implemented by Andreas Schiller, June 26 2002
C      SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
      SIGMAA(J)=DELTAA(J)*SQRT(2./(CHISQ3-2.*CHISQ2+CHISQ1))
   90 CONTINUE
C
C  EVALUATE FIT AND CHI SQUARE FOR FINAL PARAMETERS
C
      DO 92 I=1,NPTS
      XX=X(I)
   92 YFIT(I)=FUNC(XX,A)
      CHISQR=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
   10 RETURN
      END

      FUNCTION FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      DIMENSION Y(100),SIGMAY(100),YFIT(100)
      CHISQ=0.
      IF(NFREE) 13,13,20
   13 FCHISQ=0.
      GO TO 40
C
C  ACCUMULATE CHI SQUARE
C
   20 DO 30 I=1,NPTS
      IF(MODE)22,27,29
   22 IF(Y(I))25,27,23
   23 WEIGHT=1./Y(I)
      GO TO 30
   25 WEIGHT=1./(-Y(I))
      GO TO 30
   27 WEIGHT=1.
      GO TO 30
   29 WEIGHT=1./SIGMAY(I)**2
   30 CHISQ=CHISQ+WEIGHT*(Y(I)-YFIT(I))**2
C
C  DIVIDE BY NUMBER OF DEGREES OF FREEDOM
C
      FREE=NFREE
      FCHISQ=CHISQ/FREE
   40 RETURN
      END

      FUNCTION FUNC(X,A)
      REAL A(5)
      FUNC=A(1)+A(2)*X+A(3)*X*X
      RETURN          
      END

