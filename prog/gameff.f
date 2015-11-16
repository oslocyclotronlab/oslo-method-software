      PROGRAM GAMEFF
      EXTERNAL FUNC
      REAL E,EFF
      DIMENSION YFIT(100)
      DIMENSION X(100),Y(100),SIGMAY(100),
     1A(5),DELTAA(5),SIGMAA(5),CH(1010)
      DIMENSION XD(100),YD(100),SIGD(100)

      WRITE(6,*)'       ******************************************'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *              G A M E F F               *'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *        PROGRAM TO CALCULATE THE        *'
      WRITE(6,*)'       *    EFFICIENCY OF GEM-DETECTORS AS A    *'
      WRITE(6,*)'       *   FUNCTION OF GAMMA-ENERGY IN KEV.     *'
      WRITE(6,*)'       * DATA IS READ FROM FILE gameffin.dat    *'
      WRITE(6,*)'       *     THE DATA IS ARRANGED LIKE THIS:    *'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *          121.78   1.175  0.023         *'
      WRITE(6,*)'       *          244.69   2.232  0.060         *'
      WRITE(6,*)'       *          .                             *'
      WRITE(6,*)'       *          .                             *'
      WRITE(6,*)'       *          0        0      0             *'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *     REMEMBER TO STOP WITH THREE 0s     *'
      WRITE(6,*)'       *             1.4.1991/MG                *'
      WRITE(6,*)'       ******************************************'

      WRITE(6,10)
  10  FORMAT(/'Type RETURN')
      CALL READI(5,I)

C     READING PARAMETERS FROM DISK

   1  CONTINUE

C First looking at own home-directory
      OPEN(23,FILE='gameffin.dat',STATUS='OLD',ERR=21)
      GO TO 20
21    WRITE(6,*)'Did not find gameffin.dat in your directory'
      WRITE(6,*)'Taking instead /d2/fys/mama/resp/gameffin.dat'
      OPEN(23,FILE='/d2/fys/mama/resp/gameffin.dat',
     1 STATUS='OLD',ERR=508)
20    CONTINUE

      DO I=1,100
        READ(23,*)XD(I),YD(I),SIGD(I)
        IF(XD(I).LT.0.00001) GO TO 2
        X(I)=ALOG(XD(I))
        Y(I)=ALOG(YD(I))
        SIGMAY(I)=0.5*ABS(ALOG(YD(I)+SIGD(I))-ALOG(YD(I)-SIGD(I)))
      ENDDO
   2  CLOSE(23)
      NPTS=I-1
      NA=4
      IF(NPTS.LT.6)THEN
        WRITE(6,*)' Sorry, at least 6 data-points must be used'
        GO TO 999
      ENDIF

C     INITIAL STARTING VALUES
      A(1)=5.
      A(2)=0.5
      A(3)=5.0
      A(4)=0.005
      A(5)=0.00

      WRITE(6,3)A(1)
  3   FORMAT(/'START VALUE A(1) = <',F6.3,'>:',$)
      CALL READF(5,A(1))
      WRITE(6,4)A(2)
  4   FORMAT(/'START VALUE A(2) = <',F6.3,'>:',$)
      CALL READF(5,A(2))
      WRITE(6,5)A(3)
  5   FORMAT(/'START VALUE A(3) = <',F6.3,'>:',$)
      CALL READF(5,A(3))
      WRITE(6,6)A(4)
  6   FORMAT(/'START VALUE A(4) = <',F6.3,'>:',$)
      CALL READF(5,A(4))

      SIGMAA(1)=ABS(A(1)/5.)
      DELTAA(1)=ABS(A(1)/5.)
      SIGMAA(2)=ABS(A(2)/10.)
      DELTAA(2)=ABS(A(2)/10.)
      SIGMAA(3)=ABS(A(3)/10.)
      DELTAA(3)=ABS(A(3)/10.)
      SIGMAA(4)=ABS(A(4)/20.)
      DELTAA(4)=ABS(A(4)/20.)
      SIGMAA(5)=ABS(A(5)/20.)
      DELTAA(5)=ABS(A(5)/20.)

C     PREPARATION FOR FIT
      PROG=0.000002
      MODE=1
      M=0
      CHISQR=999999.
      BEST=999999.
      NBAD=0
C
C     STARTING FIT  ***********************************************
C
      WRITE(6,80)
  80  FORMAT(' LOOP    CHISQR        A(1)         A(2).....')
      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
 505  CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A
     1,DELTAA,SIGMAA,NA,YFIT,CHISQR)
      M=M+1
      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
  81  FORMAT(1X,I3,6(1X,E11.5))
      CH(M)=CHISQR
      IF(M.LT.2) GO TO 505
      VER=ABS(CH(M)-CH(M-1))/CH(M-1)
      IF(CHISQR.LT.BEST)THEN
        BEST=CHISQR
        AA1=A(1)
        AA2=A(2)
        AA3=A(3)
        AA4=A(4)
        AA5=A(5)
        SIGAA1=SIGMAA(1)
        SIGAA2=SIGMAA(2)
        SIGAA3=SIGMAA(3)
        SIGAA4=SIGMAA(4)
        SIGAA5=SIGMAA(5)
      ENDIF
      IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
      IF(NBAD.GT.6)GO TO 506
      IF(M.GT.1000)GO TO 506
      IF(VER.GT.PROG) GO TO 505
 506  CONTINUE   

C     FIT IS FINISHED *********************************************

      A(1)=AA1
      SIGMAA(1)=SIGAA1
      A(2)=AA2
      SIGMAA(2)=SIGAA2
      A(3)=AA3
      SIGMAA(3)=SIGAA3
      A(4)=AA4
      SIGMAA(4)=SIGAA4
      A(5)=AA5
      SIGMAA(5)=SIGAA5

      WRITE(6,88)M,BEST
  88  FORMAT(/' FINAL RESULT FROM LOOP',I4,' WITH CHI.=',F7.3)
      WRITE(6,83)  A(1),SIGMAA(1)
      WRITE(6,84)  A(2),SIGMAA(2)
      WRITE(6,85)  A(3),SIGMAA(3)
      WRITE(6,86)  A(4),SIGMAA(4)
      WRITE(6,87)  A(5),SIGMAA(5)
  83  FORMAT(//' A(1) ',E12.5,' +/- ',E12.5)
  84  FORMAT(' A(2) ',E12.5,' +/- ',E12.5)
  85  FORMAT(' A(3) ',E12.5,' +/- ',E12.5)
  86  FORMAT(' A(4) ',E12.5,' +/- ',E12.5)
  87  FORMAT(' A(5) ',E12.5,' +/- ',E12.5)


 999  WRITE(6,34)
  34  FORMAT(/' Calc. efficiency at a certain energy (keV) (0)',/,
     1        ' Make new read of file and fits             (1)',/,
     2        ' List data and fit-parameters               (2)',/,
     3        ' EXIT                                       (3)')
      WRITE(6,32)IANS
  32  FORMAT(/'PLEASE, GIVE YOUR ANSWER',
     1  '                  <',I2,'>:',$)
      CALL READI(5,IANS)

      IF(IANS.LT.0.OR.IANS.GT.3) GO TO 999

      IF(IANS.EQ.0)THEN
  101   WRITE(6,102)
  102   FORMAT(/'TYPE ENERGY IN KEV (STOP = 0): ',$)
        CALL READF(5,E)
        IF(E.EQ.0) GO TO 999
         EFF=EXP(AA1-(AA2+AA3*EXP(-AA4*E))*ALOG(E))
C        EFF=EXP(AA1-(AA2+AA3*EXP(-AA4*E))*EXP(-AA5*E)*ALOG(E))
C        EFF=EXP(AA1-AA2*ALOG(E))
        WRITE(6,103)E,EFF
  103   FORMAT(' Efficiency at ',F8.2,'keV is: ',F6.3)
        GO TO 101
      ENDIF

      IF(IANS.EQ.1)GO TO 1

      IF(IANS.EQ.2) THEN
        WRITE(6,*)' Energy(keV)  Rel.int.  Uncert.  Fit'
        DO I=1,NPTS
          YFITD=EXP(YFIT(I))
          WRITE(6,104)XD(I),YD(I),SIGD(I),YFITD
  104     FORMAT(F10.2,F10.3,F10.4,F10.3)
        ENDDO
        WRITE(6,105)AA1,AA2,AA3,AA4,AA5
  105   FORMAT(' Fit-parameters A(I): ',5F7.3)
        WRITE(6,106)SIGAA1,SIGAA2,SIGAA3,SIGAA4,SIGAA5
  106   FORMAT(' Uncertainties dA(I): ',5F7.4)
        GO TO 999
      ENDIF
      GO TO 9999
 508  WRITE(6,*) 'Could not read gameffin.dat'
9999  CONTINUE
      END


      SUBROUTINE GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A,DELTAA,
     1SIGMAA,NA,YFIT,CHISQR)
      DIMENSION X(100),Y(100),SIGMAY(100),A(5),DELTAA(5),
     1SIGMAA(5),YFIT(100)
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
      SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
   90 CONTINUE
C
C  EVALUATE FIT AND CHI SQUARE FOR FINAL PARAMETERS
C
      DO 92 I=1,NPTS
      XX=X(I)
   92 YFIT(I)=FUNC(XX,A)
      CHISQR=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
  10  RETURN
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
      FUNC=0
      E=EXP(X)
      FUNC=A(1)-(A(2)+A(3)*EXP(-A(4)*E))*X
C      FUNC=A(1)-(A(2)+A(3)*EXP(-A(4)*E))*EXP(-A(5)*E)*X
C      FUNC=A(1)-A(2)*X
      RETURN          
      END


       SUBROUTINE READI(IDEV,INTEG)
       CHARACTER X*50
       READ(IDEV,1)X
    1  FORMAT(50A)
       IF(X.EQ.'')RETURN
       READ(X,*)INTEG
       RETURN
       END

       SUBROUTINE READF(IDEV,REELL)
       CHARACTER X*50
       READ(IDEV,1)X
    1  FORMAT(50A)
       IF(X.EQ.'')RETURN
       READ(X,*)REELL
       RETURN
       END

       SUBROUTINE READA(IDEV,KAR)
       CHARACTER X*50
       CHARACTER KAR*50
       READ(IDEV,1)X
    1  FORMAT(50A)
       IF(X.EQ.'')RETURN
       READ(X,*)KAR
       RETURN
       END
