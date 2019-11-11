      SUBROUTINE GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A,DELTAA,
     +SIGMAA,NA,YFIT,CHISQR)
       DIMENSION X(50000),Y(50000),SIGMAY(50000),A(5),DELTAA(5),
     +SIGMAA(5),YFIT(50000)
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
        ntry=try/1000
        IF(1000*ntry.EQ.try)THEN
          write(6,FMT='(A1,$)')'.'
          CALL FLUSH(6)
        ENDIF
        IF(try.GT.5000)THEN
          WRITE(6,*)'Warning, iteration stopped after 5000 tries'
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
        ntry=try/1000
        IF(1000*ntry.EQ.try)THEN
          write(6,FMT='(A1,$)')'.'
          CALL FLUSH(6)
        ENDIF
        IF(try.GT.5000)THEN
          WRITE(6,*)'Warning, iteration stopped after 5000 tries'
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
C Must be wrong to use FREE here, taken away, Magne, June 23. 2002
C Implemented by Andreas Schiller, June 26 2002
C        SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
        SIGMAA(J)=DELTAA(J)*SQRT(2./(CHISQ3-2.*CHISQ2+CHISQ1))
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
      DIMENSION Y(50000),SIGMAY(50000),YFIT(50000)
      CHISQ=0.
      IF(NFREE) 13,13,20
   13 FCHISQ=0.
      GO TO 40

C  ACCUMULATE CHI SQUARE
c   20 DO 30 I=1,NPTS
c      IF(MODE)22,27,29
c   22 IF(Y(I))25,27,23
c   23 WEIGHT=1./Y(I)
c      GO TO 30
c   25 WEIGHT=1./(-Y(I))
c      GO TO 30
c   27 WEIGHT=1.
c      GO TO 30
c   29 WEIGHT=1./SIGMAY(I)**2
c   30 CHISQ=CHISQ+WEIGHT*(Y(I)-YFIT(I))**2
C Modified by Magne
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

