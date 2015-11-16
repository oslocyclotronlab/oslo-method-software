C     Function ERF copied from the CERN mathematical library.

C     VERSION KERNFOR 3.15 820113

C     ENTRY POINTS : ERF,ERFC

C     THESE TWO FUNCTIONS ARE COMPUTED FROM THE RATIONAL APPRO-
C     XIMATIONS OF W.J.CODY, MATHEMATICS OF COMPUTATION, VOL 22
C     (1969), PP 631-637.

C     IF ABS(X) <= 0.47 THEN
C            ERF(X)=ERF(X)
C            ERFC(X)=1-ERF(X)
C     ELSE     (THAT IS ABS(X) > 0.47)
C            IF X > 0 THEN
C                   ERF(X)=1-ERFC(X)
C                   ERFC(X)=ERFC(X)
C            ELSE    (IF X < 0)
C                   ERF(X)=ERFC(-X)-1
C                   ERFC(X)=2-ERFC(-X)
C            ENDIF
C     ENDIF

C--------------------------------------------------------------

      FUNCTION ERF(X)
      REAL P1(4),Q1(4),P2(8),Q2(8),P3(5),Q3(5)
      DATA P1/2.42667955230532E2,
     1        2.19792616182942E1,
     2        6.99638348861914E0,
     3       -3.56098437018154E-2/
      DATA Q1/2.15058875869861E2,
     1        9.11649054045149E1,
     2        1.50827976304078E1,
     3        1.00000000000000E0/
      DATA P2/3.00459261020162E2,
     1        4.51918953711873E2,
     2        3.39320816734344E2,
     3        1.52989285046940E2,
     4        4.31622272220567E1,
     5        7.21175825088309E0,
     6        5.64195517478974E-1,
     7       -1.36864857382717E-7/
      DATA Q2/3.00459260956983E2,
     1        7.90950925327898E2,
     2        9.31354094850610E2,
     3        6.38980264465631E2,
     4        2.77585444743988E2,
     5        7.70001529352295E1,
     6        1.27827273196294E1,
     7        1.00000000000000E0/
      DATA P3/-2.99610707703542E-3,
     1        -4.94730910623251E-2,
     2        -2.26956593539687E-1,
     3        -2.78661308609648E-1,
     4        -2.23192459734185E-2/
      DATA Q3/1.06209230528468E-2,
     1        1.91308926107830E-1,
     2        1.05167510706793E0,
     3        1.98733201817135E0,
     4        1.00000000000000E0/
      DATA CONST2/0.564189583547756/
C        ( CONST2=SQRT(1/PI) )

C--------------------------------------------------------------

C     entry points. set ientry =1 for erf and =2 for erfc

C--------------------------------------------------------------

      IENTRY=1
      IF (ABS(X).GT.6.0)THEN
         ERF=SIGN(1.0,X)
         RETURN
      ENDIF
      GO TO 10

      ENTRY ERFC(X)

      IENTRY=2
      IF (X.LT.-6.0) THEN
         ERF=2.0
         RETURN
      ELSEIF (X.GT.26.0) THEN
         ERF=0.0
         RETURN
      ENDIF

C     select basic function (ibasic=1 for erf and =2 for erfc)

10    T=X
      A=ABS(X)
      S=T**2
      IF (A.LE.0.47) THEN
C     ibasic=1 set y=erf(t)

         IBASIC=1
         Y=T*(P1(1)+S*(P1(2)+S*(P1(3)+S*P1(4) )))
     1      /(Q1(1)+S*(Q1(2)+S*(Q1(3)+S*Q1(4) )))
      ELSE
         IBASIC=2
         IF (A.LE.4.0) THEN
            Y=EXP(-S)*(P2(1)+A*(P2(2)+A*(P2(3)+A*(P2(4)+A*(P2(5)+
     1         A*(P2(6)+A*(P2(7)+A*P2(8) )))) )))
     2         /(Q2(1)+A*(Q2(2)+A*(Q2(3)+A*(Q2(4)+A*(Q2(5)+
     3         A*(Q2(6)+A*(Q2(7)+A*Q2(8) )))) )))
         ELSE

            Y=0.0
            IF (A.LE.26.0) THEN
               R=1.0/A
               U=R**2
               Y=R*EXP(-S)*(CONST2+
     1           U*(P3(1)+U*(P3(2)+U*(P3(3)+U*(P3(4)+U*P3(5) ))))
     2          /(Q3(1)+U*(Q3(2)+U*(Q3(3)+U*(Q3(4)+U*Q3(5) )))) )
            ENDIF
         ENDIF
      ENDIF

C     express final result in terms of y

      IF (IENTRY.EQ.1) THEN
         IF (IBASIC.NE.2) THEN
            ERF=Y
         ELSE
            ERF=1.0-Y
            IF (X.LT.0.0) ERF=-ERF
         ENDIF
      ELSE
         IF (IBASIC.NE.2) THEN
            ERF=1.0-Y
         ELSE
            IF (X.LT.0.0) Y=2.0-Y
            ERF=Y
         ENDIF
      ENDIF

      RETURN
      END
