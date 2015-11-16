      SUBROUTINE GRAX(X1,X2,DEX,NV,IYFLAG)

C           define tick-points for marking of axes....
C           a = low axis value....
C           b = high axis value....
C           dex = array of tick points....
C           nv = number of tick points....
C           kh = 1/2/3 for lin/sqrt/log axis....

      REAL DEX(14),DEX1(14),DEX2(14)
C Takes into account that A and B might be pos./neg. and not
C increasing, etc. Modified by Magne

      XA=X1
      XB=X2

      IF(XB.LT.XA)THEN
        WAIT=XA
        XA=XB
        XB=WAIT
      ENDIF

      IF(XA.GE.0.AND.XB.GE.0)THEN             !case (+1,+2) (normal)
        CALL MGRAX(XA,XB,DEX,NV,IYFLAG)   
        RETURN
      ENDIF

      IF(XA.LE.0.AND.XB.LT.0)THEN             !case (-2,-1)
        CALL MGRAX(ABS(XB),ABS(XA),DEX,NV,IYFLAG) 
        DO i=1,NV
          DEX(i)=-DEX(i)
        ENDDO  
        RETURN
      ENDIF

      IF(XA.LE.0.AND.XB.GE.0)THEN             !case (-1,+1)
        CALL MGRAX(0,XB,DEX1,NV1,IYFLAG)
        CALL MGRAX(0,ABS(XA),DEX2,NV2,IYFLAG)
        DEXX=DEX1(1)
        IF(DEX1(1).LT.DEX2(1))DEXX=DEX2(1)
        IF(NV1+NV2.GE.12)DEXX=DEXX*2.
C Going from 0 to positive
        DEX1(1)=0
        DO i=2,14
          DEX1(i)=DEX1(i-1)+DEXX
          IF(DEX1(i).GT.XB)GO TO 10
        ENDDO
  10    ih=i-1
C Going from 0 to negative
        DEX2(1)=0
        DO i=2,14 
          DEX2(i)=DEX2(i-1)-DEXX
          IF(DEX2(i).LT.XA)GO TO 11
        ENDDO
  11    il=i-1
C Putting together DEX(i)
        DO i=1,il-1
          DEX(i)=DEX2(il+1-i)
        ENDDO
        DEX(il)=0
        DO i=il+1,il+ih-1
          DEX(i)=DEX1(i+1-il)
        ENDDO
        NV=il+ih-1
        RETURN
      ENDIF

      END


      SUBROUTINE MGRAX(A,B,DEX,NV,IYFLAG)

C           define tick-points for marking of axes....
C           a = low axis value....
C           b = high axis value....
C           dex = array of tick points....
C           nv = number of tick points....
C           kh = 1/2/3 for lin/sqrt/log axis....

      REAL DEX(14)
C Takes into account that A and B might be pos./neg. and not
C increasing, etc. Modified by Magne

      IF (IYFLAG.EQ.3) GO TO 40
C         linear or sqrt axis....
      X=B-A
      C=1.0
      DO 10 K=1,10
         IF (X.LT.15.0) GO TO 20
         C=C*10.0
         X=X/10.0
10    CONTINUE


20    IF (X.LT.1.0) THEN
         TX=X/10.
      ELSEIF(X.LT.1.5) THEN
         TX=0.1
      ELSEIF (X.LT.3.0) THEN
         TX=0.2
      ELSEIF (X.LT.7.5) THEN
         TX=0.5
      ELSE
         TX=1.0
      ENDIF

      CTX=C*TX
      J=A/CTX
      C=CTX*J
      IF (C.LE.A) C=C+CTX
      DO 30 NV=1,14
         DEX(NV)=C
         C=C+CTX
         IF (C.GE.B) RETURN
30    CONTINUE
      NV=14
      RETURN

C        logarithmic axis....
40    TX=ALOG(10.)
      J=ALOG(A)/TX
      K=ALOG(B-1.0)/TX
      C=10.0**J
      IF (K-J.LT.2) THEN
         DEX(1)=2.0*C
         DEX(2)=5.0*C
         DEX(3)=10.0*C
         DEX(4)=20.0*C
         DEX(5)=50.0*C
         NV=5
      ELSE
         NV=K-J
         DO 60 I=1,NV
            C=C*10.0
            DEX(I)=C
60       CONTINUE
      ENDIF
      RETURN
      END
