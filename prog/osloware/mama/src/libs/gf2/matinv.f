
C=======================================================================

      SUBROUTINE MATINV(ARRAY,NORDER,IDIM)
      REAL*8 ARRAY(IDIM,IDIM),AMAX,SAVE
      INTEGER IK(100),JK(100)
      DO 100 K=1,NORDER

c        find largest element array(i,j) in rest of matrix....

         AMAX=0.
  21     DO 31 I=K,NORDER
            DO 30 J=K,NORDER
               IF (DABS(AMAX)-DABS(ARRAY(I,J))) 24,24,30
  24           AMAX=ARRAY(I,J)
               IK(K)=I
               JK(K)=J
  30        CONTINUE
  31     CONTINUE

C        interchange rows and columns to put amax in array(k,k)....

         IF (AMAX) 41,140,41
  41     I=IK(K)
         IF (I-K) 21,51,43
  43     DO 50 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=ARRAY(I,J)
            ARRAY(I,J)=-SAVE
  50     CONTINUE
  51     J=JK(K)
         IF (J-K) 21,61,53
  53     DO 60 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=ARRAY(I,J)
            ARRAY(I,J)=-SAVE
  60     CONTINUE

C        accumulate elements of inverse matrix....

  61     DO 70 I=1,NORDER
            IF (I-K) 63,70,63
  63        ARRAY(I,K)=-ARRAY(I,K)/AMAX
  70     CONTINUE
         DO 81 I=1,NORDER
            DO 80 J=1,NORDER
               IF (I-K) 74,80,74
  74           IF (J-K) 75,80,75
  75           ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
  80        CONTINUE
  81     CONTINUE
         DO 90 J=1,NORDER
            IF (J-K) 83,90,83
  83        ARRAY(K,J)=ARRAY(K,J)/AMAX
  90     CONTINUE
         ARRAY(K,K)=1./AMAX
 100  CONTINUE

C        restore ordering of matrix....

      DO 130 L=1,NORDER
         K=NORDER-L+1
         J=IK(K)
         IF (J-K) 111,111,105
 105     DO 110 I=1,NORDER
            SAVE=ARRAY(I,K)
            ARRAY(I,K)=-ARRAY(I,J)
            ARRAY(I,J)=SAVE
 110     CONTINUE
 111     I=JK(K)
         IF (I-K) 130,130,113
 113     DO 120 J=1,NORDER
            SAVE=ARRAY(K,J)
            ARRAY(K,J)=-ARRAY(I,J)
            ARRAY(I,J)=SAVE
 120     CONTINUE
 130  CONTINUE
 140  RETURN
      END
