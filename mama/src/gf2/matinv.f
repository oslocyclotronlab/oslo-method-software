
C=======================================================================

      SUBROUTINE MATINV(ARRAY,NORDER,IDIM)

      REAL*8  ARRAY(IDIM,IDIM), AMAX, SAVE
      INTEGER IK(100), JK(100)

      DO K = 1, NORDER

c        find largest element array(i,j) in rest of matrix....

         AMAX = 0.0D0
21       DO I = K, NORDER
            DO J = K, NORDER
               IF (DABS(AMAX)-DABS(ARRAY(I,J)).LE.0.0D0) THEN
                  AMAX = ARRAY(I,J)
                  IK(K) = I
                  JK(K) = J
               ENDIF
            ENDDO
         ENDDO
         IF (AMAX.EQ.0.0) RETURN

C        interchange rows and columns to put amax in array(k,k)....

         I = IK(K)
         IF (I.LT.K) GO TO 21
         IF (I.GT.K) THEN
            DO J = 1, NORDER
               SAVE = ARRAY(K,J)
               ARRAY(K,J) = ARRAY(I,J)
               ARRAY(I,J) = -SAVE
            ENDDO
         ENDIF

         J = JK(K)
         IF (J.LT.K) GO TO 21
         IF (J.GT.K) THEN
            DO I = 1, NORDER
               SAVE = ARRAY(I,K)
               ARRAY(I,K) = ARRAY(I,J)
               ARRAY(I,J) = -SAVE
            ENDDO
         ENDIF

C        accumulate elements of inverse matrix....

         DO I = 1, NORDER
            IF (I.NE.K) ARRAY(I,K) = -ARRAY(I,K)/AMAX
         ENDDO
         DO I = 1, NORDER
            DO J = 1, NORDER
               IF (I.NE.K .AND. J.NE.K)
     +               ARRAY(I,J) = ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J)
            ENDDO
         ENDDO
         DO J = 1, NORDER
            IF (J.NE.K) ARRAY(K,J) = ARRAY(K,J)/AMAX
         ENDDO
         ARRAY(K,K)=1./AMAX
      ENDDO

C        restore ordering of matrix....

      DO L = 1, NORDER
         K = NORDER - L + 1
         J = IK(K)
         IF (J.GT.K) THEN
            DO I = 1, NORDER
               SAVE = ARRAY(I,K)
               ARRAY(I,K) = -ARRAY(I,J)
               ARRAY(I,J) = SAVE
            ENDDO
         ENDIF

         I = JK(K)
         IF (I.GT.K) THEN
            DO J = 1, NORDER
               SAVE = ARRAY(K,J)
               ARRAY(K,J) = -ARRAY(I,J)
               ARRAY(I,J) = SAVE
            ENDDO
         ENDIF
      ENDDO

      RETURN
      END
