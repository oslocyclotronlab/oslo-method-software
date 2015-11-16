
C=======================================================================

          SUBROUTINE PSPOT(X,Y)

      REAL     X, Y
      INTEGER  IX, IY, K

C          position graphics cursor at (x,y) w.r.to axes....
      K=0
      GO TO 5

          ENTRY VECT(X,Y)
C          draw vector from current position to (x,y) w.r.to axes....
      K=1
      GO TO 5

          ENTRY POINT(X,Y)
C          put a point mark at (x,y) w.r.to axes....
      K=2
5     CALL CVXY(X,Y,IX,IY,1)
      CALL KTRAS(IX,IY,K)
      RETURN
      END
