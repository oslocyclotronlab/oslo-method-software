
C=======================================================================

          SUBROUTINE SYMBG(I,X,Y,LG,C)

C           put a graphics symbol at (x,y) w.r.to the axes....
C           lg = size of symbol in graphics pixels
C           i = 1/2/3/4 for circle/horiz. dash/vert. dash/vert. cross
C           i = 5/6/7 for inclined cross/square/diamond
C           i = 8/9 for arrow head / triangle pointing to (x,y);
C              here c = 0/90/180/270 for orientation in degrees

C           i = 10 for filled square; here c = 0.0 to 1.0 gives density
C                    of spiral used to fill the square....

      INTEGER  I, LG
      REAL     X, Y, C

      REAL     DX(4),DY(4),FX(4),FY(4)
      DATA     DX /-0.866,-0.5,   0.866,-0.5  /
      DATA     DY / 0.5,  -0.866, 0.5,   0.866/
      DATA     FX /-0.866, 0.5,   0.866, 0.5  /
      DATA     FY /-0.5,  -0.866,-0.5,   0.866/


      ML=LG/2
      CALL CVXY(X,Y,IX,IY,1)
      NX=IX+ML
      MX=IX-ML
      NY=IY+ML
      MY=IY-ML
      GO TO (10,20,30,20,50,60,70,80,80,100),I
C         circle....
10    DO 15 N=-1,1,2
         CALL MSPOT(MX,IY)
         DO 14 J=MX+1,NX
            K=SQRT(FLOAT(ML*ML-(IX-J)*(IX-J)))+.5
            CALL IVECT(J,IY+K*N)
14       CONTINUE
15    CONTINUE
      GO TO 200
C         horizontal dash or cross....
20    CALL MSPOT(MX,IY)
      CALL IVECT(NX,IY)
      IF (I.EQ.2) GO TO 200
C         vertical dash....
30    CALL MSPOT(IX,MY)
      CALL IVECT(IX,NY)
      GO TO 200
C         inlined cross....
50    CALL MSPOT(NX,NY)
      CALL IVECT(MX,MY)
      CALL MSPOT(NX,MY)
      CALL IVECT(MX,NY)
      GO TO 200
C         square....
60    CALL MSPOT(MX,MY)
      CALL IVECT(NX,MY)
      CALL IVECT(NX,NY)
      CALL IVECT(MX,NY)
      CALL IVECT(MX,MY)
      GO TO 200
C         diamond....
70    CALL MSPOT(MX,IY)
      CALL IVECT(IX,MY)
      CALL IVECT(NX,IY)
      CALL IVECT(IX,NY)
      CALL IVECT(MX,IY)
      GO TO 200
C          arrow head or triangle....
80    MY=C/90+1
      NX=DX(MY)*LG+IX+0.5
      NY=DY(MY)*LG+IY+0.5
      MX=FX(MY)*LG+IX+0.5
      MY=FY(MY)*LG+IY+0.5
      CALL MSPOT(NX,NY)
      CALL IVECT(IX,IY)
      CALL IVECT(MX,MY)
      IF (I.EQ.8) GO TO 200
      CALL IVECT(NX,NY)
      GO TO 200
C         spiral....
100   CR=C
      IF (C.GT.1.) CR=1.
      JL=2*ML
      J=JL*(JL+2)
      IF (CR*J.LT.0.5) GO TO 200
      MY=C*J
      IF (MY.GT.J) MY=J
      NX=1

      CALL MSPOT(IX,IY)
      DO 130 N=1,JL+1
         NN=N
         DO 120 K=1,2
            MY=MY-N
            IF (MY.LT.0) NN=MY+NN
            IF (K.EQ.1) IX=IX+NN*NX
            IF (K.EQ.2) IY=IY+NN*NX
            CALL IVECT(IX,IY)
            IF (MY.LE.0) GO TO 200
120      CONTINUE
         NX=-NX
130   CONTINUE
200   RETURN
      END
