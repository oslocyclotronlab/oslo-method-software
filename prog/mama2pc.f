      PROGRAM TRANSFER
C THE PROGRAM TRANSFERS 1 AND 2 DIMENSIONAL SPECTRA TO SPECTRA FOR THE
C PC-PROGRAMS GRAPHER/SURFER, WHICH NEEDS ANOTHER FORMAT THAN THE
C NORDIC ONE. THE FORMAT SHOULD BE ON THE FORM (2 DIM. CASE):
C    X1 Y1 Z1 (CR)
C    X2 Y2 Z2 (CR)
C         .
C         . 
C         .
C        ETC.
C
C          FOR MORE INFORMATION ASK MAGNE GUTTORMSEN.
C                  DEPARTMENT OF PHYSICS
C                   UNIVERSITY OF OSLO
C                   OSLO  1.3 1994
C

C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)

      CHARACTER fname*8,comm*60
      CHARACTER APP*4
      CHARACTER FILNAM*40 

      WRITE(6,*)'    **************************************************'
      WRITE(6,*)'    *                 M A M A 2 P C                  *'
      WRITE(6,*)'    *                                                *'
      WRITE(6,*)'    *         Program to transfer spectra from       *'
      WRITE(6,*)'    *    MAMA format to the format used by various   *'
      WRITE(6,*)'    *      PC-programs. The data is written like:    *'
      WRITE(6,*)'    *   x1 y1 z1 (CR)                                *'
      WRITE(6,*)'    *   x2 y2 z2 (CR) ...       and so on. You can   *'
      WRITE(6,*)'    * choose if x and y should be channels or energy *'
      WRITE(6,*)'    *          OSLO CYCLOTRON LABORATORY             *'
      WRITE(6,*)'    *                  1.3.1994/MG                   *'
      WRITE(6,*)'    **************************************************'


      AX0=0.
      AX1=1.
      AY0=0.
      AY1=1.
      ITYPE=3
      MAXCH=4191
      XDIM=512
      YDIM=512
      IDEST=1
      Istatus=0

      CALL READFILE

      WRITE(6,*)' '
      WRITE(6,*)'You might get x- and y-values in energies if'
      WRITE(6,*)'you answer something else than a0=0 and a1=1'
      
      
      IF(Istatus.NE.0)STOP
      IF(ITYPE.LE.1) THEN
        WRITE(6,7)AX0
 7      FORMAT(/' a0 of spectrum (MeV)    <',F6.3,'>:',$)
        CALL READF(5,AX0)
        WRITE(6,8)AX1
 8      FORMAT( ' a1 of spectrum (MeV/ch) <',F6.3,'>:',$)
        CALL READF(5,AX1)
      ELSE
        WRITE(6,9)AX0
 9      FORMAT(/' a0 of x-axis (MeV)      <',F6.3,'>:',$)
        CALL READF(5,AX0)
        WRITE(6,10)AX1
 10     FORMAT( ' a1 of x-axis (MeV/ch)   <',F6.3,'>:',$)
        CALL READF(5,AX1)
        WRITE(6,11)AY0
 11     FORMAT(/' a0 of y-axis (MeV)      <',F6.3,'>:',$)
        CALL READF(5,AY0)
        WRITE(6,12)AY1
 12     FORMAT( ' a1 of y-axis (MeV/ch)   <',F6.3,'>:',$)
        CALL READF(5,AY1)
      ENDIF
      IF(Istatus.NE.0)STOP

      EXL=AX0+ 0*AX1
      EXH=AX0+(XDIM-1)*AX1
      EYL=AY0+ 0*AY1
      EYH=AY0+(YDIM-1)*AY1
      IF(ITYPE.LE.1) EXH=AX0+(MAXCH)*AX1

      IF(EXL.GT.EXH)THEN
        WAIT=EXH
        EXH=EXL
        EXL=WAIT
      ENDIF
      IF(EYL.GT.EYH)THEN
        WAIT=EYH
        EYH=EYL
        EYL=WAIT
      ENDIF

      WRITE(6,*)' '
      WRITE(6,*)'Give limits (in energies or channels) for the part of'
      WRITE(6,*)'the matrix/spectrum you want to transfer. Use energies'
      WRITE(6,*)'if a0 and a1 is in energy units, otherwise channels'
      IF(ITYPE.LE.1) THEN
        WRITE(6,13)EXL
 13     FORMAT(/' Lower limit             <',F9.3,'>:',$)
        CALL READF(5,EXL)
        WRITE(6,14)EXH
 14     FORMAT( ' Higher limit            <',F9.3,'>:',$)
        CALL READF(5,EXH)
      ELSE
        WRITE(6,15)EXL
 15     FORMAT(/' Lower limit on x-axis   <',F9.3,'>:',$)
        CALL READF(5,EXL)
        WRITE(6,16)EXH
 16     FORMAT( ' Higher limit on x-axis  <',F9.3,'>:',$)
        CALL READF(5,EXH)
        WRITE(6,17)EYL
 17     FORMAT(/' Lower limit on y-axis   <',F9.3,'>:',$)
        CALL READF(5,EYL)
        WRITE(6,18)EYH
 18     FORMAT( ' Higher limit on y-axis  <',F9.3,'>:',$)
        CALL READF(5,EYH)
      ENDIF
      IF(Istatus.NE.0)STOP

      I1=((EXL-AX0)/AX1+0.5)
      I2=((EXH-AX0)/AX1+0.5)
      J1=((EYL-AY0)/AY1+0.5)
      J2=((EYH-AY0)/AY1+0.5)
      IL=MIN(I1,I2)
      IH=MAX(I1,I2)
      JL=MIN(J1,J2)
      JH=MAX(J1,J2)


        
C      ********   WRITING TO FILE   *******
      CALL LENGDE(FILNAM,LIN)
      FILNAM=FILNAM(1:LIN)//'.pc'

      OPEN(UNIT=21,FILE=FILNAM,ACCESS='SEQUENTIAL')
      IF(ITYPE.LE.1) THEN
        DO I=IL,IH
          X=AX0+I*AX1
          Y=rSPEC(IDEST,I)
          IF(X.GE.EXL.AND.X.LE.EXH)WRITE(21,100)X,Y
 100      FORMAT(1X,F12.4,1X,F12.1)
        ENDDO
      ELSE

        DO J=JL,JH
          DO I=IL,IH
            X=AX0+I*AX1
            Y=AY0+J*AY1
            Z=0
            IF(I.GE.0.AND.I.LT.XDIM.AND.J.GE.0.AND.J.LT.YDIM)THEN
              Z=rMAT(IDEST,I,J)
            ENDIF
            IF(X.GE.EXL.AND.X.LE.EXH.AND.Y.GE.EYL.AND.Y.LE.EYH)THEN
              WRITE(21,101)X,Y,Z
            ENDIF
          ENDDO
        ENDDO
 101    FORMAT(1X,F12.4,1X,F12.4,1X,F12.2)
      ENDIF
      CLOSE(21)

      WRITE(6,*)' '
      WRITE(6,*)'Filename of transferred spectrum is:'
      CALL LENGDE(FILNAM,LIN)
      WRITE(6,20)FILNAM(1:LIN)
  20  FORMAT(A40)

      GO TO 9998
      WRITE(6,*)' Reading or writting error'
 9998 CONTINUE
      END 
 
