      SUBROUTINE TRAX(TX,XI,TY,YI,K,ixAxis)
C CHANGED FOR THE SPY PROGRAM!!!!!
C      draw axes in graphics window....
C           x-axis from xi to xi+tx....
C           y-axis from yi to yi+ty....
C           k = 1/2/3 for lin/sqrt/log y-axis on left hand side....
C           k = -1/-2/-3 for lin/sqrt/log y-axis on right hand side....
C           k = 0 for linear y-axis, no axes drawn....

      REAL        TX,XI,TY,YI
      INTEGER     K

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3 

      REAL DEX(14)
      CHARACTER*12 BC

      IYFLAG=1

      FDX = TX
      FX0 = XI
      FY0 = YI
      FDY = TY

      X1 = FX0              !channel-region
      X2 = FX0+FDX
      Y1 = FY0
      Y2 = FY0+TY


C Gives graphic window parameters to global parameters of minig_x
      CALL PUTGLOBALS(FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG)


      CALL CVXY(X2,Y2,IXmax,IYmax,1)  !max pixelvalue for x- and y-axis
 
      ax0=cal(1,1,1,1)
      ax1=cal(1,1,1,2)
      ax2=cal(1,1,1,3)
      ay0=0.
      ay1=1.
      ay2=0.

      EX1=ax0+ax1*(X1)+ax2*(X1)*(X1)
      EX2=ax0+ax1*(X2)+ax2*(X2)*(X2)
      EY1=Y1
      EY2=Y2

      ETX=ABS(EX2-EX1)
      ETY=ABS(EY2-EY1)

C       draw x-axis....
      CALL PSPOT(X1,Y1)
      CALL VECT(X2,Y1)
      CALL GRAX(EX1,EX2,DEX,NV,1)
C             write(6,*)'ex1,ex2,nv',ex1,ex2,nv
      IXold=10000    
      DO 20 N=1,NV
         V=DEX(N)
         ch=V
         ch=CHAN(V,ax0,ax1,ax2)+0.5
C                write(6,*)'N,V,ch',N,V,ch
         IF (ch.EQ.X1) GO TO 20
         CALL CVXY(ch,Y1,IX,IY,1)
         CALL MSPOT(IX,IY+3)
         CALL IVECT(IX,IY-0)
         CALL MSPOT(IX,IY-1)
         IF((IABS(IXold-IX).GT.20).AND.IX.LT.(IXmax-2).AND.ixAxis.EQ.1)THEN
            IF(V.LT.0)V=V+24.
            WRITE(BC,6,ERR=8) V
5           FORMAT(F5.3,' ')
6           FORMAT(F5.1,' ')
7           FORMAT(F8.0,' ')

8           DO 10 I=1,7
               IF (BC(I:I).NE.' ') GO TO 15
10          CONTINUE
15          NC=8-I
            CALL PUTG(BC(I:8),-NC,6)
            IXold=IX
         ENDIF
20    CONTINUE

C     draw y-axis....
      X=X1
      IF (K.LT.0) X=X2
      CALL PSPOT(X,Y1)
      CALL VECT(X,Y2)
      CALL GRAX(EY1,EY2,DEX,NV,IYFLAG)
      DO 40 N=1,NV
         V=DEX(N)
         ch=V
         IF (ch.LE.Y1.OR.ch.GE.Y2) GO TO 40
         CALL CVXY(X,ch,IX,IY,1)
         CALL MSPOT(IX+3,IY)
         CALL IVECT(IX-0,IY)
         IF ((IYFLAG.EQ.3.OR.(N+1)/4*4.EQ.N+1).AND.IY.LT.IYmax-15) THEN
C              write value in graphtext....
            IF(ETY.LE.1.0)THEN
               WRITE(BC,5,ERR=25)V
            ELSEIF(ETY.LE.15.0) THEN
               WRITE(BC,6,ERR=25) V
            ELSE
               WRITE(BC,7,ERR=25) V
            ENDIF
25          DO 30 I=1,7
               IF (BC(I:I).NE.' ') GO TO 35
30          CONTINUE
35          NC=8-I
            IF (K.LT.0) THEN
               CALL MSPOT(IX-7,IY-7)
               CALL PUTG(BC(I:8),-NC,8)
            ELSE
               CALL MSPOT(IX+6,IY-7)
               CALL PUTG(BC(I:8),-NC,2)
            ENDIF
         ENDIF
40    CONTINUE
      RETURN
      END

      
      FUNCTION CHAN(E,a0,a1,a2)
C Finds the most probable solution of the 2. degree polynom:
C E=a0+a1*ch+a2*ch*ch. We assume ch to be positive (channels),
C and that a2 only makes a small change in E

C Without a1 and/or a2 term
      chan=0
      IF(a1.EQ.0.AND.a2.EQ.0)RETURN
      IF(a1.NE.0)chan=(E-a0)/a1      
      IF(a2.EQ.0)THEN
        IF(chan.LT.0)chan=0
        RETURN
      ENDIF

      tch=chan                          !to test against

C Assuming full equation with a0,a1 and a2
      chan=0
      root=a1*a1-4.*a2*(a0-E)
      IF(root.LT.0)RETURN
      ch1=(-a1+sqrt(root))/(2.*a2)
      ch2=(-a1-sqrt(root))/(2.*a2)
      t1=ABS(ch1-tch)
      t2=ABS(ch2-tch)
      IF(t1.LE.t2.AND.ch1.GE.0)chan=ch1
      IF(t2.LE.t1.AND.ch2.GE.0)chan=ch2
    
      RETURN

      END
