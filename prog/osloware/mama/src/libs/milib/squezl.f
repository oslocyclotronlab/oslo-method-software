C$PROG SQUEZL
      SUBROUTINE SQUEZL(IBY,IA,IB)
C*RL      BYTE IBY(1)
C*RL      changed to agreee with the last version of Dmilib...
      BYTE IBY(*),JBY(80)
C
C     ROUTINE TO REMOVE BLANKS FROM IWD (IA THRU IB) AND SQUEEZ LEFT
C
      J=0
      N=0
      DO 10 I=IA,IB
      JTEMP=IBY(I)
      J=J+1
      JBY(J)='20'X
      IF(JTEMP.EQ.'20'X) GO TO 10
      N=N+1
      JBY(N)=JTEMP
   10 CONTINUE
      N=IA
      DO 20 I=1,J
      IBY(N)=JBY(I)
      N=N+1
   20 CONTINUE
      RETURN
      END
