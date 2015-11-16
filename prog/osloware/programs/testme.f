      PROGRAM TESTME
      INTEGER REP,N(3)
      READ(5,'(I1)')REP
      N(1)=1
      N(2)=2
      N(3)=3
      WRITE(6,10)(N(I),I=1,REP)
 10   FORMAT(3I3)
      STOP
      END
