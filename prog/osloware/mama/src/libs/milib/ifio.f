C$PROG IFIO
      SUBROUTINE IFIO(MODE,LU,LRECL,IDAT,IA,NW,STAT)
C
      INTEGER*4 STAT
      INTEGER*2 ITMH(1024),IDAT(1)
C
C     ****************************************************************
C     MODE=1,2 SAYS READ, WRITE
C
C     IA    = FIRST HALF-WORD ON FILE TO XFER
C     NW    = # OF HALF-WORDS TO XFER
C     LRECL = LOGICAL RECORD LENGTH IN BYTES
C     NWN   = NEXT HALF-WORD ON FILE TO XFER
C     IREC  = RECORD # CONTAINING NWN (1ST REC# ON FILE IS 1)
C     KSIZ  = LOGICAL RECORD SIZE IN HALF-WORDS
C     ****************************************************************
C
      KSIZ=LRECL/2
      NWN=IA
      IB=IA+NW-1
      N=0
      NLEFT=NW
      GO TO (10,110),MODE
C
C     ****************************************************************
C     THIS IS THE READ SECTION
C     ****************************************************************
C
   10 IREC=(NWN-1)/KSIZ+1
      IFW=KSIZ*(IREC-1)+1
      READ(LU,REC=IREC,IOSTAT=STAT)ITMH
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    RETURN
                    ENDIF
C
C     IFW = WORD # OF FIRST HALF-WORD XFERED
C
      ILO=NWN-IFW+1
      IHI=ILO+NLEFT-1
      IF(IHI.GT.1024) IHI=1024
      NAD=IHI-ILO+1
      NLEFT=NLEFT-NAD
      DO 30 I=ILO,IHI
      N=N+1
      IDAT(N)=ITMH(I)
   30 CONTINUE
      IF(N.GE.NW) RETURN
      NWN=NWN+NAD
      GO TO 10
C
C     ****************************************************************
C     THIS IS THE WRITE SECTION
C     ****************************************************************
C
  110 IREC=(NWN-1)/KSIZ+1
      IFW=KSIZ*(IREC-1)+1
      IHI=0
      IF(IFW.EQ.NWN) GO TO 150
C
C     READ IN BLOCK TO BE PARTIALLY REPLACED
C
      READ(LU,REC=IREC,IOSTAT=STAT)ITMH
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    RETURN
                    ENDIF
C
      ILO=NWN-IFW+1
      IHI=ILO+NLEFT-1
      IF(IHI.GT.1024) IHI=1024
      NLEFT=NLEFT-(IHI-ILO)-1
      DO 130 I=ILO,IHI
      N=N+1
      ITMH(I)=IDAT(N)
  130 CONTINUE
C
C     WRITE FIRST BLOCK - FULLY OR PARTIALLY REPLACED
C
      WRITE(LU,REC=IREC,IOSTAT=STAT)ITMH
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    RETURN
                    ENDIF
C
      IREC=IREC+1
C
C     WRITE OUT SUBSEQUENT BLOCKS
C
      IHI=N
  150 ILO=IHI+1
      IF(ILO.GT.NW) RETURN
      IHI=ILO+1023
      IF(IHI.GT.NW) IHI=NW
      N=0
      DO 160 I=ILO,IHI
      N=N+1
      ITMH(N)=IDAT(I)
  160 CONTINUE
      WRITE(LU,REC=IREC,IOSTAT=STAT)ITMH
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    RETURN
                    ENDIF
C
      IREC=IREC+1
      GO TO 150
      END
