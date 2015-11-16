C$PROG SPKIO
      SUBROUTINE SPKIO(MODE,LU,IDN,IHED,MAXH,IDAT,NDX,NCH,IERR)
C
      INTEGER*4 IHED(32),IDAT(1),NDX(1)
C
      INTEGER*4 IDIR(512),ITM(32),STAT
C
      EQUIVALENCE (NID,IDIR(3)),(NXWD,IDIR(4))
C
      EQUIVALENCE (ID  ,ITM(1)),
     &            (NWDH,ITM(9)),
     &            (LENC,ITM(12)),
     &            (LRAW,ITM(13)),
     &            (LSCA,ITM(14)),
     &            (MINC,ITM(16)),
     &            (MAXC,ITM(17))
C
      DATA I33,ITM/33,32*0/
C
      DATA LRECL,LLU,MODL/2048,0,'    '/
C
C     **************************************************************
C     IERR = 0 SAYS ALL OK
C     IERR = 1 SAYS REQUESTED ID NOT FOUND
C     IERR = 2 SAYS INVALID VALUE OF "NDX"
C     IERR = 3 SAYS DIRECTORY OVERFLOW
C     IERR = 4 SAYS I/O ERROR OF SOME SORT
C     IERR = 5 SAYS ILLEGAL REQUEST MODE
C     IERR = 6 SAYS ID TO BE DELETED NOT IN DIRECTORY (NOT USED)
C     IERR = 7 SAYS ID TO BE STORED ALREADY IN DIRECTORY
C     IERR = 8 SAYS FILE NOT OPEN
C     **************************************************************
C     MODE = 0,1,2 SAYS INITIALIZE, INPUT, OUTPUT
C     MODE = 3 SAYS LIST THE DIRECTORY        (NO LONGER SUPPORTED)
C     MODE = 4 SAYS DELETE IDN FROM DIRECTORY (NO LONGER SUPPORTED)
C     MODE = 5 SAYS DISPLAY THE DIRECTORY     (NO LONGER SUPPORTED)
C     MODE = 6 SAYS RETURN ID-LIST IN IDAT (IDAT(1) = # OF ID'S)
C     LU   = LOGICAL UNIT #
C     IDN  = REQUESTED ID #
C     IHED - ARRAY TO CONTAIN HEADER
C     MAXH = MAXIMUM LENGTH OF IHED IN HALF-WORDS
C     IDAT - ARRAY TO CONTAIN DATA
C     NDX  - ARRAY TO CONTAIN INDICES OF 1ST CHANNEL TO XFER
C     NCH  = # OF CHANNELS TO XFER
C     **************************************************************
C     IDIR - CONTAINS THE DIRECTORY
C     IDIR(1), IDIR(2) CONTAINS 8-CHARACTER "DATA TYPE"
C     IDIR(3) CONTAINS "NID" (# OF SPECTRA ON FILE)
C     IDIR(4) CONTAINS "NXWD" (NEXT HALF-WORD # ON FILE TO USE)
C     IDIR(5) CONTAINS ID #
C     IDIR(6) CONTAINS HALF-WORD # WHERE HEADER STARTS
C     **************************************************************
C
C
C     HEADER STRUCTURE
C
C     WORD#    CONTENTS
C         1    ID #
C     2 - 4    PARAMETER LABEL (FROM .HIS-FILE)
C     5 - 7    RESERVED FOR (DATE - TIME)
C         8    BYTES/CHANNEL = -4  (NO LONGER NEEDED BUT SET TO -4)
C                                  (SO THAT OLD SPKIO WILL GET ERR)
C         9    HEADER LENGTH (HALF WORDS)
C        10    DATA LENGTH (HALF WORDS)          (NO LONGER USED)
C        11    DATA DIMENSIONALITY (# PARMS = 1) (NO LONGER USED)
C        12    HIST LENGTH (MAX # CHANS TO BE RETURNED)
C        13    LENGTH OF RAW    PARAMETER (PWR OF 2)
C        14    LENGTH OF SCALED PARAMETER (PWR OF 2)
C        15    DATA RECORD BLKSIZE (BYTES) FOR MAG TAPES
C              (0 IMPLIES ONE CONTIGUOUS DATA RECORD)
C        16    MIN NON-ZERO CHANNEL #
C        17    MAX NON-ZERO CHANNEL #
C     18-20    CALIBRATION CONSTANTS (UP TO 3)
C     21-22    RESERVED
C     23-32    TITLE (40 BYTES)
C     **************************************************************
C
      IERR=0
      IF(LU.LE.0) GO TO 370
      IF(MODE.GE.1.AND.MODE.LE.6) GO TO 30
      IF(MODE.EQ.0) GO TO 10
      IERR=5
      RETURN
C
C     RESET THE DIRECTORY
C
   10 NDO=LRECL/4
      DO 20 I=1,NDO
      IDIR(I)=0
   20 CONTINUE
      IDIR(1)='HHIR'
      IDIR(2)='FSPK'
      IDIR(4)=1025
      IREC=1
      WRITE(LU,REC=IREC,IOSTAT=STAT)IDIR
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 340
                    ENDIF
      RETURN
C
C     **************************************************************
C     READ IN DIRECTORY FOR ALL MODES BUT INIT
C     **************************************************************
C
   30 CALL IFIO(1,LU,LRECL,IDIR(1),1,1024,STAT)
      IF(STAT.NE.0) GO TO 340
      IF(MODE.EQ.1) GO TO 100
      IF(MODE.EQ.2) GO TO 200
      IF(MODE.EQ.6) GO TO 60
      IERR=5
      RETURN
C
C     ************************************** RETURN ID-LIST IN IDAT
C
   60 IA=5
      IB=2*IDIR(3)+4
      IDAT(1)=IDIR(3)
      N=1
      DO 65 I=IA,IB,2
      N=N+1
      IDAT(N)=IDIR(I)
   65 CONTINUE
      RETURN
C
C     **************************************************************
C     MODE = INPUT  -  TEST FOR REQUESTED ID
C     **************************************************************
C
  100 N=LOCID(IDIR,1,NID,IDN)
      IF(N.GT.0) GO TO 120
      IERR=1
      RETURN
  120 NWN=IDIR(N+1)
      LOCH=NWN
C
C     READ FIRST 64 HALF-WORDS OF HEADER INTO ITM
C
      CALL IFIO(1,LU,LRECL,ITM(1),NWN,64,STAT)
      IF(STAT.NE.0) GO TO 340
      NDO=32
      IF(NDO.GT.MAXH/2) NDO=MAXH/2
      DO 130 I=1,NDO
      IHED(I)=ITM(I)
  130 CONTINUE
C
C     ************************************** READ MORE HEADER
C
      IF(NWDH.LE.64.OR.MAXH.LE.64) GO TO 140
      NWN=NWN+64
      IF(MAXH.GE.NWDH) NWD=NWDH-64
      IF(MAXH.LT.NWDH) NWD=MAXH-64
      CALL IFIO(1,LU,LRECL,IHED(I33),NWN,NWD,STAT)
      IF(STAT.NE.0) GO TO 340
C
C     --------------------------------------------------------------
C     COMPUTE 1ST HALF-WORD # ON DISK TO XFER - NWN
C             # OF CHANNELS TO XFER           - NC
C             STARTING INDEX IN IDAT          - IDX
C     --------------------------------------------------------------
C
  140 IF(IHED(10).LE.0) IHED(10)=2*(MAXC+1) !# HALF-WDS DATA
      IF(IHED(14).GT.IHED(12)) IHED(12)=IHED(14) !MODIFY # CHANS
      IF(NCH.LE.0) RETURN                   !TST FOR NO-DATA REQUEST
      IF(IHED(8).LT.0) GO TO 150            !TST FOR NEW FORM
C
C     ************************************** OLD SPK-FILE FORM
C
C
      IF(NDX(1).LE.0.OR.NDX(1).GT.LENC)  GO TO 300 !TST FOR LEGAL
C
      NWN=LOCH+NWDH+2*(NDX(1)-1)            !1ST HALF-WD# ON DISK
      NC=NCH                                !# CHANNELS REQUESTED
      NCMAX=LENC-NDX(1)+1                   !MAX # CHANS AVAILABLE
      IDX=1                                 !1ST INDEX IN IDAT
      GO TO 170                             !GO DO READ
C
C     ************************************** NEW SPK-FILE FORM
C
  150 DO 155 I=1,NCH                        !ZERO REQUESTED # CHANS
      IDAT(I)=0
  155 CONTINUE
C
      MIND=MINC+1                           !MIN DATA INDEX
      MAXD=MAXC+1                           !MAX DATA INDEX
      MINR=NDX(1)                           !MIN INDEX REQUESTED
      MAXR=NDX(1)+NCH-1                     !MAX INDEX REQUESTED
C
      IF(MINR.GT.MAXD) RETURN               !TST FOR NO OVERLAP
      IF(MAXR.LT.MIND) RETURN               !TST FOR NO OVERLAP
C
      IF(MINR.GT.MIND) GO TO 160            !TST FOR ALL ABOVE MIN
C                                           !OTHERWISE, START AT
      NWN=LOCH+NWDH                         !1ST ELEMENT OF DATA
      IDX=MIND-MINR+1                       !1ST INDEX IN IDAT
      NC=NCH-IDX+1                          !# CHANS TO READ
      NCMAX=MAXD-MIND+1                     !MAX # CHANS AVAILABLE
      GO TO 170                             !GO READ IT IN
C
  160 NWN=LOCH+NWDH+2*(MINR-MIND)           !1ST HALF-WD# ON DISK
      IDX=1                                 !1ST INDEX IN IDAT
      NC=NCH                                !# CHANS TO READ
      NCMAX=MAXD-MINR+1                     !MAX # CHANS AVAILABLE
C
  170 IF(NC.GT.NCMAX) NC=NCMAX              !LIMIT # CHANS TO THAT
C                                           !WHICH IS AVAILABLE
      NWD=2*NC                              !# OF HALF-WDS TO READ
C
      CALL IFIO(1,LU,LRECL,IDAT(IDX),NWN,NWD,STAT)
C
      IF(STAT.NE.0) GO TO 340
      RETURN
C
C     **************************************************************
C     MODE = OUTPUT  -  SEE IF ID# ALREADY EXISTS
C     **************************************************************
C
  200 ID=IHED(1)                            !ID NUMBER
C
      IF(NID.LE.0)   GO TO 220              !TST FOR EMPTY DIRECTORY
      N=LOCID(IDIR,1,NID,ID)                !IF NOT, DOES ID EXIST
      IF(N.NE.0)     GO TO 360              !IF YES, RETURN ERROR
      IF(NID.GE.254) GO TO 320              !TST FOR DIRECTORY FULL
C
  220 NC=IHED(12)                           !# CHANS FROM HEADER
      NWDH=2*((IHED(9)+1)/2)                !# HALF-WDS OF HEADER
C
C     **************************************************************
C     ADD TO FILE AND UPDATE DIRECTORY
C     **************************************************************
C
      NID=NID+1                             !INC # OF ID'S STORED
      II=2*NID+3                            !DIRECTORY INDEX FOR ID
      IDIR(II)=ID                           !STORE ID IN DIRECTORY
      IDIR(II+1)=NXWD                       !STORE DISK LOC IN DIR
C
      DO 240 I=1,NC                         !LOOP ON # CHANS TO
      IF(IDAT(I).NE.0) GO TO 250            !FIND 1ST NON-ZERO DATA
  240 CONTINUE
C                                           !IF ALL 0, STORE 1 CHAN
      MINC=0                                !MIN CHAN #
      MAXC=0                                !MAX CHAN #
      IA=1                                  !IDAT INDEX
      NWDD=2                                !# HALF-WDS TO STORE
      GO TO 280                             !GO STORE IT
C
  250 MINC=I-1                              !SET MIN CHAN#
      IA=I+1                                !INIT LOOP TO FIND LAST
      N=NC                                  !NON-ZERO DATA
      DO 260 I=IA,NC                        !REVERSE LOOP ON IDAT
      IF(IDAT(N).NE.0) GO TO 270            !TST FOR NON-ZERO
      N=N-1                                 !DEC IDAT-INDEX
  260 CONTINUE
      N=IA                                  !IF ALL 0, SET TO IA
  270 MAXC=N-1                              !SET MAX CHAN#
      NWDD=2*(MAXC-MINC+1)                  !# HALF-WDS DATA TO STOR
C
  280 IDX=MINC+1                            !START INDEX IN IDAT
C
      IH8=IHED(8)                           !SAVE IHED(8)
      IH16=IHED(16)                         !SAVE IHED(16)
      IH17=IHED(17)                         !SAVE IHED(17)
C
      IHED(8)=-4                            !SO OLD SPKIO GETS ERROR
      IHED(16)=MINC                         !STORE MINC IN IHED(16)
      IHED(17)=MAXC                         !STORE MAXC IN IHED(17)
C
      CALL IFIO(2,LU,LRECL,IHED(1),NXWD,NWDH,STAT)  !STORE HEADER
C
      IHED(8)=IH8                           !RESTORE IHED(8)
      IHED(16)=IH16                         !RESTORE IHED(16)
      IHED(17)=IH17                         !RESTORE IHED(17)
C
      IF(STAT.NE.0) GO TO 340               !TST FOR STORE ERROR
      NXWD=NXWD+NWDH                        !NEXT DISK ADDRESS
C
      CALL IFIO(2,LU,LRECL,IDAT(IDX),NXWD,NWDD,STAT)  !STORE DATA
C
      IF(STAT.NE.0) GO TO 340               !TST FOR STORE ERROR
      NXWD=NXWD+NWDD                        !NEXT DISK ADDRESS
C
      CALL IFIO(2,LU,LRECL,IDIR(1),1,1024,STAT)      !STORE DIRECTORY
C
      IF(STAT.NE.0) GO TO 340               !TST FOR STORE ERROR
      RETURN
C
C     **************************************************************
C     RETURN ERROR CODES
C     **************************************************************
C
  300 IERR=2
      RETURN
  320 IERR=3
      RETURN
  340 IERR=4
      RETURN
C*350 IERR=6
C*    RETURN
  360 IERR=7
      RETURN
  370 IERR=8
      RETURN
      END
