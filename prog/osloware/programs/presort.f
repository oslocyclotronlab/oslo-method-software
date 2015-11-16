      PROGRAM PRESORT
      CHARACTER*14 FILENAME
      CHARACTER*7 ARTC
      INTEGER*4 NR,RTC,D801(8192),D802(8192),TDC(8192),BIT(8192),CLO(8192)
      INTEGER*4 CL1(8192),CL2(8192),CL3(8192),CL4(8192),MULT(9),ZEROS,RZ,RO
      INTEGER MS,PAT,D(8),I,J,M,DA,TD1(1024),TD2(1024),TT(1024),TB(1024)
      INTEGER TC1(1024),TC2(1024),TC3(1024),TC4(1024),RS,TCL(1024),CLC,CLR
      REAL R
      DOUBLE PRECISION GS(12)
      LOGICAL CLV
      WRITE(6,*)' ________________________________________'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|           P R E S O R T  1.0           |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| First sorting program  for the data of |'
      WRITE(6,*)'| the Los Alamos  Yb-171(n,2gamma)Yb-172 |'
      WRITE(6,*)'|      experiment  in November 2001      |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Lawrence Livermore National Laboratory |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|         Created:  11/07 - 2002         |'
      WRITE(6,*)'|            Andreas Schiller            |'
      WRITE(6,*)'|________________________________________|'
      FILENAME='run???.lst.new'
      WRITE(6,11)FILENAME
 11   FORMAT('Name of the file to be sorted <',A14,'>:',$)
      CALL READA(5,FILENAME)
      OPEN(2,ERR=99,FILE='/lun1/schiller/n2gamma/new/'//FILENAME,STATUS='OLD')
      A='y'
      WRITE(6,15)A
 15   FORMAT('RTC is important <',A1,'>:',$)
      CALL READA(5,A)
      OPEN(3,ERR=99,FILE='gs.dat',STATUS='OLD')
      READ(3,'(E15.1,E15.1)')(GS(I),I=1,12)
      CLOSE(3)
      WRITE(6,'(A19)')'Gainshift file read'
      WRITE(6,'(A13)')'Start reading'
      WRITE(6,49)NR
 12   CONTINUE
      J=0
      M=1
      READ(2,'(I2,X,A7,X,I3,8(X,I4))',ERR=98,END=50)MS,ARTC,PAT,(D(I),I=1,8)
      NR=NR+1
      IF(ARTC.EQ.'*******')THEN
         RS=RS+1
         RTC=0
      ELSE
         READ(ARTC,'(I7)')RTC
      END IF
      IF((RTC.NE.0).OR.(A.NE.'y'))THEN
         RTC=10000000-RTC
         IF(((RTC.GT.5000000).OR.(RTC.LT.5000)).AND.(A.EQ.'y'))THEN
            RO=RO+1
         ELSE
            IF(A.NE.'y')THEN
               RTC=1
            ELSE
               RTC=RTC/5000
            END IF
            IF(MOD(PAT,2).EQ.1)THEN
               PAT=PAT-1
               J=J+1
               DA=D(J)
               IF(DA.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA=INT((REAL(DA)+R)*GS(2)+GS(1)+1.5)
                  IF((DA.LT.1).OR.(DA.GT.8192))DA=1
                  M=M+1
                  D801(DA)=D801(DA)+1
                  TD1(RTC)=TD1(RTC)+1
               END IF
            END IF
            IF(MOD(PAT,4).EQ.2)THEN
               PAT=PAT-2
               J=J+1
               DA=D(J)
               IF(DA.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA=INT((REAL(DA)+R)*GS(4)+GS(3)+1.5)
                  IF((DA.LT.1).OR.(DA.GT.8192))DA=1
                  M=M+1
                  D802(DA)=D802(DA)+1
                  TD2(RTC)=TD2(RTC)+1
               END IF
            END IF
            IF(MOD(PAT,8).EQ.4)THEN
               PAT=PAT-4
               J=J+1
               DA=D(J)+1
               IF(DA.EQ.1)THEN
                  ZEROS=ZEROS+1
               ELSE
                  M=M+1
                  TDC(DA)=TDC(DA)+1
                  TT(RTC)=TT(RTC)+1
               END IF
            END IF
            IF(MOD(PAT,16).EQ.8)THEN
               PAT=PAT-8
               J=J+1
               DA=D(J)+1
               IF(DA.EQ.1)THEN
                  ZEROS=ZEROS+1
               ELSE
                  M=M+1
                  BIT(DA)=BIT(DA)+1
                  TB(RTC)=TB(RTC)+1
               END IF
            END IF
            IF(MOD(PAT,32).EQ.16)THEN
               CLV=.TRUE.
               PAT=PAT-16
               J=J+1
               DA=D(J)
               IF(DA.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA=INT((REAL(DA)+R)*GS(6)+GS(5)+1.5)
                  IF((DA.LT.1).OR.(DA.GT.8192))DA=1
                  M=M+1
                  CL1(DA)=CL1(DA)+1
                  TC1(RTC)=TC1(RTC)+1
                  CLC=CLC+DA-1                
                  CLR=RTC
               END IF
            END IF
            IF(MOD(PAT,64).EQ.32)THEN
               CLV=.TRUE.
               PAT=PAT-32
               J=J+1
               DA=D(J)
               IF(DA.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA=INT((REAL(DA)+R)*GS(8)+GS(7)+1.5)
                  IF((DA.LT.1).OR.(DA.GT.8192))DA=1
                  M=M+1
                  CL2(DA)=CL2(DA)+1
                  TC2(RTC)=TC2(RTC)+1
                  CLC=CLC+DA-1
                  CLR=RTC
               END IF
            END IF
            IF(MOD(PAT,128).EQ.64)THEN
               CLV=.TRUE.
               PAT=PAT-64
               J=J+1
               DA=D(J)
               IF(DA.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA=INT((REAL(DA)+R)*GS(10)+GS(9)+1.5)
                  IF((DA.LT.1).OR.(DA.GT.8192))DA=1
                  M=M+1
                  CL3(DA)=CL3(DA)+1
                  TC3(RTC)=TC3(RTC)+1
                  CLC=CLC+DA-1
                  CLR=RTC
               END IF
            END IF
            IF(PAT.EQ.128)THEN
               CLV=.TRUE.
               J=J+1
               DA=D(J)
               IF(DA.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA=INT((REAL(DA)+R)*GS(12)+GS(11)+1.5)
                  IF((DA.LT.1).OR.(DA.GT.8192))DA=1
                  M=M+1
                  CL4(DA)=CL4(DA)+1
                  TC4(RTC)=TC4(RTC)+1
                  CLC=CLC+DA-1
                  CLR=RTC
               END IF
            END IF
            IF(CLV.EQ..TRUE.)THEN
               CLC=CLC+1
               IF((CLC.GT.1).AND.(CLC.LE.8192))CLO(CLC)=CLO(CLC)+1
               CLC=0
               TCL(CLR)=TCL(CLR)+1
               CLV=.FALSE.
            END IF   
            MULT(M)=MULT(M)+1
         END IF
      ELSE
         RZ=RZ+1
      END IF
      IF(MOD(NR,100000).EQ.0)THEN
         WRITE(6,49)NR
      END IF
      GO TO 12
 50   CONTINUE
      CLOSE(2)
      WRITE(6,51)NR
 49   FORMAT('Number of lines read: ',I11,', continue reading')
 51   FORMAT('Number of lines read: ',I11,', finished reading')
      WRITE(6,55)RS
 55   FORMAT('Number of RTC stars: ',I11) 
      WRITE(6,52)RZ
 52   FORMAT('Number of RTC zeros: ',I11) 
      WRITE(6,53)RO
 53   FORMAT('Number of RTC out of range: ',I11) 
      WRITE(6,54)ZEROS
 54   FORMAT('Number of zero data words: ',I11) 
      WRITE(6,'(A24,I9)')'Number of mult 0 events ',MULT(1)
      WRITE(6,'(A24,I9)')'Number of mult 1 events ',MULT(2)
      WRITE(6,'(A24,I9)')'Number of mult 2 events ',MULT(3)
      WRITE(6,'(A24,I9)')'Number of mult 3 events ',MULT(4)
      WRITE(6,'(A24,I9)')'Number of mult 4 events ',MULT(5)
      WRITE(6,'(A24,I9)')'Number of mult 5 events ',MULT(6)
      WRITE(6,'(A24,I9)')'Number of mult 6 events ',MULT(7)
      WRITE(6,'(A24,I9)')'Number of mult 7 events ',MULT(8)
      WRITE(6,'(A24,I9)')'Number of mult 8 events ',MULT(9)
      OPEN(2,ERR=99,FILE='ge1.dat')
      WRITE(2,'(8192(I11/))')(D801(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A20)')'File ge1.dat written'
      OPEN(2,ERR=99,FILE='ge2.dat')
      WRITE(2,'(8192(I11/))')(D802(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A20)')'File ge2.dat written'
C      OPEN(2,ERR=99,FILE='tdc.dat')
C      WRITE(2,'(8192(I11/))')(TDC(I),I=1,8192)
C      CLOSE(2)
C      WRITE(6,'(A20)')'File tdc.dat written'
C      OPEN(2,ERR=99,FILE='bit.dat')
C      WRITE(2,'(8192(I11/))')(BIT(I),I=1,8192)
C      CLOSE(2)
C      WRITE(6,'(A20)')'File bit.dat written'
C      OPEN(2,ERR=99,FILE='cl1.dat')
C      WRITE(2,'(8192(I11/))')(CL1(I),I=1,8192)
C      CLOSE(2)
C      WRITE(6,'(A20)')'File cl1.dat written'
C      OPEN(2,ERR=99,FILE='cl2.dat')
C      WRITE(2,'(8192(I11/))')(CL2(I),I=1,8192)
C      CLOSE(2)
C      WRITE(6,'(A20)')'File cl2.dat written'
C      OPEN(2,ERR=99,FILE='cl3.dat')
C      WRITE(2,'(8192(I11/))')(CL3(I),I=1,8192)
C      CLOSE(2)
C      WRITE(6,'(A20)')'File cl3.dat written'
C      OPEN(2,ERR=99,FILE='cl4.dat')
C      WRITE(2,'(8192(I11/))')(CL4(I),I=1,8192)
C      CLOSE(2)
C      WRITE(6,'(A20)')'File cl4.dat written'
      OPEN(2,ERR=99,FILE='clo.dat')
      WRITE(2,'(8192(I11/))')(CLO(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A20)')'File clo.dat written'
      IF(A.EQ.'y')THEN
         OPEN(2,ERR=99,FILE='tg1.dat')
         WRITE(2,'(1024(I11/))')(TD1(I),I=1,1024)
         CLOSE(2)
         WRITE(6,'(A20)')'File tg1.dat written'
         OPEN(2,ERR=99,FILE='tg2.dat')
         WRITE(2,'(1024(I11/))')(TD2(I),I=1,1024)
         CLOSE(2)
         WRITE(6,'(A20)')'File tg2.dat written'
C         OPEN(2,ERR=99,FILE='tt.dat')
C         WRITE(2,'(1024(I11/))')(TT(I),I=1,1024)
C         CLOSE(2)
C         WRITE(6,'(A20)')'File  tt.dat written'
C         OPEN(2,ERR=99,FILE='tb.dat')
C         WRITE(2,'(1024(I11/))')(TB(I),I=1,1024)
C         CLOSE(2)
C         WRITE(6,'(A20)')'File  tb.dat written'
C         OPEN(2,ERR=99,FILE='tc1.dat')
C         WRITE(2,'(1024(I11/))')(TC1(I),I=1,1024)
C         CLOSE(2)
C         WRITE(6,'(A20)')'File tc1.dat written'
C         OPEN(2,ERR=99,FILE='tc2.dat')
C         WRITE(2,'(1024(I11/))')(TC2(I),I=1,1024)
C         CLOSE(2)
C         WRITE(6,'(A20)')'File tc2.dat written'
C         OPEN(2,ERR=99,FILE='tc3.dat')
C         WRITE(2,'(1024(I11/))')(TC3(I),I=1,1024)
C         CLOSE(2)
C         WRITE(6,'(A20)')'File tc3.dat written'
C         OPEN(2,ERR=99,FILE='tc4.dat')
C         WRITE(2,'(1024(I11/))')(TC4(I),I=1,1024)
C         CLOSE(2)
C         WRITE(6,'(A20)')'File tc4.dat written'
         OPEN(2,ERR=99,FILE='tcl.dat')
         WRITE(2,'(1024(I11/))')(TCl(I),I=1,1024)
         CLOSE(2)
         WRITE(6,'(A20)')'File tcl.dat written'
      END IF
      STOP
 98   WRITE(6,102)NR+1
 102  FORMAT('Error during reading record ',I7)
      STOP
 99   WRITE(6,101)
 101  FORMAT('Error during opening file')
      STOP
      END

