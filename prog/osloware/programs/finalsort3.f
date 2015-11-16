      PROGRAM MAINSORT
      CHARACTER*14 FILENAME
      CHARACTER*7 ARTC
      INTEGER*4 NR,RTC
      INTEGER MS,PAT,D(8),I,J
      INTEGER CLC
      INTEGER*4 SUM0(8192),SUM2(8192),SUM_1(8192),SUM_2(8192)
      INTEGER MU,DA1,DA2,DA3,DA4,DA5,DA6,DA7,DA8,RB
      INTEGER GT(12)
      REAL R
      DOUBLE PRECISION GS(12)
      WRITE(6,*)' ________________________________________'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|        F I N A L S O R T 3  1.0        |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Fifth sorting program  for the data of |'
      WRITE(6,*)'| the Los Alamos  Yb-171(n,2gamma)Yb-172 |'
      WRITE(6,*)'|      experiment  in November 2001      |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Lawrence Livermore National Laboratory |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|         Created:  08/21 - 2003         |'
      WRITE(6,*)'|            Andreas Schiller            |'
      WRITE(6,*)'|________________________________________|'
      FILENAME='run???.lst.new'
      WRITE(6,11)FILENAME
 11   FORMAT('Name of the file to be sorted <',A14,'>:',$)
      CALL READA(5,FILENAME)
      OPEN(2,ERR=99,FILE='/lun1/schiller/n2gamma/new/'//FILENAME,STATUS='OLD')
      OPEN(3,ERR=99,FILE='gs.dat',STATUS='OLD')
      READ(3,'(E15.1,E15.1)')(GS(I),I=1,12)
      CLOSE(3)
      WRITE(6,'(A19)')'Gainshift file read'
      WRITE(6,'(A13)')'Start reading'
      WRITE(6,49)NR
 12   CONTINUE
      CLC=1
      J=0
      MU=0
      DA1=1
      DA2=1
      DA3=1
      DA4=1
      DA5=1
      DA6=1
      DA7=1
      DA8=1
      READ(2,'(I2,X,A7,X,I3,8(X,I4))',ERR=98,END=50)MS,ARTC,PAT,(D(I),I=1,8)
      NR=NR+1
      IF(ARTC.EQ.'*******')THEN
         RTC=0
      ELSE
         READ(ARTC,'(I7)')RTC
      END IF
      IF(RTC.NE.0)THEN
         RTC=10000000-RTC
         IF((RTC.LE.5000000).AND.(RTC.GE.5000))THEN
            RTC=RTC/5000
            IF(MOD(PAT,2).EQ.1)THEN
               PAT=PAT-1
               J=J+1
               DA1=D(J)
               IF(DA1.EQ.0)THEN
                  DA1=1
               ELSE
                  R=RAND(0)-0.5
                  DA1=INT((REAL(DA1)+R)*GS(2)+GS(1)+1.5)
                  IF((DA1.LE.1).OR.(DA1.GT.8192))THEN
                     DA1=1
                  ELSE
                     MU=MU+1
                  END IF   
               END IF
            END IF
            IF(MOD(PAT,4).EQ.2)THEN
               PAT=PAT-2
               J=J+1
               DA2=D(J)
               IF(DA2.EQ.0)THEN
                  DA2=1
               ELSE
                  R=RAND(0)-0.5
                  DA2=INT((REAL(DA2)+R)*GS(4)+GS(3)+1.5)
                  IF((DA2.LE.1).OR.(DA2.GT.8192))THEN
                     DA2=1
                  ELSE
                     MU=MU+1
                  END IF
               END IF
            END IF
            IF(MOD(PAT,8).EQ.4)THEN
               PAT=PAT-4
               J=J+1
               DA3=D(J)
               IF(DA3.EQ.0)THEN
                  DA3=1
               ELSE
                  DA3=DA3+1
                  IF((DA3.LT.1).OR.(DA3.GT.8192))DA3=1
               END IF
            END IF
            IF(MOD(PAT,16).EQ.8)THEN
               PAT=PAT-8
               J=J+1
               DA4=D(J)
               IF(DA4.EQ.0)THEN
                  DA4=1
               ELSE
                  DA4=DA4+1
                  IF((DA4.LT.1).OR.(DA4.GT.8192))DA4=1
               END IF
            END IF
            IF(MOD(PAT,32).EQ.16)THEN
               PAT=PAT-16
               J=J+1
               DA5=D(J)
               IF(DA5.NE.0)THEN
                  R=RAND(0)-0.5
                  DA5=INT((REAL(DA5)+R)*GS(6)+GS(5)+1.5)
                  IF((DA5.LT.1).OR.(DA5.GT.8192))DA5=1
                  CLC=CLC+DA5-1                
               END IF
            END IF
            IF(MOD(PAT,64).EQ.32)THEN
               PAT=PAT-32
               J=J+1
               DA6=D(J)
               IF(DA6.NE.0)THEN
                  R=RAND(0)-0.5
                  DA6=INT((REAL(DA6)+R)*GS(8)+GS(7)+1.5)
                  IF((DA6.LT.1).OR.(DA6.GT.8192))DA6=1
                  CLC=CLC+DA6-1
               END IF
            END IF
            IF(MOD(PAT,128).EQ.64)THEN
               PAT=PAT-64
               J=J+1
               DA7=D(J)
               IF(DA7.NE.0)THEN
                  R=RAND(0)-0.5
                  DA7=INT((REAL(DA7)+R)*GS(10)+GS(9)+1.5)
                  IF((DA7.LT.1).OR.(DA7.GT.8192))DA7=1
                  CLC=CLC+DA7-1
               END IF
            END IF
            IF(PAT.EQ.128)THEN
               J=J+1
               DA8=D(J)
               IF(DA8.NE.0)THEN
                  R=RAND(0)-0.5
                  DA8=INT((REAL(DA8)+R)*GS(12)+GS(11)+1.5)
                  IF((DA8.LT.1).OR.(DA8.GT.8192))DA8=1
                  CLC=CLC+DA8-1
               END IF
            END IF
            IF((CLC.LE.1).OR.(CLC.GT.8192))THEN
               CLC=1
            ELSE
               MU=MU+1
            END IF
            IF((MU.EQ.2).AND.(RTC.GE.12).AND.(RTC.LE.966))THEN
               RB=DA1+DA2+CLC-2
               IF(((RB.GE.5156).AND.(RB.LE.5212)).OR.((RB.GE.6001).AND.(RB.LE.6088)))THEN
                  IF(((DA1.EQ.1).AND.(DA3.GE.1100).AND.(DA3.LE.1350)).OR.
     +               ((DA2.EQ.1).AND.(DA3.GE.3270).AND.(DA3.LE.3800)).OR.
     +               ((CLC.EQ.1).AND.(DA3.GE.5300).AND.(DA3.LE.5900)))THEN
                     IF((RB.GE.6001).AND.(RB.LE.6008))THEN
                        SUM2(DA1)=SUM2(DA1)-1
                        SUM2(DA2)=SUM2(DA2)-1
                        SUM2(CLC)=SUM2(CLC)-1
                        GT(1)=GT(1)+1
                     END IF
                     IF((RB.GE.6009).AND.(RB.LE.6023))THEN
                        SUM2(DA1)=SUM2(DA1)+1
                        SUM2(DA2)=SUM2(DA2)+1
                        SUM2(CLC)=SUM2(CLC)+1
                        GT(2)=GT(2)+1
                     END IF
                     IF((RB.GE.6024).AND.(RB.LE.6030))THEN
                        SUM2(DA1)=SUM2(DA1)-1
                        SUM2(DA2)=SUM2(DA2)-1
                        SUM2(CLC)=SUM2(CLC)-1
                        GT(3)=GT(3)+1
                     END IF
                     IF((RB.GE.6063).AND.(RB.LE.6069))THEN
                        SUM0(DA1)=SUM0(DA1)-1
                        SUM0(DA2)=SUM0(DA2)-1
                        SUM0(CLC)=SUM0(CLC)-1
                        GT(4)=GT(4)+1
                     END IF
                     IF((RB.GE.6070).AND.(RB.LE.6082))THEN
                        SUM0(DA1)=SUM0(DA1)+1
                        SUM0(DA2)=SUM0(DA2)+1
                        SUM0(CLC)=SUM0(CLC)+1
                        GT(5)=GT(5)+1
                     END IF
                     IF((RB.GE.6083).AND.(RB.LE.6088))THEN
                        SUM0(DA1)=SUM0(DA1)-1
                        SUM0(DA2)=SUM0(DA2)-1
                        SUM0(CLC)=SUM0(CLC)-1
                        GT(6)=GT(6)+1
                     END IF
                     IF((RB.GE.5189).AND.(RB.LE.5194))THEN
                        SUM_1(DA1)=SUM_1(DA1)-1
                        SUM_1(DA2)=SUM_1(DA2)-1
                        SUM_1(CLC)=SUM_1(CLC)-1
                        GT(7)=GT(7)+1
                     END IF
                     IF((RB.GE.5195).AND.(RB.LE.5206))THEN
                        SUM_1(DA1)=SUM_1(DA1)+1
                        SUM_1(DA2)=SUM_1(DA2)+1
                        SUM_1(CLC)=SUM_1(CLC)+1
                        GT(8)=GT(8)+1
                     END IF
                     IF((RB.GE.5207).AND.(RB.LE.5212))THEN
                        SUM_1(DA1)=SUM_1(DA1)-1
                        SUM_1(DA2)=SUM_1(DA2)-1
                        SUM_1(CLC)=SUM_1(CLC)-1
                        GT(9)=GT(9)+1
                     END IF
                     IF((RB.GE.5156).AND.(RB.LE.5161))THEN
                        SUM_2(DA1)=SUM_2(DA1)-1
                        SUM_2(DA2)=SUM_2(DA2)-1
                        SUM_2(CLC)=SUM_2(CLC)-1
                        GT(10)=GT(10)+1
                     END IF
                     IF((RB.GE.5162).AND.(RB.LE.5174))THEN
                        SUM_2(DA1)=SUM_2(DA1)+1
                        SUM_2(DA2)=SUM_2(DA2)+1
                        SUM_2(CLC)=SUM_2(CLC)+1
                        GT(11)=GT(11)+1
                     END IF
                     IF((RB.GE.5175).AND.(RB.LE.5181))THEN
                        SUM_2(DA1)=SUM_2(DA1)-1
                        SUM_2(DA2)=SUM_2(DA2)-1
                        SUM_2(CLC)=SUM_2(CLC)-1
                        GT(12)=GT(12)+1
                     END IF
                  END IF
               END IF
            END IF
         END IF
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
      WRITE(6,'(A25,I5)')'Number of gate 1  events ',GT(1)
      WRITE(6,'(A25,I5)')'Number of gate 2  events ',GT(2)
      WRITE(6,'(A25,I5)')'Number of gate 3  events ',GT(3)
      WRITE(6,'(A25,I5)')'Number of gate 4  events ',GT(4)
      WRITE(6,'(A25,I5)')'Number of gate 5  events ',GT(5)
      WRITE(6,'(A25,I5)')'Number of gate 6  events ',GT(6)
      WRITE(6,'(A25,I5)')'Number of gate 7  events ',GT(7)
      WRITE(6,'(A25,I5)')'Number of gate 8  events ',GT(8)
      WRITE(6,'(A25,I5)')'Number of gate 9  events ',GT(9)
      WRITE(6,'(A25,I5)')'Number of gate 10 events ',GT(10)
      WRITE(6,'(A25,I5)')'Number of gate 11 events ',GT(11)
      WRITE(6,'(A25,I5)')'Number of gate 12 events ',GT(12)
      OPEN(2,ERR=99,FILE='sum0s.dat')
      WRITE(2,'(8192(I11/))')(SUM0(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A22)')'File sum0s.dat written'
      OPEN(2,ERR=99,FILE='sum2s.dat')
      WRITE(2,'(8192(I11/))')(SUM2(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A22)')'File sum2s.dat written'
      OPEN(2,ERR=99,FILE='sum_1s.dat')
      WRITE(2,'(8192(I11/))')(SUM_1(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A23)')'File sum_1s.dat written'
      OPEN(2,ERR=99,FILE='sum_2s.dat')
      WRITE(2,'(8192(I11/))')(SUM_2(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A23)')'File sum_2s.dat written'
      STOP
 98   WRITE(6,102)NR+1
 102  FORMAT('Error during reading record ',I7)
      STOP
 99   WRITE(6,101)
 101  FORMAT('Error during opening file')
      STOP
      END

