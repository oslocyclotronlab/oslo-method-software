      PROGRAM MAINSORT
      CHARACTER*14 FILENAME
      CHARACTER*7 ARTC
      INTEGER*4 NR,RTC,RTC2
      INTEGER*4 MULT(9),ZEROS,RZ,RO
      INTEGER MS,PAT,D(8),I,J,M
      INTEGER RS,CLC,CLR
      INTEGER*4 SING1R(8192),SING2R(8192),SINGCR(8192)
      INTEGER*4 SING1T(8192),SING2T(8192),SINGCT(8192)
      INTEGER*4 SUM12R(8192),SUM1CR(8192),SUM2CR(8192)
      INTEGER*4 SUM12T(8192),SUM1CT(8192),SUM2CT(8192)
      INTEGER*4 TOF(1024),TOF2(1024)
      INTEGER*4 GM(3)
      INTEGER MU,DA1,DA2,DA3,DA4,DA5,DA6,DA7,DA8,RB
      REAL R
      DOUBLE PRECISION GS(12)
      WRITE(6,*)' ________________________________________'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|          M A I N S O R T  1.0          |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Second sorting program for the data of |'
      WRITE(6,*)'| the Los Alamos  Yb-171(n,2gamma)Yb-172 |'
      WRITE(6,*)'|      experiment  in November 2001      |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Lawrence Livermore National Laboratory |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|         Created:  11/22 - 2002         |'
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
      M=1
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
         RS=RS+1
         RTC=0
      ELSE
         READ(ARTC,'(I7)')RTC
      END IF
      IF(RTC.NE.0)THEN
         RTC=10000000-RTC
         IF((RTC.GT.5000000).OR.(RTC.LT.5000))THEN
            RO=RO+1
         ELSE
            RTC2=0
            IF(RTC.LT.100000)RTC2=RTC/100
            RTC=RTC/5000
            IF(MOD(PAT,2).EQ.1)THEN
               PAT=PAT-1
               J=J+1
               DA1=D(J)
               IF(DA1.EQ.0)THEN
                  ZEROS=ZEROS+1
                  DA1=1
               ELSE
                  R=RAND(0)-0.5
                  DA1=INT((REAL(DA1)+R)*GS(2)+GS(1)+1.5)
                  IF((DA1.LE.1).OR.(DA1.GT.8192))THEN
                     DA1=1
                  ELSE
                     MU=MU+1
                  END IF   
                  M=M+1
               END IF
            END IF
            IF(MOD(PAT,4).EQ.2)THEN
               PAT=PAT-2
               J=J+1
               DA2=D(J)
               IF(DA2.EQ.0)THEN
                  ZEROS=ZEROS+1
                  DA2=1
               ELSE
                  R=RAND(0)-0.5
                  DA2=INT((REAL(DA2)+R)*GS(4)+GS(3)+1.5)
                  IF((DA2.LE.1).OR.(DA2.GT.8192))THEN
                     DA2=1
                  ELSE
                     MU=MU+1
                  END IF
                  M=M+1
               END IF
            END IF
            IF(MOD(PAT,8).EQ.4)THEN
               PAT=PAT-4
               J=J+1
               DA3=D(J)
               IF(DA3.EQ.0)THEN
                  ZEROS=ZEROS+1
                  DA3=1
               ELSE
                  DA3=DA3+1
                  IF((DA3.LT.1).OR.(DA3.GT.8192))DA3=1
                  M=M+1
               END IF
            END IF
            IF(MOD(PAT,16).EQ.8)THEN
               PAT=PAT-8
               J=J+1
               DA4=D(J)
               IF(DA4.EQ.0)THEN
                  ZEROS=ZEROS+1
                  DA4=1
               ELSE
                  DA4=DA4+1
                  IF((DA4.LT.1).OR.(DA4.GT.8192))DA4=1
                  M=M+1
               END IF
            END IF
            IF(MOD(PAT,32).EQ.16)THEN
               PAT=PAT-16
               J=J+1
               DA5=D(J)
               IF(DA5.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA5=INT((REAL(DA5)+R)*GS(6)+GS(5)+1.5)
                  IF((DA5.LT.1).OR.(DA5.GT.8192))DA5=1
                  M=M+1
                  CLC=CLC+DA5-1                
                  CLR=RTC
               END IF
            END IF
            IF(MOD(PAT,64).EQ.32)THEN
               PAT=PAT-32
               J=J+1
               DA6=D(J)
               IF(DA6.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA6=INT((REAL(DA6)+R)*GS(8)+GS(7)+1.5)
                  IF((DA6.LT.1).OR.(DA6.GT.8192))DA6=1
                  M=M+1
                  CLC=CLC+DA6-1
                  CLR=RTC
               END IF
            END IF
            IF(MOD(PAT,128).EQ.64)THEN
               PAT=PAT-64
               J=J+1
               DA7=D(J)
               IF(DA7.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA7=INT((REAL(DA7)+R)*GS(10)+GS(9)+1.5)
                  IF((DA7.LT.1).OR.(DA7.GT.8192))DA7=1
                  M=M+1
                  CLC=CLC+DA7-1
                  CLR=RTC
               END IF
            END IF
            IF(PAT.EQ.128)THEN
               J=J+1
               DA8=D(J)
               IF(DA8.EQ.0)THEN
                  ZEROS=ZEROS+1
               ELSE
                  R=RAND(0)-0.5
                  DA8=INT((REAL(DA8)+R)*GS(12)+GS(11)+1.5)
                  IF((DA8.LT.1).OR.(DA8.GT.8192))DA8=1
                  M=M+1
                  CLC=CLC+DA8-1
                  CLR=RTC
               END IF
            END IF
            IF((CLC.LE.1).OR.(CLC.GT.8192))THEN
               CLC=1
            ELSE
               MU=MU+1
            END IF
            IF(DA4.GT.1)THEN
               IF(MU.EQ.1)THEN
                  IF((DA1.GT.1).AND.(DA4.GE.2785).AND.(DA4.LE.2820))THEN
                     IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                        SING1T(DA1)=SING1T(DA1)+1
                     ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                        SING1R(DA1)=SING1R(DA1)+1
                     END IF
                  END IF
                  IF((DA2.GT.1).AND.(DA4.GE.2785).AND.(DA4.LE.2820))THEN
                     IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                        SING2T(DA2)=SING2T(DA2)+1
                     ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                        SING2R(DA2)=SING2R(DA2)+1
                     END IF
                  END IF
                  IF((CLC.GT.1).AND.(DA4.GE.980).AND.(DA4.LE.1200))THEN
                     IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                        SINGCT(CLC)=SINGCT(CLC)+1
                     ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                        SINGCR(CLC)=SINGCR(CLC)+1
                     END IF
                  END IF
               END IF
               IF(MU.GT.1)THEN
                  RB=INT(2*RAND(0))
                  IF(MU.EQ.3)THEN
                     IF((RB.EQ.0).AND.(DA4.GE.2785).AND.(DA4.LE.2820))THEN
                        IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                           SING1T(DA1)=SING1T(DA1)+1
                        ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                           SING1R(DA1)=SING1R(DA1)+1
                        END IF
                     END IF
                     IF((RB.EQ.1).AND.(DA4.GE.2785).AND.(DA4.LE.2820))THEN
                        IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                           SING2T(DA1)=SING2T(DA1)+1
                        ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                           SING2R(DA1)=SING2R(DA1)+1
                        END IF
                     END IF
                     IF((DA4.GE.980).AND.(DA4.LE.1200))THEN
                        IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                           SINGCT(CLC)=SINGCT(CLC)+1
                        ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                           SINGCR(CLC)=SINGCR(CLC)+1
                        END IF
                     END IF
                  END IF
                  IF(MU.EQ.2)THEN
                     IF(DA1.EQ.1)THEN
                        IF((DA4.GE.2785).AND.(DA4.LE.2820))THEN
                           IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                              SING2T(DA2)=SING2T(DA2)+1
                           ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                              SING2R(DA2)=SING2R(DA2)+1
                           END IF
                        END IF
                        IF((DA4.GE.980).AND.(DA4.LE.1200))THEN
                           IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                              SINGCT(CLC)=SINGCT(CLC)+1
                           ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                              SINGCR(CLC)=SINGCR(CLC)+1
                           END IF
                        END IF
                     END IF
                     IF(DA2.EQ.1)THEN
                        IF((DA4.GE.2785).AND.(DA4.LE.2820))THEN
                           IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                              SING1T(DA1)=SING1T(DA1)+1
                           ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                              SING1R(DA1)=SING1R(DA1)+1
                           END IF
                        END IF
                        IF((DA4.GE.980).AND.(DA4.LE.1200))THEN
                           IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                              SINGCT(CLC)=SINGCT(CLC)+1
                           ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                              SINGCR(CLC)=SINGCR(CLC)+1
                           END IF
                        END IF
                     END IF
                     IF((CLC.EQ.1).AND.(DA4.GE.2785).AND.(DA4.LE.2820))THEN
                        IF(RB.EQ.0)THEN
                           IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                              SING1T(DA1)=SING1T(DA1)+1
                           ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                              SING1R(DA1)=SING1R(DA1)+1
                           END IF
                        END IF
                        IF(RB.EQ.1)THEN
                           IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                              SING2T(DA2)=SING2T(DA2)+1
                           ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                              SING2R(DA2)=SING2R(DA2)+1
                           END IF
                        END IF
                     END IF
                  END IF
               END IF                  
            END IF
            IF(MU.EQ.2)THEN
               IF(DA1.EQ.1)THEN
                  RB=DA2+CLC-1
                  IF((RB.LE.8192).AND.(DA3.GE.1100).AND.(DA3.LE.1350))THEN
                     IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                        SUM2CT(RB)=SUM2CT(RB)+1
                     ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                        SUM2CR(RB)=SUM2CR(RB)+1
                     END IF
                     IF((RB.GE.4000).AND.(RB.LT.7000))THEN
                        TOF(RTC)=TOF(RTC)+1
                        IF(RTC2.GT.0)TOF2(RTC2)=TOF2(RTC2)+1
                     END IF
                  END IF
               END IF
               IF(DA2.EQ.1)THEN
                  RB=DA1+CLC-1
                  IF((RB.LE.8192).AND.(DA3.GE.3270).AND.(DA3.LE.3800))THEN
                     IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                        SUM1CT(RB)=SUM1CT(RB)+1
                     ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                        SUM1CR(RB)=SUM1CR(RB)+1
                     END IF
                     IF((RB.GE.4000).AND.(RB.LT.7000))THEN
                        TOF(RTC)=TOF(RTC)+1
                        IF(RTC2.GT.0)TOF2(RTC2)=TOF2(RTC2)+1
                     END IF
                  END IF
               END IF
               IF(CLC.EQ.1)THEN
                  RB=DA1+DA2-1
                  IF((RB.LE.8192).AND.(DA3.GE.5300).AND.(DA3.LE.5900))THEN
                     IF((RTC.GE.12).AND.(RTC.LE.966))THEN
                        SUM12T(RB)=SUM12T(RB)+1
                     ELSE IF((RTC.GE.3).AND.(RTC.LE.11))THEN
                        SUM12R(RB)=SUM12R(RB)+1
                     END IF
                     IF((RB.GE.4000).AND.(RB.LT.7000))THEN
                        TOF(RTC)=TOF(RTC)+1
                        IF(RTC2.GT.0)TOF2(RTC2)=TOF2(RTC2)+1
                     END IF
                  END IF
               END IF
            END IF
            MULT(M)=MULT(M)+1
            IF(MU.GT.0)GM(MU)=GM(MU)+1
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
      WRITE(6,'(A30,I9)')'Number of gamma mult 1 events ',GM(1)
      WRITE(6,'(A30,I9)')'Number of gamma mult 2 events ',GM(2)
      WRITE(6,'(A30,I9)')'Number of gamma mult 3 events ',GM(3)
      OPEN(2,ERR=99,FILE='s1r.dat')
      WRITE(2,'(8192(I11/))')(SING1R(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File  s1r.dat written'
      OPEN(2,ERR=99,FILE='s1t.dat')
      WRITE(2,'(8192(I11/))')(SING1T(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File  s1t.dat written'
      OPEN(2,ERR=99,FILE='s2r.dat')
      WRITE(2,'(8192(I11/))')(SING2R(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File  s2r.dat written'
      OPEN(2,ERR=99,FILE='s2t.dat')
      WRITE(2,'(8192(I11/))')(SING2T(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File  s2t.dat written'
      OPEN(2,ERR=99,FILE='scr.dat')
      WRITE(2,'(8192(I11/))')(SINGCR(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File  scr.dat written'
      OPEN(2,ERR=99,FILE='sct.dat')
      WRITE(2,'(8192(I11/))')(SINGCT(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File  sct.dat written'
      OPEN(2,ERR=99,FILE='s12r.dat')
      WRITE(2,'(8192(I11/))')(SUM12R(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File s12r.dat written'
      OPEN(2,ERR=99,FILE='s12t.dat')
      WRITE(2,'(8192(I11/))')(SUM12T(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File s12t.dat written'
      OPEN(2,ERR=99,FILE='s1cr.dat')
      WRITE(2,'(8192(I11/))')(SUM1CR(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File s1cr.dat written'
      OPEN(2,ERR=99,FILE='s1ct.dat')
      WRITE(2,'(8192(I11/))')(SUM1CT(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File s1ct.dat written'
      OPEN(2,ERR=99,FILE='s2cr.dat')
      WRITE(2,'(8192(I11/))')(SUM2CR(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File s2cr.dat written'
      OPEN(2,ERR=99,FILE='s2ct.dat')
      WRITE(2,'(8192(I11/))')(SUM2CT(I),I=1,8192)
      CLOSE(2)
      WRITE(6,'(A21)')'File s2ct.dat written'
      OPEN(2,ERR=99,FILE='tofr.dat')
      WRITE(2,'(1024(I11/))')(TOF2(I),I=1,1024)
      CLOSE(2)
      WRITE(6,'(A21)')'File tofr.dat written'
      OPEN(2,ERR=99,FILE='toft.dat')
      WRITE(2,'(1024(I11/))')(TOF(I),I=1,1024)
      CLOSE(2)
      WRITE(6,'(A21)')'File toft.dat written'
      STOP
 98   WRITE(6,102)NR+1
 102  FORMAT('Error during reading record ',I7)
      STOP
 99   WRITE(6,101)
 101  FORMAT('Error during opening file')
      STOP
      END

