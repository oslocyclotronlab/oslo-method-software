      PROGRAM STRIPHEADER
      BYTE LISTDATA(1024),BLOCK(2048),B,A,C,D,T1,T2,T3,T4,T5,T6
      INTEGER*4 NR,MS,NS,NH,NT,TIME,ND,NB,NZ,DT1,DT2,DT3,DT4,DT5,DT6,DT7,DT8
      INTEGER*4 A1,A2,A3,A4,A5,A6,A7,A8,MULT(9),DB,BY,RM,CZ,RZ,GL
      INTEGER CB,MB,KB,BB,RL,RI,LI,NL,INF,I,HB,LIC,DU,NA,CH(8),LB,GB
      LOGICAL HEAD,MST,LR
      CHARACTER*10 FILENAME
      CHARACTER*14 NEWFILE
      CHARACTER*15 HEADER
      CHARACTER*14 LOGFILE
      CHARACTER LC,RC
      CHARACTER LINE(99)
      CHARACTER*2 NLC
      CHARACTER*6 LFMT
      CHARACTER*10 SLINE
      CHARACTER*22 DFMT
      RL=1024
      WRITE(6,*)' ________________________________________'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|       S T R I P H E A D E R  1.0       |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Program to strip the ASCII header, and |'
      WRITE(6,*)'|  to swap the bytes of double words, in |'
      WRITE(6,*)'| order to convert data files from INTEL |'
      WRITE(6,*)'| processor format to something sensible |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'| Lawrence Livermore National Laboratory |'
      WRITE(6,*)'|       Oslo Cyclotron Laboratory        |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|         Created:  10/23 - 2002         |'
      WRITE(6,*)'|   Andreas Schiller  Alexander Voinov   |'
      WRITE(6,*)'|________________________________________|'
      FILENAME='run???.lst'
      WRITE(6,11)FILENAME
 11   FORMAT('Name of the file to be converted <',A10,'>:',$)
      CALL READA(5,FILENAME)
      NEWFILE=FILENAME//'.new'
      HEADER=FILENAME//'.head'
      LOGFILE=FILENAME//'.log'
      OPEN(2,ERR=99,FILE=FILENAME,STATUS='OLD',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=RL)
      OPEN(3,ERR=99,FILE=NEWFILE)
      OPEN(4,ERR=99,FILE=HEADER)
      OPEN(66,ERR=99,FILE=LOGFILE)
      WRITE(66,*)' ________________________________________'
      WRITE(66,*)'|                                        |'
      WRITE(66,*)'|       S T R I P H E A D E R  1.0       |'
      WRITE(66,*)'|                                        |'
      WRITE(66,*)'| Program to strip the ASCII header, and |'
      WRITE(66,*)'|  to swap the bytes of double words, in |'
      WRITE(66,*)'| order to convert data files from INTEL |'
      WRITE(66,*)'| processor format to something sensible |'
      WRITE(66,*)'|                                        |'
      WRITE(66,*)'| Lawrence Livermore National Laboratory |'
      WRITE(66,*)'|       Oslo Cyclotron Laboratory        |'
      WRITE(66,*)'|                                        |'
      WRITE(66,*)'|         Created:  10/23 - 2002         |'
      WRITE(66,*)'|   Andreas Schiller  Alexander Voinov   |'
      WRITE(66,*)'|________________________________________|'
      MB=0
      WRITE(6,7)MB
 7    FORMAT('Number of megabytes to be read <',I3,'>:',$)
      CALL READI(5,MB)
      KB=0
      WRITE(6,8)KB
 8    FORMAT('Number of kilobytes to be read <',I3,'>:',$)
      CALL READI(5,KB)
      BB=0
      WRITE(6,9)BB
 9    FORMAT('Number of bytes to be read <',I3,'>:',$)
      CALL READI(5,BB)
      GB=MB/1024
      GL=1024*1024*1024/RL
      BY=BB+1024*(KB+1024*MOD(MB,1024))
      RM=BY/RL+GB*GL
      LB=MOD(BY,RL)
      IF(LB.EQ.0)THEN
         LB=LB+RL
      ELSE
         RM=RM+1
      END IF
      NR=1
      MS=0
      NS=0
      NH=0
      NT=0
      ND=0
      NB=0
      NZ=0
      DT1=0
      DT2=0
      DT3=0
      DT4=0
      DT5=0
      DT6=0
      DT7=0
      DT8=0
      A1=0
      A2=0
      A3=0
      A4=0
      A5=0
      A6=0
      A7=0
      A8=0
      MULT(1)=0
      MULT(2)=0
      MULT(3)=0
      MULT(4)=0
      MULT(5)=0
      MULT(6)=0
      MULT(7)=0
      MULT(8)=0
      RI=1
      LI=1
      NL=0
      INF=0
      HEAD=.TRUE.
      MST=.TRUE.
      LR=.FALSE.
      CZ=0
      RZ=0
      READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,RL)
      DO 12,I=1,RL
         BLOCK(I+RL)=LISTDATA(I)
 12   CONTINUE
 13   NR=NR+1
      IF(NR.EQ.RM)THEN
         READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,LB)
         DO 21,I=RI,2*RL
            BLOCK(I-LB)=BLOCK(I)
 21      CONTINUE
         DO 22,I=1,LB
            BLOCK(2*RL-LB+I)=LISTDATA(I)
 22      CONTINUE
         RI=RI-LB
         LR=.TRUE.
      ELSE IF(NR.LT.RM)THEN
         READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,RL)
         DO 14,I=1,RL
            BLOCK(I)=BLOCK(I+RL)
            BLOCK(I+RL)=LISTDATA(I)
 14      CONTINUE
         RI=MOD(RI,RL)
      ELSE
         NR=NR-1
      END IF
 15   IF(HEAD.EQ..TRUE.)THEN
         B=BLOCK(RI)
         RI=RI+1
         IF(((B.GE.32).AND.(B.LE.93)).OR.((B.GE.95).AND.(B.LE.126)))THEN
            CB=B
            LC=CHAR(CB)
            LINE(LI)=LC
            LI=LI+1
         ELSE IF(B.EQ.13)THEN
            IF(BLOCK(RI).EQ.10)THEN
               RI=RI+1
               NLC=CHAR(MOD(LI-1,10)+48)
               IF((LI-1).GE.10)NLC=CHAR(MOD(INT(REAL(LI-1)/10.0),10)+48)//NLC
               LFMT='('//NLC//'A1)'
               WRITE(4,LFMT)(LINE(I),I=1,LI-1)
               NL=NL+1
               IF(LI.GE.10)THEN
                  SLINE=LINE(1)//LINE(2)//LINE(3)//LINE(4)//LINE(5)//LINE(6)//LINE(7)//LINE(8)//LINE(9)//LINE(10)
                  IF(SLINE.EQ.'[LISTDATA]')THEN
                     HEAD=.FALSE.
                     WRITE(6,'(A22)')'[LISTDATA] encountered'
                     WRITE(66,'(A22)')'[LISTDATA] encountered'
                     HB=(NR-2)*RL+RI-1
                     WRITE(6,'(A27,I4)')'Number of bytes in header: ',HB
                     WRITE(66,'(A27,I4)')'Number of bytes in header: ',HB
                     WRITE(6,'(A27,I3)')'Number of lines in header: ',NL
                     WRITE(66,'(A27,I3)')'Number of lines in header: ',NL
                     WRITE(6,'(A33,I3)')'Number of added bytes in header: ',INF
                     WRITE(66,'(A33,I3)')'Number of added bytes in header: ',INF
                     CLOSE(4)
                  END IF
               END IF
               LI=1
            ELSE
               WRITE(6,'(A45)')'Carriage return without line feed encountered'
               WRITE(66,'(A45)')'Carriage return without line feed encountered'
            END IF
         ELSE IF(B.EQ.9)THEN
            LIC=LI
            LC=' '
            DO 16,I=MOD(LIC,16),16
               LINE(LI)=LC
               LI=LI+1
               INF=INF+1
 16         CONTINUE
            INF=INF-1
         ELSE IF(B.EQ.-75)THEN
            LINE(LI)='m'
            LI=LI+1
            LINE(LI)='u'
            LI=LI+1
            INF=INF+1
         ELSE
            WRITE(6,'(A21,I3)')'Unprintable character',B
            WRITE(66,'(A21,I3)')'Unprintable character',B
            WRITE(6,'(A23)')'Replaced by character ?'
            WRITE(66,'(A23)')'Replaced by character ?'
            LINE(LI)='?'
            LI=LI+1
            WRITE(6,*)'Byte number',(NR-2)*RL+RI-1
            WRITE(66,*)'Byte number',(NR-2)*RL+RI-1
         END IF
      ELSE
 17      A=BLOCK(RI)
         B=BLOCK(RI+1)
         C=BLOCK(RI+2)
         D=BLOCK(RI+3)
         IF((B.EQ.-1).AND.((C.EQ.0).OR.(C.EQ.1)).AND.(D.EQ.64))THEN
            RI=RI+4
            MS=MS+1
            MST=.TRUE.
            IF(A.NE.-1)THEN
               DU=MOD(A+256,256)
               IF(MOD(DU,2).EQ.0)THEN
                  DT1=DT1+1
               ELSE
                  DU=DU-1
               END IF
               IF(MOD(DU,4).EQ.0)THEN
                  DT2=DT2+1
               ELSE
                  DU=DU-2
               END IF
               IF(MOD(DU,8).EQ.0)THEN
                  DT3=DT3+1
               ELSE
                  DU=DU-4
               END IF
               IF(MOD(DU,16).EQ.0)THEN
                  DT4=DT4+1
               ELSE
                  DU=DU-8
               END IF
               IF(MOD(DU,32).EQ.0)THEN
                  DT5=DT5+1
               ELSE
                  DU=DU-16
               END IF
               IF(MOD(DU,64).EQ.0)THEN
                  DT6=DT6+1
               ELSE
                  DU=DU-32
               END IF
               IF(MOD(DU,128).EQ.0)THEN
                  DT7=DT7+1
               ELSE
                  DU=DU-64
               END IF
               IF(DU.EQ.0)DT8=DT8+1
            END IF
            IF((LR.EQ..TRUE.).AND.(RI.EQ.(2*RL+1)))GO TO 20
         ELSE IF((MST.EQ..TRUE.).AND.(A.EQ.-1).AND.(B.EQ.-1).AND.(C.EQ.-1).AND.(D.EQ.-1))THEN
            RI=RI+4
            NS=NS+1
            MST=.FALSE.
 18         A=BLOCK(RI)
            B=BLOCK(RI+1)
            C=BLOCK(RI+2)
            D=BLOCK(RI+3)
            IF((B.EQ.0).AND.((C.EQ.0).OR.(C.EQ.1)).AND.((D.EQ.16).OR.(D.EQ.-112)))THEN
               RI=RI+4
               NH=NH+1
               T1=BLOCK(RI)
               T2=BLOCK(RI+1)
               T3=BLOCK(RI+2)
               T4=BLOCK(RI+3)
               T5=BLOCK(RI+4)
               T6=BLOCK(RI+5)
               RI=RI+6
               NT=NT+1
               DU=MOD(T2+256,256)
               TIME=256*(DU+256*T3)
               DU=MOD(T1+256,256)
               TIME=5*(DU+TIME)
               IF((T4.NE.0).OR.(T5.NE.0).OR.(T6.NE.0).OR.(T3.GT.30).OR.(T3.LT.0))THEN
                  WRITE(6,'(A3,6I4,A31)')'RTC',T1,T2,T3,T4,T5,T6,' out of range, replaced by zero'
                  WRITE(66,'(A3,6I4,A31)')'RTC',T1,T2,T3,T4,T5,T6,' out of range, replaced by zero'
                  TIME=0
                  RZ=RZ+1
               END IF
               IF(D.EQ.-112)THEN
                  IF((BLOCK(RI).EQ.-1).AND.(BLOCK(RI+1).EQ.-1))THEN
                     RI=RI+2
                     ND=ND+1
                  ELSE
                     WRITE(6,*)'Missing or wrong dummy'
                     GO TO 97
                  END IF
               END IF
               NA=0
               DU=MOD(A+256,256)
               IF(MOD(DU,2).EQ.1)THEN
                  A1=A1+1
                  NA=NA+1
                  DU=DU-1
               END IF
               IF(MOD(DU,4).EQ.2)THEN
                  A2=A2+1
                  NA=NA+1
                  DU=DU-2
               END IF
               IF(MOD(DU,8).EQ.4)THEN
                  A3=A3+1
                  NA=NA+1
                  DU=DU-4
               END IF
               IF(MOD(DU,16).EQ.8)THEN
                  A4=A4+1
                  NA=NA+1
                  DU=DU-8
               END IF
               IF(MOD(DU,32).EQ.16)THEN
                  A5=A5+1
                  NA=NA+1
                  DU=DU-16
               END IF
               IF(MOD(DU,64).EQ.32)THEN
                  A6=A6+1
                  NA=NA+1
                  DU=DU-32
               END IF
               IF(MOD(DU,128).EQ.64)THEN
                  A7=A7+1
                  NA=NA+1
                  DU=DU-64
               END IF
               IF(DU.EQ.128)THEN
                  A8=A8+1
                  NA=NA+1
               END IF
               MULT(NA+1)=MULT(NA+1)+1
               DO 19,I=1,NA
                  DU=MOD(BLOCK(RI+1)+256,256)
                  CH(I)=256*DU
                  DU=MOD(BLOCK(RI)+256,256)
                  CH(I)=CH(I)+DU
                  RI=RI+2
                  NB=NB+1
                  IF(CH(I).GT.8191)THEN
                     WRITE(6,'(A8,I5,A31)')'Channel ',CH(I),' out of range, replaced by zero'
                     WRITE(66,'(A8,I5,A31)')'Channel ',CH(I),' out of range, replaced by zero'
                     CH(I)=0
                     CZ=CZ+1
                  END IF
 19            CONTINUE
               RC=CHAR(MOD(NA,10)+48)
               DFMT='(I2,X,I7,X,I3,'//RC//'(X,I4))'
               WRITE(3,DFMT),MOD(MS,100),TIME,MOD(A+256,256),(CH(I),I=1,NA)
               IF((LR.EQ..TRUE.).AND.(RI.EQ.(2*RL+1)))GO TO 20
               IF(RI.GT.RL)THEN
                  NR=NR+1
                  IF(NR.EQ.RM)THEN
                     READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,LB)
                     DO 41,I=RI,2*RL
                        BLOCK(I-LB)=BLOCK(I)
 41                  CONTINUE
                     DO 42,I=1,LB
                        BLOCK(2*RL-LB+I)=LISTDATA(I)
 42                  CONTINUE
                     RI=RI-LB
                     LR=.TRUE.
                  ELSE IF(NR.LT.RM)THEN
                     READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,RL)
                     DO 44,I=1,RL
                        BLOCK(I)=BLOCK(I+RL)
                        BLOCK(I+RL)=LISTDATA(I)
 44                  CONTINUE
                     RI=MOD(RI,RL)
                  ELSE
                     NR=NR-1
                  END IF
               END IF
               IF(BLOCK(RI+3).EQ.64)THEN
                  GO TO 17
               ELSE
                  GO TO 18
               END IF
            ELSE IF((A.EQ.0).AND.(B.EQ.0).AND.(C.EQ.0).AND.(D.EQ.0))THEN   
               RI=RI+4
               NZ=NZ+1
               IF((LR.EQ..TRUE.).AND.(RI.EQ.(2*RL+1)))GO TO 20
               IF(RI.GT.RL)THEN
                  NR=NR+1
                  IF(NR.EQ.RM)THEN
                     READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,LB)
                     DO 31,I=RI,2*RL
                        BLOCK(I-LB)=BLOCK(I)
 31                  CONTINUE
                     DO 32,I=1,LB
                        BLOCK(2*RL-LB+I)=LISTDATA(I)
 32                  CONTINUE
                     RI=RI-LB
                     LR=.TRUE.
                  ELSE IF(NR.LT.RM)THEN
                     READ(2,REC=NR,ERR=98)(LISTDATA(I),I=1,RL)
                     DO 34,I=1,RL
                        BLOCK(I)=BLOCK(I+RL)
                        BLOCK(I+RL)=LISTDATA(I)
 34                  CONTINUE
                     RI=MOD(RI,RL)
                  ELSE
                     NR=NR-1
                  END IF
               END IF
               IF(BLOCK(RI+3).EQ.64)THEN
                  GO TO 17
               ELSE
                  GO TO 18
               END IF
            ELSE
               WRITE(6,*)'Wrong data header'
               GO TO 97
            END IF
         ELSE   
            WRITE(6,*)'Neither time nor sync mark'
            GO TO 97
         END IF
      END IF
      GO TO (15,13) 1+((RI-1)/RL)
 20   CONTINUE
      WRITE(6,'(A13,I9)')'Number of ms ',MS
      WRITE(6,'(A16,I9)')'Number of syncs ',NS
      WRITE(6,'(A18,I9)')'Number of headers ',NH
      WRITE(6,'(A15,I9)')'Number of RTCs ',NT
      WRITE(6,'(A18,I9)')'Number of dummies ',ND
      WRITE(6,'(A21,I9)')'Number of data words ',NB
      WRITE(6,'(A28,I9)')'Number of zero double words ',NZ
      WRITE(6,'(A32,I9)')'Number of channels out of range ',CZ
      WRITE(6,'(A28,I9)')'Number of RTCs out of range ',RZ
      WRITE(6,'(A27,I9)')'Number of dead ADC1 events ',DT1
      WRITE(6,'(A27,I9)')'Number of dead ADC2 events ',DT2
      WRITE(6,'(A27,I9)')'Number of dead ADC3 events ',DT3
      WRITE(6,'(A27,I9)')'Number of dead ADC4 events ',DT4
      WRITE(6,'(A27,I9)')'Number of dead ADC5 events ',DT5
      WRITE(6,'(A27,I9)')'Number of dead ADC6 events ',DT6
      WRITE(6,'(A27,I9)')'Number of dead ADC7 events ',DT7
      WRITE(6,'(A27,I9)')'Number of dead ADC8 events ',DT8
      WRITE(6,'(A22,I9)')'Number of ADC1 events ',A1
      WRITE(6,'(A22,I9)')'Number of ADC2 events ',A2
      WRITE(6,'(A22,I9)')'Number of ADC3 events ',A3
      WRITE(6,'(A22,I9)')'Number of ADC4 events ',A4
      WRITE(6,'(A22,I9)')'Number of ADC5 events ',A5
      WRITE(6,'(A22,I9)')'Number of ADC6 events ',A6
      WRITE(6,'(A22,I9)')'Number of ADC7 events ',A7
      WRITE(6,'(A22,I9)')'Number of ADC8 events ',A8
      WRITE(6,'(A24,I9)')'Number of mult 0 events ',MULT(1)
      WRITE(6,'(A24,I9)')'Number of mult 1 events ',MULT(2)
      WRITE(6,'(A24,I9)')'Number of mult 2 events ',MULT(3)
      WRITE(6,'(A24,I9)')'Number of mult 3 events ',MULT(4)
      WRITE(6,'(A24,I9)')'Number of mult 4 events ',MULT(5)
      WRITE(6,'(A24,I9)')'Number of mult 5 events ',MULT(6)
      WRITE(6,'(A24,I9)')'Number of mult 6 events ',MULT(7)
      WRITE(6,'(A24,I9)')'Number of mult 7 events ',MULT(8)
      WRITE(6,'(A24,I9)')'Number of mult 8 events ',MULT(9)
      WRITE(66,'(A13,I9)')'Number of ms ',MS
      WRITE(66,'(A16,I9)')'Number of syncs ',NS
      WRITE(66,'(A18,I9)')'Number of headers ',NH
      WRITE(66,'(A15,I9)')'Number of RTCs ',NT
      WRITE(66,'(A18,I9)')'Number of dummies ',ND
      WRITE(66,'(A21,I9)')'Number of data words ',NB
      WRITE(66,'(A28,I9)')'Number of zero double words ',NZ
      WRITE(66,'(A32,I9)')'Number of channels out of range ',CZ
      WRITE(66,'(A28,I9)')'Number of RTCs out of range ',RZ
      WRITE(66,'(A27,I9)')'Number of dead ADC1 events ',DT1
      WRITE(66,'(A27,I9)')'Number of dead ADC2 events ',DT2
      WRITE(66,'(A27,I9)')'Number of dead ADC3 events ',DT3
      WRITE(66,'(A27,I9)')'Number of dead ADC4 events ',DT4
      WRITE(66,'(A27,I9)')'Number of dead ADC5 events ',DT5
      WRITE(66,'(A27,I9)')'Number of dead ADC6 events ',DT6
      WRITE(66,'(A27,I9)')'Number of dead ADC7 events ',DT7
      WRITE(66,'(A27,I9)')'Number of dead ADC8 events ',DT8
      WRITE(66,'(A22,I9)')'Number of ADC1 events ',A1
      WRITE(66,'(A22,I9)')'Number of ADC2 events ',A2
      WRITE(66,'(A22,I9)')'Number of ADC3 events ',A3
      WRITE(66,'(A22,I9)')'Number of ADC4 events ',A4
      WRITE(66,'(A22,I9)')'Number of ADC5 events ',A5
      WRITE(66,'(A22,I9)')'Number of ADC6 events ',A6
      WRITE(66,'(A22,I9)')'Number of ADC7 events ',A7
      WRITE(66,'(A22,I9)')'Number of ADC8 events ',A8
      WRITE(66,'(A24,I9)')'Number of mult 0 events ',MULT(1)
      WRITE(66,'(A24,I9)')'Number of mult 1 events ',MULT(2)
      WRITE(66,'(A24,I9)')'Number of mult 2 events ',MULT(3)
      WRITE(66,'(A24,I9)')'Number of mult 3 events ',MULT(4)
      WRITE(66,'(A24,I9)')'Number of mult 4 events ',MULT(5)
      WRITE(66,'(A24,I9)')'Number of mult 5 events ',MULT(6)
      WRITE(66,'(A24,I9)')'Number of mult 6 events ',MULT(7)
      WRITE(66,'(A24,I9)')'Number of mult 7 events ',MULT(8)
      WRITE(66,'(A24,I9)')'Number of mult 8 events ',MULT(9)
      DB=BY-HB
      DB=DB+1024*1024*1024*GB
      WRITE(6,'(A21,I11)')'Number of data bytes ',DB
      WRITE(66,'(A21,I11)')'Number of data bytes ',DB
      CLOSE(2)
      CLOSE(3)
      WRITE(6,'(A14)')'File converted'
      WRITE(66,'(A14)')'File converted'
      CLOSE(66)
 10   STOP
 97   WRITE(6,'(A30,I9,X,I4)')'Number of records',NR-2,RI
      WRITE(6,*)MOD(BLOCK(RI-32)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-31)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-30)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-29)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-28)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-27)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-26)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-25)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-24)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-23)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-22)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-21)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-20)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-19)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-18)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-17)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-16)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-15)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-14)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-13)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-12)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-11)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-10)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-9)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-8)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-7)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-6)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-5)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-4)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-3)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-2)+256,256)
      WRITE(6,*)MOD(BLOCK(RI-1)+256,256)
      WRITE(6,*)'Here it stopped'
      WRITE(6,*)MOD(BLOCK(RI)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+1)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+2)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+3)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+4)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+5)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+6)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+7)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+8)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+9)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+10)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+11)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+12)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+13)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+14)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+15)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+16)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+17)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+18)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+19)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+20)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+21)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+22)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+23)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+24)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+25)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+26)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+27)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+28)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+29)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+30)+256,256)
      WRITE(6,*)MOD(BLOCK(RI+31)+256,256)
      STOP
 98   WRITE(6,102)NR
 102  FORMAT('Error during reading record ',I7)
      STOP
 99   WRITE(6,101)
 101  FORMAT('Error during opening file')
      STOP
      END

