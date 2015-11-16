      PROGRAM READLOG
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,XD,YD
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4,filename*80,String*80,key1*7,key2*6,test*7,name*80

      XD=15
      YD=3
      iEvent=0
      iAccept=0
      key1='Strip #'
      key2='Data ='

4     name='siri'
      WRITE(6,5)name(1:4)
5     FORMAT(/'Sort logfile   <',A4,'>:',$)
      CALL READA(5,name)
      CALL LENGDE(name,LIN)
      filename=name(1:LIN)

C Opening file
      OPEN(11,FILE=filename,STATUS='OLD',ERR=51)
      GO TO 50
51    WRITE(6,*)'Did not find ',filename(1:LIN),' in your directory'
      GO TO 4

C Event loop

50    READ(11,'(A)',ERR=99)String                         !Reading first line
      iEvent=iEvent+1
      DO i=1,70
        test='xxxxxxx'
        READ(String(i:i+7),'(A7)',ERR=50)test(1:7)
        IF(test(1:7).EQ.key1(1:7))THEN                 
          READ(String(i+7:i+8),'(Z2)',ERR=50)iStrip       !Strip number is read
          READ(11,'(A)',ERR=50)String
          DO j=1,70
            test='xxxxxxx'
            READ(String(j:j+5),'(A6)',ERR=50)test(1:6)    !Reading second line
            IF(test(1:6).EQ.key2(1:6))THEN
              READ(String(j+6:j+9),'(I4)',ERR=50)iEnergy  !Energy is read
              IF(iStrip.GE.0.AND.iStrip.LE.511.AND.iEnergy.GE.0.AND.iEnergy.LE.4095)THEN
                IF(iStrip.GT.YD)YD=iStrip
                IF(iEnergy.GT.XD)XD=iEnergy
                rMAT(1,iEnergy,iStrip)=rMAT(1,iEnergy,iStrip)+1
                iAccept=iAccept+1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO 
      GO TO 50

99    CLOSE(11)                                            !File finished

      WRITE(6,10)iAccept,iEvent
 10   FORMAT(/,I9,' events accepted out of ',I9,' events')

      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=128.5
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      XDIM=XD
      YDIM=YD
      IF(XD.GT.15-2)XDIM=32
      IF(XD.GT.31-2)XDIM=64
      IF(XD.GT.63-2)XDIM=128
      IF(XD.GT.127-2)XDIM=256
      IF(XD.GT.255-2)XDIM=512
      IF(XD.GT.511-2)XDIM=1024
      IF(XD.GT.1023-2)XDIM=2048
      IF(XD.GT.2047-2)XDIM=4096
      IF(YD.GT.7-2)YDIM=16
      IF(YD.GT.15-2)YDIM=32
      IF(YD.GT.31-2)YDIM=64
      IF(YD.GT.63-2)YDIM=128
      IF(YD.GT.127-2)YDIM=256
      IF(YD.GT.255-2)YDIM=512       
      ITYPE=3
      IDEST=1

      CALL WRITEFILE

      END

