      PROGRAM READTELNY
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,ede
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4,filename*80,String*80,name*80
      DIMENSION iEDESP(0:4095,0:63)

      nEvent =0
      nAccept=0
      nCoinc =0

      iEventOld=0
      iStripOld=0
      iChipOld =0

4     name='SIRI'
      WRITE(6,5)name(1:4)
5     FORMAT(/'Sort logfile   <',A4,'>:',$)
      CALL READA(5,name)
      CALL LENGDE(name,LIN)
      filename=name(1:LIN)

C Opening file
      OPEN(11,FILE=filename,STATUS='OLD',ERR=51)
      GO TO 54
51    WRITE(6,*)'Did not find ',filename(1:LIN),' in your directory'
      GO TO 4

C Testing number of events and asking for number to be sorted
54    iTotal=0
52    READ(11,'(A)',ERR=53)String
      IF(String(1:1).EQ.'#')iTotal=iTotal+1 
      GO TO 52
53    iStart=1
      iStop=iTotal
      WRITE(6,7)iStart
7     FORMAT(/'Start sorting from event number <',I8,'>:',$)
      CALL READI(5,iStart)
      WRITE(6,8)iStop
8     FORMAT( 'Stop sorting at event number    <',I8,'>:',$)
      CALL READI(5,iStop)
      
C Event loop
      REWIND(11)
50    READ(11,'(A)',ERR=99)String     


      iHit=0                                              !No hit

      IF(String(1:1).EQ.'#')THEN 
        nEvent=nEvent+1
        IF(nEvent.LT.iStart.OR.nEvent.GT.iStop)GO TO 50
        JT=(nEvent/1000)*1000
        IF(JT.EQ.nEvent)THEN
           WRITE(6,1111)
 1111     FORMAT('.',$)
c          ist=putc('.')
          call flush(6)
        ENDIF

        i1=0
        i2=0
        DO i=2,8
          IF(String(i:i).EQ.'-'.AND.i1.EQ.0)i1=i
          IF(String(i:i).EQ.'-'.AND.i1.NE.0)i2=i
        ENDDO
        n1=i1-2
        n2=i2-i1-1
c          write(6,*)'n1,n2',n1,n2
        READ(String(2:n1+1),'(Z<n1>)',ERR=50)iEvent       !Event number is read
        READ(String(n1+3:n1+n2+2),'(Z<n2>)',ERR=50)iChip  !Chip number is read
        READ(String(i2+1:i2+2),'(Z2)',ERR=50)iStrip       !Strip number is read
       

        READ(11,'(A)',ERR=99)String                       !Reading second line
        n1=5
        DO i=2,7
          IF(String(i:i+2).EQ.'eoe')n1=i-1
        ENDDO
        READ(String(1:n1),'(I<n1>)',ERR=50)iEnergy        !Energy is read
c         write(6,*)'iEvent,iChip,iStrip,iEnergy',iEvent,iChip,iStrip,iEnergy

        iHit=1
      ENDIF

      IF(iHit.EQ.1)THEN            !It was a good event
        nAccept=nAccept+1
        iYch=iStrip+(32*iChip)
        IF(iYch.LT.0.OR.iYch.GT. 511)iYch=0
        iXch=iEnergy                              !/2
        IF(iXch.LT.0.OR.iXch.GT.4095)iXch=0
        rMAT(1,iXch,iYch)=rMAT(1,iXch,iYch)+1               !Singles incremented

        IF(iEventOld.EQ.iEvent)THEN                       !Picking a certain telescope
          iHit=0
c             write(6,*)iEvent,iStrip,iStripOld,iChip,iChipold
          IF(iStrip     .EQ.20.AND.iChip   .EQ.0)THEN     !dE detector
            iYchC=iXch
            IF(iStripOld.EQ. 4.AND.iChipOld.EQ.1)THEN     ! E detector
              iXchC=iXchOld
              iHit=1
            ENDIF
          ENDIF
          IF(iStripOld .EQ.20.AND.iChipOld .EQ.0)THEN     !dE detector
            iYchC=iXchOld
            IF(iStrip  .EQ. 4.AND.iChip    .EQ.1)THEN     ! E detector
              iXchC=iXch
              iHit=1
            ENDIF
          ENDIF
          IF(iYchC.GT.511)iYchC=0
          IF(iHit.EQ.1)THEN
c            write(6,*)nAccept,iEventOld,iStripOld,iXchOld,iEvent,iStrip,iXch
            rMAT(2,iXchC,iYchC)=rMAT(2,iXchC,iYchC)+1       !Coincidence incremented
            nCoinc=nCoinc+1
          ENDIF

          ede=(iEnergy+iEnergyOld)                         !/2
          IF(ede.GT.4095.OR.ede.LT.0)ede=0                      
          iEDESP(ede,iYch)=iEDESP(ede,iYch)+1           !Making dE+E, no tests...

        ENDIF

        iEventOld =iEvent
        iChipOld  =iChip
        iStripOld =iStrip
        iXchOld   =iXch
        iEnergyOld=iEnergy

      ENDIF
      GO TO 50

 99   CLOSE(11)                                           !File finished

      WRITE(6,10)nAccept,nCoinc,nEvent
 10   FORMAT(/,'Singles events detected     ',I9,
     +       /,'Coincidence events detected ',I9,
     +       /,'Total number of events      ',I9)

C Writting out matrices
     
      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=1.
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      ITYPE=3
      IDEST=1
      CALL FINDDIMENSION
      WRITE(6,12)
   12 FORMAT(//,'Write your E and dE singles matrix')
      CALL WRITEFILE

      cal(1,2,1,1)=0. 
      cal(1,2,1,2)=1.
      cal(1,2,1,3)=0.
      cal(1,2,2,1)=0.
      cal(1,2,2,2)=1.
      cal(1,2,2,3)=0.
      ITYPE=3
      IDEST=2
      CALL FINDDIMENSION
      WRITE(6,14)
   14 FORMAT(//,'Write your coincidence matrix (use another name as above)')
      CALL WRITEFILE

      cal(1,2,1,1)=0. 
      cal(1,2,1,2)=1.
      cal(1,2,1,3)=0.
      cal(1,2,2,1)=0.
      cal(1,2,2,2)=1.
      cal(1,2,2,3)=0.
      ITYPE=3
      IDEST=1
      DO j=0,511
        DO i=0,4095
          rMAT(1,i,j)=0
        ENDDO
      ENDDO
      DO j=0,63
        DO i=0,4095
          rMAT(1,i,j)=iEDESP(i,j)
        ENDDO
      ENDDO
      CALL FINDDIMENSION
      WRITE(6,16)
   16 FORMAT(//,'Write your EdE singles matrix (use another name as above)')
      CALL WRITEFILE


      END


      SUBROUTINE FINDDIMENSION
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4
      INTEGER XD,YD
      
      XD=10
      YD=4

      DO i=4095,10,-1      
        DO j=0,511,1
          IF(rMAT(IDEST,i,j).NE.0)GO TO 1
        ENDDO
      ENDDO
 1    XD=MIN0(i,4095)
      DO j=511,10,-1     
         DO i=0,XD,1
          IF(rMAT(IDEST,i,j).NE.0)GO TO 2
        ENDDO
      ENDDO
 2    YD=MIN0(j,511)
         
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

      END
