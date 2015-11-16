      PROGRAM main
C The spy program plots the intensity rates of counts in various SIRIUS/OFFLINE matrices
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER specnai*8, name*8, namej*8,ans*1
      DIMENSION name(0:9)
      INTEGER xmax(0:9),ymax(0:9),lxi,hxi,lyi,hyi
	  INTEGER i,j,k

      COMMON/spy1/m1,m2,lx(0:23),hx(0:23),ly(0:23),hy(0:23),mon(0:23),specna(0:23),iD,sleeptime
      INTEGER lx,hx,ly,hy,mon,iD,sleeptime
      CHARACTER specna*8
      COMMON/spy2/dCounts(0:511,0:23)
      REAL dCounts
      COMMON/spy3/OLhi(24),OLlc(24),OLhc(24)
      REAL Olhi,OLlc,OLhc

      WRITE(6,*)'     _________________________________________'
      WRITE(6,*)'    |                                         |'
      WRITE(6,*)'    |               S P Y  1.0                |'
      WRITE(6,*)'    |                                         |'
      WRITE(6,*)'    |       Program to monitor counts of      |'
      WRITE(6,*)'    |     various SIRIUS/OFFLINE matrices     |'
      WRITE(6,*)'    |                                         |'
      WRITE(6,*)'    |   Oslo Cyclotron Laboratory, May 2008   |'
      WRITE(6,*)'    |            Magne Guttormsen             |'
      WRITE(6,*)'    |_________________________________________|'
      WRITE(6,*) ' '

C Putting up defaults
      iPlotMax  = 24
      iPlot     = 5
      iD        = 1
      sleeptime = 60           !accumulate 60 seconds


      DO i=0,iPlotMax-1
        lx(i)=10
        hx(i)=-1
        ly(i)=0
        hy(i)=-1
        mon(i)=1               !divide by the monitor
        specna(i)='        '
      ENDDO
      mon(0)=0                 !the monitor can not be divided by itself
	  
      name(0) ='ESP'
      xmax(0) =2047
      ymax(0) =63
      name(1) ='DESP'
      xmax(1) =2047
      ymax(1) =63
      name(2) ='EDESP'
      xmax(2) =2047
      ymax(2) =63
      name(3) ='THICKSP'
      xmax(3) =2047
      ymax(3) =63
      name(4) ='NASP'
      xmax(4) =2047
      ymax(4) =31
      name(5) ='GESP'
      xmax(5) =4095
      ymax(5) =5
      name(6) ='TNASP'
      xmax(6) =511
      ymax(6) =31
      name(7) ='TGESP'
      xmax(7) =511
      ymax(7) =5
      name(8) ='MAT'
      xmax(8) =2047
      ymax(8) =63
      name(9) ='SINGLES'
      xmax(9) =4095
      ymax(9) =9

C Reading spyinfo.dat from directory, if possible
  33  ReadStatus=0
      Istatus=0
      OPEN(23,FILE='spyinfo.dat',STATUS='old',ERR=777)
      READ(23,*,ERR=666)iPlot,monitor,iD,sleeptime
      DO i=0,iPlot-1
        READ(23,*,ERR=666)specna(i),lx(i),hx(i),ly(i),hy(i),mon(i)
      ENDDO
      ReadStatus=1                  !The parameters are OK
      GO TO 777
 666  WRITE(6,*)'Warning: Something wrong with your spyinfo.dat file'
 777  CLOSE(23)

      IF(ReadStatus.EQ.0)THEN
        WRITE(6,*)'Missing valid spyinfo.dat file - using default values'
        monitor=1
        iPlot=5
        specna(0)=name(2)     !EDESP
        lx(0)    =100
        hx(0)    =1800
        ly(0)    =0
        hy(0)    =63
        mon(0)   =0
        specna(1)=name(4)     !NASP
        lx(1)    =100
        hx(1)    =1000
        ly(1)    =0
        hy(1)    =31
        mon(1)   =1
        specna(2)=name(6)     !TNASP
        lx(2)    =200
        hx(2)    =300
        ly(2)    =0
        hy(2)    =511
        mon(2)   =1
        specna(3)=name(5)     !GESP
        lx(3)    =100
        hx(3)    =1000
        ly(3)    =0
        hy(3)    =5
        mon(3)   =1
        specna(4)=name(7)     !TGESP
        lx(4)    =200
        hx(4)    =300
        ly(4)    =0
        hy(4)    =5
        mon(4)   =1
      ENDIF
      
      WRITE(6,10)iD
 10   FORMAT(/,'Monitoring SIRIUS(1) or OFFLINE(2) matrices <',I1,'>:',$)
      CALL READI(5,iD)
      IF(iD.LT.1.OR.iD.GT.2) STOP

      WRITE(6,11)sleeptime
 11   FORMAT(/,'Integration time for count rates ( > 5 sec) <',I2,'>:',$)
      CALL READI(5,sleeptime)
      IF(sleeptime.LT.10)sleeptime=5

      WRITE(6,12)
 12   FORMAT(/,'Do you want to use a monitor? This can be e.g. the',/, 
     +         'counts in the elastic particle peak or counts in another',/,
     +         'detector which is proportional to the beam intensity.')
      ans='y'
      IF(monitor.EQ.0)ans='n'
      WRITE(6,14)ans
 14   FORMAT(/,'Use a monitor (y/n) <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
        monitor=1
      ELSE
        monitor=0
      ENDIF

      WRITE(6,16)
 16   FORMAT(//,'Give names of matrices (ESP, DESP, EDESP, THICKSP,',/,
     +       'NASP, GESP, TNASP, TGESP, MAT or SINGLES) and their integration',/,
     +       'limits on x- and y-axis. You may repeat integration',/,
     +       'on the same matrix, but with other limits. A total of 24 integrals',/,
     +       'may be monitored. The matrix-name STOP terminates the definitions.')
      IF(monitor.EQ.1)WRITE(6,18)
 18   FORMAT('Each matrix count rate may be divided by the monitor - or not.',/,
     +       'The first matrix you define, will be used as the monitor.')

      DO i=0,iPlotMax-1
99      continue !specnai='        '       
        specnai=specna(i)
        IF(i.GE.iPlot)specnai='STOP'
        lastno = lnblnk(specnai)
        WRITE(6,20)specnai(1:lastno)
20      FORMAT(/,'Name of matrix (no more = STOP)  <',A,'>:',$)
        CALL READA(5,specnai)
        lastno = lnblnk(specnai)
        IF(specnai(1:lastno).EQ.'STOP'.OR.specnai(1:lastno).EQ.'stop')THEN
          iPlot=i
          GO TO 98
        ENDIF
        k=-1
        DO j=0,9
          namej=name(j)
          lastno=lnblnk(namej)
          IF(specnai(1:lastno).EQ.namej(1:lastno))k=j
        ENDDO
        IF(k.GT.-1)THEN
          hx(i)=xmax(k)
          hy(i)=ymax(k)
        ENDIF
        IF(k.EQ.-1)THEN
          WRITE(6,22)
 22       FORMAT('Use names:',/,
     +           'ESP, DESP, EDESP, THICKSP, NASP, GESP, TNASP, TGESP, MAT or SINGLES')
          GO TO 99
        ENDIF
  
        specna(i)=specnai

        lxi=lx(i)
        WRITE(6,24)lxi
 24     FORMAT('Lower channel on x-axis   <',I4,'>:',$)
        CALL READI(5,lxi)
        IF(lxi.LT.0.OR.lxi.GT.xmax(k))lxi=10
        lx(i)=lxi

        hxi=hx(i)
        IF(hxi.LT.0.OR.hxi.GT.4095)hxi=511
        WRITE(6,25)hxi
 25     FORMAT('Higher channel on x-axis  <',I4,'>:',$)
        CALL READI(5,hxi)
        IF(hxi.LT.0.OR.hxi.GT.xmax(k))hxi=xmax(k)
        hx(i)=hxi

        lyi=ly(i)
        WRITE(6,26)lyi
 26     FORMAT('Lower channel on y-axis   <',I4,'>:',$)
        CALL READI(5,lyi)
        IF(lyi.LT.0.OR.lyi.GT.ymax(k))lyi=10
        ly(i)=lyi

        hyi=hy(i)
        IF(hyi.LT.0.OR.hyi.GT.511)hyi=5
        WRITE(6,27)hyi
 27     FORMAT('Higher channel on y-axis  <',I4,'>:',$)
        CALL READI(5,hyi)
        IF(hyi.LT.0.OR.hyi.GT.ymax(k))hyi=ymax(k)
        hy(i)=hyi
      
        IF(monitor.EQ.1.AND.i.GT.0)THEN
          ans='y'
          moni=mon(i)
          IF(moni.EQ.0)ans='n'
          WRITE(6,28)ans
 28       FORMAT('Divide by monitor (y/n)      <',A1,'>:',$)
          CALL READA1(5,ans)
          IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
            mon(i)=1
          ELSE
            mon(i)=0
          ENDIF
        ELSE
          mon(i)=0
        ENDIF

      ENDDO
      iPlot=i

 98   CONTINUE     

C Writting out parameters
      OPEN(UNIT=23,FILE='spyinfo.dat')
      WRITE(23,*,ERR=666)iPlot,monitor,iD,sleeptime
      DO i=0,iPlot-1
        WRITE(23,*,ERR=666)specna(i),lx(i),hx(i),ly(i),hy(i),mon(i)
      ENDDO
      CLOSE(23)
      WRITE(6,*)'Parameters stored in file: spyinfo.dat'

      WRITE(6,30)
30    FORMAT(//,' No   Matrix    LowX HighX   LowY HighY  1/mon')
      DO i=0,iPlot-1
        WRITE(6,32)i,specna(i),lx(i),hx(i),ly(i),hy(i),mon(i)
32      FORMAT(I3,4X,A8,I5,I6,I7,I6,I6)     
      ENDDO
      
      iAns=0
      IF(Istatus.NE.0)iAns=1

      WRITE(6,34)iAns
 34   FORMAT(//,'OK, start endless monitoring loop (0)',/,
     +          'Change input parameters           (1)',/,
     +          'Exit and stop program             (2)',/,
     +          'Give answer                       <',I1,'>:',$)
      CALL READI(5,iAns)
      IF(iAns.EQ.2)STOP
      IF(iAns.EQ.1)GO TO 33

      Istatus=0
      m1=0
      m2=iPlot-1

      CALL LOOP

      END

      
  
      SUBROUTINE READI(IDEV,INTEG)
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER X*80
      READ(IDEV,1,ERR=99)X
    1 FORMAT(80A)
      IF(X.EQ.'')RETURN
      READ(X,*,ERR=99)INTEG
      RETURN
 99   Istatus=1
      RETURN
      END


      SUBROUTINE READF(IDEV,REELL)
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER X*80
      READ(IDEV,1,ERR=99)X
    1 FORMAT(80A)
      IF(X.EQ.'')RETURN
      READ(X,*,ERR=99)REELL
      RETURN
99    Istatus=1
      RETURN
      END


      SUBROUTINE READA(IDEV,KAR)
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER X*80
      CHARACTER KAR*(*)
      READ(IDEV,1,ERR=99)X
    1 FORMAT(80A)   
      IF(X.EQ.'')RETURN
      READ(X,1,ERR=99)KAR
      RETURN
99    Istatus=1
      RETURN
      END


      SUBROUTINE READA1(IDEV,KAR)
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER X*1
      CHARACTER KAR*1
      READ(IDEV,1,ERR=99)X
    1 FORMAT(A1)
      IF(X.EQ.''.OR.X.EQ.' ')RETURN
      READ(X,*,ERR=99)KAR
      RETURN
99    Istatus=1
      RETURN
      END


