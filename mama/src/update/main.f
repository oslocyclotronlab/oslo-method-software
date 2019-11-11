      PROGRAM main
      LOGICAL DISP
      REAL               rLDZ,rHDZ,rLOCNT,rHICNT
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3

      COMMON/OL/I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                         OLlow,OLhigh,OLlocnt,OLhicnt

      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      INTEGER COLORMAP(20),Limit(0:19),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color
      REAL rLimit(0:19)
      REAL rOLlocnt,rOLhicnt

      CHARACTER fil*4, specna*8
      COMMON/update/iD,ixy,i1,i2,specna

      COMMON/SAVEOUTLAY/OLhi,OLlc,OLhc
      INTEGER OLhi(64),OLlc(64),OLhc(64)

C The update program shows (by calculated time intervals) the spectrum just 
C displayed by MAMA before the UD command is given. The spectrum can be directly
C an SD or OD spectrum - or a spectrum deduced from these by projection along the
C x or y axis. It can also be an OUTLAY of a matrix with up to 64 spectra with 
C auto-limits on x and y (that is actually z) axis.

C Before the loop with updating starts, we have to know:
C Is it a SD or OD spectrum: iD = 1 or 2
C Is it a projected spectrum along x or y axis: ixy = 1 or 2
C The channels projected: i1 - i2 
C Is it singles (DSPSP), matrix (DSPMA) or outlay (OUTLAY) of rows (coloumns)
C It is either rows (iRC=1) or coloumns (iRC=2) shown in OUTLAY
C The channels shown as singles in OUTLAY: m1 - m2

C All this information is written to the file updateinfo.dat by MAMA, and 
C the first part of the program main.f interprets this text file

C Initializing in case of no file or incorrect input from file: updateinfo.dat
      DATA DISP/.FALSE./,IYAXIS/1/,
     +     LDX/0/,HDX/4095/,LDY/0/,HDY/511/,LDZ/4/,HDZ/10000/,
     +     LOCH/0/,HICH/8191/,LOCNT/0/,HICNT/1000/,
     +     I3/2/,iRC/1/,m1/0/,m2/511/,Idistype/0/,
     +     OLlow/0/,OLhigh/511/,OLlocnt/0/,OLhicnt/1000/
      DATA COLORMAP /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20/
      DATA Istatus/0/,ITYPE/3/,IDEST/1/
      DATA fname(1,1)/'matrix1'/, fname(1,2)/'matrix2 '/,
     +     fname(2,1)/'singles1'/,fname(2,2)/'singles2'/
      DATA iCE/0/,itext/1/              

      XDIM= 2048
      YDIM=  32
      MAXCH=8191
      Idistype=0
      ITYPE=3
      IDEST=1
      iD=1
      specna='NASP'

      DO i=1,64
        OLhi(i)=OLhicnt
        OLlc(i)=OLlow
        OLhc(i)=OLhight
      ENDDO

C Calibration default. cal(i,j,k,l) i deduced from ITYPE
C i=1 is ITYPE=2,3 and i=2 is ITYPE=1 ,j=1/2,k=x/y-cal, (3-terms)
C ITYPE=1: singles spectrum. ITYPE=2 and 3 is 2-dimensional matrices
C j is destination IDEST or source ISP spectrum
      DO i=1,2
        DO j=1,2
          DO k=1,2
            DO l=1,3
              cal(i,j,k,l)=0.
            ENDDO
            cal(i,j,k,2)=1.
            IF(i.EQ.2.AND.k.EQ.2)cal(i,j,k,2)=0. ! y not in use for
          ENDDO                                  ! singles spectrum
        ENDDO
      ENDDO
 
      OPEN(UNIT=7,FILE='updateinfo.dat',STATUS='old',err=777)
      read(7,*,err=666)IYAXIS,LDX,HDX,LDY,HDY,rLDZ,rHDZ,LOCH,HICH,rLOCNT,rHICNT
      read(7,*,err=666)XDIM,YDIM,MAXCH
      read(7,*,err=666)I3,iRC,m1,m2,Idistype,OLlow,OLhigh,rOLlocnt,rOLhicnt
      read(7,*,err=666)(rLimit(i),i=0,19)
      read(7,*,err=666)(COLORMAP(i),i=1,20)
      read(7,*,err=666)Istatus,ITYPE,IDEST,iCE,itext
      read(7,*,err=666)cal
      read(7,1,err=666)UNITx,UNITy
      read(7,2,err=666)fname(1,1)
      read(7,2,err=666)fname(2,1)
      read(7,2,err=666)fname(1,2)
      read(7,2,err=666)fname(2,2)
      read(7,2,err=666)comm(1,1)
      read(7,2,err=666)comm(2,1)
      read(7,2,err=666)comm(1,2)
      read(7,2,err=666)comm(2,2)
1     FORMAT(2A3)
2     FORMAT(A60)

	  LDZ   = rLDZ
	  HDZ   = rHDZ
	  LOCNT = rLOCNT
	  HICNT = rHICNT
      OLlocnt = rOLlocnt
      OLhicnt = rOLhicnt
	  Limit(0) = -1
	  Limit(1) =  1
	  Limit(2) = 2
	  DO i = 3,19
        Limit(i) = 2*Limit(i-1)
      ENDDO
      CLOSE(7)
      GO TO 777
666   write(6,*)'Warning, updateinfo.dat contains wrong parameters'
777   continue

C Setting up Color(1:19) vektors for 3D-plot
      Color(0) =COLORMAP(1 )     !blue
      Color(1) =COLORMAP(19)     !white
      Color(2) =COLORMAP(2 )     !deep sky blue
      Color(3) =COLORMAP(3 )     !light sky blue
      Color(4) =COLORMAP(4 )     !sea green
      Color(5) =COLORMAP(5 )     !medium sea green
      Color(6) =COLORMAP(6 )     !green
      Color(7) =COLORMAP(13)     !yellow3
      Color(8) =COLORMAP(15)     !yellow
      Color(9) =COLORMAP(12)     !orange
      Color(10) =COLORMAP(8 )    !chocolate
      Color(11) =COLORMAP(11)    !coral
      Color(12) =COLORMAP(10)    !red
      Color(13) =COLORMAP(19)    !white
      Color(14) =COLORMAP(1 )    !blue
      Color(15) =COLORMAP(19)    !white
      Color(16) =COLORMAP(2 )    !deep sky blue
      Color(17) =COLORMAP(3 )    !light sky blue
      Color(18) =19              !white   
      Color(19) =20              !black

C A rough test if you got the wright parameters
      IF(IDEST.LE.0.OR.IDEST.GE.3)go to 888
      IF(ITYPE.LE.0.OR.ITYPE.GE.4)go to 888

      IF(Idistype.EQ.1.OR.Idistype.EQ.3)THEN
        write(6,*)'Cannot update more than one matrix or one spectrum at the time'
        STOP
      ENDIF
      i12 = 1                               !data displayed as matrix or OL
      IF(ITYPE.EQ.1) i12 = 2                !data displayed as singles

C Find the history of spectrum displayed in mama by analyzing the string
C called: comm, which is written in the header of the spectrum file.
C In case it is displayed as singles: is it a projection (PM) or not?
C ixy = 0, 1, 2 means no, yes: x (1) or y (2) projection
      ixy=0
      IF(i12.EQ.2)THEN
        do i=1,57
          IF(comm(i12,IDEST)(58-i:61-i).EQ.'PMx:')THEN
            ixy=1
            GO TO 22
          ENDIF
          IF(comm(i12,IDEST)(58-i:61-i).EQ.'PMy:')THEN
            ixy=2
            GO TO 22
          ENDIF
        enddo
 22     IF(ixy.GT.0)THEN                             ! the spectrum has been projected
          i1=0
          do j1=min(62-i,60),min(67-i,60)          !reading low limit
            IF(comm(i12,IDEST)(j1:j1).EQ.'-'.OR.comm(i12,IDEST)(j1:j1).EQ.' ')GOTO 33
            read(comm(i12,IDEST)(j1:j1),10,err=33)ii
            i1=i1*10+ii
          enddo
 33       i2=0
          do j2=min(j1+1,60),min(j1+4,60) !reading higher limit
            IF(comm(i12,IDEST)(j2:j2).EQ.' '.OR.comm(i12,IDEST)(j2:j2).EQ.'')GOTO 34
            read(comm(i12,IDEST)(j2:j2),10,err=34)ii
            i2=i2*10+ii
          enddo
 34       continue
 10       FORMAT(I1)
        ENDIF
        IF(i1.LT.0.OR.i1.GT.4095.OR.i2.LT.0.OR.i2.GT.4095.OR.i1.GT.i2)THEN
          write(6,*)'Sorry, could not find low and high channel for projection'
          write(6,*)'i1= ',i1,'   i2= ',i2
          i1=0
          i2=0
          ixy=0
        ENDIF
      ENDIF                     

C Is it an SD or OD spectrum, and what is the name?            iD = 1 or 2
C This info come always at comm(1,1), since all SIRIUS spectra are put
C by definition in first spectrum - and it is always a matrix
      iD=0
      do i=1,57
        IF(comm(1,1)(58-i:61-i).EQ.'|SD:')THEN
          iD=1
          GO TO 44
        ENDIF
        IF(comm(1,1)(58-i:61-i).EQ.'|OD:')THEN
          iD=2
          GO TO 44
        ENDIF
      enddo

      IF(iD.EQ.0)THEN
        WRITE(6,*)'The update spectrum is neither a SIRIUS (SD) nor an OFFLINE (OD) spectrum.'
        WRITE(6,*)''
		STOP
      ENDIF

44    IF(iD.NE.0)THEN
        j1=min(62-i,60)
        j2=min(64-i,60)
        j3=min(65-i,60)
        read(comm(1,1)(j1:j3),11,err=35)fil(1:4)
        go to 13
35      read(comm(1,1)(j1:j2),12,err=13)fil(1:3)
11      format(A4)
12      format(A3)
13      specna='NASP'                              ! finding proper name

        IF(fil(1:3).EQ. 'ESP')specna='ESP'
        IF(fil(1:4).EQ.'DESP')specna='DESP'
        IF(fil(1:4).EQ.'EDES')specna='EDESP'
        IF(fil(1:4).EQ.'THIC')specna='THICKSP'
        IF(fil(1:4).EQ.'NASP')specna='NASP'
        IF(fil(1:4).EQ.'GESP')specna='GESP'
        IF(fil(1:4).EQ.'TNAS')specna='TNASP'
        IF(fil(1:4).EQ.'TGES')specna='TGESP'
        IF(fil(1:4).EQ.'SING')specna='SINGLES'
        IF(fil(1:4).EQ.'ALFN')specna='ALFNA' 
        IF(fil(1:4).EQ.'ALFG')specna='ALFGE'
        IF(fil(1:4).EQ.'MAT')specna='MAT'
		
      ENDIF
      CALL LOOP
      STOP

888   write(6,*)'Sorry, updateinfo.dat contains fatal wrong parameters'

      end
     
