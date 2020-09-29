C*******************************************************************
C  READ-WRITE-PROCEDURES FOR 1 OR 2 DIMENSIONAL SPECTRA
C  mama-version
C  Version 7. Oct. 2003/mg
C*******************************************************************

      SUBROUTINE WRITEFILE
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      INTEGER XDIM,YDIM, dim
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER UTFIL1*80,UTFIL2*80,UTFIL3*80,FILNAM*80,APP*4
      REAL Calib(6), Spec(0:8191)

      WRITE(6,1)IDEST
1     FORMAT(/'Spectrum to write            <',I1,'>:',$)
      CALL READI(5,IDEST)
      IF(IDEST.GT.2)Istatus=1
      IF(Istatus.NE.0)RETURN

      UTFIL1='SPEC'
      UTFIL2='NA-'
      UTFIL3='TEST'
      IUTF=21

 888  CONTINUE
      WRITE(6,2)
   2  FORMAT('Singles spectrum                1',/,
     1       'Set of spectra NA-0, NA-1,...   2',/,
     2       '2-dimensional spectrum (matrix) 3')
      WRITE(6,3)ITYPE
   3  FORMAT('Please, choose your type     <',I1,'>:',$)
      CALL READI(5,ITYPE)
      IF(Istatus.NE.0)RETURN
 
      IF(ITYPE.EQ.1)THEN
        WRITE(6,23)  cal(2,IDEST,1,1)
        CALL READF(5,cal(2,IDEST,1,1))
        IF(Istatus.NE.0)RETURN
        WRITE(6,24)  cal(2,IDEST,1,2)
        CALL READF(5,cal(2,IDEST,1,2))
        IF(Istatus.NE.0)RETURN
        WRITE(6,25)  cal(2,IDEST,1,3)
        CALL READF(5,cal(2,IDEST,1,3))
        IF(Istatus.NE.0)RETURN
        m=0
        DO i=1,1
          DO j=1,3
            m=m+1
            Calib(m)=cal(2,IDEST,i,j)
          ENDDO
        ENDDO
        iCal=m
        LIN=MAXCH+1
        WRITE(6,4)LIN
    4   FORMAT(/'Length of output-spectrum <',I4,'>:',$)
        CALL READI(5,LIN) 
        IF(Istatus.NE.0)RETURN
        dim=LIN
        WRITE(6,5)UTFIL1(1:4)
    5   FORMAT( 'Filename                  <',A,'>:',$)
        CALL READA(5,UTFIL1)
        IF(Istatus.NE.0)RETURN
        DO i=0,8191
          Spec(i)=rSPEC(IDEST,i)
        ENDDO
        CALL LENGDE(UTFIL1,LIN)
        FILNAM=UTFIL1(1:LIN)         
        OPEN(IUTF,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
        CALL norw1dim(IUTF,comm(2,IDEST),dim,Spec,Calib)
        CLOSE(IUTF)
      ENDIF

      IF(ITYPE.EQ.2)THEN
        WRITE(6,23)  cal(1,IDEST,1,1)
        CALL READF(5,cal(1,IDEST,1,1)) 
        IF(Istatus.NE.0)RETURN
        WRITE(6,24)  cal(1,IDEST,1,2)
        CALL READF(5,cal(1,IDEST,1,2))
        IF(Istatus.NE.0)RETURN 
        WRITE(6,25)  cal(1,IDEST,1,3)
        CALL READF(5,cal(1,IDEST,1,3))
        IF(Istatus.NE.0)RETURN
        m=0
        DO i=1,1
          DO j=1,3
            m=m+1
            Calib(m)=cal(1,IDEST,i,j)
          ENDDO
        ENDDO
        iCal=m
        NSPEC=YDIM
        WRITE(6,9)NSPEC
   9    FORMAT(/,'Number of spectra (max=2048) <',I3,'>:',$)
        CALL READI(5,NSPEC)
        IF(Istatus.NE.0)RETURN
        WRITE(6,10)XDIM
  10    FORMAT('Length of output-spectra   <',I4,'>:',$)
        CALL READI(5,XDIM)
        IF(Istatus.NE.0)RETURN
        dim=XDIM
        WRITE(6,11)UTFIL2(1:3)
  11    FORMAT('Filename                    <',A,'>:',$)
        CALL READA(5,UTFIL2)
        IF(Istatus.NE.0)RETURN
        CALL LENGDE(UTFIL2,LIN)
        DO j=0,NSPEC-1
          DO i=0,4095
            Spec(i)=rMAT(IDEST,i,j)
          ENDDO
          FILNAM=UTFIL2(1:LIN)//APP(j+1)
          OPEN(IUTF,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
          CALL norw1dim(IUTF,COMM(1,IDEST),dim,Spec,Calib)
          CLOSE(IUTF)
        ENDDO
      ENDIF

      IF(ITYPE.EQ.3)THEN
        WRITE(6,23)cal(1,IDEST,1,1)
 23     FORMAT(/'Cal. coeff. a0 (keV) on x-axis     <',F11.1,'>:',$)
        CALL READF(5,cal(1,IDEST,1,1))
        IF(Istatus.NE.0)RETURN
        WRITE(6,24)cal(1,IDEST,1,2)
 24     FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis  <',F11.3,'>:',$)
        CALL READF(5,cal(1,IDEST,1,2))
        IF(Istatus.NE.0)RETURN
        WRITE(6,25)cal(1,IDEST,1,3)
 25     FORMAT( 'Cal. coeff. a2 (keV/ch2) on x-axis <',E11.4,'>:',$)
        CALL READF(5,cal(1,IDEST,1,3))
        IF(Istatus.NE.0)RETURN
        WRITE(6,26)cal(1,IDEST,2,1)
 26     FORMAT(/'Cal. coeff. a0 (keV) on y-axis     <',F11.1,'>:',$)
        CALL READF(5,cal(1,IDEST,2,1)) 
        IF(Istatus.NE.0)RETURN
        WRITE(6,27)cal(1,IDEST,2,2)
 27     FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis  <',F11.3,'>:',$)
        CALL READF(5,cal(1,IDEST,2,2))
        IF(Istatus.NE.0)RETURN
        WRITE(6,28)cal(1,IDEST,2,3)
 28     FORMAT( 'Cal. coeff. a2 (keV/ch2) on y-axis <',E11.4,'>:',$)
        CALL READF(5,cal(1,IDEST,2,3))
        IF(Istatus.NE.0)RETURN
        WRITE(6,30)XDIM
  30    FORMAT(/'Dimension on x-axis (max=4096) <',I4,'>:',$)
        CALL READI(5,XDIM)
        IF(Istatus.NE.0)RETURN
        IF(XDIM.GT.4096)XDIM=4096
        WRITE(6,31)YDIM
  31    FORMAT( 'Dimension on y-axis (max=2048) <',I4,'>:',$)
        CALL READI(5,YDIM)
        IF(Istatus.NE.0)RETURN
        IF(YDIM.GT.2048)YDIM=2048
        IF(Istatus.NE.0)RETURN
        WRITE(6,32)UTFIL3(1:4)
  32    FORMAT( 'Filename                       <',A,'>:',$)
        CALL READA(5,UTFIL3)
        IF(Istatus.NE.0)RETURN
        FILNAM=UTFIL3
        OPEN(IUTF,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
        CALL norw2dim(IUTF,COMM(1,IDEST))
        CLOSE(IUTF)         
      ENDIF

      CALL SetMarker(1,1,1)

      IF(ITYPE.NE.1.AND.ITYPE.NE.2.AND.ITYPE.NE.3)GO TO 888
      GO TO 99
9999  WRITE(6,*)'No file access'
      CLOSE(IUTF)
  99  CONTINUE
      END
 
   
      SUBROUTINE READFILE
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      INTEGER XDIM,YDIM,dim
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      CHARACTER FILNAM*80,FILNAMX*80,APP*4,text*110,tableline*5000
      REAL Calib(6), Spec(0:8191)
      CHARACTER*1 ans

      INTEGER*2 mctype,mca,seg,strtch,lngtdt      !MCA (MultiChannelAnalyzer on PC)
      INTEGER*2 begrec,endrec,Iold2               !MCA
      INTEGER*4 spcin(8),xdum,lvetme,rltime,Iold4 !MCA
      CHARACTER*1 srttme(4),srtsec(2),srtdte(8)   !MCA

C******************************************************* 
C
C  INPUT-PROCEDURE OF READING 1 OR 2 DIMENSIONAL SPECTRA
C
C******************************************************* 
      ITYPEold=ITYPE
      IDESTold=IDEST
      IDEST=1
      WRITE(6,1)IDEST
1     FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      IF(IDEST.GT.2)Istatus=1
      IF(Istatus.NE.0)RETURN
      NFILIN=0
      INF=20
      ITYP=0
      ITYPE=0
      FILNAM='TEST'

C Trying first to open file and extract parameters from
C the header file. After this we ask, with proper defaults
      WRITE(6,2)FILNAM(1:4)
  2   FORMAT('Filename          <',A,'>:',$)
      CALL READA(5,FILNAM)
      IF(Istatus.NE.0)RETURN
      OPEN(INF,FILE=FILNAM,ACCESS='SEQUENTIAL',status='old',ERR=901)
      GO TO 3

C No file with this name. Checks if it is of type NA-0, NA-1, NA-2,..etc.
 901  CLOSE(INF)
      CALL LENGDE(FILNAM,LIN)
      DO i=1,2048
        FILNAMX=FILNAM(1:LIN)//APP(i)
        OPEN(INF,FILE=FILNAMX,ACCESS='SEQUENTIAL',status='old',ERR=902)
        ITYP=2        ! The spectra are of type NA-0, NA-1, NA-2,..
        GO TO 3       ! out of loop and reads spectra of type 2
 902    CONTINUE
      ENDDO

      WRITE(6,4)FILNAM(1:LIN),FILNAM(1:LIN)
 4    FORMAT('File: ',A,' or ',A,'xx (xx=number) do not exist')
      CLOSE(INF)
      ITYPE=ITYPEold
      IDEST=IDESTold
      RETURN

 3    CONTINUE                       !OK, file exist, reads the header

      READ(INF,5,ERR=999)text
 5    FORMAT(A110)
      READ(text(1:1),6,ERR=903)
 6    FORMAT(A)

      IF(text(1:1).EQ.'!')THEN       ! It is a mama file
        REWIND(INF)
        DO i=1,10 
          READ(INF,5,ERR=903)text
          IF(text(1:10).EQ.'!DIMENSION')THEN
            READ(text(12:12),7)I1
 7          FORMAT(I1)
            IF(I1.EQ.1.AND.ITYP.EQ.0)THEN
              ITYPE=1 
            ENDIF
            IF(I1.EQ.1.AND.ITYP.EQ.2)THEN
              ITYPE=2 
            ENDIF
            IF(I1.EQ.2.AND.ITYP.EQ.0)THEN
              ITYPE=3 
            ENDIF
          ENDIF
        ENDDO
      
 903    REWIND(INF)

        IF(ITYPE.EQ.1)THEN             !singles spectrum
          DO i=0,8191
            Spec(i)=0
          ENDDO
          CALL norr1dim(INF, comm(2,IDEST), dim, Spec, Calib)
          DO i=1,3            !Adopting the calibration from file
            IF(Calib(1).NE.0.OR.Calib(2).NE.1)cal(2,IDEST,1,i)=Calib(i)
          ENDDO
          DO i=0,8191
            rSPEC(IDEST,i)=Spec(i)
          ENDDO
          MAXCH=dim-1
          CALL SetMarker(1,2,0)
        ENDIF

        IF(ITYPE.EQ.2)THEN             ! spectra NA-0, NA-1,...
          CLOSE(INF)
          DO j=0,2047
            DO i=0,4095
              Spec(i)=0
            ENDDO
            FILNAMX=FILNAM(1:LIN)//APP(j+1)
            OPEN(INF,FILE=FILNAMX,ACCESS='SEQUENTIAL',status='old',ERR=904)
            CALL norr1dim(INF, comm(1,IDEST), dim, Spec, Calib)
            CLOSE(INF)
            YDIM=j+1     !The last successful spectrum determine Y-dimension
904         CONTINUE
            DO i=0,4095
              rMAT(IDEST,i,j)=Spec(i)
            ENDDO
          ENDDO
          XDIM=dim
          DO i=1,3
            IF(Calib(1).NE.0.OR.Calib(2).NE.1)cal(1,IDEST,1,i)=Calib(i)
          ENDDO
          cal(1,IDEST,2,1)=0.
          cal(1,IDEST,2,2)=1.
          cal(1,IDEST,2,3)=0.
          CALL SetMarker(1,1,1)
        ENDIF

        IF(ITYPE.EQ.3)THEN                 !matrix
          CALL norr2dim(INF)
          CALL SetMarker(1,1,1)
        ENDIF

905     CONTINUE
        CLOSE(INF)
        GO TO 3333
      ENDIF
C Checks if it is a PAW file - just one array of numbers 
C or many coloumns of arrayes of numbers (table)
C No of coloumns is ydim and rows is xdim
C Checks if it is a table, e.g. Is no of coloumns > 1?

      READ(INF,*,ERR=906)xdumr     !ERR=It was no number here, not a PAW-file
      REWIND(INF)
      icolo = 0
      READ(INF,24,END=29,ERR=29)tableline
 24   FORMAT(A1000)
      REWIND(INF)
      DO n=1,2047
         READ(tableline(1:5000),*,END=20,ERR=20)(Spec(j),j=0,n-1)
      ENDDO
		
 20   IF(n.LT.3)GO TO 29
      ITYPE = 3
      YDIM = n-1
      REWIND(INF)
      DO i=0,4095
         READ(INF,*,END=21,ERR=21) (rMAT(IDEST,i,j),j=0,YDIM-1)
      ENDDO
 21   XDIM=i
      WRITE(6,22)XDIM,YDIM
 22   FORMAT('Paw matrix (table) has dimension ',I4,' x ',I3)
      CALL SetMarker(1,1,1)
      comm(1,IDEST)='paw '
      CLOSE(INF)
      GO TO 3333

C We have one coloumn, but it may still be a paw-matrix
 29   kch=0
      DO k=0,(4096*2048)-1
        READ(INF,*,END=10,ERR=906)xdumr
        kch=kch+1
      ENDDO
 10   IF(kch.LE.1)GO TO 906
      IF(k.LE.8192)THEN
        ans='y'
        ITYPE=1
        WRITE(6,11)kch+0      !new for g77 mama6.1
 11     FORMAT(/'PAW file with ',I8,' channels')
        WRITE(6,12)ans
 12     FORMAT('Is it a singles spectrum (y/n)? <',A1,'>:',$)
        CALL READA1(5,ans)
        IF(Istatus.NE.0)RETURN
        IF(ans.NE.'y')ITYPE=3
      ELSE
        ans='y'
        ITYPE=3
        WRITE(6,11)kch+1
        WRITE(6,13)ans
 13     FORMAT('Is it a 2-dimensional matrix (y/n)? <',A1,'>:',$) 
        CALL READA1(5,ans)
        IF(Istatus.NE.0)RETURN
        IF(ans.NE.'y')ITYPE=1
      ENDIF
      IF(ITYPE.EQ.1)THEN            !PAW singles
        REWIND(INF)
        DO i=0,8191
          READ(INF,*,END=14,ERR=906) rSPEC(IDEST,i)
        ENDDO
14      MAXCH=i-1
        CALL SetMarker(1,2,0)
        comm(2,IDEST)='paw '
        CLOSE(INF)
        GO TO 3333
      ENDIF
      IF(ITYPE.EQ.3)THEN           !PAW matrix
        REWIND(INF)
        XDIM=MIN0(4096,kch+1)
        WRITE(6,15)XDIM
15      FORMAT('Give dimension on x-axis <',I4,'>:',$)
        CALL READI(5,XDIM)
        IF(Istatus.NE.0)GO TO 906
        DO j=0,2047
          DO i=0,XDIM-1
            READ(INF,*,END=16,ERR=906) rMAT(IDEST,i,j)
          ENDDO
        ENDDO
16      YDIM=j
        WRITE(6,17)XDIM,YDIM
17      FORMAT('Paw matrix has dimension ',I4,' x ',I3)
        CALL SetMarker(1,1,1)
        comm(1,IDEST)='paw '
        CLOSE(INF)
        GO TO 3333
      ENDIF
         
906   CONTINUE
 
C It could be an MCA-spectrum (ORTEC multichannel analyzer)
C Uncertain to me if we should have RECL=8 or RECL=32 here, depends on -xl:
      CLOSE(INF)
      OPEN(INF,FILE=FILNAM,STATUS='OLD',ACCESS='DIRECT',RECL=32,ERR=907)     
      READ(INF,REC=1,ERR=907)mctype,mca,seg,srtsec,rltime,lvetme,srtdte,srttme,strtch,lngtdt

C Transform from Intel to Sparc representation of INTEGER number
      Iold2=mctype
      CALL TransI2(Iold2,mctype)
      IF(mctype.NE.-1)GO TO 907    !Not an ORTEC MCA-spectrum
      Iold2=mca
      CALL TransI2(Iold2,mca)
      Iold2=seg
      CALL TransI2(Iold2,seg)
      Iold4=rltime
      CALL TransI4(Iold4,rltime)
      Iold4=lvetme
      CALL TransI4(Iold4,lvetme)
      Iold2=strtch
      CALL TransI2(Iold2,strtch)
      Iold2=lngtdt
      CALL TransI2(Iold2,lngtdt)
      WRITE(6,40)
 40   FORMAT(/'ORTEC MCA singles spectrum:')
      WRITE(6,41)mctype,mca,seg,rltime/50,
     1 lvetme/50,srttme,srtsec,srtdte,strtch,lngtdt
 41   FORMAT('Type=',I4,' mca #',I2,' Segment # ',
     1 I3,/,'Realtime= ',I10,' seconds, Livetime= ',
     2 I10,' seconds',/,'Data collected at ',2A1,':',2A1,
     3 ':',2A1,' on ',2A1,'-',3A1,'-',3A1,/,
     4 'Starting channel=',I6,' Number of channels=' ,
     5 I6,//)

C Reading spectrum
      ii=0
      DO ich=strtch+1,lngtdt,8
        chanel=ich-1
        endrec=chanel/8.
        begrec=chanel/8.
        DO n=begrec+2,endrec+2
          READ(INF,rec=n,ERR=42)(spcin(l),l=1,8)
          DO l=1,8
            ii=ii+1
            Iold4=spcin(l)
            CALL TransI4(Iold4,xdum)
            IF(ABS(xdum).GT.1677215)xdum=0
            rSPEC(IDEST,ii-1)=xdum
            IF(xdum.GT.0)itop=ii
          ENDDO
        ENDDO
      ENDDO
 42   IF(ii.LT.3)GO TO 907           !Not a real MCA-spectrum
      IF(itop.LT.(lngtdt/2)) THEN
         WRITE(6,*)'Warning: Less than half spectrum filled with data!'
         WRITE(6,*)'You probably forgot to use binary transfer (FTP binary).'
      ENDIF
      MAXCH=ii-1
      ITYPE=1
      CALL SetMarker(1,2,0)
      comm(2,IDEST)='mca '
      CLOSE(INF)

C Stripping away path-name from spectrum name
3333  CALL LENGDE(FILNAM,LIN)
      ii=0
      DO i=1,LIN
        IF(FILNAM(i:i).EQ.'/')ii=i  ! ii marks the position of last '/'
      ENDDO
      I1=1                !matrix
      IF(ITYPE.EQ.1)I1=2  !singles
      DO i=1,MIN0(LIN,8)
        j=i+ii
        IF(j.LE.80)fname(I1,IDEST)(i:i)=FILNAM(j:j)
      ENDDO
      DO i=MIN0(LIN,8)+1,8          !blanks out the rest
        fname(I1,IDEST)(i:i)=' '
      ENDDO
      xcomm='|RE:'//fname(I1,IDEST)(1:MIN0(LIN,8))
      CALL AddComment(xcomm,12)
      RETURN 

907   IF(ITYPE.EQ.0)THEN
        WRITE(6,*)'Sorry, not a spectrum/matrix file'
        WRITE(6,*)'The spectrum is of unknown type (mama (1,2,3), MCA or PAW)'
        CLOSE(INF)
        ITYPE=ITYPEold
        IDEST=IDESTold
        RETURN
      ENDIF
999   WRITE(6,*)'Tullball with the header file, could not retrieve info, go home!'
      ITYPE=ITYPEold
      IDEST=IDESTold
      RETURN
      END


      SUBROUTINE norr1dim(device, comment, dim, Spec, Calib)
C Reads a one-dimensional spectrum from disk.
C Dimension and calibration returned in dim and Calib. IF dim=-1 at
C startup, then no headers are shown at reading
      INTEGER device, dim
      CHARACTER text*110,dum*1,comment*60
      REAL Calib(6), Spec(0:8191)
      NoOutput=0
      IF(dim.EQ.-1)NoOutput=1
      dim=10
      linemax=12
C Read file header
      DO i=1,12
        READ(device,100,ERR=110)text
100     FORMAT(A110)
        IF(text(1:1).NE.'!')THEN
          linemax=i-1
          GO TO 91
        ENDIF
        DO ii=1,110                       ! Finding length of textstring
          iii=111-ii
          IF(text(iii:iii).NE.' ')GO TO 90
        ENDDO
 90     IF(NoOutput.NE.1)WRITE(6,*)text(2:iii)
        IF(text(1:12).EQ.'!CALIBRATION')THEN
          READ(text(20:61),105,ERR=106)dum,Calib(1),dum,Calib(2),dum,Calib(3)
105       FORMAT(3(A1,E13.6))
        ENDIF
106     CONTINUE
        IF(text(1:10).EQ.'!DIMENSION')THEN
          READ(text(16:19),107,ERR=108)dim
107       FORMAT(I4)
        ENDIF
108     CONTINUE
        IF(text(1:9).EQ.'!COMMENT=')THEN
          DO k=10,69
            comment(k-9:k-9)=text(k:k)
          ENDDO
        ENDIF
      ENDDO
 91   dim=dim+1
C Read spectrum
      REWIND(device)
      DO i=1,linemax
        READ(device,100,ERR=110)text
      ENDDO
      READ(device,*,ERR=110)(Spec(i),i=0,dim-1)
      IF(NoOutput.NE.1)WRITE(6,*)' '
      RETURN
110   WRITE(6,*)'Reading problems. Wrong dimension of spectrum?'
      RETURN
      END


      SUBROUTINE norr2dim(device)
C Read a two-dimensional spectrum matrix from disk 
      CHARACTER text*110,APP*4,dum*1
      INTEGER device,XDIM,YDIM
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

C Read file header
      DO i=1,10
        READ(device,100,ERR=800)text
100     FORMAT(A110)
        DO ii=1,110                          !Finding length of textstring
          iii=111-ii
          IF(text(iii:iii).NE.' ')GO TO 90
        ENDDO
 90     WRITE(6,*)text(2:iii)
        IF(text(1:12).EQ.'!CALIBRATION')THEN
          READ(text(20:103),105,ERR=106)
     +    dum,ax0,dum,ax1,dum,ax2,
     +    dum,ay0,dum,ay1,dum,ay2
105       FORMAT(6(A1,E13.6))
c          IF(ax0.NE.0.OR.ax1.NE.1.OR.ay0.NE.0.OR.ay1.NE.1)THEN
             cal(1,IDEST,1,1)=ax0
             cal(1,IDEST,1,2)=ax1
             cal(1,IDEST,1,3)=ax2
             cal(1,IDEST,2,1)=ay0
             cal(1,IDEST,2,2)=ay1
             cal(1,IDEST,2,3)=ay2
c          ENDIF
        ENDIF
106     CONTINUE
        IF(text(1:10).EQ.'!DIMENSION')THEN
          j1=0
          DO j=1,110-3
            IF(text(j:j+2).EQ.',0:')THEN  !Finding dimensions
              IF(j1.EQ.0)THEN
                j1=j+3
              ELSE
                j2=j-1
                j3=j+3
                j4=j3+3
              ENDIF
            ENDIF
          ENDDO
          READ(text(j1:j2),107,ERR=108)NX
          READ(text(j3:j4),107,ERR=108)NY
          XDIM=NX+1
          YDIM=NY+1
 107      FORMAT(I4)
        ENDIF
 108    CONTINUE
        IF(text(1:9).EQ.'!COMMENT=')THEN
          DO k=10,69
            comm(1,IDEST)(k-9:k-9)=text(k:k)
          ENDDO
        ENDIF
      ENDDO
 
C Read matrix
      DO j=0,NY
        jt=((j+1)/50)*50
        IF(jt.EQ.j+1)THEN
          write(6,FMT='(A1,$)')'.'
          call flush(6)
        ENDIF
        READ(device,*,ERR=800)(rMAT(IDEST,i,j),i=0,NX)
      ENDDO
      WRITE(6,*)' '
      RETURN
  800 WRITE(6,*)'Reading problems. Dimension of matrix wrong?'
      RETURN
      END

 
      SUBROUTINE norw1dim(device, comment, dim, Spec, Calib)
C Writes a singles spectrum array to disk
      character comment*60, id*20
      real Calib(6), Spec(0:8191)
      integer device, dim
      iCal=3
      CALL DATETIME(id)

C --  Write file header
      WRITE (device,100)
      WRITE (device,101)
      WRITE (device,102) comment
      WRITE (device,103) id
      WRITE (device,104) iCal,(Calib(i),i=1,iCal)
      WRITE (device,105) dim-1, dim-1
 100  FORMAT ('!FILE=Disk')
 101  FORMAT ('!KIND=Spectrum',/,
     +        '!LABORATORY=Oslo Cyclotron Laboratory (OCL)',/,
     +        '!EXPERIMENT=mama')
 102  FORMAT ('!COMMENT=',A60)
 103  FORMAT ('!TIME=DATE:',A20)
 104  FORMAT ('!CALIBRATION EkeV=',I1,3(',',E13.6))
 105  FORMAT ('!PRECISION=16',/,'!DIMENSION=1,0:',I4,/,'!CHANNEL=(0:',I4,')')

C --  Write spectrum
      WRITE (device,*)(Spec(i),i=0,dim-1)
      WRITE (device,106)
 106  FORMAT ('!IDEND='/)
      RETURN
      END

 
      SUBROUTINE norw2dim(device,comment)
C Write a two-dimensional spectrum matrix to disk 
      CHARACTER comment*60,APP*4,id*20
      REAL Calib(6)
      INTEGER device,XDIM,YDIM
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      NX=XDIM-1
      NY=YDIM-1
      iCal=6
      m=0
      DO i=1,2
        DO j=1,3
          m=m+1
          Calib(m)=cal(1,IDEST,i,j)
        ENDDO
      ENDDO

      CALL DATETIME(id)
   
C --  Write file header
      WRITE(device,100)
      WRITE(device,101)
      WRITE(device,102) comment
      WRITE(device,103) id
      WRITE(device,104) iCal,(Calib(i),i=1,iCal)
      WRITE(device,105) NX,NY,NX,NY
 100  FORMAT('!FILE=Disk')
 101  FORMAT('!KIND=Spectrum',/,
     +'!LABORATORY=Oslo Cyclotron Laboratory (OCL)',/,
     +'!EXPERIMENT=mama')
 102  FORMAT('!COMMENT=',A60)
 103  FORMAT('!TIME=DATE:',A20)
 104  FORMAT('!CALIBRATION EkeV=',I1,6(',',E13.6))
 105  FORMAT('!PRECISION=16',/,'!DIMENSION=2,0:',I4,',0:',I4,/,
     +'!CHANNEL=(0:',I4,',0:',I4,')')

C Write matrix
      DO j=0,NY
        WRITE(device,*)(rMAT(IDEST,i,j),i=0,NX)
      ENDDO
      WRITE(device,106)
 106  FORMAT('!IDEND='/)
      RETURN
      END


      SUBROUTINE WRITEPAW
C Write spectra or matrix to file that can be accessed by PAW 
C The spectra is simply a free-format write of integers (no header, calibr., etc.)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER UTFIL1*80,UTFIL3*80,FILNAM*80,APP*4

      WRITE(6,1)IDEST
1     FORMAT(/'Spectrum to write            <',I1,'>:',$)
      CALL READI(5,IDEST)
      IF(IDEST.GT.2)Istatus=1
      IF(Istatus.NE.0)RETURN

      UTFIL1='SPEC'
      UTFIL3='TEST'
      IUTF  = 21

 888  CONTINUE
      WRITE(6,2)
   2  FORMAT('Singles spectrum                1',/,
     1       '2-dimensional spectrum (matrix) 3')
      WRITE(6,3)ITYPE
   3  FORMAT('Please, choose your type     <',I1,'>:',$)

      CALL READI(5,ITYPE)
      IF(Istatus.NE.0)RETURN
      IF(ITYPE.NE.1.AND.ITYPE.NE.3)GO TO 888
 
      IF(ITYPE.EQ.1)THEN
        LEN=MAXCH+1
        WRITE(6,4)LEN
    4   FORMAT('Length of output-spectrum <',I4,'>:',$)
        CALL READI(5,LEN)
        IF(Istatus.NE.0)RETURN
        WRITE(6,5)UTFIL1(1:4)
    5   FORMAT('Filename                  <',A,'>:',$)
        CALL READA(5,UTFIL1)
        IF(Istatus.NE.0)RETURN
        CALL LENGDE(UTFIL1,LIN)
        FILNAM=UTFIL1(1:LIN)         
        OPEN(IUTF,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
        DO i=0,LEN-1
          WRITE(IUTF,*,ERR=9999)rSPEC(IDEST,i)
        ENDDO
      ENDIF

      IF(ITYPE.EQ.3)THEN
        IF(Istatus.NE.0)RETURN
        WRITE(6,30)XDIM
  30    FORMAT(/'Dimension on x-axis (max=4096) <',I4,'>:',$)
        CALL READI(5,XDIM)
        IF(Istatus.NE.0)RETURN
        IF(XDIM.GT.4096)XDIM=4096
        WRITE(6,31)YDIM
  31    FORMAT( 'Dimension on y-axis (max=2048) <',I4,'>:',$)
        CALL READI(5,YDIM)
        IF(Istatus.NE.0)RETURN
        IF(YDIM.GT.2048)YDIM=2048
        IF(Istatus.NE.0)RETURN
        WRITE(6,32)UTFIL3(1:4)
  32    FORMAT( 'Filename                       <',A,'>:',$)
        CALL READA(5,UTFIL3)
        IF(Istatus.NE.0)RETURN
        FILNAM=UTFIL3
        OPEN(IUTF,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
        NX=XDIM-1
        NY=YDIM-1
C  Write matrix
        DO j=0,NY
          DO i=0,NX
            WRITE(IUTF,*)rMAT(IDEST,i,j)
          ENDDO
        ENDDO
      ENDIF

      CALL SetMarker(1,1,1)

      GO TO 99
9999  WRITE(6,*)'No file access or writing problems'
  99  CONTINUE
      CLOSE(IUTF)
      END		
		
c		SUBROUTINE CLOCK(string)
c      CHARACTER*30 string
c      INTEGER i, time
c      i=time8()
c      call ctime(i,string)
c      RETURN
c      END

		SUBROUTINE CLOCK(id)
		character*8 date
		character*10 time
		character*17 id
		call date_and_time(date,time) 
		id=date(7:8)//"/"//date(5:6)//"/"//date(3:4)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)		
		RETURN
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


       SUBROUTINE LENGDE(TEXT,LEN)
C Calculating the length of a string of characters             
       CHARACTER TEXT*80
       CHARACTER CH*1
       DO K=1,80
         CH=TEXT(K:K)
         IF(CH.EQ.' ')THEN
           LEN=K-1
           GO TO 20
         ENDIF
       ENDDO
   20  RETURN
       END


!     Routine to build filenames from an environment variable plus a
!     tail. Purpose is to avoid putting absolute paths into the
!     programs.
!
!     Example:
!     CHARACTER filnam*255
!     CALL MAKEPATH("UIO_APPLICATIONS","prog/dummydir/dummyfile",filnam)
!
!     In the special case of 'UIO_APPLICATIONS', "/Applications" is used
!     if the environment variable is not set.
      SUBROUTINE MAKEPATH(BASE,TAIL,FULL)
      CHARACTER BASE*(*), BASEENV*255, TAIL*(*), FULL*255
      INTEGER LB
      CALL getenv(BASE, BASEENV)
      CALL LENGDE(BASEENV,LB)
      if(LB.eq.0.and.base.eq.'UIO_APPLICATIONS') then
         BASEENV="/Applications"
         CALL LENGDE(BASEENV,LB)
      end if
      if(LB.eq.0.and.base.eq.'MAMA_MYRESP') then
         BASEENV="myresp"
         CALL LENGDE(BASEENV,LB)
      end if
      FULL = BASEENV(1:LB)//'/'//TAIL
      RETURN
      END


      SUBROUTINE TransI2(I1,I2)
C Routine that transform a I*2 integer of Intel (or VMS) processor to
C an I*2 integer of Sparc (or MC680xx) prosessor
C See p.426 in SPARCompiler FORTRAN 2.0.1, Reference Manual
C These operations are wonderful. Personally, I get a cick for each shift...
      INTEGER*2 mask1,mask2,I1,I2,J1,J2,I,ISHFT

c      mask1=B'0000000011111111'
c      mask2=B'1111111100000000'
      mask1=255
      mask2=-256

      I2=0
      J1=0
      J2=0
   
      I =0
      I =iAND(I1,mask1)
      J1=ISHFT(I,8)          !shifts I-bits 8 places to the left 

      I =0
      I =iAND(I1,mask2)
      J2=ISHFT(I,-8)          !shifts I-bits 8 places to the right

C Putting together again the 16 bit word 
      I2=iOR(J1,I2)     
      I2=iOR(J2,I2)

      END


      SUBROUTINE TransI4(I1,I2)
C Routine that transform a I*4 integer of Intel (or VMS) processor to
C an I*4 integer of Sparc (or MC680xx) prosessor
C See p.426 in SPARCompiler FORTRAN 2.0.1, Reference Manual
      INTEGER*4 mask1,mask2,mask3,mask4,I1,I2,I,J1,J2,J3,J4,ISHFT
      
      mask1=B'00000000000000000000000011111111'
      mask2=B'00000000000000001111111100000000'
      mask3=B'00000000111111110000000000000000'
c      mask4=B'11111111000000000000000000000000'
      mask4=-16777216
      
      I2=0
      J1=0
      J2=0
      J3=0
      J4=0
      
      I =0
      I =iAND(I1,mask1)
      J1=ISHFT(I,24)        !Shift 24 to left

      I =0
      I =iAND(I1,mask2)
      J2=ISHFT(I,8)         !Shift 8 to left

      I =0
      I =iAND(I1,mask3)
      J3=ISHFT(I,-8)        !Shift 8 to right

      I =0
      I =iAND(I1,mask4)
      J4=ISHFT(I,-24)       !Shift 24 to right

      I2=iOR(J1,I2)
      I2=iOR(J2,I2)
      I2=iOR(J3,I2)
      I2=iOR(J4,I2)
      
      END
