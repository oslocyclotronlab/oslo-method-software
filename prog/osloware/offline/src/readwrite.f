C BE AWARE THAT DIM IN THEESE ROUTINES IS NOT DIMENSION, BUT 1 LESS.
C A SPECTRUM WITH DIM HAS DATA SPEC(0:DIM), THAT MEANS DIM+1 NUMBERS
C (IN MAMA THE CONVENSION IS TO USE THE NUMBER OF CHANNELS = DIMENSION)
C==============================================================%
      SUBROUTINE norw1dim(device, comment, xdim, ax)
C==============================================================%
C     PURPOSE : Write a spectrum array to disk.                %
C     INPUT via parameter  : device  : INTEGER                 %
C                               = filenumber                   %
C                            comment : CHARACTER               %
C                               = comment in spectrum file     %
C                            xdim    : INTEGER                 %
C                               = x-dimension of matrix        %
C     OUTPUT via parameter : NONE                              %
C                                                              %
C                            ax      : integer                 %
C                               = spectrum vector              %
C     INPUT via common     : NONE                              %
C     OUPTUT via common    : NONE                              %
C==============================================================%
      IMPLICIT NONE

      character comment*60
      character ctime*24, currenttime*24
      integer systime, time

      real cal(3)
      integer device,xdim
      integer ical,i

      real ax(0:4095)

      ical = 3
      cal(1)  = 0.0
      cal(2)  = 1.0
      cal(3)  = 0.0

      systime = time()
      currenttime = ctime( systime )		! Get current time

C --  Write file header
      write (device,100)
      write (device,101)
      write (device,102) comment
      write (device,103) currenttime
      write (device,104) ical,(cal(i),i=1,ical)
      write (device,105) xdim,xdim

C --  Write specter
      write (device,*)(ax(i),i=0,xdim)

      write (device,106)

 100  FORMAT ('!FILE=Disk')
 101  FORMAT ('!KIND=Spectrum',/,
     +        '!LABORATORY=Oslo Cyclotron Laboratory (OCL)',/,
     +        '!EXPERIMENT=Sirius')
 102  FORMAT ('!COMMENT=',A60)
 103  FORMAT ('!TIME=DATE:',A24)
 104  FORMAT ('!CALIBRATION EkeV=',I1,3(',',E13.6))
 105  FORMAT ('!PRECISION=16',/,'!DIMENSION=1,0:',I4,/,'!CHANNEL=(0:',I4,')')
 106  FORMAT ('!IDEND='/)

      return
      end          ! End-Of-Subroutine norw1dim


C==============================================================%
      SUBROUTINE norw2dim( device, comment, xdim, ydim, mx )
C==============================================================%
C     PURPOSE : Write a two-dimensional spectrum matrix to     %
C               disk.                                          %
C     INPUT via parameter  : device  : INTEGER                 %
C                               = filenumber                   %
C                            comment : CHARACTER               %
C                               = comment in spectrum file     %
C                            xdim    : INTEGER                 %
C                               = x-dimension of matrix        %
C                            ydim    : INTEGER                 %
C                               = y-dimension of matrix        %
C                            mx      : integer                 %
C                               = spectrum matrix              %
C     OUTPUT via parameter : NONE                              %
C                                                              %
C     INPUT via common     : NONE                              %
C     OUPTUT via common    : NONE                              %
C==============================================================%
      IMPLICIT NONE

      character comment*60
      character ctime*24, currenttime*24
      integer systime, time
      real cal(6)
      integer device,xdim,ydim
      integer ical,i,j

      real mx(0:4095,0:511)

      ical = 6
      do i = 1,ical
         cal(i)  = 0.0
      end do
      cal(2)=1.
      cal(4)=1.


      systime = time()
      currenttime = ctime( systime )		! Get current time


C --  Write file header
      write (device,100)
      write (device,101)
      write (device,102) comment
      write (device,103) currenttime
      write (device,104) ical,(cal(i),i=1,ical)
      write (device,105) xdim,ydim,xdim,ydim

C --  Write matrix
      do j=0,ydim
        write (device,*)(mx(i,j),i=0,xdim)
      enddo
      write (device,106)

 100  FORMAT('!FILE=Disk')
 101  FORMAT('!KIND=Spectrum',/,
     +'!LABORATORY=Oslo Cyclotron Laboratory (OCL)',/,
     +'!EXPERIMENT=Sirius')
 102  FORMAT('!COMMENT=',A60)
 103  FORMAT('!TIME=DATE:',A24)
 104  FORMAT('!CALIBRATION EkeV=',I1,6(',',E13.6))
 105  FORMAT('!PRECISION=16',/,'!DIMENSION=2,0:',I4,',0:',I3,/,
     +'!CHANNEL=(0:',I4,',0:',I3,')')
 106  format ('!IDEND='/)

      return
      end          ! End-Of-Subroutine norw2dim 


C==============================================================%
      SUBROUTINE norr1dim(device,ioutp,efil,f,xdim,ia,a)
C==============================================================%
C     PURPOSE : Read a spectrum array from disk.               %
C     INPUT via parameter  : device  : INTEGER                 %
C                               = filenumber                   %
C                            ioutp   : INTEGER                 %
C                               = error message terminal       %
C                            efil    : INTEGER                 %
C                               = file number at call          %
C                            smx     : REAL                    %
C                               = x-dimension of matrix        %
C                                                              %
C     OUTPUT via parameter : f       : REAL                    %
C                               = spectrum array               %
C                                                              %
C     INPUT via common     : NONE                              %
C     OUPTUT via common    : NONE                              %
C==============================================================%
      IMPLICIT NONE

      INTEGER i,ii,iii,k,linemax,xdim
      INTEGER device,ioutp,ia
      REAL efil
      REAL f(0:4095),a(3)
      CHARACTER text*110,dum*1,comment*60

      efil=1         !not used
      ia=3
      xdim=10
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
 90     continue 
        IF(text(1:12).EQ.'!CALIBRATION')THEN
          READ(text(20:62),105,ERR=106)dum,a(1),dum,a(2),dum,a(3)
105       FORMAT(3(A1,E13.6))
        ENDIF
106     CONTINUE
        IF(text(1:10).EQ.'!DIMENSION')THEN
          READ(text(16:19),107,ERR=108)xdim
107       FORMAT(I4)
        ENDIF
108     CONTINUE
        IF(text(1:9).EQ.'!COMMENT=')THEN
          DO k=10,69
            comment(k-9:k-9)=text(k:k)
          ENDDO
        ENDIF
      ENDDO

 91   CONTINUE
      REWIND(device)
C Read spectrum
      DO i=1,linemax
        READ(device,100,ERR=110)text
      ENDDO
      READ(device,*,ERR=110)(f(i),i=0,xdim)

      RETURN
110   WRITE(ioutp,*)'Reading problems. Wrong dimension of spectrum?'
      RETURN
      END                   ! End-Of-Subroutine norr1dim


C=============================================================
      SUBROUTINE norr2dim(device,comment, xdim, ydim, mx )
C============================================================= %
C     PURPOSE : Read a two-dimensional spectrum matrix from    %
C               disk. Assume dimensions given in call          %
C     INPUT via parameter  : device  : INTEGER                 %
C                               = filenumber                   %
C                            comment : CHARACTER               %
C                               = comment in spectrum file     %
C                            xdim    : INTEGER                 %
C                               = x-dimension of matrix        %
C                            ydim    : INTEGER                 %
C                               = y-dimension of matrix        %
C     OUTPUT via parameter : NONE                              %
C                                                              %
C     INPUT via common     : COMMON /file2dim/                 %
C                               = spectrum matrix              %
C     OUPTUT via common    : NONE                              %
C============================================================= %

      IMPLICIT NONE

      character comment*60, text*110
      integer device,xdim,ydim
      integer i,j
      real mx(0:4095,0:511)

C --  Read file header. Assumes 10 lines
      DO I=1,10
        READ(device,100,ERR=800)text
        WRITE(6,100)text
 100    FORMAT(A110)
      ENDDO
      
C --  Read matrix
      DO j=0,ydim
        READ(device,*,ERR=800)(mx(i,j),i=0,xdim)
      ENDDO

      RETURN
  800 WRITE(6,*)' Reading problems. Dimension of matrix wrong?'
      RETURN
      END          ! End-Of-Subroutine norr2dim



