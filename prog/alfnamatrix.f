      program  alfnamatrix_from_simulations
C     
c     Program to produce ALFNA from simulations by Milan nov. 2006 and june 2007 dy162
c     
      CHARACTER*2 EXT
      character*1 dumx
C     Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
      XDIM = 101
      YDIM = 68
      IDEST = 1
      NNRI=1
      NNR=68
      DO INR=NNRI,NNR
         write(*,*) 'Spectrum from:',INR
         WRITE (EXT,100) INR
 100     FORMAT (I2.2)
         OPEN (UNIT=20,FILE='SP02_'//EXT//'.DAT',STATUS='OLD',ERR=1000)
         READ (20,*,END=1000) dumx ! Dropping the first line with text
         DO i=0,100
            j = INR-1
            READ (20,*,END=1000) dum, dum, rMAT(IDEST, i, j), dum
         ENDDO
 1000    CONTINUE
         CLOSE(20)
      ENDDO                     !INR

C     Writting spectra to matrix
      cal(1,1,1,1)=60.
      cal(1,1,1,2)=120.
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=8140.
      cal(1,1,2,2)=-120.
      cal(1,1,2,3)=0.
      outfile='alfnasort.dat'
      comment='Sorted simulated data'
      OPEN(21,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(21,comment)
      CLOSE(21)

      GO TO 98  
 99   WRITE(6,*)'Could not open file'
 98   CONTINUE
      
      END
