      program  Spectra_and_multiplicity_distrib_for_oslo
C     
c     Program produces ALFNA from events simulated by Milan dec. 2005
c     - total spectra (also smeared) 
c     - primary spectra (also smeared)           
c     
      CHARACTER*2 EXT
      DIMENSION  EN(0:128),IC(128)
      
C     Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
      XDIM = 100
      YDIM = 100
      IDEST = 1
      NNRI=1
      NNR=78
      DO INR=NNRI,NNR
         write(*,*) 'Spectrum from:',INR
         WRITE (EXT,100) INR
 100     FORMAT (I2.2)
         IEV=0 
         OPEN (UNIT=20,FILE='C_EV_03.'//EXT//'.txt',STATUS='OLD',ERR=1000)

    2    CONTINUE
         IEV=IEV+1
         
         IF (MOD(IEV,5000).EQ.0) WRITE(*,*) IEV
         READ (20,*,END=1000) EN(0),SPIN,IPAR,NSTEP
         READ (20,*,END=1000) (EN(J),J=1,NSTEP)
         READ (20,*,END=1000) (IC(J),J=1,NSTEP)
         j = INT((EN(0)*1000./120.)+0.5)
         DO k=1,NSTEP
            IF(IC(k).EQ.0)THEN
               gamma = EN(k-1) - EN(k)
               i = INT((gamma*1000./120.)+0.5)
               rMAT(IDEST,i,j) =  rMAT(IDEST,i,j) + 1
            ENDIF
         ENDDO
         GOTO 2
         
 1000    CONTINUE
         CLOSE(20)
         IF (IEV.EQ.0) IEV=-1
      ENDDO                     !INR

C     Writting spectra to matrix
      cal(1,1,1,1)=0
      cal(1,1,1,2)=120.
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=120.
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
