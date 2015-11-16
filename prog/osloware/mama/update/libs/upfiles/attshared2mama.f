C     =========================================================%
      subroutine  sirius_spectra( TYPE,specna )
C     =========================================================%
C     PURPOSE : Attach the SIRIUS shared memory histogram      %
C               area to the mama program
C     =========================================================%
      IMPLICIT NONE

      integer TYPE, I1
      integer specno
      character specna*8
      integer i, j

C --  Commons used by mama applications
      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      integer Istatus, ITYPE, IDEST, Idim, MAXCH, MSPEC
      real cal
      integer MAT, XDIM, YDIM      
      character APP*4

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT

C --  Define spectra 
      integer esp(0:2047,0:63)			
      integer desp(0:2047,0:63)
      integer edesp(0:2047,0:63)	
      integer thicksp(0:2047,0:63)
      integer gesp(0:4095,0:5)
      integer tgesp(0:511,0:5)
      integer nasp(0:2047,0:31)
      integer tnasp(0:511,0:31)
      integer singles(0:4095,0:9)

      pointer (pesp, esp)
      pointer (pdesp, desp)
      pointer (pedesp, edesp)
      pointer (pthicksp, thicksp)
      pointer (pgesp, gesp)
      pointer (ptgesp, tgesp)
      pointer (pnasp, nasp)
      pointer (ptnasp, tnasp)
      pointer (psingles , singles)


C --  Attach SIRIUS acq spectrum area shared memory
      integer attspec
      external attspec !$pragma C( attspec )	

C --  Detach SIRIUS acq spectrum area shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 
  


C --  ---------------------------------------------------------------------
C --  Copy the spectrum to be displayed into mama spectrum matrix 1
C --  ---------------------------------------------------------------------

      

C --  E-counter  matrix 
      if      ( specna .EQ. 'ESP') then

         specno = 2
         pesp = attspec( %VAL(specno) ) 	
         if ( pesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach  E-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = esp(i,j)
            end do
         end do

         if ( detshared( %VAL(pesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E spectra failed'
         end if


C --  DE-counter  matrix 
      else if ( specna .EQ. 'DESP') then

         specno = 3
         pdesp = attspec( %VAL(specno) ) 	
         if ( pdesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = desp(i,j)
            end do
         end do

         if ( detshared( %VAL(pdesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach DE spectra failed'
         end if


C --  E-DE-spectrum matrix to file 
      else if ( specna .EQ. 'EDESP') then

         specno = 4
         pedesp = attspec( %VAL(specno) ) 	
         if ( pedesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach E-DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = edesp(i,j)
            end do
         end do
 
         if ( detshared( %VAL(pedesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E-DE spectra failed'
         end if

C --  Thickness  matrix
      else if ( specna .EQ. 'THICKSP') then

         specno = 5
         pthicksp = attspec( %VAL(specno) ) 	
         if ( pthicksp  .EQ. 0) then
            write(*,*)'**** ERROR ****  Attach thickness spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = thicksp(i,j)
            end do
         end do

         if ( detshared( %VAL(pthicksp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Thick spectra failed'
         end if

C --  GE-spectrum matrix to file
      else if ( specna .EQ. 'GESP') then

         specno = 6
         pgesp = attspec( %VAL(specno) ) 	
         if ( pgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = gesp(i,j)
            end do
         end do

         if ( detshared( %VAL(pgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge spectra failed'
         end if


C --  Ge-T spectrum matrix to file
      else if ( specna .EQ. 'TGESP') then

         specno = 7
         ptgesp = attspec( %VAL(specno) ) 	
         if ( ptgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-T-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tgesp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge-T spectra failed'
         end if

C --  NaI spectrum matrix to file
      else if ( specna .EQ. 'NASP') then

         specno = 8
         pnasp = attspec( %VAL(specno) ) 	
         if ( pnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = nasp(i,j)
            end do
         end do

         if ( detshared( %VAL(pnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI spectra failed'
         end if

C --  NaI-T spectrum matrix to file
      else if ( specna .EQ. 'TNASP') then

         specno = 9
         ptnasp = attspec( %VAL(specno) ) 	
         if ( ptnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI-T spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tnasp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI-T spectra failed'
         end if

C --  Spectrum SINGLES
      else if ( specna .EQ. 'SINGLES') then

         specno = 1
         psingles = attspec( %VAL(specno) ) 	
         if ( psingles  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 10
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = singles(i,j)
            end do
         end do

         if ( detshared( %VAL(psingles) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Singles spectra failed'
         end if


      else
         write (*,*) ' *** ERROR *** Unknown spectrum name '	
         TYPE = -1
      end if 

      if(TYPE.EQ.2)then
        ITYPE=3
        I1=1
      endif
      if(TYPE.EQ.1)then        
        ITYPE=1
        I1=2
      endif
      if((TYPE.NE.-1).AND.(fname(I1,1).NE.specna))then
        fname(I1,1)=specna
      endif

      end  	! End-Of-Subroutine sirius_spectra



C     =========================================================%
      subroutine  offline_spectra( TYPE,specna )
C     =========================================================%
C     PURPOSE : Attach the OFFLINE shared memory histogram     %
C               area to the mama program
C     =========================================================%
      IMPLICIT NONE

      integer TYPE, I1
      integer specno
      character specna*8
      integer i, j

C --  Commons used by mama applications
      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      integer Istatus, ITYPE, IDEST, Idim,MAXCH,MSPEC
      real cal
      integer MAT, XDIM, YDIM      
      character APP*4

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT


C --  Define spectra 
      integer esp(0:2047,0:63)			
      integer desp(0:2047,0:63)
      integer edesp(0:2047,0:63)	
      integer thicksp(0:2047,0:63)
      integer gesp(0:4095,0:5)
      integer tgesp(0:511,0:5)
      integer nasp(0:2047,0:31)
      integer tnasp(0:511,0:31)
      integer alfna(0:2047,0:511)
      integer alfge(0:2047,0:511)
      integer xmat(0:2047,0:63)
      integer singles(0:4095,0:9)
			
      pointer (pesp, esp)
      pointer (pdesp, desp)
      pointer (pedesp, edesp)
      pointer (pthicksp, thicksp)
      pointer (pgesp, gesp)
      pointer (ptgesp, tgesp)
      pointer (pnasp, nasp)
      pointer (ptnasp, tnasp)
      pointer (psingles , singles)
      pointer (pansp, alfna)
      pointer (pagsp, alfge)
      pointer (pmtsp, xmat)


C --  Attach spectrum area shared memory
      integer offspec
      external offspec !$pragma C( offspec )	


C --  Detach spectrum area shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 




C --  ---------------------------------------------------------------------
C --  Copy the spectrum to be displayed into mama spectrum matrix 1
C --  ---------------------------------------------------------------------

      

C --  E-counter  matrix 
      if      ( specna .EQ. 'ESP') then

         specno = 2
         pesp = offspec( %VAL(specno) ) 	
         if ( pesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach  E-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = esp(i,j)
            end do
         end do

         if ( detshared( %VAL(pesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E spectra failed'
         end if


C --  DE-counter  matrix 
      else if ( specna .EQ. 'DESP') then

         specno = 3
         pdesp = offspec( %VAL(specno) ) 	
         if ( pdesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = desp(i,j)
            end do
         end do

         if ( detshared( %VAL(pdesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach DE spectra failed'
         end if

C --  E-DE-spectrum matrix to file 
      else if ( specna .EQ. 'EDESP') then

         specno = 4
         pedesp = offspec( %VAL(specno) ) 	
         if ( pedesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach E-DE-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = edesp(i,j)
            end do
         end do
 
         if ( detshared( %VAL(pedesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach E-DE spectra failed'
         end if

C --  Thickness  matrix
      else if ( specna .EQ. 'THICKSP') then

         specno = 5
         pthicksp = offspec( %VAL(specno) ) 	
         if ( pthicksp  .EQ. 0) then
            write(*,*)'**** ERROR ****  Attach thickness spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = thicksp(i,j)
            end do
         end do

         if ( detshared( %VAL(pthicksp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Thick spectra failed'
         end if


C --  GE-spectrum matrix to file
      else if ( specna .EQ. 'GESP') then

         specno = 6
         pgesp = offspec( %VAL(specno) ) 	
         if ( pgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = gesp(i,j)
            end do
         end do

         if ( detshared( %VAL(pgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge spectra failed'
         end if


C --  Ge-T spectrum matrix to file
      else if ( specna .EQ. 'TGESP') then

         specno = 7
         ptgesp = offspec( %VAL(specno) ) 	
         if ( ptgesp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach Ge-T-spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 6
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tgesp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptgesp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach Ge-T spectra failed'
         end if

C --  NaI spectrum matrix 
      else if ( specna .EQ. 'NASP') then

         specno = 8
         pnasp = offspec( %VAL(specno) ) 	
         if ( pnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = nasp(i,j)
            end do
         end do

         if ( detshared( %VAL(pnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI spectra failed'
         end if

C --  NaI-T spectrum matrix
      else if ( specna .EQ. 'TNASP') then

         specno = 9
         ptnasp = offspec( %VAL(specno) ) 	
         if ( ptnasp  .EQ. 0) then
            write (*,*) '**** ERROR ****  Attach NaI-T spectra failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 512
         ydim = 32
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = tnasp(i,j)
            end do
         end do

         if ( detshared( %VAL(ptnasp) ) .EQ. -1) then
            write (*,*) '**** ERROR ****  Detach NaI-T spectra failed'
         end if

C --  Spectrum SINGLES
      else if ( specna .EQ. 'SINGLES') then

         specno = 1
         psingles = offspec( %VAL(specno) ) 	
         if ( psingles  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach singles spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 4096
         ydim = 10
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = singles(i,j)
            end do
         end do

         if ( detshared( %VAL(psingles) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Singles spectra failed'
         end if

C --  Spectrum ALFNA
      else if ( specna .EQ. 'ALFNA') then
         specno = 10
         pansp = offspec( %VAL(specno) ) 	
         if ( pansp  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach Alpha-NaI spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = alfna(i,j)
            end do
         end do

         if ( detshared( %VAL(pansp) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Alpha-NaI spectra failed'
         end if

C --  Spectrum ALFGE
      else if ( specna .EQ. 'ALFGE') then
         specno = 11
         pagsp = offspec( %VAL(specno) ) 	
         if ( pagsp  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach Alpha-Ge spectrum area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 512
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = alfge(i,j)
            end do
         end do

         if ( detshared( %VAL(pagsp) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach Alpha-Ge spectra failed'
         end if

C --  Spectrum MAT
      else if ( specna .EQ. 'MAT') then
         specno = 12
         pmtsp = offspec( %VAL(specno) ) 	
         if ( pmtsp  .EQ. 0) then
            write(*,*)'*** ERROR ***  Attach General Purpose Matrix area failed !'
            stop
         end if

         TYPE = 2	! 2-D spectrum
         xdim = 2048
         ydim = 64
         do j = 0,ydim-1
            do i = 0,xdim-1
               MAT(i,j) = xmat(i,j)
            end do
         end do

         if ( detshared( %VAL(pmtsp) ) .EQ. -1) then
            write (*,*) '**** ERROR **** Detach  General Purpose Matrix spectra failed'
         end if


      else
         write (*,*) ' *** ERROR *** Unknown spectrum name '	
         TYPE = -1
      end if  

      if(TYPE.EQ.2)then
        ITYPE=3
        I1=1
      endif
      if(TYPE.EQ.1)then        
        ITYPE=1
        I1=2
      endif
      if((TYPE.NE.-1).AND.(fname(I1,1).NE.specna))then
        fname(I1,1)=specna
      endif


      end  	! End-Of-Subroutine offline_spectra
