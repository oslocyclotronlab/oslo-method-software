C     =========================================================%
      program  specclear
C     =========================================================%
C     PURPOSE : Clear shared spectrum area                     %
C     =========================================================%
      IMPLICIT NONE
      INCLUDE 'spec_params.decl'
      INCLUDE 'spec_pointers.decl'


      integer i,j, lfile
      integer lastno, lnblnk
      integer specno
      integer attmode

      integer telescopeno

C --  Attach shared memory
      integer attshared
      external attshared !$pragma C( attshared )	

C --  Attach spectrum area shared memory
      integer attspec
      external attspec !$pragma C( attspec )	

C --  Detach spectrum area shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 
	
      integer message_box(0:19)
      pointer (messp, message_box)		! Get access to shared message box segment

      character lockfile*80
      character lockfile_name*30
      character default_path*30
      character offdir*80

      data offdir/'/user/schiller/osloware/offline/'/		! Default directory for offline
      data lockfile_name/'system/specclear.lock'/
      data default_path/'/user/schiller/osloware/offline'/	! Default home directory for acquistion

      data lfile/97/


C --  Check if the specclear lock-file is present, create as SCRATCH if not 
C --  This is a security precaution, multiple processes may cause system crash
      lastno = lnblnk( offdir)
      lockfile = offdir(1:lastno)//lockfile_name
      open(lfile, FILE=lockfile,STATUS='NEW', ERR=823)
      close( lfile )					! OK, No lock file present
      open(lfile, FILE=lockfile,STATUS='SCRATCH')
      goto 42
 823       write (*,*) ' **** ERROR **** Cannot start offline_specclear, Lock file present'
      stop
 42   continue


C --  Attach shared memory message box segment
      attmode = 2
      messp = attshared( %VAL(attmode) )	
      if ( messp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Att. mess box to sorting task'
         stop
      end if


      telescopeno 	= message_box(11)		! Number of telescopes used


C --  ---------------------------------------------------------------------
C     Attach the spectrum areas in shared memory. 
C --  ---------------------------------------------------------------------

C     Maximum number of segments for process: 50 
C     Maximum segment size       : 8 MB 
C     Set in the file /etc/system :
C					set shmsys:shminfo_shmmax=8388608
C 					set shmsys:shminfo_shmseg=50
C					set shmsys:shminfo_shmmni=100


C --  Attach singles spectrum area
      specno = 1
      psingles = attspec( %VAL(specno) ) 	
      if ( psingles  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach singles spectrum area failed !'
         stop
      end if


C --  Attach  E-spectra
      specno = 2
      pesp = attspec( %VAL(specno) ) 	
      if ( pesp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach  E-spectra failed !'
         stop
      end if

C --  Attach  DE-spectra
      specno = 3
      pdesp = attspec( %VAL(specno) ) 	
      if ( pdesp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach DE-spectra failed !'
         stop
      end if

C --  Attach  E-DE-spectra
      specno = 4
      pedesp = attspec( %VAL(specno) ) 	
      if ( pedesp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach E-DE-spectra failed !'
         stop
      end if

C --  Attach  Thickness-spectra
      specno = 5
      pthicksp = attspec( %VAL(specno) ) 	
      if ( pthicksp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach thickness spectra failed !'
         stop
      end if

C --  Attach  Ge-spectra
      specno = 6
      pgesp = attspec( %VAL(specno) ) 	
      if ( pgesp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach Ge-spectra failed !'
         stop
      end if

C --  Attach  Ge-T-spectra
      specno = 7
      ptgesp = attspec( %VAL(specno) ) 	
      if ( ptgesp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach Ge-T-spectra failed !'
         stop
      end if

C --  Attach  NaI spectra
      specno = 8
      pnasp = attspec( %VAL(specno) ) 	
      if ( pnasp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach NaI spectra failed !'
         stop
      end if

C --  Attach  NaI-T spectra
      specno = 9
      ptnasp = attspec( %VAL(specno) ) 	
      if ( ptnasp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach NaI-T spectra failed !'
         stop
      end if

C --  Attach  Alpha-NaI spectra
      specno = 10
      pansp = attspec( %VAL(specno) ) 	
      if ( pansp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach Alpha-NaI spectra failed !'
         stop
      end if

C --  Attach  Alpha-Ge spectra
      specno = 11
      pagsp = attspec( %VAL(specno) ) 	
      if ( pagsp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach Alpha-Ge spectra failed !'
         stop
      end if

C --  Attach  GP matrix spectra
      specno = 12
      pmtsp = attspec( %VAL(specno) ) 	
      if ( pmtsp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach General Purpose Matrix failed !'
         stop
      end if


C --  ---------------------------------------------------------------------
C --  Clear the spectrum areas in shared memory
C --  ---------------------------------------------------------------------

C --  Clear all particle spectra
      do j = 0,telescopeno-1
         do i = 0,2047
            esp(i,j)     = 0
            desp(i,j)    = 0
            edesp(i,j)   = 0
            thicksp(i,j) = 0
         end do
      end do

C --  Clear Ge-spectrum
      do j = 0,5
         do i = 0,4095
            gesp(i,j) = 0
         end do
      end do

C --  Clear Ge-T-spectrum
      do j = 0,5
         do i = 0,511
            tgesp(i,j) = 0
         end do
      end do

C --  Clear NaI-spectrum
      do j = 0,31
         do i = 0,2047
            nasp(i,j) = 0
         end do
      end do

C --  Clear NaI-T-spectrum
      do j = 0,31
         do i = 0,511
            tnasp(i,j) = 0
         end do
      end do

C --  Clear singles spectrum area
      do j = 0,9
         do i = 0, 4095
            singles(i,j) = 0
         end do
      end do

C --  Clear Alpha-NaI-spectrum
      do j = 0,511
         do i = 0,2047
            alfna(i,j) = 0
         end do
      end do

C --  Clear Alpha-Ge-spectrum
      do j = 0,511
         do i = 0,2047
            alfge(i,j) = 0
         end do
      end do

C --  Clear GP matrix
      do j = 0,63
         do i = 0,2047
            mat(i,j) = 0
         end do
      end do

 
      goto 656

C --  ---------------------------------------------------------------------
C --  Detach spectra in shared memory before exit
C --  ---------------------------------------------------------------------

      if ( detshared( %VAL(psingles) ) .EQ. -1) then
         write (*,*) '**** ERROR **** Detach Singles spectra failed'
      end if
      if ( detshared( %VAL(pesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach E spectra failed'
      end if
      if ( detshared( %VAL(pdesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach DE spectra failed'
      end if
      if ( detshared( %VAL(pedesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach E-DE spectra failed'
      end if
      if ( detshared( %VAL(pthicksp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach Thick spectra failed'
      end if
      if ( detshared( %VAL(pgesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach Ge spectra failed'
      end if
      if ( detshared( %VAL(ptgesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach Ge-T spectra failed'
      end if
      if ( detshared( %VAL(pnasp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach NaI spectra failed'
      end if
      if ( detshared( %VAL(ptnasp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach NaI-T spectra failed'
      end if
      if ( detshared( %VAL(pansp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach Alpha-NaI spectra failed'
      end if
      if ( detshared( %VAL(pagsp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach Alpha-Ge spectra failed'
      end if
      if ( detshared( %VAL(pmtsp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach General Purpose Matrix failed'
      end if

C --  ---------------------------------------------------------------------

 656  Continue

C --  Remove lock-file
      close ( lfile )

      
      end          ! End-Of-Subroutine zerospectrum
