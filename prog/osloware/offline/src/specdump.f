      program specdump

      IMPLICIT NONE
      INCLUDE 'spec_params.decl'
      INCLUDE 'spec_pointers.decl'


      integer specno, lfile
      character spname*8
      integer lastno, lnblnk

      character comment*60
      character filena*100 
                 
      real mx(0:4095,0:511)
      integer file,xdim,ydim,i,j
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


      character specdir*80, specdir_env*30
      character offdir*80
      character default_path*30
      character lockfile*80
      character lockfile_name*30

      data offdir/'/user/schiller/osloware/offline/'/		! Default directory for offline
      data lockfile_name/'system/specdump.lock'/
      data specdir_env/'OFF_SPECDIR'/
      data default_path/'/user/schiller/osloware/offline'/	! Default home directory for offline

      data lfile/98/


C --  Check if the specdump lock-file is present, create as SCRATCH if not 
C --  This is a security precaution, multiple processes may cause system crash
      lastno = lnblnk( offdir)
      lockfile = offdir(1:lastno)//lockfile_name
      open(lfile, FILE=lockfile,STATUS='NEW', ERR=823)
      close( lfile )					! OK, No lock file present
      open(lfile, FILE=lockfile,STATUS='SCRATCH')
      goto 42
 823       write (*,*) ' **** ERROR **** Cannot start offline_specdump, Lock file present'
      stop
 42   continue


C --  Get directory where to save the spectrum files
      call getenv(specdir_env, specdir)
      lastno = lnblnk( specdir)
      if (lastno .eq. 0) then 		! Environment variable not set
         lastno = lnblnk( offdir )
         specdir = offdir(1:lastno)//'spectra/'
         lastno = lnblnk( specdir)
      endif   
     

C --  Attach shared memory message box segment
      attmode = 2
      messp = attshared( %VAL(attmode) )	
      if ( messp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Att. mess box to dump spectra task'
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
C --  Write the spectra to disk file
C --  ---------------------------------------------------------------------

C --  Write E-counter  matrix 
      spname = 'ESP'
C      filena = specdir(1:lastno)//spname    THE OLD VERSION
      filena =spname
      file = 10
      xdim = 2047
      ydim = telescopeno-1
      comment = 'E matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = esp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=701)
      goto 1 
 701  call fileopenerror(file,filena)
 1    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write DE-counter  matrix
      spname = 'DESP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 11
      xdim = 2047
      ydim = telescopeno-1
      comment = 'DE matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = desp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=702)
      goto  2
 702  call fileopenerror(file,filena)
 2    call norw2dim(file, comment, xdim, ydim, mx)
      close(file) 

C --  Write E-DE-spectrum matrix to file 
      spname = 'EDESP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 12
      xdim = 2047
      ydim = telescopeno-1
      comment = 'E-DE matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = edesp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=703) 
      goto 3  
 703  call fileopenerror(file,filena)
 3    call norw2dim(file, comment, xdim, ydim, mx)
      close(file) 

C --  Write thickness  matrix
      spname = 'THICKSP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 13
      xdim = 2047
      ydim = telescopeno-1
      comment = 'Thickness matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = thicksp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=704)
      goto 4
 704  call fileopenerror(file,filena)
 4    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write GE-spectrum matrix to file
      spname = 'GESP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 14
      xdim = 4095
      ydim = 5
      comment = 'Ge matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = gesp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=705) 
      goto 5
 705  call fileopenerror(file,filena)
 5    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write Ge-T spectrum matrix to file
      spname = 'TGESP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 15
      xdim = 511
      ydim = 5
      comment = 'Ge TDC matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = tgesp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=706)
      goto 6
 706  call fileopenerror(file,filena)
 6    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write NaI spectrum matrix to file
      spname = 'NASP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 16
      xdim = 2047
      ydim = 31
      comment = 'NaI matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = nasp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=707)
      goto 7
 707  call fileopenerror(file,filena)
 7    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write NaI-T spectrum matrix to file
      spname = 'TNASP'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 17
      xdim = 511
      ydim = 31
      comment = 'NaI-T matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = tnasp(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=808)
      goto 8
 808  call fileopenerror(file,filena)
 8    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)      

C --  Write Alpha-NaI spectrum matrix to file
      spname = 'ALFNA'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 18
      xdim = 2047
      ydim = 511
      comment = 'Alpha-NaI matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = alfna(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=809)
      goto 9
 809  call fileopenerror(file,filena)
 9    call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write Alpha-Ge spectrum matrix to file
      spname = 'ALFGE'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 19
      xdim = 2047
      ydim = 511
      comment = 'Alpha-Ge matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = alfge(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=810)
      goto 10
 810  call fileopenerror(file,filena)
 10   call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write General purpose matrix to file
      spname = 'MAT'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 20
      xdim = 2047
      ydim = 63
      comment = 'General Purpose matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = mat(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=811)
      goto 11
 811  call fileopenerror(file,filena)
 11   call norw2dim(file, comment, xdim, ydim, mx)
      close(file)

C --  Write singles spectra (in matrix) to file
      spname = 'SINGLES'
c      filena = specdir(1:lastno)//spname
      filena =spname
      file = 21
      xdim = 4095
      ydim = 9
      comment = 'Singles spectra matrix'
      do j = 0,ydim
         do i = 0,xdim
            mx(i,j) = singles(i,j)
         end do
      end do
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=812)
      goto 12
 812  call fileopenerror(file,filena)
 12   call norw2dim(file, comment, xdim, ydim, mx)
      close(file)


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


      end ! end-of-main specdump

C     =========================================================%
      subroutine fileopenerror(file,filena)
C     =========================================================%
C     PURPOSE : Makes retries if a file open error occurs when %
C               saving histograms to disc. After unsuccessfull %
C               reties an error message is printed on terminal %
C     INPUT  via parameter : file   : INTEGER                  %
C                          : filena : CHARACTER                %
C     OUTPUT via parameter : NONE                              %
C                                                              %
C     INPUT  via common    : NONE                              %
C     OUTPUT via common    : NONE                              %
C     =========================================================%
      IMPLICIT NONE
     
      character filena*40 
      integer file,retry

      retry = 0

C --  Try to open the file once more
 1    continue
      open(file,FILE=filena,ACCESS='SEQUENTIAL',ERR=888)
      goto 2

 888       retry = retry + 1
           if (retry .GE. 4) GOTO 9
           call sleep(2)
           goto 1


C --  Error message
 9    write(*,100) filena 

 100  format(/,' ? *** ERROR : Cannot open file :',A40,/,
     +       ' Opened by another user ?')

 2    continue
      return
      end          ! End-Of-Subroutine fileopenerror
