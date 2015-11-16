      program offline

C     //////////////////////////////////////////////////////////////////
C     / PROGRAM    : offline                                           /
C     / FILE       : /user/schiller/osloware/offline/src/offline_main.f/
C     / WRITTEN BY : Tore Ramsoy                                       /
C     / DATE       : November 1994                                     / 
C     / MODIFIED BY: Andreas Schiller for opal.nscl.msu.edu, Sep 2003  /
C     / OS         : Solaris 2.1                                       /
C     / COMPILER   : SPARCompiler FORTRAN 2.0.1                        /
C ---------------------------------------------------------------------/
C     / PURPOSE    : Off-line sorting of data.                         /       
C     /                                                                / 
C     /                                                                /
C     / MESSAGE_BOX:                                                   /
C     /              ------------------------                          /
C     /         (0)  I       SO_FLAG        I                          /
C     /              ------------------------                          /
C     /         (1)  I        Time          I                          /
C     /              ------------------------                          /
C     /         (2)  I        exades        I                          /
C     /              ------------------------                          /
C     /         (3)  I   Tape Record Type   I                          /
C     /              ------------------------                          /
C     /         (4)  I    offline status    I                          /
C     /              ------------------------ 			       /
C     /         (5)  I    Records sorted    I                          /
C     /              ------------------------ 			       /
C     /         (6)  I     Bad records      I                          /
C     /              ------------------------                          /
C     /         (7)  I       Files2do       I                          /
C     /              ------------------------                          /
C     /         (8)  I       Recs2do        I                          /
C     /              ------------------------                          /
C     /         (9)  I     Returnstatus     I                          /
C     /              ------------------------                          /
C     /         (10) I                      I                          /
C     /              ------------------------                          /
C     /         (11) I    No Of Telescopes  I                          /
C     /              ------------------------                          /
C     /         (12) I                      I                          /
C     /              ------------------------                          /
C     /         (13) I                      I                          /
C     /              ------------------------                          /
C     /         (14) I                      I                          /
C     /              ------------------------ 			       /
C     /         (15) I                      I                          /
C     /              ------------------------ 			       /
C     /         (16) I                      I                          /
C     /              ------------------------                          /
C     /         (17) I                      I                          /
C     /              ------------------------                          /
C     /         (18) I                      I                          /
C     /              ------------------------                          /
C     /         (19) I                      I                          /
C     /              ------------------------                          /

C     /                                                                /
C     / INPUT:                                                         /
C     / message_box(0) : SO_FLAG            = 9  Stop Sorting          /
C     / message_box(1) : Time		Sorting started at "time"      /   
C     / message_box(2) : exades		file descriptor for exabyte    /   
C     / message_box(3) : Tape Rec.Type	SIRIUS=0, DAISY=1              /   
C     / message_box(7) : Files2do       0 = don't care                 /
C     / message_box(8) : Recs2do        0 = don't care                 /
C     /                                                                / 
C     / OUTPUT:                                                        /
C     / message_box(5) : Records sorted                                /
C     / message_box(6) : Bad Records                                   /
C     / message_box(7) : Fraction sorted                               /
C     / message_box(9) : Returnstatus     = 0  Running ...             /
C     /                                   = 1  Stopped by user         /  
C     /                                   = 2  File count reached      /
C     /                                   = 3  EOM encountered in read /
C     /					  = 4  Record count reached    /
C     /					  = 5  End of Data  reached    /
C     /                                   = 9  Error in read           /
C     /                                                                /
C     //////////////////////////////////////////////////////////////////

      IMPLICIT NONE
      INCLUDE 'common_defs.decl'
      INCLUDE 'spec_params.decl'
      INCLUDE 'spec_pointers.decl'

C --  Define C-routines to be called from fortran. This will save us from the
C --  f&#!@ underscore ....
C --  All C-functions are called by VALUE !
C
C 
C      ------------------------------------------------------------------------
C 		Glendower: 	I can call spirits from the vasty deep.
C 	
C		Hotspur: 	Why, so can I, or so can many man;
C				But will they come when you call for them?
C
C					Henry IV
C
C      ------------------------------------------------------------------------

   
C --  Attach shared memory
      integer attshared
      external attshared !$pragma C( attshared )	

C --  Detach shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 

C --  Attach spectrum area shared memory
      integer attspec
      external attspec !$pragma C( attspec )

C --  Read data from Exabyte tape, take care of filemarks and EOM (SIRIUS)
      integer read_exa
      external read_exa !$pragma C( read_exa )

C --  Read data from Exabyte tape, take care of filemarks and EOM (DAISY)
      integer read_daisy
      external read_daisy !$pragma C( read_daisy )

C --  User the Sun fortran POINTER extension to get access to shared memory
C --  The fortran pointer is for address reference only, no other operations 
C --  can be performed. The pointer is of integer type.

      integer rawdata(0:32767)		
      pointer (bufp, rawdata)		! Get access to shared databuffer segment
	
      integer message_box(0:19)
      pointer (messp, message_box)	! Get access to shared message box segment


      integer specno
      integer attmode   
      integer errstat
      logical running
      integer i,j,k,l,o,n
      integer file,rfile, lfile
      integer bad_buffer
      integer errcount
      integer files2do, recs2do
      integer fileno, recordno
      integer readstatus

      integer  telescopeno


      integer ia, dim
      real fspec(0:4095),a(3),efil,dumout

      character rangefile*80, rangename*30
      character gainfile*80,  gainname*30
      character lockfile*80,  lockname*30
      character offdir*80
      integer lastno, lnblnk



      data offdir/'/user/schiller/osloware/offline/'/	! Default directory for offline
      data rangename/'data/RANGE.DATA'/
      data gainname/'data/gainshift.tmp'/
      data lockname/'system/offline.lock'/

      data file/40/
      data rfile/41/
      data lfile/99/
              
      running = .TRUE.


C --  Check if the offline sort lock-file is present, create as SCRATCH if not 
C --  This is a security precaution, multiple processes may cause system crash
      lastno = lnblnk( offdir)
      lockfile = offdir(1:lastno)//lockname
      open(lfile, FILE=lockfile,STATUS='NEW', ERR=823)
      close( lfile )					! OK, No lock file present
      open(lfile, FILE=lockfile, STATUS='SCRATCH')
      goto 42
 823       write (*,*) ' **** ERROR **** Cannot start offline_sort, Lock file present'
      stop
 42   continue
   

C --  Attach shared memory databuffer segment
      attmode = 1
      bufp = attshared( %VAL(attmode) ) 	
      if ( bufp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Att. memory to sorting task'
         stop
      end if


C --  Attach shared memory message box segment
      attmode = 2
      messp = attshared( %VAL(attmode) )	
      if ( messp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Att. mess box to sorting task'
         stop
      end if


C --  ---------------------------------------------------------------------
C --  Attach the spectrum areas in shared memory. 
C --  Maximum number of segments : 50 
C --  Maximum segment size       : 8388608 bytes ( 8 MB)


C --  Attach  Singles spectra
      specno = 1
      psingles = attspec( %VAL(specno) ) 	
      if ( psingles  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach Singles spectra failed !'
         stop
      end if

C --  Attach  E-spectra
      specno = 2
      pesp = attspec( %VAL(specno) ) 	
      if ( pesp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach E-spectra failed !'
         stop
      end if


C --  Attach  E-DE-spectra
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
         write (*,*) '**** ERROR ****  Attach Thickness-spectra failed !'
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


C --  Initialize variables
      telescopeno 	= message_box(11)		! Number of telescopes used
      bad_buffer = 0
      message_box(9) = 0				! Return status "Running" 
      errcount = 0
      files2do = message_box(7)
      recs2do  = message_box(8)
      fileno = 0
      recordno = 0


C --  Initialization of gain & shift variables (written by offline_gui)
      do i = 0, telescopeno-1
         gaine(i)   = 1.0  
         gainde(i)  = 1.0
         shifte(i)  = 0.0
         shiftde(i) = 0.0
      end do
                            
      do i = 0, 5 
         gainge(i)  = 1.0
         shiftge(i) = 0.0
         shifttge(i)= 0.0
      end do

      do i = 0, 31
         gainna(i)  = 1.0
         shiftna(i) = 0.0
         shifttna(i)= 0.0
      end do

C --  Read gain and shift values from file /user/schiller/osloware/offline/data/gainshift.tmp
      gainfile = offdir(1:lastno)//gainname
      open(file, FILE=gainfile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=888)
      goto 2
 888       write (*,800)
 2    continue

         read (file,*) (gaine(i)  ,i = 0,telescopeno-1)
         read (file,*) (gainde(j) ,j = 0,telescopeno-1)
         read (file,*) (gainge(k) ,k = 0,5)
         read (file,*) (gainna(l) ,l = 0,31)

         read (file,*) (shifte(i)  ,i = 0,telescopeno-1)
         read (file,*) (shiftde(j) ,j = 0,telescopeno-1)
         read (file,*) (shiftge(k) ,k = 0,5)
         read (file,*) (shiftna(l) ,l = 0,31)
         read (file,*) (shifttge(o),o = 0,5)
         read (file,*) (shifttna(n),n = 0,31)

      close(file)

C --  Read range curve from file /user/schiller/osloware/offline/data/RANGE.DATA
      rangefile = offdir(1:lastno)//rangename
      open(rfile, FILE=rangefile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=777)
      goto 3
 777       write (*,700)
 3    continue
      efil = 0.
      ia   = 0
      a(1) = 0.
      a(2) = 1.
      a(3) = 0.
      call norr1dim(rfile,dumout,efil,fspec,dim,ia,a)

      close(rfile)
      
      do i = 0, 2047
         range(i) = fspec(i)                       ! Load RANGE
      end do


C     *****************************************************************
C --  Main sorting loop
C 
      do while (running)         



C --    ------------------------------------------------------------------
C       Check for STOP command from offline_gui
        if ( message_box(0) .EQ. 9) then
           message_box(9) = 1 	! Return status "Stopped by user"
           goto 999
        end if
C --    ------------------------------------------------------------------

C --    Read data buffer from Exabyte tape, call C-routine read_exa (SIRIUS) or read_daisy (DAISY)
	if      ( message_box(3) .eq. 0 ) then
           readstatus = read_exa  ( %VAL(bufp), %VAL( messp ) )
        else if ( message_box(3) .eq. 1 ) then
           readstatus = read_daisy( %VAL(bufp), %VAL( messp ) )
	end if


C --    Handle exception in read
	if ( readstatus .ne. 0 ) then 				! Exception deteced
	   if ( readstatus .eq. 2 ) fileno = fileno + 1		! EOF      	
	   if ( (files2do - fileno) .eq . 0) then		! Filecount reached
	      message_box(9) = 2				
              goto 999
           end if
           if ( readstatus .eq. 3 ) then			! EOM
	      message_box(9) = 3
	      goto 999
           end if
	   if ( readstatus .eq. 9 ) then			! ERROR
	      message_box(9) = 9
              errcount = errcount + 1
              if ( errcount .ge. 100) then			! Give up after 100 consequtive read errors
	         message_box(9) = 5 				! End-Of-Recorded-Data reached
                 goto 999			           
	      end if						! ... else try again
	   end if


	else							! OK, Data ready in memory
	   errcount = 0
	   recordno = recordno + 1				! Increment rec. count	
           if ( (recs2do - recordno) .eq. 0 ) then		! Recordcount reached
	      message_box(9) = 4
	      goto 999
           end if

C --       Start processing
           call format_event ( rawdata, 
     +                      message_box, 
     +                      singles,
     +                      esp,
     +                      desp,
     +                      edesp,
     +                      thicksp,
     +                      gesp,
     +                      tgesp,
     +                      nasp,
     +                      tnasp,
     +                      alfna,
     +                      alfge,
     +                      mat,
     +                      errstat )   


C --	   Update Message_Box
	   message_box(5) = recordno
           if ( errstat .eq. -1 ) then
              bad_buffer = bad_buffer + 1
	      message_box(6) = bad_buffer
	   end if

	end if
	

      end do     ! End Of Sort loop  
                            


C     *****************************************************************


  

 999  continue ! Clean up before exit
      message_box(4) = 0	! Stopped
      message_box(5) = recordno

C --  Message to the gui task that the sorting has terminated
C --  How to do this with Motif - Inte vet jag ...
C --  Disable possible confirmation dialog when removing files
    

C ****
C ---  Detachment of shared memory segments removed
C ---  Not necesary since task terminates and detaches automatic ?
      goto 656
C ****

C --  Detach shared memory before exit
      if ( detshared( %VAL(messp) ) .EQ. -1) then
         write (*,*) '**** ERROR **** Detach Shared Databuffer Failed'
      end if
      if ( detshared( %VAL(bufp) ) .EQ. -1) then
         write (*,*) '**** ERROR **** Detach Shared Message_Box Failed'
      end if


C --  ---------------------------------------------------------------------
C --  Detach spectra in shared memory before exit
      if ( detshared( %VAL(psingles) ) .EQ. -1) then
         write (*,*) '**** ERROR **** Detach Singles spectra failed'
      end if
      if ( detshared( %VAL(pesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach E-spectra failed'
      end if
      if ( detshared( %VAL(pdesp) ) .EQ. -1) then
         write (*,*) '**** ERROR ****  Detach DE-spectra failed'
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

C --  Close and remove lock-file
      close( lfile )    

	
      stop


 800  format(/,' ? *** ERROR : Cannot open GAINSHIFT.DATA',
     +       /,' Execute STOP command ! ')
 700  format(/,' ? *** ERROR : Cannot open RANGE.DATA',/,
     +         ' Execute STOP command ! ') 

      end          ! End-Of-Main sorter

