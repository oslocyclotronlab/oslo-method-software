      program reduc

C     //////////////////////////////////////////////////////////////////
C     / PROGRAM    : reduc                                             /
C     / FILE       : /user/schiller/osloware/reduc/src/reduc_main.f    /
C     / WRITTEN BY : Tore Ramsoy, modified December 1997/magne         /
C     / DATE       : Januar 1995, 1997                                 / 
C     / MODIFIED BY: Andreas Schiller for opal.nscl.msu.edu, Sep 2003  /
C     /              made more standard repetitive formats             /
C     / OS         : Solaris 2.1, 2.5                                  /
C     / COMPILER   : SPARCompiler FORTRAN 2.0.1, 3.1                   /
C ---------------------------------------------------------------------/
C     / PURPOSE    : The program runs through the tape twice.          /
C     /              Pass 1 calculates the centroid of the elastic     /
C     /              scattering peak in the E-counter and adjust the   /
C     /              gain towards a "target" value.                    /
C     /              Pass 2 makes a datareduction putting windows in   /
C     /              the thickness spectrum. There is one fixed window /
C     /              for p,d,t and one variable for the alpha-particles/
C     /              The accepted events are multiplied by the adjusted/
C     /              gain and written to a target device.              / 
C     /                                                                /
C     / MESSAGE_BOX:                                                   /
C     /              ------------------------                          /
C     /         (0)  I       RED_FLAG       I                          /
C     /              ------------------------                          /
C     /         (1)  I        Time          I                          /
C     /              ------------------------                          /
C     /         (2)  I      in-exades       I     <- stor1p            /
C     /              ------------------------                          /
C     /         (3)  I   Tape Record Type   I                          /
C     /              ------------------------                          /
C     /         (4)  I     reduc status     I                          /
C     /              ------------------------                          /
C     /         (5)  I    Buffers sorted    I                          /
C     /              ------------------------                          /
C     /         (6)  I     Bad records      I                          /
C     /              ------------------------                          /
C     /         (7)  I       Files2do       I                          /
C     /              ------------------------                          /
C     /         (8)  I        Recs2do       I                          /
C     /              ------------------------                          /
C     /         (9)  I     Returnstatus     I                          /
C     /              ------------------------                          /
C     /         (10) I    gainadj Factor    I      <- gainfp           /
C     /              ------------------------                          /
C     /         (11) I    No Of Telescopes  I      <- telep            /
C     /              ------------------------                          /
C     /         (12) I      out-exades      I      <- stor2p           /
C     /              ------------------------                          /
C     /         (13) I                      I                          /
C     /              ------------------------                          /
C     /         (14) I  % of events accep.  I                          /
C     /              ------------------------                          /
C     /         (15) I   Records extracted  I                          /
C     /              ------------------------                          /
C     /         (16) I    Max gainpointer   I                          /
C     /              ------------------------                          /
C     /         (17) I       Startfile      I                          /
C     /              ------------------------                          /
C     /         (18) I       Startrec       I                          /
C     /              ------------------------                          /
C     /         (19) I        Pass #        I                          /
C     /              ------------------------                          /
C     /                                                                /
C     /                                                                /
C     / INPUT:                                                         /
C     / message_box(0) : SO_FLAG         = 9  Stop Sorting             /
C     / message_box(1) : Time		     Sorting started at "time"      /   
C     / message_box(2) : exades		file descriptor for exabyte   /   
C     / message_box(3) : Tape Rec.Type	SIRIUS=0, DAISY=1             /   
C     / message_box(7) : Files2do        0 = don't care                /
C     / message_box(8) : Recs2do         0 = don't care                /
C     / message_box(19): Pass # 	      1 = Gainext, 2 = Data red.    /
C     /                                                                / 
C     / OUTPUT:                                                        /
C     / message_box(5) : Records sorted                                /
C     / message_box(6) : Bad Records                                   /
C     / message_box(7) : Fraction sorted                               /
C     / message_box(9) : Returnstatus     = 0  Running                 /
C     /                                   = 1  Stopped by user         /  
C     /                                   = 2  File count reached      /
C     /                                   = 3  EOM encountered in read /
C     /					       = 4  Record count reached    /
C     /					       = 5  End of Data  reached    /
C     /					       = 6  Repositioning tape      /
C     /                                   = 8  Error in read           /
C     /                                   = 9  Error in write          /
C     /					       = 11 Gainmatrix full         /
C     /                                                                /
C     //////////////////////////////////////////////////////////////////



      IMPLICIT NONE
      INCLUDE 'common_defs.decl'
      INCLUDE 'spec_params.decl'

C --  Define C-routines to be called from fortran. This will save us from the
C --  f&#!@ underscore ....
C --  All C-functions are called by VALUE !

C --  Attach shared memory
      integer attshared
      external attshared !$pragma C( attshared )	

C --  Detach shared memory
      integer detshared
      external detshared !$pragma C( detshared ) 

C --  Read data from Exabyte tape, take care of filemarks and EOM (SIRIUS)
      integer read_exa
      external read_exa !$pragma C( read_exa )

C --  Read data from Exabyte tape, take care of filemarks and EOM (DAISY)
      integer read_daisy
      external read_daisy !$pragma C( read_daisy )

C --  Write data to Exabyte tape (SIRIUS format)
      integer write_exa
      external write_exa !$pragma C( write_exa )

C --  User the Sun fortran POINTER extension to get access to shared memory
C --  The fortran pointer is for address reference only, no other operations 
C --  can be performed. The pointer is of integer type.

      integer rawdata(0:32767)		
      pointer (bufp, rawdata)		! Get access to shared databuffer segment
	
      integer message_box(0:19)
      pointer (messp, message_box)	! Get access to shared message box segment

      INTEGER outp, MALLOC, idum
      CHARACTER dummy*1

      integer attmode
      integer errstat, writestat
      logical running
      integer i,j,k,l,o,n
      integer bad_buffer
      integer errcount
      integer files2do, recs2do
      integer fileno, recordno
      integer readstatus
 
      integer passno                ! Pass no 1 = gain ext, pass no 2 = Data reduction
      integer telescopeno           ! Number of telescopes used (8 or 64)
      real goalp(0:63)              ! The goal peak values, read from file
      real adfac(0:63)              ! Gain adjustment factors, returned from gaincalc
      real centroid(0:63)
      real gain_matrix(0:63,0:70000)! The extracted gain values, extended to 64 telescopes
      real final_egain(0:63)        ! Gain values to be applied to accepted events
      integer maxgain               ! Max number of gainsets, Current value 70000	
      integer AdjustGain            ! Adjust gain for every n.th record 
      integer gain_pointer
      integer max_gain_pointer	
      integer file, gfile, lfile, rfile
      integer lastno, lnblnk

      integer ia, dim
      real fspec(0:4095),a(3),efil,dumout  ! RANGE file parameters

      character lockfile*80
      character lockfile_name*30
      character rangefile*80, rangename*30
      character gainfile*80,  gainname*30
      character peakfile*80,  peakname*30
      character winfile*80,   winname*30
      character extgainfile*80, extgain*30
      character resultfile*80, result*30  
      character reddir*80

C Three more variables added, A.S.
      CHARACTER*11 F1STR,F2STR
      CHARACTER*15 F3STR

C --  Window (gate values)
      integer alfa_low,alfa_high,h_low,h_high 
      common /window/alfa_low(0:63),alfa_high,h_low,h_high 

      data reddir/'/user/schiller/osloware/reduc/'/	! Default directory for reduc
      data lockfile_name/'system/reduc.lock'/
      data rangename/'data/RANGE.DATA'/
      data gainname/'data/gainshift.tmp'/
      data extgain/'data/extracted.gain'/
      data result/'data/result.out'/
      data peakname/'data/peak.tmp'/
      data winname/'data/win.tmp'/

      data file/40/
      data rfile/41/
      data gfile/66/
      data lfile/67/  


C --  Check if the reduc lock-file is present, create as SCRATCH if not 
C --  This is a security precaution, multiple processes may cause system crash
      lastno = lnblnk( reddir)
      lockfile = reddir(1:lastno)//lockfile_name
      open(lfile, FILE=lockfile,STATUS='NEW', ERR=823)
      close( lfile )					! OK, No lock file present
      open(lfile, FILE=lockfile,STATUS='SCRATCH')
      goto 42
 823  write (*,*) ' **** ERROR **** Cannot start REDUC, Lock file present'
      stop
 42   continue

C --  Attach shared memory databuffer segment
      attmode = 1
      bufp = attshared( %VAL(attmode) ) 	
      if ( bufp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach memory to sorting task'
         stop
      end if

C --  Attach shared memory message box segment
      attmode = 2
      messp = attshared( %VAL(attmode) )	
      if ( messp  .EQ. 0) then
         write (*,*) '**** ERROR ****  Attach message box to sorting task'
         stop
      end if

C --  Initialize variables
      files2do          = message_box(7)
      recs2do           = message_box(8)
      AdjustGain        = message_box(10) ! Adjust gain for every Adjustgain.th record
      telescopeno       = message_box(11) ! Number of telescopes used
      max_gain_pointer  = message_box(16) ! Max valid entry in gainset
      passno            = message_box(19) ! Pass no (1=Gain extraction, 2=data reduction)
      message_box(9)    = 0               ! Return status "Running" 
      message_box(15)   = 0 			 ! Record extracted
      message_box(5)    = 0               ! Sorted
      message_box(6)    = 0               ! Bad recs
      message_box(14)   = 0               ! % events accepted
      fileno            = 0
      recordno          = 0
      gain_pointer      = 0
      bad_buffer        = 0
      errcount          = 0
      running           = .TRUE.
      maxgain           = 70000           ! Max no. gain sets
      
C --  Reset temp result file
      lastno = lnblnk( reddir)
      resultfile = reddir(1:lastno)//result
      open(file, FILE=resultfile, STATUS='UNKNOWN', ACCESS='SEQUENTIAL')
      write(file,*) ' '
      close(file)

C --  Read gain and shift start values from file /user/schiller/osloware/reduc/data/gainshift.tmp
      lastno = lnblnk( reddir)
      gainfile = reddir(1:lastno)//gainname
      open(file, FILE=gainfile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=888)
      goto 2
 888  write (*,800)
 2    continue

      read (file,*) (gaine(i)   ,i = 0,telescopeno-1)
      read (file,*) (gainde(j)  ,j = 0,telescopeno-1)
      read (file,*) (gainge(k)  ,k = 0,5)
      read (file,*) (gainna(l)  ,l = 0,31)
      read (file,*) (shifte(i)  ,i = 0,telescopeno-1)
      read (file,*) (shiftde(j) ,j = 0,telescopeno-1)
      read (file,*) (shiftge(k) ,k = 0,5)
      read (file,*) (shiftna(l) ,l = 0,31)
      read (file,*) (shifttge(o),o = 0,5)
      read (file,*) (shifttna(n),n = 0,31)

      close(file)
     


C ---------------------------------------------------------------------- 
      if ( passno .EQ. 1 ) then
C     S T A R T   G A I N   E X T R A C T I O N               
C ---------------------------------------------------------------------- 

C Some good advices to the user
      WRITE(*,*)' ---------------------------------------------------------'
      WRITE(*,*)' Congratulation, you are now extracting gain factors for'
      WRITE(*,*)' the E detector using the original data set (input tape).'
      WRITE(*,*)' After this, you have to POSITION the input tape at the'
      WRITE(*,*)' position where the gain extraction started. Then, you '
      WRITE(*,*)' choose DATA REDUCTION from the pull down menu, and you '
      WRITE(*,*)' choose the same number of files/records as previously.'
      WRITE(*,*)' The corrected E and dE values are now written on the '
      WRITE(*,*)' output tape and gain/shifts are fixed for these detectors.'
      WRITE(*,*)' You CAN NOT run GAIN EXTRACTION without choosing DATA '
      WRITE(*,*)' REDUCTION in between. (This is because the file with gain'
      WRITE(*,*)' values is reset each time you start GAIN EXTRACTION.)'
      WRITE(*,*)' If this was confusing, please contact:'
      WRITE(*,*)' schiller@nscl.msu.edu                          Good luck!'
      WRITE(*,*)' ---------------------------------------------------------'



C --  Read goal peak values from file /user/schiller/osloware/reduc/data/peaks.goal
      lastno = lnblnk( reddir)
      peakfile = reddir(1:lastno)//peakname
      open(file, FILE=peakfile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=822)
      goto 8
 822  write (*,800)
 8    continue
      do i = 0, telescopeno-1
         read ( file, *) goalp(i)
      end do
      close(file)

 800  format(/,' ? *** ERROR : Cannot open data file',/,' Execute STOP command ! ')

C --  Initialize gain matrix
      do j = 0, maxgain
         do i = 0, telescopeno-1
            gain_matrix(i,j) = 1.0
         end do
      end do 


      do while (running)         
C --    ------------------------------------------------------------------
C       Check for STOP command from offline_gui
        if ( message_box(0) .EQ. 9) then
           message_box(9) = 1 	! Return status "Stopped by user"
           goto 999
        end if
C --    ------------------------------------------------------------------

C --    Read data buffer from Exabyte tape, call C-routine read_exa (SIRIUS) or read_daisy (DAISY)
	if ( message_box(3) .eq. 0 ) then
           readstatus = read_exa  ( %VAL(bufp), %VAL( messp ) )
      else if ( message_box(3) .eq. 1 ) then
           readstatus = read_daisy( %VAL(bufp), %VAL( messp ) )
	end if


C --    Handle exception in read
	if ( readstatus .ne. 0 ) then                    ! Exception deteced
	   if ( readstatus .eq. 2 ) fileno = fileno + 1  ! EOF      	
	   if ( (files2do - fileno) .eq . 0) then        ! Filecount reached
	      message_box(9) = 2				
            goto 999
         end if
         if ( readstatus .eq. 3 ) then         ! EOM
	      message_box(9) = 3
	      goto 999
         end if
	   if ( readstatus .eq. 9 ) then         ! ERROR
	      message_box(9) = 9
            errcount = errcount + 1
            if ( errcount .ge. 100) then       ! Give up after 100 consequtive read errors
	       message_box(9) = 5                ! End-Of-Recorded-Data reached
             goto 999			           
	       end if                            ! ... else try again
	    end if

	else                                     ! OK, Data ready in memory

	   errcount = 0
	   recordno = recordno + 1               ! Increment rec. count	
				
 
             
C --     Start processing, find e-counter centroid
         call format_gain ( rawdata, 
     +                      esp,
     +                      errstat )

C --	   Update Message_Box
	   message_box(5) = recordno
         if ( errstat .eq. -1 ) then
            bad_buffer = bad_buffer + 1
	      message_box(6) = bad_buffer
	   end if

C --     Calculate E-counter gain for every nth record  
         if ( mod(recordno, AdjustGain) .EQ. 0) then
            call gain_calc ( 	goalp, adfac, telescopeno, esp, centroid )
            do i = 0, telescopeno-1
               gain_matrix(i,gain_pointer) = gaine(i) * adfac(i)
            end do

            open(file, FILE=resultfile, STATUS='UNKNOWN', ACCESS='SEQUENTIAL')
            write( file, 884) gain_pointer+1

C Changed the following four lines A.S.
            WRITE(F1STR,885)telescopeno
            WRITE(F2STR,887)telescopeno
            write( file, FMT=F1STR ) (centroid(j), j = 0,telescopeno-1)
            write( file, FMT=F2STR ) (gain_matrix(j,gain_pointer), j = 0,telescopeno-1)
            close( file )
 884        format('Resultset # ',I5)

C Changed the following two lines A.S.
 885        format('(',I5.5,'F8.1)')
 887        format('(',I5.5,'F8.4)')

            if (gain_pointer .GT. maxgain) then
	         message_box(9) = 11 			! Gain matrix full
               goto 999
            end if 

            gain_pointer = gain_pointer + 1 

C --        Clear E-spectrum before next fit
            do j = 0, telescopeno-1
               do i = 0,2047
                  esp(i,j) = 0
               end do
            end do

         end if 

         if ( (recs2do - recordno) .eq. 0 ) then       ! Recordcount reached
	      message_box(9) = 4
	      goto 999
         end if

         end if                  ! Exception test
   
      end do                     ! End Of Gain Extraction loop 



C --  Gain extraction session terminated, Write extracted gain values to file
 999  CONTINUE 
                                  
      lastno = lnblnk( reddir)
      extgainfile = reddir(1:lastno)//extgain
      open(gfile, FILE=extgainfile, STATUS='UNKNOWN', ACCESS='SEQUENTIAL', ERR=834)
      goto 7
 834  write (*,800)
 7    continue
      WRITE(gfile,*)gain_pointer-1
      WRITE(gfile,*)'GainNo Detec.(0) Detec.(1) Detec.(2) ...'

C added the following line A.S.
      WRITE(F3STR,886)telescopeno
      
      do i = 0, gain_pointer-1
C changed the following line A.S.
         write( gfile,FMT=F3STR ) i,(gain_matrix(j,i), j = 0,telescopeno-1)
      end do

C changed the following line A.S.
 886  format('(I6,',I5.5,'F10.5)')
      close( gfile )

      max_gain_pointer = gain_pointer - 1       ! Highest valid entry
      message_box(16)  = max_gain_pointer       ! Save in message box
      gain_pointer     = 0
      message_box(4)   = 0                      ! Stopped
      message_box(5)   = recordno

      goto 6666

      end if ! End of Pass1 - Gain extraction 
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





C ---------------------------------------------------------------------- 
      if ( passno .EQ. 2 ) then
C     S T A R T   D A T A   R E D U C T I O N               
C ---------------------------------------------------------------------- 


C Reads in the extracted gainmatrix values
      lastno = lnblnk( reddir)
      extgainfile = reddir(1:lastno)//extgain
      open(gfile, FILE=extgainfile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=835)
      goto 6
 835  write (*,800)
 6    continue
      READ(gfile,*)max_gain_pointer
      READ(gfile,*)dummy
      do i = 0, max_gain_pointer
         read( gfile,*,ERR=836) idum,(gain_matrix(j,i), j = 0,telescopeno-1)
      end do
      GO TO 9
 836  WRITE(6,*)'Problems of reading extgain file - please, push STOP-button or live dangeress'
 9    CONTINUE
      close( gfile )

      message_box(16)  = max_gain_pointer       ! Save in message box

      outp = MALLOC(131072) ! Allocate 128 kB for the buffer

C --  Read range curve from file /user/schiller/osloware/reduc/data/RANGE.DATA
      rangefile = reddir(1:lastno)//rangename
      open(rfile, FILE=rangefile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=777)
      goto 3
 777  write (*,700)
 3    continue
 700  format(/,' ? *** ERROR : Cannot open RANGE.DATA',/,' Execute STOP command ! ') 

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


C --  Read window (gate) values from file /user/schiller/osloware/reduc/data/peaks.goal
      lastno = lnblnk( reddir)
      winfile = reddir(1:lastno)//winname
      open(file, FILE=winfile, STATUS='OLD', ACCESS='SEQUENTIAL', ERR=847)
      goto 17
 847  write (*,800)
 17   continue
      read (file, *) (alfa_low(i) ,i = 0, telescopeno-1)
      read (file, *)  alfa_high
      read (file, *)  h_low
      read (file, *)  h_high
      close(file)

      do while (running)         
C --    ------------------------------------------------------------------
C       Check for STOP command from reduc_gui
        if ( message_box(0) .EQ. 9) then
           message_box(9) = 1 	! Return status "Stopped by user"
           goto 111
        end if
C --    ------------------------------------------------------------------

C --    Read data buffer from Exabyte tape, call C-routine read_exa (SIRIUS) or read_daisy (DAISY)
	if ( message_box(3) .eq. 0 ) then
         readstatus = read_exa  ( %VAL(bufp), %VAL( messp ) )
      else if ( message_box(3) .eq. 1 ) then
         readstatus = read_daisy( %VAL(bufp), %VAL( messp ) )
	end if

C --    Handle exception in read
	if ( readstatus .ne. 0 ) then                   ! Exception deteced
	   if ( readstatus .eq. 2 ) fileno = fileno + 1 ! EOF      	
	   if ( (files2do - fileno) .eq . 0) then       ! Filecount reached
	      message_box(9) = 2				
            goto 111
         end if
         if ( readstatus .eq. 3 ) then    ! EOM
	      message_box(9) = 3
	      goto 111
         end if
	   if ( readstatus .eq. 9 ) then    ! ERROR
	      message_box(9) = 9
            errcount = errcount + 1
            if ( errcount .ge. 100) then  ! Give up after 100 consequtive read errors
	         message_box(9) = 5         ! End-Of-Recorded-Data reached
               goto 111			           
	      end if                        ! ... else try again
	   end if

	else                                ! OK, Data ready in memory
	   errcount = 0
	   recordno = recordno + 1          ! Increment rec. count	

C --     Change gain factors for every n.th record, save in vector final_egain
         if ( MOD(recordno, AdjustGain) .EQ. 0) then
            do i = 0,  telescopeno - 1
               final_egain(i) = gain_matrix(i,gain_pointer)
            end do
            gain_pointer = gain_pointer + 1
C --        Check that entry is valid
            if (gain_pointer .GT. max_gain_pointer) then
               if (gain_pointer .GT. max_gain_pointer+1)THEN
                   WRITE(*,*)'Warning, gain_pointer > max_gain_pointer+1'
               endif
               gain_pointer = max_gain_pointer
            end if
         end if

C	   Sort databuffer, accept events within the window and gain adjust.
         call reduce_format ( rawdata,
     +                        outp,
     +                        messp,
     +                        final_egain,
     +                        writestat,
     +                        errstat )
			
C --	   Update Message_Box
	   message_box(5) = recordno
         if ( errstat .eq. -1 ) then
            bad_buffer = bad_buffer + 1
	      message_box(6) = bad_buffer
	   end if
         if (writestat .eq. -1) then
           message_box(9) = 9 ! Return status = write error
            goto 111                                      	
         end if 

         if ( (recs2do - recordno) .eq. 0 ) then  ! Recordcount reached
	      message_box(9) = 4
	      goto 111
         end if

        end if                ! Exception test
      end do                  ! End Of Gain Extraction loop 

C --  Data reduction session terminated
 111  CONTINUE
      message_box(4) = 0      ! Stopped
      goto 6666

      end if                  ! End of Pass2 - Data reduction


C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --  Terminate task, detach shared memory
 6666 continue
C --  Detach shared memory before exit
      if ( detshared( %VAL(messp) ) .EQ. -1) then
         write (*,*) '**** ERROR **** Detach Shared Databuffer Failed'
      end if
      if ( detshared( %VAL(bufp) ) .EQ. -1) then
         write (*,*) '**** ERROR **** Detach Shared Message_Box Failed'
      end if

C --  Remove temporar lockfile
      close( lfile )

      end                          ! End-Of-Main REDUC
