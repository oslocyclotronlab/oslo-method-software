C    ===========================================================
      SUBROUTINE reduce_format(  
     +                           rawdata,
     +                           outp,
     +                           messp,
     +                           final_egain,
     +                           writestat,
     +                           errstat )

C    ===========================================================
C     Purpose : Sort data into a matrix which is used by       C
C               the sorting application routine.               C
C               The event matrix is processed by the subr.     C
C               reduce_sort which returns whether the event    C
C               is to be stored or not.                        C
C                                                              C
C     INPUT  via parameter : rawdata : integer                 C
C     OUTPUT via parameter :                                   C
C                            errstat : integer=-1 buffer err   C
C                            writestat : integer=-1 write err  C
C                                                              C
C     OUTPUT via common    : COMMON /event/matrix(0:11,0:15)   C
C                                                              C
C     =========================================================C
      IMPLICIT NONE

      INCLUDE 'common_defs.decl'

      integer rawdata(0:32767)
      real final_egain(0:63) 		
      integer errstat
      integer writestat
      integer messp ,outp  
      integer writestatus
      real randval1, randval2, rand
      real evfrac

      integer OP			! "Pointer" used in local databuffer
      integer rawevent(0:1023)
      integer bufferlength 
      integer grec,gevents,events
      integer i,j,k,l,n
      integer evlen
      integer tel
      integer header_id,header_mask,temp,tpu
      integer xindex,yindex,xrow
      integer nextpointer,currentpointer,header,pattern
      logical good

C --  Window (gate values)
      integer alfa_low,alfa_high,h_low,h_high                                   
      common /window/alfa_low(0:63),alfa_high,h_low,h_high 

C --  Write to Exabyte tape
      integer write_exa
      external write_exa !$pragma C( write_exa )

C --  Get access to shared message box segment
      integer message_box(0:19)
      pointer (messp, message_box)	

C --  Local databuffer with accepted events
      integer OutBuf(0:32767)
      pointer (outp, OutBuf) ! Pointer to output buffer used by C-routine write_exa
      bufferlength = 32767

C     Bit masks
      header_id   = X'F000'                 ! Event header (Hex)
      header_mask = X'FF'        

C --  Initialize Variables
      gevents         = 0
      events          = 0
      errstat         = 0  
      writestat       = 0
      currentpointer  = 0
      nextpointer     = 0 
      good            = .FALSE.

C ======================================================================
      do i = 0,bufferlength                 ! Event loop

c         if(currentpointer.GT.300.AND.currentpointer.LT.315.AND.message_box(5).EQ.1)then ! Magnes test
c           WRITE(*,*)'word  rawdata reducdata'
c           do n=0,300
c              write(*,1111)n,rawdata(n),OutBuf(n)
c 1111         format(I4,Z8,Z8)
c           enddo
c         endif

         m(0,0) = 0                         ! Reset TPU id loc.
         m(3,0) = 0                         !         -"-
         m(6,0) = 0                         !         -"-
         m(9,0) = 0                         !         -"-

         currentpointer = nextpointer       ! Pointer to current
         header = rawdata(currentpointer)   ! Start-Of-Event

C --     No more data in buffer if header=0,return from subroutine
C --     If this should occur to early (partly filled buffer) it may
C --     be a serious problem. Mark such a buffer as bad buffer.
         if (header .EQ. 0 .AND. currentpointer .GT. 32500 ) goto 999

C --     Check header for start-of-event code (=Fxxx)
           temp = header .AND. header_id
           if (temp .ne. header_id) then         ! Error has occured
              errstat = -1
              goto 999                           ! Drop current buffer
           end if

C --     Save pointer to next event 
           evlen = header .AND. header_mask
           IF(evlen.GT.1024)THEN
              WRITE(*,*)'Warning, eventlength > 1024'
              errstat = -1
              GO TO 999
           ENDIF
           nextpointer = evlen + currentpointer


C --     Save the raw event
           do l= 0,evlen - 1
              rawevent(l) = rawdata(currentpointer + l)
           end do

           currentpointer = currentpointer + 1

C --  ------------------------------------------------------------------
      do while (currentpointer .LT. nextpointer) ! Sub-event loop

C --     Identify TPU
         tpu = ((rawdata(currentpointer)) .and. X'F')
         if      (tpu .EQ. X'A') then
            xindex = 0
         else if (tpu .EQ. X'B') then
            xindex = 3
         else if (tpu .EQ. X'C') then
            xindex = 6
         else if (tpu .EQ. X'D') then
            xindex = 9
         end if
         currentpointer = currentpointer + 1

         if (xindex .ne. 9) then                   ! No pattern for PUR
C --        Put the pattern into matrix
            pattern = rawdata(currentpointer)
            m(xindex,1) = pattern
            currentpointer = currentpointer + 1
         end if



C ---------------------------------------------------------------------
C       T P U 1 :
C 
C     Fill in data according to pattern.
C     For TPU1 only even-numbered pattern bits have two data words.
C     The odd-numbered are logical bits which may be used by the
C     sorting routine.

      if (xindex .eq. 0) then                      ! TPU1 is identified
         m(xindex,0) = 1

         do j = 0,14,2
             if (btest(pattern,j)) then
                 xrow=xindex+1
                 yindex=j
                 m(xrow,yindex) = rawdata(currentpointer)
                 currentpointer = currentpointer + 1
                 xrow=xindex+2
                 m(xrow,yindex) = rawdata(currentpointer)
                 currentpointer = currentpointer + 1
             end if
         end do

C ---------------------------------------------------------------------
C       T P U 2 :
C 
C     Fill in data according to pattern
C     For TPU all pattern bits have two data words, the first may
C     i.e. denote an energy while word two contains the time 

      else if (xindex .eq. 3) then                  ! TPU2 is identified
         m(xindex,0) = 1

         do l = 0,15
             if (btest(pattern,l)) then
                 xrow=xindex+1
                 yindex=l
                 m(xrow,yindex) = rawdata(currentpointer)
                 currentpointer = currentpointer + 1
                 xrow=xindex+2
                 m(xrow,yindex) = rawdata(currentpointer)
                 currentpointer = currentpointer + 1
             end if
         end do

C ---------------------------------------------------------------------
C       T P U 3 :
C 
C     Fill in data according to pattern
C     For TPU all pattern bits have two data words, the first may
C     i.e. denote an energy while word two contains the time 

      else if (xindex .EQ. 6) then                  ! TPU3 is identified
         m(xindex,0) = 1

         do n = 0,15
             if (btest(pattern,n)) then
                 xrow=xindex+1
                 yindex=n
                 m(xrow,yindex) = rawdata(currentpointer)
                 currentpointer = currentpointer + 1
                 xrow=xindex+2
                 m(xrow,yindex) = rawdata(currentpointer)
                 currentpointer = currentpointer + 1
             end if
         end do

C-----------------------------------------------------------------------
C       T P U 4 :
C
C     Virtual TPU
C     The Pile-Up pattern words are stored under "virtual TPU 4"
C     energy words 0 and 1

      else if (xindex .EQ. 9) then             ! PUR is identified

         m(xindex,0) = 1

         m(10,0)   = rawdata(currentpointer)   ! PUR(0)
         currentpointer = currentpointer + 1
         m(10,1)   = rawdata(currentpointer)   ! PUR(1)
         currentpointer = currentpointer + 1
      end if

C --  Fetch next subevent
      end do                                   ! End-Of-Subevent loop
C-----------------------------------------------------------------------

C --  Now, the event is formatted and ready to be sorted
      call reduce_sort( good,tel )             ! Gate on particles
 
               
C --  Subroutine "reduce_sort" return good = .TRUE. if
C --  the event has a particle within a window in the thickness
C --  spectrum.

      events = events + 1
      if (good) then                           ! Good event, save it
         gevents = gevents + 1                 ! No of good events
         evfrac = (gevents * 100) / events
         message_box(14) = INT(evfrac)         ! % events accepted

C --     Output Databuffer containing accepted events is full
         if ( OP .gt. bufferlength-evlen) then 
   
C --        Write databuffer to Target Device
            writestatus = write_exa( %VAL(outp), %VAL(messp) )

            if (writestatus .EQ. -1) then      ! Error in write, terminate session
               writestat = -1
               goto 999
            end if 
                        
            grec = grec + 1                    ! Increment "good" buffers
            OP = 0                             ! Reset local output buffer pointer
            DO n = 0, bufferlength             ! Zeroing local output buffer
               OutBuf(n) = 0
            ENDDO
         end if 

	   message_box(15) = grec                ! Number of accepted buffers


C --   Find a random number between -0.5 and +0.5
         randval1 = rand(0) - 0.5
         randval2 = rand(0) - 0.5


C  ***
C  ***   This code must be modified to accomodate 64 telescopes
C  ***
C --     Multiply E and DE in raw event by final gain. The event has one 
C --     and only one particle subevent. This subevent is the first in the event.
C --     Thus the DE-energy word will always be located in rawevent(3) and the
C --     E-energy word in rawevent(4). The rawevent energy words must be normalized
C --     to [0,2047] in advance to gain multiplication and restored to full length
C --     before saved. 
C --     1. Telescope group A, tel = [0,3]
         if (tel .GE. 0 .AND. tel .LE. 3) then
            rawevent(3) = ((rawevent(3) - (tel*2048) + randval1) * gainde(tel)) + shiftde(tel) +0.5 
            if ( rawevent(3) .GT. 2047) rawevent(3) = 0
            if ( rawevent(3) .LT.    0) rawevent(3) = 0

            rawevent(3) = rawevent(3) + (tel*2048)
            rawevent(4) = ((rawevent(4) - (tel*2048) + randval2) * final_egain(tel)) + shifte(tel) + 0.5
            if ( rawevent(4) .GT. 2047) rawevent(4) = 0
            if ( rawevent(4) .LT.    0) rawevent(4) = 0
            rawevent(4) = rawevent(4) + (tel*2048) 
         end if
C --     2. Telescope group B, tel = [4,7]
         if (tel .GE. 4 .AND. tel .LE. 7) then
            rawevent(3) = ((rawevent(3) - ((tel-4)*2048) + randval1) * gainde(tel)) + shiftde(tel) + 0.5 
            if ( rawevent(3) .GT. 2047) rawevent(3) = 0
            if ( rawevent(3) .LT.    0) rawevent(3) = 0
            rawevent(3) = rawevent(3) + ((tel-4)*2048)
            rawevent(4) = ((rawevent(4) - ((tel-4)*2048) + randval2) * final_egain(tel)) + shifte(tel) + 0.5
            if ( rawevent(4) .GT. 2047) rawevent(4) = 0
            if ( rawevent(4) .LT.    0) rawevent(4) = 0
            rawevent(4) = rawevent(4) + ((tel-4)*2048)
         end if                            
C  ***
C  ***   
C  ***

         if (rawevent(3).LT.0.OR.rawevent(3).GT.8192) rawevent(3) = 0
         if (rawevent(4).LT.0.OR.rawevent(4).GT.8192) rawevent(4) = 0

C --     Store E-counter gain-stabilized event in "good" Buffer
         do k = 0, evlen - 1
            OutBuf(OP) = rawevent(k)
            OP = OP + 1
         end do
      end if                             ! End of good event block


C --  Fetch next event
      end do                             ! End-Of-Event Loop
C=======================================================================

C --  Buffer finished or error has occured, return to main
 999  continue
      return
      end                                ! End-Of-Subroutine reduc_winformat
