C    ===========================================================
      subroutine format_event( rawdata, 
     +                         message_box, 
     +                         singles,
     +                         esp,
     +                         desp,
     +                         edesp,
     +                         thicksp,
     +                         gesp,
     +                         tgesp,
     +                         nasp,
     +                         tnasp,
     +                         alfna,
     +                         alfge,
     +                         mat,
     +                         errstat)   

C    ===========================================================
C     Purpose : Sort data into a matrix which is used by       C
C               the sorting application routine.               C
C                                                              C
C     INPUT  via parameter : rawdata : integer                 C
C                            message_box : integer             C
C     OUTPUT via parameter : errstat : integer                 C
C                             = -1  <=> buffer error           C
C                                                              C
C     OUTPUT via common    : COMMON /event/matrix(0:11,0:15)   C
C                                                              C
C     =========================================================C
      IMPLICIT NONE
      INCLUDE 'common_defs.decl'
      INCLUDE 'spec_params.decl'

      integer rawdata(0:32767)		
      integer message_box(0:19)

      integer errstat

      integer bufferlength
      integer i,j,l,n
      integer header_id,header_mask,temp,tpu
      integer evlen,sumlen,counter
      integer xindex,yindex,xrow
      integer nextpointer,currentpointer,header,pattern


      bufferlength = 32767

C     Bit masks
      header_id   = X'F000'          ! Event header (Hex)
      header_mask = X'FF'               


C     Initialisering
      errstat = 0
      currentpointer = 0
      nextpointer = 0  
      sumlen=0
      counter=0


C     ==================================================================
      do i = 0,bufferlength                      ! Event loop

         m(0,0) = 0                         ! Reset TPU id loc.
         m(3,0) = 0                         !         -"-
         m(6,0) = 0                         !         -"-
         m(9,0) = 0                         !         -"-

         currentpointer = nextpointer            ! Pointer to current
         header = rawdata(currentpointer)        ! Start-Of-Event

C --     No more data in buffer if header=0,return from subroutine
C --     If this should occur to early (partly filled buffer) it may
C --     be a serious problem. Mark such a buffer as bad buffer.
         IF (header .EQ. 0 .AND. currentpointer .GT. 32500 ) GOTO 999

C --     Check header for start-of-event code (=Fxxx)
           temp = header .AND. header_id
           if (temp .ne. header_id) then         ! Error has occured
              errstat = -1
              goto 999                           ! Drop current buffer
           end if

C --  Save pointer to next event 
           evlen = header .AND. header_mask
           nextpointer = evlen + currentpointer
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


         if (xindex .ne. 9) then                   ! Not PUR "TPU"
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

      else if (xindex .EQ. 9) then                  ! PUR is identified

         m(xindex,0) = 1

         m(10,0)   = rawdata(currentpointer)   ! PUR(0)
         currentpointer = currentpointer + 1
         m(10,1)   = rawdata(currentpointer)   ! PUR(1)
         currentpointer = currentpointer + 1
      end if


C --     Fetch next subevent
         end do                                   ! End-Of-Subevent loop
C-----------------------------------------------------------------------



C --     Now, the event is formatted and ready to be sorted
         CALL eventsort ( 
     +                         singles,
     +                         esp,
     +                         desp,
     +                         edesp,
     +                         thicksp,
     +                         gesp,
     +                         tgesp,
     +                         nasp,
     +                         tnasp,
     +                         alfna,
     +                         alfge,
     +                         mat )

 



      end do                                      ! End-Of-Event Loop
C=======================================================================


C --  Finished or error has occured, return to main
 999  continue
      return
      end          ! End-Of-Subroutine format_event
