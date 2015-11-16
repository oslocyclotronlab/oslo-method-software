C     =========================================================%
      subroutine gain_sort ( esp )
C     =========================================================%
C                                                              %
C     DESCRIPTION : Use in on-line data acquisition            %
C     COMMENTS    :                                            %
C     =========================================================%
C     PURPOSE : Sort an event residing in the event matrix     %
C               and increment the spectrum accumulators.       %
C     INPUT  via parameters  : NONE                            %
C     OUTPUT via parameters  : NONE                            %
C                                                              %
C     INPUT  via common      : COMMON /gainshift/...           %
C                                  gain and shift parameters   %
C                              COMMON /event/m(0:11,0:15)      %
C                                  the event matrix            %
C                              COMMON /partid/range(0:2047)    %
C                                  particle id. curve          %
C     OUTPUT via common      : COMMON /spectra/...             %
C                                  the spectrum area,          %
C                                  In file SPEC-DEF:DECL       %
C     =========================================================%
      IMPLICIT NONE

      INCLUDE 'common_defs.decl'
      INCLUDE 'spec_params.decl'      
       
      real randval,rand
      integer i, status
      integer e, de, telenu
      real randval

C --  Reset 
      e      = -2048
      status = 0
      telenu = -1

C***********************************************************
C     Testing TPU1
C***********************************************************
C--   Only accepting events with particles
C      if (m(0,0).EQ.0)                            goto 9999


C--   Reading e and de from telescope-group A 
      if (btest(m(0,1),0))then
        de=m(1,0)
        e =m(2,0)
        telenu=e /2048
        i     =de/2048
        if (telenu.NE.i)                           goto 9999
        de = de - telenu*2048
        e  = e  - telenu*2048
        status = status + 1
      end if
  
C --  Reading e and de, telescope group B
      if (btest(m(0,1),2)) then
        de=m(1,2)
        e =m(2,2)
        telenu=e /2048
        i=     de/2048
        if (telenu.ne.i)                           goto 9999
        de = de - telenu*2048
        e  = e  - telenu*2048
        telenu = telenu + 4
        status = status + 1
      end if            
                                    
C --  Exit if unvalid telescopenumber
      if(telenu.LT.0.OR.telenu.GT.7)               goto 9999

C --  Exit if none or both telescope groups identified
      if (status .NE. 1)                           goto 9999  
                           
C --  Find a random number between -0.5 and +0.5
      randval = rand(0) - 0.5
                               
      e  = INT ((e + randval) * gaine(telenu)  + shifte(telenu) + 0.5)

C --  Exit if wring energy
      if(e.LT.0.OR.e.GT.2047)                     goto 9999

C --  Increment Energy spectrum matrix, esp
      esp(e,telenu) = esp(e,telenu) + 1


 9999 continue                                   ! Exit point
      return
      end          ! End-Of-Subroutine gain_sort

