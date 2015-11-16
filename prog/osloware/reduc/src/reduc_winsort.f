C     =========================================================%
      subroutine reduce_sort(good, tel)
C     =========================================================%
C     PURPOSE : Picks out events within a window in the        %
C               thickness spectrum. Alfa particle and H events %
C               are marked as good events                      %
C     INPUT  via parameters  : NONE                            %
C                                                              %
C     INPUT  via common      : COMMON /gainshift/...           %
C                                  gain and shift parameters   %
C                              COMMON /event/m(0:11,0:15)      %
C                                  the event matrix            %
C                              COMMON /partid/range(0:2047)    %
C                                 particle id. curve           %
C                              COMMON /window/ alfa_low(0:7).. %
C                                  lower limit for alpha peak  %
C     OUTPUT via parameter    : good  :: LOGICAL               %
C                                  = .TRUE.  => The event was  %
C                                            good and is saved % 
C                             : tel  :: INTEGER                %
C                                       The telescope hit      %
C     =========================================================%
      IMPLICIT NONE

      INCLUDE 'common_defs.decl'
c      INCLUDE 'spec_params.decl'      !Magne
              
      logical good
      integer tel

      integer i   
      integer e,de,ede,thick,telenu
      integer status

C --  The window limits are read from file by reduc_main
      integer alfa_low,alfa_high,h_low,h_high                                   
      common /window/alfa_low(0:63),alfa_high,h_low,h_high 

C --  Reset
      telenu = -1
      e      = -2048
      status = 0
      good   = .FALSE.

C***********************************************************
C     Testing TPU1
C***********************************************************

C--   Only accepting events with particles
      IF(m(0,0).EQ.0)                        	   goto 9999

C--   Reading e and de from telescope-group A 
      if (btest(m(0,1),0))then
        de=m(1,0)
        e =m(2,0)
        telenu=e /2048
        i     =de/2048
        if (telenu.NE.i)                           goto 9999
        de = de - telenu*2048
        e = e - telenu*2048
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
                                         
C --  if(telenu.LT.0.OR.telenu.GT.7)               goto 9999

C --  Exit if none or both telescope groups identified
      if ( status .NE. 1 )                         goto 9999
      
C --  Save telescope #
      tel = telenu

C --  Calculating energies for endcounter (e), frontcounter (de) and
C --  the sum (ede). Also included are gain and shift of energies
      e  = e *gaine(telenu)  + shifte(telenu)
      de = de*gainde(telenu) + shiftde(telenu)

      if (e.LT.0.OR.e.GT.2047)                     goto 9999
      if (de.LT.0.OR.de.GT.2047)                   goto 9999

      ede=e+de

      if (ede.LT.0.OR.ede.GT.2047)                 goto 9999

      thick=range(ede)-range(e)

C --  Window on p,d and t   
      if (thick .GE. h_low .AND. thick .LE. h_high) good = .TRUE.

C --  Window on alpha
      if (thick .GE. alfa_low(telenu) .AND. thick .LE. alfa_high) good = .TRUE.

 9999 continue                                     ! Exit point
      return
      end          ! End-Of-Subroutine reduce_sort     
