      PROGRAM MAMA
C Simplified by Magne

      CHARACTER*40 ANS
c      integer status, system

      WRITE(IW,592)
592   FORMAT(1X/
     +' ____________________________________________________'/
     +'|                                                    |'/ 
     +'|          Welcome to the real M A M A  5.1          |'/
     +'|                                                    |'/
     +'|   MAtrix MAnipulation, Oslo Cyclotron Laboratory   |'/
     +'|           Magne Guttormsen, January 1999           |'/
     +'|           Compiled for opal.nscl.msu.edu           |'/
     +'|          Andreas Schiller  September 2003          |'/
     +'|                                                    |'/
     +'|   MAMA handles 2 matrices of dimension 4096x512    |'/
     +'|       and 2 singles spectra of length 8192         |'/
     +'|                                                    |'/
     +'| Imortant commands:                                 |'/
     +'|  HE - help            ST - stop MAMA               |'/
     +'|  RE - read file       WR - write file              |'/
     +'|  DS - display spec.   CR - curser, activate spec.  |'/
     +'|  SD - display SIRIUS  OD - display OFFLINE         |'/
     +'|  HE NW - news                                      |'/
     +'|____________________________________________________|')



c      status=system('xv -geometry -15+105 /user/schiller/osloware/mama/doc/Welcome &')


      CALL GFINIT(2)
 
10    CALL CASK('mama>',ANS,NC)
      IF (NC.GT.1) CALL GFEXEC(ANS,NC) !Decode and execute command
      GO TO 10                         !Ask for new command

      END
