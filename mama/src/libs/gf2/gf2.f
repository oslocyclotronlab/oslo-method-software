      PROGRAM GF2

      CHARACTER*40 ANS
      WRITE(6,5)
 5    FORMAT(' ',/,
     +' ____________________________________________________'/
     +'|                                                    |'/ 
     +'|              Welcome to M A M A  7.5.3             |'/
     +'|                                                    |'/
     +'|   MAtrix MAnipulation, Oslo Cyclotron Laboratory   |'/
     +'|           Magne Guttormsen, January 1995           |'/
     +'|                                                    |'/
     +'|   MAMA handles 2 matrices of dimension 4096x2048   |'/
     +'|       and 2 singles spectra of length 8192         |'/
     +'|                                                    |'/
     +'| Important commands:                                |'/
     +'|  HE - help            ST - stop MAMA               |'/
     +'|  RE - read file       WR - write file              |'/
     +'|  DS - display spec.   CR - curser, activate spec.  |'/
     +'|  HE NW - news                                      |'/
     +'|____________________________________________________|')

      CALL GFINIT(2) 
 10   CALL CASK('mama>',ANS,NC)
      IF (NC.GT.1) CALL GFEXEC(ANS,NC) !Decode and execute command
      GO TO 10                         !Ask for new command
      END
