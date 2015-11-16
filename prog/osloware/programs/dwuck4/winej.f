      
      FUNCTION WINEJ(J1,J2,J3,J4,J5,J6,J7,J8,J9)                        
      WINEJ=0.0                                                         
      NUMIN=MAX0(IABS(J1-J9),IABS(J2-J6),IABS(J4-J8))                   
      NUMAX=MIN0(J1+J9,J2+J6,J4+J8)                                     
      IF(NUMAX.LT.NUMIN) GO TO 40                                       
      DO 20 NUX=NUMIN,NUMAX,2                                           
      NU=NUX                                                            
      PROD=RACAH(J1,J4,J9,J8,J7,NU)*RACAH(J2,J5,NU,J4,J8,J6)            
     &    *RACAH(J9,NU,J3,J2,J1,J6)*FLOAT(NU+1)                         
      WINEJ=WINEJ+PROD                                                  
   20 CONTINUE                                                          
      WINEJ=WINEJ*PHASEF((J1+J3+J5+J8)/2+J2+J4+J9)                      
   40 RETURN                                                            
      END
