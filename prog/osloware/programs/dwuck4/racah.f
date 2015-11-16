      
      FUNCTION RACAH(J1,J2,J3,J4,J5,J6)                                 
      DIMENSION F(111),FACT(110)                                        
      COMMON /FACTRL/ F                                                 
      EQUIVALENCE (FACT(1),F(2))                                        
C                                                                       
      RACAH=0.0                                                         
      Z1=DELR(J1,J2,J5)                                                 
      IF(Z1.LE.-1.0E10) GO TO 90                                        
      Z1=DELR(J3,J4,J5)+Z1                                              
      IF(Z1.LE.-1.0E10) GO TO 90                                        
      Z2=DELR(J1,J3,J6)                                                 
      IF(Z2.LE.-1.0E10) GO TO 90                                        
      Z2=DELR(J2,J4,J6)+Z2                                              
      IF(Z2.LE.-1.0E10) GO TO 90                                        
      Z1=0.5*(Z1+Z2)                                                    
      JT1=(J1+J2+J5)/2                                                  
      JT2=(J3+J4+J5)/2                                                  
      JT3=(J1+J3+J6)/2                                                  
      JT4=(J2+J4+J6)/2                                                  
      JZ1=(J1+J2+J3+J4)/2                                               
      JZ2=(J1+J4+J5+J6)/2                                               
      JZ3=(J2+J3+J5+J6)/2                                               
      NUMIN=MAX0(JT1,JT2,JT3,JT4)                                       
      NUMAX=MIN0(JZ1,JZ2,JZ3)                                           
      IF(NUMAX.LT.NUMIN) GO TO 90                                       
      PHASE=PHASEF(NUMIN+JZ1)                                           
      DO 80 NU=NUMIN,NUMAX                                              
      JY1=NU-JT1                                                        
      JY2=NU-JT2                                                        
      JY3=NU-JT3                                                        
      JY4=JZ1-NU                                                        
      JY5=JZ2-NU                                                        
      JY6=JZ3-NU                                                        
      FCTOR=FACT(JY1)+FACT(JY2)+FACT(JY3)+YXFCT(NU+1,NU-JT4)            
     &  +FACT(JY4)+FACT(JY5)+FACT(JY6)                                  
      ARG=Z1-FCTOR                                                      
      IF(ARG.GT.-40.0) RACAH=RACAH+PHASE*EXP(ARG)                       
      PHASE=-PHASE                                                      
   80 CONTINUE                                                          
   90 RETURN                                                            
      END
