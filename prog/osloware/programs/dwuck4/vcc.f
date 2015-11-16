      
      FUNCTION VCC(JX1,JX2,JX3,MX1,MX2)                                 
      DIMENSION F(111),FACT(110)                                        
      COMMON /FACTRL/ F                                                 
      EQUIVALENCE (FACT(1),F(2))                                        
C                                                                       
      VCC=0.0                                                           
      J1=JX1                                                            
      J2=JX2                                                            
      J3=JX3                                                            
      M1=MX1                                                            
      M2=MX2                                                            
      IF(J1.LT.J2) GO TO 20                                             
      IF(J3.LT.J2) GO TO 30                                             
      ICNTR=0                                                           
      GO TO 40                                                          
   20 IF(J3.LT.J1) GO TO 30                                             
      ICNTR=-1                                                          
      IT=J1                                                             
      J1=J2                                                             
      J2=IT                                                             
      IT=M1                                                             
      M1=M2                                                             
      M2=IT                                                             
      GO TO 40                                                          
   30 ICNTR=1                                                           
      IT=J2                                                             
      J2=J3                                                             
      J3=IT                                                             
      M2=-M1-M2                                                         
   40 CONTINUE                                                          
      IF(PHASEF(J1+J2+J3).LT.0.0) GO TO 150                             
      JZ1=(J1+J2-J3)/2                                                  
      IF(JZ1.LT.0) GO TO 150                                            
      JZ2=(J1+J3-J2)/2                                                  
      IF(JZ2.LT.0) GO TO 150                                            
      JZ3=(J2+J3-J1)/2                                                  
      IF(JZ3.LT.0) GO TO 150                                            
      IF(J1-IABS(M1).LT.0) GO TO 150                                    
      IF(J2-IABS(M2).LT.0) GO TO 150                                    
      IF(J3-IABS(M1+M2).LT.0) GO TO 150                                 
      JT1=(J1-J3+M2)/2                                                  
      JT2=(J2-J3-M1)/2                                                  
      NUMIN=MAX0(JT1,JT2,0)                                             
      JT3=(J1-M1)/2                                                     
      JT4=(J2+M2)/2                                                     
      NUMAX=MIN0(JT3,JT4,JZ1)                                           
      JT5=(J2-M2)/2                                                     
      IF(NUMAX.LT.NUMIN) GO TO 150                                      
      J4=J1/2                                                           
      J5=J3/2                                                           
      PHAS=PHASEF(NUMIN)                                                
      FCTOR=YXFCT(J4,(J1+M1)/2)+YXFCT(J4,JT3)+YXFCT((J1+J2+J3)/2+1,JZ2) 
     &  +YXFCT(J5,(J3+M1+M2)/2)+YXFCT(J5,(J3-M1-M2)/2)                  
     &  +FACT(JZ1)+FACT(JZ3)+FACT(JT4)+FACT(JT5)+ALOG(FLOAT(J3+1))      
      FCTOR=0.5*FCTOR                                                   
      DO 100 NU=NUMIN,NUMAX                                             
      TERM=FCTOR    +(YXFCT(JT3-NU,J4)+YXFCT(NU-JT2,J5))                
     &  -(FACT(JT4-NU)+FACT(NU-JT1)+FACT(JZ1-NU)+FACT(NU))              
      VCC=VCC+PHAS*EXP(TERM)                                            
      PHAS=-PHAS                                                        
  100 CONTINUE                                                          
      IF(ICNTR) 120,150,110                                             
  110 VCC=VCC*SQRT(FLOAT(J2+1)/FLOAT(J3+1))*PHASEF(JT3)                 
      GO TO 150                                                         
  120 VCC=VCC*PHASEF(JZ1)                                               
  150 RETURN                                                            
      END
