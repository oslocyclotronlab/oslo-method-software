      
      FUNCTION DELR(J1,J2,J3)                                           
      DIMENSION F(111),FACT(110)                                        
      COMMON /FACTRL/ F                                                 
      EQUIVALENCE (FACT(1),F(2))                                        
C                                                                       
      IF(PHASEF(J1+J2+J3).LT.0.0) GO TO 130                             
      JZ1=(J1+J2-J3)/2                                                  
      IF(JZ1.LT.0) GO TO 130                                            
      JZ2=(J1-J2+J3)/2                                                  
      IF(JZ2.LT.0) GO TO 130                                            
      JZ3=(J2+J3-J1)/2                                                  
      IF(JZ3.LT.0) GO TO 130                                            
      JZ4=(J1+J2+J3)/2+1                                                
      IF(JZ3.LT.JZ2) GO TO 80                                           
      IF(JZ3.LT.JZ1) GO TO 70                                           
      DELR=YXFCT(JZ4,JZ3)+FACT(JZ1)+FACT(JZ2)                           
      GO TO 150                                                         
   70 DELR=YXFCT(JZ4,JZ1)+FACT(JZ2)+FACT(JZ3)                           
      GO TO 150                                                         
   80 IF(JZ2.LT.JZ1) GO TO 70                                           
      DELR=YXFCT(JZ4,JZ2)+FACT(JZ1)+FACT(JZ3)                           
      GO TO 150                                                         
  130 DELR=-1.1E10                                                      
  150 RETURN                                                            
      END
