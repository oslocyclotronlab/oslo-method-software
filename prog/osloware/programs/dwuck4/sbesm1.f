      
      FUNCTION SBESM1(LP,X)                                             
C     COMPUTES EXP(-X) * MODIFIED SPHERICAL BESSEL FUNC., 1ST KIND      
C     ANGULAR MOMENTUM  LP = L+1                                        
      DATA TEST/1.0E20/                                                 
C                                                                       
      MAX=X+X+10.0                                                      
      MAX=MAX0(MAX,LP+5)                                                
      A3=0.0                                                            
      A2=1.0                                                            
      A1=1.0                                                            
      TEMP=1.0                                                          
      IF(LP.EQ.1) GO TO 80                                              
      FL=MAX+MAX+3                                                      
      DO 70 I=1,MAX                                                     
      N=MAX+1-I                                                         
      FL=FL-2.0                                                         
      A1=A3+FL*A2/X                                                     
      IF(N.LT.LP) GO TO 65                                              
      IF(ABS(A1).LT.TEST) GO TO 65                                      
      A1=A1/TEST                                                        
      A2=A2/TEST                                                        
   65 CONTINUE                                                          
      IF(LP.EQ.N) TEMP=A1                                               
      A3=A2                                                             
      A2=A1                                                             
   70 CONTINUE                                                          
   80 CONTINUE                                                          
      TWOX=X+X                                                          
      F1=0.0                                                            
      IF(TWOX.LT.16.0) F1=EXP(-TWOX)                                    
      AZ=(1.0-F1)/TWOX                                                  
      SBESM1=(TEMP/A1)*AZ                                               
      RETURN                                                            
      END                                                               
