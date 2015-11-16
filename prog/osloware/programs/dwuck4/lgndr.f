      
      SUBROUTINE LGNDR(PLM,MPLUS,LPLUS,THET)                            
C     THE NEXT 3 CARDS CONVERT DWUCK TO AN IBM 360                      
C     IMPLICIT REAL*8(A-H,O-Z)                                          
C     SIN(XXX)=DSIN(XXX)                                                
C     COS(XXX)=DCOS(XXX)                                                
      DIMENSION PLM(459)                                                
      THETA=THET /57.295779                                             
      Y=COS(THETA)                                                      
      Z=SIN(THETA)                                                      
      IX=0                                                              
      DO 100 M=1,MPLUS                                                  
      LX=M-1                                                            
      L2=0                                                              
      P3=1.0                                                            
      FL1=LX                                                            
      IF(LX.EQ.0) GO TO 41                                              
      DO 40 LT=1,LX                                                     
      FL1=FL1+1.0                                                       
      P3=P3*FL1*Z/2.0                                                   
   40 CONTINUE                                                          
   41 P2=0.0                                                            
      FL2=FL1+1.0                                                       
      FL3=1.0                                                           
      DO 90 LT=1,LPLUS                                                  
      IX1=IX+LT                                                         
      IF(L2-LX)50,70,60                                                 
   50 PLM(IX1)=0.0                                                      
      GO TO 75                                                          
   60 P3=(FL2*Y*P2-FL1*P1)/FL3                                          
      FL1=FL1+1.0                                                       
      FL2=FL2+2.0                                                       
      FL3=FL3+1.0                                                       
   70 PLM(IX1)=P3                                                       
      P1=P2                                                             
      P2=P3                                                             
   75 L2=L2+1                                                           
   90 CONTINUE                                                          
      IX=IX+LPLUS                                                       
  100 CONTINUE                                                          
      RETURN                                                            
      END
