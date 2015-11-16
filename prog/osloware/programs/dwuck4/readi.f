      
      SUBROUTINE READI(JUNIT,IP,I,N,U)                                  
C     FINDS AND READS I-TH RECORD FROM UNIT JUNIT, OF LENGTH N WORDS    
C     INTO ARRAY U.  IP IS LAST RECORD READ.                            
      DIMENSION U(N)                                                    
      J=I-IP                                                            
      IF(I.LE.0) WRITE(6,90) I                                          
      IF(J.EQ.0) GO TO 20                                               
      IF(J.GT.0) GO TO 5                                                
      JP=IABS(J)+1                                                      
      DO 4 K=1,JP                                                       
      BACKSPACE JUNIT                                                   
    4 CONTINUE                                                          
      GO TO 15                                                          
    5 CONTINUE                                                          
      JP=J-1                                                            
      IF(JP.EQ.0) GO TO 15                                              
      DO 10 K=1,JP                                                      
      READ(JUNIT)                                                       
   10 CONTINUE                                                          
   15 CONTINUE                                                          
      READ(JUNIT) U                                                     
      IP=I                                                              
   20 CONTINUE                                                          
      RETURN                                                            
   90 FORMAT(17H IN READI--BAD I=,I5)                                   
      END
