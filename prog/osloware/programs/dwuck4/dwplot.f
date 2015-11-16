      
      SUBROUTINE DWPLOT(NTH,NCYCLE,SIGMA,DTH,THZERO,XLABL,YLABL)        
      DIMENSION SIGMA(1),ARRAY(26),STAR(4),STAR1(4)                     
      DATA BAR,BLNK/4H|   ,4H    /                                      
      DATA STAR /4H*   ,4H *  ,4H  * ,4H   */                           
      DATA STAR1/4H*   ,4H|*  ,4H| * ,4H|  */                           
      DATA ARRAY/26*4H    /                                             
      A = SIGMA(1)                                                      
      DO 40 I=2,NTH                                                     
      IF (SIGMA(I).GT.A) A = SIGMA(I)                                   
   40 CONTINUE                                                          
      IF (A.LE.0.) GO TO 200                                            
      ARG = ALOG10(A) - FLOAT(NCYCLE)/200.                              
      N = INT(ARG)                                                      
      IF (A.LT.1.) N = N - 1                                            
      N = N - NCYCLE + 1                                                
      NN1 = N + 1                                                       
      NN2 = N + 2                                                       
      NN3 = N + 3                                                       
      NN4 = N + 4                                                       
      NN5 = N + 5                                                       
      GO TO (50,60,70,80,90), NCYCLE                                    
   50 WRITE(6,11) N,NN1,XLABL,YLABL                                     
      GO TO 100                                                         
   60 WRITE(6,12) N,NN1,NN2,XLABL,YLABL                                 
      GO TO 100                                                         
   70 WRITE(6,13) N,NN1,NN2,NN3,XLABL,YLABL                             
      GO TO 100                                                         
   80 WRITE(6,14) N,NN1,NN2,NN3,NN4,XLABL,YLABL                         
      GO TO 100                                                         
   90 WRITE(6,15) N,NN1,NN2,NN3,NN4,NN5,XLABL,YLABL                     
  100 A = N                                                             
      B = THZERO                                                        
      C = 100./FLOAT(NCYCLE)                                            
      BASE = 10.**N                                                     
      IWORD=1                                                           
      WRITE(6,16)                                                       
      DO 180 I=1,NTH                                                    
      ARRAY(1)=BAR                                                      
      ARRAY(26)=BAR                                                     
      IF (SIGMA(I).LT.BASE) GO TO 130                                   
      ARG = (ALOG10(SIGMA(I))-A)*C + 0.5                                
      J = INT(ARG)                                                      
      IWORD=J/4 + 1                                                     
      IKAR=J - 4*(IWORD-1)                                              
      IF(IWORD.NE.1) GO TO 125                                          
      ARRAY(1)=STAR1(IKAR+1)                                            
      GO TO 130                                                         
  125 CONTINUE                                                          
      ARRAY(IWORD)=STAR(IKAR+1)                                         
  130 CONTINUE                                                          
      IF(I.NE.1) GO TO 160                                              
      WRITE(6,17) B,SIGMA(I),ARRAY                                      
      GO TO 170                                                         
  160 WRITE(6,18) B,SIGMA(I),ARRAY                                      
  170 CONTINUE                                                          
      ARRAY(IWORD)=BLNK                                                 
      B = B + DTH                                                       
  180 CONTINUE                                                          
      WRITE(6,16)                                                       
  200 RETURN                                                            
C                                                                       
C     STAR (*) CARRIAGE CONTROL PRINTS OVER PAGE EDGE.                  
C                                                                       
   11 FORMAT(1H0,21X,4H10**,I3,97X,I3/5X,A5,6X,A5,7X,1HI,99X,1HI)       
   12 FORMAT(1H0,21X,4H10**,2(I3,47X),I3/5X,A5,6X,A5,7X,2(1HI,49X),1HI) 
   13 FORMAT(1H0,21X,4H10**,2(I3,30X),I4,30X,I3/5X,A5,6X,A5,7X,2(1HI,32X
     &),2H I,32X,1HI)                                                   
   14 FORMAT(1H0,21X,4H10**,4(I3,22X),I3/5X,A5,6X,A5,7X,4(1HI,24X),1HI) 
   15 FORMAT(1H0,21X,4H10**,5(I3,17X),I3/5X,A5,6X,A5,7X,5(1HI,19X),1HI) 
   16 FORMAT(1H*,27X,101(1H-))                                          
   17 FORMAT(1H+,F9.4,E14.4,4X,26A4)                                    
   18 FORMAT(1H*,F9.4,E14.4,4X,26A4)                                    
      END
