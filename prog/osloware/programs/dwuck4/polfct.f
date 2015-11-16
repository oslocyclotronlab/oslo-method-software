      
      SUBROUTINE POLFCT(POL,SUMR,SUMI,JSX,JRX)                          
      DIMENSION SUMR(5,5),SUMI(5,5),B(2,2),C(3,3),D(4,4),TSUM(8),POL(9) 
     &  ,CC(3,3,3),DD(4,4,3),DR(3,3,3),DI(3,3),SF(3,3)                  
C                                                                       
C     DATA FOR SIGMA-Y                                                  
C                                                                       
C     SPIN-1/2                                                          
      DATA B/0.0,-1.0,1.0,0.0/                                          
C                                                                       
C     SPIN-1                                                            
      DATA C/0.0,-0.707106781,0.0, 0.707106781,0.0,-0.707106781         
     &    ,0.0,0.707106781,0.0/                                         
C                                                                       
C     SPIN-3/2                                                          
      DATA D/0.0,-0.577350269,0.0,0.0, 0.577350269,0.0,-0.6666666667,0.0
     &    ,0.0,0.6666666667,0.0,-0.577350269, 0.0,0.0,0.577350269,0.0/  
C                                                                       
C     DATA FOR TENSOR SIGMA X SIGMA (SPIN 1)                            
      DATA CC/0.0,0.0,1.732050807, 0.0,0.0,0.0, 0.0,0.0,0.0             
     &    ,0.0,-1.224744871,0.0, 0.0,0.0,1.224744871, 0.0,0.0,0.0       
     &    ,0.707106781,0.0,0.0,0.0,-1.414213561,0.0,0.0,0.0,0.707106781/
C                                                                       
C     DATA FOR TENSOR SIGMA X SIGMA (SPIN 3/2)                          
      DATA DD/0.0,0.0,1.414213561,0.0, 0.0,0.0,0.0,1.414213561, 8*0.0   
     &    ,0.0,1.414213561,0.0,0.0,4*0.0,0.0,0.0,0.0,-1.414213561,4*0.0 
     &    ,1.0,4*0.0,-1.0,4*0.0,-1.0,4*0.0,1.0/                         
C                                                                       
C     DATA FOR SPIN-FLIP CALCULATIONS                                   
      DATA DR/1.,0.,0.,0.,0.,0.,0.,0.,0., .707106781,0.,0.,0.,          
     & .707106781,0.,0.,0.,0., .5,0.,-.5,0.,0.,0.,-.5,0.,.5/            
      DATA DI/0.,.707106781,0., .707106781,0.,.707106781, 0.,           
     & .70716781,0./                                                    
      DATA SF/0.,1.,0., 1.,0.,1., 0.,1.,0./                             
      DO 2 I=1,9                                                        
      POL(I)=0.0                                                        
    2 CONTINUE                                                          
      JS=JSX                                                            
      JR=JRX                                                            
      LDEL=0                                                            
      M=1                                                               
      GO TO 6                                                           
    5 JS=JRX                                                            
      JR=JSX                                                            
      LDEL=4                                                            
      M=2                                                               
    6 CONTINUE                                                          
      I0 = 0                                                            
      J0 = 0                                                            
      IF (JR.EQ.1) I0 = 1                                               
      IF (JS.EQ.1) J0 = 1                                               
      DO 100 I=1,JR                                                     
      DO 90 J=1,JS                                                      
      DO 7 K=1,8                                                        
      TSUM(K)=0.0                                                       
    7 CONTINUE                                                          
      TR = 0.                                                           
      TI = 0.                                                           
      DO 80 K=1,JS                                                      
      IF(JS.GT.4) GO TO 80                                              
      GO TO (10,20,30,40),JS                                            
   10 TEMP=1.0                                                          
      GO TO 50                                                          
   20 TEMP=B(K,J)                                                       
      GO TO 50                                                          
   30 TEMP=C(K,J)                                                       
      GO TO 50                                                          
   40 TEMP=D(K,J)                                                       
   50 CONTINUE                                                          
      GO TO (61,62),M                                                   
   61 T1=SUMR(K,I)                                                      
      T2=SUMI(K,I)                                                      
      GO TO 70                                                          
   62 T1=SUMR(I,K)                                                      
      T2=SUMI(I,K)                                                      
      IF(IABS(JS-JR).EQ.1) GO TO 70                                     
      IF((JS.GT.3).OR.(JR.GT.3)) GO TO 70                               
      FR=0.0                                                            
      FI=0.0                                                            
      DO 64 L=1,JR                                                      
      FR = FR + DR(I,L,JR)*SUMR(L,K) + DI(I,L)*SUMI(L,K)                
      FI = FI + DR(I,L,JR)*SUMI(L,K) - DI(I,L)*SUMR(L,K)                
   64 CONTINUE                                                          
      TR = TR + DR(J,K,JS)*FR - DI(J,K)*FI                              
      TI = TI + DR(J,K,JS)*FI + DI(J,K)*FR                              
   70 CONTINUE                                                          
      TSUM(1)=TSUM(1)-T2*TEMP                                           
      TSUM(2)=TSUM(2)+T1*TEMP                                           
      IF(JS.LT.3) GO TO 80                                              
      IF(JS.GT.4) GO TO 80                                              
   71 CONTINUE                                                          
      DO 75 L=1,3                                                       
      LL=L+L+1                                                          
      IF(JS.EQ.4) GO TO 72                                              
      TEMP=CC(K,J,L)                                                    
      GO TO 74                                                          
   72 TEMP=DD(K,J,L)                                                    
   74 CONTINUE                                                          
      TSUM(LL  )=TSUM(LL  )+T1*TEMP                                     
      TSUM(LL+1)=TSUM(LL+1)+T2*TEMP                                     
   75 CONTINUE                                                          
   80 CONTINUE                                                          
      GO TO (81,82),M                                                   
   81 T1=SUMR(J,I)                                                      
      T2=SUMI(J,I)                                                      
      GO TO 83                                                          
   82 T1=SUMR(I,J)                                                      
      T2=SUMI(I,J)                                                      
      POL(9) = POL(9) + SF(I+I0,J+J0)*(TR*TR+TI*TI)                     
   83 CONTINUE                                                          
      DO 85 L=1,4                                                       
      LL=L+L-1                                                          
      K=L+LDEL                                                          
      POL(K)=POL(K)+TSUM(LL  )*T1+TSUM(LL+1)*T2                         
   85 CONTINUE                                                          
   90 CONTINUE                                                          
  100 CONTINUE                                                          
      GO TO (5,105),M                                                   
  105 POL(7)=-POL(7)                                                    
      POL(5)=-POL(5)                                                    
      RETURN                                                            
      END
