      
      SUBROUTINE SLATR (KT,DRF,VB,MINL,FMU,ICODE,KMAX)                  
C     COMPUTES G-SUB-L(X,R)                                             
      DIMENSION VB(*)                                                   
C                                                                       
C     YUKAWA SLATER EXPANSION                                           
C                                                                       
      R=0.0                                                             
      IF(ICODE.EQ.2) GO TO 101                                          
      IF(ICODE.EQ.4) GO TO 201                                          
C                                                                       
C     HANKEL FUNCTION (MODIFIED SPHERICAL BESSEL FUNCTION, 3RD KIND)    
C       TIMES EXP(X)                                                    
C                                                                       
C     F1=1.0                                                            
C     F2=EXP(-FMU*DRF)                                                  
      DO 100 M=1,KT                                                     
      R=R+DRF                                                           
      X=FMU*R                                                           
C     F1=F1*F2                                                          
      B2=1.0/X                                                          
      IF(MINL.EQ.1) GO TO 55                                            
      B1=B2*(1.0+B2)                                                    
      FL=-1.0                                                           
      DO 50 LL=1,MINL                                                   
      B3=FL*B2/X+B1                                                     
      B1=B2                                                             
      B2=B3                                                             
      FL=FL+2.0                                                         
   50 CONTINUE                                                          
   55 CONTINUE                                                          
      VB(M+KMAX)=B2                                                     
C                                                                       
C     BESSEL FUNCTION (MODIFIED SPHERICAL BESSEL FUNCTION, 1ST KIND)    
C                                                                       
      VB(M     )=SBESM1(MINL,X)                                         
  100 CONTINUE                                                          
      GO TO 400                                                         
C                                                                       
C     COULOMB SLATER EXPANSION                                          
C                                                                       
  101 CONTINUE                                                          
      FL=MINL+MINL-1                                                    
      DO 200 M=1,KT                                                     
      R=R+DRF                                                           
      A2=1.0                                                            
      DO 105 LL=1,MINL                                                  
      A2=A2*R                                                           
  105 CONTINUE                                                          
      VB(M     )=A2/(R*FL)                                              
      VB(M+KMAX)=1.0/A2                                                 
  200 CONTINUE                                                          
      GO TO 400                                                         
C                                                                       
C     GAUSSIAN PART OF GAUSSIAN INTERACTION                             
C                                                                       
  201 CONTINUE                                                          
      X=FMU*DRF                                                         
      X2=X*X                                                            
      F1=1.0                                                            
      F2=EXP(-X2)                                                       
      VB(1)=1.0                                                         
      VB(2)=F2                                                          
      M=1                                                               
      DO 300 K=3,KT                                                     
      M=M+1                                                             
      M2=M*M                                                            
      IF(F1.GT.1.E-12) GO TO 250                                        
      VB(K)=0.0                                                         
      GO TO 300                                                         
  250 CONTINUE                                                          
      F1=F2**M2                                                         
      VB(K)=F1                                                          
  300 CONTINUE                                                          
  400 RETURN                                                            
      END
