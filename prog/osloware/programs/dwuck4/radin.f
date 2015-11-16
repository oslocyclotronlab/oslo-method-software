      
      SUBROUTINE RADIN(KT,DRF,VB,UB,SL,RMS,OPT,SI,ICODE,LTR1,LAM,       
     &  FMUV,KMAX)                                                      
C     THE NEXT 1 CARDS CONVERT DWUCK TO AN IBM 360                      
C     IMPLICIT REAL*8(A-H,O-Z)                                          
C     INTEGRATES U1(X)*U2(X)*G-SUB-LAM,L(X,R)*X**2*DX                   
C     USES SIMPSON'S RULE  (BOTH END POINTS ARE SMALL)                  
      DIMENSION UB(KMAX,4),VB(KMAX,4),SI(1),DG(2)                       
C                                                                       
      DX=FMUV*DRF                                                       
      F3=EXP(DX)                                                        
      MDEL=0                                                            
      IF(ICODE.NE.2) MDEL=30.0/DX                                       
      IF(ICODE.EQ.4) MDEL=5.5/DX                                        
      MDEL=2*(MDEL/2)+1                                                 
      TERM=2.0*FMUV*FMUV                                                
      LTR2=LTR1+LTR1-2                                                  
      A1=FLOAT(LTR2+3)/FLOAT(LTR2+1)                                    
      A2=FLOAT(LTR2-1)/FLOAT(LTR2+1)                                    
      DG(1)=2.0*DRF/3.0                                                 
      DG(2)=2.0*DG(1)                                                   
      MY=1                                                              
      SL=0.0                                                            
      RMS=0.0                                                           
      R2=0.0                                                            
      DO 500 M=1,KT                                                     
      MK=M+M-1                                                          
      R2=R2+DRF                                                         
      R2SQ=R2*R2                                                        
      SLL=0.0                                                           
      IF(ICODE.EQ.2) GO TO 320                                          
      M1=MAX0( 1,M-MDEL-MY)                                             
      M2=MIN0(KT,M+MDEL+MY)                                             
      X=FLOAT(M1-M)*DX                                                  
      R1=FLOAT(M1-1)*DRF                                                
      IF(ICODE.EQ.4) GO TO 120                                          
      IF(ICODE.EQ.8) GO TO 220                                          
C                                                                       
C     YUKAWA SECTION                                                    
C                                                                       
      F2=EXP(X)                                                         
      DO 100 MM=M1,M2                                                   
      F1=UB(MM,1)                                                       
      IF(M-MM)70,90,80                                                  
   70 TEMP=VB(M,1)*VB(MM,4)*F2                                          
      F2=F2/F3                                                          
      GO TO 95                                                          
   80 TEMP=VB(MM,3)*VB(M,2)*F2                                          
      F2=F2*F3                                                          
      GO TO 95                                                          
   90 TEMP=0.5*(VB(M,1)*VB(MM,4)+VB(MM,3)*VB(M,2))*F2                   
      F2=F2/F3                                                          
   95 CONTINUE                                                          
      SLL=SLL+F1*TEMP                                                   
  100 CONTINUE                                                          
      GO TO 450                                                         
  120 CONTINUE                                                          
C                                                                       
C     GAUSSIAN SECTION                                                  
C                                                                       
      DO 200 MM=M1,M2                                                   
      R1=R1+DRF                                                         
      F1=UB(MM,1)                                                       
      M12=IABS(M-MM)+1                                                  
      X=TERM*R1*R2                                                      
      TEMP=VB(M12,1)*SBESM1(LTR1,X)                                     
      SLL=SLL+F1*TEMP                                                   
  200 CONTINUE                                                          
      GO TO 450                                                         
  220 CONTINUE                                                          
C                                                                       
C     R**2 X YUKAWA TENSOR SECTION                                      
C     SEE LOVE AND PARISH, NUCL.PHYS. A157, 625 (1970)                  
C       AND HORIE AND SASAKI, PROG.THEO. PHYS. 25, 475 (1960).          
C                                                                       
      F2=EXP(X)                                                         
      DO 300 MM=M1,M2                                                   
      R1=R1+DRF                                                         
      R1SQ=R1*R1                                                        
      F1=UB(MM,1)                                                       
      IF(LAM.NE.LTR1) GO TO 260                                         
      IF(M-MM) 230,230,240                                              
  230 TEMP=((R1SQ+R2SQ)*UB(M,3)*UB(MM,4)                                
     &    -R1*R2*(A1*VB(M,1)*VB(MM,2)+A2*VB(M,3)*VB(MM,4)))*F2          
      F2=F2/F3                                                          
      GO TO 295                                                         
  240 TEMP=((R1SQ+R2SQ)*UB(MM,3)*UB(M,4)                                
     &    -R1*R2*(A1*VB(MM,1)*VB(M,2)+A2*VB(MM,3)*VB(M,4)))*F2          
      F2=F2*F3                                                          
      GO TO 295                                                         
  260 CONTINUE                                                          
      IF(M-MM)270,270,280                                               
  270 TEMP=(R1SQ*VB(M,1)*VB(MM,2)+R2SQ*VB(M,3)*VB(MM,4)                 
     &    - 2.0*R1*R2*UB(M,3)*UB(MM,4))*F2                              
      F2=F2/F3                                                          
      GO TO 295                                                         
  280 TEMP=(R1SQ*VB(MM,1)*VB(M,2)+R2SQ*VB(MM,3)*VB(M,4)                 
     &    - 2.0*R1*R2*UB(MM,3)*UB(M,4))*F2                              
      F2=F2*F3                                                          
  295 CONTINUE                                                          
      SLL=SLL+F1*TEMP                                                   
  300 CONTINUE                                                          
      GO TO 450                                                         
  320 CONTINUE                                                          
C                                                                       
C     COULOMB SECTION                                                   
C                                                                       
      DO 400 MM=1,KT                                                    
      F1=UB(MM,1)                                                       
      IF(M-MM) 370,390,380                                              
  370 TEMP=VB(M,1)*VB(MM,4)                                             
      GO TO 395                                                         
  380 TEMP=VB(MM,3)*VB(M,2)                                             
      GO TO 395                                                         
  390 TEMP=0.5*(VB(M,1)*VB(MM,4)+VB(MM,3)*VB(M,2))                      
  395 CONTINUE                                                          
      SLL=SLL+F1*TEMP                                                   
  400 CONTINUE                                                          
C                                                                       
  450 CONTINUE                                                          
      TEMP=SLL*OPT                                                      
      SI(MK)=SI(MK)+TEMP                                                
      MY=3-MY                                                           
      TEMP=DG(MY)*TEMP*R2SQ                                             
      SL=SL+TEMP                                                        
      RMS=RMS+TEMP*R2SQ                                                 
  500 CONTINUE                                                          
      RETURN                                                            
      END
