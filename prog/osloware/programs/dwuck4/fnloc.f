      
      SUBROUTINE FNLOC(U,W,VB,LTR)                                      
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
      DIMENSION U(800,2),W(800),VB(800)                                 
      DIMENSION RT(8,7),FACTNL(2)                                       
      DATA  RT/  0.0   ,  4.4934,  7.7253, 10.9041, 14.0662, 17.2208,   
     & 20.3713, 23.5195,  2.0816,  5.9404,  9.2058, 12.4044, 15.5792,   
     & 18.7426, 21.8997, 25.0528,  3.3421,  7.2899, 10.6139, 13.8461,   
     & 17.0429, 20.2219, 23.3905, 26.5526,  4.5141,  8.5838, 11.9727,   
     & 15.2445, 18.4681, 21.6666, 24.8501, 28.0239,  5.6467,  9.8404,   
     & 13.2956, 16.6093, 19.8624, 23.0828, 26.2833, 29.4706,  6.7565,   
     & 11.0702, 14.5906, 17.9472, 21.2311, 24.4748, 27.6937, 30.8960,   
     &  7.8511, 12.2793, 15.8632, 19.2627, 22.5781, 25.8461, 29.0843,   
     & 32.3025/                                                         
C                                                                       
C     FINITE RANGE CORRECTION                                           
C                                                                       
      IF(FNRNG.EQ.0.0) GO TO 348                                        
      IF(FM(1)-FM(2))143,348,142                                        
C                                                                       
C     I1 IS FOR LARGER MASS, I2 IS FOR SMALLER MASS                     
C                                                                       
  142 I1=1                                                              
      I2=2                                                              
      GO TO 144                                                         
  143 I1=2                                                              
      I2=1                                                              
  144 CONTINUE                                                          
      FM1=FMU(I1)                                                       
      FM2=FMU(I2)                                                       
      TEMP=FM(I2)*FNRNG*FNRNG/FM(I1)                                    
      FACT=TEMP*FMA(I1)/FMA(I2)                                         
      FMX=FM(I1)-FM(I2)                                                 
      TEMP=TEMP*FMX                                                     
      IF((FMX.LT.1.5).OR.(ICON(2).NE.2)) GO TO 260                      
      FNRNG=-ABS(FNRNG)                                                 
C                                                                       
C     FORM FACTOR PART BY ROST-KUNZ METHOD                              
C     USED FOR MULTI-NUCLEON TRANSFER REACTIONS                         
C                                                                       
      DR2=DR(3)**2                                                      
      FLF=LTR*(LTR+1)                                                   
      R3=DR(3)*FLOAT(K+1)                                               
      D3=0.0                                                            
      A3=0.0                                                            
      KM=K+K+1                                                          
      R2=R3-DR(3)                                                       
      D2=-FLF/R3                                                        
      A2=W (KM-2)*R2                                                    
      DO 220 M=1,K                                                      
      KM=KM-2                                                           
      R1=R2-DR(3)                                                       
      A1=W (KM-2)*R1                                                    
      D1=-FLF/(R2*R2)                                                   
      IF(A3.EQ.0.0) GO TO 215                                           
      D1=D1+(A3-2.0*A2+A1)/(A2*DR2)                                     
      W (KM+2)=EXP(D2*FACT)*W (KM+2)                                    
      DX2=(D3*A3-2.0*D2*A2+D1*A1)/(R2*DR2)-A2*D2*D2/R2                  
      W (KM+2)=W (KM+2)+0.5*DX2*FACT*FACT                               
  215 CONTINUE                                                          
      IF(ABS(W (KM-2)).LT.ABS(W (KM  ))) GO TO 225                      
      R3=R2                                                             
      R2=R1                                                             
      D3=D2                                                             
      D2=D1                                                             
      A3=A2                                                             
      A2=A1                                                             
  220 CONTINUE                                                          
  225 CONTINUE                                                          
      KM=KM/2+1                                                         
      DO 227 M=1,KM                                                     
      MK=M+M-1                                                          
      VB(M)=W(MK)                                                       
      W(MK)=0.0                                                         
  227 CONTINUE                                                          
      RKM=FLOAT(KM)*DR(3)                                               
      LL=MIN0(LTR,6)                                                    
      FLF=LL*(LL+1)                                                     
      DR12=DR2/12.0                                                     
      DO 250 N=1,8                                                      
      FKX=(RT(N,LL+1)/RKM)**2                                           
      A1=0.0                                                            
      D1=0.0                                                            
      RF=DR(3)                                                          
      A2=DR(3)                                                          
      D2=(1.0+DR12*(FKX-FLF/(RF*RF)))*A2                                
      FNORM=0.0                                                         
      AN=0.0                                                            
      DO 230 M=1,KM                                                     
      AN=AN+(VB(M)*A2*RF)                                               
      FNORM=FNORM+A2*A2                                                 
      VB(M+400)=A2/RF                                                   
      RF=RF+DR(3)                                                       
      D3=1.0+DR12*(FKX-FLF/(RF*RF))                                     
      A3=(12.0*A2-10.0*D2-D1)/D3                                        
      D1=D2                                                             
      D2=D3*A3                                                          
      A1=A2                                                             
      A2=A3                                                             
  230 CONTINUE                                                          
      AN=AN-0.5*VB(KM)*A1*(RF-DR(3))                                    
      FNORM=FNORM-0.5*A1*A1                                             
      FNORM=AN*EXP(-FACT*FKX)/FNORM                                     
      DO 240 M=1,KM                                                     
      MK=M+M-1                                                          
      W (MK)=W (MK)+VB(M+400)*FNORM                                     
  240 CONTINUE                                                          
  250 CONTINUE                                                          
      GO TO 300                                                         
  260 CONTINUE                                                          
      IF(FNRNG.GT.0.0) GO TO 300                                        
C                                                                       
C     SINGLE PARTICLE PART, GAUSSIAN FACTOR                             
C                                                                       
      DO 270 M=1,K                                                      
      MK=M+M-1                                                          
      FACT=EXP(-TEMP*VB(M)/FMU(3))                                      
      W(MK  )=W(MK  )*FACT                                              
      W(MK+1)=W(MK+1)*FACT                                              
  270 CONTINUE                                                          
  300 CONTINUE                                                          
C                                                                       
C     DISTORTED WAVE PART                                               
C                                                                       
      DO 345 M=1,K                                                      
      MK=M+M-1                                                          
      CTEMP1=TEMP*(U(MK  ,I2)/FM2-U(MK  ,I1)/FM1   )                    
      CTEMP2=TEMP*(U(MK+1,I2)/FM2-U(MK+1,I1)/FM1   )                    
      IF(FNRNG.LT.0.0) GO TO 320                                        
      UT1=CTEMP1+TEMP*VB(M)/FMU(3)+1.0                                  
      UT2=CTEMP2                                                        
      DET=UT1*UT1+UT2*UT2                                               
      UT1= UT1/DET                                                      
      UT2= UT2/DET                                                      
      GO TO 330                                                         
  320 CONTINUE                                                          
      FACT=EXP(-CTEMP1)                                                 
      UT1=FACT*COS(CTEMP2)                                              
      UT2=FACT*SIN(CTEMP2)                                              
  330 CONTINUE                                                          
      UF1   =UT1*W(MK  )+UT2*W(MK+1)                                    
      UF2   =UT1*W(MK+1)-UT2*W(MK  )                                    
      W(MK  )=UF1                                                       
      W(MK+1)=UF2                                                       
  345 CONTINUE                                                          
  348 CONTINUE                                                          
C                                                                       
C     NON LOCALITY CORRECTION FACTOR FOR DISTORTED WAVES                
C                                                                       
      DO 350 I=1,2                                                      
      FACTNL(I)=PNLOC(I)**2/4.0                                         
  350 CONTINUE                                                          
      IF(FACTNL(1)+FACTNL(2).EQ.0.0) GO TO 410                          
      DO 400 M=1,K                                                      
      MK=M+M-1                                                          
      DO 390 I=1,2                                                      
      IF(FACTNL(I).EQ.0.0) GO TO 390                                    
      CTEMP1=-FACTNL(I)*(U(MK  ,I)-FK2(I))                              
      CTEMP2=-FACTNL(I)* U(MK+1,I)                                      
      IF(PNLOC(I).LT.0.0) GO TO 370                                     
      CTEMP1=1.0-CTEMP1                                                 
      THETA=ATAN(CTEMP2/CTEMP1)/2.0                                     
      RX=SQRT(SQRT(CTEMP1*CTEMP1+CTEMP2*CTEMP2))                        
      UT1= COS(THETA)/RX                                                
      UT2= SIN(THETA)/RX                                                
      GO TO 380                                                         
  370 CONTINUE                                                          
      CTEMP1=CTEMP1/2.0                                                 
      FACT=EXP(CTEMP1)                                                  
      CTEMP2=CTEMP2/2.0                                                 
      UT1=FACT*COS(CTEMP2)                                              
      UT2=FACT*SIN(CTEMP2)                                              
  380 CONTINUE                                                          
      UF1   =UT1*W(MK  )-UT2*W(MK+1)                                    
      UF2   =UT1*W(MK+1)+UT2*W(MK  )                                    
      W(MK  )=UF1                                                       
      W(MK+1)=UF2                                                       
  390 CONTINUE                                                          
  400 CONTINUE                                                          
  410 CONTINUE                                                          
      RETURN                                                            
      END
