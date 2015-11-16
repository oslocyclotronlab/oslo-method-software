      
      SUBROUTINE BETAFN(D,E,LTR,JX)                                     
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      DIMENSION D(1),E(1),CTEMP(2)                                      
      ISB=IS(3)                                                         
      JR=NS(1)                                                          
      JS=NS(2)                                                          
      MPLUS=JX/2+1                                                      
      INCR=LPL2*(LTR+1)                                                 
      IFACT=2*MPLUS*JR*JS                                               
      J2K=(1.0+PHASEF(NS(2)))/2.0                                       
      M2K=JX-MPLUS-MPLUS+2                                              
      LX=LTR+LTR                                                        
      TEMP1=SQRT(FLOAT((JX+1)*(IS(1)+1)))                               
      IF(FN.NE.0.0) GO TO 11                                            
      IND=LPLUS*IFACT                                                   
      DO 10 M=1,IND                                                     
      E(M)=0.0                                                          
   10 CONTINUE                                                          
   11 CONTINUE                                                          
      IS1=-IS(1)                                                        
      DO 95 I=1,JR                                                      
      IS2=-IS(2)                                                        
      DO 90 J=1,JS                                                      
      IF(NLTR.NE.1) GO TO 14                                            
      IF(JR*JS.EQ.1) GO TO 15                                           
      IF((IBF(5)+IBF(6)).EQ.0) GO TO 15                                 
      IF(ICON(16).GT.0) GO TO 15                                        
   14 CONTINUE                                                          
      IJTST=I*J                                                         
      IF((ICON(16).GT.0).AND.(IJTST.GT.1)) GO TO 15                     
C                                                                       
C     READ RADIAL MATRIX ELEMENTS FROM TAPE 8                           
C                                                                       
      READ(8)(D(INDEX),INDEX=1,INCR)                                    
   15 FLX=1.0                                                           
      DO 80 LL=1,LPLUS                                                  
      L=LL-1                                                            
      LLX=L+L                                                           
      JLX=LLX+IS2                                                       
      IF(JLX.LT.0) GO TO 75                                             
      TEMP2=TEMP1*SQRT(FLOAT(JLX+1))                                    
      LSTOR=L*IFACT                                                     
      LP1=IABS(LL-LTR-1)+1                                              
      LP2=MIN0(LL+LTR,LPLUS)                                            
      LK=0                                                              
      DO 60 LP=LP1,LP2,2                                                
      LPX=LP+LP-2                                                       
      JPX=LPX+IS1                                                       
      IF(JPX.LT.0) GO TO 59                                             
      TEMP=VCC(LLX,LX,LPX,0,0)*PHASEF((LP-LL-LTR)/2)*FLX*FL             
     &         *TEMP2*SQRT(FLOAT(LPX+1))                                
     &*WINEJ(LLX,IS(2),JLX,LX,ISB,JX,LPX,IS(1),JPX)                     
      INDEX=LK+LLX+1                                                    
      KT=0                                                              
      MS =-IS(2)                                                        
      DO 57 MS2=1,JS                                                    
      MSP=-IS(1)                                                        
      DO 55 MS1=1,JR                                                    
      VCP=VCC(LPX,IS(1),JPX,0,MSP)                                      
      DO 50 M=1,MPLUS                                                   
      MK=M+M-1                                                          
      MX=MK-1+M2K                                                       
      ML2=MSP-MX-MS                                                     
      ML=IABS(ML2/2)                                                    
      IF(ML.GT.L) GO TO 50                                              
      IND=LSTOR+KT+MK                                                   
      FACT=VCP*VCC(JLX,JX,JPX,MSP-MX,MX)*VCC(LLX,IS(2),JLX,ML2,MS)      
     & *EXP(0.5*YXFCT(L+ML,L-ML))*TEMP                                  
      E(IND  )=E(IND  )+D(INDEX  )*FACT                                 
      E(IND+1)=E(IND+1)+D(INDEX+1)*FACT                                 
   50 CONTINUE                                                          
      KT=KT+MPLUS+MPLUS                                                 
      MSP=MSP+2                                                         
   55 CONTINUE                                                          
      MS =MS +2                                                         
   57 CONTINUE                                                          
   59 LK=LK+LPL2                                                        
   60 CONTINUE                                                          
   75 CONTINUE                                                          
      FLX=FLX+2.0                                                       
   80 CONTINUE                                                          
      IS2=IS2+2                                                         
   90 CONTINUE                                                          
      IS1=IS1+2                                                         
   95 CONTINUE                                                          
      RETURN                                                            
      END
