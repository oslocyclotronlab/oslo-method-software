      
      SUBROUTINE DSTRIP(IQ,DR,K,F,FR,QNUM,OPT,RMS,KM,SL)                
C     THE NEXT 3 CARDS CONVERT DWUCK TO AN IBM 360                      
C     IMPLICIT REAL*8(A-H,O-Z)                                          
C     SQRT(XXX)=DSQRT(XXX)                                              
C     FLOAT(III)=DFLOAT(III)                                            
      DIMENSION F(400,2),FR(800),QNUM(4,2),TVCC(10),IQ(3),D1(10),D2(10) 
     &,         AH(36),WH(36),AG(15),WG(15)                             
C                                                                       
C     GAUSS-HERMITE  POINTS AND WEIGHTS                                 
C                                                                       
      DATA AH/7.0710678E-01,5.2464762E-01,1.6506801E 00,4.3607741E-01,  
     &        1.3358491E 00,2.3506050E 00,3.8118699E-01,1.1571937E 00,  
     &        1.9816568E 00,2.9306374E 00,3.4290133E-01,1.0366108E 00,  
     &        1.7566836E 00,2.5327317E 00,3.4361591E 00,3.1424038E-01,  
     &        9.4778839E-01,1.5976826E 00,2.2795071E 00,3.0206370E 00,  
     &        3.8897249E 00,2.9174551E-01,8.7871379E-01,1.4766827E 00,  
     &        2.0951833E 00,2.7484707E 00,3.4626569E 00,4.3044486E 00,  
     &        2.7348105E-01,8.2295145E-01,1.3802585E 00,1.9517880E 00,  
     &        2.5462022E 00,3.1769992E 00,3.8694479E 00,4.6887389E 00/  
      DATA WH/8.8622692E-01,8.0491408E-01,8.1312835E-02,7.2462959E-01,  
     &        1.5706732E-01,4.5300099E-03,6.6114701E-01,2.0780232E-01,  
     &        1.7077983E-02,1.9960407E-04,6.1086263E-01,2.4013861E-01,  
     &        3.3874394E-02,1.3436457E-03,7.6404328E-06,5.7013523E-01,  
     &        2.6049231E-01,5.1607985E-02,3.9053906E-03,8.5736870E-05,  
     &        2.6585517E-07,5.3640591E-01,2.7310561E-01,6.8505534E-02,  
     &        7.8500547E-03,3.5509261E-04,4.7164843E-06,8.6285911E-09,  
     &        5.0792947E-01,2.8064746E-01,8.3810041E-02,1.2880311E-02,  
     &        9.3228400E-04,2.7118601E-05,2.3209808E-07,2.6548075E-10/  
C                                                                       
C     GAUSS-LEGENDRE POINTS AND WEIGHTS                                 
C                                                                       
      DATA AG/0.57735027,0.33998104,0.86113631,0.23861919,0.66120939,   
     &        0.93246951,0.18343464,0.52553241,0.79666648,0.96028986,   
     &        0.14887434,0.43339539,0.67940957,0.86506337,0.97390653/   
      DATA WG/1.00000000,0.65214515,0.34785485,0.46791393,0.36076157,   
     &        0.17132449,0.36268378,0.31370665,0.22238130,0.10122854,   
     &        0.29552422,0.26926672,0.21908636,0.14945135,0.06667134/   
      DATA PI   /3.141592/                                              
      FKAP=RMS                                                          
      L =IQ(1)                                                          
      IS=IQ(2)                                                          
      JJ=IQ(3)                                                          
      N1=QNUM(1,1)                                                      
      N2=QNUM(1,2)                                                      
      L1=QNUM(2,1)                                                      
      L2=QNUM(2,2)                                                      
      J1=QNUM(3,1)                                                      
      J2=QNUM(3,2)                                                      
      IS1=QNUM(4,1)*2.0                                                 
      IS2=QNUM(4,2)*2.0                                                 
      IF(PHASEF(L1+L2-L).LT.0.0) GO TO 1000                             
      ITEMP=N1+N1+N2+N2                                                 
      KR=(ITEMP+L1+L2-L+3)/4+8                                          
      KR=(KR+1)/2                                                       
      NX=(ITEMP+L1+L2+L+2)/4+2                                          
      NX=(NX+1)/2                                                       
      WRITE(6,9000)KR,NX                                                
 9000 FORMAT(15H0 NO. R STEPS =,I3,18H     NO. X STEPS =,I3)            
      IF(KR.GT.8) KR=8                                                  
      NKR=(KR*(KR-1))/2                                                 
      IF(NX.GT.5) NX=5                                                  
      NKX=(NX*(NX-1))/2                                                 
      LL=L+L                                                            
      LL1=L1+L1                                                         
      LL2=L2+L2                                                         
      FACT1=EXP(YXFCT(L1,LL1))/2.0**L1                                  
      FACT2=EXP(YXFCT(L2,LL2))/2.0**L2                                  
      TEMP=2.0                                                          
      DO 20 I=1,4                                                       
      IF(QNUM(I,1)-QNUM(I,2))25,20,25                                   
   20 CONTINUE                                                          
      TEMP=4.0                                                          
   25 CONTINUE                                                          
      FNORM=(16.0/PI)*SQRT(FLOAT((LL1+1)*(LL2+1))/(FLOAT(LL+1)*TEMP))   
      FNORM=FNORM*SQRT(FLOAT((LL+1)*(IS+1)*(J1+1)*(J2+1)))              
     &  *WINEJ(LL1,IS1,J1,LL2,IS2,J2,LL,IS,JJ)                          
      FM1=1.0                                                           
      FM2=1.0                                                           
      FL1=L1                                                            
      FL2=L2                                                            
      LPL=MIN0(L1,L2)+1                                                 
      DO 30 M=1,LPL                                                     
      M2=M+M-2                                                          
      FM=M-1                                                            
      TVCC(M)=VCC(LL1,LL2,LL,M2,-M2)*2.0/SQRT(FM1*FM2)                  
      FM1=FM1*(FL1+FM+1)*(FL1-FM)                                       
      FM2=FM2*(FL2+FM+1)*(FL2-FM)                                       
   30 CONTINUE                                                          
      TVCC(1)=TVCC(1)/2.0                                               
C                                                                       
C     CENTER OF MASS R LOOP                                             
C                                                                       
      R=0.0                                                             
      DO 500 M=1,K                                                      
      R=R+DR                                                            
      RSQ=R*R                                                           
      SUMR=0.0                                                          
      MK=M+M-1                                                          
C                                                                       
C     RELATIVE R LOOP                                                   
C                                                                       
      DO 400 MR=1,KR                                                    
      INDX=NKR+MR                                                       
      RHO=AH(INDX)                                                      
      RS=RHO*FKAP                                                       
      WR=WH(INDX)                                                       
      RSSQ=RS*RS+RSQ                                                    
      RPROD=2.0*R*RS                                                    
C                                                                       
C     RELATIVE R ANGLE LOOP                                             
C                                                                       
      SUMX=0.0                                                          
      DO 300 MX=1,NX                                                    
      INDX=NKX+MX                                                       
      X=AG(INDX)                                                        
      WX=WG(INDX)                                                       
      R1=SQRT(RSSQ-RPROD*X)                                             
      R2=SQRT(RSSQ+RPROD*X)                                             
      FK1=R1/DR                                                         
      K1=FK1                                                            
      FK1=FK1-FLOAT(K1)                                                 
      IF(K1.GT.K) GO TO 300                                             
      FK2=R2/DR                                                         
      K2=FK2                                                            
      FK2=FK2-FLOAT(K2)                                                 
      IF(K2.GT.K) GO TO 300                                             
      COS1=(R-RS*X)/R1                                                  
      COS2=(R+RS*X)/R2                                                  
      SIN1=SQRT(1.0-COS1*COS1)                                          
      SIN2=SQRT(1.0-COS2*COS2)                                          
      IX=0                                                              
  120 CONTINUE                                                          
      DM1=0.0                                                           
      DL1=FACT1                                                         
      IF(L1.EQ.0) GO TO 141                                             
      DO 140 LI=1,L1                                                    
      DL1=DL1*SIN1                                                      
  140 CONTINUE                                                          
  141 CONTINUE                                                          
      D1(L1+1)=DL1                                                      
      DM2=0.0                                                           
      DL2=FACT2                                                         
      IF(L2.EQ.0) GO TO 151                                             
      DO 150 LI=1,L2                                                    
      DL2=DL2*SIN2                                                      
  150 CONTINUE                                                          
  151 CONTINUE                                                          
      D2(L2+1)=DL2                                                      
      IF(L1.EQ.0) GO TO 171                                             
      FJ1=1.0                                                           
      FL1=LL1                                                           
      FM1=LL1                                                           
      DO 170 LI=1,L1                                                    
      DK1=(FM1*COS1*DL1/SIN1-DM1)/(FJ1*FL1)                             
      FJ1=FJ1+1.0                                                       
      FL1=FL1-1.0                                                       
      FM1=FM1-2.0                                                       
      DM1=DL1                                                           
      DL1=DK1                                                           
      INDX=L1+1-LI                                                      
      D1(INDX)=DL1                                                      
  170 CONTINUE                                                          
  171 CONTINUE                                                          
      IF(L2.EQ.0) GO TO 181                                             
      FJ2=1.0                                                           
      FL2=LL2                                                           
      FM2=LL2                                                           
      DO 180 LI=1,L2                                                    
      DK2=(FM2*COS2*DL2/SIN2-DM2)/(FJ2*FL2)                             
      FJ2=FJ2+1.0                                                       
      FL2=FL2-1.0                                                       
      FM2=FM2-2.0                                                       
      DM2=DL2                                                           
      DL2=DK2                                                           
      INDX=L2+1-LI                                                      
      D2(INDX)=DL2                                                      
  180 CONTINUE                                                          
  181 CONTINUE                                                          
      PROD=0.0                                                          
      DO 185 LI=1,LPL                                                   
      PROD=PROD+D1(LI)*D2(LI)*TVCC(LI)                                  
  185 CONTINUE                                                          
  280 CONTINUE                                                          
      F1=FK1*(F(K1+1,1)-F(K1,1))+F(K1,1)                                
      F2=FK2*(F(K2+1,2)-F(K2,2))+F(K2,2)                                
      SUMX=SUMX+WX*PROD*F1*F2                                           
      IF(IX.NE.0) GO TO 300                                             
      IX=1                                                              
      ITEMP=K1                                                          
      K1=K2                                                             
      K2=ITEMP                                                          
      ATEMP=FK1                                                         
      FK1=FK2                                                           
      FK2=ATEMP                                                         
      ATEMP=COS1                                                        
      COS1=COS2                                                         
      COS2=ATEMP                                                        
      ATEMP=SIN1                                                        
      SIN1=SIN2                                                         
      SIN2=ATEMP                                                        
      IF(L1.EQ.L2) GO TO 280                                            
      GO TO 120                                                         
  300 CONTINUE                                                          
      SUMR=SUMR+WR*SUMX*RHO*RHO                                         
  400 CONTINUE                                                          
      SUMR=SUMR*FNORM*OPT                                               
      FR(MK)=FR(MK)+SUMR                                                
      IF(M.EQ.KM) SL=SUMR                                               
  500 CONTINUE                                                          
 1000 CONTINUE                                                          
      RETURN                                                            
      END
