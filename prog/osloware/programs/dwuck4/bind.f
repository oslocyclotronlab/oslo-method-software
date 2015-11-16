      
CDW$EOD

CDW$DECK DWLIB2.FOR

      SUBROUTINE BIND(U,F,DR,RM,FNODE,FL,K,FK,ETA,V,E,FK2,ISW,IERR)     
C     THE NEXT 5 CARDS CONVERT CHUCK TO AN IBM 360                      
C     IMPLICIT REAL*8(A-H,O-Z)                                          
C     SQRT(XXX)=DSQRT(XXX)                                              
C     FLOAT(III)=DFLOAT(III)                                            
C     ABS(XXX)=DABS(XXX)                                                
C     EXP(XXX)=DEXP(XXX)                                                
      DIMENSION U(*),F(*)                                               
      DATA ICMAX/16/                                                    
      DATA EPS,FLAMX,DETX/1.0E-06,1.0E-04,1.0E-03/                      
      DATA PI,UMAX,UMAXSQ/3.141593,1.0E+16,1.0E+32/                     
C                                                                       
      ICNT=0                                                            
      FLP=FL*(FL+1.0)                                                   
      DRHO=FK*DR                                                        
      FACT=DRHO*DRHO                                                    
      FACT=0.7*FACT                                                     
      IF(FK2.LT.0.0) FACT=-FACT                                         
      BE1=(5.0-FACT)/6.0                                                
      BE2=(1.0+FACT)/12.0                                               
      AL1=(BE1+2.0*BE2)/BE2                                             
      AL2=AL1-2.0                                                       
      DR2=DR*DR*BE2                                                     
      MT=K+K-1                                                          
      LL=FL+1.0                                                         
C                                                                       
C     CALCULATE OUTER BOUNDARY CONDITION                                
C                                                                       
   10 CONTINUE                                                          
      ICNT=ICNT+1                                                       
      RNORM=0.0                                                         
      R=DR*FLOAT(K)                                                     
      IF(FK2.LT.0.0) GO TO 23                                           
      M=MAX0(LL+1,3)                                                    
      T3=FK*(R-DR)                                                      
      T4=FK* R                                                          
      R1=T3*1.01                                                        
      R2=T4*1.01                                                        
      ETAP=ETA/1.01                                                     
      FLL=FL+1.0                                                        
      CALL COU(R1,R2,ETAP,M,DR,F( 1),F(21),F(41),F(61),F(81))           
      SURF=1.01*FK*(SQRT(FLL*FLL+ETAP*ETAP)*F(LL+61)/F(LL+60))/FLL      
      R1=0.99*T3                                                        
      R2=0.99*T4                                                        
      ETAP=ETA/0.99                                                     
      CALL COU(R1,R2,ETAP,M,DR,F( 1),F(21),F(41),F(61),F(81))           
      SURF=0.99*FK*(SQRT(FLL*FLL+ETAP*ETAP)*F(LL+61)/F(LL+60))/FLL-SURF 
      R1=T3                                                             
      R2=T4                                                             
      CALL COU(R1,R2,ETA ,M,DR,F( 1),F(21),F(41),F(61),F(81))           
      T6=F(LL+60)                                                       
      T5=F(LL+40)                                                       
      T4=F(LL+20)                                                       
      T3=F(LL   )                                                       
      SURF=SURF*25.0*T6*T6/FK2                                          
      IF(ISW.NE.2) GO TO 40                                             
      KM=K                                                              
      GO TO 120                                                         
   23 CONTINUE                                                          
      T3=FLP/(R-DR)**2-U(MT-1)-V*U(MT-2)                                
      T4=FLP/ R    **2-U(MT+1)-V*U(MT  )                                
      T3=SQRT(T3)                                                       
      T4=SQRT(T4)                                                       
      T5=DR*(T3+T4)/2.0                                                 
      T5=SQRT(T4/T3)*EXP(T5)                                            
      T5=T5*EPS                                                         
      T6=EPS                                                            
   40 FNCT=0.0                                                          
      KT=K-2                                                            
C                                                                       
C     INTEGRATE FROM INFINITY TO CLASSICAL TURNING POINT                
C                                                                       
      IFLAG=0                                                           
   41 CONTINUE                                                          
      R=DR*FLOAT(K-1)                                                   
      G6=U(MT+1)-FLP/(R+DR)**2                                          
      G5=U(MT-1)-FLP/ R    **2                                          
      IF(FK2.LT.0.0) GO TO 46                                           
      IF(G5.LT.0.0) IFLAG=IFLAG+1                                       
   46 CONTINUE                                                          
      Q6=1.0+DR2*G6                                                     
      Q5=1.0+DR2*G5                                                     
      F6=T6                                                             
      F5=T5                                                             
      W2=0.0                                                            
      FNORM2=0.0                                                        
      F(K  )=F6                                                         
      F(K-1)=F5                                                         
      DO 100 M=1,KT                                                     
      MM=K-M-1                                                          
      MK=MM+MM-1                                                        
      R=R-DR                                                            
      G4=U(MK+1)+V*U(MK  )-FLP/R**2                                     
      Q4=1.0+DR2*G4                                                     
      F4=((AL1-AL2*Q5)*F5-Q6*F6)/Q4                                     
      Q6=Q5                                                             
      Q5=Q4                                                             
      F6=F5                                                             
      F5=F4                                                             
      G6=G5                                                             
      G5=G4                                                             
      F(MM)=F4                                                          
      IF(G6*G5.GT.0.0) GO TO 90                                         
      IFLAG=IFLAG+1                                                     
      IF(FK2.LT.0.0) GO TO 110                                          
      IF(IFLAG.GE.2) GO TO 110                                          
   90 CONTINUE                                                          
      TEMP=F6*F6                                                        
      IF(TEMP.LT.1.E-18) GO TO 100                                      
      FNORM2=FNORM2+TEMP                                                
      W2=W2+U(MK+2)*TEMP                                                
      IF(TEMP.LE.UMAXSQ) GO TO 100                                      
      RNORM=RNORM+1.0                                                   
      FNORM2=FNORM2/UMAXSQ                                              
      W2=W2/UMAXSQ                                                      
      F5=F5/UMAX                                                        
      F6=F6/UMAX                                                        
      MP=M+2                                                            
      DO 95 I=1,MP                                                      
      MPP=K+1-I                                                         
      IF(ABS(F(MPP)).GE.1.0/UMAX) GO TO 92                              
      F(MPP)=0.0                                                        
      GO TO 95                                                          
   92 CONTINUE                                                          
      F(MPP)=F(MPP)/UMAX                                                
   95 CONTINUE                                                          
  100 CONTINUE                                                          
      IF(IFLAG.GE.2) GO TO 110                                          
      IFLAG=2                                                           
      KT=RM/DR+1.0                                                      
      KT=K-KT-1                                                         
      GO TO 41                                                          
  110 CONTINUE                                                          
      KM=MM+1                                                           
C                                                                       
C     INTEGRATE FROM ORIGIN TO CLASSICAL TURNING POINT                  
C                                                                       
  120 CONTINUE                                                          
      KS=FL/3.4+2.0                                                     
      W1=0.0                                                            
      FNORM1=0.0                                                        
      F2=0.0                                                            
      Q2=0.0                                                            
      R =0.0                                                            
      DO 200 M=1,KM                                                     
      MK=M+M-1                                                          
      R=R+DR                                                            
      Q3=1.0+DR2*(U(MK+1)+V*U(MK  )-FLP/R**2)                           
      IF(M.GT.KS) GO TO 150                                             
      F3=R**LL                                                          
      GO TO 151                                                         
  150 CONTINUE                                                          
      F3=((AL1-AL2*Q2)*F2-Q1*F1)/Q3                                     
  151 CONTINUE                                                          
      Q1=Q2                                                             
      Q2=Q3                                                             
      F1=F2                                                             
      F2=F3                                                             
      F(M)=F3                                                           
      TEMP=F2*F2                                                        
      FNORM1=FNORM1+TEMP                                                
      W1=W1+U(MK)*TEMP                                                  
      IF(F1*F2.LT.0.0) FNCT=FNCT+1.0                                    
      IF(TEMP.LE.UMAXSQ) GO TO 200                                      
      FNORM1=FNORM1/UMAXSQ                                              
      W1=W1/UMAXSQ                                                      
      F1=F1/UMAX                                                        
      F2=F2/UMAX                                                        
      DO 195 I=1,M                                                      
      IF(ABS(F(I)).GE.1.0/UMAX) GO TO 192                               
      F(I)=0.0                                                          
      GO TO 195                                                         
  192 CONTINUE                                                          
      F(I)=F(I)/UMAX                                                    
  195 CONTINUE                                                          
  200 CONTINUE                                                          
      IF(ISW.EQ.2) GO TO 650                                            
      DET=(F1*F6-F5*F2)/(F2*F6*DR)                                      
      FN=FNODE-FNCT                                                     
      RM=FLOAT(KM)*DR                                                   
      FNORM1=FNORM1/(F2*F2)                                             
      FNORM2=FNORM2/(F6*F6)                                             
      FNORM=FNORM1+FNORM2                                               
      IF(ICNT.EQ.ICMAX) GO TO 600                                       
      IF(ISW.EQ.1) GO TO 451                                            
C                                                                       
C     CHOOSE NEXT GUESS ON WELL DEPTH                                   
C                                                                       
      IF(FN.EQ.0.0) GO TO 280                                           
      FLAM=1.0+ABS(E)*3.0*FN/(V*FK*RM)                                  
      GO TO 382                                                         
  280 FLAM=1.0-DET/(V*DR*(W1/(F2*F2)+W2/(F6*F6)))                       
  382 CONTINUE                                                          
      IF((ABS(DET).LE.DETX).AND.(ABS(FLAM-1.0).LE.FLAMX)) GO TO 500     
      IF(FLAM.GT.1.2) FLAM=1.2                                          
      IF(FLAM.LT.0.8) FLAM=0.8                                          
      V=V*FLAM                                                          
      GO TO 10                                                          
C                                                                       
C     CHOOSE NEXT GUESS ON BINDING ENERGY                               
C                                                                       
  451 CONTINUE                                                          
      IF(FN.EQ.0.0) GO TO 480                                           
      FLAM=1.0+ABS(E)*3.0*FN/(E*FK*RM)                                  
      GO TO 482                                                         
  480 CONTINUE                                                          
      FLAM=1.0-DET/(DR*FK2*FNORM1)                                      
  482 CONTINUE                                                          
      IF((ABS(DET).LE.DETX).AND.(ABS(FLAM-1.0).LE.FLAMX)) GO TO 500     
      IF(FLAM.GT.1.2) FLAM=1.2                                          
      IF(FLAM.LT.0.8) FLAM=0.8                                          
      TEMP=SQRT(FLAM)                                                   
      FK=FK*TEMP                                                        
      ETA=ETA/TEMP                                                      
      TEMP=FK2*FLAM-FK2                                                 
      FK2=FK2+TEMP                                                      
      E=E*FLAM                                                          
      DO 485 M=1,K                                                      
      MK=M+M-1                                                          
      U(MK+1)=U(MK+1)+TEMP                                              
  485 CONTINUE                                                          
      GO TO 10                                                          
C                                                                       
C     NORMALIZE FUNCTION                                                
C     FOR E .GT. 0., NORMALIZE TO VINCENT AND FORTUNE EQ 7 OR 30        
C                                                                       
  500 CONTINUE                                                          
      WRITE(6,9500) V,DET,FNCT,RM,E,ICNT                                
      VOL=FNORM*DR*F6*F6                                                
      FNORM=SQRT(FNORM*DR)                                              
      IF(FK2.LT.0.0) GO TO 505                                          
      RNORM=UMAX**IFIX(RNORM+0.01)                                      
      TEMP=VOL+SURF                                                     
      IF(RNORM.GT.2.0) TEMP=VOL                                         
      GAM=2.0*E/(FK*TEMP)                                               
      VOL=VOL*RNORM*RNORM                                               
      WRITE(6,9502) GAM,VOL,SURF                                        
      FNORM=SQRT(ABS(TEMP))/F6                                          
  505 CONTINUE                                                          
      TEMP=1.0/(F2*FNORM)                                               
      R=0.0                                                             
      DO 510 M=1,KM                                                     
      R=R+DR                                                            
      F(M)=F(M)*TEMP/R                                                  
  510 CONTINUE                                                          
      KM=KM+1                                                           
      TEMP=1.0/(F6*FNORM)                                               
  515 CONTINUE                                                          
      DO 520 M=KM,K                                                     
      R=R+DR                                                            
      F(M)=F(M)*TEMP/R                                                  
  520 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C     ERROR MESSAGE                                                     
C                                                                       
  600 WRITE(6,9001) ICMAX                                               
      IERR=1                                                            
      GO TO 500                                                         
C                                                                       
C     NORM TO SIN(K*R+DELTA)/(K*R)                                      
C                                                                       
  650 CONTINUE                                                          
      DET=T3*T6-T4*T5                                                   
      A1=(F1*T6-F2*T5)/DET                                              
      B1=(F2*T3-F1*T4)/DET                                              
      DET=1.0/SQRT(A1*A1+B1*B1)                                         
      A1=A1*DET                                                         
      B1=B1*DET                                                         
      TEMP=DET*SQRT(FK/(PI*E))                                          
      WRITE(6,9501)A1,B1                                                
      KM=1                                                              
      R=0.0                                                             
      GO TO 515                                                         
 9001 FORMAT(1H0,28HBOUND STATE SEARCH FAILS IN ,I2,11H ITERATIONS)     
 9500 FORMAT(21X,6HV    =,F9.4,3X,6HDET  =,F9.4,3X,6HNODES=,F9.4,3X,    
     & 6HRM   =,F9.4,3X,6HE    =,F9.4,3X,6HITER.=,I4)                   
 9501 FORMAT(21X,6HCOSD =,F9.4,9H   SIND =,F9.4)                        
 9502 FORMAT(31H0UNBOUND FORM FACTOR    GAMMA =,E12.4,15H MEV      VOL =
     &,E12.4,10H    SURF =,E12.4)                                       
      END
