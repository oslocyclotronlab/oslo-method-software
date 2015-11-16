      
      SUBROUTINE TAILYY(NMAX,RMAX,YMAX,FL,FK,E,UP)                      
C                                                                       
C     COMPUTES WAVE FUNCTIONS FOR HEAVY-MASS PARTICLE FROM Y=YMAX TO    
C     Y=0.0 AT X=RMAX.                                                  
C                                                                       
      COMPLEX S(2),T(2),RHO(2),UYM(2),TN,AI,FE1,FE2,OVZ,ZODZ            
      DIMENSION UP(2),YU(2),Y(4),B1(2),B2(2),B3(2)                      
      DATA ERR,AI/1.0E-06,(0.0,1.0)/                                    
C                                                                       
      P=FK*FK                                                           
      Q=FL*(FL+1.0)                                                     
      R=2.0*E*FK                                                        
      FLPIH=FL*3.1415927/2.0                                            
      DELK=YMAX/FLOAT(NMAX)                                             
      DRR2=-DELK*DELK/12.0                                              
      RLIM=SQRT(Q+E*E)                                                  
      YM=AMAX1(1.5*YMAX,2.0*RLIM/FK)                                    
      NCMAX=YM/DELK                                                     
      CYMAX=FLOAT(NCMAX)*DELK                                           
C                                                                       
C     COMPUTE ASYMPTOTIC VALUES                                         
C                                                                       
      S(1)=(1.0,0.0)                                                    
      S(2)=(1.0,0.0)                                                    
      T(1)=(1.0,0.0)                                                    
      T(2)=(1.0,0.0)                                                    
      RHO(1)=FK*CMPLX(RMAX,CYMAX)                                       
      RHO(2)=FK*CMPLX(RMAX,CYMAX-DELK)                                  
      ZODZ=RHO(1)/RHO(2)                                                
      FE1=FL+1.0-AI*E                                                   
      FE2=FL+AI*E                                                       
      OVZ=0.5*AI/RHO(1)                                                 
      XI=1.0                                                            
      TN=T(1)                                                           
      DO 15 I=1,40                                                      
      TN=(FE1-XI)*(FE2+XI)*OVZ/XI                                       
      T(1)=T(1)*TN                                                      
      T(2)=T(2)*TN*ZODZ                                                 
      S(1)=S(1)+T(1)                                                    
      S(2)=S(2)+T(2)                                                    
      XI=XI+1.0                                                         
      IF(I.LT.6) GO TO 15                                               
      DET=CABS(T(1)/S(1))                                               
      IF(DET.LT.ERR) GO TO 20                                           
   15 CONTINUE                                                          
      WRITE(6,900) FL,DET                                               
   20 CONTINUE                                                          
      XI=0.0                                                            
      FAC=1.0                                                           
      DET=AIMAG(RHO(1))                                                 
      IF(DET.LT.50.0) GO TO 25                                          
      I=DET-50.0                                                        
      XI=I                                                              
      FAC=EXP(-XI)                                                      
   25 CONTINUE                                                          
      DO 30 I=1,2                                                       
      UYM(I)=S(I)*CEXP(AI*(RHO(I)-E*CLOG(2.0*RHO(I))-FLPIH)+XI)         
   30 CONTINUE                                                          
C                                                                       
C     INTEGRATE SCHROEDINGER EQUATION                                   
C                                                                       
      Y(1)= REAL(UYM(1))                                                
      Y(2)=AIMAG(UYM(1))                                                
      Y(3)= REAL(UYM(2))                                                
      Y(4)=AIMAG(UYM(2))                                                
      NP1 =NMAX+1                                                       
      NCMAX1=NCMAX+1                                                    
      NCMAX2=NCMAX+2                                                    
      ZR=RMAX                                                           
      ZI=CYMAX+DELK                                                     
      ZR2=ZR*ZR                                                         
      TWOX=-2.0*ZR                                                      
      RZR=R*ZR                                                          
      YU(1)=Y(3)                                                        
      YU(2)=Y(4)                                                        
      DO 100 I=1,NCMAX1                                                 
      II=I+I-1                                                          
      ZI=ZI-DELK                                                        
      ZI2=ZI*ZI                                                         
      DET=ZR2+ZI2                                                       
      DET2=DET*DET                                                      
      GR=Q*(ZR2-ZI2)/DET2 + RZR /DET - P                                
      GI=Q* TWOX*ZI /DET2 - R*ZI/DET                                    
      FACR=1.0-DRR2*GR                                                  
      FACI=   -DRR2*GI                                                  
      DET=FACR*FACR+FACI*FACI                                           
      IF(I.GT.2) GO TO 50                                               
      B2(1)=Y(II  )*FACR-Y(II+1)*FACI                                   
      B2(2)=Y(II+1)*FACR+Y(II  )*FACI                                   
      IF(I.EQ.2) GO TO 100                                              
      B1(1)=B2(1)                                                       
      B1(2)=B2(2)                                                       
      GO TO 100                                                         
   50 CONTINUE                                                          
      B3(1)=12.0*YU(1)-10.0*B2(1)-B1(1)                                 
      B3(2)=12.0*YU(2)-10.0*B2(2)-B1(2)                                 
      YU(1)=(B3(1)*FACR+B3(2)*FACI)/DET                                 
      YU(2)=(B3(2)*FACR-B3(1)*FACI)/DET                                 
      M=NCMAX2-I                                                        
      IF(M.GT.NP1) GO TO 80                                             
      MM=M+M-1                                                          
      UP(MM  )=FAC*YU(1)                                                
      UP(MM+1)=FAC*YU(2)                                                
   80 CONTINUE                                                          
      B1(1)=B2(1)                                                       
      B1(2)=B2(2)                                                       
      B2(1)=B3(1)                                                       
      B2(2)=B3(2)                                                       
  100 CONTINUE                                                          
      RETURN                                                            
  900 FORMAT(53H ASYMPTOTIC SERIES DOES NOT CONVERGE IN TAILYY FOR L=,F5
     &.0,11H  AT RATIO ,E10.3)                                          
      END
