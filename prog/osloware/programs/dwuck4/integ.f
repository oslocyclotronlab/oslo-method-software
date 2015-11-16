      
      SUBROUTINE INTEG                                                  
      DOUBLE PRECISION BETA                                             
      DIMENSION F(150),FP(150),G(150),GP(150),S(150)                    
     &         ,C(600),D(600),E(600),CPHAS(600),ALP1(2),ALP2(2),BT2(2)  
     &         ,Q(4),A(2),B(2),CTEMP(2),DRR2(2),DR2(2),R(2),QLD(2)      
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  U(800,2),V(800,2),F1(600),F2(600),Q1(600),Q2(600) 
     &               ,Q3(600),KMIN(150,2)                               
      COMMON /LDEPEN/ ULD(800,2),ILDSAV(8),ILD,IDSOSV(8),IDSO           
      COMMON /UBOUND/ DY,YMAX,EISIG0(2)                                 
      EQUIVALENCE (CPHAS(1),U(1,2))                                     
      EQUIVALENCE (D (1),F1(1)), (E (1),F2(1))                          
     &           ,(F (1),Q1(1)), (FP(1),Q1(301))                        
     &           ,(G (1),Q2(1)), (GP(1),Q2(301))                        
     &           ,(S (1),Q3(1)), (C (1),U(1,1))                         
      DATA ETA1,ETA2/1.0E-08,1.0E-01/                                   
C                                                                       
      IAMD=1                                                            
      IF(FK2(1).LT.FK2(2)) IAMD=2                                       
      JT=NS(1)+NS(2)                                                    
      NP=LPL2*JT                                                        
      DO 30 N=1,2                                                       
      DRHO=FK(N)*DR(N)                                                  
      FACT=DRH0*DRHO/20.0                                               
      FACT=0.7*FACT                                                     
      BE1=(5.0-FACT)/6.0                                                
      BE2=(1.0+FACT)/12.0                                               
      BT2(N)=BE2                                                        
      ALP1(N)=(BE1+2.0*BE2)/BE2                                         
      ALP2(N)=ALP1(N)-2.0                                               
      DR2(N)=BE2*DR(N)**2                                               
C                                                                       
C     COMPUTE STARTING RADII                                            
C                                                                       
      DO 20 LL=1,LPLUS                                                  
      FL=LL-1                                                           
      FLFACT=FL*FL+FL                                                   
      M1=SQRT(FLFACT/12.0)+1.0                                          
      EXPO=1.0/(2.0*FL+1.0)                                             
      M2=((1.0E-16)**EXPO)*(2.0*FL/(2.718*FK(N)*DR(N)))+1.0             
      KMIN(LL,N)=MAX0(M1,M2)                                            
   20 CONTINUE                                                          
   30 CONTINUE                                                          
      DO 40 IQ=1,NP                                                     
      F1(IQ)=0.0                                                        
      F2(IQ)=0.0                                                        
      Q1(IQ)=0.0                                                        
      Q2(IQ)=0.0                                                        
   40 CONTINUE                                                          
      DO 100 M=1,K                                                      
      MK=M+M-1                                                          
      IX=0                                                              
      DO 90 N=1,2                                                       
      R(N)=FLOAT(M)*DR(N)                                               
      DRR2(N)=BT2(N)/FLOAT(M*M)                                         
      AL1=ALP1(N)                                                       
      AL2=ALP2(N)                                                       
      Q(1)=1.0+DR2(N)* U(MK  ,N)                                        
      Q(2)=    DR2(N)* U(MK+1,N)                                        
      QLD(1)=DR2(N)*ULD(MK  ,N)                                         
      QLD(2)=DR2(N)*ULD(MK+1,N)                                         
      FI=-FS(N)                                                         
      JS=NS(N)                                                          
      SFACT=FS(N)**2+FS(N)                                              
      DO 89 ISS=1,JS                                                    
      FL=0.0                                                            
      DO 80 LL=1,LPLUS                                                  
      PHAS=PHASEF(LL-1)                                                 
      FJ=FL+FI                                                          
      IX1=IX+LL+LL-1                                                    
      FLFACT=FL*FL+FL                                                   
      IF(FJ.LT.ABS(FL-FS(N))) GO TO 72                                  
      MMIN=KMIN(LL,N)                                                   
      IF(M.LT.MMIN) GO TO 82                                            
      FACT=DR2(N)*(FJ*FJ+FJ-FLFACT-SFACT)*0.5                           
      Q3(IX1  )=Q(1)+FACT*V(MK  ,N)-DRR2(N)*FLFACT + PHAS*QLD(1)        
      Q3(IX1+1)=Q(2)+FACT*V(MK+1,N)                + PHAS*QLD(2)        
      IF(M.GT.MMIN) GO TO 70                                            
      F2(IX1  )=ETA1                                                    
      F2(IX1+1)=0.0                                                     
      Q2(IX1  )=ETA1*Q3(IX1  )                                          
      Q2(IX1+1)=ETA1*Q3(IX1+1)                                          
C                                                                       
C     EVALUATE Q AT ORIGIN FOR L=1                                      
C                                                                       
      IF(LL.EQ.2) Q2(IX+3)=-ETA1/6.0                                    
      GO TO 72                                                          
   70 CONTINUE                                                          
      CTEMP(1)=AL1*F2(IX1  )-AL2*Q2(IX1  )-Q1(IX1  )                    
      CTEMP(2)=AL1*F2(IX1+1)-AL2*Q2(IX1+1)-Q1(IX1+1)                    
      F1(IX1  )=F2(IX1  )                                               
      F1(IX1+1)=F2(IX1+1)                                               
      DET=Q3(IX1  )**2+Q3(IX1+1)**2                                     
      F2(IX1  )=(CTEMP(1)*Q3(IX1  )+CTEMP(2)*Q3(IX1+1))/DET             
      F2(IX1+1)=(CTEMP(2)*Q3(IX1  )-CTEMP(1)*Q3(IX1+1))/DET             
      Q1(IX1  )=Q2(IX1  )                                               
      Q1(IX1+1)=Q2(IX1+1)                                               
      Q2(IX1  )=CTEMP(1)                                                
      Q2(IX1+1)=CTEMP(2)                                                
   72 CONTINUE                                                          
      FL=FL+1.0                                                         
   80 CONTINUE                                                          
   82 CONTINUE                                                          
      FI=FI+1.0                                                         
      IX=IX+LPL2                                                        
   89 CONTINUE                                                          
   90 CONTINUE                                                          
C                                                                       
C     WRITE RADIAL WAVE FUNCTIONS ON TAPE 4                             
C                                                                       
      WRITE(4)(F2(IQ),IQ=1,NP)                                          
  100 CONTINUE                                                          
      IX=0                                                              
      DO 200 N=1,2
      R2=FK(N)*R(N)                                                     
      R1=R2-DR(N)*FK(N)
      CALL COU(R1,R2,ETA(N),LPLUS,ETA2,F,FP,G,GP,S)
      JS=NS(N)                                                          
      FI=-FS(N)                                                         
      RSIG(N)=0.0                                                       
      IF(IAMD.NE.N) GO TO 110                                           
      ARG=S(1)                                                          
      EISIG0(1)=COS(ARG)                                                
      EISIG0(2)=SIN(ARG)                                                
  110 CONTINUE
      DO 199 ISS=1,JS                                                   
      FL=0.0                                                            
      DO 190 LL=1,LPLUS
      FJ=FL+FI                                                          
      LK=LL+LL-1                                                        
      IX1=IX+LL+LL-1                                                    
      DET=F(LL)*GP(LL)-FP(LL)*G(LL)                                     
      A(1)=(F1(IX1  )*GP(LL)-F2(IX1  )*G (LL))/DET                      
      A(2)=(F1(IX1+1)*GP(LL)-F2(IX1+1)*G (LL))/DET                      
      B(1)=(F2(IX1  )*F (LL)-F1(IX1  )*FP(LL))/DET                      
      B(2)=(F2(IX1+1)*F (LL)-F1(IX1+1)*FP(LL))/DET                      
      GO TO 180                                                         
  170 CONTINUE                                                          
      CTEMP(1)=0.0                                                      
      CTEMP(2)=0.0                                                      
      Q(1)=0.0                                                          
      Q(2)=0.0                                                          
      GO TO 185                                                         
  180 CONTINUE                                                          
      IF(FJ.LT.ABS(FL-FS(N))) GO TO 170                                 
      ARG=S(LL)-S(1)                                                    
      Q(1)=COS(ARG)                                                     
      Q(2)=SIN(ARG)                                                     
      CTEMP(1)=A(1)+B(2)                                                
      CTEMP(2)=B(1)-A(2)                                                
      DET=CTEMP(1)**2+CTEMP(2)**2                                       
      CTEMP(1)=CTEMP(1)/DET                                             
      CTEMP(2)=CTEMP(2)/DET                                             
  185 CONTINUE                                                          
C                                                                       
C     C=NORMALIZATION CONSTANTS                                         
C                                                                       
      C(IX1  )=Q(1)*CTEMP(1)-Q(2)*CTEMP(2)                              
      C(IX1+1)=Q(1)*CTEMP(2)+Q(2)*CTEMP(1)                              
C                                                                       
C     E=PARTIAL WAVE SCATTERING AMPLITUDES                              
C                                                                       
      E(IX1  )=B(1)*CTEMP(1)-B(2)*CTEMP(2)                              
      E(IX1+1)=B(1)*CTEMP(2)+B(2)*CTEMP(1)                              
      Q(3)=Q(1)**2-Q(2)**2                                              
      Q(4)=2.0*Q(1)*Q(2)                                                
C                                                                       
C     D=PARTIAL WAVE SCATTERING AMPLITUDES* COULOMB PHASE / WAVE NUMBER 
C                                                                       
      D(IX1  )=(Q(3)*E(IX1  )-Q(4)*E(IX1+1))/FK(N)                      
      D(IX1+1)=(Q(3)*E(IX1+1)+Q(4)*E(IX1  ))/FK(N)                      
C                                                                       
C     TOTAL PHASE AMPLITUDES NEEDED FOR TAIL CALCULATION                
C                                                                       
      ARG=2.0*S(LL)                                                     
      A(1)=COS(ARG)                                                     
      A(2)=SIN(ARG)                                                     
      B(1)=1.0-2.0*E(IX1+1)                                             
      B(2)=    2.0*E(IX1  )                                             
      CPHAS(IX1  )=A(1)*B(1)-A(2)*B(2)                                  
      CPHAS(IX1+1)=A(1)*B(2)+A(2)*B(1)                                  
C                                                                       
C     CALCULATE REACTION SIGMA                                          
C                                                                       
      RSIG(N)=RSIG(N)+(FJ+FJ+1.0)*(E(IX1+1)-E(IX1  )**2-E(IX1+1)**2)    
      FL=FL+1.0                                                         
  190 CONTINUE                                                          
      FI=FI+1.0                                                         
      IX=IX+LPL2                                                        
  199 CONTINUE                                                          
      RSIG(N)=RSIG(N)*12.566371/((2.0*FS(N)+1.0)*FK(N)**2)              
  200 CONTINUE                                                          
      IX=MOD(ICON(19),4)                                                
      IF(IX.EQ.0) GO TO 220                                             
      IF(IX.GT.1) GO TO 205                                             
      NI=1                                                              
      NF=LPL2*NS(1)-1                                                   
      GO TO 210                                                         
  205 CONTINUE                                                          
      NI=LPL2*NS(1)+1                                                   
      NF=NP-1                                                           
  210 CONTINUE                                                          
      DO 215 N=NI,NF,2                                                  
      C(N  )=1.0                                                        
      C(N+1)=0.0                                                        
  215 CONTINUE                                                          
  220 CONTINUE                                                          
C                                                                       
C     WRITE NORMALIZATION CONSTANTS ON TAPE 4                           
C                                                                       
      WRITE(4)(C(I),I=1,NP)                                             
      WRITE(4)(CPHAS(I),I=1,NP)                                         
      END FILE 4                                                        
      REWIND 4                                                          
      RETURN
      END
