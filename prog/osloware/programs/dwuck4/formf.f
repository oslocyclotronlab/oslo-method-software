      
      SUBROUTINE FORMF(U,V,N,LAM)                                       
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LDEPEN/ ULD(800,2),ILDSAV(8),ILD,IDSOSV(8),IDSO           
      COMMON /ISOSPN/ TWBF(800),TT,TR,CANAL,CCORE                       
      DIMENSION U(800),V(800),G(4)                                      
      EQUIVALENCE (G(1),FN)                                             
      DATA ETA6,PI/60.0,3.141593/                                       
C                                                                       
C     READ IN CARD SET 5,6,OR 7   ENERGY CARD                           
C                                                                       
      READ (5,9000)E,FM(N),Z(N),FMA(N),ZA(N),RY,AC(N),PNLOC(N),FS(N),QCD
      IF(N.NE.2) GO TO 6                                                
      Q=E                                                               
      IF(QCD.GT.0.0) GO TO 6                                            
      IF(QCD.LT.0.0) Q=Q+QCD                                            
      E=(FM(2)+FMA(2))*(ECM(1)+Q)/FMA(2)                                
    6 CONTINUE                                                          
      IF(N.NE.3) GO TO 8                                                
      E=E+QCD                                                           
    8 CONTINUE                                                          
      IS(N)=FS(N)                                                       
      NS(N)=IS(N)+1                                                     
      IF(AMASS.EQ.0.0) AMASS=FMA(1)                                     
      AFACT=FMA(N)**.333333333                                          
      BFACT=FM (N)**.333333333                                          
      RC(N)=ABS(RY)*AFACT                                               
      IF(RY.LT.0.0) RC(N)=RC(N)+ABS(RY)*BFACT                           
      DR(N)=DRF*AMASS/FMA(N)                                            
      DO 12 M=1,800                                                     
      U(M)=0.0                                                          
      V(M)=0.0                                                          
      IF(N.GE.3) GO TO 12                                               
      ULD(M,N)=0.0                                                      
   12 CONTINUE                                                          
      IF(E.EQ.0.0) GO TO 66                                             
      IF(ICON(10).EQ.0) GO TO 30                                        
      IF(ICON(10).EQ.2) GO TO 30                                        
C                                                                       
C     ICON(10).NE.0 GIVES RELATIVISTIC KINEMATICS                       
C                                                                       
      FM1=FM(N)*AMU                                                     
      FM2=FMA(N)*AMU                                                    
      FMT=FM1+FM2                                                       
      IF(N.NE.2) GO TO 26                                               
      IF(QCD.GT.0.0) GO TO 26                                           
      E=E+(ECM(1)+Q)**2/(2.0*FM2)                                       
   26 CONTINUE                                                          
      IF(N.EQ.3) E=E*(0.5*E+FMT)/FM2                                    
      T1=SQRT(2.0*E*FM2+FMT*FMT)                                        
      W1=(FMT*FM1+FM2*E)/T1                                             
      W2=FM2*(E+FMT)/T1                                                 
      FMU(N)=W1*W2/(W1+W2)                                              
      ECM(N)=T1-FMT                                                     
      FACT=2.0*FMU(N)/(HBARC*HBARC)                                     
      FMU(N)=FMU(N)/AMU                                                 
      FK2(N)=(E*E+2.0*E*FM1)*(FM2/T1)**2/(HBARC*HBARC)                  
      GO TO 36                                                          
   30 CONTINUE                                                          
      FMU(N)=FM(N)*FMA(N)/(FM(N)+FMA(N))                                
      ECM(N)=FMU(N)*E/FM(N)                                             
      IF(N.EQ.3) ECM(N)=E                                               
      FACT=2.0*FMU(N)*AMU/(HBARC*HBARC)                                 
      FK2(N)=FACT*ECM(N)                                                
   36 CONTINUE                                                          
      FK(N)=SQRT(ABS(FK2(N)))                                           
      ETAK=CHGSQ*Z(N)*ZA(N)*FACT                                        
      ETA(N)=ETAK*0.5/FK(N)                                             
C                                                                       
C     ADD COULOMB AND KINETIC ENERGIES TO U                             
C                                                                       
      RCX=RC(N)                                                         
      IF(RCX.EQ.0.0) RCX=DR(N)                                          
      F1=0.5*ETAK/RCX                                                   
      RC2=RCX*RCX                                                       
      R=0.0                                                             
      DO 50 M=1,400                                                     
      R=R+DR(N)                                                         
      MK=M+M-1                                                          
      IF(R.GE.RCX) GO TO 40                                             
      F2=F1*(3.0-R*R/RC2)                                               
      GO TO 43                                                          
   40 CONTINUE                                                          
      F2=ETAK/R                                                         
   43 CONTINUE                                                          
      IF(N.NE.3) GO TO 46                                               
      U(MK+1)=FK2(N)-F2                                                 
      GO TO 50                                                          
   46 CONTINUE                                                          
      U(MK  )=FK2(N)-F2                                                 
   50 CONTINUE                                                          
      GO TO 67                                                          
   66 CONTINUE                                                          
      FK(N)=0.0                                                         
      ETA(N)=0.0                                                        
      ECM(N)=0.0                                                        
   67 CONTINUE                                                          
      IF((N.EQ.3).AND.(ICON(4).GE.3)) GO TO 69                          
      WRITE(6,9010)N                                                    
      WRITE(6,9503)E,RY,AC(N),FS(N)                                     
      WRITE(6,9504)FM(N),FMA(N)                                         
      WRITE(6,9505)Z(N),ZA(N),PNLOC(N)                                  
      WRITE(6,9500)                                                     
      RHO=FK(N)*RC(N)                                                   
      WRITE(6,9506)ECM(N),RC(N),RHO,FMU(N)                              
      WRITE(6,9507)FK(N),ETA(N),DR(N)                                   
      WRITE(6,9008)                                                     
   69 CONTINUE                                                          
      FS(N)=FS(N)/2.0                                                   
      IF(N.NE.3) GO TO 80                                               
      IF(ICON(19).LT.4) GO TO 80                                        
      DO 75 M=1,800                                                     
      U(M)=0.0                                                          
      TWBF(M)=0.0                                                       
   75 CONTINUE                                                          
      READ(9) K,CANAL,CCORE,(TWBF(2*M-1),M=1,K),(U(2*M-1),M=1,K)        
      WRITE(6,9600) K,CANAL,CCORE                                       
      GO TO 3000                                                        
   80 CONTINUE                                                          
      IBF(1)=0                                                          
      CALL POTEN(U,V,E,AFACT,BFACT,FACT,RM,N,LAM)                       
      IF(N.NE.3) GO TO 3000                                             
      IF(E.EQ.0.0) GO TO 3000                                           
      IF(IBF(1).NE.0) GO TO 3000                                        
      IBF(4)=0                                                          
C                                                                       
C     SINGL PARTICLE ORBITAL                                            
C                                                                       
C                                                                       
C     READ IN QUANTUM NUMBERS FOR SINGLE PARTICLE ORBITAL               
C                                                                       
      READ(5,9000)G,VTRIAL,FISW,DAMP                                    
      ISW=FISW                                                          
      WRITE(6,9500)                                                     
      FJ=FJ2/2.0                                                        
      IF(VTRIAL.EQ.0.0) VTRIAL=ETA6                                     
      WRITE(6,9508)G,VTRIAL,FISW                                        
      IF(DAMP.NE.0.0) WRITE(6,9511) DAMP                                
      FSS=FSS/2.0                                                       
      FACT=(FJ*FJ+FJ-FL*FL-FL-FSS*FSS-FSS)*0.5                          
      DO 2028 M=1,400                                                   
      MK=M+M-1                                                          
      U(MK+1)=U(MK+1)+V(MK+1)*FACT                                      
      V(M)=V(MK  )*FACT                                                 
      U(MK  )=U(MK  )+V(M)                                              
 2028 CONTINUE                                                          
      WRITE(6,9500)                                                     
      IF(RM.EQ.0.0) RM=1.20*AFACT                                       
      CALL BIND(U,  V(401),DR(3),RM,FN,FL,K,FK(3),ETA(3),VTRIAL,ECM(3)  
     &,FK2(3)    ,ISW,IBF(3))                                           
      IBF(2)=RM/DR(3)                                                   
      FACT=PNLOC(3)**2/4.0                                              
C                                                                       
C     NON-LOCAL CORRECTION FOR SINGLE PARTICLE FUNCTION                 
C                                                                       
      SUM=0.0                                                           
      R=0.0                                                             
      DO 2075 M=1,K                                                     
      MK=M+M-1                                                          
      R=R+DR(3)                                                         
      U(MK)=U(MK)*VTRIAL+U(MK+1)                                        
      TEMP=FACT*(U(MK)-FK2(3))                                          
      IF(PNLOC(3)) 2072,2075,2070                                       
 2070 CONTINUE                                                          
      V(M+400)=V(M+400)/SQRT(1.0+TEMP)                                  
      GO TO 2074                                                        
 2072 CONTINUE                                                          
      V(M+400)=V(M+400)*EXP(-TEMP/2.0)                                  
 2074 CONTINUE                                                          
      SUM=SUM+(V(M+400)*R)**2                                           
 2075 CONTINUE                                                          
      IF(FACT.EQ.0.0) SUM=1.0                                           
      IF(FACT.EQ.0.0) GO TO 2076                                        
      SUM=1.0/SQRT(SUM*DR(3))                                           
 2076 CONTINUE                                                          
      IF(DAMP.EQ.0.0) GO TO 2090                                        
C                                                                       
C     APPLY DAMPING FACTOR, EXP(-DAMP*R*R) TO FORM FACTOR               
C                                                                       
      R=0.0                                                             
      H=ABS(DAMP)                                                       
      DO 2085 M=1,K                                                     
      R=R+DR(3)                                                         
      F2=H*R*R                                                          
      F2=-AMIN1(F2,30.0)                                                
      F1=EXP(F2)                                                        
      V(M+400)=V(M+400)*F1                                              
 2085 CONTINUE                                                          
 2090 CONTINUE                                                          
      DO 2100 M=1,400                                                   
      MK=M+M-1                                                          
      V(M)=U(MK)                                                        
      IF(M.LE.K) GO TO 2095                                             
      V(M)=0.0                                                          
      V(M+400)=0.0                                                      
 2095 CONTINUE                                                          
      U(MK  )=V(M+400)*SUM                                              
      U(MK+1)=0.0                                                       
 2100 CONTINUE                                                          
 3000 CONTINUE                                                          
      RETURN                                                            
 9000 FORMAT(10F8.4)                                                    
 9008 FORMAT(21H0POTENTIAL PARAMETERS)                                  
 9010 FORMAT( 9H0PARTICLE,I2,120(1H*))                                  
 9500 FORMAT(1H )                                                       
 9503 FORMAT(18H INPUT DATA       ,9H   ELAB =,F9.4,9H   RC0  =,F9.4,9H 
     &  AC   =,F9.4,9H   2*STR=,F9.4)                                   
 9504 FORMAT(18X,9H   MASSP=,F9.4,9H   MASST=,F9.4)                     
 9505 FORMAT(18X,9H   ZP   =,F9.4,9H   ZT   =,F9.4,9H   PNLOC=,F9.4)    
 9506 FORMAT(18H DERIVED DATA     ,9H   ECM  =,F9.4,9H   RC   =,F9.4,9H 
     &  RHO  =,F9.4,9H   FMU  =,F9.4)                                   
 9507 FORMAT(18X,9H   K    =,F9.4,9H   ETA  =,F9.4,9H   DR   =,F9.4)    
 9508 FORMAT(18X,9H   NODES=,F9.4,9H   L    =,F9.4,9H   2*J  =,F9.4,9H  
     & 2*S  =,F9.4,9H   VTRL =,F9.4,9H   FISW =,F9.4)                   
 9511 FORMAT(18X,9H   DAMP = ,F9.4)                                     
 9600 FORMAT(1H0,10X,4HK = ,I3,5X,12HCANALOGUE = ,F7.5,5X,8HCCORE = ,F7.
     &5)                                                                
      END
