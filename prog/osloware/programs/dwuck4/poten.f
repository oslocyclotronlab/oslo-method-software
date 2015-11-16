      
      SUBROUTINE POTEN(U,V,E,AFACT,BFACT,FACT,RM,N,LAM)                 
      DOUBLE PRECISION BETA,B                                           
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LDEPEN/ ULD(800,2),ILDSAV(8),ILD,IDSOSV(8),IDSO           
      DIMENSION U(800),V(800),UT(5),G(4),ABETA(3),FLDF(3),LDFRM(3)      
      DIMENSION B(3,17),CN(16),YLAM(16),X(8),W(8),CP(16)                
      EQUIVALENCE (G(1),FN)                                             
      DATA B(1,1),B(2,1),B(3,1)/6H NX=1 ,6HVOLUME,6H W-S  /             
      DATA B(1,2),B(2,2),B(3,2)/6H NX=2 ,6HSURFAC,6HE W-S /             
      DATA B(1,3),B(2,3),B(3,3)/6H NX=3 ,6H2ND DE,6HRIV   /             
      DATA B(1,4),B(2,4),B(3,4)/6H NX=4 ,6HL.S VO,6HLUME  /             
      DATA B(1,5),B(2,5),B(3,5)/6H NX=5 ,6HL.S SU,6HRFACE /             
      DATA B(1,6),B(2,6),B(3,6)/6H NX=6 ,6HVOL*R*,6H*POWR /             
      DATA B(1,7),B(2,7),B(3,7)/6H NX=7 ,6HSUR*R*,6H*POWR /             
      DATA B(1,8),B(2,8),B(3,8)/6H NX=8 ,6HEXTERN,6H FORMF/             
      DATA B(1,9),B(2,9),B(3,9)/6H NX=9 ,6HHARMON,6HIC OSC/             
      DATA B(1,10),B(2,10),B(3,10)/6H NX=10,6H GAUSS,6HIAN   /          
      DATA B(1,11),B(2,11),B(3,11)/6H NX=11,6H DEFOR,6HM VOL /          
      DATA B(1,12),B(2,12),B(3,12)/6H NX=12,6H DEFOR,6HM SURF/          
      DATA B(1,13),B(2,13),B(3,13)/6H NX=13,6H WATSO,6HN L.S /          
      DATA B(1,14),B(2,14),B(3,14)/6H NX=14,6H VOL. ,6HEXCHNG/          
      DATA B(1,15),B(2,15),B(3,15)/6H NX=15,6H 1ST D,6HER EXC/          
      DATA B(1,16),B(2,16),B(3,16)/6H NX=16,6H 2ND D,6HER EXC/          
      DATA B(1,17),B(2,17),B(3,17)/6H NX= O,6HUT OF ,6HRANGE /          
C                                                                       
C     POINTS FOR GAUSS LEGENDRE INTEGRATION                             
C                                                                       
      DATA X/0.0950125098,0.2816035507,0.4580167776,0.6178762444,       
     &       0.7554044083,0.8656312023,0.9445750230,0.9894009439/       
C                                                                       
C     WEIGHTS FOR GAUSS LEGENDRE INTEGRATION                            
C                                                                       
      DATA W/0.1894506104,0.1826034150,0.1691565193,0.1495959888,       
     &       0.1246289712,0.0951585116,0.0622535239,0.0271524594/       
      DATA ETA4,ETA5/4.0,10.0/                                          
      DATA SQRPI/1.772453851/                                           
C     THE NEXT 1 STATEMENT FAKES OUT A BUG IN THE IBM 360 COMPILER,     
C       FORTRAN IV, LEVEL H, OPTIMIZER=2.  N IS ALWAYS LESS THAN 4.     
      IF (N.GT.10) GO TO 83                                             
      RM=0.0                                                            
      ILD=0                                                             
      ITM=0                                                             
C                                                                       
C     READ IN CARD SET 5,6,OR 7   POTENTIAL CARDS                       
C                                                                       
   70 CONTINUE                                                          
      ITM=ITM+1                                                         
      READ (5,9000)FZ,VR,RY,AR,VSOR,VI,RZ,AI,VSOI,PWR                   
      NX=ABS(FZ)                                                        
      IF(NX.LE. 0) NX=17                                                
      IF(NX.GT.17) NX=17                                                
      RR=ABS(RY)*AFACT                                                  
      RI=ABS(RZ)*AFACT                                                  
      IF(RY.LT.0.0) RR=RR+ABS(RY)*BFACT                                 
      IF(RY.LT.0.0) RI=RI+ABS(RZ)*BFACT                                 
      IF((N.EQ.3).AND.(ICON(4).GE.3)) GO TO 75                          
      WRITE(6,9509)(B(J,NX),J=1,3),VR,RY,AR,RR,VSOR                     
      WRITE(6,9510)                VI,RZ,AI,RI,VSOI,PWR                 
   75 CONTINUE                                                          
      RY=ABS(RY)                                                        
      RZ=ABS(RZ)                                                        
C      NX=1 VOLUME WOODS-SAXON                                          
C      NX=2 SURFACE WOODS-SAXON                                         
C      NX=3 SECOND DERIVATIVE WOODS-SAXON                               
C      NX=4 L.S POTENTIAL FOR WOODS-SAXON VOLUME                        
C      NX=5 L.S POTENTIAL FOR WOODS-SAXON SURFACE                       
C      NX=6 WOOD-SAXON*R**PWR                                           
C      NX=7 1ST-DER WOOD-SAXON*R**PWR                                   
C      NX=8 FORMF8 EXTERNAL FORM FACTOR                                 
C      NX=9 HARMONIC OSCILLATOR                                         
C      NX=10 GAUSSIAN*R**POWR                                           
C      NX=11 DEFORMED VOLUME                                            
C      NX=12 DEFORMED SURFACE                                           
C      NX=13 WATSON L.S POTENTIAL                                       
C      NX=14 VOLUME WOODS-SAXON, L-DEPENDENT EXCHANGE                   
C      NX=15 1ST DER. WOODS-SAXON, L-DEPENDENT EXCHANGE                 
C      NX=16 2ND DER. WOODS-SAXON, L-DEPENDENT EXCHANGE                 
      KFLAG=0                                                           
      IF(N-3)79,76,79                                                   
   76 CONTINUE                                                          
      IF(E)81,80,81                                                     
   79 CONTINUE                                                          
      VR=VR*FACT                                                        
      VI=VI*FACT                                                        
      KT=FK(N)*AMAX1(RR,RI)+ETA5                                        
      LPLUS=MAX0(LPLUS,KT)                                              
   80 KT=(2.3*ETA4*AMAX1(AR,AI)+AMAX1(RR,RI))/DR(N)                     
      GO TO 82                                                          
   81 CONTINUE                                                          
      RM=AMAX1(RR,RM)                                                   
      VR=VR*FACT                                                        
      VI=VI*FACT                                                        
      IF(E.GT.0.0) GO TO 80                                             
      ALPHB=2.3*ETA4                                                    
      TEMP=1.0+ALPHB/10.0                                               
      KT=((ALPHB-ETA(N)*TEMP)/FK(N)+RR)/DR(N)                           
   82 K=MIN0(MAX0(K,KT),400)                                            
   83 CONTINUE                                                          
      IF(AR.NE.0.0) GO TO 85                                            
      F1=0.0                                                            
      F2=0.                                                             
      GO TO 86                                                          
   85 F2=EXP(-DR(N)/AR)                                                 
      F1=EXP( RR/AR)                                                    
   86 CONTINUE                                                          
      IF(AI.NE.0.0) GO TO 95                                            
      F3=0.0                                                            
      F4=0.                                                             
      GO TO 96                                                          
   95 F4=EXP(-DR(N)/AI)                                                 
      F3=EXP( RI/AI)                                                    
   96 CONTINUE                                                          
      IF(N.NE.3 ) GO TO 98                                              
      IF(E.NE.0.0) GO TO 98                                             
      IF((NX.GT.5).AND.(NX.LT.13)) GO TO 98                             
      PWR=PWR+1.0                                                       
      IF(AR.NE.0.0) VR=VR*(RR/AR)**PWR                                  
      IF(AI.NE.0.0) VI=VI*(RI/AI)**PWR                                  
   98 CONTINUE                                                          
      IF(NX.GT.16) GO TO 2009                                           
      GO TO (100,200,300,400,500,600,700,800,900,1000,                  
     & 1100,1200,1300,1400,1500,1600),NX                                
C                                                                       
C     VOLUME WOODS SAXON                                                
C                                                                       
  100 CONTINUE                                                          
      DO 160 M=1,K                                                      
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      U(MK  )=U(MK  )-VR*F1/(1.0+F1)                                    
      U(MK+1)=U(MK+1)-VI*F3/(1.0+F3)                                    
  160 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     1ST DER WOODS SAXON                                               
C                                                                       
  200 CONTINUE                                                          
      DO 260 M=1,K                                                      
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      U(MK  )=U(MK  )+VR*F1/(1.0+F1)**2                                 
      U(MK+1)=U(MK+1)+VI*F3/(1.0+F3)**2                                 
  260 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     2ND DER WOODS SAXON                                               
C                                                                       
  300 CONTINUE                                                          
      DO 360 M=1,K                                                      
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      U(MK  )=U(MK  )-VR*F1*(1.0-F1)/(1.0+F1)**3                        
      U(MK+1)=U(MK+1)-VI*F3*(1.0-F3)/(1.0+F3)**3                        
  360 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     L.S VOLUME WOODS SAXON                                            
C                                                                       
  400 CONTINUE                                                          
      IF(AR.NE.0.0) VR=VR/AR                                            
      IF(AI.NE.0.0) VI=VI/AI                                            
      IF((VR.NE.0.0).OR.(VI.NE.0.0)) IBF(4)=1                           
      R=0.0                                                             
      DO 460 M=1,K                                                      
      R=R+DR(N)                                                         
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      V(MK  )=V(MK  )-VR*F1/(R*(1.0+F1)**2)                             
      V(MK+1)=V(MK+1)-VI*F3/(R*(1.0+F3)**2)                             
  460 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     L.S 1ST DER WOODS SAXON                                           
C                                                                       
  500 CONTINUE                                                          
      IF(AR.NE.0.0) VR=VR/AR                                            
      IF(AI.NE.0.0) VI=VI/AI                                            
      IF((VR.NE.0.0).OR.(VI.NE.0.0)) IBF(4)=1                           
      SPO=0.0                                                           
      IF(VSOR.GT.0.0) SPO=1.0                                           
      R=0.0                                                             
      DO 560 M=1,K                                                      
      R=R+DR(N)                                                         
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      TEMP=1.0+F1                                                       
      V(MK  )=V(MK  )+VR*F1*((1.0-F1)/TEMP+SPO*AR/R)/(R*TEMP*TEMP)      
      TEMP=1.0+F3                                                       
      V(MK+1)=V(MK+1)+VI*F3*((1.0-F3)/TEMP+SPO*AI/R)/(R*TEMP*TEMP)      
  560 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     WOOD SAXON*R**PWR                                                 
C                                                                       
  600 CONTINUE                                                          
      R=0.0                                                             
      DO 660 M=1,K                                                      
      R=R+DR(N)                                                         
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      RPWR=R**PWR                                                       
      U(MK  )=U(MK  )-VR*F1*RPWR/(1.0+F1)                               
      U(MK+1)=U(MK+1)-VI*F3*RPWR/(1.0+F3)                               
  660 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C      1ST DER WOOD SAXON*R**PWR                                        
C                                                                       
  700 CONTINUE                                                          
      R=0.0                                                             
      DO 760 M=1,K                                                      
      R=R+DR(N)                                                         
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      RPWR=R**PWR                                                       
      U(MK  )=U(MK  )+VR*F1*RPWR/(1.0+F1)**2                            
      U(MK+1)=U(MK+1)+VI*F3*RPWR/(1.0+F3)**2                            
  760 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     EXTERNAL FORM FACTOR                                              
C                                                                       
  800 CONTINUE                                                          
      READ(5,9000)F1,F2,F3                                              
      IF((N.EQ.3).AND.(ICON(4).GE.3)) GO TO 802                         
      WRITE(6,9513) F1,F2,F3                                            
  802 CONTINUE                                                          
      IF(N.NE.3) GO TO 810                                              
      IF(E.EQ.0.0) GO TO 810                                            
      IF(F3.EQ.0.0) GO TO 810                                           
      IBF(1)=1                                                          
      VR=VR/FACT                                                        
      VI=VI/FACT                                                        
      IF(ITM.GT.1) GO TO 810                                            
      DO 805 M=1,800                                                    
      U(M)=0.0                                                          
  805 CONTINUE                                                          
  810 CONTINUE                                                          
      F3=VR                                                             
      IGT=F2+1.0                                                        
      IF(MOD(IGT,2).EQ.0) F3=VI                                         
      IF(F3.EQ.0.0) F3=1.0                                              
      KT=F1                                                             
      IF(IGT.GT.2) GO TO 840                                            
      MK=IGT                                                            
      DO 830 M=1,KT,5                                                   
      READ(5,9100)UT                                                    
      DO 820 I=1,5                                                      
      U(MK  )=U(MK  )-UT(I)*F3                                          
      MK=MK+2                                                           
  820 CONTINUE                                                          
  830 CONTINUE                                                          
      GO TO 880                                                         
  840 CONTINUE                                                          
      MK=IGT-2                                                          
      DO 860 M=1,KT,5                                                   
      READ(5,9100) UT                                                   
      DO 850 I=1,5                                                      
      V(MK  )=V(MK  )-UT(I)*F3                                          
      MK=MK+2                                                           
  850 CONTINUE                                                          
  860 CONTINUE                                                          
  880 CONTINUE                                                          
      K=MAX0(K,KT)                                                      
      GO TO 2000                                                        
C                                                                       
C     HARMONIC OSCILLATOR                                               
C                                                                       
  900 CONTINUE                                                          
      READ (5,9000)G                                                    
      WRITE(6,9508)G                                                    
      FSS=FSS/2.0                                                       
      F1=1.0/(RY*RY)                                                    
      F2=F1/RY                                                          
      F3=0.5                                                            
      F4=SQRPI*0.5                                                      
      L=FL                                                              
      IF(L.EQ.0) GO TO 931                                              
      DO 930 I=1,L                                                      
      F3=F3+1.0                                                         
      F4=F4*F3                                                          
      F2=F2*F1                                                          
  930 CONTINUE                                                          
  931 CONTINUE                                                          
      NN=FN                                                             
      T1=1.0                                                            
      T2=F4                                                             
      CN(1)=(-1.0)**NN                                                  
      IF(NN.EQ.0) GO TO 941                                             
      DO 940 I=1,NN                                                     
      F3=F3+1.0                                                         
      T1=T1*FLOAT(I)                                                    
      T2=T2*F3                                                          
      CN(I+1)=-CN(I)*F1*FLOAT(NN+1-I)/(FLOAT(I)*F3)                     
  940 CONTINUE                                                          
  941 CONTINUE                                                          
      T2=SQRT(2.0*F2*T2/T1)/F4                                          
      IF(VR.NE.0.0) T2=T2*VR                                            
      KT=10.0*RY/DR(N)                                                  
      KT=MIN0(KT,K)                                                     
      R=0.0                                                             
      F1=F1/2.0                                                         
      DO 960 M=1,KT                                                     
      MK=M+M-1                                                          
      R=R+DR(N)                                                         
      R2=R*R                                                            
      F2=CN(1)                                                          
      IF(NN.EQ.0) GO TO 951                                             
      F3=1.0                                                            
      DO 950 I=1,NN                                                     
      F3=F3*R2                                                          
      F2=F2+CN(I+1)*F3                                                  
  950 CONTINUE                                                          
  951 CONTINUE                                                          
      U(MK  )=U(MK  )+F2*T2*EXP(-F1*R2)*R**L                            
  960 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     EXP(-((R-R0)/A)**2)*R**POWR                                       
C                                                                       
 1000 CONTINUE                                                          
      IF(VR.EQ.0.0) GO TO 1061                                          
      IF(AR.EQ.0.0) GO TO 1061                                          
      M1=(RR-6.0*AR)/DR(N)+1.0                                          
      M1=MAX0(M1,1)                                                     
      M2=(RR+6.0*AR)/DR(N)+1.0                                          
      M2=MIN0(M2,K)                                                     
      R=FLOAT(M1-1)*DR(N)                                               
      DO 1060 M=M1,M2                                                   
      MK=M+M-1                                                          
      R=R+DR(N)                                                         
      TEMP=((R-RR)/AR)**2                                               
      U(MK  )=U(MK  )-VR*EXP(-TEMP)*R**PWR                              
 1060 CONTINUE                                                          
 1061 CONTINUE                                                          
      IF(VI.EQ.0.0) GO TO 1071                                          
      IF(AI.EQ.0.0) GO TO 1071                                          
      M1=(RI-6.0*AI)/DR(N)+1.0                                          
      M1=MAX0(M1,1)                                                     
      M2=(RI+6.0*AI)/DR(N)+1.0                                          
      M2=MIN0(M2,K)                                                     
      R=FLOAT(M1-1)*DR(N)                                               
      DO 1070 M=M1,M2                                                   
      MK=M+M-1                                                          
      R=R+DR(N)                                                         
      TEMP=((R-RI)/AI)**2                                               
      U(MK+1)=U(MK+1)-VI*EXP(-TEMP)*R**PWR                              
 1070 CONTINUE                                                          
 1071 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     DEFORMED VOLUME OR SURFACE BY YLM EXPANSION                       
C                                                                       
 1100 CONTINUE                                                          
      READ (5,9000) (ABETA(J),FLDF(J),J=1,3)                          
      WRITE(6,9512) (ABETA(J),FLDF(J),J=1,3)                          
      LMAX=LAM+1                                                        
      DO 1101 J=1,3                                                     
      LDFRM(J)=FLDF(J)                                                  
      LMAX=MAX0(LMAX,LDFRM(J)+1)                                        
 1101 CONTINUE                                                          
      T2=(-1.0)**LAM                                                    
      DO 1108 I=1,8                                                     
      CN(I  )=0.0                                                       
      CN(I+8)=0.0                                                       
      P2=0.0                                                            
      P3=1.0                                                            
      DO 1106 M=1,LMAX                                                  
      L=M-1                                                             
      FL=L-1                                                            
      IF(L.EQ.0) GO TO 1102                                             
      P3=((2.0*FL+1.0)*X(I)*P2-FL*P1)/(FL+1.0)                          
 1102 CONTINUE                                                          
      DO 1103 J=1,3                                                     
      IF(ABETA(J).EQ.0.0) GO TO 1103                                    
      IF(L.NE.LDFRM(J)) GO TO 1103                                      
      FACTOR=P3*ABETA(J)*SQRT(FLOAT(L+L+1))/(SQRPI*2.0)                 
      CN(I  )=CN(I  )+FACTOR                                            
      CN(I+8)=CN(I+8)+FACTOR*(-1.0)**LDFRM(J)                           
 1103 CONTINUE                                                          
      IF (L.NE.LAM) GO TO 1104                                          
      YLAM(I  )=P3*W(I)*SQRT(FLOAT(L+L+1))*SQRPI                        
      YLAM(I+8)=YLAM(I  )*T2                                            
 1104 CONTINUE                                                          
      P1=P2                                                             
      P2=P3                                                             
 1106 CONTINUE                                                          
 1108 CONTINUE                                                          
      DO 1170 I=1,2                                                     
      GO TO (1110,1120),I                                               
 1110 CONTINUE                                                          
      IF(VR.EQ.0.0) GO TO 1170                                          
      VX=VR                                                             
      RX=RR                                                             
      AX=AR                                                             
      F1=1.0                                                            
      IFL=-1                                                            
      GO TO 1130                                                        
 1120 CONTINUE                                                          
      IF(VI.EQ.0.0) GO TO 1170                                          
      VX=VI                                                             
      RX=RI                                                             
      AX=AI                                                             
      F1=1.0                                                            
      F2=F4                                                             
      IFL=0                                                             
 1130 CONTINUE                                                          
      DO 1135 J=1,16                                                    
      CP(J)=EXP((1.0+CN(J))*RX/AX)                                      
 1135 CONTINUE                                                          
      IF(LAM.EQ.0) VX=VX/(SQRPI*2.0)                                    
      JT=NX-10                                                          
      DO 1160 M=1,K                                                     
      MK=M+M+IFL                                                        
      VTEMP=0.0                                                         
      F1=F1*F2                                                          
      DO 1155 J=1,16                                                    
      F3=F1*CP(J)                                                       
      GO TO (1140,1145),JT                                              
 1140 CONTINUE                                                          
      VTEMP=VTEMP-YLAM(J)*VX*F3/(1.0+F3)                                
      GO TO 1155                                                        
 1145 CONTINUE                                                          
      VTEMP=VTEMP+YLAM(J)*VX*F3/(1.0+F3)**2                             
 1155 CONTINUE                                                          
      U(MK  )=U(MK  )+VTEMP                                             
 1160 CONTINUE                                                          
 1170 CONTINUE                                                          
      GO TO 2000                                                        
 1200 CONTINUE                                                          
      GO TO 1100                                                        
C                                                                       
C     WATSON L.S POTENTIAL                                              
C                                                                       
 1300 IF(AR.NE.0.0) VR=VR/AR                                            
      IF(AI.NE.0.0) VI=VI/AI                                            
      IF((VR.NE.0.0).OR.(VI.NE.0.0)) IBF(4)=1                           
      DO 1360 M=1,K                                                     
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      V(MK  )=V(MK  )-VR*F1/(RR*(1.0+F1)**2)                            
      V(MK+1)=V(MK+1)-VI*F3/(RI*(1.0+F3)**2)                            
 1360 CONTINUE                                                          
      GO TO 2000                                                        
C                                                                       
C     VOLUME WOODS-SAXON, L-DEPENDENT EXCHANGE                          
C                                                                       
 1400 CONTINUE                                                          
      DO 1440 M=1,K                                                     
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      UR=VR*F1/(1.0+F1)                                                 
      UI=VI*F3/(1.0+F3)                                                 
      IF(N.GE.3) GO TO 1430                                             
      ULD(MK  ,N)=ULD(MK  ,N)-UR                                        
      ULD(MK+1,N)=ULD(MK+1,N)-UI                                        
      GO TO 1440                                                        
 1430 CONTINUE                                                          
      U(MK  )=U(MK  )-UR                                                
      U(MK+1)=U(MK+1)-UI                                                
 1440 CONTINUE                                                          
      GO TO 1990                                                        
C                                                                       
C     1ST DER. WOODS-SAXON, L-DEPENDENT EXCHANGE                        
C                                                                       
 1500 CONTINUE                                                          
      DO 1540 M=1,K                                                     
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      UR=VR*F1/(1.0+F1)**2                                              
      UI=VI*F3/(1.0+F3)**2                                              
      IF(N.GE.3) GO TO 1530                                             
      ULD(MK  ,N)=ULD(MK  ,N)+UR                                        
      ULD(MK+1,N)=ULD(MK+1,N)+UI                                        
      GO TO 1540                                                        
 1530 CONTINUE                                                          
      U(MK  )=U(MK  )+UR                                                
      U(MK+1)=U(MK+1)+UI                                                
 1540 CONTINUE                                                          
      GO TO 1990                                                        
C                                                                       
C     2ND DER. WOODS-SAXON, L-DEPENDENT EXCHANGE                        
C                                                                       
 1600 CONTINUE                                                          
      DO 1640 M=1,K                                                     
      MK=M+M-1                                                          
      F1=F1*F2                                                          
      F3=F3*F4                                                          
      UR=VR*F1*(1.0-F1)/(1.0+F1)**3                                     
      UI=VI*F3*(1.0-F3)/(1.0+F3)**3                                     
      IF(N.GE.3) GO TO 1630                                             
      ULD(MK  ,N)=ULD(MK  ,N)-UR                                        
      ULD(MK+1,N)=ULD(MK+1,N)-UI                                        
      GO TO 1640                                                        
 1630 CONTINUE                                                          
      U(MK  )=U(MK  )-UR                                                
      U(MK+1)=U(MK+1)-UI                                                
 1640 CONTINUE                                                          
 1990 CONTINUE                                                          
      IF (N.GE.3) ILD=1                                                 
 2000 CONTINUE                                                          
      IF(KFLAG.NE.0) GO TO 2009                                         
      IF(ABS(VSOR)+ABS(VSOI).EQ.0.0) GO TO 2009                         
      NX=NX+3                                                           
      IF(NX.GT.5) GO TO 2009                                            
      KFLAG=1                                                           
      VR=VR*VSOR/45.2                                                   
      VI=VI*VSOI/45.2                                                   
      GO TO 83                                                          
 2009 CONTINUE                                                          
      IF(FZ.GT.0.0) GO TO 70                                            
      RETURN                                                            
 9000 FORMAT(10F8.4)                                                    
 9100 FORMAT(5E16.7)                                                    
 9508 FORMAT(18X,9H   NODES=,F9.4,9H   L    =,F9.4,9H   2*J  =,F9.4,9H  
     & 2*S  =,F9.4)                                                     
 9509 FORMAT(3A6,9H   V RL =,F9.4,9H   R0RL =,F9.4,9H   A RL =,F9.4,9H  
     & R RL =,F9.4,9H   VSOR =,F9.4)                                    
 9510 FORMAT(18X,9H   V IM =,F9.4,9H   R0IM =,F9.4,9H   A IM =,F9.4,9H  
     & R IM =,F9.4,9H   VSOI =,F9.4,9H   POWR =,F9.4)                   
 9512 FORMAT(18X,9H   BETA1=,F9.4,9H   LDFR1=,F9.4,9H   BETA2=,F9.4,9H  
     & LDFR2=,F9.4,9H   BETA3=,F9.4,9H   LDFR3=,F9.4)                   
 9513 FORMAT(18X,9H   F1   =,F9.4,9H   F2   =,F9.4,9H   F3   =,F9.4)    
      END
