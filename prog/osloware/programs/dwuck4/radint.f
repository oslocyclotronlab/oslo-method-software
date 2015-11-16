      
      SUBROUTINE RADINT(F,LTR)                                          
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  FLL(4000),SPACE(2400),UB(800)                     
      COMMON /LDEPEN/ ULD(800,2),ILDSAV(8),ILD,IDSOSV(8),IDSO           
      COMMON /ISOSPN/ TWBF(800),TSPN(2),CANAL,CCORE                     
      DIMENSION F1(408),F2(408),CT(408),ITA(2),ITAZ(2),ITPZ(2),DSO(4)   
      DIMENSION F(600),C(600),CTEMP(2),VTEMP(2),DRX(2),VB(800)          
      EQUIVALENCE (C(1),UB(1)), (ULD(1,2),VB(1))                        
     &  ,(F1(1),SPACE(   1)), (F2(1),SPACE( 601)), (CT(1),SPACE(1801))  
C                                                                       
CMV   VARIABLES AND SETUP FOR THE UNBOUND TAIL INTEGRATION              
      DIMENSION FC1(2),FC2(6),FC3(900),FC3P(6),CPHAS(600)               
     &  ,F2D(2),F3D(2),E2(2),FKN(2),S1(2),ANS(2)                        
      COMMON /UBOUND/ DY,YMAX,EISIG0(2)                                 
      EQUIVALENCE (SPACE(   1),FC3(1)), (ULD(1,2),CPHAS(1))             
      DATA NTMAX/200/                                                   
C                                                                       
      NTAIL=0                                                           
      IF(RC(3).EQ.0.0) RC(3)=DR(3)                                      
      VFACT=VCE*3.0*CHGSQ*Z(1)*ZA(1)*RC(3)**LTR/FLOAT(LTR+LTR+1)        
      IF(DY.EQ.0.0) GO TO 300                                           
C     PROJECTILE INDICES--ID=HEAVIER,IP=LIGHTER                         
      ID=1                                                              
      IP=2                                                              
      IF(FK2(1).GT.FK2(2)) GO TO 210                                    
      ID=2                                                              
      IP=1                                                              
  210 CONTINUE                                                          
      FKD=FK(ID)                                                        
      FKP=FK(IP)                                                        
C     FKN AND E2 WOULD BE IMAGINARY FOR WEAKLY BOUND FORM FACTOR,       
C     AND COMPLEX FOR A GAMOW FORM FACTOR (PERISH THE THOUGHT).         
      IR1=1                                                             
      IR2=2                                                             
      IF(FK2(3).GT.0.0) GO TO 220                                       
      IR1=2                                                             
      IR2=1                                                             
  220 CONTINUE                                                          
      FKN(IR1)=FK(3)                                                    
      FKN(IR2)=0.0                                                      
      E1=ETA(ID)                                                        
      E2(IR1)=ETA(3)*(-1)**IR2                                          
      E2(IR2)=0.0                                                       
      E3=ETA(IP)                                                        
      IF(ABS(FMU(1)-FMU(2)).GT.0.1) GO TO 230                           
C     INELASTIC SCATTERING, COULOMB EXCITATION                          
      FKN(IR1)=0.0                                                      
      E2(IR1)=0.0                                                       
  230 CONTINUE                                                          
      AB=DR(IP)/DR(ID)                                                  
      II=1                                                              
      DO 240 KM3=1,3                                                    
      M=KM3+K-3                                                         
      MK=M+M-1                                                          
      R=FLOAT(M)*DR(3)                                                  
      FC2(II  )=UB(MK  )*R + VFACT/R**LTR                               
      FC2(II+1)=UB(MK+1)*R                                              
      II=II+2                                                           
  240 CONTINUE                                                          
      F2D(1)=(0.5*FC2(1)-2.0*FC2(3)+1.5*FC2(5))/DR(3)                   
      F2D(2)=(0.5*FC2(2)-2.0*FC2(4)+1.5*FC2(6))/DR(3)                   
      FLN=LTR                                                           
      ANS(1)=0.0                                                        
      ANS(2)=0.0                                                        
C     SETUP DEFAULT YMAX AND CHECK LIMITS ON NTAIL                      
      IF(YMAX.EQ.0.0) YMAX=6.0/(FKD-FKP-FKN(1))                         
      YM1=ABS(YMAX)                                                     
      NTAIL=YM1/ABS(DY)                                                 
      NTAIL=2*(NTAIL/2)                                                 
      IF(NTAIL.LE.NTMAX) GO TO 250                                      
      WRITE(6,910) NTAIL,NTMAX                                          
      NTAIL=NTMAX                                                       
      IF(DY.LT.0.0) NTAIL=-NTMAX                                        
      DY=YM1/NTAIL                                                      
      NTAIL=NTMAX                                                       
  250 CONTINUE                                                          
      DY1=ABS(DY)                                                       
      WRITE(6,911) YM1,DY1,NTAIL,NTMAX,VFACT                            
      NEW=1                                                             
      IBIAS=0                                                           
      IF(IP.EQ.2) IBIAS=NS(1)*LPL2                                      
      NP2=NS(IP)*LPL2                                                   
CMV   ENDS TAIL SETUP SECTION                                           
C                                                                       
  300 CONTINUE                                                          
      NMTR=LTR+1                                                        
      JR=NS(1)                                                          
      JS=NS(2)                                                          
      JT=JR+JS                                                          
      IS1=IS(1)                                                         
      IS2=IS(2)                                                         
      DRX(1)=2.0*DR(3)/3.0                                              
      DRX(2)=2.0*DRX(1)                                                 
      NP=LPL2*JT                                                        
      IZZ=NS(1)*LPL2                                                    
      INCR=LPL2*NMTR                                                    
      INC=INCR*NS(1)*NS(2)                                              
      DO 3 II=1,INC                                                     
      FLL(II)=0.0                                                       
    3 CONTINUE                                                          
      IX=MOD(ICON(19),4)                                                
      IF(IX.EQ.0) GO TO 30                                              
      I1=IX                                                             
      IF(I1.EQ.3) I1=2                                                  
      I2=3-I1                                                           
      DO 5 I=1,2                                                        
      ITA (I)=2.0*TSPN(I)+0.1                                           
      ITAZ(I)=FMA(I)+0.1-2.0*ZA(I)                                      
      ITPZ(I)=FM (I)+0.1-2.0*Z (I)                                      
    5 CONTINUE                                                          
      IF(ICON(19).GT.3) GO TO 20                                        
      IF(FM(2).GT.FM(1)) GO TO 10                                       
      IA=1                                                              
      IB=2                                                              
      GO TO 15                                                          
   10 IA=2                                                              
      IB=1                                                              
   15 CONTINUE                                                          
      ITZ=ITPZ(IA)-ITPZ(IB)                                             
      ITANZ=ITAZ(IA)                                                    
      IF((IA+I1).NE.3) ITANZ=ITANZ+2*ITZ                                
      CCORE=VCC(ITA(IA),1,ITA(IB),ITAZ(IA), ITZ)                        
      CANAL=VCC(ITA(IA),1,ITA(IB),ITANZ   ,-ITZ)                        
   20 CONTINUE                                                          
      IF(CCORE.EQ.0.) GO TO 22                                          
      C1=CANAL/CCORE                                                    
      GO TO 25                                                          
   22 C1=0.0                                                            
   25 CONTINUE                                                          
      IA=IFIX(FM(I1)+0.1)+IFIX(Z(I1)+0.1)                               
      IB=IFIX(FM(I2)+0.1)+IFIX(Z(I2)+0.1)                               
      IF(IB.NE.3) GO TO 26                                              
      IF(IA.EQ.4) C1=0.9379*C1                                          
      IF(IA.EQ.5) C1=1.0662*C1                                          
   26 CONTINUE                                                          
      IF((FM(1).GT.FM(2)).AND.(I1.EQ.2)) C1=-C1                         
      IF((FM(1).LT.FM(2)).AND.(I1.EQ.1)) C1=-C1                         
      TTZ=FLOAT(ITAZ(1))/2.                                             
      TTR=FLOAT(ITAZ(2))/2.                                             
      WRITE(6,900) TSPN(1),TTZ,TSPN(2),TTR,C1                           
      SMALL=1.0E-18                                                     
      DO 28 M=1,K                                                       
      MK=M+M-1                                                          
      IF(ABS(UB(MK)).LT.SMALL) UB(MK)=SMALL                             
      TWBF(M)=C1*TWBF(MK)/UB(MK)                                        
   28 CONTINUE                                                          
      NMJ=0                                                             
      IF(I1.NE.1) NMJ=IZZ                                               
      NT=2*NS(I1)*LPL2                                                  
      READ(12) (CT(N),N=1,NT)                                           
   30 CONTINUE                                                          
      MX=1                                                              
      R=0.0                                                             
      PHAS=1.0                                                          
      DO 100 M=1,K                                                      
      R=R+DR(3)                                                         
C                                                                       
C     READ IN DISTORTED WAVE RADIAL FUNCTIONS                           
C                                                                       
      READ(4)(F(N),N=1,NP)                                              
      IF(IX.EQ.0) GO TO 40                                              
      READ(10) (F1(N),N=1,NT),(F2(N),N=1,NT)                            
      DO 35 I=1,LPL2                                                    
      N1=I+I-1                                                          
      N2=N1+1                                                           
      N3=N1+NMJ                                                         
      N4=N3+1                                                           
      N5=N1+NS(I1)*LPL2                                                 
      N6=N5+1                                                           
      F(N3) =CT(N1)*F1(N1)-CT(N2)*F1(N2)+CT(N5)*F1(N5)-CT(N6)*F1(N6)    
      F(N4) =CT(N1)*F1(N2)+CT(N2)*F1(N1)+CT(N5)*F1(N6)+CT(N6)*F1(N5)    
      F1(N1)=CT(N1)*F2(N1)-CT(N2)*F2(N2)+CT(N5)*F2(N5)-CT(N6)*F2(N6)    
      F1(N2)=CT(N1)*F2(N2)+CT(N2)*F2(N1)+CT(N5)*F2(N6)+CT(N6)*F2(N5)    
      F(N3) =F(N3)+TWBF(M)*F1(N1)                                       
      F(N4) =F(N4)+TWBF(M)*F1(N2)                                       
   35 CONTINUE                                                          
   40 CONTINUE                                                          
      MK=M+M-1                                                          
      MX=3-MX                                                           
      VTEMP(1)=UB(MK  )*DRX(MX)                                         
      VTEMP(2)=UB(MK+1)*DRX(MX)                                         
      IF(VFACT.EQ.0.0) GO TO 41                                         
C                                                                       
C     COULOMB EXCITATATION ADDITION TO FORM FACTOR                      
C                                                                       
      IF(R.LT.RC(3)) GO TO 41                                           
      VTEMP(1)=VTEMP(1)+DRX(MX)*VFACT/R**NMTR                           
   41 CONTINUE                                                          
C     ENDPOINT CORRECTION OF SIMPSON'S RULE---                          
C     NEEDED FOR LONG-RANGED FORM FACTORS (DY.NE.0)                     
      IF(M.NE.K) GO TO 43                                               
      VTEMP(1)=VTEMP(1)*0.5                                             
      VTEMP(2)=VTEMP(2)*0.5                                             
   43 CONTINUE                                                          
      IF(IDSO.EQ.0) GO TO 45                                            
      DSO(1)=VB(MK  )*DRX(MX)                                           
      DSO(2)=VB(MK+1)*DRX(MX)                                           
   45 CONTINUE                                                          
C                                                                       
C     READ IN NORMALIZATION CONSTANTS                                   
C                                                                       
      IF(M.EQ.K) READ(4)(C(I),I=1,NP)                                   
      IF(M.EQ.K.AND.NTAIL.GT.0) READ(4) (CPHAS(I),I=1,NP)               
      IF(NTAIL.EQ.0) GO TO 50                                           
      IF(M.LT.K-2) GO TO 50                                             
      KM3=3-K+M                                                         
      IR1=2*(KM3-1)+1                                                   
      DO 48 II=1,NP2,2                                                  
      IR2=II+IBIAS                                                      
      FC3(IR1  )=F(IR2  )                                               
      FC3(IR1+1)=F(IR2+1)                                               
      IR1=IR1+6                                                         
   48 CONTINUE                                                          
   50 CONTINUE                                                          
      INC=0                                                             
      IY=0                                                              
      MS1=-IS(1)                                                        
      DO 96 I= 1,JR                                                     
      IZ=IZZ                                                            
      MS2=-IS(2)                                                        
      DO 95 J=1,JS                                                      
      DO 90 LL=1,LPLUS                                                  
      LE=LL-1                                                           
      LK=LL+LL-1                                                        
      IZ1=LK+IZ                                                         
      LP1=IABS(LL-LTR-1)+1                                              
      LP2=MIN0(LL+LTR,LPLUS)                                            
      IF(ILD.GT.0) PHAS=PHASEF(LP1-1)                                   
C                                                                       
C     FORM FACTOR TIMES EXIT PARTIAL WAVE FUNCTION                      
C                                                                       
      CTEMP(1)=VTEMP(1)*F(IZ1  )-VTEMP(2)*F(IZ1+1)                      
      CTEMP(2)=VTEMP(1)*F(IZ1+1)+VTEMP(2)*F(IZ1  )                      
      IF(IDSO.EQ.0) GO TO 60                                            
      DSO(3)=DSO(1)*F(IZ1  )-DSO(2)*F(IZ1+1)                            
      DSO(4)=DSO(1)*F(IZ1+1)+DSO(2)*F(IZ1  )                            
      IF(MS2) 52,54,56                                                  
   52 GAM1=-IS2*(LE+1)                                                  
      GO TO 60                                                          
   54 GAM1=-IS2*(IS2+2)                                                 
      GAM1=GAM1/2.0                                                     
   56 GAM1=IS2*LE                                                       
   60 CONTINUE                                                          
C                                                                       
C     LOOP OVER INCIDENT PARTIAL WAVES THAT COUPLE                      
C                                                                       
      DO 80 LP=LP1,LP2,2                                                
      LI=LP-1                                                           
      IY1=LP+LP-1+IY                                                    
      IND=LK+INC                                                        
      IF(M.LT.KZ) GO TO 79                                              
      FLL(IND  )=FLL(IND  )+(CTEMP(1)*F(IY1  )-CTEMP(2)*F(IY1+1))*PHAS  
      FLL(IND+1)=FLL(IND+1)+(CTEMP(1)*F(IY1+1)+CTEMP(2)*F(IY1  ))*PHAS  
      IF(IDSO.EQ.0) GO TO 70                                            
      IF(MS1) 62,64,66                                                  
   62 GAM2=-IS1*(LI+1)                                                  
      GO TO 68                                                          
   64 GAM2=-IS1*(IS1+2)                                                 
      GAM2=GAM2/2.0                                                     
      GO TO 68                                                          
   66 GAM2=IS1*LI                                                       
   68 GAM=(GAM1+GAM2)/4.0                                               
      FLL(IND  )=FLL(IND  )+GAM*(DSO(3)*F(IY1  )-DSO(4)*F(IY1+1))       
      FLL(IND+1)=FLL(IND+1)+GAM*(DSO(3)*F(IY1+1)+DSO(4)*F(IY1  ))       
   70 CONTINUE                                                          
CMV   INDICIES FOR WAVEFNS TO BE SAVED AT LAST 3 POINTS.                
      IF(M.LT.K-2) GO TO 79                                             
      IF(NTAIL.EQ.0) GO TO 72                                           
      IYZD=IY1                                                          
      IYZP=IZ1                                                          
      FLD=LI                                                            
      FLP=LE                                                            
      IF(ID.NE.2) GO TO 72                                              
      IYZD=IZ1                                                          
      IYZP=IY1                                                          
      FLD=LE                                                            
      FLP=LI                                                            
   72 CONTINUE                                                          
      IF(M.NE.K) GO TO 79                                               
      IF(NTAIL.EQ.0) GO TO 75                                           
      IR1=1                                                             
      IR2=3*(IYZP-IBIAS-1)+1                                            
      DO 74 II=1,3                                                      
      FC3P(IR1  )=C(IYZP  )*FC3(IR2  )-C(IYZP+1)*FC3(IR2+1)             
      FC3P(IR1+1)=C(IYZP  )*FC3(IR2+1)+C(IYZP+1)*FC3(IR2  )             
      IR1=IR1+2                                                         
      IR2=IR2+2                                                         
   74 CONTINUE                                                          
      F3D(1)=(0.5*FC3P(1)-2.0*FC3P(3)+1.5*FC3P(5))/DR(3)                
      F3D(2)=(0.5*FC3P(2)-2.0*FC3P(4)+1.5*FC3P(6))/DR(3)                
      FC1(1)=C(IYZD  )*F(IYZD  )-C(IYZD+1)*F(IYZD+1)                    
      FC1(2)=C(IYZD  )*F(IYZD+1)+C(IYZD+1)*F(IYZD  )                    
      TEMP  =FC1(1)*EISIG0(1)-FC1(2)*EISIG0(2)                          
      FC1(2)=FC1(2)*EISIG0(1)+FC1(1)*EISIG0(2)                          
      FC1(1)=TEMP                                                       
      S1(1)=CPHAS(IYZD  )                                               
      S1(2)=CPHAS(IYZD+1)                                               
      FCODE=TAILS(ANS,NTAIL,AB,FLD,FLN,FLP,FKD,FKN,FKP,                 
     &   E1,E2,E3,R,S1,FC2(5),F2D,FC3P(5),F3D,FC1,LPLUS,NEW)            
      TEMP  =ANS(1)*EISIG0(1)+ANS(2)*EISIG0(2)                          
      ANS(2)=ANS(2)*EISIG0(1)-ANS(1)*EISIG0(2)                          
      ANS(1)=TEMP                                                       
C                                                                       
C     NORMALIZE RADIAL INTEGRALS                                        
C                                                                       
   75 CTEMP1=C(IZ1  )*FLL(IND  )-C(IZ1+1)*FLL(IND+1)                    
      CTEMP2=C(IZ1  )*FLL(IND+1)+C(IZ1+1)*FLL(IND  )                    
      FLL(IND  )=CTEMP1*C(IY1  )-CTEMP2*C(IY1+1)                        
      FLL(IND+1)=CTEMP1*C(IY1+1)+CTEMP2*C(IY1  )                        
      IF(DY.EQ.0.0) GO TO 79                                            
      FLL(IND  )=FCODE*(FLL(IND  )+ANS(1))                              
      FLL(IND+1)=FCODE*(FLL(IND+1)+ANS(2))                              
   79 CONTINUE                                                          
      LK=LK+LPL2                                                        
   80 CONTINUE                                                          
   90 CONTINUE                                                          
      INC=INC+INCR                                                      
      IZ=IZ+LPL2                                                        
      MS2=MS2+2                                                         
   95 CONTINUE                                                          
      IY=IY+LPL2                                                        
      MS1=MS1+2                                                         
   96 CONTINUE                                                          
  100 CONTINUE                                                          
      REWIND 4                                                          
      IF(NTAIL.NE.0) CLOSE(UNIT=21,DISPOSE='DELETE')                    
      IF(NLTR.NE.1) GO TO 105                                           
      IF(JT.EQ.2)  GO TO 130                                            
  105 CONTINUE                                                          
C                                                                       
C     WRITE RADIAL MATRIX ELEMENTS ON TAPE 8                            
C                                                                       
      JR=IBF(7)+1                                                       
      JS=IBF(8)+1                                                       
      JSX=JS                                                            
      IF(IBF(6).EQ.0) JSX=1                                             
      DO 120 I=1,JR                                                     
      IY=I-1                                                            
      IF(IBF(5).EQ.0) IY=0                                              
      DO 110 J=1,JS                                                     
      IZ=J-1                                                            
      IF(IBF(6).EQ.0) IZ=0                                              
      INC=INCR*(JSX*IY+IZ)+1                                            
      INDEX=INC+INCR-1                                                  
      WRITE(8)(FLL(II),II=INC,INDEX)                                    
  110 CONTINUE                                                          
  120 CONTINUE                                                          
  130 RETURN                                                            
  900 FORMAT(6H0*****,3X,4HTA =,F5.1,4X,5HTAZ =,F5.1,6X,4HTB =,F5.1,4X,5
     &HTBZ =,F5.1,6X,3HR =,F9.5)                                        
  910 FORMAT(15H REDUCED NTAIL=,I5,10H TO NTMAX=,I5)                    
  911 FORMAT(51H0 TAILS CALCULATED WITH YMAX,DY,NTAIL,NTMAX,VFACT= ,2F9.
     &4,2I5,F9.4)                                                       
      END
