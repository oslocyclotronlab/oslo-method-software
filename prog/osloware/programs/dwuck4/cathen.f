      
      SUBROUTINE CATHEN(LOPT,JOPT,UB,VB,SI)                             
      DOUBLE PRECISION BETA                                             
      DIMENSION UB(800,2),VB(*),SI(*),C(10),QNUM(4,2),G(4),IQN(3)   
      DIMENSION JI(2),JJ(2),FJT(2),DG(2)                                
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      EQUIVALENCE (G(1),FN)                                             
      EQUIVALENCE (C(1),CNTROL), (C(2),QCODE), (C(3),FMUV), (C(4),OPT)  
      DATA SQR4PI/3.54490780/, SQRTEN/3.16227766/                       
      DATA KMAX/400/                                                    
C                                                                       
      IQFLG=0                                                           
      KMFLG=0                                                           
      IQN(1)=LOPT                                                       
      IQN(3)=JOPT                                                       
      KT=400                                                            
  100 CONTINUE                                                          
      READ(5,9001)C                                                     
      CNTRL=ABS(CNTROL)                                                 
      IF(CNTRL.EQ.0.0) GO TO 3010                                       
      IF(QCODE)2020,3000,1100                                           
 1100 CONTINUE                                                          
C                                                                       
C     LOOP FOR TWO ORBITALS                                             
C                                                                       
      ML=1                                                              
      DO 2010 I=1,2                                                     
      FJT(1)=C(5)                                                       
      FJT(2)=C(I+5)                                                     
      IF(I.EQ.1) GO TO 2004                                             
      IF(CNTRL.EQ.1.0) GO TO 2007                                       
 2004 CONTINUE                                                          
      CALL FORMF(UB(1,2),VB,3,LOPT)                                     
      RAM=(DR(3)/DRF)**3                                                
      DRX=DR(3)                                                         
      IF(KMFLG.EQ.0) KM=IBF(2)                                          
      KMFLG=1                                                           
      KT=MIN0(KT,K)                                                     
 2005 CONTINUE                                                          
C                                                                       
C     PRINT OUT SINGLE PARTICLE INFORMATION                             
C                                                                       
      IF(ICON(4).GE.2) GO TO 2007                                       
      WRITE(6,9501) I,CNTRL                                             
      R=DRX                                                             
      K2=K+K                                                            
      DO 2006 M=1,K2,20                                                 
      MK4=MIN0(M+19,K2)                                                 
      WRITE(6,9052)R,(UB(N,2),N=M,MK4,2)                                
      R=R+DRX*10.0                                                      
 2006 CONTINUE                                                          
 2007 CONTINUE                                                          
      DO 2008 M=1,400                                                   
      MK=M+M-1                                                          
      UB(ML,1)=UB(MK,2)                                                 
      ML=ML+1                                                           
 2008 CONTINUE                                                          
      DO 2009 M=1,4                                                     
      QNUM(M,I)=G(M)                                                    
 2009 CONTINUE                                                          
      JI(I)=FJT(1)                                                      
      JJ(I)=FJT(2)                                                      
 2010 CONTINUE                                                          
      SLP=0.0                                                           
      RMSP=0.0                                                          
      DG(1)=2.0*DRX/3.0                                                 
      DG(2)=2.0*DG(1)                                                   
      MX=1                                                              
      R=0.0                                                             
      DO 2015 M=1,KT                                                    
      R=R+DRX                                                           
      RSQ=R*R                                                           
      MX=3-MX                                                           
      TEMP=DG(MX)*UB(M    ,1)*UB(M+400,1)*RSQ                           
      SLP =SLP +TEMP                                                    
      RMSP=RMSP+TEMP*RSQ                                                
 2015 CONTINUE                                                          
      IF(SLP.EQ.0.0) GO TO 2020                                         
      TEMP=RMSP/SLP                                                     
      RMSP=SQRT(ABS(TEMP))                                              
      RMSP=SIGN(RMSP,TEMP)                                              
 2020 CONTINUE                                                          
      ICODE=ABS(QCODE)                                                  
      KT=MIN0(KT,K)                                                     
      LTR1=IQN(1)+1                                                     
      IQN(2)=IS(3)                                                      
      IF(IS(3).EQ.0) IQN(3)=IQN(1)+IQN(1)                               
      JLX=QNUM(3,1)                                                     
      JPX=QNUM(3,2)                                                     
      WRITE(6,9050) JLX,JI(1),JJ(1),JPX,JI(2),JJ(2)                     
      WRITE(6,9051) QCODE,FMUV,OPT,SLP,RMSP                             
C     ENTER TIME REVERSAL PHASE                                         
      LVR=QNUM(2,1)+QNUM(2,2)                                           
      OPT=OPT*PHASEF((IQN(1)-LVR)/2)                                    
      IFLAG=1                                                           
      IF(ICODE.LE.10) GO TO 2025                                        
      IFLAG=2                                                           
      ICODE=ICODE-10                                                    
 2025 CONTINUE                                                          
C                                                                       
      GO TO (2100,2100,2200,2100,2500,2100,2200,2200,2990,2990),ICODE   
C                                                                       
C     ICODE=1  YUKAWA POTENTIAL                                         
C     ICODE=2  COULOMB POTENTIAL                                        
C     ICODE=3  OPEP TENSOR POTENTIAL                                    
C     ICODE=4  GAUSSIAN POTENTIAL                                       
C     ICODE=5  TWO PARTICLE TRANSFER                                    
C     ICODE=6  ZERO RANGE KNOCK OUT (CENTRAL)                           
C     ICODE=7  ZERO RANGE TENSOR                                        
C     ICODE=8  R**2 X YUKAWA TENSOR                                     
C     ICODE=11-18  GIVES IMAGINARY FORM FACTOR                          
C                                                                       
 2100 LVR=0                                                             
      TOPT=1.0                                                          
      IKD=ICODE                                                         
      GO TO 2300                                                        
 2200 IF(ICODE.LT.7) ICODE=1                                            
      LVR=2                                                             
      TOPT=-SQRTEN                                                      
      IKD=1                                                             
 2300 CONTINUE                                                          
      MNNL=IABS(LTR1-LVR-1)+1                                           
      MXXL=LTR1+LVR                                                     
      LTR2=LTR1+LTR1-2                                                  
      LVR2=LVR+LVR                                                      
      LLX=QNUM(2,1)*2.0                                                 
      LPX=QNUM(2,2)*2.0                                                 
      JLX=QNUM(3,1)                                                     
      JPX=QNUM(3,2)                                                     
      IS1=QNUM(4,1)*2.0                                                 
      IS2=QNUM(4,2)*2.0                                                 
      OPT=OPT*PHASEF(LLX/2)                                             
      DR(3)=DRF                                                         
      DRX=DRF                                                           
      IF(QCODE.LT.0.0) GO TO 2330                                       
      DG(1)=2.0*DRX/3.0                                                 
      DG(2)=2.0*DG(1)                                                   
      MX=1                                                              
      R=0.0                                                             
      DO 2320 M=1,KT                                                    
      R=R+DRX                                                           
      MX=3-MX                                                           
      UB(M    ,1)=DG(MX)*UB(M    ,1)*UB(M+400,1)*R*R                    
 2320 CONTINUE                                                          
 2330 CONTINUE                                                          
      DO 2490 LAM =MNNL,MXXL,2                                          
      LAM2 =LAM +LAM -2                                                 
      VOPT=TOPT*OPT*SQR4PI*SQRT(FLOAT(LAM2 +1)*FLOAT(IQN(2)+1))         
     & *VCC(LAM2 ,LVR2,LTR2 ,0,0)                                       
     & *RACAH(IQN(2),IQN(2),LAM2 ,LTR2 ,LVR2,IQN(3))                    
      RME=0.0                                                           
      IF(IQN(2).EQ.0) RME=1.0                                           
      IF(IQN(2).EQ.2) RME=SQRT(FLOAT(IS1*(IS1+2)))                      
     &  *SQRT(FLOAT(IS(1)*(IS(1)+2)))                                   
     &  *SQRT(1.0+FLOAT(IABS(IS(1)-IS(2)))/4.0)*PHASEF((IS(1)-IS(2))/2) 
      RME=RME*SQRT(FLOAT(IS1+1))                                        
     & *SQRT(FLOAT((LLX+1)*(LAM2+1)))*VCC(LLX,LAM2,LPX,0,0)             
     & *SQRT(FLOAT(JLX+1)*FLOAT(JPX+1)*FLOAT(IQN(3)+1))                 
     & *WINEJ(LPX,LLX,LAM2 ,IS2,IS1,IQN(2),JPX,JLX,IQN(3))              
      IF(JI(1).NE.0) RME=RME*PHASEF((JI(1)+IQN(3)-JLX-JJ(2))/2)         
     & *RACAH(JPX,JJ(2),JLX,JJ(1),JI(1),IQN(3))                         
     & *SQRT(FLOAT(JJ(1)+1)*FLOAT(JJ(2)+1))                             
      VOPT=VOPT*RME*SQRT(FLOAT(LTR2+1)/FLOAT((JJ(1)+1)*(IQN(3)+1)))     
      VOPT=VOPT*RAM                                                     
      SL=0.0                                                            
      RMS=0.0                                                           
      IF(VOPT.EQ.0.0) GO TO 2485                                        
      IF(ICODE.LT.6) GO TO 2460                                         
      IF(ICODE.GT.7) GO TO 2400                                         
      SCALE=VOPT/(SQR4PI*SQR4PI)                                        
      MX=1                                                              
      R=0.0                                                             
      MK=IFLAG                                                          
      DO 2360 M=1,KT                                                    
      R=R+DRX                                                           
      RSQ=R*R                                                           
      MX=3-MX                                                           
C     TEMP=UB(M    ,1)*UB(M+400,1)*SCALE                                
      TEMP=UB(M,1)*SCALE                                                
      SI(MK)=SI(MK)+TEMP/(DG(MX)*RSQ)                                   
      SL    =SL    +TEMP                                                
      RMS   =RMS   +TEMP*RSQ                                            
      MK=MK+2                                                           
 2360 CONTINUE                                                          
      GO TO 2485                                                        
C                                                                       
 2400 CONTINUE                                                          
C     CONSTRUCT G-SUB-LAM,L(X,R) FOR R**2-YUKAWA TENSOR                 
      IF(LAM.EQ.LTR1) GO TO 2420                                        
      CALL SLATR(KT,DRX,VB(  1),LTR1,FMUV,1,KMAX)                       
      LAMP=LAM                                                          
      CALL SLATR(KT,DRX,VB(801),LAMP,FMUV,1,KMAX)                       
      LAMP=(LAM+LTR1)/2                                                 
      CALL SLATR(KT,DRX,UB(1,2),LAMP,FMUV,1,KMAX)                       
      GO TO 2480                                                        
 2420 CONTINUE                                                          
      CALL SLATR(KT,DRX,VB(  1),LTR1-1,FMUV,1,KMAX)                     
      CALL SLATR(KT,DRX,VB(801),LTR1+1,FMUV,1,KMAX)                     
      CALL SLATR(KT,DRX,UB(1,2),LTR1,FMUV,1,KMAX)                       
      GO TO 2480                                                        
 2460 CONTINUE                                                          
C     CONSTRUCT G-SUB-LAM,L(X,R) FOR NORMAL CENTRAL AND TENSOR TERMS    
      CALL SLATR(KT,DRX,VB(  1),LTR1,FMUV,IKD,KMAX)                     
      IF(LAM.NE.LTR1) GO TO 2475                                        
      DO 2470 I=1,KT                                                    
      VB(I+ 800)=VB(I    )                                              
      VB(I+1200)=VB(I+400)                                              
 2470 CONTINUE                                                          
      GO TO 2480                                                        
 2475 CONTINUE                                                          
      LAMP=LAM                                                          
      CALL SLATR(KT,DRX,VB(801),LAMP,FMUV,IKD,KMAX)                     
 2480 CONTINUE                                                          
      LAMP=LAM                                                          
      CALL RADIN(KT,DRX,VB,UB,SL,RMS,VOPT,SI(IFLAG),ICODE,LTR1,LAMP     
     &  ,FMUV,KMAX)                                                     
C                                                                       
 2485 CONTINUE                                                          
      IF(SL.EQ.0.0) GO TO 2488                                          
      TEMP=RMS/SL                                                       
      RMS=SQRT(ABS(TEMP))                                               
      RMS=SIGN(RMS,TEMP)                                                
 2488 CONTINUE                                                          
      I=LAM -1                                                          
      WRITE(6,9102) IQN(1),I,RME,SL,RMS                                 
 2490 CONTINUE                                                          
      GO TO 2990                                                        
C                                                                       
C     HERE FOR TWO NUCLEON TRANSFER                                     
C                                                                       
 2500 CONTINUE                                                          
      M=0                                                               
      UB(M,1)=0.0                                                       
      IF(FMUV.EQ.0.0) FMUV=1.7                                          
      CALL DSTRIP(IQN,DRX,KT,UB,SI(IFLAG),QNUM,OPT,FMUV,KM,SL)          
      WRITE(6,9002)KM,SL                                                
      IQFLG=1                                                           
 2990 CONTINUE                                                          
 3000 CONTINUE                                                          
      IF(CNTROL.GT.0.0) GO TO 100                                       
      IF((IFF.EQ.NLTR).AND.(IQFLG.EQ.1)) KT=KT-5                        
      K=KT                                                              
 3010 RETURN                                                            
 9001 FORMAT(10F8.4)                                                    
 9002 FORMAT(15H FORM FACTOR,M=,I3,E18.6)                               
 9050 FORMAT(33H0ANGULAR MOMENTUM COUPLING  2*J1=,I3,3X,5H2*JC=,I3,3X,5H
     &2*JI=,I3,5X,5H2*J2=,I3,5X,5H2*JC=,I3,5X,5H2*JF=,I3)               
 9051 FORMAT(18H0PARAMETERS       ,9H   QCODE=,F9.4,9H   RANGE=,F9.4,7H 
     &  V0 =,F11.4,3X,21HSING. PART. OVERLAP =,F12.4,9H   RMS R=,F9.4)  
 9052 FORMAT(1H ,F6.2,10E12.4)                                          
 9102 FORMAT(18X,9H   LTR  =,I4,5X,9H   LAM  =,I4,5X,9H   RME  =,F9.4,3X
     &,21HF.F. VOL. INTEGRAL  =,F12.4,9H   RMS R=,F9.4)                 
 9501 FORMAT(25H0FORM FACTOR DATA   PART.,I3,3H OF,F3.0)                
 9998 FORMAT(1H1)                                                       
      END
