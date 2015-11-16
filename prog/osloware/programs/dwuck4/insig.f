      
      SUBROUTINE INSIG(D,PLM,JTR,FACTR)                                 
      DOUBLE PRECISION BETA                                             
      DIMENSION D(1),SUM(5,5,2),PLM(1),POLX(9),POL(181,9),TSIG(181)     
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  DTEMP(7200)                                       
      EQUIVALENCE (DTEMP(1),SUM(1,1,1)), (DTEMP(51),TSIG(1))            
     &           ,(DTEMP(232),POL(1,1))                                 
      EQUIVALENCE (ANGLE(1),THETAN),(ANGLE(2),THETA1),(ANGLE(3),DTHETA) 
     &,(ANGLE(4),AANG),(ANGLE(5),BANG),(ANGLE(6),DELHLF)                
      DATA NX/181/                                                      
C                                                                       
      NTHETA=THETAN                                                     
      NTHETA=MIN0(NTHETA,NX)                                            
      JR=NS(1)                                                          
      JS=NS(2)                                                          
      FACTA=FACTR/(2.0*FS(1)+1.0)                                       
      FACTF=(FK(1)/FK(2))**2                                            
      M2K=(1.0+PHASEF(NS(3)))/2.0                                       
      NPLUS=(JTR+IS(1)+IS(2))/2+1                                       
      MPLUS=JTR/2+1                                                     
      IFACT=2*MPLUS*JR*JS                                               
      WRITE(6,9000)                                                     
      IF(NTHETA.EQ.0) GO TO 210                                         
      IP=DELHLF/DTHETA+0.5                                              
      IF(IP.EQ.0) IP=1                                                  
      P=DELHLF/(FLOAT(IP)*DTHETA)                                       
      PSQ=P*P                                                           
      AQ=ABS(FK2(1))+ABS(FK2(2))                                        
      BQ=2.0*FK(1)*FK(2)                                                
      THETA=THETA1                                                      
      IF ((ICON(13).EQ.1).OR.(ICON(13).EQ.3)) WRITE(7,9004) ALPHA       
C                                                                       
C     ANGLE LOOP                                                        
C                                                                       
      DO 100 N=1,NTHETA                                                 
      CALL LGNDR(PLM,NPLUS,LPLUS,THETA)                                 
      SIGMA=0.0                                                         
      DO 10 I=1,9                                                       
      POL(N,I)=0.0                                                      
   10 CONTINUE                                                          
      DO 80 M=1,MPLUS                                                   
      MK=M+M-1                                                          
      KT=MK                                                             
      IS2=-IS(2)                                                        
      DO 60 J=1,JS                                                      
      IS1=-IS(1)                                                        
      DO 50 I=1,JR                                                      
      ML=(MK-1+M2K+IS2-IS1)/2                                           
      PHAS=1.0                                                          
      IF(ML.GT.0) PHAS=PHASEF(ML)                                       
      ML=IABS(ML)*LPLUS                                                 
      SUM1=0.0                                                          
      SUM2=0.0                                                          
      IND=KT                                                            
      DO 40 LL=1,LPLUS                                                  
      ML=ML+1                                                           
      SUM1=SUM1+D(IND  )*PLM(ML)                                        
      SUM2=SUM2+D(IND+1)*PLM(ML)                                        
      IND=IND+IFACT                                                     
   40 CONTINUE                                                          
      IF(ABS(SUM1).LT.1.E-16) SUM1=0.0                                  
      IF(ABS(SUM2).LT.1.E-16) SUM2=0.0                                  
      SUM(J,I,1)=SUM1*PHAS                                              
      SUM(J,I,2)=SUM2*PHAS                                              
      FACT=0.0                                                          
      TEMP=SUM(J,I,1)                                                   
      IF(ABS(TEMP).GT.1.0E-18) FACT=TEMP*TEMP                           
      TEMP=SUM(J,I,2)                                                   
      IF(ABS(TEMP).GT.1.0E-18) FACT=FACT+TEMP*TEMP                      
      IF(M+M2K.NE.1) FACT=FACT+FACT                                     
      SIGMA=SIGMA+FACT                                                  
      KT=KT+MPLUS+MPLUS                                                 
      IS1=IS1+2                                                         
   50 CONTINUE                                                          
      IS2=IS2+2                                                         
   60 CONTINUE                                                          
      CALL POLFCT(POLX,SUM(1,1,1),SUM(1,1,2),JS,JR)                     
      DO 70 I=1,9                                                       
      IF(M+M2K.NE.1) POLX(I)=POLX(I)*2.0                                
      POL(N,I)=POL(N,I)+POLX(I)                                         
   70 CONTINUE                                                          
   80 CONTINUE                                                          
      IF(SIGMA.EQ.0.0) GO TO 90                                         
      DO 85 I=1,9                                                       
      POL(N,I)=POL(N,I)/SIGMA                                           
   85 CONTINUE                                                          
   90 CONTINUE                                                          
      TSIG(N)=SIGMA*FACTA                                               
      THETA=THETA+DTHETA                                                
  100 CONTINUE                                                          
C                                                                       
C     PRINT RESULTS                                                     
C                                                                       
      IF(DELHLF.GT.0.0) 

     &OPEN(UNIT=21,STATUS='SCRATCH',FORM='UNFORMATTED')
      THETA=THETA1                                                      
      DO 200 N=1,NTHETA                                                 
      SIG1=TSIG(N)                                                      
      DO 120 I=1,9                                                      
      POLX(I)=POL(N,I)                                                  
  120 CONTINUE                                                          
      IF(DELHLF.LE.0.0) GO TO 150                                       
C                                                                       
C     AVERAGE OVER ANGULAR RESOLUTION                                   
C                                                                       
      N1=N-1-IP                                                         
      N1=IABS(N1)+1                                                     
      N2=N+IP                                                           
      IF(N2.GT.NTHETA) N2=NTHETA+NTHETA-N2                              
      TEMP=PSQ                                                          
      IF((N.EQ.1).AND.(THETA.GT.0.1)) TEMP=0.0                          
      IF((N.EQ.NTHETA).AND.((180.0-THETA).GT.0.1)) TEMP=0.0             
      SIG1 =(6.0*TSIG(N)+TEMP*(TSIG(N1)+TSIG(N2)-2.0*TSIG(N)))/6.0      
      DO 140 I=1,9                                                      
      POLX(I)=(6.0*POL(N,I)+TEMP*(POL(N1,I)+POL(N2,I)                   
     &  -2.0*POL(N,I)))/6.0                                             
  140 CONTINUE                                                          
C                                                                       
  150 CONTINUE                                                          
      SIG2=SIG1*FACTF                                                   
      ARG=THETA/57.29578                                                
      QTR=AQ-BQ*COS(ARG)                                                
      IF(QTR.LT.0.0) QTR=0.0                                            
      QTR=SQRT(QTR)                                                     
      WRITE(6,9001)THETA,QTR,SIG1,(POLX(I),I=1,4),SIG2,(POLX(I),I=5,9)  
      IF((ICON(13).EQ.1).OR.(ICON(13).EQ.3)) WRITE(7,9010) THETA,SIG1,  
     &  (POLX(I),I=1,9)                                                 
      IF(DELHLF.GT.0.0) WRITE(21) SIG1,POLX                             
      THETA=THETA+DTHETA                                                
  200 CONTINUE                                                          
  210 CONTINUE                                                          
      IF(DELHLF.LE.0.0) GO TO 230                                       
      REWIND 21                                                         
      DO 220 N=1,NTHETA                                                 
      READ(21) TSIG(N),(POL(N,I),I=1,9)                                 
  220 CONTINUE                                                          
      CLOSE(UNIT=21)

  230 CONTINUE                                                          
C                                                                       
C     CALCULATE TOTAL INELASTIC SIGMA                                   
C                                                                       
      SIGMA=0.0                                                         
      KT=0                                                              
      IS2=-IS(2)                                                        
      DO 270 J=1,JS                                                     
      IS1=-IS(1)                                                        
      DO 260 I=1,JR                                                     
      DO 250 M=1,MPLUS                                                  
      MK=M+M-1                                                          
      IND=KT+MK                                                         
      MK=MK-1+M2K                                                       
      ML=IABS(MK+IS2-IS1)/2                                             
      DO 240 L=1,LPLUS                                                  
      LL=L-1                                                            
      IF(ML.GT.LL) GO TO 235                                            
      FACT=0.0                                                          
      TEMP=D(IND  )                                                     
      IF(ABS(TEMP).GT.1.0E-18) FACT=TEMP*TEMP                           
      TEMP=D(IND+1)                                                     
      IF(ABS(TEMP).GT.1.0E-18) FACT=FACT+TEMP*TEMP                      
      IF(FACT.LT.1.E-36) GO TO 235                                      
      FACT=ALOG(FACT)+YXFCT(LL-ML,LL+ML)                                
      FACT=EXP(FACT)/FLOAT(LL+LL+1)                                     
      IF(MK.NE.0) FACT=FACT+FACT                                        
      SIGMA=SIGMA+FACT                                                  
  235 IND=IND+IFACT                                                     
  240 CONTINUE                                                          
  250 CONTINUE                                                          
      IS1=IS1+2                                                         
      KT=KT+MPLUS+MPLUS                                                 
  260 CONTINUE                                                          
      IS2=IS2+2                                                         
  270 CONTINUE                                                          
      SIG1 =SIGMA*12.5663706*FACTA                                      
      SIG2 =SIG1 *FACTF                                                 
      WRITE(6,9002)SIG1,SIG2                                            
      IF(BANG.EQ.0.0) GO TO 330                                         
C                                                                       
C     CALCULATE SUMMED SIGMA                                            
C                                                                       
      MK=AANG/DTHETA+1.00001                                            
      ML=BANG/DTHETA+1.00001                                            
      SIG1=0.0                                                          
      DO 320 M=MK,ML                                                    
      SIG1=SIG1+TSIG(M)                                                 
  320 CONTINUE                                                          
      SIG2=SIG1*FACTF                                                   
      WRITE(6,9003)SIG1,SIG2,AANG,BANG                                  
  330 CONTINUE                                                          
      IF (ICON(9).GT.5) GO TO 500                                       
      IF (ICON(9).EQ.0) ICON(9) = 3                                     
      TEMP=ECM(2)-ECM(1)                                                
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
      IF(ICON(3).NE.2) GO TO 345                                        
      WRITE(6,9923) LTRT(1),ISTRT(1),JSAV(1),TEMP                       
      IF(NLTR.GT.1) WRITE(6,9924) (LTRT(I),ISTRT(I),JSAV(I),I=2,NLTR)   
      GO TO 350                                                         
  345 CONTINUE                                                          
      WRITE(6,9923) LTRT(IFF),ISTRT(IFF),JSAV(IFF),TEMP                 
  350 CONTINUE                                                          
      CALL DWPLOT(NTHETA,ICON(9),TSIG,ANGLE(3),ANGLE(2)                 
     &  ,5HTHETA,5HSIGMA)                                               
  500 RETURN                                                            
 9000 FORMAT(7H0 THETA,4X,3HQTR,2X,13HINEL-SIG,F**2,7X,4HPOL.,5X,3HP22,6
     &X,3HP21,6X,3HP20,2X,13HT.R.-SIG,F**2,7X,4HA.P.,5X,3HA22,6X,3HA21,6
     &X,3HA20,5X,7HSF-PROB)                                             
 9001 FORMAT(F7.2,F8.4,E14.4,2X,4F9.4,E14.4,2X,4F9.4,2X,F9.4)           
 9002 FORMAT(8H0TOT-SIG,7X,E14.4,38X,E14.4)                             
 9003 FORMAT(8H0SUM-SIG,7X,E14.4,38X,E14.4,10H    LIMITS,F8.3,5H  AND,F8
     &.3,9H  DEGREES)                                                   
 9004 FORMAT(15HINELASTIC SIGMA,5X,15A4)                                
 9010 FORMAT(F6.2,1PE11.4,9(0PF7.4))                                    
 9923 FORMAT(33H INELASTIC CROSS SECTION.  LTR = ,I2,4X,8H2*STR = ,I2,4X
     &,8H2*JTR = ,I2,4X,4HQ = ,F8.4)                                    
 9924 FORMAT(27X,6HLTR = ,I2,4X,8H2*STR = ,I2,4X,8H2*JTR = ,I2)         
 9999 FORMAT(1H1,15A4,4X,A10,2X,F7.2)                                   
      END
