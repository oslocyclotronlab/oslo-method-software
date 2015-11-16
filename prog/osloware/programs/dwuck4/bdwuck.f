      
      SUBROUTINE BDWUCK                                                 
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  DTEMP(6400),UB(800)                               
      COMMON /LDEPEN/ ULD(800,2),ILDSAV(8),ILD,IDSOSV(8),IDSO           
      COMMON /ISOSPN/ TWBF(800),TT,TR,CANAL,CCORE                       
      DIMENSION SIGSAV(181),VB(800),RELF(2)                             
      EQUIVALENCE (ULD(601,1),SIGSAV(1)), (ULD(1,2),VB(1))              
      DATA FOURPI/12.5663706/                                           
      IBUF=IBF(1)                                                       
      NTHETA=ANGLE(1)                                                   
      NTHETA=MIN0(NTHETA,181)                                           
      ICON3=ICON(3)                                                     
      IF(ICON3.GT.2) ICON(3)=1                                          
      ICO=0                                                             
      DO 60 I=1,NTHETA                                                  
      SIGSAV(I)=0.0                                                     
   60 CONTINUE                                                          
C                                                                       
C     PRINT ELASTIC CROSS-SECTIONS IF ICON(6).NE.0                      
C                                                                       
      IF(ICON(6).EQ.0) GO TO 65                                         
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999)ALPHA,BETA                                           
      WRITE(6,9904)                                                     
C                                                                       
      CALL ELSIG                                                        
C                                                                       
   65 CONTINUE                                                          
      RZ=FLOAT(KZ)*DRF                                                  
      RMAX=FLOAT(K)*DRF                                                 
      JR=NS(1)                                                          
      JS=NS(2)                                                          
      DO 101 II=1,NLTR                                                  
      LTR=LTRT(II)                                                      
      MPLUS=LTR+1                                                       
      ILD=ILDSAV(II)                                                    
      IDSO=IDSOSV(II)                                                   
      READ(3)UB                                                         
      IF(ICON(19).NE.0) READ(3) TWBF                                    
      IF(IDSO.NE.0) READ(3) VB                                          
C                                                                       
      CALL RADINT(ULD(1,1),LTR)                                         
C                                                                       
      IF(ICON(7).EQ.0) GO TO 100                                        
C                                                                       
C     WRITE RADIAL MATRIX ELEMENTS IF ICON(7).NE.0                      
C                                                                       
      INCR=LPL2*LTR                                                     
      INC=1                                                             
      IS1=-IS(1)                                                        
      DO 99 I=1,JR                                                      
      IS2=-IS(2)                                                        
      DO 98 J=1,JS                                                      
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999)ALPHA,BETA                                           
      WRITE(6,9900)IS2,IS1                                              
      WRITE(6,9901)                                                     
      DO 90 LL=1,LPLUS                                                  
      LM=LL-1                                                           
      IND=INC+INCR                                                      
      WRITE(6,9902)LM,(DTEMP(INDEX),DTEMP(INDEX+1),INDEX=INC,IND,LPL2)
      INC=INC+2                                                         
   90 CONTINUE                                                          
      INC=INC+INCR                                                      
      IS2=IS2+2                                                         
   98 CONTINUE                                                          
      IS1=IS1+2                                                         
   99 CONTINUE                                                          
  100 CONTINUE                                                          
  101 CONTINUE                                                          
      END FILE 8                                                        
      REWIND 8                                                          
C                                                                       
C     RESTORE SPIN STORAGE                                              
C                                                                       
      IF (ICON(16).EQ.2) GO TO 103                                      
      IS(1)=IBF(7)                                                      
      NS(1)=IS(1)+1                                                     
      FS(1)=FLOAT(IS(1))/2.                                             
      JR=NS(1)                                                          
      IS(2)=IBF(8)                                                      
      NS(2)=IS(2)+1                                                     
      FS(2)=FLOAT(IS(2))/2.                                             
      JS=NS(2)                                                          
  103 CONTINUE                                                          
      DO 201 II=1,NLTR                                                  
      IFF=II                                                            
      LTR=LTRT(II)                                                      
      JTR=JTRT(II)                                                      
      IS(3)=ISTRT(II)                                                   
      RENORM=1.                                                         
      IF(ICON(16).LT.2) GO TO 104                                       
      FLTR=LTRT(II)                                                     
      FJTR=JSAV(II)                                                     
      FSTR=ISTRT(II)                                                    
      IS(3)=0                                                           
      RENORM=(FJTR+1.)/((2.*FLTR+1.)*(FSTR+1.))                         
  104 CONTINUE                                                          
C                                                                       
C     CALCULATE NORMALIZATION FACTOR                                    
C                                                                       
      FL=1.0                                                            
      FACT=     2.0*FMU(1)/(HBARC*FK(1))**2                             
      FACT=FACT*2.0*FMU(2)/(HBARC*FK(2))**2                             
      FACT=FACT*AMU*AMU/FOURPI                                          
      FACT=FACT*FK(2)/FK(1)                                             
      FACT=FACT*RENORM                                                  
      IF(ICON(10).LT.2) GO TO 110                                       
      DO 108 N=1,2                                                      
      FM1=FM(N) *AMU                                                    
      FM2=FMA(N)*AMU                                                    
      IF(ICON(10).EQ.3) GO TO 105                                       
      E1=ECM(N)*FMU(N)/FM(N)+FM1                                        
      E2=ECM(N)*FMU(N)/FMA(N)                                           
      E2=E2/AINT(FMA(N)+0.2)+FM1                                        
      E1E2=E1*E2                                                        
      EPS=ECM(N)*FM(N)/FMU(N)+FM1                                       
      GO TO 107                                                         
  105 CONTINUE                                                          
      T1=ECM(N)+FM1+FM2                                                 
      EPS=(T1*T1-FM1*FM1-FM2*FM2)/(2.0*FM2)                             
      E1E2=FMU(N)*AMU*T1/AINT(FMA(N)+0.2)                               
  107 E0SQ=0.5*FM1*(EPS+FM1)                                            
      RELF(N)=E0SQ/E1E2                                                 
  108 CONTINUE                                                          
      FACT=FACT*((RELF(1)+RELF(2))/2.0)**2                              
  110 CONTINUE                                                          
      T1=AMAX1(DR(1),DR(2))                                             
      T2=AMIN1(DR(1),DR(2))                                             
      FACT=FACT*(T1*T1/(DR(3)*T2))**2                                   
      I1=ABS(FM(1)-FM(2))+0.1                                           
      IF(I1.EQ.0) GO TO 112                                             
      FACT=FACT*1.0E+04                                                 
      FL=SQRT(FLOAT(LTR+LTR+1))                                         
  112 CONTINUE                                                          
      FN=(II-1)*ICON(3)*(ICON(3)-1)                                     
C                                                                       
      CALL BETAFN(DTEMP(   1),DTEMP(IBUF),LTR,JTR)                      
C                                                                       
      MPLUS=JTR/2+1                                                     
      IF(ICON(8).EQ.0) GO TO 120                                        
      IF(ICO.GT.0) GO TO 120                                            
C                                                                       
C     WRITE BETA TABLES IF ICON(8).NE.0                                 
C                                                                       
      I1=MPLUS+MPLUS                                                    
      IFACT=I1     *NS(1)*NS(2)                                         
      KT=0                                                              
      IS2=-IS(2)                                                        
      DO 118 J=1,JS                                                     
      IS1=-IS(1)                                                        
      DO 116 I=1,JR                                                     
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999)ALPHA,BETA                                           
      WRITE(6,9905)IS2,IS1                                              
      WRITE(6,9903)                                                     
      DO 114 LL=1,LPLUS                                                 
      LM=LL-1                                                           
      IND=KT+LM*IFACT+IBUF                                              
      INDF=IND+I1-1                                                     
      WRITE(6,9902)LM,(DTEMP(INDEX),INDEX=IND,INDF)                     
  114 CONTINUE                                                          
      KT=KT+I1                                                          
      IS1=IS1+2                                                         
  116 CONTINUE                                                          
      IS2=IS2+2                                                         
  118 CONTINUE                                                          
  120 CONTINUE                                                          
      IF(FN.NE.0.0) GO TO 135                                           
C                                                                       
C     SET HEADINGS FOR INELASTIC SIGMA                                  
C                                                                       
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999)ALPHA,BETA                                           
      WRITE(6,9507)DRF,RZ,RMAX,VCE,FS(1)                                
      WRITE(6,9508)FNRNG,PNLOC,FS(2)                                    
  135 CONTINUE                                                          
      TEMP=ECM(2)-ECM(1)                                                
      WRITE(6,9505)LTR,JSAV(II),ISTRT(II),TEMP                          
      IF(ICON(3).EQ.0) GO TO 140                                        
      IF(ICON(3).EQ.1) GO TO 140                                        
      IF(II.EQ.NLTR) GO TO 140                                          
      GO TO 201                                                         
  140 CONTINUE                                                          
C                                                                       
      CALL INSIG(DTEMP(IBUF),DTEMP(1861),JTR,FACT)                      
      DO 180 I=1,NTHETA                                                 
      SIGSAV(I)=SIGSAV(I)+DTEMP(I+50)                                   
  180 CONTINUE                                                          
C                                                                       
  201 CONTINUE                                                          
      REWIND 8                                                          
      IF(ICON3.LE.2) GO TO 300                                          
      IF(ICO.EQ.1) GO TO 300                                            
      IF(ICON(9).GT.5) GO TO 220                                        
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
      WRITE(6,9923) LTRT(1),ISTRT(1),JSAV(1),TEMP                       
      IF(NLTR.GT.1) WRITE(6,9924) (LTRT(I),ISTRT(I),JSAV(I),I=2,NLTR)   
      CALL DWPLOT(NTHETA,ICON(9),SIGSAV(1),ANGLE(3),ANGLE(2)            
     &  ,5HTHETA,5HSIGMA)                                               
  220 CONTINUE                                                          
      IF(ICON3.LE.3) GO TO 300                                          
      ICON(3)=2                                                         
      ICO=1                                                             
      GO TO 103                                                         
  300 CONTINUE                                                          
      RETURN                                                            
 9505 FORMAT(18H ANG MOM TRANSFER ,9H   LTR  =,I4,14H.       2*JTR=,I4, 
     &14H.       2*STR=,I4,14H.       Q    =,F9.4)                      
 9507 FORMAT(18H0BASIC DATA       ,9H   DR   =,F9.4,9H   RMIN =,F9.4,9H 
     &  RMAX =,F9.4,9H   COUEX=,F9.4,9H     IS1=,F9.4)                   
 9508 FORMAT(18X,9H   FNRNG=,F9.4,9H   PNLC1=,F9.4,9H   PNLC2=,F9.4,9H  
     & PNLC3=,F9.4,9H     IS2=,F9.4)                                     
 9900 FORMAT(24H0 RADIAL MATRIX ELEMENTS ,9H,  J2=L2+,I2,2H/2,9H,  J1=L1
     &+,I2,2H/2 )                                                       
 9901 FORMAT(20H0 L2  F(L2,/L2-LTR/),10X,16HF(L2,/L2-LTR/+2),8X,16HF(L2,
     &/L2-LTR/+4),8X,16HF(L2,/L2-LTR/+6),8X,16HF(L2,/L2-LTR/+8))        
 9902 FORMAT(I4,10E12.5/(4X,10E12.5))                                   
 9903 FORMAT(16H0 L2  BETA(L2,0),14X,10HBETA(L2,1),14X,10HBETA(L2,2),14X
     &,10HBETA(L2,3),14X,10HBETA(L2,4))                                 
 9904 FORMAT(1H0,32HELASTIC SCATTERING CROSS-SECTION )                  
 9905 FORMAT(1H0,8H    MS2=,I2,2H/2,8H    MS1=,I2,2H/2)                 
 9923 FORMAT(30H INCOHERENT SIGMA SUM.  LTR = ,I2,4X,8H2*STR = ,I2,4X,8H
     &2*JTR = ,I2,4X,4HQ = ,F8.4)                                       
 9924 FORMAT(24X,6HLTR = ,I2,4X,8H2*STR = ,I2,4X,8H2*JTR = ,I2)         
 9999 FORMAT(1H1,15A4,4X,A10,2X,F7.2)                                   
      END
