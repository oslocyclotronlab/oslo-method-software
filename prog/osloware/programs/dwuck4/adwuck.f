      
      SUBROUTINE ADWUCK                                                 
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  U(800,2),V(800,2),SPACE(3200),UB(800)             
      COMMON /LDEPEN/ ULD(800,2),ILDSAV(8),ILD,IDSOSV(8),IDSO           
      COMMON /ISOSPN/ TWBF(800),TT,TR,CANAL,CCORE                       
      COMMON /UBOUND/ DY,YMAX,EISIG0(2)                                 
      DIMENSION DTEMP(6400),E(600),TLJ(6),VB(800)                       
      EQUIVALENCE (U(1,1),DTEMP(1)), (DTEMP(3801),E(1))                 
C                                                                       
      IF(ICON(1).EQ.0) GO TO 6                                          
C                                                                       
C     READ CARD SET 2                                                   
C                                                                       
      READ (5,9001)ANGLE                                                
      IF(ANGLE(2).LE.0.0) ANGLE(2)=0.0                                  
      IF(ANGLE(3).LE.0.0) ANGLE(3)=5.0                                  
      IF(ICON(1).EQ.1) GO TO 5                                          
      IF(ANGLE(1).GE.ANGLE(2)) GO TO 4                                  
      ST=ANGLE(1)                                                       
      ANGLE(1)=ANGLE(2)                                                 
      ANGLE(2)=ST                                                       
    4 CONTINUE                                                          
      IF(ANGLE(1).LE.0.0) ANGLE(1)=180.0                                
      NTHETA=(ANGLE(1)-ANGLE(2))/ANGLE(3)+1.1                           
      ANGLE(1)=NTHETA                                                   
      GO TO 6                                                           
    5 CONTINUE                                                          
      IF(ANGLE(1).LE.0.0) ANGLE(1)=37.0                                 
    6 CONTINUE                                                          
      WRITE(6,9010)ANGLE                                                
C                                                                       
C     READ CARD SET 3                                                   
C                                                                       
      READ (5,9002)L,NLTR,(LTRT(I),I=1,NLTR),(JTRT(I),I=1,NLTR)         
      WRITE(6,9008) L   ,(LTRT(I),I=1,NLTR)                             
      WRITE(6,9009) NLTR,(JTRT(I),I=1,NLTR)                             
C                                                                       
C     READ CARD SET 4                                                   
C                                                                       
      READ(5,9001) DRF,RZ,RMAX,VCE,FNRNG,AMASS,DY,YMAX,TT,TR            
      IF(ABS(RMAX).GT.ABS(RZ)) GO TO 16                                 
      R=RZ                                                              
      RZ=RMAX                                                           
      RMAX=R                                                            
   16 CONTINUE                                                          
      IF(DRF.LE.0.0) DRF=0.1                                            
      IF(RZ.LE.0.0) RZ=0.0                                              
      IF(RMAX.EQ.0.0) RMAX=20.0                                         
      IF(AMASS.LT.0.0) AMASS=0.0                                        
      WRITE(6,9011)DRF,RZ,RMAX,VCE,FNRNG                                
      IF(AMASS.NE.0.0) WRITE(6,9012) AMASS                              
      IF(DY.NE.0.0) WRITE(6,9013) DY,YMAX                               
      IF((TT.NE.0.0).OR.(TR.NE.0.0)) WRITE(6,9014) TT,TR                
      KZ=RZ/DRF+1.0E-04                                                 
      K=ABS(RMAX)/DRF+1.0E-04                                           
      KC=K                                                              
      KC=MIN0(KC,400)                                                   
      LPLUS=IABS(L)+1                                                   
      LC=LPLUS                                                          
      IND=0                                                             
      INCR=0                                                            
      IBF(5)=0                                                          
      IBF(6)=0                                                          
      IF(((ICON(3).EQ.2).OR.(ICON(3).EQ.4))                             
     &  .AND.(ICON(16).EQ.2)) ICON(16) = 1                              
      IF(ICON(19).EQ.0) GO TO 7                                         
      ICON( 2)=0                                                        
      ICON(16)=0                                                        
    7 CONTINUE                                                          
      WRITE(6,9504)                                                     
C                                                                       
C     READ IN DISTORTED WAVE INFORMATION                                
C                                                                       
      IBF(4)=0                                                          
      CALL FORMF(U(1,1),V(1,1),1,0)                                     
      IF ((IBF(4).NE.0).AND.(ICON(16).EQ.0)) IBF(5)=1                   
      IBF(4)=0                                                          
      CALL FORMF(U(1,2),V(1,2),2,0)                                     
      IF ((IBF(4).NE.0).AND.(ICON(16).EQ.0)) IBF(6)=1                   
C                                                                       
C     FORM FACTOR LOOP                                                  
C                                                                       
      DO 35 II=1,NLTR                                                   
      IBF(4)=0                                                          
      IFF=II                                                            
      IF(ICON(2).EQ.1) GO TO 28                                         
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
      IF(ICON(3).NE.0) GO TO 8                                          
      IF(II     .NE.1) GO TO 12                                         
    8 CONTINUE                                                          
      IF(ICON(2).EQ.2) GO TO 9                                          
      WRITE(6,9503)                                                     
      IF(ICON(19).GT.3) GO TO 88                                        
      IF(ICON(19).EQ.0) GO TO 88                                        
      CALL FORMF(TWBF,VB,3,LTRT(II))                                    
   88 CONTINUE                                                          
C                                                                       
      CALL FORMF(UB,VB,3,LTRT(II))                                      
      IDSO=IBF(4)                                                       
C                                                                       
      GO TO 11                                                          
    9 CONTINUE                                                          
      DO 10 I=1,800                                                     
      UB(I)=0.0                                                         
   10 CONTINUE                                                          
   11 IF(ICON(2).EQ.0) GO TO 12                                         
      CALL CATHEN(LTRT(II),JTRT(II),DTEMP(3201),DTEMP(4801),UB)         
   12 CONTINUE                                                          
      ISTRT(II)=IS(3)                                                   
      IF((ICON(4).EQ.1).OR.(ICON(4).EQ.4)) GO TO 18                     
      IWRITE=1                                                          
      GO TO 3004                                                        
   18 CONTINUE                                                          
      IF(ICON(14).EQ.0) GO TO 19                                        
      IPNCH=(ICON(14)+2)/3                                              
      IWRITE=1                                                          
      IF(IPNCH.NE.2) GO TO 3100                                         
   19 CONTINUE                                                          
      IF(ICON(3).NE.0) GO TO 20                                         
      IF(II     .NE.1) GO TO 27                                         
   20 CONTINUE                                                          
C                                                                       
      CALL FNLOC(U,UB,VB,LTRT(II))                                      
C                                                                       
      IF(ICON(11).EQ.0) GO TO 25                                        
      IWRITE=2                                                          
      GO TO 3003                                                        
   25 CONTINUE                                                          
      IF(ICON(14).LT.4) GO TO 27                                        
      IWRITE=2                                                          
      IPNCH=(ICON(14)+2)/3                                              
      GO TO 3101                                                        
   27 CONTINUE                                                          
      WRITE(3) UB                                                       
      IF(ICON(19).NE.0) WRITE(3) TWBF                                   
      IF(IDSO.NE.0) WRITE(3) VB                                         
      GO TO 29                                                          
C                                                                       
C     SKIP TO HERE IF NO NEW FORM FACTOR                                
C                                                                       
   28 CONTINUE                                                          
      ILD=ILDSAV(II)                                                    
      IDSO=IDSOSV(II)                                                   
   29 CONTINUE                                                          
      IS(3)=ISTRT(II)                                                   
      ILDSAV(II)=ILD                                                    
      IDSOSV(II)=IDSO                                                   
      JSAV(II)=JTRT(II)                                                 
      IF((IS(3).EQ.0).OR.(ICON(16).EQ.2)) JTRT(II)=LTRT(II)+LTRT(II)    
      INC=LTRT(II)+1                                                    
      IND=MAX0(INC,IND)                                                 
      INCR=MAX0(INCR,JTRT(II))                                          
C                                                                       
C     CHECK ON SPINS AND STATISTICS                                     
C                                                                       
      IF(ICON(16).EQ.2) GO TO 35                                        
      LTR=LTRT(II)                                                      
      JTR=JTRT(II)                                                      
      IF(PHASEF(IS(1)+IS(2)+IS(3)).LT.0.0) GO TO 33                     
      IF(PHASEF(LTR+LTR+JTR+IS(3)).LT.0.0) GO TO 33                     
      GO TO 35                                                          
   33 CONTINUE                                                          
C                                                                       
C     SET ERROR FLAG                                                    
C                                                                       
      IBF(3)=1                                                          
      WRITE(6,9906) IS,LTR,JTR                                          
   35 CONTINUE                                                          
      ENDFILE 3                                                         
      REWIND 3                                                          
      IF(NLTR.GT.8) IBF(3)=1                                            
C                                                                       
C     CHECK ON COMPATIBILITY OF LMAX AND STORAGE                        
C                                                                       
      IF (ICON(16).LT.2) GO TO 50                                       
      FS(1) = 0.                                                        
      FS(2) = 0.                                                        
      IS(1) = 0                                                         
      IS(2) = 0                                                         
      NS(1) = 1                                                         
      NS(2) = 1                                                         
      FS(3)=0.                                                          
      IS(3)=0                                                           
      NS(3)=1                                                           
   50 CONTINUE                                                          
      IF(L.LT.0) LPLUS=LC                                               
C                                                                       
C     MAXIMUM NO. OF PARTIAL WAVES = 150 FOR EACH CHANNEL.              
C     NUMBER OF R.M.E. = 2*LPLUS*NS1*NS2*(LTR+1) MUST BE .LT. 4000.     
C     NUMBER OF BETAS  = 2*LPLUS*NS1*NS2*(JTR+1).                       
C                                                                       
      LPLUS=MIN0(LPLUS,300/(NS(1)+NS(2)),4000/(NS(1)*NS(2)*IND))        
      IF(ICON(19).NE.0) LPLUS=MIN0(LPLUS,51)                            
      L=LPLUS-1                                                         
      LPL2=LPLUS+LPLUS                                                  
      IF (RMAX.LT.0.0) K=KC                                             
      K=2*(K/2)                                                         
      WRITE(6,9506)L,K,NLTR                                             
      IBUF=LPL2*IND+1                                                   
      INC=(INCR+IS(1)+IS(2))/2+1                                        
      IBUF=MAX0(IBUF,INC*LPLUS+1861)                                    
      IBF(1)=IBUF                                                       
      IF(ICON(16).EQ.2) GO TO 52                                        
      IBF(7)=IS(1)                                                      
      IF (IBF(5).NE.0) GO TO 51                                         
      FS(1)=0.                                                          
      IS(1)=0                                                           
      NS(1)=1                                                           
   51 CONTINUE                                                          
      IBF(8)=IS(2)                                                      
      IF (IBF(6).NE.0) GO TO 52                                         
      FS(2)=0.                                                          
      IS(2)=0                                                           
      NS(2)=1                                                           
   52 CONTINUE                                                          
      IF(ICON(15).EQ.0) GO TO 56                                        
C                                                                       
C     PRINT OUT K(R)**2 FOR DISTORTED WAVES                             
C                                                                       
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
      DO 55 I=1,2                                                       
      WRITE(6,9057) I                                                   
      IND=0                                                             
      R=DR(I)                                                           
      DO 54 M=1,K,5                                                     
      MK=M+M-1                                                          
      MK4=MIN0(MK +9,K+K)                                               
      WRITE(6,9052) R,(U(J,I),J=MK,MK4)                                 
      R=R+5.0*DR(I)                                                     
   54 CONTINUE                                                          
   55 CONTINUE                                                          
   56 CONTINUE                                                          
C                                                                       
      CALL INTEG                                                        
C                                                                       
      IF(ICON(5).EQ.0) GO TO 70                                         
C                                                                       
C     WRITE ELASTIC PARTIAL WAVE SCATTERING AMPLITUDES                  
C                                                                       
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
      IF(ICON(5).EQ.2) GO TO 65                                         
      WRITE(6,9600)                                                     
      WRITE(6,9601)                                                     
      DO 60 LL=1,LPLUS                                                  
      LX=LL-1                                                           
      INC=LX+LL                                                         
      IND=INC+LPL2*(NS(1)-1)                                            
      WRITE(6,9602)LX,(E(INDEX  ),E(INDEX+1),INDEX=INC,IND,LPL2)      
      INC=IND+LPL2                                                      
      IND=INC+LPL2*(NS(2)-1)                                            
      WRITE(6,9603)LX,(E(INDEX  ),E(INDEX+1),INDEX=INC,IND,LPL2)      
   60 CONTINUE                                                          
      IF(ICON(5).EQ.1) GO TO 70                                         
      IF(LPLUS-27) 62,62,63                                             
   62 CONTINUE                                                          
      WRITE(6,9604)                                                     
      GO TO 65                                                          
   63 CONTINUE                                                          
      WRITE(6,9605)                                                     
   65 CONTINUE                                                          
      WRITE(6,9606)                                                     
      WRITE(6,9607)                                                     
      DO 68 LL=1,LPLUS                                                  
      LX=LL-1                                                           
      INC=LX+LL                                                         
      IND=INC+LPL2*(NS(1)-1)                                            
      M=0                                                               
      DO 66 INDEX=INC,IND,LPL2                                          
      M=M+2                                                             
      SR=1.0-2.0*E(INDEX+1)                                             
      SI=    2.0*E(INDEX  )                                             
      TLJ(M-1)=SQRT(SR*SR+SI*SI)                                        
      TLJ(M  )=57.29578*ATAN2(SI,SR)                                    
      IF(TLJ(M  ).LT.0.0) TLJ(M  )=360.0+TLJ(M  )                       
      IF(ABS(TLJ(M-1)-1.0).GE.1.0E-6) GO TO 66                          
      TLJ(M-1)=1.0                                                      
      TLJ(M  )=0.0                                                      
   66 CONTINUE                                                          
      WRITE(6,9608) LX,(TLJ(I),I=1,M)                                   
      INC=IND+LPL2                                                      
      IND=INC+LPL2*(NS(2)-1)                                            
      M=0                                                               
      DO 67 INDEX=INC,IND,LPL2                                          
      M=M+2                                                             
      SR=1.0-2.0*E(INDEX+1)                                             
      SI=    2.0*E(INDEX  )                                             
      TLJ(M-1)=SQRT(SR*SR+SI*SI)                                        
      TLJ(M  )=57.29578*ATAN2(SI,SR)                                    
      IF(TLJ(M  ).LT.0.0) TLJ(M  )=360.0+TLJ(M  )                       
      IF(ABS(TLJ(M-1)-1.0).GE.1.0E-6) GO TO 67                          
      TLJ(M-1)=1.0                                                      
      TLJ(M  )=0.0                                                      
   67 CONTINUE                                                          
      WRITE(6,9609) LX,(TLJ(I),I=1,M)                                   
   68 CONTINUE                                                          
   70 CONTINUE                                                          
      IF(ICON(12).EQ.0) GO TO 80                                        
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
C                                                                       
      CALL TAPED                                                        
C                                                                       
   80 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C     FORM FACTOR PRINT OUT                                             
C                                                                       
 3003 CONTINUE                                                          
      CALL CUDATE(BETA)                                                 
      WRITE(6,9999) ALPHA,BETA                                          
 3004 CONTINUE                                                          
      IF(ICON(19).EQ.0) GO TO 3014                                      
      R=DR(3)                                                           
      WRITE(6,9110) LTRT(IFF),ISTRT(IFF),JTRT(IFF)                      
      WRITE(6,9101)                                                     
      DO 3013 M=1,K,5                                                   
      MK=M+M-1                                                          
      MK4=MIN0(MK+9,K+K)                                                
      WRITE(6,9052) R,(TWBF(N),N=MK,MK4)                                
      R=R+5.0*DR(3)                                                     
 3013 CONTINUE                                                          
 3014 CONTINUE                                                          
      R=DR(3)                                                           
      WRITE(6,9100) LTRT(IFF),ISTRT(IFF),JTRT(IFF)                      
      WRITE(6,9101)                                                     
      DO 3006 M=1,K,5                                                   
      MK=M+M-1                                                          
      MK4=MIN0(MK+9,K+K)                                                
      WRITE(6,9052)R ,(UB(N),N=MK,MK4)                                  
      R=R+5.0*DR(3)                                                     
 3006 CONTINUE                                                          
      IF(IDSO.EQ.0) GO TO 3010                                          
      R=DR(3)                                                           
      WRITE(6,9102)                                                     
      WRITE(6,9101)                                                     
      DO 3008 M=1,K,5                                                   
      MK=M+M-1                                                          
      MK4=MIN0(MK+9,K+K)                                                
      WRITE(6,9052) R,(VB(N),N=MK,MK4)                                  
      R=R+5.0*DR(3)                                                     
 3008 CONTINUE                                                          
 3010 CONTINUE                                                          
      GO TO(18,25),IWRITE                                               
 3100 CONTINUE                                                          
      WRITE(7,3191) ALPHA                                               
      GO TO 3110                                                        
 3101 CONTINUE                                                          
      WRITE(7,3192) ALPHA                                               
 3110 CONTINUE                                                          
      K2=K+K                                                            
      PTS=K                                                             
      ST=1.0                                                            
      IPNCH=ICON(14)-3*(IPNCH-1)                                        
      IF(IPNCH.EQ.2) GO TO 3120                                         
      WRITE(7,9001) PTS                                                 
      WRITE(7,3190) (UB(I),I=1,K2,2)                                    
      IF(IPNCH.EQ.1) GO TO (19,27),IWRITE                               
 3120 WRITE(7,9001) PTS,ST                                              
      WRITE(7,3190) (UB(I),I=2,K2,2)                                    
      GO TO (19,27),IWRITE                                              
 3190 FORMAT(5E16.7)                                                    
 3191 FORMAT(20HF.F. BEFORE FRNL    ,15A4)                              
 3192 FORMAT(20HF.F. AFTER FRNL     ,15A4)                              
 9001 FORMAT(10F8.4)                                                    
 9002 FORMAT(18I3)                                                      
 9008 FORMAT(18H0CARD SET 3 DATA  ,9H   LMAX =,I4,8X,6H  LTR=,8I4)      
 9009 FORMAT(18H                  ,9H   NLTR =,I4,8X,6H2*JTR=,8I4)      
 9010 FORMAT(18H0ANGLE DATA        ,9H   THETN=,F9.4,9H   THET1=,F9.4,9H
     &   DTHET=,F9.4,9H   A-ANG=,F9.4,9H   B-ANG=,F9.4,9H   DELHF=,F9.4)
 9011 FORMAT(18H0CARD SET 4 DATA   ,9H   DRF  =,F9.4,9H   RZ   =,F9.4,9H
     &   RMAX =,F9.4,9H   VCE  =,F9.4,9H   FNRNG=,F9.4)                 
 9012 FORMAT(1H+,107X,9H   AMASS=,F9.4)                                 
 9013 FORMAT(18X,9H   DY   =,F9.4,9H   YMAX =,F9.4)                     
 9014 FORMAT(18X,9H   TT   =,F9.4,9H   TR   =,F9.4)                     
 9052 FORMAT(1H ,F6.2,10(0PE12.4))                                      
 9057 FORMAT(27H0K(R)**2 FOR DISTORTED WAVE,I2)                         
 9100 FORMAT(13H0 FORM FACTOR,10X,5HLTR =,I2,4X,7H2*STR =,I2,4X,8H2*JTR 
     &= ,I2)                                                            
 9101 FORMAT(132H0    R    RL,R        IM,R        RL,R+DR     IM,R+DR  
     &   RL,R+DR*2   IM,R+DR*2   RL,R+DR*3   IM,R+DR*3   RL,R+DR*4   IM,
     &R+DR*4       )                                                    
 9102 FORMAT(21H0SPIN-ORBIT POTENTIAL)                                  
 9110 FORMAT(14H0 TFORM FACTOR,9X,5HLTR =,I2,4X,7H2*STR =,I2,4X,8H2*JTR 
     &= ,I2)                                                            
 9503 FORMAT(18H0FORM FACTOR DATA )                                     
 9504 FORMAT(18H PARTICLE DATA    )                                     
 9506 FORMAT(18X,9H   LMAX =,I4,14H        NSTEP=,I4,14H        NLTR =,I
     &4)                                                                
 9600 FORMAT(1H0,35HPARTIAL WAVE SCATTERING AMPLITUDES )                
 9601 FORMAT(4H   L,20H  REAL D1   IMAG D1 ,20H  REAL D2   IMAG D2 ,20H 
     & REAL D3   IMAG D3 ,4X,4H   L,20H  REAL D1   IMAG D1 ,20H  REAL D2
     &   IMAG D2 ,20H  REAL D3   IMAG D3 )                              
 9602 FORMAT(1H ,    I3,6F10.7)                                         
 9603 FORMAT(1H+,68X,I3,6F10.7)                                         
 9604 FORMAT(1H0)                                                       
 9605 FORMAT(1H1)                                                       
 9606 FORMAT(1H ,29HS-MATRIX AMPLITUDES AND PHASE)                      
 9607 FORMAT(1H ,2(3H  L,4X,4HETA1,6X,6HPHASE1,4X,4HETA2,6X,6HPHASE2,4X,
     &4HETA3,6X,6HPHASE3,5X))                                           
 9608 FORMAT(1H ,    I3,3(F10.7,F10.2))                                 
 9609 FORMAT(1H+,68X,I3,3(F10.7,F10.2))                                 
 9906 FORMAT(1H0,28HSPIN STATISTICS NOT CORRECT ,7H 2*IS1=,I3,7H 2*IS2=,
     &I3,7H 2*IS3=,I3,7H   LTR=,I3,7H 2*JTR=,I3)                        
 9999 FORMAT(1H1,15A4,4X,A10,2X,F7.2)                                   
      END
