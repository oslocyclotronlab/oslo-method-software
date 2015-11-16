      
      SUBROUTINE ELSIG                                                  
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  DTEMP(7200)                                       
      DIMENSION SUM(5,5,2),SIG(2),RATIO(2),PLM(450),SUMX(50)            
     &         ,POL(10),TEMP(2),INCR(2),D(600),SIGPLT(181,2)            
      EQUIVALENCE (SUM(1,1,1),DTEMP(3101)), (SUMX(1),DTEMP(3101))       
     &           ,(DTEMP(3201),D(1)), (DTEMP(3801),SIGPLT(1,1))         
     &           ,(PLM(1),DTEMP(4201))                                  
      EQUIVALENCE (ANGLE(1),THETAN),(ANGLE(2),THETA1),(ANGLE(3),DTHETA) 
      DATA NX/181/                                                      
      NTHETA=THETAN                                                     
      IF(NTHETA.EQ.0) GO TO 101                                         
C                                                                       
C     TABLE SCATTERING AMPLITUDES WITH ANG-MOM COEFFICENTS              
C                                                                       
      ICNT=LPL2*(NS(1)**2+NS(2)**2)                                     
      DO 3 IP=1,ICNT                                                    
      DTEMP(IP)=0.0                                                     
    3 CONTINUE                                                          
      JR=1                                                              
      JS=0                                                              
      ICNT=0                                                            
      IC1=0                                                             
      DO 26 N=1,2                                                       
      ISS=NS(N)                                                         
      JS=JS+ISS                                                         
      INCR(N)=LPLUS*ISS*(ISS+1)                                         
      FL=1.0                                                            
      IS2=IS(N)                                                         
      DO 25 LL=1,LPLUS                                                  
      L=LL-1                                                            
      LK=LL+LL-1                                                        
      L2=LK-1                                                           
      J2=L2-IS2                                                         
      IQ=LK+IC1                                                         
      DO 20 JJ=JR,JS                                                    
      IP=LK+ICNT                                                        
      MS2=-IS2                                                          
      DO 15 I=1,ISS                                                     
      VCI=VCC(L2,IS2,J2,0,MS2)*FL                                       
      MT2=MS2                                                           
      DO 14 J=I,ISS                                                     
      ML2=MS2-MT2                                                       
      ML=IABS(ML2/2)                                                    
      IF(ML.GT.L) GO TO 13                                              
      VTEMP=VCI*VCC(L2,IS2,J2,ML2,MT2)*EXP(0.5*YXFCT(L+ML,L-ML))        
      DTEMP(IP  )=DTEMP(IP  )+D(IQ  )*VTEMP                             
      DTEMP(IP+1)=DTEMP(IP+1)+D(IQ+1)*VTEMP                             
   13 CONTINUE                                                          
      IP=IP+LPL2                                                        
      MT2=MT2+2                                                         
   14 CONTINUE                                                          
      MS2=MS2+2                                                         
   15 CONTINUE                                                          
      J2=J2+2                                                           
      IQ=IQ+LPL2                                                        
   20 CONTINUE                                                          
      FL=FL+2.0                                                         
   25 CONTINUE                                                          
      ICNT=ICNT+INCR(N)                                                 
      IC1=IC1+LPL2*ISS                                                  
      JR=JS+1                                                           
   26 CONTINUE                                                          
C                                                                       
C     CALCULATE ELASTIC CROSS SECTION                                   
C                                                                       
      IMAX=MAX0(NS(1),NS(2))                                            
      THETA=THETA1                                                      
      WRITE(6,9000)                                                     
      IF (ICON(13).GE.2) WRITE(7,9004) ALPHA                            
      DO 100 NTH=1,NTHETA                                               
      CALL LGNDR(PLM,IMAX,LPLUS,THETA)                                  
      ARG=THETA/114.591559                                              
      S=SIN(ARG)**2                                                     
      IF(S.LT.1.E-10) GO TO 28                                          
      TL=ALOG(S)                                                        
   28 CONTINUE                                                          
      ICNT=0                                                            
      DO 80 N=1,2                                                       
      ISS=NS(N)                                                         
      DO 30 I=1,50                                                      
      SUMX(I)=0.0                                                       
   30 CONTINUE                                                          
      DO 40 LL=1,LPLUS                                                  
      LK=LL+LL-1                                                        
      IP=LK+ICNT                                                        
      DO 35 I=1,ISS                                                     
      DO 34 J=I,ISS                                                     
      ML=(J-I)*LPLUS+LL                                                 
      VTEMP=PLM(ML)                                                     
      SUM(I,J,1)=SUM(I,J,1)+DTEMP(IP  )*VTEMP                           
      SUM(I,J,2)=SUM(I,J,2)+DTEMP(IP+1)*VTEMP                           
      IP=IP+LPL2                                                        
   34 CONTINUE                                                          
   35 CONTINUE                                                          
   40 CONTINUE                                                          
C                                                                       
C     CALCULATE COULOMB AMPLITUDE                                       
C                                                                       
      IF(S.GE.1.E-10) GO TO 42                                          
      CL=0.0                                                            
      SL=0.0                                                            
      GO TO 50                                                          
   42 CONTINUE                                                          
      ARG=ETA(N)*TL                                                     
      FACT=ETA(N)/(2.0*FK(N)*S)                                         
      CL=COS(ARG)*FACT                                                  
      SL=SIN(ARG)*FACT                                                  
   50 CONTINUE                                                          
      SIG(N)=0.0                                                        
      POL(N)=0.0                                                        
      DO 60 I=1,ISS                                                     
      DO 59 J=1,ISS                                                     
      ML=I-J                                                            
      IF(ML)56,55,54                                                    
   54 CONTINUE                                                          
      PHAS=PHASEF(ML)                                                   
      SUM(I,J,1)=SUM(J,I,1)*PHAS                                        
      SUM(I,J,2)=SUM(J,I,2)*PHAS                                        
      GO TO 56                                                          
   55 CONTINUE                                                          
      SUM(I,J,1)=SUM(I,J,1)-CL                                          
      SUM(I,J,2)=SUM(I,J,2)+SL                                          
   56 CONTINUE                                                          
      SIG(N)=SIG(N)+SUM(I,J,1)**2+SUM(I,J,2)**2                         
   59 CONTINUE                                                          
   60 CONTINUE                                                          
      CALL POLFCT(POL(N),SUM(1,1,1),SUM(1,1,2),ISS,ISS)                 
      POL(N)=POL(N)/SIG(N)                                              
      SIG(N)=SIG(N)/(2.0*FS(N)+1.0)                                     
      IF(ETA(N))66,65,66                                                
   65 RATIO(N)=0.0                                                      
      GO TO 70                                                          
   66 CONTINUE                                                          
      IF(S.GE.1.E-10) GO TO 67                                          
      RATIO(N)=1.0                                                      
      GO TO 70                                                          
   67 RATIO(N)=SIG(N)/FACT**2                                           
   70 CONTINUE                                                          
      ICNT=ICNT+INCR(N)                                                 
      IF(NTH.GT.NX) GO TO 80                                            
      SIGPLT(NTH,N)=RATIO(N)                                            
      IF(Z(N).EQ.0.0) SIGPLT(NTH,N)=SIG(N)                              
   80 CONTINUE                                                          
      WRITE(6,9001)(THETA,SIG(N),RATIO(N),POL(N),N=1,2)               
      IF(ICON(13).GE.2) WRITE(7,9010) THETA,(SIG(N),RATIO(N),POL(N),  
     & N=1,2)                                                           
      THETA=THETA+DTHETA                                                
  100 CONTINUE                                                          
  101 CONTINUE                                                          
      WRITE(6,9002)(RSIG(N),N=1,2)                                      
      NTH=MIN0(NTHETA,NX)                                               
      IF(ICON(6).EQ.1) GO TO 190                                        
      IF(ICON(6).GT.5) ICON(6)=5                                        
      WRITE(6,9100) ALPHA                                               
      WRITE(6,9101)                                                     
      IF(Z(1).NE.0.0) WRITE(6,9103)                                     
      CALL DWPLOT(NTH,ICON(6),SIGPLT(1,1),ANGLE(3),ANGLE(2)             
     &  ,5HTHETA,5HSIGMA)                                               
      WRITE(6,9100) ALPHA                                               
      WRITE(6,9102)                                                     
      IF(Z(2).NE.0.0) WRITE(6,9103)                                     
      CALL DWPLOT(NTH,ICON(6),SIGPLT(1,2),ANGLE(3),ANGLE(2)             
     &  ,5HTHETA,5HSIGMA)                                               
  190 CONTINUE                                                          
      RETURN                                                            
 9000 FORMAT(1H0,7H  THETA,14H   EL-SIG,F**2,14H  SIGMA/COUL  ,14H     P
     &OL      ,5H     ,8H   THETA,14H   EL-SIG,F**2,14H  SIGMA/COUL  ,14
     &H     POL      )                                                  
 9001 FORMAT(2(F8.2,3E14.5,5X))                                         
 9002 FORMAT(1H0,7HREACSIG,E14.5,41X,E14.5)                             
 9004 FORMAT(14HELASTIC SIGMAS,6X,15A4)                                 
 9010 FORMAT(F9.3,1X,2(1PE10.4,0PF10.7,F10.7))                          
 9100 FORMAT(1H1,15A4)                                                  
 9101 FORMAT(37H ENTRANCE CHANNEL ELASTIC SCATTERING.)                  
 9102 FORMAT(33H EXIT CHANNEL ELASTIC SCATTERING.)                      
 9103 FORMAT(1H+,39X,20HRATIO TO RUTHERFORD.)                           
      END
