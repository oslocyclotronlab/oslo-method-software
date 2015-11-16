      
      FUNCTION TAILS(TAIL,N,AB,FL1,FL2,FL3,FK1,FK2,FK3,E1,E2,E3,RMAX,   
     &  S1,F2,F2D,F3,F3D,UNOR,LPLUS,NEW)                                
C                                                                       
C     FORMS RADIAL INTEGRAL OF TAIL WAVE FUNCTIONS.                     
C                                                                       
      DIMENSION FK2(2),E2(2),F2(2),F2D(2),F3(2),F3D(2)                  
     &         ,S1(2),TAIL(2),UNOR(2),Y3P(402),Y3M(402)                 
      COMMON /LOCAL/ FSPAC(4900),Y2P(402),Y2M(402),UP(402)              
     &              ,TERM(2),RENORM(2),F3LAST(2),F3DLAS(2),DRX(3)       
      COMMON /UBOUND/ DY,YMAX,EISIG0(2)                                 
      DATA TOL/1.0E-5/, IDUNIT/21/                                      
C                                                                       
      NP1=N+1                                                           
      NNP1=2*NP1                                                        
      IF(NEW.NE.1) GO TO 10                                             
      FK3AB=AB*FK3                                                      
      YM1=ABS(YMAX)                                                     
      DEL=YM1/N                                                         
C                                                                       
C     WRITE OUTGOING COULOMB FUNCTIONS FOR COMPLEX R                    
C                                                                       
      OPEN(UNIT=21,STATUS='SCRATCH',FORM='UNFORMATTED')
      REWIND IDUNIT                                                     
      DO 5 II=1,LPLUS                                                   
      XL1=II-1                                                          
      CALL TAILYY(N,RMAX,YM1,XL1,FK1,E1,UP)                             
      WRITE(IDUNIT) (UP(I),I=1,NNP1)                                    
    5 CONTINUE                                                          
      REWIND IDUNIT                                                     
      IP21=0                                                            
      LASTL=-1                                                          
C                                                                       
C     CALCULATION OF Y2P(I)                                             
C                                                                       
      CALL NUMINT(N,RMAX,YM1,FL2,FK2,E2,F2,F2D,Y2P,Y2M)                 
   10 CONTINUE                                                          
      IL1 =FL1+TOL                                                      
      IL1P=IL1+1                                                        
      IF(IL1.NE.LASTL) CALL READI(IDUNIT,IP21,IL1P,NNP1,UP)             
      IF(NEW.EQ.1) GO TO 30                                             
      TEST=(F3(1)-F3LAST(1))**2+(F3(2)-F3LAST(2))**2                    
      IF(TEST.GT.TOL) GO TO 30                                          
      TEST=(F3D(1)-F3DLAS(1))**2+(F3D(2)-F3DLAS(2))**2                  
      IF(TEST.GT.TOL) GO TO 30                                          
      GO TO 40                                                          
   30 CONTINUE                                                          
C                                                                       
C     CALCULATION OF Y3P(I)                                             
C                                                                       
      CALL NUMINT(N,RMAX,YM1,FL3,FK3AB,E3,F3,F3D,Y3P,Y3M)               
   40 CONTINUE                                                          
C                                                                       
C     SIMPSON QUADRATURE                                                
C                                                                       
      DRX(1)=2.0*DEL/6.0                                                
      DRX(2)=2.0*DRX(1)                                                 
      DRX(3)=    DEL/6.0                                                
      ZR=RMAX                                                           
      ZI=-DEL                                                           
      ZR2=ZR*ZR                                                         
      TAIL(1)=0.0                                                       
      TAIL(2)=0.0                                                       
      MX=2                                                              
      DO 100 I=1,NP1                                                    
      II=I+I-1                                                          
      MX=3-MX                                                           
      ZI=ZI+DEL                                                         
      ZI2=ZI*ZI                                                         
      DET=ZR2+ZI2                                                       
      QR=(UP(II  )*ZR+UP(II+1)*ZI)/DET                                  
      QI=(UP(II+1)*ZR-UP(II  )*ZI)/DET                                  
      PR=Y2P(II  )*Y3P(II  )-Y2P(II+1)*Y3P(II+1)                        
      PI=Y2P(II+1)*Y3P(II  )+Y2P(II  )*Y3P(II+1)                        
      PT=PR*QR-PI*QI                                                    
      PI=PI*QR+PR*QI                                                    
      PR=PT                                                             
      TERM(1)=S1(1)*PR-S1(2)*PI                                         
      TERM(2)=S1(2)*PR+S1(1)*PI                                         
      PR=Y2M(II  )*Y3M(II  )-Y2M(II+1)*Y3M(II+1)                        
      PI=Y2M(II+1)*Y3M(II  )+Y2M(II  )*Y3M(II+1)                        
      TERM(1)=TERM(1)+(PR*QR+PI*QI)                                     
      TERM(2)=TERM(2)+(PI*QR-PR*QI)                                     
      IF(I.EQ.1) GO TO 80                                               
      IF(I.EQ.NP1) GO TO 80                                             
      TAIL(1)=TAIL(1)+DRX(MX)*TERM(1)                                   
      TAIL(2)=TAIL(2)+DRX(MX)*TERM(2)                                   
      GO TO 100                                                         
   80 CONTINUE                                                          
      TAIL(1)=TAIL(1)+DRX(3)*TERM(1)                                    
      TAIL(2)=TAIL(2)+DRX(3)*TERM(2)                                    
  100 CONTINUE                                                          
C                                                                       
      PR= UP(1)-(S1(1)*UP(1)-S1(2)*UP(2))                               
      PI=-UP(2)-(S1(2)*UP(1)+S1(1)*UP(2))                               
      DET=PR*PR+PI*PI                                                   
      RENORM(1)= 2.0*(UNOR(2)*PR-UNOR(1)*PI)/DET                        
      RENORM(2)=-2.0*(UNOR(1)*PR+UNOR(2)*PI)/DET                        
      RERR=(RENORM(1)-1.0)**2+RENORM(2)**2                              
      IF(YMAX.LT.0.0) WRITE(6,190) FL1,FL3,TAIL,RENORM                  
      TAIL(1)=TAIL(1)*RENORM(1)-TAIL(2)*RENORM(2)                       
      TAIL(2)=TAIL(2)*RENORM(1)+TAIL(1)*RENORM(2)                       
      TAILS=1.0                                                         
      IF(RERR.LE.0.1) GO TO 180                                         
      TAILS=0.0                                                         
      WRITE(6,191)                                                      
      WRITE(6,192) FL1,RENORM                                           
  180 CONTINUE                                                          
      LASTL=IL1                                                         
      F3LAST(1)=F3(1)                                                   
      F3LAST(2)=F3(2)                                                   
      F3DLAS(1)=F3D(1)                                                  
      F3DLAS(2)=F3D(2)                                                  
      NEW=0                                                             
      RETURN                                                            
  190 FORMAT(5H  L1=,F4.0,5H  L3=,F4.0,7H  TAIL=,2E16.8,9H  RENORM=,2E16
     &.8)                                                               
  191 FORMAT(42H TAIL FAILED, SO RADIAL INTEGRAL WAS SET=0)             
  192 FORMAT(12H FL1,RENORM=,F6.1,2E16.8)                               
      END
