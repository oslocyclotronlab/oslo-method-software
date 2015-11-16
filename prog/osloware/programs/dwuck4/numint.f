      
      SUBROUTINE NUMINT(NMAX,RMAX,YMAX,FL,FK,E,FX,FXD,YUQ,YDQ)          
C                                                                       
C     INTEGRATES SCHROEDINGER EQUATION FROM Y=0.0 TO +YMAX AND -YMAX    
C     AT X=RMAX.                                                        
C                                                                       
      DIMENSION FX(2),FXD(2),YUQ(4),YDQ(4),XU(2),XD(2),RM(2)            
     &         ,YU(2),YD(2),YUD(2),YDD(2)                               
     &         ,BU1(2),BU2(2),BU3(2),BD1(2),BD2(2),BD3(2)               
C                                                                       
      P=FK*FK                                                           
      Q=FL*(FL+1.0)                                                     
      R=2.0*E*FK                                                        
      DELK=YMAX/FLOAT(NMAX)                                             
      DRR2=-DELK*DELK/12.0                                              
      RM(1)=RMAX                                                        
      RM(2)=0.0                                                         
      XU(1)= RMAX                                                       
      XU(2)= DELK                                                       
      XD(1)= RMAX                                                       
      XD(2)=-DELK                                                       
C                                                                       
      CALL  INTDE(P,Q,R,FX,FXD,RM,XU,YU,YUD)                            
      CALL  INTDE(P,Q,R,FX,FXD,RM,XD,YD,YDD)                            
C                                                                       
      YUQ(1)=FX(1)                                                      
      YUQ(2)=FX(2)                                                      
      YUQ(3)=YU(1)                                                      
      YUQ(4)=YU(2)                                                      
      YDQ(1)=FX(1)                                                      
      YDQ(2)=FX(2)                                                      
      YDQ(3)=YD(1)                                                      
      YDQ(4)=YD(2)                                                      
      NP1=NMAX+1                                                        
      ZR= RMAX                                                          
      ZI=-DELK                                                          
      ZR2=ZR*ZR                                                         
      TWOX=-2.0*ZR                                                      
      RZR=R*ZR                                                          
      DO 100 I=1,NP1                                                    
      II=I+I-1                                                          
      ZI=ZI+DELK                                                        
      ZI2=ZI*ZI                                                         
      DET=ZR2+ZI2                                                       
      DET2=DET*DET                                                      
      GR=Q*(ZR2-ZI2)/DET2 + RZR /DET - P                                
      GI=Q* TWOX*ZI /DET2 - R*ZI/DET                                    
      FACR=1.0-DRR2*GR                                                  
      FACI=   -DRR2*GI                                                  
      DET=FACR*FACR+FACI*FACI                                           
      IF(I.GT.2) GO TO 50                                               
      BU2(1)=YUQ(II  )*FACR-YUQ(II+1)*FACI                              
      BU2(2)=YUQ(II+1)*FACR+YUQ(II  )*FACI                              
      BD2(1)=YDQ(II  )*FACR+YDQ(II+1)*FACI                              
      BD2(2)=YDQ(II+1)*FACR-YDQ(II  )*FACI                              
      IF(I.EQ.2) GO TO 100                                              
      BU1(1)=BU2(1)                                                     
      BU1(2)=BU2(2)                                                     
      BD1(1)=BD2(1)                                                     
      BD1(2)=BD2(2)                                                     
      GO TO 100                                                         
   50 CONTINUE                                                          
      BU3(1)=12.0*YU(1)-10.0*BU2(1)-BU1(1)                              
      BU3(2)=12.0*YU(2)-10.0*BU2(2)-BU1(2)                              
      BD3(1)=12.0*YD(1)-10.0*BD2(1)-BD1(1)                              
      BD3(2)=12.0*YD(2)-10.0*BD2(2)-BD1(2)                              
      YU(1)=(BU3(1)*FACR+BU3(2)*FACI)/DET                               
      YU(2)=(BU3(2)*FACR-BU3(1)*FACI)/DET                               
      YD(1)=(BD3(1)*FACR-BD3(2)*FACI)/DET                               
      YD(2)=(BD3(2)*FACR+BD3(1)*FACI)/DET                               
      YUQ(II  )=YU(1)                                                   
      YUQ(II+1)=YU(2)                                                   
      YDQ(II  )=YD(1)                                                   
      YDQ(II+1)=YD(2)                                                   
      BU1(1)=BU2(1)                                                     
      BU1(2)=BU2(2)                                                     
      BU2(1)=BU3(1)                                                     
      BU2(2)=BU3(2)                                                     
      BD1(1)=BD2(1)                                                     
      BD1(2)=BD2(2)                                                     
      BD2(1)=BD3(1)                                                     
      BD2(2)=BD3(2)                                                     
  100 CONTINUE                                                          
      RETURN                                                            
      END
