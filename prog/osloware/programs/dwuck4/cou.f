      
      SUBROUTINE COU(RS,RPS,ES,LL,HS,F,FP,G,GP,S)
      
C     THE NEXT 8 CARDS CONVERT DWUCK TO AN IBM 360                      
C     IMPLICIT REAL*8(A-H,O-Z)                                          
C     SQRT(XXX)=DSQRT(XXX)                                              
C     FLOAT(III)=DFLOAT(III)                                            
C     ABS(XXX)=DABS(XXX)                                                
C     SIN(XXX)=DSIN(XXX)                                                
C     COS(XXX)=DCOS(XXX)                                                
C     ALOG(XXX)=DLOG(XXX)                                               
C     ATAN(XXX)=DATAN(XXX)                                              
C     THE NEXT 2 CARDS ARE FOR USE ON THE DEC-10 COMPUTER               
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      REAL*4 RS,RPS,ES,HS,F,FP,G,GP,S                                   
      DIMENSION F(1),FP(1),G(*),GP(*),S(1)
C                                                                       
C     F,G   ARE FUNCTIONS AT RS                                         
C     FP,GP ARE FUNCTIONS AT RPS                                        
C     S     ARE COULOMB PHASE SHIFTS                                    
C     LL    IS TOTAL NUMBER OF L VALUES, INCLUDING 0                    
C                                                                       
      R=RS                                                              
      RP=RPS                                                            
      E=ES                                                              
      H=HS                                                              
      TE=2.*E                                                           
      TF=E*E                                                            
      J=50                                                              
      IF(LL.GT.50) J=LL                                                 
      ELP=J                                                             
      A=ATAN (E/ELP)                                                    
      B=SQRT (TF+ELP*ELP)                                               
      Y=A*(ELP-0.5)+E*(LOG(B)-1.)-SIN (A)/(12.*B)                       
     &  +SIN (3.*A)/(360.*B**3)-SIN (5.*A)/(1260.*B**5)                 
     &  +SIN (7.*A)/(1680.*B**7)-SIN (9.*A)/(1188.*B**9)                
      K=J-1                                                             
      IF(J.EQ.1) S1=Y                                                   
      IF(J.LE.LL) S(J)=Y                                                
      DO 100 I=1,K                                                      
      J=J-1                                                             
      ELP=J                                                             
      Y=Y-ATAN (E/ELP)                                                  
      IF(J.EQ.1) S1=Y                                                   
      IF(J.LE.LL) S(J)=Y                                                
  100 CONTINUE                                                          
      DEL1=R-TE                                                         
      RMAX=.41666667*(TF+4.*E+3.)                                       
      IF(RMAX.LT.10.0) RMAX=10.0                                        
      DEL=R-RMAX                                                        
      IF(E.LT.5.0) GO TO 280                                            
      IF(ABS(DEL1).GT.ABS(DEL)) GO TO 280                               
      DEL=DEL1                                                          
      I=1                                                               
      IF(DEL.EQ.0.0) I=2                                                
      X=TE                                                              
      T1=TF                                                             
      T2=T1*T1                                                          
      T3=E** .666666667                                                 
      T4=T3*T3                                                          
      T5=T4*T4                                                          
      T6=T3*T5                                                          
      T7=T4*T6                                                          
      T8=T3*T7                                                          
      T9=E** .166666667                                                 
      Y=1.22340402*T9*(1.+.495957017E-1/T4-.888888889E-2/T1+.245519918  
     &E-2/T6-.910895806E-3/T2+.845362E-3/T8)                            
      Z=-.707881773/T9*(1.-.172826039/T3+.317460317E-3/T1-.358121485    
     &E-2/T5+.311782468E-3/T2-.907396643E-3/T7)                         
      GO TO 665                                                         
  280 CONTINUE                                                          
      IF((E.NE.0.0).AND.(DEL.LT.0.0)) GO TO 310                         
      X=R                                                               
      I=2                                                               
      GO TO 320                                                         
  310 X=RMAX                                                            
      I=1                                                               
  320 T1=TF                                                             
      T2=2.*X                                                           
      T3=X-E*LOG(T2)+S1                                                 
      T4=E/T2                                                           
      SS=1.                                                             
      TS=0.                                                             
      SL=0.                                                             
      TL=1.-E/X                                                         
      SSS=1.                                                            
      STS=0.                                                            
      SSL=0.                                                            
      STL=TL                                                            
      EN=0.                                                             
      DO 620 K=1,25                                                     
      T5=EN+1.                                                          
      T6=T5+EN                                                          
      T7=EN*T5                                                          
      T8=T6*T4/T5                                                       
      T9=(T1-T7)/(T2*T5)                                                
      T5=T8*SS-T9*TS                                                    
      TS=T8*TS+T9*SS                                                    
      SS=T5                                                             
      IF(ABS(SS/SSS).LE.1.0E-10) GO TO 630                              
      T5=T8*SL-T9*TL-SS/X                                               
      TL=T8*TL+T9*SL-TS/X                                               
      SL=T5                                                             
      SSS=SSS+SS                                                        
      STS=STS+TS                                                        
      SSL=SSL+SL                                                        
      STL=STL+TL                                                        
      EN=EN+1.                                                          
  620 CONTINUE                                                          
  630 T8=SIN (T3)                                                       
      T9=COS (T3)                                                       
      Y=SSS*T9-STS*T8                                                   
      Z=SSL*T9-STL*T8                                                   
  665 GO TO (670,810),I                                                 
  670 M=1                                                               
  671 N=ABS (DEL/H)+1.0                                                 
      DX=DEL/FLOAT(N)                                                   
      T1=DX/2.                                                          
      T2=DX/8.                                                          
      T3=TE
      DO 805 I=1,N                                                      
      T4=DX*(T3/X-1.)*Y                                                 
      X=X+T1                                                            
      T5=DX*(T3/X-1.)*(Y+T1*Z+T2*T4)                                    
      X=X+T1                                                            
      T6=DX*(T3/X-1.)*(Y+DX*Z+T1*T5)                                    
      Y=Y+DX*(Z+(T4+2.*T5)/6.)                                          
      Z=Z+(T4+4.*T5+T6)/6.                                              
  805 CONTINUE
      GO TO (810,828),M                                                 
  810 G(1)=Y                                                            
      G1=Y                                                              
      GM2=Y                                                             
      M=2                                                               
      DEL=RP-R                                                          
      W=Z                                                               
      GO TO 671                                                         
  828 GP(1)=Y                                                           
      GP1=Y                                                             
      GPM2=Y                                                            
      T1=TF                                                             
      T8=SQRT (1.+T1)                                                   
      G2=((1./R+E)*G1-W)/T8                                             
      GP2=((1./RP+E)*Y-Z)/T8                                            
      G(2)=G2                                                           
      GP(2)=GP2                                                         
      GM1=G2                                                            
      GPM1=GP2                                                          
      T2=1.                                                             
      T3=2.                                                             
      DO 910 I=3,LL                                                     
      T4=T2+T3                                                          
      T5=T2*T3                                                          
      T6=T3*SQRT (T2*T2+T1)                                             
      T7=T2*SQRT (T3*T3+T1)                                             
      GI =(T4*(E+T5/R )*GM1 -T6*GM2 )/T7                                
      GPI=(T4*(E+T5/RP)*GPM1-T6*GPM2)/T7                                
      G (I)=GI                                                          
      GP(I)=GPI                                                         
      GM2=GM1                                                           
      GM1=GI                                                            
      GPM2=GPM1                                                         
      GPM1=GPI                                                          
      T2=T2+1.                                                          
      T3=T3+1.                                                          
  910 CONTINUE                                                          
      I=LL+11                                                           
      N=2.*R+11.                                                        
      IF(I.GT.N) N=I                                                    
      Y=1.0E-20                                                         
      YP=Y                                                              
      X=Y                                                               
      XP=X                                                              
      Z=0.                                                              
      ZP=Z                                                              
      T2=N                                                              
 1000 T3=T2+1.                                                          
      T4=T2+T3                                                          
      T5=T2*T3                                                          
      T6=T2*SQRT (T3*T3+T1)                                             
      T7=T3*SQRT (T2*T2+T1)                                             
      Y =(T4*(E+T5/R )*Y -T6*Z )/T7                                     
      YP=(T4*(E+T5/RP)*YP-T6*ZP)/T7                                     
      IF(N-2) 1040,1045,1050                                            
 1040 F1=Y                                                              
      FP1=YP                                                            
      GO TO 1050                                                        
 1045 F2=Y                                                              
      FP2=YP                                                            
 1050 CONTINUE                                                          
      IF(N.GT.LL) GO TO 1080                                            
 1060 F(N)=Y                                                            
      FP(N)=YP                                                          
      GO TO 1120                                                        
 1080 IF(ABS(Y).LT.1.0) GO TO 1120                                      
 1090 CONTINUE                                                          
      Y =Y *1.0E-20                                                     
      YP=YP*1.0E-20                                                     
      X =X *1.0E-20                                                     
      XP=XP*1.0E-20                                                     
 1120 N=N-1                                                             
      Z=X                                                               
      ZP=XP                                                             
      X=Y                                                               
      XP=YP                                                             
      T2=T2-1.                                                          
      IF(N.GT.0) GO TO 1000                                             
      Y=F1*G2-F2*G1                                                     
      YP=FP1*GP2-FP2*GP1                                                
      Z=1./(Y*T8)                                                       
      ZP=1./(YP*T8)                                                     
      DO 1180 I=1,LL                                                    
      FP(I)=FP(I)*ZP                                                    
 1180 F(I)=F(I)*Z                                                       
      RETURN                                                            
      END
