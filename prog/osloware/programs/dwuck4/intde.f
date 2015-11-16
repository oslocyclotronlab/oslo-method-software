      
      SUBROUTINE INTDE(P,Q,R,YL,YLP,XL,XU,YU,YUP)                       
C                                                                       
C     INTDE INTEGRATES THE DIFFERENTIAL EQUATION D2Y/DX2_(-P+Q/X2+R/X)Y 
C     FROM XL TO XU, WITH VALUE AND DERIVATIVE YL,YLP AT XL, AND RETURNS
C     THE VALUE AND DERIVATIVE YU,YUP AT XU.                            
C                                                                       
      COMPLEX YL,YLP,XL,XU,YP(20),XLSQ,FAC,DIFF,POW,SUM,SUMP,           
     &  TEST,TESTLA,SR,SQ,TR,TRY,TQY,TESTP,YU,YUP                       
C                                                                       
      ERR=1.E-4                                                         
      XLSQ=XL*XL                                                        
      YP(1)=YLP                                                         
      FAC=Q/XLSQ+R/XL-P                                                 
      YP(2)=FAC*YL                                                      
      DIFF=XU-XL                                                        
      POW=DIFF*DIFF/2.                                                  
      SUM=YL+YLP*DIFF+YP(2)*POW                                         
      SUMP=YLP+YP(2)*DIFF                                               
      ERR=ERR*AMAX1(CABS(YL),CABS(SUM))                                 
      I=3                                                               
      TEST=YP(2)*POW                                                    
   30 CONTINUE                                                          
      XI=I                                                              
      TESTLA=TEST                                                       
      SR=(0.,0.)                                                        
      SQ=(0.,0.)                                                        
      TR=(1.,0.)                                                        
      IM2=I-2                                                           
      DO 50 J=1,IM2                                                     
      XJ=J                                                              
      TR=-(XI-XJ-1.)*TR/XL                                              
      IF((I-J).NE.2) GO TO 5                                            
      TRY=TR*YL                                                         
      GO TO 6                                                           
    5 CONTINUE                                                          
      TRY=TR*YP(IM2-J)                                                  
    6 CONTINUE                                                          
      TQY=(XJ+1.)*TRY                                                   
      SR=SR+TRY                                                         
      SQ=SQ+TQY                                                         
   50 CONTINUE                                                          
      YP(I)=FAC*YP(I-2)+R*SR/XL+Q*SQ/XLSQ                               
      TESTP=YP(I)*POW                                                   
      TEST=TESTP*DIFF/XI                                                
      SUM=SUM+TEST                                                      
      SUMP=SUMP+TESTP                                                   
      POW=POW*DIFF/XI                                                   
      I=I+1                                                             
      T=AMAX1(CABS(TEST),CABS(TESTLA))                                  
      IF(T.GT.ERR) GO TO 30                                             
      YU=SUM                                                            
      YUP=SUMP                                                          
      RETURN                                                            
      END
