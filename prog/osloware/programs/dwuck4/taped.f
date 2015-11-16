      
      SUBROUTINE TAPED                                                  
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  C(600),FR(600),R(2),CTEMP(2)                      
      IK=ICON(12)                                                       
      FLIK=IK                                                           
      JT=NS(1)+NS(2)                                                    
      NP=LPL2*JT                                                        
      R(1)=0.0                                                          
      R(2)=0.0                                                          
      IKTEMP=0                                                          
      DO 50 M=1,K                                                       
      IKTEMP=IKTEMP+1                                                   
C                                                                       
C     READ IN DISTORTED WAVE RADIAL FUNCTIONS                           
C                                                                       
      READ(4)(FR(N),N=1,NP)                                             
      IF(IKTEMP.NE.IK) GO TO 50                                         
      IKTEMP=0                                                          
      IX=0                                                              
      DO 40 N=1,2                                                       
      R(N)=R(N)+DR(N)*FLIK                                              
      JX=NS(N)                                                          
      DO 39 J=1,JX                                                      
      DO 30 LL=1,LPLUS                                                  
      LK=LL+LL-1                                                        
      IX1=LK+IX                                                         
      IX2=IX1+1                                                         
C                                                                       
C     NORMALIZE RADIAL FUNCTIONS                                        
C                                                                       
      CTEMP(1)=FR(IX1)*C(IX1)-FR(IX2)*C(IX2)                            
      CTEMP(2)=FR(IX1)*C(IX2)+FR(IX2)*C(IX1)                            
      FR(IX1)=CTEMP(1)                                                  
      FR(IX2)=CTEMP(2)                                                  
   30 CONTINUE                                                          
      IX=IX+LPL2                                                        
   39 CONTINUE                                                          
   40 CONTINUE                                                          
      WRITE(6,9001)R(1),R(2)                                            
      WRITE(6,9601)                                                     
      DO 45 LL=1,LPLUS                                                  
      LM=LL-1                                                           
      LK1=LM+LL                                                         
      LK2=LPL2*(NS(1)-1)+LK1                                            
      WRITE(6,9602)LM,(FR(LK),FR(LK+1),LK=LK1,LK2,LPL2)               
      LK1=LK2+LPL2                                                      
      LK2=LPL2*(NS(2)-1)+LK1                                            
      WRITE(6,9603)LM,(FR(LK),FR(LK+1),LK=LK1,LK2,LPL2)               
   45 CONTINUE                                                          
   50 CONTINUE                                                          
      REWIND 4                                                          
      RETURN                                                            
 9001 FORMAT(1H0,3HR1=,F8.4,57X,3HR2=,F8.4)                             
 9601 FORMAT(4H   L,20H  REAL D1   IMAG D1 ,20H  REAL D2   IMAG D2 ,20H 
     & REAL D3   IMAG D3 ,4X,4H   L,20H  REAL D1   IMAG D1 ,20H  REAL D2
     &   IMAG D2 ,20H  REAL D3   IMAG D3 )                              
 9602 FORMAT(1H ,    I3,6F10.7)                                         
 9603 FORMAT(1H+,68X,I3,6F10.7)                                         
      END
