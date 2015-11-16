      
      FUNCTION PHASEF(N)                                                
      DIMENSION FAZ(2),A(3)                                             
      EQUIVALENCE (FAZ(1),A(2))                                         
      DATA A/1.0,-1.0,0.0/                                              
C                                                                       
C     NORMAL (UNIVERSAL) CODE                                           
C     I=IABS(MOD(N,2))                                                  
C                                                                       
C     ALTERNATE CODE FOR FORTRAN WITH BIT MASKING EXPRESSIONS           
      I=IAND(IABS(N),1)
C                                                                       
      PHASEF=FAZ(I)                                                     
      RETURN                                                            
      END
