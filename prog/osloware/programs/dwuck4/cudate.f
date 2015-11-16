      SUBROUTINE CUDATE(BETA)                                           
      DOUBLE PRECISION BETA, BT                                           
      DIMENSION BETA(2),BT(2)
      CHARACTER*1 6ADTE
      EQUIVALENCE(ADTE,BT)
C                                                                       
      CALL DATE(ADTE)
      BETA(1) = BT(1)
      BETA(2) = BT(2)
C      BETA(2)=XEQTIM(0)/1000.                                          
C                                                                       
      RETURN                                                            
      END
