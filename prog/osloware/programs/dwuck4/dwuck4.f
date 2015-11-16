C     PROGRAM DWUCK4(INPUT,OUTPUT,PUNCH,TAPE4,TAPE8,TAPE7=PUNCH,        
C    1 TAPE9,TAPE10,TAPE11,TAPE12,TAPE21,TAPE6=OUTPUT,TAPE5=INPUT)      
C***********************************************************************
C                                                                       
C                  *****     DWUCK-4     *****                          
C                                                                       
C     CHANGES FROM BASIC COLORADO EXPORT VERSION,  J.R.COMFORT          
C                                                                       
C   1. THE NUMBER OF CONTROL INTEGERS WAS INCREASED FROM 16 TO 20.      
C   2. CONTROL INTEGER NO. 3 WAS MODIFIED.  NEW OPTIONS WERE ADDED.     
C            C.I. 3 = 3  SEPARATE SIGMA FOR EACH LTR AND INCOHERENT SUM.
C                   = 4  SEPARATE SIGMA FOR EACH LTR, INCOHERENT SUM,   
C                        AND COHERENT SUM.                              
C        NOTE. INCOHERENT SUM APPEARS ON A PLOT ONLY.                   
C   3. CONTROL INTEGER NO. 4 WAS MODIFIED.                              
C            C.I. 4 = 0  PRINT ALL FORM-FACTOR INFORMATION              
C                   = 1  SUPPRESS FINAL FORM FACTOR (BEFORE FRNL) ONLY  
C                   = 2  SUPPRESS SING.-PART. WAVE FUNCTIONS, ICON(2)=2 
C                   = 3  SUPPRESS F.F. PARAMETERS IN FORMF, CATHEN      
C                   = 4  SUPPRESS ALL FORM-FACTOR INFORMATION           
C   4. CONVENTIONS FOR C.I. NO. 5 WERE CHANGED TO ALLOW FOR PRINTOUT OF 
C        S-MATRIX AMPLITUDES AND PHASES.  THE CONTROL IS NOW            
C            C.I. 5 = 0  NO PRINTOUT                                    
C                   = 1  PRINT ELASTIC-SCATTERING AMPLITUDES            
C                   = 2  PRINT S-MATRIX AMPLITUDES AND PHASES           
C                   = 3  PRINT BOTH                                     
C   5. CONTROL INTEGER 6 WAS MODIFIED.                                  
C            C.I. 6 = 0  NO ELASTIC CROSS SECTIONS OR PLOTS             
C                   = 1  ELASTIC CROSS SECTIONS ONLY                    
C                   = N  CROSS SECTS. AND PLOTS, N CYCLES SEMILOG (N<6) 
C   6. CONTROL INTEGER 9 GIVES 3-CYCLE SEMILOG PLOTS BY DEFAULT.        
C        PLOTS ARE SUPPRESSED IF C.I. 9 IS GREATER THAN 5.              
C   7. CONTROL INTEGER NO. 10 WAS MODIFIED.                             
C            C.I.10 = 2  INCLUDE RELATIVISTIC SCALE FACTOR IN T MATRIX  
C                        FOR INELASTIC NUCLEON SCATTERING.              
C                   = 3  SAME AS 2 WITH FULL RELATIVISTIC KINEMATICS.   
C   8. CONTROL INTEGER NO. 13 IS THE CROSS SECION PUNCH CONTROL.        
C            C.I. 13 = 0  NO PUNCHING                                   
C                    = 1  PUNCH INELASTIC DATA ONLY                     
C                    = 2  PUNCH ELASTIC DATA ONLY                       
C                    = 3  PUNCH BOTH 1 AND 2                            
C   9. C.I. NO. 14 CONTROLS FORM-FACTOR PUNCHING.  FORMAT (5E16.7).     
C            C.I. 14 = 0  NO PUNCHING                                   
C                    = 1  PUNCH REAL F.F. ONLY                          
C                    = 2  PUNCH IMAGINARY F.F. ONLY                     
C                    = 3  PUNCH BOTH 1 AND 2                            
C                    = 1-3  PUNCHING DONE BEFORE FRNL CORRECTIONS       
C                    = 4-6  PUNCHING DONE AFTER FRNL CORRECTIONS,       
C                           AS FOR 1-3.                                 
C                    = 7-9  PUNCHING DONE BOTH BEFORE AND AFTER FRNL    
C                           CORRECTIONS, SAME AS FOR 1-3.               
C  10. C.I. NO. 16 (IF NOT EQUAL TO 0) SHUTS OFF THE SPIN-ORBIT COUPLING
C        IN THE INTEGRATIONS.  CORRECT CROSS SECTIONS ARE PRODUCED.     
C        CHANGES WERE MADE IN ADWUCK, BDWUCK, AND BETAFN.  IF C.I.      
C        16 = 1, ALL THE PRINTOUT (INCLUDING POLIZATIONS AND SPIN-FLIP) 
C        IS CORRECT.  IF C.I. 16 = 2, ONLY THE CROSS SECTIONS ARE       
C        CORRECT, BUT THE SAVING IN TIME IS MUCH GREATER.               
C        YOU CANNOT USE C.I.16 = 2 WHEN C.I.3 = 2 OR 4.                 
C  11. COTANCH ISOSPIN FEATURES ADDED.  MAY ONLY BE USED FOR            
C        1-NUCLEON TRANSFER.                                            
C            C.I. 19 = 0  NORMAL DWUCK, WITH NO ISOSPIN FEATURES.       
C                    = 1  ISOSPIN COUPLING IN ENTRANCE CHAN.  DWUCK F.F.
C                    = 2  ISOSPIN COUPLING IN EXIT CHAN.  DWUCK F.F.    
C                    = 3  (SAME AS 2 AT PRESENT)                        
C                    = 4-7  SIMILAR TO 0-3, BUT F. F.'S GIVEN BY TBIND. 
C  12. THE PUNCH FORMAT IN ELSIG WAS CHANGED FROM (E10.4,5(4X,E10.4))   
C         TO (F9.3,1X,2(1PE10.4,0PF10.7,F10.7)).                        
C  13. THE PUNCH FORMAT IN INSIG WAS CHANGED FROM (E10.4,5(4XE10.4))    
C        TO (F6.2,1PE11.4,9(0PF7.4)).                                   
C  14. THE PHYSICAL CONSTANTS IN THIS VERSION ARE BASED ON TAYLOR,      
C        PARKER, AND LANGENBERG (RMP 41,375,1969), EXCEPT FOR AMU WHICH 
C        IS BASED ON THE 1973 TABLE OF FUNDAMENTAL CONSTANTS.           
C  15. THE LENGTH OF COMMON IN THIS MAIN ROUTINE WAS INCREASED TO EQUAL 
C        THE LONGEST LENGTH OF BLANK COMMON (IN INSIG).  THE SEQUENCE   
C        WAS ADJUSTED TO INSURE GOOD BOUNDARY ALIGNMENT ON S/360        
C        COMPUTERS.                                                     
C  16. TAPE 2 WAS CHANGED TO TAPE 8.  TAPE 4 WAS SPLIT INTO TAPES 3     
C        AND 4.                                                         
C  17. THE DATE AND TIME ARE CURRENT DATE AND CPU TIME OBTAINED BY CALLS
C        TO SUBROUTINE CUDATE.  THE DATA WORDS ARE DOUBLE PRECISION AND 
C        THE OUTPUT FORMAT IS (A10,2X,F7.2).  THE FORMATS ARE 9803 IN   
C        DWUCK AND 9999 IN ADWUCK, BDWUCK, AND INSIG.                   
C  18. THE FIRST READ STATEMENT AND INITIAL PRINTOUTS WERE MOVED TO THE 
C        MAIN ROUTINE.                                                  
C  19. THE END-OF-FILE AND TERMINATION SEQUENCE WAS MODIFIED IN ADWUCK. 
C  20. ADDITIONS WERE MADE TO CALCULATE EXCHANGE CONTRIBUTIONS TO       
C        REACTIONS VIA L-DEPENDENT POTENTIALS.                          
C  21. REVISION OF ZERO DEGREE CALCULATION IN THE ELASTIC-SCATTERING    
C        ROUTINE ELSIG.  IF PARTICLE IS UNCHARGED, CODE PRINTS ELASTIC  
C        SIGMA.  IF CHARGED, IT GIVES SIGMA FOR NUCLEAR FORCE ONLY.     
C  22. SPIN-FLIP PROBABILITY CALCULATIONS WERE ADDED TO SUBROUTINES     
C        INSIG AND POLFCT.  THE SIZE OF ARRAY POL WAS INCREASED.        
C  23. ALL NON-LOCALITY CORRECTIONS USE THE OLD L.E.A. METHOD (HULTHEN  
C        FORM AS IN DWUCK-2) AS STANDARD.  THE ROST-KUNZ (EXPONENTIAL)  
C        FORM IS OBTAINED BY SETTING PNLOC LESS THAN ZERO FOR EACH      
C        PROJECTILE FOR WHICH IT IS TO BE APPLIED.                      
C  24. SUBROUTINE DWPLOT WAS REWRITTEN.  THE CALL SEQUENCE IS THE SAME. 
C  25. GAUSSIAN MICROSCOPIC INTERACTION INCLUDED IN CATHEN WITH ICODE=4.
C  26. OPEN AND CLOSE STATEMENTS ADDED FOR SCRATCH FILES.               
C  27. THE ANGULAR MOMENTUM PACKAGE HAS BEEN MODIFIED SO AS TO WORK WITH
C        LOG FACTORIALS.  THE INTERNAL TABLES ARE EXPANDED UP TO        
C        FACTORIAL 120.  THE HANDLING OF THE YXFCT FUNCTION CALLS IN    
C        SUBROUTINES ELSIG AND INSIG WERE MODIFIED.                     
C  28. CORRECTIONS MADE FOR THE PROPER HANDLING OF INTEGRATION          
C        COORDINATES IN MICROSCOPIC INELASTIC SCATTERING.  USE CORE     
C        MASS = A-1.                                                    
C  29. MODIFICATIONS WERE MADE IN THE HANDLING OF UNBOUND FORM FACTORS, 
C        FOLLOWING VINCENT AND FORTUNE.  THE HUBY-MINES DAMPING WAS     
C        CHANGED TO GAUSSIAN DAMPING.                                   
C  30. NEGATIVE RADIUS VALUES IMPLY A1**(1/3)+A2**(1/3).                
C  31. PROVISION IS MADE FOR DEFORMED SPIN-ORBIT POTENTIALS IN          
C        MACROSCOPIC INELASTIC SCATTERING.  THIS IS OAK RIDGE FORM,     
C        <L.S>+<L.S>*VLS.                                               
C  32. NEW ALGORITHM INSERTED FOR STARTING POINTS OF THE INTEGRATIONS.  
C  33. MICROSCOPIC INELASTIC SCATTERING MODIFIED FOR BETTER TREATMENT.  
C  34. VINCENT-FORTUNE TREATMENT FOR STRIPPING TO UNBOUND STATES ADDED. 
C        SOME CHANGES IN ADWUCK AND INTEG, BUT MAINLY IN RADINT.  I/O   
C        UNIT 21 IS USED. CHECK FOR OPEN AND CLOSE STATEMENTS IN RADINT.
C  35. ZERO-RANGE AND R**2-YUKAWA TENSOR MICROSCOPIC INTERACTIONS ADDED.
C  36. SIMPLE PROCEDURE FOR AVERAGING OVER THE ANGULAR RESOLUTION OF THE
C        DETECTOR WAS INCORPORATED IN INSIG.  CHECK USE OF SCRATCH      
C        UNIT 21 IN INSIG.                                              
C  37. NUMEROV INTEGRATION PROCEDURE MODIFIED IN INTEG SO AS TO HANDLE  
C        LARGE STEP SIZES WITH BETTER STABILITY.                        
C                                                                       
C***********************************************************************
      DOUBLE PRECISION BETA                                             
      COMMON ALPHA(15),BETA(2),ICON(20),ANGLE(6),HBARC,AMU,AMASS,CHGSQ  
     &,DRF,Z(3),ZA(3),FM(3),FMA(3),RC(3),AC(3),PNLOC(3),FS(3),ECM(3)    
     &,FK(3),FK2(3),ETA(3),DR(3),FMU(3),FN,FL,FJ2,FSS,VCE,FNRNG,RSIG(2) 
     &,       K,KZ,LPLUS,LPL2,IS(3),NS(3),NLTR,IFF,LTRT(8),JTRT(8)      
     &,ISTRT(8),JSAV(8),IBF(8)                                          
      COMMON /LOCAL/  SPACE(7200)                                       
C                                                                       
C      CALL ERRSET(6)                                                   
C                                                                       
      ANGLE(1)=37.                                                      
      ANGLE(2)=0.                                                       
      ANGLE(3)=5.                                                       
      ANGLE(4)=0.                                                       
      ANGLE(5)=0.                                                       
      FSCONT=137.03602                                                  
      HBARC=197.329                                                     
      AMU=931.502                                                       
      CHGSQ=HBARC/FSCONT                                                
      OPEN(UNIT=3,STATUS='SCRATCH',FORM='UNFORMATTED')

      OPEN(UNIT=4,STATUS='SCRATCH',FORM='UNFORMATTED')

      OPEN(UNIT=8,STATUS='SCRATCH',FORM='UNFORMATTED')

 1001 CONTINUE                                                          
C                                                                       
C     TAPE 3 STORES FORM FACTORS                                        
      REWIND 3                                                          
C                                                                       
C     TAPE 4 STORES DISTORTED WAVES                                     
      REWIND 4                                                          
C                                                                       
C     TAPE 8 STORES RADIAL INTEGRALS                                    
      REWIND 8                                                          
C                                                                       
C     DIVISION FOR OVERLAY MAY BE MADE  1-ADWUCK, 2-BDWUCK              
C                                                                       
      IBF(3)=0                                                          
C                                                                       
C     READ CARD SET 1                                                   
C                                                                       
      READ(5,9802,END=2) ICON,ALPHA                                     
      IF(ICON(1).NE.9) GO TO 3                                          
    2 CONTINUE                                                          
      CLOSE(UNIT=3,DISPOSE='DELETE')                                    
      CLOSE(UNIT=4,DISPOSE='DELETE')                                    
      CLOSE(UNIT=8,DISPOSE='DELETE')                                    
      STOP                                                              
    3 CONTINUE                                                          
      CALL CUDATE(BETA)                                                 
      WRITE(6,9998)                                                     
      WRITE(6,9502)                                                     
      WRITE(6,9804)                                                     
      WRITE(6,9803)ICON,ALPHA,BETA                                      
      CALL ADWUCK                                                       
      IF(IBF(3).NE.0) GO TO 1001                                        
      CALL BDWUCK                                                       
      GO TO 1001                                                        
 9502 FORMAT(18H0CONTROL INTEGERS )                                     
 9802 FORMAT(20I1,15A4)                                                 
 9803 FORMAT(1H ,20I2,4X,15A4,4X,A10,2X,F7.2)                           
 9804 FORMAT(41H  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0,22H    RUN IDE
     &NTIFICATION)                                                      
 9998 FORMAT(98H1DWUCK4-DISTORTED WAVES U.COLORADO-VERSION 01/JULY/1978.
     &   J.COMFORT EXTENDED VERSION 25/AUG./1981)                       
      END
