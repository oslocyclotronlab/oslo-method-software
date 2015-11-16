
      PROGRAM SCATTER


C  *******    Elastic/Inelastic Scattering Program *****
C  *******                                         ***** 
C  *******        J.Taylor  Oct.93                 *****
C  *******      University of Oslo                 *****

      CHARACTER COMMAN
      REAL M1,M2,M3,M4,Q,R14,R13,R23,R24
      REAL CRIT3,TCRIT,CRIT41,CRIT42,THETA,PHI1,PHI2
      REAL E1,ET,E31,E32,E41,E42

      PRINT *,'       INELASTIC / ELASTIC SCATTERING'
      PRINT *,' '     
      PRINT *,' '
      PRINT *,'     THIS PROGRAM COMPUTES SCATTERING REACTIONS,'
      PRINT *,'     WITH AND WITHOUT Q-REACTION ENERGY.'
      PRINT *,'     THE DEFINITION OF TERMS IS AS FOLLOWS:'
      PRINT *,' '  
      PRINT *,' '  
      PRINT *,'                       + M3 (M1")'
      PRINT *,'                      /' 
      PRINT *,'                     /'
      PRINT *,'                    / THETA'
      PRINT *,'    M1--------> M2 @      ----------------------'
      PRINT *,'                     -'
      PRINT *,'                        -  PHI'
      PRINT *,'                           -'
      PRINT *,'                               * M4 (M2")'
      PRINT *,' '
      PRINT *,' '
      PRINT *,'       KNOCK-OUT REACTIONS ARE TAKEN INTO ACCOUNT,'
      PRINT *,'        SO PARTICLES LABELLED "RECOIL NUC." AND'
      PRINT *,'        "RECOIL PROJ." SHOULD BE DEFINED CORRECTLY.'
      PRINT *,' '
      PRINT *,' '      
      PRINT *,'      MASSES ENTERED SHOULD BE ATOMIC WEIGHTS,'
      PRINT *,'       AND ENERGY IN MeV. '
      PRINT *,' '  
      PRINT *,'      ANGLES AND ENERGY ARE GIVEN IN DEGREES'
      PRINT *,'       AND MeV RESPECTIVELY.'
      PRINT *,' '
      PRINT *,' '
      PRINT *,'      FOR PRINTING/GRAPHICAL PURPOSES, AN OUTPUT'
      PRINT *,'      FILE IS CREATED AUTOMATICALLY, "scatter.out",'
      PRINT *,'      IF WANTED,THIS SHOULD BE SAVED,UNDER A NEW NAME,'
      PRINT *,'      BEFORE THE PROGRAM IS RE-RUN.'      
      PRINT *,' '

C ***************************************************************
   5  PRINT *,' '
      OPEN(UNIT=22,FILE='scatter.out',ACCESS='SEQUENTIAL') 
      PRINT 10
  10  FORMAT(/'Give projectile mass :',$)
      READ *, M1
      PRINT 11
  11  FORMAT('Give target (rest) nucleus mass :',$)
      READ *,M2
      PRINT 12
  12  FORMAT('Give scattered particle mass :',$)
      READ *,M3
      PRINT 13
  13  FORMAT('Give recoil nucleus mass :',$)
      READ *,M4
      PRINT 14
  14  FORMAT('Give projectile energy (MeV) :',$)
      READ *,E1
      PRINT 15
  15  FORMAT('Give Q of reaction :',$)
      READ *,Q
 
C *******SETTING VARIABLES TO NULL

      THETA = 1.0 
      PHI1 = 0.0
      PHI2 = 0.0
      CRIT41= 0.0
      CRIT42= 0.0
      CRIT3= 0.0
      TCRIT= 0.0


C *******DEFINITION OF TERMS
      ET  = E1+Q
      R14 = (M1*M4*(E1/ET))/((M1+M2)*(M3+M4))
      R13 = (M1*M3*(E1/ET))/((M1+M2)*(M3+M4))
      R23 = (M2*M3)*(1+(M1*Q/(M2*ET)))/((M1+M2)*(M3+M4))
      R24 = (M2*M4)*(1+(M1*Q/(M2*ET)))/((M1+M2)*(M3+M4))

      CRIT3  =  (R24/R13)-(SIND(THETA))**2
      
      TCRIT  =  ASIN(SQRT(R24/R13))

       PHI1  = ASIND(SQRT(M3*E31/(M4*(ET-E31)))*SIND(THETA))
       PHI2  = ASIND(SQRT(M3*E32/(M4*(ET-E32)))*SIND(THETA))

      CRIT41 = SQRT( R23/R14-SIND(PHI1)**2)
      CRIT42 = SQRT( R23/R14-SIND(PHI2)**2)

C ********START OF LOGIC STRUCTURE

      IF (M3.GT.M4) THEN
         PRINT *, '    THETA       E31      PHI1       E41      E32 
     *  PHI2      E42'
       

          DO WHILE(CRIT3.GT.0.0)
              
               E31   =ET*R13*(COSD(THETA)+SQRT(CRIT3))**2
               PHI1  = ASIND(SQRT(M3*E31/(M4*(ET-E31)))*SIND(THETA))
               E41   =ET-E31
              CRIT41 = SQRT( R23/R14-SIND(PHI1)**2)
 
               E32   =ET*R13*(COSD(THETA)-SQRT(CRIT3))**2
               PHI2  = ASIND(SQRT(M3*E32/(M4*(ET-E32)))*SIND(THETA))
               E42   =ET-E32
              CRIT42 = SQRT( R23/R14-SIND(PHI2)**2)

                      WRITE(22,*) THETA,E31,PHI1,E41,E32,PHI2,E42

                      PRINT 25, THETA,E31,PHI1,E41,E32,PHI2,E42 
  25  FORMAT('     ',F6.3,'    ',F6.3,'    ',F6.3,'    ',F6.3,
     *'    ',F6.3,'    ',F6.3,'    ',F6.3)


              THETA=THETA+1.0
     
              CRIT3  =  ((R24/R13)-(SIND(THETA))**2)

                 ENDDO
               TCRIT  =  ASIND(SQRT(R24/R13))

                 THETA=TCRIT    
               
             
               E31   =ET*R13*(COSD(THETA))**2
               PHI1  = ASIND(SQRT(M3*E31/(M4*(ET-E31)))*SIND(THETA))
               E41   =ET-E31
     
               E32   =ET*R13*(COSD(THETA))**2
               PHI2  = ASIND(SQRT(M3*E32/(M4*(ET-E32)))*SIND(THETA))
               E42   =ET-E32

             WRITE(22,*) THETA,E31,PHI1,E41,E32,PHI2,E42
        PRINT 30, THETA,E31,PHI1,E41,E32,PHI2,E42 
  30  FORMAT('     ',F6.3,'    ',F6.3,'    ',F6.3,'    ',F6.3,
     *'    ',F6.3,'    ',F6.3,'    ',F6.3,/)


      ELSEIF(M3.LE.M4) THEN        

      PRINT *, '    THETA       E31      PHI1       E41      E32 
     *      PHI2      E42'
         
        DO WHILE((THETA).LE.90.0)

              
              CRIT3  =  (R24/R13)-(SIND(THETA))**2
               E31   =ET*R13*(COSD(THETA)+SQRT(CRIT3))**2
               PHI1  = ASIND(SQRT(M3*E31/(M4*(ET-E31)))*SIND(THETA))
               E41   =ET-E31

               E32   =ET*R13*(COSD(THETA)-SQRT(CRIT3))**2
               PHI2  = ASIND(SQRT(M3*E32/(M4*(ET-E32)))*SIND(THETA))
               E42   =ET-E32
           
           WRITE(22,*) THETA,E31,PHI1,E41,E32,PHI2,E42

                PRINT 35, THETA,E31,PHI1,E41,E32,PHI2,E42 
  35  FORMAT('     ',F6.3,'    ',F6.3,'    ',F6.3,'    ',F6.3,
     *'    ',F6.3,'    ',F6.3,'    ',F6.3)

          THETA=THETA+1.0 
 
        ENDDO
      ENDIF

         
  40  PRINT *,'    ANOTHER SCATTERING PROBLEM (S), OR EXIT (E)'
      READ *, COMMAN
      DO WHILE (COMMAN .NE. 'E')
         IF (COMMAN.EQ. 'S') THEN
      CLOSE(UNIT=22)
         GOTO 5
         ELSEIF (COMMAN.NE.'E') THEN
             PRINT *,' NO SUCH COMMAND'
             PRINT *,' '
         ENDIF
      GOTO 40      
      ENDDO

      CLOSE(UNIT=22)

      END
