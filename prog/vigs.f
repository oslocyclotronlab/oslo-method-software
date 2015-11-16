       PROGRAM VIGS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      THIS PROGRAM CCALCULATES THE FEEDING TO THE VIBR.-   C
C      STATES RELATIV TO THE FEEDING TO THE VIBR. + GSB    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)

      CHARACTER fname*8,comm*60
      CHARACTER APP*4

      CHARACTER TEX*4,CH(4)*1,ANS*1
      DIMENSION IASC(0:9)

      REAL GAMW(0:2047),N(0:2047),E1,E2,ER,AMASS,FWHM,RHOV,RHOG
      REAL VA,GA,VVGA,V3,G3,VVG3,V4,G4,VVG4,V5,G5,VVG5

      WRITE(6,*)'       ******************************************'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *                V I G S                 *'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *    PROGRAM TO CCALCULATE THE FEEDING   *'
      WRITE(6,*)'       *   TO THE VIBR. AND GROUND STATE BANDS  *'
      WRITE(6,*)'       *             1.3.1994/MG                *'
      WRITE(6,*)'       ******************************************'

C******************INITIALISATION******************************
      Istatus=0
      IDEST=1
      ITYPE=3
      XDIM=2048
      YDIM=512

C Calibration default. cal(i,j,k,l) i deduced from ITYPE
C i=1 is ITYPE=2,3 and i=2 is ITYPE=1 ,j=1/2,k=x/y-cal, (3-terms)
C ITYPE=1: singles spectrum. ITYPE=2 and 3 is 2-dimensional matrices
C j is destination IDEST or source ISP spectrum
      DO i=1,2
         DO j=1,2
         DO k=1,2
            DO l=1,3
              cal(i,j,k,l)=0.
            ENDDO
            cal(i,j,k,2)=1.
            IF(i.EQ.2)cal(i,j,k,2)=0.
          ENDDO
        ENDDO
      ENDDO

      IASC(0)=48
      IASC(1)=49
      IASC(2)=50
      IASC(3)=51
      IASC(4)=52
      IASC(5)=53
      IASC(6)=54
      IASC(7)=55
      IASC(8)=56
      IASC(9)=57

      DO i=1,512
        nn=1000
        II=i
        IISTAT=0
        JJ=1
        CH(1)=' '
        DO J=1,4
          IDIG=II/nn
          CH(JJ)=CHAR(IASC(IDIG))
          IF(IDIG.EQ.0.AND.IISTAT.EQ.0)GO TO 9
          II=II-IDIG*nn
          IISTAT=1
          JJ=JJ+1
 9        nn=nn/10
        ENDDO
        TEX=CH(1)//CH(2)//CH(3)//CH(4)
        LIN=LEN(TEX)
        APP(i)=TEX(1:LIN)
      ENDDO

C
C
C                            INITIALIZATION
C
C

      IREAD=5
      IRINP=6
      IOUTP=6
      IDEVICE=6
      ETHRES=430.
      AMASS=172.
      FWHM=5000
      RHOV=44./1000.              !Levels pr. keV
      RHOG=6./1000.
      VIBE=1200.
      GROE=400.
      WIDTH1=400.
      WIDTH2=500.

      CALL READFILE
      IF(Istatus.NE.0)STOP
      LSPEC=XDIM
      IF(ITYPE.EQ.1)LSPEC=MAXCH+1

C READING IN CALIBRATION
      AX0=cal(1,IDEST,1,1)
      AX1=cal(1,IDEST,1,2) 
      AY0=cal(1,IDEST,2,1)
      AY1=cal(1,IDEST,2,2)
      IF(AX0+AX1.EQ.1.)THEN
        AX0=11.
        AX1=20.
      ENDIF
      IF(AY0+AY1.EQ.1.)THEN
        AY0=9660.
        AY1=-200.
      ENDIF

 91   WRITE(6,10)AX0
  10  FORMAT(/'Cal. coeff. a0 (keV) on x-axis   <',F8.1,'>:',$)
      CALL READF(5,AX0)
      WRITE(6,11)AX1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch) on x-axis<',F8.1,'>:',$)
      CALL READF(5,AX1)

      WRITE(6,12)AY0
  12  FORMAT(/'Cal. coeff. a0 (keV) on y-axis   <',F8.1,'>:',$)
      CALL READF(5,AY0)
      WRITE(6,13)AY1
  13  FORMAT( 'Cal. coeff. a1 (keV/ch) on y-axis<',F8.1,'>:',$)
      CALL READF(5,AY1)
      DEL=ABS(AY1)
      IF(Istatus.NE.0)GO TO 91

C Finding excitation energy of highest gate
      EX=AY0+AY1*(YDIM-1)
      IF(EX.LT.AY0)EX=AY0
      IF(EX.GT.9000.OR.EX.LT.100)EX=9000
      WRITE(6,100)EX
 100  FORMAT(/'Exc. energy of highest gate (keV)<',F8.1,'>:',$)
  92  CALL READF(5,EX)
      IF(Istatus.NE.0)GO TO 92
      IF(ITYPE.LE.1)THEN
        DO I=0,MAXCH
          rMAT(IDEST,I,0)=rSPEC(IDEST,I)
        ENDDO
      ENDIF


C Summing up
      CALL CHAN(CEX,EX,AY0,AY1,YDIM)
      CALL CHAN(CE0,0.,AY0,AY1,YDIM)
      JEX=CEX+0.5
      JE0=CE0+0.5
      IF(JEX.GT.YDIM-1)JEX=YDIM-1
      IF(JE0.GT.YDIM-1)JE0=YDIM-1
      IF(JEX.LT.0)JEX=0
      IF(JE0.LT.0)JE0=0
      EX =AY0+AY1*JEX
      EX0=AY0+AY1*JE0
      WRITE(6,401)EX,JEX,EX0,JE0
 401  FORMAT(/'The calculations will be performed',/,
     +'from    Ex= ',F7.1,' keV corresponding to Y-ch=',I5,/,
     +'down to Ex= ',F7.1,' keV corresponding to Y-ch=',I5)
      ANS='y'
      WRITE(6,400)ANS
 400  FORMAT(/'Are you happy so far? <',A1,'>',$)
      CALL READA1(5,ANS)
      IF(ANS.EQ.'n'.OR.ANS.EQ.'N')GO TO 91
      Jincr=-1
      IF(JEX.LT.JE0)Jincr=+1


 93   WRITE(6,402)VIBE
 402  FORMAT(/'Exc. energy of vibr. states  (keV)<',F6.1,'>:',$)
      CALL READF(5,VIBE)

      WRITE(6,409)GROE
 409  FORMAT( 'Exc. energy of g.s.b. states (keV)<',F6.1,'>:',$)
      CALL READF(5,GROE)

      WRITE(6,404)ETHRES
 404  FORMAT(/'Threshold (keV) for statistical gammas <',F6.1,'>:',$)
      CALL READF(5,ETHRES)

      WRITE(6,403)WIDTH1
 403  FORMAT(/'Width (keV) of 1 MeV gamma bump        <',F6.1,'>:',$)
      CALL READF(5,WIDTH1)

      WRITE(6,408)WIDTH2
 408  FORMAT( 'Width (keV) of gamma bump at Egam=Ex   <',F6.1,'>:',$)
      CALL READF(5,WIDTH2)

      WRITE(6,407)AMASS
 407  FORMAT(/'Mass number A <',F5.1,'>:',$)
      CALL READF(5,AMASS)

      WRITE(6,405)RHOV
 405  FORMAT(/'Number of accessible vibr. states/keV   <',F8.4,'>:',$)
      CALL READF(5,RHOV)

      WRITE(6,406)RHOG
 406  FORMAT( 'Number of accessible gr.band states/keV <',F8.4,'>:',$)
      CALL READF(5,RHOG)                           

      WRITE(6,400)ANS
      CALL READA1(5,ANS)
      IF(ANS.EQ.'n'.OR.ANS.EQ.'N')GO TO 93

C STARTING THE CALCULATION AND OUTPUTS
      ER = 80000.*(AMASS**(-1./3.))
      DO I = 0,2047
        E1 = I*AX1 + AX0
        GAMW(I) = (E1**4)*(FWHM**2)/((ER**2-E1**2)**2+
     $  (E1**2)*(FWHM**2))
      ENDDO
      DO I = 0,2046
        E1 = I*AX1 + AX0
        E2 = (I+1)*AX1 + AX0
        N(I) = ALOG(GAMW(I+1)/GAMW(I))/ALOG(E2/E1)
      ENDDO

      OPEN(UNIT=21,FILE='vigsout.dat')
      WRITE(6,420)
      WRITE(21,420)
 420  FORMAT(/'Y-ch  Ex    Itot  Igsb  Ivib G/tot V/tot V/V+G  Axel',
     $'  n=3   n=4   n=5  Axel-n' )
 421  FORMAT(I4,F6.0,3I6,7F6.3,2F4.1)


      DO J=JEX,JE0,Jincr
        WIDTH=WIDTH1+((WIDTH2-WIDTH1)/7000.)*(EX-1000.)
        IF(EX.LT.1)WIDTH=WIDTH1

        EVL=EX-VIBE-WIDTH/2.
        EVH=EX-VIBE+WIDTH/2.
        EGL=EX-GROE-WIDTH/2.
        EGH=EX-GROE+WIDTH/2.

        CALL CHAN(CHTH,ETHRES,AX0,AX1,LSPEC)
        CALL CHAN(CHVL,EVL,AX0,AX1,LSPEC)
        CALL CHAN(CHVH,EVH,AX0,AX1,LSPEC)
        CALL CHAN(CHGL,EGL,AX0,AX1,LSPEC)
        CALL CHAN(CHGH,EGH,AX0,AX1,LSPEC)

        ITOT=0
        IGSB=0
        IVIB=0
        VA=0.
        V3=0.
        V4=0.
        V5=0.
        GA=0.
        G3=0.
        G4=0.
        G5=0.
        VVGA=0.
        VVG3=0.
        VVG4=0.
        VVG5=0.

        DO I=0,LSPEC-1
          ISP=rMAT(IDEST,I,J)
          IF(I.GT.CHTH.AND.I.LT.CHGH)ITOT=ITOT+ISP
          IF(I.GT.CHVL.AND.I.LT.CHVH)THEN
            IVIB=IVIB+ISP
          ENDIF
          IF(I.GT.CHGL.AND.I.LT.CHGH)THEN
            IGSB=IGSB+ISP
          ENDIF
        ENDDO

        TOT=ITOT
        IF(TOT.LT.1.1)TOT=99999
        GTOT=IGSB/TOT
        VTOT=IVIB/TOT
        VG=IVIB+IGSB
        IF(VG.LT.1.1)VG=99999
        VVG=IVIB/VG

        EXV=EX-VIBE
        EXG=EX-GROE
        IF(EXV.LT.0.)EXV=0
        CALL CHAN(CH1,EXG,AX0,AX1,LSPEC)
        CALL CHAN(CH2,EXV,AX0,AX1,LSPEC)
C         GA=(EXG)**(N(CH1))*RHOG
C         VA=(EXV)**(N(CH2))*RHOV
        GA=RHOG*FWHM*(EXG**4)/((EXG*EXG-ER*ER)**2+FWHM*FWHM*EXG*EXG)
        VA=RHOV*FWHM*(EXV**4)/((EXV*EXV-ER*ER)**2+FWHM*FWHM*EXV*EXV)
        G3=(EXG)**3*RHOG
        V3=(EXV)**3*RHOV
        G4=(EXG)**4*RHOG
        V4=(EXV)**4*RHOV
        G5=(EXG)**5*RHOG
        V5=(EXV)**5*RHOV


        IF (VA.GT.0.) VVGA = VA/(VA+GA)
        IF (V3.GT.0.) VVG3 = V3/(V3+G3)
        IF (V4.GT.0.) VVG4 = V4/(V4+G4)
        IF (V5.GT.0.) VVG5 = V5/(V5+G5)


        WRITE(21,421)J,EX,ITOT,IGSB,IVIB,GTOT,VTOT,VVG,VVGA,VVG3,
     $  VVG4,VVG5,N(INT(CH1)),N(INT(CH2))
        WRITE(6,421) J,EX,ITOT,IGSB,IVIB,GTOT,VTOT,VVG,VVGA,VVG3,
     $  VVG4,VVG5,N(INT(CH1)),N(INT(CH2))

        EX=EX-DEL
      ENDDO
      END


      SUBROUTINE CHAN(CH,E,AX0,AX1,LSPEC)
      CH=(E-AX0)/AX1
      IF(CH.LT.0)CH=0
      IF(CH.GT.LSPEC)CH=LSPEC-1
      END
