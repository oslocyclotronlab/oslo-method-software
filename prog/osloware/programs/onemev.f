       PROGRAM ONEMEV
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      THIS PROGRAM CCALCULATES THE 1 MeV BUMP IN NAI SPECTRA C
C      STATES RELATIV TO THE FEEDING TO THE VIBR. + GSB      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      REAL Spec(0:8191)
      REAL Calib(6)
      CHARACTER fname*8,comm*60,comment*60

      CHARACTER APP*4
      CHARACTER TEX*4,CH(4)*1,ANS*1
      DIMENSION IASC(0:9)
      DIMENSION HV(0:511),HH(0:511)
      REAL NBRU(0:2047),NBAC(0:2047),NNET(0:2047)
  
      WRITE(6,*)'      ******************************************'
      WRITE(6,*)'      *                                        *'
      WRITE(6,*)'      *             O N E M E V                *'
      WRITE(6,*)'      *                                        *'
      WRITE(6,*)'      *     PROGRAM TO CALCULATE THE 1 MeV     *'
      WRITE(6,*)'      *   BUMP IN NAI SPECTRA. BACKGROUND IS   *'
      WRITE(6,*)'      * A PARABOLA GOING THROUGH Y(430 keV)=0  *'
      WRITE(6,*)'      *   AND HAVING DERIVATE = 0 AT 1700 keV  *'
      WRITE(6,*)'      *             1.3.1994/MG                *'
      WRITE(6,*)'      ******************************************'

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

      IRINP=3
      IOUTP=6
      IDEVICE=20

C PARAMETER AND DATA INPUT
      DO J=0,511
       HV(J)=0.
        DO I=0,2047
          rMAT(IDEST,I,J)=0
          NBRU(I)=0
          NBAC(I)=0
          NNET(I)=0
        ENDDO
      ENDDO
 
      EX=9000.
      EL=700.
      EH=1700.
      BL=430.
      BH=1900.
      WIDTH=400.
      EXL=3000.
      E1=2300.
      E2=8000.
      Efeed=370.

  90  CALL READFILE
      IF(Istatus.NE.0)GO TO 90
      LSPEC=XDIM
      IF(ITYPE.EQ.1)LSPEC=MAXCH+1

C READING IN CCALIBRATION
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
 100  FORMAT(/'Exc. energy of highest gate (keV)<',F7.1,'>:',$)
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


 93    WRITE(6,402)EL
 402  FORMAT(/'Lower marker for bump  (keV)       <',F6.1,'>:',$)
      CALL READF(5,EL)

      WRITE(6,409)EH
 409  FORMAT( 'Higher marker for bump (keV)       <',F6.1,'>:',$)
      CALL READF(5,EH)

      WRITE(6,404)BL
 404  FORMAT(/'Lower marker for background  (keV) <',F6.1,'>:',$)
      CALL READF(5,BL)

      WRITE(6,403)BH
 403  FORMAT( 'Higher marker for background (keV) <',F6.1,'>:',$)
      CALL READF(5,BH)

      WRITE(6,408)WIDTH
 408  FORMAT(/'Width of fit-region for higher backgr.  <',F6.1,'>:',$)
      CALL READF(5,WIDTH)

      WRITE(6,407)EXL
 407  FORMAT( 'Up to what exc. energy (keV) is bckg.=0 <',F6.1,'>:',$)
      CALL READF(5,EXL)

      WRITE(6,410)E1
 410  FORMAT(/'Lower exc. energy for total spec.  (keV) <',F7.1,'>:',$)
      CALL READF(5,E1)

      WRITE(6,411)E2
 411  FORMAT( 'Higher exc. energy for total spec. (keV) <',F7.1,'>:',$)
      CALL READF(5,E2)

      WRITE(6,412)Efeed
 412  FORMAT(/'Aver. exc. energy (keV) for gamma-feeding <',F6.1,'>:',$)
      CALL READF(5,Efeed)

      WRITE(6,400)ANS
      CALL READA1(5,ANS)
      IF(ANS.EQ.'n'.OR.ANS.EQ.'N')GO TO 93

C STARTING THE CALCULATION AND OUTPUTS
      FL=BH-WIDTH/2.
      FH=BH+WIDTH/2.
      CALL CHAN(CF1,FL,AX0,AX1,LSPEC)
      CALL CHAN(CF2,FH,AX0,AX1,LSPEC)
      CALL CHAN(CL1,BL,AX0,AX1,LSPEC)
      CALL CHAN(CL2,BH,AX0,AX1,LSPEC)
      CALL CHAN(CM1,EL,AX0,AX1,LSPEC)
      CALL CHAN(CM2,EH,AX0,AX1,LSPEC)
      
      DO J=JEX,JE0,Jincr
        ICF=0
        DO I=0,LSPEC-1
          ISP=rMAT(IDEST,I,J)
          IF(I.GT.CF1.AND.I.LT.CF2)THEN
            HV(J)=HV(J)+ISP
            ICF=ICF+1
          ENDIF
        ENDDO
        HV(J)=HV(J)/ICF
      ENDDO

C SMOOTHING H AND TERMINATING TO ZERO FOR EX=EH
      HH(JEX)=(HV(JEX)+HV(JEX+Jincr))/2.
      HH(JE0)=(HV(JE0)+HV(JE0-Jincr))/2.
      DO J=JEX+Jincr,JE0-Jincr,Jincr
        HH(J)=0.25*HV(J-Jincr)+0.5*HV(J)+0.25*HV(J+Jincr)
      ENDDO

      N2=(EX-EXL)/DEL+0.5
      N1=(EX-EH)/DEL+0.5
      H2=HH(N2)
      DO I=N2,NGATES
        HH(I)=H2+(H2*(I-N2)/(N2-N1))
        IF(HH(I).LT.0)HH(I)=0.
      ENDDO

      OPEN(UNIT=21,FILE='onemevout.dat')
      WRITE(6,420)
      WRITE(21,420)
 420  FORMAT(/' Y-ch   Ex    Tot  H  Brutto Back Netto Net/Tot',
     1 ' Mult  No1MeV Ngam   Eave')
 421  FORMAT(I4,F7.0,I6,I4,3I6,F6.2,2F7.2,I6,F7.1)


      DO J=JEX,JE0,Jincr
        EGH=EX+300. ! Going somewhat outside due to energy resolution
        CALL CHAN(CEX,EGH,AX0,AX1,LSPEC) ! Gammaspectrum < channel CEX
        ITOT=0
C CCALCULATING COEFF. A B AND C FOR PARABOLA-BACKGR.
        A=-HH(J)/(CL2-CL1)**2.
        B=-2*A*CL2
        C=A*CL2**2.+HH(J)
        IB=0
        IBC=0
        IN=0
        DO I=0,LSPEC-1
          ISP=rMAT(IDEST,I,J)
          IF(I.GT.CL1.AND.I.LT.CEX)ITOT=ITOT+ISP
          IBAC=0
          IF(I.GT.CL1.AND.I.LT.CF2)IBAC=A*I*I+B*I+C+0.5
          IF(I.GT.CM1.AND.I.LT.CM2)THEN
            INETT=ISP-IBAC
            IB=IB+ISP
            IBC=IBC+IBAC
            IN=IN+INETT
          ENDIF
          IF(EX.GT.E1.AND.EX.LT.E2)THEN
            NBRU(I)=NBRU(I)+ISP
            NBAC(I)=NBAC(I)+IBAC
            IF(I.LE.CL1.OR.I.GE.CF2)IBAC=ISP
            NNET(I)=NNET(I)+ISP-IBAC
          ENDIF
        ENDDO

        TOT=ITOT
        IF(TOT.LT.1.1)TOT=99999
        IH=HH(J)+0.5
        FRAC=IN
        FRAC=FRAC/TOT

C CALCULATING MULTIPLICITY FROM FORMULA: M=Ex/EaveGamma
        EAVE=0
        IAVE=0
        DO I=CL1,CEX
          EAVE=EAVE+(AX0+AX1*I)*rMAT(IDEST,I,J)
          IAVE=IAVE+rMAT(IDEST,I,J)
        ENDDO
        IF(EAVE.LE.0.OR.IAVE.LE.0)THEN
          XM=0
          XNO=0
        GO TO 426
        ENDIF
        EAVE=EAVE/IAVE
        XM = (EX-Efeed)/EAVE        ! Landing in 370 keV region
        IF(XM.LT.0)XM=0.
        XNO=FRAC*XM
  426   CONTINUE
        WRITE(21,421)J,EX,ITOT,IH,IB,IBC,IN,FRAC,XM,XNO,IAVE,EAVE
        WRITE(6,421) J,EX,ITOT,IH,IB,IBC,IN,FRAC,XM,XNO,IAVE,EAVE
        EX=EX-DEL
      ENDDO

C WRITING TOT-SPECTRA TO DISK
        SPSIZE=LSPEC

        DO i=0,LSPEC-1
          Spec(i)=NBRU(i)
        ENDDO
        comment='Brutto-spectrum'
        OPEN(IDEVICE,FILE='BRUTTO-1MEV',ACCESS='SEQUENTIAL',ERR=425)
        CALL norw1dim(IDEVICE,comment,LSPEC,Spec,Calib)
        CLOSE(IDEVICE)
        WRITE(IOUTP,430)
  430   FORMAT(/' Spectrum BRUTTO-1MEV written to disc')

        DO i=0,LSPEC-1
          Spec(i)=NBAC(i)
        ENDDO
        comment='Background-spectrum'
        OPEN(IDEVICE,FILE='BACKGROUND-1MEV',ACCESS='SEQUENTIAL',ERR=425)
        CALL norw1dim(IDEVICE,comment,LSPEC,Spec,Calib)
        CLOSE(IDEVICE)
        WRITE(IOUTP,431)
  431   FORMAT(/' Spectrum BACKGROUND-1MEV written to disc')

        DO i=0,LSPEC-1
          Spec(i)=NNET(i)
        ENDDO
        comment='Netto-spectrum'
        OPEN(IDEVICE,FILE='NETTO-1MEV',ACCESS='SEQUENTIAL',ERR=425)
        CALL norw1dim(IDEVICE,comment,LSPEC,Spec,Calib)
        CLOSE(IDEVICE)
        WRITE(IOUTP,432)
  432   FORMAT(/' Spectrum NETTO-1MEV written to disc')

        GO TO 9998
  425   WRITE(6,*)' WRITTING FILE ERROR'

        GO TO 9998
 9999   WRITE(6,*)' NO FILE ACCESS'
 9998   CONTINUE
        END
         

       SUBROUTINE CHAN(CH,E,AX0,AX1,LSPEC)
       CH=(E-AX0)/AX1              ! Remember only real numbers here
       IF(CH.LT.0)CH=0
       IF(CH.GT.LSPEC)CH=LSPEC-1
       END
