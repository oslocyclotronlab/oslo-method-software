      PROGRAM AUTOFIT
C THIS PROGRAM calibrCULATES THE CalibrationATION CONSTANTS FOR  
C NAI SPECTRA OF BACKGROUND-LINES 1460.8 AND 2614.6 keV 

      DIMENSION HV(32),HVS(32)                                       !autofit
      COMMON/calibrIB/NUMB,TOPS(400,32,3),IPEAK(32),MT(2048,32),FWHMCH  !autofit
      COMMON/SPECT/ISPEC(8192),NOCH,calibr(5),Ncalibr                      !autofit
      COMMON/PEAK/NOPEAK,PEAKS(400,4)                                !autofit
    
CRead/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER APP*4
                                         
      WRITE(6,*)'        ________________________________________ '
      WRITE(6,*)'       |                                        |'
      WRITE(6,*)'       |            A U T O F I T               |'
      WRITE(6,*)'       |                                        |'
      WRITE(6,*)'       |        Program to calculate the        |'
      WRITE(6,*)'       |    calibration of NaI spectra using    |'
      WRITE(6,*)'       |  the background-lines at 1460.8 (40K)  |'
      WRITE(6,*)'       |        and 2614.6 keV (228Th).         |'
      WRITE(6,*)'       |    New PMT HV-values are suggested     |'
      WRITE(6,*)'       |              1.9.1994/MG               |'
      WRITE(6,*)'       |________________________________________|'

C INITIALIZATION

      Iout=6
      Idev=6
      IDEST=1
      ITYPE=3

      APP(1)='1'
      APP(2)='2'
      APP(3)='3'
      APP(4)='4'
      APP(5)='5'
      APP(6)='6'
      APP(7)='7'
      APP(8)='8'
      APP(9)='9'
      APP(10)='10'
      APP(11)='11'
      APP(12)='12'
      APP(13)='13'
      APP(14)='14'
      APP(15)='15'
      APP(16)='16'
      APP(17)='17'
      APP(18)='18'
      APP(19)='19'
      APP(20)='20'
      APP(21)='21'
      APP(22)='22'
      APP(23)='23'
      APP(24)='24'
      APP(25)='25'
      APP(26)='26'
      APP(27)='27'
      APP(28)='28'
      APP(29)='29'
      APP(30)='30'
      APP(31)='31'
      APP(32)='32'

C PARAMETER AND DATA INPUT
 9997 CONTINUE
      DO I=1,2048
        DO J=1,32
          MT(I,J)=0
        ENDDO
      ENDDO

      DO I=1,5
        calibr(I)=0.
      ENDDO

      NSPEC=32
      NOCH=2048
      XDIM=2048
      YDIM=32

      WA1=1.0
      WA0=0.
      FWHM=6.0
         
      DO J=1,32
        HV(J)=850.
      ENDDO
      ILP=0
      IANS=0
      ITIME=0

C First looking at own home-directory
      OPEN(23,FILE='autoin.dat',STATUS='OLD',ERR=51)
      GO TO 50
51    WRITE(6,*)'Did not find autoin.dat in your directory'
      WRITE(6,*)'Taking instead /d2/fys/mama/resp/autoin.dat'
      OPEN(23,FILE='/d2/fys/mama/resp/autoin.dat',
     1STATUS='OLD',ERR=90)
50    CONTINUE
      READ(23,*,ERR=90)XDIM,YDIM,IANS,ILP
      READ(23,*,ERR=90)NSPEC,ITYPE,NOCH,WA1,WA0,FWHM
      READ(23,*,ERR=90)(HV(J),J=1,32)
90    CLOSE(23)

C Reading in spectra to be fitted
      CALL READFILE
      IF(ITYPE.GT.1)THEN
        IF(XDIM.GT.2048)XDIM=2048
        IF(YDIM.GT.32  )YDIM=32
        DO I=0,XDIM-1
          DO J=0,YDIM-1
            MT(I+1,J+1)=rMAT(IDEST,I,J)   !matrix
          ENDDO
        ENDDO
        NSPEC=YDIM
        NOCH=XDIM
      ELSE
        IF(MAXCH.GT.2047)MAXCH=2047
        J=0
        DO I=0,MAXCH
          MT(I+1,J+1)=rSPEC(IDEST,I)     !singles spectrum
        ENDDO
        NSPEC=1
        NOCH=MAXCH+1
      ENDIF

 9996 ITIME=0
      WRITE(6,15)WA0
  15  FORMAT('Wanted a0(keV) calibration coefficient for ', 
     +'NaI detector    <',F8.1,'>:',$)
      CALL READF(5,WA0)
      calibr(1)=WA0

      WRITE(6,16)WA1
  16  FORMAT('Wanted a1(keV/ch) calibration coefficient for ', 
     +'NaI detector <',F8.4,'>:',$)
      CALL READF(5,WA1)
      calibr(2)=WA1

      WRITE(6,17)FWHM
  17  FORMAT('Resolution FWHM (%) at 1332 keV <',F5.1,'>:',$)
      CALL READF(5,FWHM)
      FWHMCH=(1332*FWHM)/(100*WA1)

 999  ITIME=ITIME+1

      WRITE(6,*)' '
      DO J=1,NSPEC
        WRITE(6,18)J,HV(J)
  18    FORMAT('High-voltage for NaI',I3,' is <',F6.1,'>:',$)
        CALL READF(5,HV(J))
      ENDDO

      OPEN(23,FILE='autoin.dat',ERR=666)
      WRITE(23,*)XDIM,YDIM,IANS,ILP
      WRITE(23,*)NSPEC,ITYPE,NOCH,WA1,WA0,FWHM
      WRITE(23,*)(HV(J),J=1,32)
      CLOSE(23)

 666  CONTINUE

C STARTING THE calibrCULATION
      DISC=40
      KMAX=2
      IPLOT=0
      Ncalibr=2

      DO J=1,32
        IPEAK(J)=0
        DO I=1,400
          DO K=1,3
            TOPS(I,J,K)=0
          ENDDO
        ENDDO
      ENDDO

C MAKING PEAKFIND
      DO J=1,NSPEC
        DO I=1,NOCH
          ISPEC(I)=MT(I,J)
        ENDDO
        NUMB=J
        Iout=0
        CALL PEAKFI(FWHMCH,DISC,KMAX,IPLOT,Iout,APP(J))
      ENDDO

   37 IF(Idev.EQ.22)THEN
        OPEN(Idev,FILE='autofitout.dat',ACCESS='SEQUENTIAL',ERR=9999)
      ENDIF

      WRITE(Idev,20)
      WRITE(Idev,21)FWHM,FWHMCH
      WRITE(Idev,23)WA0,WA1
      WRITE(Idev,20)
      WRITE(Idev,24)
      WRITE(Idev,25)
      WRITE(Idev,26)
      WRITE(Idev,27)
      WRITE(Idev,28)
   21 FORMAT(
     1' FWHM in peakfind search is ',F4.1,' %  (',F5.1,' channels)')
   20 FORMAT(
     1' =====================================',
     2'========================================')
   23 FORMAT(
     1' Wanted calibration in spectrum: a0=',F7.1,' keV',/
     2'                                 a1=',F7.4,' keV/ch')
   28 FORMAT(
     1' -------------------------------------',
     2'----------------------------------------')
   24 FORMAT(
     1'    Detector                    Present        ',
     2'             Suggested ')
   25 FORMAT(
     1' ---------------   ---------------------------------  ',
     2'------------------------')
   26 FORMAT(
     1' Det. FWHM  Int.    40K  228Th   a0      a1    HV  ',
     2'   HV    HVsh  gain  ADCsh')
   27 FORMAT(
     1' no.  (%)  (1000)  (ch)  (ch)  (keV) (keV/ch)  (V)  ',
     2'  (V)    (V)         (ch)  ')


      AVFWR=0.
      AVAR =0.
      AVA0 =0.
      AVA1 =1.
      NACTIV=NSPEC
      DO J=1,NSPEC
        NUMB=J
        IF(IPEAK(J).LT.1) THEN
          chK=0
          chTh=0
          FWR=0
          AR=0
          A0=0
          A1=0
          HVS(J)=0
          ISHIFT=0
          ISHIFTS=0
          GO TO 29
        ENDIF
        CALL Calibration(A0,A1,FWR,AR,NOCH,IA1,IA2,chK,chTh)
        ISHIFT=A0/A1
        ALPHA=A1/WA1
        HVG=HV(J)
        CALL NEW(HVG,HVN,ALPHA)
        HVS(J)=HVN
        ISHIFTS=A0/WA1

   29   CONTINUE
        IF(IA1.EQ.0)THEN
          WRITE(Idev,*)'  No peaks found'
        ELSE
          DeltaHV=0
          IF(HVS(J).GT.0)DeltaHV=HVS(J)-HV(J)
          WRITE(Idev,30)J,FWR,AR,chK,chTh,A0,A1,HV(J),
     1    HVS(J),DeltaHV,ALPHA,ISHIFTS
        ENDIF


        IF(FWR.EQ.0.OR.AR.EQ.0.OR.A1.EQ.0)NACTIV=NACTIV-1
        AVFWR=AVFWR+FWR
        AVAR=AVAR+AR
        AVA0=AVA0+A0
        AVA1=AVA1+A1
   30 FORMAT(I3,F6.1,F6.1,F7.1,F7.1,F7.1,F7.3,F7.1,F8.1,F6.1,F7.4,I5)
        IX=(J+0.0001)/5.
        IX=IX*5.
        IF(J.EQ.IX)WRITE(Idev,28)
      ENDDO

      WRITE(Idev,20)
      AVFWR=AVFWR/NACTIV
      AVAR=AVAR/NACTIV
      AVA0=AVA0/NACTIV
      AVA1=AVA1/NACTIV
      WRITE(Idev,31)AVFWR,AVAR,AVA0,AVA1
  31  FORMAT(1X,'  ',F6.1,F6.1,14X,F7.1,F7.3,'  are average values')
      WRITE(Idev,20)
      IF(Idev.EQ.22)THEN
        WRITE(Idev,41)
  41    FORMAT(//)
        WRITE(Idev,20)
        DO J=1,NSPEC
          WRITE(Idev,42)J
  42      FORMAT(1X,'Spectrum no:',I3)
          DO I=1,10
            WRITE(Idev,40)I,(TOPS(I,J,L),L=1,3),I+10,
     +      (TOPS(I+10,J,L),L=1,3)
  40        FORMAT(1X,I3,F8.1,F8.0,F5.1,5X,I3,F8.1,F8.0,F5.1)
          ENDDO
          WRITE(Idev,20)
        ENDDO
      ENDIF
      IF(Idev.EQ.22)THEN
        CLOSE(22)
        GO TO 9998
      ENDIF
      WRITE(6,36)ILP
  36  FORMAT(/'Dump result on file (autofitout.dat) (yes/no=1/0)',
     1          '<',I2,'>:',$)
      CALL READI(5,ILP)
      IF(ILP.EQ.1)THEN
        Idev=22
        GO TO 37
      ENDIF

      GO TO 9998
 9999 WRITE(6,*)'NO FILE ACCESS'

 9998 Idev=6
      WRITE(6,34)
   34 FORMAT(/'Replace new suggested HV-values and continue (0)',/,
     1        'Change parameters                            (1)',/,
     2        'Start all over                               (2)',/,
     3        'EXIT                                         (3)')
      WRITE(6,32)IANS
  32  FORMAT(/'Please, give your answer <',I1,'>:',$)
      CALL READI(5,IANS)

      IF(IANS.NE.0.AND.IANS.NE.1.AND.IANS.NE.2.AND.IANS.NE.3)
     1GO TO 9998
      IF(IANS.EQ.0)THEN
        DO J=1,32
          HV(J)=HVS(J)
        ENDDO
        GO TO 999
      ENDIF
      IF(IANS.EQ.1)GO TO 9996
      IF(IANS.EQ.2)GO TO 9997
      END


      SUBROUTINE PEAKFI(FWHM,DISC,KMAX,IPLOT,Iout,RUN)
C  PEAKFIND VERSION FOR DEC-10.20/47/77.F.I.GENERATES
C  PEAK LIST WITH OPTIONAL PRINTERPLOT TO SELECTED
C  DEVICE. CALLED FROM SPECTIO, WITH 8192 CHANNEL
C  SPECTRUM IN COMMON.
      CHARACTER RUN*4
      DIMENSION SPEC(8192),F(200),F1(200)
      DIMENSION PKS(200),JMARK(200),SOUTP(200)
      INTEGER SOUTP,BLNK,STAR,EXX,BBC,SPEC,MEN,ZRO,ONE
      DIMENSION XCHA(50),YCHA(50),WG(50),EF(5,5),C(5)
      COMMON/SPECT/NSPEC(8192),NOCH,CE(5),Ncalibr
      COMMON/PEAK/NOPEAK,PEAKS(400,4)
      COMMON/calibrIB/NUMB,TOPS(400,32,3),IPEAK(32),MT(2048,32),FWHMCH

      BBC=4HBBBB
      BLNK=4H
      STAR=4H****
      IBCH=4HIIII
      EXX=4HXXXX
      MEN=-1
      ZRO=0
      ONE=1
      C0=CE(1)
      C1=CE(2)
      C2=CE(3)
      C3=CE(4)
      C4=CE(5)
      
C  MAKE THE CORRELATION SHAPE (GAUSSIAN IN THIS VERSION)
C   CON1=2.*SQRT(ALOG(2.)/3.1415926)/0.6667/FWHM
C   CON2=4.*ALOG(2.)/(0.6667*FWHM)**2
      CON1=1.40908546/FWHM
 770  CON2=4.42677*CON1/FWHM
      K=1.25*FWHM+0.5
      NBCKGD=K+K
      IF(NBCKGD.GT.10)NBCKGD=10
      MINST=NBCKGD/4
      IF(MINST.LT.2)MINST=2

C  MINST=SMALLEST NUMBER OF BACKGROUND CHANNELS ON ONE SIDE OF PEAK
      KK=2*K+1
      DO 1 I=1,KK
      F(I)=CON1*EXP(-CON2*(I-K-1)**2)
    1 F1(I)=(1./KK-F(I))**2
      DO 2 I=1,NOCH
    2 SPEC(I)=ZRO
      N0=0
      CENT=0.
      SUMINT=0.
      PKS(1)=0.
      KKK1=KK+1
      KKK2=NOCH-KK
      DO 10 I=KKK1,KKK2
      SUM=0.
      VAR=0.
      IK1=I-K
      IK2=I+K
      SUM2=0.

C  SUM2=CORRELATION FUNCTION
      DO 12 J=IK1,IK2
      TMP=NSPEC(J)
      SUM=SUM+TMP
      JIK1=J-I+K+1
      VAR=VAR+F1(JIK1)*TMP
   12 SUM2=SUM2+F(JIK1)*TMP
      B=0
      IF(VAR.GT.0.01)B=1.6*SQRT(VAR)
      SUM2=SUM2-SUM/KK-B

C  IS CORRELATION SIGNIFICANT
      IF(SUM2.LE.0.)GO TO 725

C  YES,WAS LAST A 0 OR -1
      IF(SPEC(I-1).EQ.ONE)GO TO 720

C  LAST WAS A 0 OR -1,INITIATE STORE OF THIS PEAK
      PEAK=SUM2+B
      IINIT=I
      INX=1

C  INX=ARRAY INDEX FOR PEAK, IINIT  =FIRST CHANNEL, PEAK TO BE MAX CH.
 720  SUM2=SUM2+B
      INX=INX+1
      PKS(INX)=SUM2
      CENT=CENT+(I-1)*SUM2
      SUMINT=SUMINT+SUM2
      SPEC(I)=ONE
      IF(SUM2.GT.PEAK)PEAK=SUM2
      GO TO 10

C INSIGNIFICANT CORRELATION. WAS LAST A 1
  725 IF(SPEC(I-1).NE.ONE)GO TO 10

C YES, PEAK IS JUST PASSED
      INX=INX+1
      PKS(INX)=0.
      IFIN=I-1
      N0=N0+1
      PEAKS(N0,1)=CENT/SUMINT
      CENT=0.
      SUMINT=0.

C  FIND WIDTH OF CORREL.N PEAK
      PEAK=PEAK*0.5
      KL=1
      DO 46 J=2,INX
      GO TO(47,48),KL
   47 IF(PKS(J).LT.PEAK)GO TO 46
      CH11=(J-1)+(PEAK-PKS(J-1))/(PKS(J)-PKS(J-1))
      KL=2
      GO TO 46
   48 IF(PKS(J).GT.PEAK)GO TO 46
      CH22=(J-1)+(PEAK-PKS(J-1))/(PKS(J)-PKS(J-1))
      GO TO 50
   46 CONTINUE

C  STORE THE WIDTH
   50 PEAKS(N0,2)=CH22-CH11

C  IGNORE ONE-CHANNEL "SPIKES"
      INTPK=PEAKS(N0,2)*100.
      IF(INTPK.NE.100)GO TO 49
      N0=N0-1
      GO TO 10
  49  IF(N0.LT.400)GO TO 11
      IF(Iout.NE.0)WRITE(Iout,650)
  650 FORMAT(18H PEAK ARRAY FILLED)
      GO TO 13
   11 MEXP=(KK+IINIT-IFIN-1)/2.+0.5
      IF(MEXP.LE.0)GO TO 10

C  THE INPUT WIDTH IS LARGER THAN THE calibrCULATED,
C  MAKE FORBIDDEN REGION FOR BACKGRD.
      JJJ1=IINIT-MEXP
      JJJ2=IINIT-1
      DO 21 J=JJJ1,JJJ2
      IF(SPEC(J).NE.ONE)SPEC(J)=MEN
   21 CONTINUE
      JJJ1=IFIN+1
      JJJ2=IFIN+MEXP
      DO 22 J=JJJ1,JJJ2
      IF(SPEC(J).NE.ONE)SPEC(J)=MEN
   22 CONTINUE
   10 CONTINUE
   13 IF(Iout.NE.0)WRITE(Iout,110)RUN,(CE(I),I=1,3)
      KSIGF=0
      NOBAC=0
  110 FORMAT(/,' RUN: ',A2,
     1' Calibration. COEFF.: ',3E12.5)
      IF(Iout.NE.0)WRITE(Iout,108)FWHM,DISC,N0
  108 FORMAT(' FWHM: ',F6.2,' CH.',
     1' PEAKS LISTED IF AREA ERROR IS <',F4.0,' %',/,
     2' TOTAL NUMBER OF PEAKS FOUND:',I4,' ,PEAKS ACCEPTED:',
     3/,' N0.',4X,'CHANNEL',7X,'ENERGY',11X,'AREA',7X,
     4'FWHM CHISQ LOWM HIGM LBGD HBGD ')
      IMX=N0
      PEAKS(N0+1,1)=100000.
      MARKR=1
      JMARK(1)=100000
      N0=0
      DO 40 II=1,IMX
      CENT=PEAKS(II,1)
      WIDTH=PEAKS(II,2)
      IPK=CENT+0.5
      IB=IPK+1
  749 IF(SPEC(IB).EQ.ONE) GO TO 750
      IINIT=IB
      GO TO 751
  750 IB=IB-1
      GO TO 749
  751 IB=IPK
  752 IB=IB+1
      IF(SPEC(IB).EQ.ONE) GO TO 752
      IFIN=IB-1

C  FIND AREA OF THE REAL PEAK
      BCKGD=0.
      MEXP=(KK+IINIT-IFIN-1)/2.+0.5
      IF(MEXP.LT.0)MEXP=0
      JJJ3=IINIT-MEXP
      JJJ4=IFIN+MEXP
      IF(NOBAC.EQ.1)GO TO 73
      JJJ=IINIT-MEXP
      LCASE=1
      LBCGD=0
      DO 51 J=1,NBCKGD
   52 JJ=JJJ-J
      IF(JJ.LE.0)GO TO 40
      IF(SPEC(JJ).NE.ZRO)GO TO 60
      XCHA(J)=JJ
      YCHA(J)=NSPEC(JJ)
      WG(J)=0.5
      IF(YCHA(J).GE.10.)WG(J)=1./YCHA(J)
      LBCGD=LBCGD+1
      GO TO 51
   60 IF(LBCGD.GE.MINST)GO TO 61
      LCASE=2
      JJJ=JJJ-1
      GO TO 52
   51 CONTINUE
   61 JJJ=IFIN+MEXP
      JLOW=JJ-1
      JSTR=LBCGD
      MCASE=1
      MBCGD=0
      DO 54 J=1,NBCKGD
   55 JJ=JJJ+J
      IF(JJ.GE.NOCH)GO TO 40
      IF(SPEC(JJ).NE.ZRO)GO TO 70
      JARR=J+JSTR
      XCHA(JARR)=JJ
      YCHA(JARR)=NSPEC(JJ)
      WG(JARR)=0.5
      IF(YCHA(JARR).GE.10.)WG(JARR)=1./YCHA(JARR)
      MBCGD=MBCGD+1
      GO TO 54
   70 IF(MBCGD.GT.MINST)GO TO 71
      MCASE=2
      JJJ=JJJ+1
      GO TO 55
   54 CONTINUE
   71 NPTS=LBCGD+MBCGD
      JHIG=JJ-1
      DO 72 J=1,NPTS
   72 XCHA(J)=XCHA(J)-JJ
      CALL FITS(XCHA,YCHA,WG,C,EF,KMAX,NPTS,CHI)
   73 ARVAR=0.
      PKVAR=0.
      SUM=0.

C  CHECK DOUBLETS AND THEIR INTEGRATION LIMITS
      NOBAC=0
      IF(II.EQ.1)GO TO 74
      ICDIF=PEAKS(II-1,1)+0.5

C  IS LOW LIMIT FOR PLOT BELOW LAST PEAK
      IF(JLOW.GT.ICDIF)GO TO 74
      ICDIF=(IPK-ICDIF)/2.-0.5
      IDIF2=IPK-JJJ3+1

C  CHECK DISTANCE BETWEEN ADJACENT PEAKS
      IF(ICDIF.GE.IDIF2)GO TO 74
      JJJ3=IPK+1-ICDIF
   74 CONTINUE
      ICDIF=PEAKS(II+1,1)+0.5
      IF(JHIG.LE.ICDIF)GO TO 75
      ICDIF=(ICDIF-IPK)/2.-0.5
      IDIF2=JJJ4-1-IPK
      IF(ICDIF.GE.IDIF2)GO TO 76
      JJJ4=IPK+1+ICDIF
   76 NOBAC=1
   75 CONTINUE
      DO 57 J=JJJ3,JJJ4
      SPC=NSPEC(J)
      BCKGD=0.
      VARBCK=0.
      FAC1=1.
      DO 59 N=1,KMAX
      FAC=FAC1
      DO 58 K1=1,KMAX
      VARBCK=VARBCK+EF(N,K1)*FAC
   58 FAC=FAC*(J-JJ)
      BCKGD=BCKGD+C(N)*FAC1
   59 FAC1=FAC1*(J-JJ)
      VARBCK=VARBCK*CHI
      ARVAR=ARVAR+SPC+VARBCK
      PKVAR=PKVAR+(SPC+VARBCK)*(J-CENT)**2
   57 SUM=SUM+SPC-BCKGD
      PEAR=0
      IF(ARVAR.GT.0.01)PEAR=SQRT(ARVAR)
      IF(SUM.EQ.0.)SUM=1.
      IF(ABS(PEAR/SUM).LE.DISC/100.AND.SUM.GT.0.)GO TO 65
      IF(KSIGF.EQ.1)GO TO 398
      GO TO 40
   65 N0=N0+1
      PEPK=0
      IF(PKVAR.GT.0.01)PEPK=SQRT(PKVAR)/SUM
      EN=C0+CENT*(C1+CENT*(C2+CENT*(C3+CENT*C4)))
      PEEN=(C1+2.*CENT*C2)*PEPK
      JJ3=JJJ3-1
      JJ4=JJJ4-1
      JMARK(MARKR)=JJJ3
      JMARK(MARKR+1)=JJJ4
      MARKR=MARKR+2
      JMARK(MARKR)=100000
      KSIGF=1
      PEAKS(N0,1)=CENT
      PEAKS(N0,2)=PEPK
      PEAKS(N0,3)=SUM
      PEAKS(N0,4)=PEAR
      DA=ABS(PEAR/SUM)
      DSC=DISC/100.
      INT1=SUM+0.5
      INT2=PEAR+0.5
      IF(DA.LT.DSC.AND.SUM.GT.0.AND.WIDTH.GT..1*FWHM)THEN
        IPEAK(NUMB)=IPEAK(NUMB)+1
        TOPS(IPEAK(NUMB),NUMB,1)=CENT
        TOPS(IPEAK(NUMB),NUMB,2)=SUM
        TOPS(IPEAK(NUMB),NUMB,3)=WIDTH
      ENDIF
      IF(Iout.NE.0)THEN
        WRITE(Iout,109)
     1  N0,CENT,PEPK,EN,PEEN,INT1,INT2,WIDTH
     1  ,CHI,JJ3,JJ4,JLOW,JHIG
  109   FORMAT(1X,I3,1X,F7.2,'(',F5.2,')',F8.1,'(',F4.1,')',I7,
     1  '(',I6,')',1X,F6.2,F5.1,4I5)
      ENDIF

  398 IF(IPLOT.NE.1)GO TO 416
      IF(NOBAC.EQ.1)GO TO 417

C  PLOT THIS PEAK AND THE BACKGROUND AREA
  406 ITOT=0
      KSIGF=0
      JFIRST=LBCGD/2
      DO 399 I=1,JFIRST
      FAC=XCHA(I)
      JLAST=LBCGD+1-I
      XCHA(I)=XCHA(JLAST)
  399 XCHA(JLAST)=FAC
      JFIRST=JLOW+1
      JLAST=JHIG+1
      MAXCHA=NSPEC(JFIRST)
      MINS=MAXCHA
      DO 405 J=JFIRST,JLAST
      IF(MAXCHA.LT.NSPEC(J))MAXCHA=NSPEC(J)
      IF(MINS.GT.NSPEC(J))MINS=NSPEC(J)
  405 CONTINUE
      NOBAC=0

C GET HIGH TICK.GE.MAXCHA,I.E.=100,200,500,1000 ETC.
      MSCALE=100
      ITEST=200
  400 IF(MAXCHA.LT.MSCALE)GO TO 402
      FACT=2.
      IF(MSCALE.NE.ITEST)GO TO 401
      ITEST=ITEST*10
      FACT=2.5
  401 MSCALE=MSCALE*FACT+0.5
      GO TO 400
  402 IF(MINS.LT.200)MINS=0
      LSCALE=(MINS/200)*200
      MISCA=LSCALE+(MSCALE-LSCALE)/2
      IF(Iout.NE.0)WRITE(Iout,450)LSCALE,MISCA,MSCALE
  450 FORMAT(30X,I6,45X,I6,43X,I6/36H CHANN  ENERGY COUNT BCKG INTGR....
     2,I,
     149(1H.),1HI,49(1H.),1HI)
      IXIND=1
      MARKR=1
      DO 415 J=JFIRST,JLAST
      JPRT=J-1
      ICNT=NSPEC(J)
      EN=C0+JPRT*(C1+JPRT*(C2+JPRT*(C3+JPRT*C4)))+0.005
      FAC=1.
      BCKGD=0.
      DO 407 N=1,KMAX
      BCKGD=BCKGD+C(N)*FAC
  407 FAC=FAC*(J-JJ)
      IBCKGD=BCKGD+0.5
      ITOT=ITOT+ICNT-IBCKGD
      FAC1=100./(MSCALE-LSCALE)
      FAC=(ICNT-LSCALE)*FAC1+5.5
      JSTR=FAC
      IF(JSTR.LT.1)JSTR=1
      IF(JSTR.GT.105)JSTR=105

C ZERO THE OUTPUT ARRAY
      DO 408 I=1,105
  408 SOUTP(I)=BLNK
      SOUTP(JSTR)=STAR
      ISTR=(IBCKGD-LSCALE)*FAC1+5.5
      IF(ISTR.LT.1)ISTR=1
      IF(ISTR.GT.105)ISTR=105
      SOUTP(ISTR)=IBCH
      IJ=XCHA(IXIND)+JJ+0.5
      IF(J.NE.IJ)GO TO 409
      IXIND=IXIND+1
      SOUTP(ISTR)=BBC
  409 IF(J.EQ.JMARK(MARKR))GO TO 410
      GO TO 412
  410 IADD=1
      IF(JSTR.GE.90)IADD=-1
      ISTR=JSTR
      MARKR=MARKR+1
      DO 411 I=1,5
      ISTR=ISTR+IADD
  411 SOUTP(ISTR)=EXX
  412 IF(Iout.NE.0)
     1WRITE(Iout,451)JPRT,EN,ICNT,IBCKGD,ITOT,(SOUTP(I),I=1,
     2105)
  451 FORMAT(1H ,I5,F8.2,I6,I5,I6,105A1)
  415 CONTINUE
      JMARK(1)=100000
      IF(Iout.NE.0)WRITE(Iout,108)
  416 MARKR=1
  417 CONTINUE
   40 CONTINUE
      NOPEAK=N0
      RETURN
      END


      SUBROUTINE FITS(X,Y,W,C,E,KMAX,NPTS,CHI)
C GENERAL SUBROUTINE FOR POLYNOMIAL FIT
C ENTER WITH X AND Y VALUES, WEIGHT, NO. OF WANTED PARAMETERS
C IN KMAX, AND NO. OF POINTS (.LE.50) IN NPTS.  RETURNS WITH COVAR
C MATRIX IN EF,PARAMETERS IN C AND NORMALLIZED CHISQUARED IN CHI
      DIMENSION X(50),Y(50),W(50),C(5),EF(5,5),E(5,5),RS(5)

      SUM1=0.
      DO 2 K=1,KMAX
      DO 1 K1=1,KMAX
    1 E(K,K1)=0.
    2 RS(K)=0.
      DO 5 J=1,NPTS
      FAC1=W(J)
      DO 4 K=1,KMAX
      FAC=FAC1
      DO 3 K1=1,KMAX
      E(K,K1)=E(K,K1)+FAC
    3 FAC=FAC*X(J)
      RS(K)=RS(K)+FAC1*Y(J)
    4 FAC1=FAC1*X(J)
    5 SUM1=SUM1+W(J)*Y(J)*Y(J)
      DO 6 K=1,KMAX
      DO 6 K1=1,K
      EF(K,K1)=E(K,K1)
    6 EF(K1,K)=EF(K,K1)
      CALL MATINV(E,KMAX)
      SUM2=0.
      DO 8 K=1,KMAX
      SUM=0.
      DO 7 K1=1,KMAX
    7 SUM=SUM+E(K,K1)*RS(K1)
      C(K)=SUM
    8 SUM2=SUM*RS(K)+SUM2
      SUM3=0.
      DO 9 K=1,KMAX
      DO 9 K1=1,KMAX
    9 SUM3=SUM3+C(K)*C(K1)*EF(K,K1)
      NDIV=NPTS-KMAX
      IF(NDIV.LE.0)NDIV=1
      CHI=(SUM1-2.*SUM2+SUM3)/NDIV
      RETURN
      END


      SUBROUTINE MATINV(A,N)
C MATRIX INVERSION BY GAUSS-JORDAN ELIMINATION
      DIMENSION A(5,5),B(5),C(5),LZ(5)

      DO 10 J=1,N
   10 LZ(J)=J
      DO 20 I=1,N
      K=I
      Y=A(I,I)
      L=I-1
      LP=I+1
      IF(N-LP)14,11,11
   11 DO 13 J=LP,N
      W=A(I,J)
      IF(ABS(W)-ABS(Y))13,13,12
   12 K=J
      Y=W
   13 CONTINUE
   14 DO 15 J=1,N
      C(J)=A(J,K)
      A(J,K)=A(J,I)
      A(J,I)=-C(J)/Y
      A(I,J)=A(I,J)/Y
   15 B(J)=A(I,J)
      A(I,I)=1./Y
      J=LZ(I)
      LZ(I)=LZ(K)
      LZ(K)=J
      DO 19 K=1,N
      IF(I-K)16,19,16
   16 DO 18 J=1,N
      IF(I-J)17,18,17
   17 A(K,J)=A(K,J)-B(J)*C(K)
   18 CONTINUE
   19 CONTINUE
   20 CONTINUE
      DO 200 I=1,N
      IF(I-LZ(I))100,200,100
  100 K=I+1
      DO 500 J=K,N
      IF(I-LZ(J))500,600,500
  600 M=LZ(I)
      LZ(I)=LZ(J)
      LZ(J)=M
      DO 700 L=1,N
      C(L)=A(I,L)
      A(I,L)=A(J,L)
  700 A(J,L)=C(L)
  500 CONTINUE
  200 CONTINUE
      RETURN
      END


      SUBROUTINE Calibration(A0,A1,FWR,AR,NOCH,IA,IB,chK,chTh)
C ROUTINE THAT PICKS OUT THE LINES 1460.8 AND 2614.6 AND CalibrationATES
C WE FIND THE FIRST MAXIMUM AND THEN THE STRONGEST PEAK ABOVE. THIS
C IS INTERPRETED AS 1460.8 KEV. THEN WE ASSUME E=0 AROUND CH=0 AND
C SEARCH FOR THE 2614.6 KEV LINE.
      COMMON/calibrIB/NUMB,TOPS(400,32,3),IPEAK(32),rMAT(2048,32),FWHMCH

      IA=0
      IB=0
      chK =0.
      chTh=0.
      FW=FWHMCH
      ILOW=FW
      IMAX=IPEAK(NUMB)
      IF(IMAX.EQ.0)THEN
        A0=9999
        A1=9999
        FWR=0.
        AR=0.
        GO TO 10
      ENDIF

C FINDING THE PLACE WHERE THE SPECTRUM FIRST TIME DECREASES (>50 CH !)
      DO J=10,NOCH/FW-3
        I1=(J+0)*FW
        I2=(J+1)*FW
        I3=(J+2)*FW
        IF(I1.LT.1)I1=1
        INTL=0
        INTH=0
        DO I=I1,I2
          INTL=INTL+rMAT(I,NUMB)
        ENDDO
        DO I=I2,I3
          INTH=INTH+rMAT(I,NUMB)
        ENDDO
        IF(INTH.LT.INTL)THEN
          ILOW=I3
          GO TO 10
        ENDIF
      ENDDO
  10  CONTINUE
      IF(ILOW.LT.50)ILOW=50

C FINDING THE STRONGEST PEAK ABOVE ILOW
      AREA=1.
      IA=1
      DO I=1,IMAX
        IF(TOPS(I,NUMB,2).GT.AREA.AND.TOPS(I,NUMB,1).
     1  GT.ILOW.AND.TOPS(I,NUMB,1).LT.NOCH*0.90)THEN
          AREA=TOPS(I,NUMB,2)
          IA=I
        ENDIF
      ENDDO
      IA1=IA

C FINDING THE STRONGEST PEAK ABOVE IA
      AREA=1.
      IAA=IA
      DO I=IA+1,IMAX
        IF(TOPS(I,NUMB,2).GT.AREA.AND.TOPS(I,NUMB,1).
     1  LT.NOCH*0.90)THEN        !Upper part of spectrum nonsence           
          AREA=TOPS(I,NUMB,2)
          IAA=I
        ENDIF
      ENDDO


C TESTING IF THE IAA PEAK IS STRONG ENOUGH TO BE THE 1460 LINE
      IF(TOPS(IAA,NUMB,2).GT.0.25*TOPS(IA,NUMB,2))IA=IAA

C MAKING ROUGH Calibration. AND FINDING THE 2614.6 KEV LINE, WHICH IS
C DEFINED AS THE STRONGEST LINE BETWEEN 1.9 AND 3.1 MEV

      AREA=1.
      IB=IA
      A0=0
      A1=1460.8/TOPS(IA,NUMB,1)
      CH1=1900./A1
      CH2=3100./A1
      DO I=IA+1,IMAX
        IF(TOPS(I,NUMB,1).GT.CH1.AND.TOPS(I,NUMB,1).LT.CH2)THEN
          IF(TOPS(I,NUMB,2).GT.0.7*AREA)THEN
            AREA=TOPS(I,NUMB,2)
            IB=I
          ENDIF
        ENDIF
      ENDDO

C THE TWO PEAKS ARE IDENTIFIED AS NUMBER IA AND IB
C It happens that the 228Th line is taken as two peaks, adding them up
      IF(IA.GT.0)chK =TOPS(IA,NUMB,1)
      IF(IB.GT.IA.AND.IA.GT.0)THEN
        chTh=TOPS(IB,NUMB,1)
        area=TOPS(IB,NUMB,2)
        fwhm=TOPS(IB,NUMB,3)
        Itest=IB-1                        !going to the left
        IF(Itest.GT.IA)THEN
          chT  =TOPS(Itest,NUMB,1)
          areaT=TOPS(Itest,NUMB,2)
          IF(chT+3.*fwhm.GT.chTh)
     1    chTh=(area*chTh+areaT*chT)/(area+areaT)
        ENDIF
        Itest=IB+1                        !going to the right
        IF(Itest.GT.IA.AND.Itest.GT.2)THEN
          chT  =TOPS(Itest,NUMB,1)
          areaT=TOPS(Itest,NUMB,2)
          IF(chT-3.*fwhm.LT.chTh)
     1    chTh=(area*chTh+areaT*chT)/(area+areaT)
        ENDIF
      ENDIF

      IF(IB.GT.IA.AND.IA.GT.0)THEN
        A1=(2614.6-1460.8)/(chTh-chK)
        A0=1460.8-A1*chK
        AR=TOPS(IA,NUMB,2)/1000.
        FWR=SQRT(1461./1332.)*(TOPS(IA,NUMB,3)*A1/1461.)*100.
      ENDIF
      IF(IB.LE.IA.AND.IA.GT.0)THEN
        A1=1460.8/chK
        A0=0.
        AR=TOPS(IA,NUMB,2)/1000.
        FWR=SQRT(1461./1332.)*(TOPS(IA,NUMB,3)*A1/1461.)*100.
      ENDIF

      END


      SUBROUTINE NEW(HVG,HVN,ALPHA)
C ROUTINE THAT calibrCULATES THE NEW HV BASED ON THE OLD VALUE(HVG)
C AND THE WANTED AMPLIFICATION ALPHA. THE AMPLIFICATION IS
C ASSUMED TO FOLLOW: A=C0+C1*V+C2*V*V

      C0=18632.9
      C1=-47.7503
      C2=0.0309362

      A=-C2*HVG*HVG
      B=-C1*HVG
      C=(ALPHA-1.)*C0+ALPHA*C1*HVG+ALPHA*C2*HVG*HVG

      BETA=0
      E=B*B-4.*A*C
      IF(E.GT.0.AND.A.NE.0.)BETA=(-B-SQRT(E))/(2.*A)
     
      HVN=BETA*HVG
      END

