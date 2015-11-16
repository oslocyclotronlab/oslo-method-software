      PROGRAM fitone
      EXTERNAL FUNC,fe11,fe12,fm11,fpy11,upbend,fe2
c      EXTERNAL PloLint
      DIMENSION YFIT(1000)
      DIMENSION X(1000),Y(1000),SIGMAY(1000),A(12),DELTAA(12),SIGMAA(12),CH(1010), Abest(12),SIGMAAbest(12)
      DIMENSION Yold(1000),sYold(1000)
      DIMENSION xx(1000),yy(1000),ss(1000)
      COMMON /resonance/ ee1,se1,ge1,ee2,se2,ge2,em1,sm1,gm1,eeq,seq,geq
      COMMON /egidy/ aLev,E1,Exmin,Exmax
      CHARACTER infile*20
      REAL M17M,E17M
 
C Give your fit region in file strength.paw and other parameters
      i1=1                   ! First point for fit
      i2=59                  ! Last point for fit
      NPTS=i2-i1+1           ! Number of fit-points
      NA=4                   ! Number of fit parameters
      Exmin = 4.15           ! Lower Ex for 1.gen. spectra
      Exmax = 8.2            ! Higher Ex for 1. gen. spectra
      Delta = 2.139          ! Delta_p+n for 96Mo
      infile='rsf34'

C Estimate values of parameters to be fitted
      A(1)=0.4              ! K-factor in front of E1+M1

C Calculates resonance parameters from systematics of RIPL
      xN=54
      xZ=42
      xA=96
      beta = 0.0
      pi=3.14159
      e0=31.2*xA**(-0.333)+20.6*xA**(-0.167)
      g0=0.026*e0**1.91
      ee1=e0/(1.+0.666*beta)
      ee2=e0/(1.-0.333*beta)
      ge1=0.026*ee1**1.91
      ge2=0.026*ee2**1.91
      se1=0.5*1.2*120.*xN*xZ/(xA*pi*g0)
      se2=0.5*1.2*120.*xN*xZ/(xA*pi*g0)

      ee1=16.20 
      se1=185   
      ge1=6.01   
c      ee2=15.90
c      se2=240
c      ge2=5.18


c  sm1=1.0E-09*1.58*xA**(0.47)*((7.0**2-em1**2)**2+7.0**2*gm1**2)/(8.6737E-08*7.0*gm1**2)

C Values taken from RIPL page 102-103 (and erratum page 1)
      em1=41.*(xA**(-1./3.))
      gm1=4.
c sm1=1.0E-09*1.58*xA**(0.47)*((7.0**2-em1**2)**2+7.0**2*gm1**2)/(8.6737E-08*7.0*gm1**2)

      E17M=(49.-ee1*ee1)**2.*ee1
      E17M=0.7*se1*ge1*ge1*49./E17M
      M17M=1./(((49.-em1**2.)/(7.*gm1))**2.+1.)
      M17M=M17M/7.
C Two estimates for sigma(M1), the first one is absolute, the second relative
C 0.018216=(3*(pi*hbar*c)**2) * (1.58E-9 MeV**-3)
      sm1=.018216*(xA**.47)/M17M
      sm1=E17M/(M17M*.0588*xA**.878)

C E2 resonance
C Now, isoscalar E2 model. Values taken from RIPL page 103 with corrections:
C 0.00014->0.00015=2*pi*alpha*r**2/(5*mc**2), alpha=fine structure constant,
C r=1.233 fm, m=nucleon mass (=1u). Value given in mb/MeV. Further, it should 
C be Eo**2 in the formula for sigma. Finally, it is -0.012 in the slope for 
C Gamma
       eeq = 63.*(xA**(-1./3.))
       geq = 6.11-(0.012*xA)
       seq = 0.00015*(eeq*xZ)**2./(geq*(xA**(1./3.)))

C Upbend, this is a very rough estimate, no systematics available
       WRITE(6,*)'Upbend modeled as a power law of the form'
       WRITE(6,*)'K*1/(3*(pi*c*hbar)**2)*A*(E_gamma/[MeV])**-B,'
       WRITE(6,*)'where the two parameters A and B>0,'
       WRITE(6,*)'1/(3*(pi*c*hbar)**2)=8.673723898E-8/(mb*MeV**2),'
       WRITE(6,*)'and the overall normalization K will be explained later.'
c       A(2)=0.5
c       A(3)=2.5
c trying Lorentzian
       A(2)=1.0
       A(3)=0.1
       A(4)=2.0

C von Egidy values
      aLev  = 0.21*(xA**0.87)
      C1    = -6.6*(xA**(-0.32)) 
      E1    = Delta + C1

C Calculating average T and sT for plotting (not used further in program)
      OPEN(20,FILE='tave.paw')
      DO i = 0,200
         sigT = 0.0
         Eg   = 0.1*FLOAT(i)        ! 0.1 MeV steps up to 20 MeV
         CALL T_of_Eg(T_Eg,T2_Eg,Eg)
         xxx  = T2_Eg-T_Eg*T_Eg
         IF(xxx.GT.0.001.AND.EG.LE.Exmax) sigT = SQRT(xxx)
         WRITE(20,*) Eg, T_Eg, sigT
      ENDDO
      CLOSE(20)
      WRITE(6,*)'File tave.paw written to disc'
     
      OPEN(23,FILE=infile,STATUS='old',ERR=666)
      GO TO 668
666   WRITE(6,*) 'Sorry, could not open', infile
      STOP
 668  DO i=1,1000
         READ(23,*,END=667)dum,xx(i),yy(i),ss(i)
      ENDDO
 667  Ntot=i-1
      CLOSE(23)

C Putting values into the fit-vectors
      DO i=1,NPTS
        X(i)      = xx(i+i1-1)
        Y(i)      = yy(i+i1-1)
        SIGMAY(i) = ss(i+i1-1)
      ENDDO

      write(6,*)'A,Exmin,Exmax = ',INT(xA),Exmin,Exmax
      write(6,*)'a,Delta,C1,E1 = ',aLev,Delta,C1,E1
      write(6,*)'Default values taken from file:',infile
      write(6,*)'Total number of data points in',infile,' = ',Ntot
      write(6,*)'Fit region in strength.paw, NPTS = ',i1,'-',i2,NPTS

      DO i=1,1000
         Yold(i)  =Y(i)
         sYold(i)=SIGMAY(i)
      ENDDO

C Making LOG which is easier to fit data, and then back again with EXP
      DO i=1,NPTS
        SIGMAY(i)=SIGMAY(i)/Y(i)
        Y(i)=LOG(Y(i))
           write(6,*)X(i),Y(i),SIGMAY(i)
      ENDDO

      DO I=1,12
         DELTAA(I)=ABS(A(I)/10.)
      ENDDO
 
C PREPARATION FOR FIT
      PROG=0.000002
      MODE=1
      M=0
      CHISQR=999999.
      BEST  =999999.
      NBAD  =0

C STARTING FIT  ***********************************************
      IF(NA.EQ.1)WRITE(6,78)
      IF(NA.EQ.2)WRITE(6,79)
      IF(NA.GT.2)WRITE(6,80)

  78  FORMAT(' Loop    Chi**2        A(1)')
  79  FORMAT(' Loop    Chi**2        A(1)         A(2)')
  80  FORMAT(' Loop    Chi**2        A(1)         A(2)     ...')

      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
 505  CALL GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A,DELTAA,SIGMAA,NA,YFIT,CHISQR)
      M=M+1
      WRITE(6,81)M,CHISQR,(A(J),J=1,NA)
  81  FORMAT(1X,I3,E12.4,8(1X,E12.5))
      CH(M)=CHISQR
      IF(M.LT.2) GO TO 505
      VER=ABS(CH(M)-CH(M-1))/CH(M-1)
      IF(CHISQR.LT.BEST)THEN
        Mbest=M
        BEST=CHISQR
        DO i=1,12
           Abest(i)=A(i)
           SIGMAAbest(i)=SIGMAA(i)
        ENDDO
      ENDIF
      IF(CH(M).GT.CH(M-1))NBAD=NBAD+1
      IF(NBAD.GT.6.OR.M.GE.500)GO TO 506
      IF(M.GT.1000)GO TO 506
      IF(VER.GT.PROG) GO TO 505
 506  CONTINUE   

C FIT IS FINISHED, WRITE OUT
      DO i=1,12
         A(i)=Abest(i)
         SIGMAA(i)=SIGMAAbest(i)
      ENDDO

      WRITE(6,*)' '
      WRITE(6,*)'          X(i)            Y(i)          Yfit(i)'
      DO I=1,NPTS
        WRITE(6,44)X(I),Y(I),FUNC(X(I),A)
44      FORMAT(3F16.4)
      ENDDO

      WRITE(6,82)Mbest,BEST
  82  FORMAT(/' Final result from loop',I4,' with Chi**2 = ',E10.4)
c      WRITE(6,83)  A(1),SIGMAA(1)
c      WRITE(6,84)  A(2),SIGMAA(2)
c      WRITE(6,85)  A(3),SIGMAA(3)
      WRITE(6,86)  A(1),SIGMAA(1)
      WRITE(6,87)  A(2),SIGMAA(2)
      WRITE(6,88)  A(3),SIGMAA(3)
	  WRITE(6,89)  A(4),SIGMAA(4)
c      BM1=35.189*A(2)*A(3)/A(1)
c  sBM1=BM1*SQRT((SIGMAA(2)/A(2))**2+(SIGMAA(3)/A(3))**2+(SIGMAA(1)/A(1))**2)
c      WRITE(6,91) BM1,sBM1
c      WRITE(6,87)  A(5),SIGMAA(5)
c      WRITE(6,88)  A(6),SIGMAA(6)
c      WRITE(6,89)  A(7),SIGMAA(7)
c      WRITE(6,90)  A(8),SIGMAA(8)

c  83  FORMAT(' A(1) e1 = ',E12.5,' +/- ',E12.5)
c  84  FORMAT(' A(2) s1 = ',E12.5,' +/- ',E12.5)
c  85  FORMAT(' A(3) g1 = ',E12.5,' +/- ',E12.5)
c  86  FORMAT(' A(4) e2 = ',E12.5,' +/- ',E12.5)
c  87  FORMAT(' A(5) s2 = ',E12.5,' +/- ',E12.5)
c  88  FORMAT(' A(6) g2 = ',E12.5,' +/- ',E12.5)
   86  FORMAT(' A(1) K  = ',E12.5,' +/- ',E12.5)
   87  FORMAT(' A(2) e  = ',E12.5,' +/- ',E12.5)
   88  FORMAT(' A(3) s  = ',E12.5,' +/- ',E12.5)
   89  FORMAT(' A(4) g  = ',E12.5,' +/- ',E12.5)
c  90  FORMAT(' A(8) T  = ',E12.5,' +/- ',E12.5)
c  91  FORMAT(' BM1     = ',E12.5,' +/- ',E12.5)


C Writing to paw file for plotting
      OPEN(24,FILE='fitted.paw',ACCESS='SEQUENTIAL',ERR=555)
      DO i=1,400
         e=i*0.1            ! up to 40 MeV
         WRITE(24,100) e, EXP(FUNC(e,A)),fe11(e,A),upbend(e,A),fm11(e,A),fe2(e,A),dummy
 100	 FORMAT(F7.2, 6E12.5)
      ENDDO
      WRITE(6,*)'File fitted.paw written to disc'
      CLOSE(24)

c      OPEN(24,FILE='linglobal.paw',ACCESS='SEQUENTIAL',ERR=555)
c      DO i=1,100
c         e=X(i)   
c         WRITE(24,*) e,fpy11(e,A),Yold(i)-(PlotLin(e,A)-fpy11(e,A)),sYold(i)
c      ENDDO
c      WRITE(6,*)'File linglobal.paw written to disc'
c      CLOSE(24)

      OPEN(24,FILE='results.dat',ACCESS='SEQUENTIAL',ERR=555)
      WRITE(24,*)'Resonance parameters (RIPL) for A=',INT(xA),'  Z=',INT(xZ),'  with beta=',beta
      WRITE(24,*)'GEDR #1 parameters: E=',ee1,'MeV  S=',se1,'mb  G=',ge1,'MeV'
c      WRITE(24,*)'GEDR #2 parameters: E=',ee2,'MeV  S=',se2,'mb  G=',ge2,'MeV'
      WRITE(24,*)'GMDR #1 parameters: E=',em1,'MeV  S=',sm1,'mb  G=',gm1,'MeV'
      WRITE(24,*)'GEQR #1 parameters: E=',eeq,'MeV  S=',seq,'mb  G=',geq,'MeV'

      WRITE(24,*)' '
      WRITE(24,*)'A,Exmin,Exmax = ',INT(xA),Exmin,Exmax
      WRITE(24,*)'a,Delta,C1,E1 = ',aLev,Delta,C1,E1
      WRITE(24,*)'Default values taken from file:',infile
      WRITE(24,*)'Total number of data points in:',infile,' = ',Ntot
      WRITE(24,*)'Fit region and NPTS = ',i1,i2,NPTS
      WRITE(24,*)' '
      WRITE(24,*)'Fitting', NPTS,' data points with', NA,' parameters'
      WRITE(24,82)Mbest,BEST
c      WRITE(24,83)  A(1),SIGMAA(1)
c      WRITE(24,84)  A(2),SIGMAA(2)
c      WRITE(24,85)  A(3),SIGMAA(3)
      WRITE(24,86)  A(1),SIGMAA(1)
      WRITE(24,87)  A(2),SIGMAA(2)
      WRITE(24,88)  A(3),SIGMAA(3)
      WRITE(24,89)  A(4),SIGMAA(4)
c      WRITE(24,91) BM1,sBM1

c      WRITE(24,87)  A(5),SIGMAA(5)
c      WRITE(24,88)  A(6),SIGMAA(6)
c      WRITE(24,89)  A(7),SIGMAA(7)
      CLOSE(24)
      WRITE(6,*)'File results.dat written to disc'

      GO TO 507
 508  WRITE(6,*) 'Error termination'
 555  WRITE(6,*) 'Output files not written'
 507  CONTINUE
      END

      SUBROUTINE GRIDLS(X,Y,SIGMAY,NPTS,MODE,FUNC,A,DELTAA,SIGMAA,NA,YFIT,CHISQR)
      DIMENSION X(1000),Y(1000),SIGMAY(1000),A(12),DELTAA(12),SIGMAA(12),YFIT(1000)
      INTEGER try

      NFREE=NPTS-NA
      IF(NFREE.EQ.0)NFREE=1
      FREE=NFREE
      CHISQR=0.
      IF(NFREE.LT.0)RETURN

      DO J=1,NA
C  EVALUATE CHI SQUARE AT FIRST TWO SEARCH POINTS
        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        CHISQ1=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)

        try=0
        DELTA=DELTAA(J)
   41   A(J)=A(J)+DELTA
        try=try+1
        ntry=try/1000
        IF(1000*ntry.EQ.try)THEN
		  write(6,FMT='(A1,$)')'.'
          CALL FLUSH(6)
        ENDIF
        IF(try.GT.5)THEN
          WRITE(6,*)'Warning, iteration stopped after 5 tries'
          GO TO 81
        ENDIF

        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        CHISQ2=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
        IF(CHISQ1-CHISQ2) 51,41,61

C  REVERSE DIRECTION OF SEARCH IF CHI SQUARE IS INCREASING
   51   DELTA=-DELTA
        A(J)=A(J)+DELTA
        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        SAVE=CHISQ1
        CHISQ1=CHISQ2
        CHISQ2=SAVE

C  INCREMENT A(J) UNTIL CHI SQUARE INCREASES
   61   try=try+1
        ntry=try/1000
        IF(1000*ntry.EQ.try)THEN
		  write(6,FMT='(A1,$)')'.'
          CALL FLUSH(6)
        ENDIF
        IF(try.GT.5)THEN
          WRITE(6,*)'Warning, iteration stopped after 5 tries'
          GO TO 81
        ENDIF
        A(J)=A(J)+DELTA
        DO I=1,NPTS
          XX=X(I)
          YFIT(I)=FUNC(XX,A)
        ENDDO
        CHISQ3=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
        IF(CHISQ3-CHISQ2) 71,81,81
   71   CHISQ1=CHISQ2
        CHISQ2=CHISQ3
        GO TO 61

C  FIND MIN OF PARABOLA DEFINED BY LAST THREE POINTS
   81   DELTA=DELTA*(1./(1.+(CHISQ1-CHISQ2)/(CHISQ3-CHISQ2))+0.5)
        A(J)=A(J)-DELTA
c Must be wrong to use FREE here, taken away, Magne, June 23. 2002
c       SIGMAA(J)=DELTAA(J)*SQRT(2./(FREE*(CHISQ3-2.*CHISQ2+CHISQ1)))
        SIGMAA(J)=DELTAA(J)*SQRT(2./(      (CHISQ3-2.*CHISQ2+CHISQ1)))
      ENDDO

C  EVALUATE FIT AND CHI SQUARE FOR FINAL PARAMETERS
      DO I=1,NPTS
        XX=X(I)
        YFIT(I)=FUNC(XX,A)
      ENDDO
      CHISQR=FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      RETURN
      END


      FUNCTION FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
      DIMENSION Y(1000),SIGMAY(1000),YFIT(1000)
      CHISQ=0.
      IF(NFREE) 13,13,20
   13 FCHISQ=0.
      GO TO 40

C  ACCUMULATE CHI SQUARE
c   20 DO 30 I=1,NPTS
c      IF(MODE)22,27,29
c   22 IF(Y(I))25,27,23
c   23 WEIGHT=1./Y(I)
c      GO TO 30
c   25 WEIGHT=1./(-Y(I))
c      GO TO 30
c   27 WEIGHT=1.
c      GO TO 30
c   29 WEIGHT=1./SIGMAY(I)**2
c   30 CHISQ=CHISQ+WEIGHT*(Y(I)-YFIT(I))**2
C Modified by Magne:
   20  continue
       DO i=1,npts
         w=1.
         yy=ABS(Y(i))
         if(MODE.LT.0.AND.yy.NE.0.)        w=1./yy
         if(MODE.GT.0.AND.SIGMAY(i).NE.0.) w=1./SIGMAY(I)**2
         CHISQ=CHISQ+w*(Y(I)-YFIT(I))**2
       ENDDO
C  DIVIDE BY NUMBER OF DEGREES OF FREEDOM
      FREE=NFREE
      FCHISQ=CHISQ/FREE
   40 RETURN
      END

      FUNCTION FUNC(X,A)
      REAL A(12)
      FUNC=LOG(fe11(x,A) + fm11(x,A) + upbend(x,A) +fe2(x,A))
      RETURN          
      END

      SUBROUTINE T_of_Eg(T_Eg,T2_Eg,eg)
      COMMON /egidy/ aLev,E1,Exmin,Exmax
      dEx   = 0.01                   ! Steps of 10keV
      T_Eg  = 0.0                    ! <T(Eg)>
      T2_Eg = 0.0                    ! <T**2(Eg)>
      sum   = 0.0                    ! Normalizing constant
      IF(eg.GT.Exmax)RETURN
      Elow  = max(0.00000001,Exmin-eg) ! Final Ex low
      Ehigh = max(0.00000001,Exmax-eg) ! Final Ex high
      i1    = Elow /dEx
      i2    = Ehigh/dEx
      DO i  = i1,i2
         Exf  = FLOAT(i)*dEx
         U    = max(0.00000001,Exf-E1)
         T    = SQRT(U/aLev)
c		   t=0.6
         if(T.LT.0)T=0.000000001
         T_Eg  = T_Eg  + T*dEx
         T2_Eg = T2_Eg + T*T*dEx
         sum   = sum   +   dEx
      ENDDO
      T_Eg  = T_Eg /sum  
      T2_Eg = T2_Eg/sum  
      RETURN
      END

      FUNCTION fe11(eg,A)
      REAL A(12)
      COMMON /resonance/ ee1,se1,ge1,ee2,se2,ge2,em1,sm1,gm1,eeq,seq,geq
      COMMON /egidy/ aLev,E1,Exmin,Exmax
c        write(6,*)ee1,se1,ge1,ee2,se2,ge2,em1,sm1,gm1,eeq,seq,geq,aLev,E1,Exmin,Exmax
      xK = A(1)
      pi = 3.14159
      diff1 = MAX(0.1,ABS(eg**2 - ee1**2))
      dEx   = 0.01                   ! Steps of 10keV
      fe11 =0.
      sum  =0.
      IF(eg.GT.Exmax)RETURN
      Elow  = max(0.00000001,Exmin-eg) ! Final Ex low
      Ehigh = max(0.00000001,Exmax-eg) ! Final Ex high
      i1    = Elow /dEx
      i2    = Ehigh/dEx
      DO i  = i1,i2
         Exf  = FLOAT(i)*dEx
         U    = max(0.00000001,Exf-E1)
         T    = SQRT(U/aLev)
c               t=0.6
         if(T.LT.0)T=0.000000001
         fe11  = fe11+(xK*(8.6737E-08)*(
     1   (0.7*se1*ge1**2*(eg**2+4*pi**2*T**2))/(ee1*(diff1)**2) ))*dEx
         sum   = sum   +   dEx
      ENDDO
      fe11=fe11/sum
      RETURN
      END

      FUNCTION fe12(eg,A)
      REAL A(12)
      COMMON /resonance/ ee1,se1,ge1,ee2,se2,ge2,em1,sm1,gm1,eeq,seq,geq
      COMMON /egidy/ aLev,E1,Exmin,Exmax
      xK = A(1)
      pi = 3.14159
      diff2 = MAX(0.1,ABS(eg**2 - ee2**2))
      dEx   = 0.01                   ! Steps of 10keV
      fe12 =0.
      sum  =0.
      IF(eg.GT.Exmax)RETURN
      Elow  = max(0.00000001,Exmin-eg) ! Final Ex low
      Ehigh = max(0.00000001,Exmax-eg) ! Final Ex high
      i1    = Elow /dEx
      i2    = Ehigh/dEx
      DO i  = i1,i2
         Exf  = FLOAT(i)*dEx
         U    = max(0.00000001,Exf-E1)
         T    = SQRT(U/aLev)
c          t=0.6
         if(T.LT.0)T=0.000000001
         fe12  = fe12+(xK*(8.6737E-08)*(
     1   (0.7*se2*ge2**2*(eg**2+4*pi**2*T**2))/(ee2*(diff2)**2) ))*dEx
         sum   = sum   +   dEx
      ENDDO
      fe12=fe12/sum
      RETURN
      END

      FUNCTION fm11(eg,A)
      REAL A(12)
      COMMON /resonance/ ee1,se1,ge1,ee2,se2,ge2,em1,sm1,gm1,eeq,seq,geq
      xK =A(1)
      fm11=xK*(8.6737E-08) * (
     1    (sm1*eg*gm1**2)/((eg**2-em1**2)**2+eg**2*gm1**2))
      RETURN
      END

      FUNCTION fe2(eg,A)
      REAL A(12)
      COMMON /resonance/ ee1,se1,ge1,ee2,se2,ge2,em1,sm1,gm1,eeq,seq,geq
      xK =A(1)
      fe2=(3./5.)*(8.6737E-08) * (
     1    (seq*eg*geq**2)/((eg**2-eeq**2)**2+eg**2*geq**2))
      RETURN
      END


      FUNCTION fpy11(eg,A)
      REAL A(12)
      ep1=A(1)
      sp1=A(2)
      gp1=A(3)
      fpy11=(8.6737E-08) * (   
     1    (sp1*eg*gp1**2)/((eg**2-ep1**2)**2+eg**2*gp1**2) )
      RETURN
      END

      FUNCTION plotlin(X,A)
      REAL A(12)
      plotlin=fe11(x,A)+fm11(x,A)                !+fe12(x,A)+fpy11(x,A)
      RETURN          
      END

      FUNCTION upbend(eg,A)
      REAL A(12)
      ep1=A(2)
      sp1=A(3)
      gp1=A(4)
	  upbend=(8.6737E-08) * (   
     1    (sp1*eg*gp1**2)/((eg**2-ep1**2)**2+eg**2*gp1**2) )
c	  xK = A(1)
c      aa = A(2)
c      bb = A(3)	  
c      upbend = xK*(8.6737E-08)*aa*(eg**(-bb))
      RETURN
      END

