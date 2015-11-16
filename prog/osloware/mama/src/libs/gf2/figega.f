      SUBROUTINE FIGEGA
C Calculates first generation gamma-ray distributions from
C various excitation energies. A full description is found
C in M. Guttormsen et al., NIM A255(1987)518
  
      CHARACTER APP*4, ans*1
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/response1/R(0:511,0:511),RDIM,A0,A1,FWHM
      REAL    SPECi(0:4095),SPECf(0:4095)
      REAL    F(0:4095),G(0:4095),Gtot(0:4095),FG(0:4095)
      REAL    Weight(0:511),Sing(0:511),Mult(0:511),FGold(0:511)
      REAL    Rold(0:511,0:511)
      INTEGER XDIM, YDIM, RDIM
      INTEGER XDIMf, YDIMf, XDIMw, YDIMw
      INTEGER ExpWeight,AreaCorr,StaTot,ReadStatus
      REAL    Nexp,MASta,MATot,MESta,METot
      INTEGER DUMMY

      WRITE(6,*)'The original gamma-matrix should be stored in'
      WRITE(6,*)'the source matrix and the extracted 1.gen. matrix'
      WRITE(6,*)'will appear in the destination spectrum. The last'
      WRITE(6,*)'weighting functions used can be accessed from the'
      WRITE(6,*)'response matrix using the command GR'
      WRITE(6,*)' '

      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
   2  FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(Istatus.NE.0)RETURN

C Zeroing spectra
      DO j=0,511
        FGold(j)  = 0.          !First gen. from last iteration 
        Weight(j) = 0.          !Weighting function used
      ENDDO
      DO j=0,511
        DO i=0,511
          R(i,j)    = 0.        !Weighting matrix for all Ex(initial)
          Rold(i,j) = 0.        !The matrix from last iteration
        ENDDO
      ENDDO
      DO i=0,4095
        SPECi(i)  = 0.          !Helping-spectra for streching etc.
        SPECf(i)  = 0.
      ENDDO
      DO j=0,511
        Sing(j) = 0.            !Singles particle strength
        Mult(j) = 0.            !Gamma-ray multiplicities
        sum = 0.
        DO i=0,4095
          rMAT(IDEST,i,j) = 0.  !Destination for extracted first gen. matrix 
          sum = sum + rMAT(ISP,i,j)
        ENDDO
        IF(sum.GT.0.)Sing(j) = 1000.  
        IF(sum.GT.0.)Mult(j) = 2.    
      ENDDO

      XDIMf=Idim(1,ISP,1)    !Index f is for the starting matrix fg = f-g
      YDIMf=Idim(1,ISP,2)    !(later we have index w for weihting-function)

      Ngates     = 0      !Number of excitation bins
      Icontinue  = 1      !Iterate once more
      RDIM       = 512    !Just to give flag that R(i,j) is used
      Iter       = 1      !Number of iterations
      ExH        = 8500.  !Highest excitation energy
      ExpWeight  = 0      !Default not to weight with exp. spectra
      a          = 16.0   !Fermi level density parameter a (1/MeV)
      Nexp       = 4.2    !Exponent for gamma energy
      Norm       = 1      !Default normalization with singles method
      StaTot     = 1      !Choose statistical multiplicity
      ThresSta   = 430.   !with threshold > 430 keV
      ThresTot   = 150.   !Lower exp. threshold for gammas
      ThresRatio = 0.3    !Upper limit = AMIN1(Eg* ThresRatio,ThresSta)
      ExEntry0   = 300.   !Average entry point in ground band
      AreaCorr   = 1      !Correct for areas versus multiplicity
      ReadStatus = 0      !Has not read figegain.dat

      OPEN(23,FILE='figegain.dat',STATUS='old',ERR=777)
      READ(23,*,ERR=666)ExH,Ax0,Ax1,Ay0,Ay1,Ngates
      READ(23,*,ERR=666)ExpWeight,A,Nexp
      READ(23,*,ERR=666)AxW0,AxW1,AyW0,AyW1
      READ(23,*,ERR=666)Norm,StaTot
      READ(23,*,ERR=666)ThresSta,AreaCorr
      READ(23,*,ERR=666)(Sing(i),i=0,Ngates-1)
      READ(23,*,ERR=666)(Mult(i),i=0,Ngates-1)
      ReadStatus=1                  !The most important parameters went OK to read
      READ(23,*,ERR=666)ThresTot,ThresRatio,ExH,ExEntry0 !parameters of new version
      GO TO 777
 666  WRITE(6,*)'Warning: Something wrong with your figegain.dat file'
 777  CLOSE(23)

      bx=cal(1,ISP,1,1)+cal(1,ISP,1,2)+cal(1,ISP,1,3)
      by=cal(1,ISP,2,1)+cal(1,ISP,2,2)+cal(1,ISP,2,3)
      cx=Ax0+Ax1
      cy=Ay0+Ay1
      IF(bx+by.EQ.2.)THEN     
        IF(Ax1+Ay1.EQ.2.)THEN
          Ax0=11.          !In case nothing exist
          Ax1=20.
          Ay0=9660.
          Ay1=-120.
        ENDIF
      ELSE
        Ax0=cal(1,ISP,1,1) !Adobt spectrum calibration
        Ax1=cal(1,ISP,1,2)
        Ay0=cal(1,ISP,2,1)
        Ay1=cal(1,ISP,2,2)
      ENDIF

C If we use another calibration than given in figegain, then these
C data are not valid any more
      IF(cx+cy.NE.bx+by)ReadStatus=0

C Reading various parameters
      WRITE(6,10)
  10  FORMAT(/,'Calibration for gamma-energies:')
      WRITE(6,12)Ax0
  12  FORMAT('Cal. coeff. a0 (keV) on x-axis   <',F8.1,'>:',$)
      CALL READF(5,Ax0)
      WRITE(6,14)Ax1
  14  FORMAT('Cal. coeff. a1 (keV/ch) on x-axis<',F8.1,'>:',$)
      CALL READF(5,Ax1)
      IF(Istatus.NE.0)RETURN
      WRITE(6,16)
  16  FORMAT(/,'Calibration for excitation energies:')
      WRITE(6,18)Ay0
  18  FORMAT('Cal. coeff. a0 (keV) on y-axis   <',F8.1,'>:',$)
      CALL READF(5,Ay0)
      WRITE(6,20)Ay1
  20  FORMAT('Cal. coeff. a1 (keV/ch) on y-axis<',F8.1,'>:',$)
      CALL READF(5,Ay1)
      IF(Istatus.NE.0)RETURN

      Exmax=AMAX1(Ay0+Ay1*0,Ay0+Ay1*(YDIMf-1.))
      IF(ExH.GT.Exmax)THEN
        ExH = Exmax
        ReadStatus = 0  !wrong figegain.dat, should be replaced
      ENDIF
      WRITE(6,22)ExH
  22  FORMAT(/'Excitation energy of highest gate (keV)  <',F8.1,'>:',$)
      CALL READF(5,ExH)                  
      IyH=INT(((ExH-Ay0)/Ay1)+0.5)
      IyL=INT(((0.0-Ay0)/Ay1)+0.5)
      IF(IyH.LT.0)  IyH=0
      IF(IyH.GT.511)IyH=511
      IF(IyL.LT.0)  IyL=0
      IF(IyL.GT.511)IyL=511

      WRITE(6,30)ThresTot
 30   FORMAT('Experimental lower gamma thresholds (keV)  <',F6.0,'>:',$)
      CALL READF(5,ThresTot)
      WRITE(6,32)ThresSta
 32   FORMAT('Upper threshold for nonstat. gammas (keV)  <',F6.0,'>:',$)
      CALL READF(5,ThresSta)
      WRITE(6,36)ThresTot,ThresSta,ThresRatio
 36   FORMAT(/,'It is recommended to use a sliding upper threshold when the',
     +       /,'excitation energy is lower than 1 - 2 MeV. This is performed',
     +       /,'by defining a ratio R, giving Thres=Ex*R with a lower and'
     +       /,'higher limit of',F5.0, ' and',F5.0, ' keV. Use R = 0.2 - 0.3.',
     +     /,/,'Give ratio R           (no sliding = 100.) <',F6.2,'>:',$)
      CALL READF(5,ThresRatio)
      WRITE(6,38)ExEntry0
 38   FORMAT('Average entry point in ground band (keV)   <',F6.0,'>:',$)
      CALL READF(5,ExEntry0)
      LS1=INT(((ThresSta-Ax0)/Ax1)+0.5) ! lower threshold for stat. gammas
      LT1=INT(((ThresTot-Ax0)/Ax1)+0.5) ! lower threshold for total gammas
      IF(LS1.LT.0)LS1=0
      IF(LT1.LT.0)LT1=0
      ExH=Ay0+Ay1*IyH
      ExL=Ay0+Ay1*IyL
      WRITE(6,40)ExH,ExL,IyH,IyL
 40   FORMAT(/,'First geneneration spectra extracted for',
     +       /,'excitation energies between ',F6.0,'-',F6.0,' keV',
     +       /,'corresponding to y-channels ',I5  ,' -',I5  )
      IyStep=+1               !Normally high exc. is low channel
      IF(IyH.GT.IyL)IyStep=-1
      Ngates=IABS(IyH-IyL)+1
     
      WRITE(6,42)
 42   FORMAT(/,'You may read weighting functions from disk')
      ans='y'
      IF(ExpWeight.EQ.0)ans='n'
      WRITE(6,44)ans
 44   FORMAT('Weighting by exp. 1.gen.-spectra from disk (y/n)<',A1,'>:',$)
      CALL READA1(5,ans)
      ExpWeight = 1
      IF(ans.EQ.'n'.OR.ans.EQ.'N')ExpWeight=0
      IF(Istatus.NE.0)RETURN

      IF(ExpWeight.EQ.1) THEN
        WRITE(6,46)
 46     FORMAT(/'Read experimental spectra for weighting. Be careful to choose',
     +        /,'the destination spectrum (1 or 2), where your original ALFNA ',
     +        /,'source matrix DO NOT resides.',/)
        ITYPE=3
        CALL READFILE       !NB, read into rMAT(IDEST), but will be cleared later on
        CALL ERASE
        DUMMY=1
        CALL DSPMA(DUMMY,1,1)
        CALL CLEANUP
        XDIMw=Idim(1,IDEST,1) !Index w is for the weighting function
        YDIMw=Idim(1,IDEST,2) 

        AxW0=cal(1,IDEST,1,1) !Adobt spectrum calibration
        AxW1=cal(1,IDEST,1,2)
        AyW0=cal(1,IDEST,2,1)
        AyW1=cal(1,IDEST,2,2)

        IF(AxW1.EQ.1.OR.AxW1.EQ.0.)THEN
          AxW0=Ax0            !Calibration for weighting spectra
          AxW1=Ax1
        ENDIF
        IF(AyW1.EQ.1.OR.AyW1.EQ.0.)THEN
          AyW0=Ay0
          AyW1=Ay1
        ENDIF
 
        WRITE(6,50)
 50     FORMAT(/,'Give calibration for weighting spectra:',
     1         /,'Calibration for gamma-energies:')
        WRITE(6,12)AxW0
        CALL READF(5,AxW0)
        WRITE(6,14)AxW1
        CALL READF(5,AxW1)

        WRITE(6,52)
 52     FORMAT(/,'Calibration for excitation energies:')
        WRITE(6,18)AyW0
        CALL READF(5,AyW0)
        WRITE(6,20)AyW1
        CALL READF(5,AyW1)

C Compress or stretch spectra in both directions
        DO j=0,YDIMw-1      
          DO i=0,XDIMw-1
            SPECi(i)=rMAT(IDEST,i,j)     !In x-direction
          ENDDO
          CALL ELASTIC(SPECi,SPECf,AxW0,AxW1,0.,ABS(Ay1),XDIMw,512)
          DO i=0,511
            R(i,j)=SPECf(i)
          ENDDO
        ENDDO
        A0=0.       !Saving the calibration in x-direction for responsematrix R(i,j)
        A1=ABS(Ay1)

        DO i=0,4095
           SPECi(i)  = 0. 
        ENDDO
        DO i=0,511       
          DO j=0,YDIMw-1
            SPECi(j)=R(i,j)             !In y-direction
          ENDDO
          CALL ELASTIC(SPECi,SPECf,AyW0,AyW1,Ay0,Ay1,YDIMw,512)
          DO j=0,511
            R(i,j)=SPECf(j)
          ENDDO       
        ENDDO
        DO j=0,511
          DO i=0,4095                   
            rMAT(IDEST,i,j)=0           !Clearing MAT again
          ENDDO
        ENDDO
             
      ELSE             ! or we choose Fermi estimate (calibration is OK)
       
        WRITE(6,60)
 60     FORMAT(/,'Assumes Fermi gas distribution')
        WRITE(6,62)a
 62     FORMAT('Level density parameter a (1/MeV!!)         <',F5.2,'>:',$)
        CALL READF(5,a)
        WRITE(6,64)Nexp
 64     FORMAT('Exponent n for Eg**n                        <',F5.2,'>:',$)
        IF(Istatus.NE.0)RETURN
        CALL READF(5,Nexp)
        DO j=IyH,IyL,IyStep
          Exi=Ay0+Ay1*j
          jjmax=((Exi+200.-0.)/ABS(Ay1))+0.5 !assuming 200 keV extra
          DO jj=0,MIN0(jjmax,511)
            Egam=ABS(Ay1)*jj
            IF(jj.EQ.0)THEN
              Egam=ABS(Ay1)*0.25 !In order to get something in ch 0
            ENDIF
            Exf=Exi-Egam
            IF(Exf.GE.0)THEN
              IF(Egam.LT.50.)Egam=50.
              IF(Exf.LT.300.)Exf=300.
              Egam=Egam/1000.      !Going to MeV not to get too
              Exf=Exf/1000.        !large numbers for RESP
              IF(jj.LT.512.AND.j.LT.512)THEN
                R(jj,j)=((Egam**Nexp)*EXP(2.*SQRT(a*Exf)))/(Exf**2.)
              ENDIF
            ENDIF
          ENDDO
        ENDDO 
      ENDIF

      WRITE(6,66)Norm
 66   FORMAT(/'Singles (1) or multiplicity (2) normalization   <',I1,'>:',$)
      CALL READI(5,Norm)
      IF(Istatus.NE.0)RETURN

      IF(Norm.EQ.2)THEN
        WRITE(6,68)StaTot
 68     FORMAT('Statistical (1) or total (2) multiplicity       <',I1,'>:',$)
        CALL READI(5,StaTot)
        IF(Istatus.NE.0)RETURN
      ENDIF

      ans='y'
      WRITE(6,70)ans
 70   FORMAT('Areacorrection for 1st generationspectra (y/n)  <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(Istatus.NE.0)RETURN
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')AreaCorr=1
      IF(ans.EQ.'n'.OR.ans.EQ.'N')AreaCorr=0

C Setting up reasonable multiplicities
      ans='n'
      IF(ReadStatus.EQ.0)ans='y'
      WRITE(6,72)ans
 72   FORMAT(/'Estimate new default multiplicities M(Ex) (y/n)',
     +      /,'WARNING: If answer = y, the multiplicities',
     +      /,'in the file figegain.dat will be overwritten    <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(Istatus.NE.0)RETURN
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
        DO j=0,Ngates-1
          Mult(j)=0.
        ENDDO
        jj=IyH
        DO j=0,Ngates-1
          xm=0.
          CenS=0.
          sum=0.
          Exj=Ay0+Ay1*jj
          LF2=INT(((Exj+200.-Ax0)/Ax1)+0.5)               
          IF(LF2.GT.XDIMf-1)LF2=XDIMf-1
          L1=MAX0(0,INT(((SLIDE(Exj,ThresTot,ThresSta,ThresRatio)-Ax0)/Ax1)+0.5))
          IF(StaTot.EQ.2)L1=LT1
          DO i=L1,LF2
            CenS=CenS+FLOAT(i)*rMAT(ISP,i,jj)
            sum =sum + rMAT(ISP,i,jj)
          ENDDO
          IF(sum.GT.0.)CenS=CenS/sum
          IF(CenS.GT.0)THEN
            CenS=Ax0+CenS*Ax1
            ExEntry=AMIN1(ExEntry0,Exj-200.)
            IF(ExEntry.LT.0.)ExEntry=0.
            xm=(Exj-ExEntry)/CenS
          ENDIF
          IF(xm.LT.0)xm=0.
          Mult(j)=xm
          jj=jj+IyStep
        ENDDO
      ENDIF

      IF(Norm.EQ.2) THEN
        WRITE(6,74)
 74     FORMAT(/' Multiplicity in each gate: ')
        WRITE(6,76)
 76     FORMAT(' -----------------------------------')
        Exx=ExH
        DO i=0,Ngates-1
          iCh=((Exx-Ay0)/Ay1)+0.5
          WRITE(6,78)iCh,Exx,Mult(i)
 78       FORMAT(' Y-ch=',I4,' Ex= ',F7.1,' keV <',F6.3,'>:',$)
          CALL READF(5,Mult(i))
          IF(Istatus.NE.0)RETURN
          Exx=Exx-ABS(Ay1)
        ENDDO
        WRITE(6,*)CHAR(7)                        ! Bell
        WRITE(6,76)
      ENDIF

      IF(Norm.EQ.1) THEN
        WRITE(6,80)
 80     FORMAT(/' Area of slices in singles: ')
        WRITE(6,76)
        Exx=ExH
        DO i=0,Ngates-1
          iCh=((Exx-Ay0)/Ay1)+0.5
          WRITE(6,82)iCh,Exx,Sing(i)
 82       FORMAT(' Y-ch=',I4,' Ex= ',F7.1,' keV <',F8.1,'>:',$)
          CALL READF(5,Sing(i))
          IF(Istatus.NE.0)RETURN
          Exx=Exx-ABS(Ay1)
        ENDDO
        WRITE(6,*)CHAR(7)
        WRITE(6,76)
      ENDIF
      IF(Istatus.NE.0)RETURN

C Writes to disk, so that it is easier to re-run the program
      OPEN(23,FILE='figegain.dat',ERR=888)
      WRITE(23,*)ExH,Ax0,Ax1,Ay0,Ay1,Ngates
      WRITE(23,*)ExpWeight,A,Nexp
      WRITE(23,*)AxW0,AxW1,AyW0,AyW1
      WRITE(23,*)Norm,StaTot
      WRITE(23,*)ThresSta,AreaCorr
      WRITE(23,*)(Sing(i),i=0,Ngates-1)
      WRITE(23,*)(Mult(i),i=0,Ngates-1)
      WRITE(23,*)ThresTot,ThresRatio,ExH,ExEntry0
      CLOSE(23)
888   CONTINUE

100   FORMAT('----------------------------------------------')

C Writting out parameters
      OPEN(UNIT=21,FILE='figegaout.dat',ERR=1001)
      GO TO 1002
1001  ifile=0
      GO TO 1111
1002  ifile=1
      WRITE(21,*)'Parameters used:'
      WRITE(21,40)ExH,ExL,IyH,IyL
      WRITE(21,102)Ngates
102   FORMAT(' Number of spectra=',I5)
      WRITE(21,104)Ax0,Ax1,Ay0,Ay1
104   FORMAT(' Ax0 =',F8.1,' Ax1 =',F8.1,' Ay0 =',F8.1,' Ay1 =',F8.1)
      WRITE(21,106)AxW0,AxW1,AyW0,AyW1
106   FORMAT(' AxW0=',F8.1,' AxW1=',F8.1,' AyW0=',F8.1,' AyW1=',F8.1)
      WRITE(21,108)ExpWeight,a,Nexp
108   FORMAT(' Weighting:',I2,' Level density parameter a=',F4.1,' Exponent n=',F3.1)
      WRITE(21,110)Norm,StaTot,AreaCorr
110   FORMAT(' Normalization=',I2,' Stat/Tot=',I2,' Areacorr.=',I2)
      WRITE(21,112)ThresTot,LT1,ThresSta,LS1, ThresRatio
112   FORMAT(' Experimental lower gamma threshold:     ',F6.1,' keV (ch=',I4,')',
     +     /,' Upper threshold for statistical gammas: ',F6.1,' keV (ch=',I4,')',
     +     /,' Sliding threshold given by Ex*R, with R = ',F4.2)
      WRITE(21,114)ExEntry0
114   FORMAT(' Average energy entry point in ground band ExEntry =',F6.0,' keV')
      WRITE(21,*)'Multiplicities: MA = Af/Afg and ME = (Ex-ExEntry)/<Eg>'
      WRITE(6,*)' '
      
C****************************************************************
C                        Fasten seatbelts                       *
C                                                               *
C****************************************************************

 1111 CONTINUE
      WRITE(6,116)Iter
      IF(ifile.EQ.1)WRITE(21,116)Iter
 116  FORMAT(/,'Iteration number: ',I3)
      IF(ifile.EQ.1)WRITE(21,121) 
      DO j=IyH,IyL,IyStep
        Ij=ABS(j-IyH)        !The index for Mult and Sing
        Exj=Ay0+Ay1*j        !The exc. energy for uppermost spectrum
        LW2=ABS(j-IyL)                
        DO i=0,XDIMf-1
          SPECi(i)=0.
          F(i)=rMAT(ISP,i,j) !The uppermost spectrum
          FG(i)=0.
          Gtot(i)=0.
        ENDDO
        DO jj=0,YDIMf-1
          IF(Iter.LT.4)THEN
            Weight(jj)=R(jj,j)
          ELSE
            Weight(jj)=Rold(jj,j)+R(jj,j)      !New, to prevent oscillations
          ENDIF
          IF(Weight(jj).LT.0.)Weight(jj)=0.    !New, to prevent negative weights
          Rold(jj,j)=R(jj,j)
        ENDDO
        DUMMY=0
        CALL INTEG(Weight,sumW,DUMMY,LW2)
        DO jj=0, LW2
          IF(sumW.GT.0.)THEN                   !Normalizing to 1.0
            Weight(jj)=Weight(jj)/sumW
          ELSE
            Weight(jj)=0.
          ENDIF
          IF(jj.GE.Ngates)Weight(jj)=0.
          R(jj,j)=Weight(jj)
        ENDDO
       
        LF2=INT(((Exj+200.-Ax0)/Ax1)+0.5)      !Taking 200keV extra                
        IF(LF2.GT.XDIMf-1)LF2=XDIMf-1
        LF1=MAX0(0,INT(((SLIDE(Exj,ThresTot,ThresSta,ThresRatio)-Ax0)/Ax1)+0.5))
        CALL INTEG(F,sumF,LF1,LF2)             !Integr. of F(i) between LF1->LF2
       
        JJ1=j
        JJ2=IyL
        Ijj=Ij-1                               !Index for Mult and Sing below
        iii=-1                                 !Index for Weight(iii)
c               if(jj1.EQ.55)write(6,*)j,jj1,jj2,Ij,mult(ij),Ijj,lf1,lf2
        DO jj=JJ1,JJ2,IyStep
          Ijj=Ijj+1
          iii=iii+1
          Exjj=Ay0+Ay1*jj                      !The exc. energy for G(i)
          LG2=INT(((Exjj+200.-Ax0)/Ax1)+0.5)
          IF(LG2.GT.XDIMf-1)LG2=XDIMf-1
          DO i=0,LG2
            G(i)=rMAT(ISP,i,jj)
          ENDDO
          LG1=MAX0(0,INT(((SLIDE(Exjj,ThresTot,ThresSta,ThresRatio)-Ax0)/Ax1)+0.5))
          CALL INTEG(G,sumG,LG1,LG2)
          fact=0.
          IF(Norm.EQ.1)THEN                    !Singles normalization
            x=Sing(Ijj)
            IF(x.GT.0.)fact=Weight(iii)*Sing(Ij)/x
          ELSE                                 !Multiplicity normalization
            x=Mult(Ij)*sumG
            IF(x.GT.0.)fact=Weight(iii)*Mult(Ijj)*sumF/x
c                 if(jj1.EQ.55)write(6,*)ijj,mult(ijj),lg1,lg2,iii,weight(iii)
          ENDIF
          DO i=0,LG2                           !Making Gtot(i), which shall
            Gtot(i)=Gtot(i)+fact*G(i)          !be subtracted from F(i)
          ENDDO
        ENDDO

C Calculates areacorrection alpha (see NIM A255 (1987) 518 eq.(7)) 
        alpha=1.0
        CALL INTEG(Gtot,sumGtot,LF1,LF2) !Same limits as for F(i)
        IF (AreaCorr.EQ.1.AND.sumGtot.GT.0) THEN
          alpha=(1.-(1./Mult(Ij)))*sumF/sumGtot
          IF(alpha.LT.0.85)alpha=0.85
          IF(alpha.GT.1.15)alpha=1.15
        ENDIF

C Performing the subtraction
        DO i=0, XDIMf-1
          FG(i) = F(i)-alpha*Gtot(i)
          rMAT(IDEST,i,j)=FG(i)          !Finished, storing in MAT
        ENDDO

C Prepairing for writting to figegaout.dat
C FG contains now the first generation spectrum for Ex=Exj
C Calculating statistical multiplicity from AF/AF-AG and (Ex-ExEntry)/Egamma
C in case of threshold of Thres keV
C Calculates centroides using:
        MASta=0.
        MATot=0.
        MESta=0.
        METot=0.
        CALL INTEG(F, sumFS, LF1,LF2)
        CALL INTEG(FG,sumFGS,LF1,LF2)
        CALL INTEG(F, sumFT, LT1,LF2)
        CALL INTEG(FG,sumFGT,LT1,LF2)
        IF(sumFGS.GT.0)MASta=sumFS/sumFGS   
        IF(sumFGT.GT.0)MATot=sumFT/sumFGT

        CenS=0.                         !Energy centroid of FG statistical
        DO i=LF1,LF2
          CenS=CenS+F(i)*FLOAT(i)
        ENDDO
        IF(sumFS.GT.0)CenS=CenS/sumFS
        IF(CenS.GT.0)THEN
          CenS=Ax0+CenS*Ax1
          ExEntry=AMIN1(ExEntry0,Exj-200.)
          IF(ExEntry.LT.0.)ExEntry=0.
          MESta=(Exj-ExEntry)/CenS
        ENDIF
        IF(MESta.LT.0)MESta=0.

        CenT=0.                          !Energy centroid of FG total
        DO i=LT1,LF2
          CenT=CenT+F(i)*FLOAT(i)
        ENDDO
        IF(sumFT.GT.0)CenT=CenT/sumFT
        IF(CenT.GT.0)THEN
          CenT=Ax0+CenT*Ax1
          ExEntry=AMIN1(ExEntry0,Exj-200.)
          IF(ExEntry.LT.0.)ExEntry=0.
          METot=(Exj-ExEntry)/CenT
        ENDIF
        IF(METot.LT.0)METot=0.

        CALL INTEG(F,   sumF,   LF1,LF2)
        CALL INTEG(Gtot,sumGtot,LF1,LF2)
        CALL INTEG(FG,  sumFG,  LF1,LF2)
        IF  = sumF     + 0.5
        IG  = sumGtot  + 0.5
        IFG = sumFG    + 0.5
        IS  = Sing(Ij) + 0.5
        dAA = 9999.99
        IF(FGold(j).GT.0.)dAA=100.*(sumFG - FGold(j)) / FGold(j)
        IF(ABS(dAA).GT.9999.99)dAA=9999.99
        WRITE(6,120)j,sumFG,dAA
120     FORMAT('Y-ch= ',I3,'  Area= ',F11.1,'  dA/A(%)= ',F8.2)
121     FORMAT('Y-ch  Ex     Af     Ag    Afg  Mult   Sing Alpha',
     +         ' MATot METot MASta MESta')
        IF(ifile.EQ.1)WRITE(21,122)j,Exj,IF,IG,IFG,Mult(Ij),IS,alpha,MATot,METot,MASta,MESta
122     FORMAT(I3,F6.0,3I7,F5.2,I7,5F6.2)
        FGold(j)=sumFG
      ENDDO
      XDIM  = XDIMf
      YDIM  = YDIMf
      ITYPE = 3
      CALL ERASE
      DUMMY=1
      CALL DSPMA(DUMMY,1,1)
      CALL CLEANUP
      cal(1,IDEST,1,1)=Ax0
      cal(1,IDEST,1,2)=Ax1
      cal(1,IDEST,1,3)=0
      cal(1,IDEST,2,1)=Ay0
      cal(1,IDEST,2,2)=Ay1
      cal(1,IDEST,2,3)=0

      WRITE(6,100)

      IF(Iter.EQ.1) WRITE(6,124)
 124    FORMAT(' You may now iterate on the last first-',/,
     +         ' generation spectra just obtained. Remember,the',/,
     +         ' input spectra must be of unfolded type.',/,
     +         ' WARNING! Your first-generation spectra just',/,
     +         ' obtained, will be overwritten by the new one',/,
     +         ' for each iteration.',//)


      WRITE(6,126)Icontinue
 126  FORMAT(/' Iterate (1) or stop (0)  <',I1,'>:',$)
      CALL READI(5,Icontinue)
     
      IF(Icontinue.EQ.1)THEN
C Reading in the 1.gen. spectra just obtain from rMAT(IDEST,i,j) to
C the new weighting matrix R(i,j). Then compress or stretch the
C spectra in x-directions (y-direction OK).
        Iter=Iter+1
        DO i=0,4095
          SPECi(i)  = 0. 
        ENDDO
        DO j=IyH,IyL,IyStep      
          DO i=0,XDIMf-1
            SPECi(i)=rMAT(IDEST,i,j)    !In x-direction
          ENDDO
          CALL ELASTIC(SPECi,SPECf,Ax0,Ax1,0,ABS(Ay1),XDIMf,512)
          DO i=0,511
            R(i,j)=SPECf(i)             !R(i,j) now contain new weighting
          ENDDO
        ENDDO
        DO i = 0, 4095
          SPECi(i) = 0.
          DO j=0, 511
            rMAT(IDEST,i,j) = 0.        !Clearing MAT again
          ENDDO
        ENDDO
        GO TO 1111                      !New iteration starts

      ELSE

        CLOSE(21)
C Updating comment in the heading of spectrum file
        xcomm(1:3)='FG:'
        fname(1,IDEST)(1:8)='FG'//fname(1,ISP)(1:6)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
        WRITE(6,128)Iter,IDEST
 128    FORMAT('Last iteration',I3, ' stored in matrix',I2,/,
     +         'Additional results written to figegaout.dat')
        RETURN
      ENDIF
 9999 WRITE(6,*) 'Problems to write on disk'
      END


      SUBROUTINE INTEG(Sp,sum,L1,L2)
C Integrates SPEC from L1 to L2 and return area
      DIMENSION Sp(0:L2)
      sum=0.0
      IF(L1.LT.0)L1=0
      IF(L1.GT.L2)RETURN
      DO i=L1,L2
        sum = sum + Sp(i)
      ENDDO
      RETURN
      END


      FUNCTION SLIDE(Ex,Thres1,Thres2,ThresRatio)
      SLIDE=ThresRatio*Ex
      IF(SLIDE.LT.Thres1)SLIDE=Thres1
      IF(SLIDE.GT.Thres2)SLIDE=Thres2
      RETURN
      END
