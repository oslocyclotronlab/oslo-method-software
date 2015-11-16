      PROGRAM DECAY
C HISTORY
C June     1996: The program of Gabriel Munoz was rewritten, and made much more 
C                effectiv ( 60 times)
C December 1996: Possible to make gamex and genex matrices
C February 1997: HinK=HinderK(Kii-1,2) for quadrupole transitions
C March    1997: TotRho(ie,j,p,k)=FermiRho(ie)*(1./(FLOAT(j)+1.))*ax1 
C                Here, j+1 is number of K values per spin j
C July     1997: CALL ARRAY(xlambda) were placed before asking of hindrancefactor.
C                Thus, K-hindrance did not work in June (I think).
C                Now corrected and tested.
C  August  1997: New output spectrum (gam2.dec) which contains gamma g1 and g2
C                giving g1 + g2 = Ex. To be compared with (n,2gamma) reactions
C
C
C THE PROGRAM
C The energies are given by the calibration constants Egam = ax0 + ax1 x ch,
C with shift parameter ax0 = 0 keV and dispersion ax1 = user spesified.
C The program is limited to energy-spectra of ax1 x Emax < 4096 channels.  
C With i.e. a dispersion of ax1=10 keV/ch and Emax = 8000 keV, we have 800 channels.
C However, such calculations might take long time since we have 800 excitation bins,
C and with all spins, parities and K values, this makes 182 starting levels for
C each bin, giving at least 800 x 182 = 145600 cascades (or a multiple of this value).
C The initial configurations are limited to spins < 13, and with K < J+1.
C The transition probability is given by  ElMag*HinK*En*TotRho(e,j,p,k), where
C the three factors in front of the level density is predefined in subroutine ARRAY.
C As input is required a file giving all known discrete levels for the nuclide
C studied. The file-name is given by lev-"nuclide-name", and you should study
C this file to make simulation for another nuclei. You then also have to give 
C other values for Amass, Arot and Epair.

C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      REAL Spec(0:8191)
      REAL Calib(6)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER APP*4

      COMMON/Fermigas/Amass,Arot,Epair,EcLow,EcHigh,FermiRho(0:4095),iLow,Cfg
      COMMON/Density/TotRho(0:4095,0:12,0:1,0:12)
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot
      COMMON/Cascades/Gam(0:4095),Gam2(0:4095),Gamex(0:4095,0:511),Gen(0:4095,0:15),Genex(0:4095,0:511)
      COMMON/Cas/Pm(0:4095,0:15),GamGam(0:511,0:511),ww(0:12,0:1,0:12)
      COMMON/Control/ncas,nTotal,nStep,genTot,gemTot,xloops,NormCas(0:4095)
      COMMON/Initial/DisK(0:12,0:12),DisJP(0:12,0:1)

      REAL Jt,wf
      REAL Mul(0:4095),Var(0:4095),Skw(0:4095)
      REAL RhoIP(0:4095,0:12,0:1)
      CHARACTER outfile*20, NucName*11,ans*1
      INTEGER p,Pi
      INTEGER*4 time,stime,itime0,itime1,elapse
      CHARACTER*24 string


      WRITE(6,*)'     ___________________________________________'
      WRITE(6,*)'    |                                           |'
      WRITE(6,*)'    |               D E C A Y  2.4              |'
      WRITE(6,*)'    |                                           |'
      WRITE(6,*)'    |      Program to calculate the gamma       |'
      WRITE(6,*)'    |       decay. The level density is         |'
      WRITE(6,*)'    |     composed of discrete levels and       |'
      WRITE(6,*)'    |     levels based on the Fermi gas.        |'
      WRITE(6,*)'    |                                           |'
      WRITE(6,*)'    |     Gabriel Munoz    Magne Guttormsen     |'
      WRITE(6,*)'    |     Sunniva Siem     Andreas Schiller     |'
      WRITE(6,*)'    |    Oslo Cyclotron Laboratory, July 1997   |'
      WRITE(6,*)'    |___________________________________________|'

      ax1      =20.              ! Energy dispersion (steplength) (keV/ch) (ax0=0 keV)
      iCompEg  =1                ! Eventuelly compress gam x gam matrix with iCompEg>1
      iCompEx  =1                ! Eventuelly compress (Eg,Ex) matrices with iCompEx>1
      Emin     =0.               ! Lower limit exc. energy interval (keV)
      Emax     =8200.            ! Upper limit exc. energy interval (keV)
      Jt       =2.5              ! Target spin og 161Dy is 5/2+
      Bn       =8200.            ! Neutron binding energy in 162Dy
      Dave     =0.0029           ! Average spacing D between 2+ and 3+ for n-capture
      Kmax     =6                ! Maximum K allowed
      Ji       =4                ! Initial spin populated
      Pi       =0                ! Initial parity (+)
      Ki       =0                ! Initial K populated
      CutGam   =430.             ! Gamma energy cut-off parameter (keV)
      NumCas   =10000            ! Total number of gamma cascades
      xen      =4.2              ! Exponent in formula (Eg)**n*RHO
      xlambda  =10.              ! Basenumber for K-hindrence
      Ioption  =1                ! Single spin-value (0) or not (1)
      IDEST    =1                ! Write matrix 1 of rMAT(2,0:4095,0:511)
      genTot   =0.               ! Average total multiplicity (Egam > 0 keV)
      gemTot   =0.               ! Average multiplicity (Egam > CutGam = 430 keV)
      nConf    =1                ! Number of initial configurations (J,P,K)
      xloops   =0.               ! Summing value (should be number of cascades)

4     NucName='162Dy'
      WRITE(6,5)NucName
5     FORMAT(/'Monte Carlo simulation for nuclide              <',A5,'>:',$)
      CALL READA(5,NucName)
      IF(Istatus.NE.0)GO TO 4
      READ(NucName(1:3),'(I3)',ERR=1)Mass
      GO TO 3
1     READ(NucName(1:2),'(I2)',ERR=2)Mass
      GO TO 3      
2     READ(NucName(1:1),'(I1)',ERR=4)Mass
3     Amass=Mass

      eo=1.
      IF((Mass/2)*2.EQ.Mass)eo=2.
      Epair=1.0*(eo*12./SQRT(Amass))*1000.         ! 100% effective pairing
      WRITE(6,6)Epair
6     FORMAT('Pairing energy gap (ee=2x,oe=1x or oo=0x Delta) <',F5.0,'>:',$)
      CALL READF(5,Epair)

      EcLow=1600.
      WRITE(6,111) EcLow
111   FORMAT( 'Lower limit for const. temp. region (keV)      <',F6.0,'>:',$)
      CALL READF(5,EcLow)
      EcHigh=Epair+200.
      WRITE(6,112) EcHigh
112   FORMAT( 'Upper limit for const. temp. region (keV)      <',F6.0,'>:',$)
      CALL READF(5,EcHigh)

      Rigid=(0.0137/1000.)*(Amass**(5./3.)) 
      Arig=1./(2.*Rigid)
      Arot=1.62*Arig
      IF(NucName.EQ.'163Dy')Arot=12.1
      IF(NucName.EQ.'162Dy')Arot=12.3
      IF(NucName.EQ.'161Dy')Arot=12.5
      IF(NucName.EQ.'160Dy')Arot=12.7
      IF(NucName.EQ.'159Dy')Arot=13.1
      IF(NucName.EQ.'158Dy')Arot=13.4
      IF(NucName.EQ.'157Dy')Arot=14.0

      WRITE(6,7)Arig,Arot
7     FORMAT('Rotational parameter (rigid = ',F4.1,' keV)          <',F4.1,'>:',$)
      CALL READF(5,Arot)

      a=(Amass/10.)/1000.
      WRITE(6,8)a
8     FORMAT('Fermi gas level density parameter a(1/keV)     <',F6.4,'>:',$)
      CALL READF(5,a)

      WRITE(6,9)xen
9     FORMAT('Exponent value (Egam)**n   (3 < n < 5 )           <',F3.1,'>:',$)        
      CALL READF(5,xen)

      WRITE(6,120)
120   FORMAT(/,'The spin of the 161Dy target in neutron capture is 5/2. Thus, the'
     +      ,/,'neutron resonances populated in 162Dy have spins 2 and 3. From the'
     +      ,/,'known spacing of 2+ and 3+ states of D=0.0029 keV at Bn=8200 keV,'
     +      ,/,'we can find the normalization constant to be applied in front of' 
     +      ,/,'the Fermi gas formula.')
      WRITE(6,121)Jt
121   FORMAT('Target spin value                                 <',F3.1,'>:',$)
      CALL READF(5,Jt)
      WRITE(6,122)Dave
122   FORMAT('Spacing of neutron capture resonances D(keV)   <',F6.5,'>:',$)
      CALL READF(5,Dave)
      WRITE(6,123)Bn
123   FORMAT('Neutron binding energy Bn (keV)               <',F7.1,'>:',$)
      CALL READF(5,Bn)

      CALL FixFermi(Jt,Bn,Dave)

      WRITE(6,124)Cfg
124   FORMAT('Fermi gas normalization constant C(fg)        <',F7.2,'>:',$)
      CALL READF(5,Cfg)

10    CONTINUE
      WRITE(6,12) ax1
12    FORMAT('Energy-dispersion a1 (keV/ch)                   <',F5.1,'>:',$)
      CALL READF(5,ax1)
      WRITE(6,14) Emin
14    FORMAT( 'Lower limit of excitation energy (keV)        <',F7.1,'>:',$)
      CALL READF(5,Emin)
      iemin=Emin/ax1+0.5
      WRITE(6,16) Emax
16    FORMAT( 'Upper limit of excitation energy (keV)        <',F7.1,'>:',$)
      CALL READF(5,Emax)
      iemax=Emax/ax1+0.5
      IF(iemax.GT.4095)THEN
        WRITE(6,18)Emax,ax1
18      FORMAT('Sorry, Emax/ax1 = ',F7.1,'/',F5.1,' > 4095 channels. Try again')
        GO TO 10
      ENDIF

C Calculating reasonable excitation bin for making matrices Gamex(Eg,Ex) and Genex(Eg,Ex).
C Assumes that approx. 100 keV excitation-bins are appropriate
      ExBin=100.
      IF(Emax-Emin.LT.ExBin)ExBin=Emax-Emin
      IF(ExBin.LE.ax1)ExBin=ax1
      WRITE(6,15) ExBin
15    FORMAT( 'Choose excitation bin-energy (keV)            <',F7.1,'>:',$)
      CALL READF(5,ExBin)
      iCompEx=INT((ExBin/ax1)+0.5)
      IF(iCompEx.LT.1)iCompEx=1

C Finding reasonable energy bin for making matrix Gamgam(Eg,Eg)
      IF(iemax.GT.511)THEN
        Egammamax=ax1*511.
        iCompEg=(1.0 + (FLOAT(iemax)/511.))
        WRITE(6,17)iemax,iCompEg,Egammamax,ax1
17      FORMAT(/,'The highest channnel for gamma-energies will be:',I5,', however the'
     +      ,/,'maximum dimension of the Egam x Egam matrix is 512 x 512 channels.'
     +      ,/,'You may compress the data by a factor of ',I2,' in order to include'
     +      ,/,'the highest energies or you may choose to see only the lowest energies'
     +      ,/,'up to ',F7.1,' keV with the dispersion of ',F6.1,' keV.'
     +      ,/,'(This compression concerns only the Egam x Egam matrix)') 
        WRITE(6,19)iCompEg
19      FORMAT('Compression factor (integer) for Egam-Egam matrix  <',I2,'>:',$)
        CALL READI(5,iCompEg)
      ENDIF

      length=4096
      IF(iemax.LT.2047)length=2048
      IF(iemax.LT.1023)length=1024
      IF(iemax.LT. 511)length= 512
      IF(iemax.LT. 255)length= 256
      IF(iemax.LT. 127)length= 128
      IF(iemax.LT.  63)length=  64

      WRITE(6,20) Ioption
20    FORMAT( 'Type (0) for initial single spin, parity and K',
     *      /,'Type (1) for initial default distribution with ',
     *      /,'J = 0 - 12, Pi = +/- and K = 0 - Kmax',
     *      /,'Select your option                                  <',I1,'>:',$)
      CALL READI(5,Ioption)
      IF (Ioption.EQ.0) THEN
        WRITE(6,22) Ji
        CALL READI(5,Ji)
        WRITE(6,24) Pi
        CALL READI(5,Pi)
        WRITE(6,26) Ki
        CALL READI(5,Ki)
22      FORMAT('Initial spin populated                             <',I2,'>:',$)
24      FORMAT('Initial positive (0) or negative (1) parity         <',I1,'>:',$)
26      FORMAT('Initial K-projection                                <',I1,'>:',$)
        DO j=0,12
          DO p=0,1
            DO k=0,MIN0(j,Kmax)
              ww(j,p,k)=0.
            ENDDO
          ENDDO
        ENDDO
        ww(Ji,Pi,Ki)=1.
      ENDIF

      WRITE(6,27) Kmax
      CALL READI(5,Kmax)
27    FORMAT('Maximum K-projection populated in decay (Kmax<13)  <',I2,'>:',$)


      IF (Ioption.EQ.1) THEN
        DO J=0,12
          DO K=0,MIN0(j,Kmax)
            DisK(J,K)=1.           !Initial flat K-distribution
          ENDDO
        ENDDO
        DisJP(0 ,0)=0.01469
        DisJP(0 ,1)=0.01469
        DisJP(1 ,0)=0.04014
        DisJP(1 ,1)=0.04014
        DisJP(2 ,0)=0.06560
        DisJP(2 ,1)=0.06560
        DisJP(3 ,0)=0.07010
        DisJP(3 ,1)=0.07010
        DisJP(4 ,0)=0.07460
        DisJP(4 ,1)=0.07460
        DisJP(5 ,0)=0.07031       !Initial spin-distribution
        DisJP(5 ,1)=0.07031
        DisJP(6 ,0)=0.06603
        DisJP(6 ,1)=0.06603
        DisJP(7 ,0)=0.04590
        DisJP(7 ,1)=0.04590
        DisJP(8 ,0)=0.02573
        DisJP(8 ,1)=0.02573
        DisJP(9 ,0)=0.01567
        DisJP(9 ,1)=0.01567
        DisJP(10,0)=0.00561
        DisJP(10,1)=0.00561
        DisJP(11,0)=0.00375
        DisJP(11,1)=0.00375
        DisJP(12,0)=0.00189
        DisJP(12,1)=0.00189
         
        CALL DJP
        CALL DK(Kmax)
58      sum=0.  
        nConf=0
        WRITE(6,59)(i,i=0,Kmax)
59      FORMAT('  J  P   Jdis  K = ',I1,11I7)
        DO j=0,12
          DO p=0,1
            DO k=0,MIN0(j,Kmax)
              ww(j,p,k)=DisJP(j,p)*DisK(j,k)
              sum=sum+ww(j,p,k)
              nConf=nConf+1
            ENDDO
            WRITE(6,52)j,p,DisJP(j,p),(DisK(j,k),k=0,MIN0(j,Kmax))
          ENDDO
        ENDDO
 52     FORMAT(2I3,14F7.4)
        WRITE(6,53)nConf,sum
 53     FORMAT('Number of configurations = ',I4,'.  Weights normalized to ',F6.4)
        Modify=0
        WRITE(6,56)Modify
 56     FORMAT('Distributions OK (0) or change J (1), change K (2)  <',I1,'>:',$)
        CALL READI(5,Modify)
        IF(Modify.EQ.1)THEN
          CALL DJP
          GO TO 58
        ENDIF
        IF(Modify.EQ.2)THEN
          CALL DK(Kmax)
          GO TO 58
        ENDIF
      ENDIF


C Initialization
      itime0=time()
      DO i=0,length-1
        Gam(i)=0.
        Mul(i)=0.
        Var(i)=0.
        Skw(i)=0.
        FermiRho(i)=0.
        DO j=0,15
          Gen(i,j)=0.
          Pm(i,j) =0.
        ENDDO
      ENDDO   
     
      DO i=0,MIN0(511,length-1)
        DO j=0,MIN0(511,length-1)
          GamGam(i,j)=0.
        ENDDO
      ENDDO

      DO j=0,12
        DO p=0,1
          DO k=0,MIN0(j,Kmax)             !ie = energy, j = spin, p = parity
            DO ie=0,length-1              !k  = projection of j on symmetry axis
              TotRho(ie,j,p,k)=0.
              RhoIP(ie,j,p)   =0.
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      CALL DisLevels(NucName)             !Reading discrete levels

      DO j=0,12
        CALL Fermi(j)                     !Adding Fermi gas level density
        DO p=0,1
          DO k=0,MIN0(j,Kmax)
            DO ie=iLow,iemax              !Fermi gas starts at iLow
              TotRho(ie,j,p,k)=FermiRho(ie)*(1./(FLOAT(j)+1.))*ax1  !The j+1 is new
c               IF(ie.GT.200.AND.ie.LT.250)TotRho(ie,j,p,k)=TotRho(ie,j,p,k)*2. !hoho
c               IF(ie.GT.350.AND.ie.LT.400)TotRho(ie,j,p,k)=TotRho(ie,j,p,k)*2. !hoho
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      
c      DO j=0,12   !dette skal bort
c        DO p=0,1
c          DO k=0,MIN0(j,Kmax)
c            DO ie=0,iemax              !Fermi gas starts at iLow
c              IF ((j.NE.0).OR.(p.NE.0))TotRho(ie,j,p,k)=0             !HOHO HAHA
c            ENDDO
c          ENDDO
c        ENDDO
c      ENDDO   

      DO ie=0,iemax
        DO j=0,12                                                   
          DO p=0,1
            DO k=0,MIN0(j,Kmax)           !Projects away k
              RhoIP(ie,j,p)=RhoIP(ie,j,p)+TotRho(ie,j,p,k)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

C Writing level density matrix RhoIP(ie,j,p) to file
      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=ax1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      XDIM=length
      YDIM=26
      
      DO i=0,XDIM-1
        DO j=0,(YDIM/2)-1
          rMAT(IDEST,i,2*j+0)=RhoIP(i,j,0)
          rMAT(IDEST,i,2*j+1)=RhoIP(i,j,1)
        ENDDO
      ENDDO
      outfile='rho.dec'
      comment='Number of (J,P) levels/ch for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,50) outfile
50    FORMAT('Level density RhoIP written to file:             ',A11)

      AveEx=(FLOAT(iemin+iemax)/2.)*ax1
      Multiple=INT((1000./FLOAT(nConf))+0.5)
      NumCas=Multiple*nConf
28    WRITE(6,30) NumCas
30    FORMAT('Number of gamma-cascades from each Ex         <',I7,'>:',$)
      CALL READI(5,NumCas)
      Multiple=INT((FLOAT(NumCas)/FLOAT(nConf))+0.5)
      CALL TESTCAS(Multiple)
      
C Estimating elapsed time for the simulation 
      xmul=0.3+0.00052*AveEx       !Rough estimate of multiplicity
      stime=0.0005550*xmul*(20./ax1)*nCasTot
      IF(Ioption.EQ.0)stime=stime*0.572
      ihour=stime/3600.
      minut=(stime-ihour*3600)/60.
      isec =stime-ihour*3600-minut*60
      WRITE(6,31)ax1,nConf
 31   FORMAT('Number of cascades per Ex (',F6.0,' keV) is adjusted to',/,
     +'a multiple of (J,P,K)-configurations (',I3,') at highest Ex')
      WRITE(6,33)Multiple,nConf,nConf*Multiple,nCasTot
 33   FORMAT('Number of cascades per Ex:  ',I8,' x ',I3,' = ',I11,/,
     +'giving a total number of cascades (all Ex) = ',I11) 
      WRITE(6,32)nCasTot,ihour,minut,isec
 32   FORMAT('Estimated time (cheetah),',I10,' cascades:',I2,'h ',I2,'m ',I2,'s')
      ans='n'
      WRITE(6,34)ans
 34   FORMAT('Change number of cascades (y/n)                     <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y') GO TO 28
      nStep=MAX0(1,nCasTot/20)

      WRITE(6,44) CutGam
44    FORMAT('Gamma-energy cut-off for multiplicity (keV)    <',F6.1,'>:',$)
      CALL READF(5,CutGam)
      mCutGam=CutGam/ax1+0.5

      WRITE(6,47)xlambda
47    FORMAT('Times K-hindered/forbiddeness 1(no) - 100(max) <',F6.1,'>:',$)
      CALL READF(5,xlambda)
      CALL ARRAY(xlambda)


C     ****************     M A I N   L O O P     *******************
      nTotal=0         
      elapse=time()+stime
c      string=ctime(elapse)
      WRITE(6,54)string(1:19)
54    FORMAT('|----------------->| Finished around ',A19)

      IF (Ioption.EQ.1) THEN  
        DO j=0,12
          DO p=0,1
            DO k=0,MIN0(j,Kmax)
              wf=ww(j,p,k)
              CALL CASCADE(j,p,k,wf)
            ENDDO
          ENDDO
        ENDDO
      ELSE
        wf=ww(Ji,Pi,Ki)
        CALL CASCADE(Ji,Pi,Ki,wf)      !Only one initial spin, parity and K
      ENDIF
      WRITE(6,*)' '
      itime1=time()-itime0
C     ***************  E N D    M A I N   L O O P  ****************

C Calculating multiplicity, variance and skewness
      DO ie=iemin,iemax
        sumy=0.
        pm1 =0.
        pm2 =0.
        pm3 =0.
        DO m=0,15
          sumy=sumy+Pm(ie,m)
          pm1 =pm1 +Pm(ie,m)*FLOAT(m)
          pm2 =pm2 +Pm(ie,m)*FLOAT(m)*FLOAT(m)
          pm3 =pm3 +Pm(ie,m)*FLOAT(m)*FLOAT(m)*FLOAT(m)
        ENDDO
        IF(sumy.LE.0.) THEN
          Mul(ie)=0.
          Var(ie)=0.
          Skw(ie)=0.
        ELSE
          x1=pm1/sumy
          x2=pm2/sumy
          x3=pm3/sumy
          sigma=SQRT(x2-x1*x1)
          sgm=sigma*sigma*sigma
          IF(sgm.GT.0.) THEN
            skew=(x3-3.*x1*x2+2.*x1*x1*x1)/sgm
          ELSE
            skew=0.
          ENDIF
          Mul(ie)=x1
          Var(ie)=sigma
          Skw(ie)=skew
        ENDIF
      ENDDO 
        
C  Writing spectra to files
      Calib(1)=0.0
      Calib(2)=ax1
      Calib(3)=0.0
      
C Writing singles gamma spectrum
      DO i=0,length-1
        Spec(i)=Gam(i)
      ENDDO
      outfile='gam.dec'
      comment=' Total gamma-spectrum  for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(20,comment,length,Spec,Calib)
      CLOSE(20)
      WRITE(6,60) outfile
60    FORMAT('Total gamma-spectrum written to file:            ',A11)

C Writing mul=2 gamma spectrum
      DO i=0,length-1
        Spec(i)=Gam2(i)
      ENDDO
      outfile='gam2.dec'
      comment=' Total gamma-spectrum with M=2 for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(20,comment,length,Spec,Calib)
      CLOSE(20)
      WRITE(6,61) outfile
61    FORMAT('Total gamma-spectrum with M=2 written to file:  ',A11)

C Writing multiplicity moments
      DO i=0,length-1
        Spec(i)=Mul(i)
      ENDDO
      outfile='mul.dec'
      comment=' Multiplicity(Ex) for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(20,comment,length,Spec,Calib)
      CLOSE(20)
      WRITE(6,62) outfile
62    FORMAT('Multiplicity(Ex) written to file:                ',A11)

      DO i=0,length-1
        Spec(i)=Var(i)
      ENDDO
      outfile='var.dec'
      comment=' Variance(Ex) for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(20,comment,length,Spec,Calib)
      CLOSE(20)
      WRITE(6,64) outfile
64    FORMAT('Variance(Ex) written to file:                    ',A11)

      DO i=0,length-1
        Spec(i)=Skw(i)
       ENDDO
      outfile='skw.dec'
      comment=' Skewness(Ex) for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(20,comment,length,Spec,Calib)
      CLOSE(20)
      WRITE(6,66) outfile
66    FORMAT('Skewness(Ex) written to file:                    ',A11)

C Writing matrices to files
      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=ax1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      XDIM=length
      YDIM=16
      DO j=0,YDIM-1
        DO i=0,XDIM-1
          rMAT(IDEST,i,j)=Pm(i,j)
        ENDDO
      ENDDO
      outfile='muldis.dec'
      comment=' Multiplicity distribution for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,68) outfile
68    FORMAT('Multiplicity distribution written to file:    ',A11)

      bb=(iCompEg-1.)/2.
      cal(1,1,1,1)=0.+ax1*bb+0.*bb*bb 
      cal(1,1,1,2)=iCompEg*(ax1+2.*0.*bb)
      cal(1,1,1,3)=iCompEg*iCompEg*0.
      cal(1,1,2,1)=0.+ax1*bb+0.*bb*bb
      cal(1,1,2,2)=iCompEg*(ax1+2.*0.*bb)
      cal(1,1,2,3)=iCompEg*iCompEg*0.
      XDIM=MIN0(512,length)
      YDIM=XDIM
      DO j=0,YDIM-1
        DO i=0,XDIM-1
          rMAT(IDEST,i,j)=GamGam(i,j)+GamGam(j,i)     !Symmetrize
        ENDDO
      ENDDO
      outfile='gamgam.dec'
      comment=' Gamma*gamma matrix for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,70) outfile
70    FORMAT('Gamma*gamma matrix written to file:           ',A11)

      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=ax1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      XDIM=length
      YDIM=16
      DO i=0,XDIM-1
        DO j=0,YDIM-1
          rMAT(IDEST,i,j)=Gen(i,j)
        ENDDO
      ENDDO
      outfile='gen.dec'
      comment=' Gamma-generation spectra for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,71)outfile
71    FORMAT('Gamma-generation spectra written to matrix:      ',A11)

      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=ax1
      cal(1,1,1,3)=0.
      bb=(iCompEx-1.)/2.
      cal(1,1,2,1)=0.+ax1*bb+0.*bb*bb
      cal(1,1,2,2)=iCompEx*(ax1+2.*0.*bb)
      cal(1,1,2,3)=iCompEx*iCompEx*0.
      XDIM=length
      YDIM=Emax/(ax1*iCompEx)
      DO i=0,XDIM-1
        DO j=0,YDIM-1
          rMAT(IDEST,i,j)=Genex(i,j)
        ENDDO
      ENDDO
      outfile='genex.dec'
      comment=' First-generation(Ex) for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,72)outfile
72    FORMAT('First-generation Fg(Eg,Ex) written to matrix:  ',A11)

      cal(1,1,1,1)=0. 
      cal(1,1,1,2)=ax1
      cal(1,1,1,3)=0.
      bb=(iCompEx-1.)/2.
      cal(1,1,2,1)=0.+ax1*bb+0.*bb*bb
      cal(1,1,2,2)=iCompEx*(ax1+2.*0.*bb)
      cal(1,1,2,3)=iCompEx*iCompEx*0.
      XDIM=length
      YDIM=Emax/(ax1*iCompEx)
      DO i=0,XDIM-1
        DO j=0,YDIM-1
          rMAT(IDEST,i,j)=Gamex(i,j)
        ENDDO
      ENDDO
      outfile='gamex.dec'
      comment=' Gamma-total spectra for '//NucName
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,73)outfile
73    FORMAT('Total-gamma Gam(Eg,Ex) written to matrix  :    ',A11)

C Estimating some parameters from the simulation
      AveGamt=0.
      AveGami=0.
      DO i=0,iemax
        AveGamt=AveGamt+Gam(i)
        AveGami=AveGami+FLOAT(i)*Gam(i)
      ENDDO
      IF(AveGamt.GT.0)AveGamt=(AveGami/AveGamt)*ax1

      AveGamc=0.
      AveGami=0.
      DO i=mCutGam,iemax
        AveGamc=AveGamc+Gam(i)
        AveGami=AveGami+FLOAT(i)*Gam(i)
      ENDDO
      IF(AveGamc.GT.0)AveGamc=(AveGami/AveGamc)*ax1
       
      AveMulc=gemTot/xloops
      AveMult=genTot/xloops

      AveSigc=0.
      AveSkwc=0.
      DO i=iemin,iemax
        AveSigc=AveSigc+Var(i)
        AveSkwc=AveSkwc+Skw(i)
      ENDDO
      AveSigc=AveSigc/FLOAT(iemax-iemin+1)
      AveSkwc=AveSkwc/FLOAT(iemax-iemin+1)

      WRITE(6,74)NumCas,iemax-iemin+1
74    FORMAT(/'Total number of cascades ',I7,' on ',I5,' excitation bins')
      IF(nTotal.NE.NumCas)THEN
        WRITE(6,75)nTotal
75      FORMAT('Warning,',I7,' effective cascades (missing levels (J,P,K))')
      ENDIF
      WRITE(6,76)AveEx
76    FORMAT('Average excitation energy (keV) ',F7.1)
      WRITE(6,78)AveGamc,CutGam,AveGamt
78    FORMAT('Average gamma-energy (keV)',F7.1,'(Eg>',F5.0,') and',F7.1,'(Eg>0.)')
      WRITE(6,80)AveMulc,CutGam,AveMult
80    FORMAT('Average gamma-multiplicity',F7.3,'(Eg>',F5.0,') and',F7.3,'(Eg>0.)')
      WRITE(6,82)AveSigc,CutGam
82    FORMAT('Average gamma-variance    ',F7.3,'(Eg>',F5.0,')')
      WRITE(6,84)AveSkwc,CutGam
84    FORMAT('Average gamma-skewness    ',F7.3,'(Eg>',F5.0,')')
      WRITE(6,86)itime1,stime
86    FORMAT('Time used ',I7,' s,  (predicted ',I7,' s)')

      STOP

99    PRINT 88, infile
88    FORMAT('Error during opening file: ',A20)
      END


      SUBROUTINE CASCADE(Ji,Pi,Ki,xw)
C  The heart of the program. The cascade goes from ei down to ground state at 0 keV.
C  The initial quantum numbers are (Ji,Pi,Ki). The next level gets numbers
C  (Jii,Pii,Kii). This set is found by a random number applied to the cummulative
C  distribution of all probable (J,P,K). When (Jii,Pii,Kii) has been determined,
C  the decay in energy is performed with a new random number for the probability
C  Prob=ElMag*HinK*En*TotRho(eii,Jii,Pii,Kii).
C  We have tried to optimalize this routine with respect to time.

      COMMON/Density/TotRho(0:4095,0:12,0:1,0:12)
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot
      COMMON/Cascades/Gam(0:4095),Gam2(0:4095),Gamex(0:4095,0:511),Gen(0:4095,0:15),Genex(0:4095,0:511)
      COMMON/Cas/Pm(0:4095,0:15),GamGam(0:511,0:511),ww(0:12,0:1,0:12)
      COMMON/Control/ncas,nTotal,nStep,genTot,gemTot,xloops,NormCas(0:4095)
      COMMON/Arrays/Egn(0:4095),HinderK(-12:12,1:2),ElMl(-1:2)
      REAL Prob(0:4095,0:181),cum(-1:4095),sum(0:181)         !182=(1+2+3+4+...+13)*2
      INTEGER ien(0:15),jv(0:181),pv(0:181),kv(0:181)
      INTEGER e,ei,eii,p,Pi,Pii

      cum(-1) = 0.                                !Dummy, for technical reasons
      DO ei=iemin,iemax                           !Excitation energy loop starts
        IF(FLOAT(ei)*xw*TotRho(ei,Ji,Pi,Ki).LE.0.)GO TO 200
        DO nc=1,NormCas(ei)                       !Cascade loop starts
          nTotal=nTotal+1
          nTest=(nTotal/nStep)*nStep
          IF(nTest.EQ.nTotal)THEN
          WRITE(6,1111)
 1111     FORMAT('.',$)
c            ist=putc('.')
            call flush(6)
          ENDIF
          igen = 0                                !Initializing for each new cascade
          igem = 0

          Jii  = Ji
          Pii  = Pi
          Kii  = Ki
          eii  = ei

999       CONTINUE

C Finding new set of quantum numbers Jii, Pii and Kii
C The index m labels each configuration (Jii,Pii,Kii) from 0 up to max(m1)
          m1=-1
          j1=MAX0( 0,Jii-2)
          j2=MIN0(12,Jii+1)
          DO j=j1,j2
            ElMag=ElMl(Jii-j)
            DO p=0,1
              DO k=0,MIN0(j,Kmax)
                m1=m1+1
                jv(m1)=j
                pv(m1)=p
                kv(m1)=k
                sum(m1)=0.
                IF ((Jii-j).GT.1)THEN     !Nytt
                  HinK=HinderK(Kii-1,2)   !nytt
                ELSE                      !nytt
                  HinK=HinderK(Kii-k,1)   !nytt
                ENDIF                     !nytt
                DO e=0,eii
                  En=Egn(eii-e)
                  Prob(e,m1)=ElMag*HinK*En*TotRho(e,j,p,k)
                  sum(m1)=sum(m1)+Prob(e,m1)
                ENDDO
                cum(m1)=cum(m1-1)+sum(m1) !Making cumulative distribution
              ENDDO
            ENDDO
          ENDDO
          IF(cum(m1).EQ.0)THEN
            eii=0
            e=0
            GO TO 23                      !No place to decay: isomer
          ENDIF

C  Normalizing the distribution and compare with random number
          x=rand(0)
          DO m=0,m1
            IF(x.LT.cum(m)/cum(m1)) GO TO 20
          ENDDO
          m=m1
20        CONTINUE

C Decay in excitation energy - in order to find Exii
          DO e=0,eii
            cum(e)=cum(e-1)+Prob(e,m)
          ENDDO

C  Normalizing the distribution and compare with random number
          x=rand(0)
          DO e=0,eii
            IF(x.LT.cum(e)/cum(eii))GO TO 22
          ENDDO
          e=eii
22        CONTINUE                  !Final energy found as e
                              
          igen=igen+1               !Number of gammas > 0

23        CONTINUE

          IF(igen.GT.15)THEN
             write(6,*)
            WRITE(6,*)'> 15 gamma-transitions found in one cascade! (not incremented)'
            GO TO 200
          ENDIF
 
C Incrementing total gamma-spectrum
          igam=eii-e                !Gamma energy = Ex(initial)-Ex(final)
          Gam(igam)=Gam(igam)+xw
          Gamex(igam,(ei/iCompEx))=Gamex(igam,(ei/iCompEx))+xw
          ien(igen)=igam
          Gen(igam,igen)=Gen(igam,igen)+xw

          IF(igen.eq.1)THEN
            Genex(igam,(ei/iCompEx))=Genex(igam,(ei/iCompEx))+xw
          ENDIF

          IF(igam.GE.mCutGam)THEN
            igem=igem+1             !Number of gammas > Ecut
          ENDIF
         
          eii=e                     !New initial Ex = old final Ex

          IF(eii.GT.0)THEN          !Not yet reached ground state, simulate new gamma
            Jii = jv(m)             !Setting up new quantum numbers for next transition
            Pii = pv(m)
            Kii = kv(m)
c          WRITE(6,24) eii,Jii,Pii,Kii
c24        FORMAT(4I5)
            GO TO 999
          ENDIF

C Making distribution of multiplicity
          Pm(ei,igem)=Pm(ei,igem)+xw

C Making gam*gam matrix
          IF(igen.GE.2)THEN
            DO ig1=1,igen-1
              iglow=ig1+1
              DO ig2=iglow,igen
                ie1=ien(ig1)/iCompEg
                ie2=ien(ig2)/iCompEg
                IF (ie1.LE.511.AND.ie2.LE.511) THEN 
                  GamGam(ie1,ie2)=GamGam(ie1,ie2)+xw
                ENDIF
              ENDDO
            ENDDO
          ENDIF

C Making mul=2 gammma spectrum
c          IF(igen.EQ.2)THEN
            ie1=ien(1)
            ie2=ien(2)
            IF(ie1+ie2.GE.ei)THEN
              Gam2(ie1)=Gam2(ie1)+xw
              Gam2(ie2)=Gam2(ie2)+xw
            ENDIF
c          ENDIF    

C Incrementing average total multiplicities
          genTot=genTot+FLOAT(igen)*xw
          gemTot=gemTot+FLOAT(igem)*xw
          xloops=xloops+xw

        ENDDO                            !Cascade loop end (DO nc=1,ncas) 
200     CONTINUE

      ENDDO                              !Energy loop end  (DO ei=iemin,iemax)
      RETURN
      END


      SUBROUTINE ARRAY(xlambda)
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot
      COMMON/Arrays/Egn(0:4095),HinderK(-12:12,1:2),ElMl(-1:2)
      INTEGER Multipol
      DO i=0,4095
        Egam=FLOAT(i)*ax1
        Egn(i)=Egam**xen
c         IF(i.GT.150.AND.i.LT.200)Egn(i)=Egn(i)*2.0  !hoho
c         IF(i.GT.300.AND.i.LT.350)Egn(i)=Egn(i)*2.0  !hoho
      ENDDO
c      DO i=0,400
c        ii=Egn(i)/1.0E+7
c        WRITE(6,*)ii
c      ENDDO                                !Denne ogsaa
      DO Multipol=1,2
        DO k=-12,12
        HinderK(k,Multipol)=1.
        dK1=FLOAT(ABS(k)-Multipol)
        IF(dK1.GT.0.0)HinderK(k,Multipol)=1./(xlambda**dK1)
c            write(6,*)k,HinderK(k,Multipol)
        ENDDO
      ENDDO
      ElMl(-1)=1.0
      ElMl( 0)=1.0
      ElMl( 1)=1.0
      ElMl( 2)=0.01
      RETURN
      END


      SUBROUTINE Fermi(J)
C     Subroutine to calculate the Fermi level density Rho for a given level
C     density parameter a, one parity and one spin J.
C     From EcLow -> EcHigh we use the constant level density formula (Gabriel)
C     From EcHigh and higher we use backshiftet Fermi gas formula
C     The excitation energy region (keV) is 0 - iemax*ax1.
      COMMON/Fermigas/Amass,Arot,Epair,EcLow,EcHigh,FermiRho(0:4095),iLow,Cfg
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot
      
C Initializing
      DO ie=0,iemax
        FermiRho(ie)=0.
      ENDDO

      xJ    =FLOAT(J)
      Eyrast=Arot*(xJ*(xJ+1.))
      iLow  =(EcLow +Eyrast)/ax1 + 0.5
      iHigh =(EcHigh+Eyrast)/ax1 + 0.5
      Rigid=(0.0137/1000.)*(Amass**(5./3.))                     !in 1/keV
      C=0.5*SQRT(a)*(2.*xJ+1.)/(24.*SQRT(2.)*(Rigid**(3./2.)))  !one parity
      DO ie=iHigh,iemax                                         !Fermi gas above iHigh
        Ex=FLOAT(ie)*ax1 
        Eyrast=Arot*(xJ*(xJ+1.))
        U=Ex-Eyrast-Epair
        IF(U.GT.0.)THEN
          Temp=(1.+SQRT(1.+4.*a*U))/(2.*a)
          FermiRho(ie)=Cfg*C*(EXP(2.*SQRT(a*U)))/((U+Temp)**2)  !Cfg from FixFermi
        ENDIF
      ENDDO
      rho1=FermiRho(iHigh)

C Calculating constant temperature formula Rho=const.*exp(Ex/T)
C Finding first spindistribution according to Fermi gas (in order to match)
C Fermigas level density for a single spin and single parity (i.e. the 4+ state)

      Ematch=FLOAT(iHigh)*ax1
      Cct=FermiRho(iHigh)/(0.5*0.0034*EXP(Ematch/570.)) !Only 1 parity: 0.5
      DO ie=iLow,iHigh                                  !See Gabriel page 10, but 0.0034
        Ex =FLOAT(ie)*ax1                               !is more accurate than 0.0043
        FermiRho(ie)=Cct*0.5*0.0034*EXP(Ex/570.)        !Sum of Cct should be appr. 1
      ENDDO                                           
      rho2=FermiRho(iHigh)

      IF(J.EQ.0)write(6,*)'  Spin   Ematch     Rho(fg)     Rho(ct)      C(ct)'
      write(6,10)J,Ematch,rho1,rho2,Cct
10    FORMAT(I6,F10.1,3F12.6)
      sum1=sum1+rho1
      sum2=sum2+rho2
      sum3=sum3+Cct
      IF(J.EQ.12)write(6,11)sum1,sum2,sum3
11    FORMAT('Totals          ',3F12.6)
      RETURN
      END


      SUBROUTINE FixFermi(Jt,Bn,Dave)
C     Determines Cfg so that Rho has right density
C     at the neutron binding energy. You populate spin = targetspin +/- 0.5.
      COMMON/Fermigas/Amass,Arot,Epair,EcLow,EcHigh,FermiRho(0:4095),iLow,Cfg
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot
      REAL Jt

      Rigid=(0.0137/1000.)*(Amass**(5./3.))                      !in 1/keV
      sum=0.
      xJ=Jt+0.5
      DO i=1,2
        Eyrast=Arot*(xJ*(xJ+1.))
        C=0.5*SQRT(a)*(2.*xJ+1.)/(24.*SQRT(2.)*(Rigid**(3./2.))) !one parity
        U=Bn-Eyrast-Epair
        Temp=(1.+SQRT(1.+4.*a*U))/(2.*a)
        sum =sum+C*(EXP(2.*SQRT(a*U)))/((U+Temp)**2)
        xJ=Jt-0.5
      ENDDO
      Cfg=1./(Dave*sum)
      RETURN
      END


      SUBROUTINE DisLevels(NucName)
C     Reading discrete levels from file
      COMMON/Density/TotRho(0:4095,0:12,0:1,0:12)
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot

      INTEGER p
      CHARACTER NucName*5,STR*20,infile*15,FILNAM*15,FILNAM2*255

      infile='lev-'//NucName
      CALL LENGDE(infile,LIN)
      FILNAM=infile(1:LIN)

C First looking at own home-directory
      OPEN(11,FILE=FILNAM,STATUS='OLD',ERR=51)
      GO TO 50
51    WRITE(6,*)'Did not find ',FILNAM,' in your directory'
      call makepath("UIO_APPLICATIONS","prog/lib/lev-162Dy",filnam2)
      WRITE(6,*)'Taking instead '//filnam2
      OPEN(11,FILE=filnam2,STATUS='OLD',ERR=98)
50    CONTINUE

      READ(11,'(A)',ERR=99) STR
10    IF (STR(2:2).EQ.'+') p=0
      IF (STR(2:2).EQ.'-') p=1
      READ(11,'(A)',ERR=99) STR
20    READ(STR(3:3),'(I1)',ERR=99) k
      READ(11,'(A)',ERR=99) STR
      DO WHILE (STR(1:1).EQ.'L')
        READ(STR(2:3),'(I2)',ERR=99) j
        READ(STR(5:15),'(F11.6)',ERR=99) Ex
        ie=(Ex*1000./ax1)+0.5
        IF(ie.LE.4095)TotRho(ie,j,p,k)=TotRho(ie,j,p,k)+1.0
c         write(6,*)Ex,j,p,k
        READ(11,'(A)',ERR=99) STR
      ENDDO
      IF (STR(1:1).EQ.'K') GOTO 20
      IF (STR(1:1).EQ.'P') GOTO 10
      CLOSE(11)
      RETURN
98    PRINT 30, FILNAM
30    FORMAT('Error during opening file: ',A15)
      STOP
99    PRINT 40, STR
40    FORMAT('Error during reading string: ',A20)
      STOP
      END


      SUBROUTINE DK(Kmax)
      DIMENSION sum(0:12)
      COMMON/Initial/DisK(0:12,0:12),DisJP(0:12,0:1)
      REAL K0
      K0     =4.
      K_distr=0
      WRITE(6,3)K_distr
3     FORMAT('K -distr.: Gauss(0), Flat(1) or Manual(2)           <',I1,'>:',$)
      CALL READI(5,K_distr)
      IF(K_distr.EQ.0)THEN
        WRITE(6,4)K0
4       FORMAT('Standard deviation K0 of K-distribution           <',F3.1,'>:',$)
        CALL READF(5,K0)
      ENDIF
      DO J=0,12
        sum(J)=0.
        DO K=0,MIN0(j,Kmax)
          IF(K_distr.EQ.0)THEN
            xK=FLOAT(K)
            DisK(J,K)=EXP(-(xK*xK)/(2.*K0*K0))
            sum(J)=sum(J)+DisK(J,K)
          ENDIF
          IF(K_distr.EQ.1)THEN
            DisK(J,K)=1.
            sum(J)=sum(J)+DisK(J,K)
          ENDIF
          IF(K_distr.EQ.2)THEN
            WRITE(6,1)J,K,DisK(J,K)
1           FORMAT('Weight for (J,K) = (',I2,',',I2,')  <',F6.3,'>:',$)
            CALL READF(5,DisK(J,K))
            sum(J)=sum(J)+DisK(J,K)
          ENDIF
        ENDDO
      ENDDO
C Normalizing
      DO J=0,12
        DO K=0,MIN0(j,Kmax)
          DisK(J,K)=DisK(J,K)/sum(J)
        ENDDO
      ENDDO
      RETURN
      END


      SUBROUTINE DJP
      COMMON/Initial/DisK(0:12,0:12),DisJP(0:12,0:1)
      DIMENSION DisNew(0:12,0:1)
      INTEGER P
      sum     =0.
      JP_distr=0
      Jshift  =1
      WRITE(6,2)JP_distr
2     FORMAT('JP-distr.: Default(0), Move default(1) or Manual(2) <',I1,'>:',$)
      CALL READI(5,JP_distr)

      IF(JP_distr.EQ.0)THEN
        DisJP(0 ,0)=0.01469
        DisJP(0 ,1)=0.01469
        DisJP(1 ,0)=0.04014
        DisJP(1 ,1)=0.04014
        DisJP(2 ,0)=0.06560
        DisJP(2 ,1)=0.06560
        DisJP(3 ,0)=0.07010
        DisJP(3 ,1)=0.07010
        DisJP(4 ,0)=0.07460
        DisJP(4 ,1)=0.07460
        DisJP(5 ,0)=0.07031     !Initial spin-distribution
        DisJP(5 ,1)=0.07031
        DisJP(6 ,0)=0.06603
        DisJP(6 ,1)=0.06603
        DisJP(7 ,0)=0.04590
        DisJP(7 ,1)=0.04590
        DisJP(8 ,0)=0.02573
        DisJP(8 ,1)=0.02573
        DisJP(9 ,0)=0.01567
        DisJP(9 ,1)=0.01567
        DisJP(10,0)=0.00561
        DisJP(10,1)=0.00561
        DisJP(11,0)=0.00375
        DisJP(11,1)=0.00375
        DisJP(12,0)=0.00189
        DisJP(12,1)=0.00189
        sum        =1.00004
      ENDIF

      IF(JP_distr.EQ.1)THEN
        WRITE(6,3)Jshift
3       FORMAT('Shift the spin distribution with an integer spin value',/,
     +  '(shifts is +/- for shift to the right/left)          <',I1,'>:',$)
        CALL READI(5,Jshift)
        DO J=0,12
          DO P=0,1
            DisNew(J,P)=0.
            Jnew=J-Jshift
            IF(Jnew.GE.0.AND.Jnew.LE.12)DisNew(J,P)=DisJP(Jnew,P)
          ENDDO
        ENDDO
        DO J=0,12
          DO P=0,1
            DisJP(J,P)=DisNew(J,P)
            sum=sum+DisJP(J,P)
          ENDDO
        ENDDO
      ENDIF
      IF(JP_distr.EQ.2)THEN
        DO J=0,12
          DO P=0,1
            WRITE(6,1)J,P,DisJP(J,P)
1           FORMAT('Weight for (J,P) = (',I2,',',I2,')  <',F6.3,'>:',$)
            CALL READF(5,DisJP(J,P))
            sum=sum+DisJP(J,P)
          ENDDO
        ENDDO
      ENDIF

C Normalizing
      DO J=0,12
        DO P=0,1
          DisJP(J,P)=DisJP(J,P)/sum
        ENDDO
      ENDDO
      RETURN
      END

      
      SUBROUTINE TESTCAS(Multiple)
C Routine to compensate for fewer configurations at low excitation energy
C The vector v(i) is proportional to number of configurations in channel i
      COMMON/Density/TotRho(0:4095,0:12,0:1,0:12)
      COMMON/Parameters/a,xen,mCutGam,ax1,iCompEg,iCompEx,iemin,iemax,Kmax,nCasTot
      COMMON/Cascades/Gam(0:4095),Gam2(0:4095),Gamex(0:4095,0:511),Gen(0:4095,0:15),Genex(0:4095,0:511)
      COMMON/Cas/Pm(0:4095,0:15),GamGam(0:511,0:511),ww(0:12,0:1,0:12)
      COMMON/Control/ncas,nTotal,nStep,genTot,gemTot,xloops,NormCas(0:4095)

      INTEGER ei,p,iConfig(0:4095)
      REAL x,v(0:4095),vc(0:4095)

C Initialization
      DO i=0,4095
        v(i)= 0.
        vc(i)=0.
        iConfig(i)=0.
      ENDDO        
      nCasTot=0           

C Number of configurations with a1=ax1*IcompEx
      DO j=0,12
        DO p=0,1
          DO k=0,MIN0(j,Kmax)
            wf=ww(j,p,k)
            DO ei=iemin,iemax
              x=FLOAT(ei)*wf*TotRho(ei,j,p,k)
              IF(x.GT.0.) THEN
                iConfig(ei)=iConfig(ei)+1              
                vc(ei/iCompEx)=vc(ei/iCompEx)+wf
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO

C Number of configurations with a1=ax1
      DO ei=iemin,iemax,iCompEx
        DO i=1,iCompEx
          v(ei+i-1)=vc(ei/iCompEx)
        ENDDO
      ENDDO
      DO ei=iemin,iemax
        x=v(ei)
        IF(x.GT.0.)THEN
          NormCas(ei)=INT((FLOAT(Multiple)*v(iemax-iCompEx)/x)+0.5)
        ELSE
          NormCas(ei)=0
        ENDIF
        nCasTot=nCasTot+iConfig(ei)*NormCas(ei)
      ENDDO
     
      RETURN
      END
