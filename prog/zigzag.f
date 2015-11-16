      PROGRAM ZIGZAG
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      REAL Spec(0:8191)
      REAL Calib(6)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER APP*4

      DIMENSION wait(0:2047),save(0:2047,0:5)
      DIMENSION chi(0:5),cexp(0:5),cteo(0:5),wexp(0:5),wteo(0:5)
      DIMENSION fteo(0:5),sigexp(0:2047)
      CHARACTER name*40,filnam*40
      CHARACTER appx(0:5)*1,ans*1
      INTEGER dimr,dimw
      REAL Myrast
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO2/ Pteo(0:2047,0:5),Pexp(0:2047,0:5),incl(0:5)
      COMMON /CO3/Rho(-100:10000,0:5),Temp(-100:10000,0:5)
      COMMON /CO4/Myrast(0:5),Egmin
      COMMON /CO5/Eyrast(0:5),EEyrast(0:5)
      COMMON /CO6/an,na,a(5),f(5),xSpin(11),Si(11)
      iread=5
      iwrite=6
      ifil1=20
      ifil2=21

 120  CONTINUE

      WRITE(6,*)'       ******************************************'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *               Z I G                    *'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *    Program to calculate the feeding    *'
      WRITE(6,*)'       *     regions for various xn-channels    *'
      WRITE(6,*)'       *     in the (3He;Alpha,xn) reaction.    *'
      WRITE(6,*)'       *  The yrast line, level densities, etc. *'
      WRITE(6,*)'       *  are taken for Dy162,161,..157. Please,*'
      WRITE(6,*)'       *  change rot.par.,binding energies,...  *'
      WRITE(6,*)'       * in the DATA-statement in SUBROUTINE    *'
      WRITE(6,*)'       *     DENSITY for your specific case.    *'
      WRITE(6,*)'       *                                        *'
      WRITE(6,*)'       *             10.3.1994/MG               *'
      WRITE(6,*)'       ******************************************'



C                            INITIALIZATION

      appx(0)='0'
      appx(1)='1'
      appx(2)='2'
      appx(3)='3'
      appx(4)='4'
      appx(5)='5'


      nnuclei=5                     !number of nuclei
      dimr   =400                   !dimension of spectra to read
      dimw   =400                   !dimension of spectra to write


      a0      =45780                !calibration (keV) for alfa-spectrum
      a1      =-120.
      a2      = 0.00
      Edel    = 400.                !energy-step
      aSpin   = 6.78                !average spin
      an      =0.016                !normal level density parameter (1/keV)
      Multstat= 0                   !if P(=0) or M(=1) calculation
      Egmin   = 430.                !Gam.mult for Eg>0.43 keV

      incl(0)=0                     !tells what xn-channels should be
      incl(1)=0                     !included
      incl(2)=0                     !yes=1, no=0
      incl(3)=0
      incl(4)=0
      incl(5)=0
      icomp  =0                     !comparison with exp.-data or not

      nspin =0                      !number of spins 0,2,4,6.. to calc.
      Spin  =6.
      Si(1) =0.0                    !feeding to 0+
      Si(2) =0.0                    !feeding to 2+, etc....
      Si(3) =0.273
      Si(4) =0.359
      Si(5) =0.176
      Si(6) =0.121
      Si(7) =0.046
      Si(8) =0.018
      Si(9) =0.007
      Si(10)=0.0
      Si(11)=0.0
      xSpin(1) =0
      xSpin(2) =2
      xSpin(3) =4
      xSpin(4) =6
      xSpin(5) =8
      xSpin(6) =10
      xSpin(7) =12
      xSpin(8) =14
      xSpin(9) =16
      xSpin(10)=18
      xSpin(11)=20

      na=0
      f(1)=.8                       !fraction which gives normal temp.
      f(2)=.2                       !fraction which gives higher temp. etc.
      f(3)=.0
      f(4)=.0
      f(5)=.0
      a(1)=.016                     !level density param. normal
      a(2)=.006                     !level density param. higher temp etc.
      a(3)=.000
      a(4)=.000
      a(5)=.000 
                                    
      
      E0min=14820.                  !Ex-region for calculation
      E0max=40380.
     

      WRITE(6,5)Multstat
  5   FORMAT('Calculate prob. P (0) or multipl. M (1)',
     1 '    <',I1,'>:',$)
      CALL READI(5,Multstat)

      IF(Multstat.EQ.1)THEN
      WRITE(6,17)Egmin
 17   FORMAT('Lower limit on gammas excepted (keV)  <',F6.0,'>:',$)
      CALL READF(5,Egmin)
      ENDIF

      WRITE(6,9)Edel
  9   FORMAT('Energy step in calculation     (keV)  <',F6.1,'>:',$)
      CALL READF(5,Edel)

      ans='n'
      WRITE(6,15)ans
 15   FORMAT('Do you want to include many spins (y/n) <',A,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
        sum1=0.
        sum2=0.
        nspin=11
        WRITE(6,*)'Give all feedings from spin I= 0 to 20'
        DO i=1,nspin
          ispin=xSpin(i)
          WRITE(6,16)ispin,Si(i)
 16       FORMAT('Feeding of spin',I2,'+ <',F6.3,'>:',$)
          CALL READF(5,Si(i))
          sum1=sum1+Si(i)
          sum2=sum2+Si(i)*xSpin(i)
        ENDDO
        WRITE(6,31)sum1,sum2 
 31     FORMAT('Sum of S(i)=',F6.3,' and sum of S(i)*I(i)=',F6.2)
        aSpin=sum2
      ELSE
        nspin=1
        xSpin(1)=aSpin
        Si(1)=1.
        WRITE(6,23)xSpin(1)
 23     FORMAT('Average spin <',F4.1,'>:',$)
        CALL READF(5,xSpin(1))
      ENDIF

      DO j=0,nnuclei-1
        ans='n'
        IF(j.EQ.2)ans='y'
        WRITE(6,14)j,ans
 14     FORMAT('Include ',I1,'-n channel (y/n)<',A,'>:',$)
        CALL READA1(5,ans)
        IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
          incl(j)=1
          jmax=j
        ENDIF
      ENDDO

C Comparing with exp.-spectra for 0-n, 1-n, 2-n,..,5-n channel
      ans='y'
      WRITE(6,1)ans
  1   FORMAT('Compare theory with exp.-spectra (y/n) <',A,'>:',$)
      CALL READA1(5,ans)

      IF(ans.EQ.'Y'.OR.ans.EQ.'y') THEN
        DO i=0,2047
          DO j=0,nnuclei-1
            Pexp(i,j)=0
          ENDDO
        ENDDO

        icomp=1
        WRITE(6,2)dimr
  2     FORMAT('Dimension of experimental spectra <',I4,'>:',$)
        CALL READI(5,dimr)

        DO j=0,nnuclei-1
          IF(incl(j).EQ.1)THEN
            name='expxn- '
            CALL LENGDE(name,len)
            filnam=name(1:len)//appx(j)
            WRITE(6,3)j,filnam
  3         FORMAT('Name of spectrum from ',I1,'-n channel<',A7,'>:',$)
            CALL READA(5,filnam)
            DO k=0,8191
              Spec(k)=0
            ENDDO
            OPEN(ifil1,FILE=filnam,ACCESS='SEQUENTIAL',ERR=99)
            CALL norr1dim(ifil1, comment, dimr, Spec, Calib)
            CLOSE(ifil1)
            DO i=1,dimr
              Pexp(i,j)=Spec(i)
            ENDDO
          ENDIF
        ENDDO
      ENDIF

C********************
C  Main LOOP
C********************
 110  CONTINUE

      WRITE(6,*)' Excitation region to explore:'
      WRITE(6,11)E0min
 11   FORMAT('Lower excitation Emin <',F8.1,'>:',$)
      CALL READF(5,E0min)
      WRITE(6,12)E0max
 12   FORMAT('Upper excitation Emax <',F8.1,'>:',$)
      CALL READF(5,E0max)

      WRITE(6,18)an
 18   FORMAT('Normal level density parameter a(1/keV)<',F7.4,'>:',$)
      CALL READF(5,an)

C Using dynamic source model
      ans='y'
      WRITE(6,43)ans
 43   FORMAT(/'Using dynamic source model for 1n (y/n) <',A,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
        na=5
        sum1=0.
        sum2=0.
        DO i=1,na
          WRITE(6,19)i,a(i)
 19     FORMAT('Level density for source ',I1,' a(1/keV)<',F7.4,'>:',$)
          CALL READF(5,a(i))
          IF(i.EQ.2)f(2)=1-f(1)
          WRITE(6,20)i,f(i)
 20       FORMAT('Fraction of total source ',I1,' f<',F6.4,'>:',$)
          CALL READF(5,f(i))
          sum1=sum1+f(i)
          sum2=sum2+f(i)*a(i)
        ENDDO
        WRITE(6,30)sum1,sum2 
 30     FORMAT('Sum of f(i)=',F6.3,' and sum of f(i)*a(i)=',F6.4)
      ELSE
        na=1
        a(1)=an
        f(1)=1. 
      ENDIF
      
      DO i=0,2047
        DO j=0,nnuclei-1
          save(i,j)=0
          Pteo(i,j)=0
        ENDDO
      ENDDO

C ********************

      DO i1=1,na
        av=a(i1)
        DO i2=1,nspin
          Spin=xSpin(i2)
          alpha=f(i1)*Si(i2)
          IF(alpha.GT.0)THEN
            CALL Density(an,av,E0min,E0max)
            CALL INTEGRATE(E0min,E0max)
            DO j=0,nnuclei-1
              DO i=0,2047
                save(i,j)=save(i,j)+alpha*Pteo(i,j)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO

C*********************
      DO i=0,2047
        DO j=0,nnuclei-1
          Pteo(i,j)=save(i,j)
        ENDDO
      ENDDO



 100  CONTINUE
Cccccccccccccccccccccccccccccc
C TRANSFORMING SPECTRA FROM
C Pteo(Ex) to Pteo(Ealpha)
Cccccccccccccccccccccccccccccc

      DO j=0,nnuclei-1
        chi(j) =0
        cexp(j)=0
        cteo(j)=0
        wexp(j)=0
        wteo(j)=0
        fteo(j)=0
      ENDDO

      itrans=0
      ans='y'
      WRITE(6,21)ans
 21   FORMAT('Transform to calibration for exp.-spectra (y/n)<',
     +A,'>:',$)

      CALL READA1(5,ans)
      IF(ans.EQ.'y'.OR.ans.EQ.'Y')THEN
        itrans=1
        WRITE(6,6)a0
  6     FORMAT('a0 of particle spectrum (keV)       <',F8.1,'>:',$)
        CALL READF(5,a0)

        WRITE(6,7)a1
  7     FORMAT('a1 of particle spectrum (keV/ch)    <',F8.1,'>:',$)
        CALL READF(5,a1)

        WRITE(6,8)a2
  8     FORMAT('a2 of particle spectrum (keV/ch*ch) <',F8.1,'>:',$)
        CALL READF(5,a2)

C Giving calibration to spectra
        DO i=1,6
          Calib(i)=0.
        ENDDO
        Calib(1)=a0
        Calib(2)=a1
        Calib(3)=a2

        DO j=0,nnuclei-1
          DO i=0,dimr-1
            wait(i)=0.
            Ex=a0+a1*i+a2*i*i           !energy in new spectrum
C Calibration in theoretical spectrum is b0=0,b1=Edel,b2=0
            x =Ex/Edel
            IF(x.GT.-1.)THEN
              i1=x
              i2=i1+1
              IF(i1.GE.0.AND.i2.LE.2047)THEN
                wait(i)=Pteo(i1,j)+(Pteo(i2,j)-Pteo(i1,j))*(x-i1)/1.
              ENDIF
              IF(x.LT.0.)THEN
                wait(i)=Pteo(i2,j)*(x+1.)/1.
              ENDIF
            ENDIF
          ENDDO                         !finished with calib. transform

          DO i=0,2047
            Pteo(i,j)=wait(i)
          ENDDO
        ENDDO
        CALL SMOOTH(nnuclei)            !folding with the particle res.
      ENDIF

C Finding centroides and standard dev. (sigma)
      DO j=0,nnuclei-1
        steo0=0
        steo1=0
        steo2=0
        sexp0=0
        sexp1=0
        sexp2=0
        iTimeTeo=0
        imin=((E0min-a0)/a1)+0.5
        imax=((E0max-a0)/a1)+0.5
        ivent=imax
        IF(imin.GT.imax)THEN
          imax=imin
          imin=ivent
        ENDIF
        IF(imin.LT.0)imin=0
        IF(imax.GT.2047)imax=2047
        iteo1=imin
        iteo2=imax
        DO i=imin,imax
          steo0=steo0+Pteo(i,j)
          steo1=steo1+Pteo(i,j)*i
          steo2=steo2+Pteo(i,j)*i*i
          IF(Pexp(i,j).GE.0)THEN
            sexp0=sexp0+Pexp(i,j)
            sexp1=sexp1+Pexp(i,j)*i
            sexp2=sexp2+Pexp(i,j)*i*i
          ENDIF
          IF(Pteo(i,j).GT.0.5.AND.iTimeTeo.EQ.0)THEN
            Iteo1=i
            iTimeTeo=1
          ENDIF
          IF(Pteo(i,j).LT.0.5.AND.iTimeTeo.EQ.1)THEN
            Iteo2=i-1
            iTimeTeo=2
          ENDIF
        ENDDO

        disp=Edel
        IF(itrans.EQ.1)disp=ABS(a1)
        IF(steo0.GT.0)THEN
          cteo(j)=steo1/steo0
          wteo(j)=SQRT((steo2/steo0)-(steo1/steo0)**2.)
          fteo(j)=(Iteo2-Iteo1)*disp
        ENDIF
        IF(sexp0.GT.0)THEN
          cexp(j)=sexp1/sexp0
          wexp(j)=SQRT((sexp2/sexp0)-(sexp1/sexp0)**2.)
        ENDIF
      ENDDO

C Finding chisquare between theory and experiment
      IF(icomp*itrans.EQ.1)THEN
        DO j=0,nnuclei-1
          IF(incl(j).EQ.1)THEN

C Finding sigma for experimental data. Averaging over 500 keV
            istep=500./ABS(a1)+0.5
            IF(istep.LT.2)istep=2
            I1=imin-istep/2
            I2=imax-istep/2
            IF(I1.LT.0   )I1=0
            IF(I2.GT.2047)I2=2047
            DO i=0,2047
              sigexp(i)=0.
            ENDDO
            DO i=I1,I2
              imid=i+istep/2
              IF(i+istep.LT.2047)THEN
                sum=0.
                DO k=0,istep-1
                  sum=sum+Pexp(i+k,j)
                ENDDO
                average=sum/FLOAT(istep)
                sum=0.
                DO k=0,istep-1
                  sum=sum+ABS(Pexp(i+k,j)-average)
                ENDDO
                sigexp(imid)=sum/istep
              ENDIF
            ENDDO

C Testing if any sigexp=0. Finding average sigexp and substitute
            sum=0.
            k  =0
            DO i=imin,imax
              IF(sigexp(i).GT.0)THEN
                k=k+1
                sum=sum+sigexp(i)
              ENDIF
              IF(k.LT.1)k=1
              avesig=sum/FLOAT(k)
            ENDDO
            DO i=imin,imax
              IF(sigexp(i).LT.0.000001)sigexp(i)=avesig
            ENDDO

            x2=0.
            nopoints=0
            DO i= imin,imax
              sig2=sigexp(i)**2.
              IF(sig2.LT.0.0000001)sig2=1.
              x2=x2+((ABS(1000.*Pteo(i,j)-Pexp(i,j)))**2.)/sig2
              nopoints=nopoints+1
            ENDDO
            chi(j)=x2/(nopoints-1-2)       !roughly 2 free parameters
          ENDIF
        ENDDO
      ENDIF

      WRITE(6,121)
      IF(na.GT.1)THEN
        WRITE(6,125)(a(i),i=1,na)
        WRITE(6,126)(f(i),i=1,na)
 125    FORMAT('Used a(i): ',5F7.4)
 126    FORMAT('Used f(i): ',5F7.3)
      ENDIF

      IF(nspin.GT.1)THEN
        WRITE(6,127)(xSpin(i),i=1,nspin)
        WRITE(6,128)(Si(i),i=1,nspin)
 127    FORMAT('Used I(i): ',11F7.1)
 128    FORMAT('Used S(i): ',11F7.3)
      ENDIF

      IF(Multstat.EQ.1)THEN
        WRITE(6,119)Egmin
 119    FORMAT(' Multiplicity for Egamma > ',F6.3)
      ENDIF

      WRITE(6,122)Edel,Bn(0),Bn(1),Bn(2),Bn(3),Bn(4),Bn(5)
      WRITE(6,123)E0min,E0max,aSpin,an
      WRITE(6,124)a0,a1,a2,AX,BX
 121  FORMAT('Results obtained with the parameters:')
 122  FORMAT(' Edel=',F5.1,', Bn=',6F8.0)
 123  FORMAT(' Exc.-region=',F8.1,'-',F8.1, 
     1' aver. spin=',F5.2,' normal a=',F7.4)
 124  FORMAT(' a0,a1,a2=',3F8.1,',  FWHM-param.=',2F5.2)
      WRITE(6,*)'--------------------------------------------------'
      WRITE(6,*)'  xn    chisqr.  centroid (ch)  st.deviation (ch)'
      WRITE(6,*)'channel           exp.   teo.      exp.    teo '
      WRITE(6,*)'--------------------------------------------------'
      DO j=0,nnuclei-1
        WRITE(6,113)j,chi(j),cexp(j),cteo(j),wexp(j),wteo(j),fteo(j)
 113    FORMAT(I5,F9.2,' ',4F8.1,'   FWHMteo = ',F8.1,' keV')
      ENDDO
      WRITE(6,*)'--------------------------------------------------'

 140  CONTINUE
      WRITE(6,*)' Change calibration and FWHM: .......(0)'
      WRITE(6,*)' New heavy calculation: .............(1)'
      WRITE(6,*)' Start all over: ....................(2)'
      WRITE(6,*)' Write spectra on disk: .............(3)'
      WRITE(6,*)' EXIT: ..............................(4)'
      ians=0
      WRITE(6,141)ians
 141  FORMAT('Give your anser <',I1,'>:',$)
      CALL READI(5,ians)
      IF(ians.EQ.0) THEN
        DO j=0,nnuclei-1
          DO i=0,2047
            Pteo(i,j)=save(i,j)
          ENDDO
        ENDDO
        GO TO 100
      ENDIF
      IF(ians.EQ.1) GO TO 110
      IF(ians.EQ.2) GO TO 120
      IF(ians.EQ.3) GO TO 130
      IF(ians.EQ.4) STOP
      GO TO 140

ccccccccccccccccccccccccccccc
C WRITING SPECTRA TO DISK
Cccccccccccccccccccccccccccccc

 130  CONTINUE
      DO j=0,nnuclei-1
        IF(incl(j).EQ.1)THEN
          name='teoxn-'
          CALL LENGDE(name,len)
          filnam=name(1:len)//appx(j)
          WRITE(6,131)j,filnam
  131     FORMAT('Name of output spectrum for the ',I1,'n channel<',A7,'>:',$)
          CALL READA(5,filnam)
          DO k=0,8191
            Spec(k)=0
          ENDDO
          DO i=1,dimw
            Spec(i)=Pteo(i,j)
          ENDDO
          comment='Feeding regions with ZIGZAG'
          OPEN(ifil2,FILE=filnam,ACCESS='SEQUENTIAL',ERR=999)
          CALL norr1dim(ifil2, comment, dimw, Spec, Calib)
          CLOSE(ifil2)
        ENDIF
      ENDDO

      GO TO 140
 999  WRITE(6,*)' Writting file error'
      GO TO 140
 99   WRITE(6,*)' No file access'
      GO TO 140
      END       


      FUNCTION Mult(Eii,ii)
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO4/Myrast(0:5),Egmin
      REAL Myrast
      REAL Mult
      Mult=1.
      IF(Multstat.NE.1)RETURN
      x=Eii/1000.          ! Units of MeV
      IF(x.GT.11.0)Eii=x                          !This is a parabola
      Mult=Myrast(ii)-0.470+0.7616*x-0.034426*x*x !taken from Dy163
      IF(Mult.LT.0.OR.x.LT.0.4)Mult=0
      END


      SUBROUTINE Density(an,av,E0min,E0max)
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO3/Rho(-100:10000,0:5),Temp(-100:10000,0:5)
      COMMON /CO4/Myrast(0:5),Egmin
      COMMON /CO5/Eyrast(0:5),EEyrast(0:5)
      REAL Myrast
      DIMENSION Amass(0:5),Arot(0:5),Eyrast0(0:5),K0(0:5)
      DIMENSION BandH0(0:5)
      DIMENSION Epair(0:5),LowDens(0:5)
      REAL K0, LowDens
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Subroutine to calculate level density Rho for a given level
C density parameter a, one parity and one spin Spin (given in COMMON).
C The present excitation energy region is determined by (E0min,E0max).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Amass:  Massnumber of the 6 nuclei
C Bn:     Neutron binding energies in keV
C Arot:   Rotational parameter (fitted between I=4 and 10)
C Eyrast0:Ex for the highest spin around the ground state
C K0:     The highest spin around the ground state
C         (0 for ee-, 4-5 for eo- and 7-8 for oo-systems)
C Epair:  Pairing energy. About 12.*/SQRT(Amass)*0, 1 or 2
C         for oo-, eo- and ee-systems, respectively
C BandH0: Ex for the lowest states (not yrast), which means:
C         "vibrational states" in ee-systems
C         "single-particle states" in eo-systems     
C         "two-part. st. not included in Fermi-gas for oo-system
C Lowdens:Density (1/keV) of the states described above for BandH0 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C The values below must be changed for another target than
C 163Dy. Please, then just comment away the following, and make
C a new data-set for your case.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA Amass  / 162.   ,161.   ,160.   ,159.   ,158.   ,157.  / 
      DATA Bn     / 0.0    ,8195.  ,14646. ,23228. ,30054. ,39115./
      DATA Arot   / 12.3   ,12.5   ,12.7   ,13.1   ,13.4   ,14.0  /
      DATA Eyrast0/ 0.0    ,80.    ,0.0    ,180.   ,0.0    ,150.  /
      DATA K0     / 0.0    ,4.0    ,0.0    ,4.0    ,0.0    ,4.0   /
      DATA Epair  / 1880.  ,940.   ,1900.  ,950.   ,1910.  ,960.  /
      DATA BandH0 / 1000.  ,0.     ,1000.  ,0.     ,1000.  ,0.    /
      DATA LowDens/ 0.004  ,0.004  ,0.004  ,0.004  ,0.004  ,0.004 /
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Initializing
      DO j=0,5
        Eyrast(j)=0.
        DO i=-100,10000
          Rho(i,j) =0.
          Temp(i,j)=0.
        ENDDO
      ENDDO
      imax=E0max/Edel+0.5
      IF(imax.GT.10000)THEN
        WRITE(6,*)'Too high E0max or too low dispersion Edel' 
        imax=10000
      ENDIF

C Calculates yrast line I(I+1)-K*(K+1) for all nuclei with I=Spin
      DO j=0,5
        Eyrast(j)=Eyrast0(j)+Arot(j)*(Spin*(Spin+1.)-K0(j)*(K0(j)+1))
        IF(Eyrast(j).LT.0.0)Eyrast(j)=0.
        Myrast(j)=0.
      ENDDO

C Checking in case of multiplicity calculation how many yrast
C transitions below I that has Egamma>Egmin. This gives Myrast.
      IF(Multstat.EQ.1)THEN
        DO j=0,5
          Eyrold=Eyrast(j)
          Spinx=Spin-2.
          DO WHILE (Spinx.GT.0)
            Eyrnew=Eyrast0(j)+Arot(j)*
     +             (Spinx*(Spinx+1.)-K0(j)*(K0(j)+1))
            IF(Eyrnew.LT.0.0)Eyrnew=0.
            IF(Eyrold-Eyrnew.GT.Egmin)Myrast(j)=Myrast(j)+1.
            Spinx=Spinx-2.
            Eyrold=Eyrnew
          ENDDO
        ENDDO
      ENDIF

C Calculating Rho=Yrast+Vibr.+Fermi and Temperatur in Fermi-gas       
C The formulas are for a single spin and single parity (i.e. 4+ states)
      DO j=0,5
        a=an
c        IF(j.EQ.1)a=av           !variable a for 1n nucleus, only 
        a=av                      !variable a for all nuclei 
        Rigid=(0.0137/1000.)*(Amass(j)**(5./3.))              !in 1/keV
        Arig=1./(2.*Rigid)                                    !in keV
        C=SQRT(a)*(2.*Spin+1.)/(24.*SQRT(2.)*(Rigid**(3./2.)))!both parities
        C=C/2.                                                !one parity
        BandH=BandH0(j)+Eyrast(j)
c       write(6,*)'Arig,C,BandH,BandH0(j)=',Arig,C,BandH,BandH0(j)
        DO i=0,imax
          Ex=FLOAT(i)*Edel 
          U=Ex-Eyrast(j)-Epair(j)
C "Yrast state:"
          IF(Eyrast(j).LT.Ex+Edel/2..AND.
     1    Eyrast(j).GE.Ex-Edel/2.)Rho(i,j)=Rho(i,j)+1.
C "Vibrational states:"
          IF(Ex.GE.BandH)Rho(i,j)=Rho(i,j)+LowDens(j)*Edel
C "Fermi gas:"
          IF(U.GE.200.)THEN
            Temp(i,j)=(1.+SQRT(1.+4.*a*U))/(2.*a)
            Rho(i,j)=Rho(i,j)+C*(EXP(2.*SQRT(a*U)))*Edel/
     1      ((U+Temp(i,j))**2)
          ENDIF   
        ENDDO
      ENDDO

C Finding Eyrast with the resolution used (Edel). This
C quantity is called EEyrast and is a multiple of Edel
      DO j=0,5
        DO i=-100,10000
          IF(Rho(i,j).GT.0.0000001)THEN
            EEyrast(j)=i*Edel
            GO TO 887
          ENDIF
        ENDDO
  887   CONTINUE
      ENDDO

C Writing out results of Rho etc.
      iWriteRho=1
      IF(iWriteRho.EQ.1)THEN
        maks=E0max/Edel
        IF(maks.GT.20)maks=20
        WRITE(6,43)maks,Rigid,Arig,Spin,a
 43     FORMAT(/'First ',I2,' energies: Rigid= ',F6.4,' 1/keV,',
     +  ' Arig= ',F4.1,' keV, Spin= ',F4.1,' and a= ',F6.4,' 1/keV') 
        WRITE(6,40)
 40     FORMAT(' Ex(keV)  Rho(0)/MeV    Temp(0)(keV)',
     +          '    Rho(1)/MeV    Temp(1)(keV)')
        DO i=0,maks
          alfa=1000./Edel               ! to give number of states/MeV
          WRITE(6,41)i*Edel,Rho(i,0)*alfa,Temp(i,0),
     +Rho(i,1)*alfa,Temp(i,1)
 41       FORMAT(F6.0,4E15.6)
        ENDDO
        DO j=0,nnuclei-1
          WRITE(6,42)Eyrast(j),EEyrast(j)
 42       FORMAT(' Eyrast= ',F10.2,' keV  EEyrast= ',F10.2,' keV')
        ENDDO
      ENDIF

      RETURN
      END

      FUNCTION Pig(Ei,i)
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO3/Rho(-100:10000,0:5),Temp(-100:10000,0:5)
      COMMON /CO5/Eyrast(0:5),EEyrast(0:5)
      COMMON /Pnorm/Pneu
      Ef=Ei-(Bn(i+1)-Bn(i))
      Thres=Ef-EEyrast(i+1)
      IF(Thres.LE.0.)THEN
        Pig=1.0
      ELSE
        Pneu=0.
        Eneu=Ef
        DO WHILE(Eneu.GT.0)
          k=(Ef-Eneu)/Edel+0.5
          Pneu=Pneu+Eneu*Rho(k,i+1)
          Eneu=Eneu-Edel
        ENDDO
        Pig=0.
c        ThMeV=Thres/1000.
c        Pig=.271-ThMeV*0.0889+ThMeV*ThMeV*0.0073    !quadratic:
                                                    !E=0.6 MeV:.235
                                                    !E=2.4 MeV:.091
                                                    !E=4.2 MeV:.035
                                                    !E=6.0 MeV:.000
c        Pig=Pig*0.658                               !changed by MG
c        IF(ThMeV.GE.6.)Pig=0.                       !Pig(E>6)=0
      ENDIF
      END



      FUNCTION Pin(Ei,Eii,i)
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO3/Rho(-100:10000,0:5),Temp(-100:10000,0:5)
      COMMON /CO5/Eyrast(0:5),EEyrast(0:5)
      COMMON /Pnorm/Pneu
      x=1.-Pig(Ei,i)
      Eneu=Ei-Eii-(Bn(i+1)-Bn(i))
      IF(x.LE.0.OR.Eneu.LE.0)THEN
        Pin=0.
      ELSE
        k=Eii/Edel+0.5
        Pin=(x/Pneu)*Eneu*Rho(k,i+1)
      ENDIF
      END


      SUBROUTINE SMOOTH(nnuclei)
      DIMENSION WEIGHT(-50:50), wait(0:2047,0:5)
      INTEGER XDIM,DELX
      CHARACTER ANS*1
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO2/ Pteo(0:2047,0:5), Pexp(0:2047,0:5),incl(0:5)
      XDIM=2048
      IXL=(40000.-a0)/a1              !chossing typical places in
      IXH=(10000.-a0)/a1              !spectrum at 10 and 40 MeV
      FWXL=ABS(300./a1)               !assumes 300 keV resol. all over
      FWXH=ABS(300./a1)

      DO j=0,nnuclei-1
        DO i=0,2047
          wait(i,j)=0
        ENDDO
      ENDDO

 34   CONTINUE

      WRITE(6,10)IXL,FWXL
 10   FORMAT('Write FWHM (ch) around ch x= ',I4,' <',F6.1,'>:',$)
      CALL READF(5,FWXL)
      WRITE(6,11)IXH,FWXH
 11   FORMAT('Write FWHM (ch) around ch x= ',I4,' <',F6.1,'>:',$)
      CALL READF(5,FWXH)


C Finding parametrization of fwhm.: FWHM = A + B * SQRT(ch)

      BX=(FWXL-FWXH)/(SQRT(FLOAT(IXL))-SQRT(FLOAT(IXH)))
      AX=FWXL-BX*SQRT(FLOAT(IXL))
      WRITE(6,*)'FWHM have been expressed by A + B * SQRT(ch):'
      WRITE(6,22)AX,BX
 22   FORMAT('Ax=',F8.4,'  Bx=',F8.4)

C Displaying smoothing-array

      IX=IXL
      CALL GAUSS(IX,AX,BX,DELX,WEIGHT)
      WRITE(6,31)IX,(WEIGHT(I),I=-3,3)
 31   FORMAT('Smoothing-array around x=(',I4,'):',7F5.3)

      IX=IXH
      CALL GAUSS(IX,AX,BX,DELX,WEIGHT)
      WRITE(6,31)IX,(WEIGHT(I),I=-3,3)

      ANS='y'
      WRITE(6,33)ANS
 33   FORMAT(/,'Smoothing-array OK (y/n)?<',A,'>:',$)
      CALL READA1(5,ANS)
      IF(ANS.EQ.'n'.OR.ANS.EQ.'N') GO TO 34

C   STARTING THE SMOOTHING PROCEDURE
      DO J=0,nnuclei-1
        DO I=0,XDIM-1
          IT=(I/10)*10
          IF(IT.EQ.I)THEN
            CALL GAUSS(I,AX,BX,DELX,WEIGHT) !calculate Gauss for every
          ENDIF                             !10th channel
          H=0.
          DO II=-DELX,DELX
            III=I+II
            IF(III.GE.0.AND.III.LT.XDIM)THEN
              H=H+Pteo(III,J)*WEIGHT(II)
            ENDIF
          ENDDO
          wait(I,J)=H
        ENDDO
      ENDDO

      DO j=0,nnuclei-1
        DO i=0,2047
          Pteo(i,j)=wait(i,j)
        ENDDO
      ENDDO

      END


      SUBROUTINE GAUSS(IX,AX,BX,DELX,WEIGHT)
C  Subroutine to calculate the weight function (Gaussian around 0)
      INTEGER DELX
      DIMENSION WEIGHT(-50:50)

      DO I=-50,50
        WEIGHT(I)=0.
      ENDDO

      FWX=AX+BX*SQRT(FLOAT(IX))+0.5
      IF(FWX.LT.0)FWX=0.
      SX=FWX/2.35
      XN=SQRT(2.)*SX
      DELX=1.5*SX+0.5
      IF(DELX.GT.50)DELX=50
      SUM=0

      DO I=-DELX,DELX
        IF(XN.NE.0)THEN
          H=EXP(-(I/XN)**2)
        ELSE
          H=0.
        ENDIF
        WEIGHT(I)=H
        SUM=SUM+H
      ENDDO
      IF(SUM.EQ.0)THEN
        SUM=1.
        WEIGHT(0)=1.
      ENDIF
      DO I=-DELX,DELX
        IF(SUM.GT.0.00001)WEIGHT(I)=WEIGHT(I)/SUM
      ENDDO
      RETURN
      END



      SUBROUTINE INTEGRATE(E0min,E0max)
      REAL Mult
      COMMON /CO1/a0,a1,a2,Edel,Bn(0:5),AX,BX,Spin,Multstat
      COMMON /CO2/ Pteo(0:2047,0:5),Pexp(0:2047,0:5),incl(0:5)
      COMMON /CO5/Eyrast(0:5),EEyrast(0:5)

C The following strange statements are made to ensure that loops
C are going with energies in multiple of Edel
      B21=INT(((Bn(2)-Bn(1))/Edel)-0.5)*Edel
      B31=INT(((Bn(3)-Bn(1))/Edel)-0.5)*Edel
      B32=INT(((Bn(3)-Bn(2))/Edel)-0.5)*Edel
      B41=INT(((Bn(4)-Bn(1))/Edel)-0.5)*Edel
      B42=INT(((Bn(4)-Bn(2))/Edel)-0.5)*Edel
      B43=INT(((Bn(4)-Bn(3))/Edel)-0.5)*Edel

Ccccccccccccccccccccccccccccc
C STARTING THE CALCULATIONS
Ccccccccccccccccccccccccccccc
      E0=E0min
      DO WHILE (E0.LE.E0max)
        i=(E0/Edel)+0.5

C Nucleus 0-n:*****************
        sum1 =0.
        IF(incl(0).EQ.0.OR.E0.LT.Eyrast(0))GO TO 30
        sum1=Pig(E0,0)*Mult(E0,0)
  30    Pteo(i,0)=sum1

C Nucleus 1-n:*****************
        sum1 =0.
        IF(incl(1).EQ.0.OR.E0.LT.Bn(1)-Bn(0)+Eyrast(1))GO TO 31
        E1   =EEyrast(1)                              
        E1max=E0-(Bn(1)-Bn(0))
        DO WHILE (E1.LT.E1max)
          sum1=sum1+Pig(E1,1)*Pin(E0,E1,0)*Mult(E1,1)
          E1=E1+Edel
        ENDDO
  31    Pteo(i,1)=sum1
C Nucleus 2-n:*****************
        sum1 =0
        IF(incl(2).EQ.0.OR.E0.LT.Bn(2)-Bn(0)+Eyrast(2))GO TO 32
        E1   =B21+EEyrast(2)
        E1max=E0-(Bn(1)-Bn(0))
        DO WHILE (E1.LT.E1max)
          sum2 =0
          E2   =EEyrast(2)
          E2max=E1-(Bn(2)-Bn(1))
          DO WHILE (E2.LT.E2max)
            sum2=sum2+Pig(E2,2)*Pin(E1,E2,1)*Mult(E2,2)
            E2=E2+Edel
          ENDDO
          sum1=sum1+Pin(E0,E1,0)*sum2
          E1=E1+Edel
        ENDDO
  32    Pteo(i,2)=sum1

C Nucleus 3-n:*****************
        sum1 =0
        IF(incl(3).EQ.0.OR.E0.LT.Bn(3)-Bn(0)+Eyrast(3))GO TO 33
        E1   =B31+EEyrast(3)
        E1max=E0-(Bn(1)-Bn(0))
        DO WHILE (E1.LT.E1max)
          sum2 =0
          E2   =B32+EEyrast(3)
          E2max=E1-(Bn(2)-Bn(1))
          DO WHILE (E2.LT.E2max)
            sum3 =0
            E3   =EEyrast(3)
            E3max=E2-(Bn(3)-Bn(2))
            DO WHILE (E3.LT.E3max)
              sum3=sum3+Pig(E3,3)*Pin(E2,E3,2)*Mult(E3,3)
              E3=E3+Edel
            ENDDO
            sum2=sum2+Pin(E1,E2,1)*sum3
            E2=E2+Edel
          ENDDO
          sum1=sum1+Pin(E0,E1,0)*sum2
          E1=E1+Edel
        ENDDO
  33    Pteo(i,3)=sum1

C Nucleus 4-n:*****************
        sum1 =0
        IF(incl(4).EQ.0.OR.E0.LT.Bn(4)-Bn(0)+Eyrast(4)) GO TO 34
        E1   =B41+EEyrast(4)
        E1max=E0-(Bn(1)-Bn(0))
        DO WHILE (E1.LT.E1max)
          sum2 =0
          E2   =B42+EEyrast(4)
          E2max=E1-(Bn(2)-Bn(1))
          DO WHILE (E2.LT.E2max)
            sum3 =0
            E3   =B43+EEyrast(4)
            E3max=E2-(Bn(3)-Bn(2))
            DO WHILE (E3.LT.E3max)
              sum4 =0
              E4   =EEyrast(4)
              E4max=E3-(Bn(4)-Bn(3))
              DO WHILE (E4.LT.E4max)
                sum4=sum4+Pig(E4,4)*Pin(E3,E4,3)*Mult(E4,4)
                E4=E4+Edel
              ENDDO
              sum3=sum3+Pin(E2,E3,2)*sum4
              E3=E3+Edel
            ENDDO
            sum2=sum2+Pin(E1,E2,1)*sum3
            E2=E2+Edel
          ENDDO
          sum1=sum1+Pin(E0,E1,0)*sum2
          E1=E1+Edel
        ENDDO
  34    Pteo(i,4)=sum1

C Writing results on terminal
        sum=Pteo(i,0)+Pteo(i,1)+Pteo(i,2)+Pteo(i,3)+Pteo(i,4)
        WRITE(6,35)E0,Pteo(i,0),Pteo(i,1),Pteo(i,2),Pteo(i,3),
     1  Pteo(i,4),sum
 35     FORMAT(' Ex=',F7.1,5F6.2,' (Sum=',F6.3,')')
      E0=E0+Edel
      ENDDO

      RETURN
      END

 


