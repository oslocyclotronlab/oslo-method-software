      PROGRAM RhoSig 
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
C Stuff for the rhosig iteration
      REAL RhoAve(0:101,0:511),RhoAvev(0:101,0:511)
      REAL SigAve(0:101,0:511),SigAvev(0:101,0:511)
      REAL SumFg(0:511),SumFgv(0:511)
      REAL Fg(0:511,0:511),FgTeo(0:511,0:511),FgN(0:511,0:511)
      REAL Fgv(0:511,0:511),sFg(0:511,0:511)
      REAL Rhow(0:511),Sigw(0:511),sRhoAve(0:511),sSigAve(0:511)
      REAL RhoBethe(0:511),SigGDR(0:511),alpha,FWHM,Eres,Cgdr
      REAL Chi(0:101),Egmin,mass,density,factor,a0,a1
      REAL sum,sumx,sumxx,sumy,sumxy,Efitmax,Efitmin                  !Best fit
      REAL RhoLn(0:511),RhoFit(0:511),C,Ct,T,weight                   !Best fit
      REAL SigLn(0:511),SigFit(0:511),n,Cn,signorm                    !Best fit
      INTEGER time,nunc,degrees
      INTEGER fitmin,fitmax                                           !Best fit
      INTEGER dim,igmin
      COMMON/iter1/N_Eg(0:511),N_U(0:511),imin,imax,igmin
      COMMON/iter2/Rho(0:511,0:511),Sig(0:511,0:511)
      DIMENSION Fi(0:511),Ff(0:511)
      CHARACTER*3 G1,G2
      CHARACTER*9 FORM1
      CHARACTER*16 FORM2
      CHARACTER*9 FORM3
      WRITE(6,*)'     _____________________________________________'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |              R H O S I G  3.0               |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Program to calculate level density Rho and  |'
      WRITE(6,*)'    |   gamma strength function Sig from first-   |'
      WRITE(6,*)'    |   generation spectra using Fg = Rho * Sig   |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |          Oslo Cyclotron Laboratory          |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Created:  01/12 - 1996                      |'
      WRITE(6,*)'    | Modified: 27/03/99 10/04/99 20/5/99         |'
      WRITE(6,*)'    | Trine Tveter      Magne Guttormsen          |'
      WRITE(6,*)'    | Lisbeth Bergholt  Andreas Schiller          |'
      WRITE(6,*)'    |_____________________________________________|'
      WRITE(6,*)' '
C History: 
C 20 mars  1998: Lisbeth rettet: antall iterasjoner -1, 
C                     tatt bort: normeringer for hver iterasjon, slik at
C                                det naa konvergerer.
C  2 april 1998: Magne  innsatt: Lorentzian alternativ for sig(Eg)
C                       innsatt: a0 maa vaere med i spekter kalibreringer
C 16 april 1998: Magne   byttet: F=0.1 instedet for 1 naar F<0
C 20 mai   1998: Magne   endret: Real matrices, rewr.f etc
C 15 juni  1998: Magne   fikset: aldri neg. energi i ch 0 
C 30 nov   1998: Lisbeth byttet: F=0 istedet for 0.1 naar F<0
C                       innsatt: Egmin - nedre grense i gamma energi
C                        endret: Deler rho og sig med best fit, ikke proevefunk.
C                        endret: Utskrifter
C    jan   1999: Lisbeth endret: Statisical uncertainty
C    mars  1999: Lisbeth endret: fra til channels, deep i rhoer vekk
C    april 1999: Magne   endret: Legger inn 1 proc av middeltellinger for < or =0
C    mai   1999: Andreas endret: ingen proevefunksjoner lenger
C                        derfor: nullte iterasjon fins ikke lenger
C                            og: iterasjon begynner med aa dele paa rho=1
C                        endret: hellningen til nivaatettheten bestemmes ut av 
C                                parametren a (tatt fra litteraturen). Dette gir 
C                                automatisk hellningen til styrkefunksjonen
C                        endret: riktig chi^2 (haaper jeg)
C                        endret: rhoex.rsg og sigex.rsg for hver 
C                                eksitasjonsbin finnes ikke lenger
C                        endret: rhoteo.rsg og sigteo.rsg finnes ikke lenger
C                           men: rho og sig matriser er stoerre 
C                                (inkludert RhoFit,RhoBethe, og SigFit,SigGDR)  
C Things that should be done in the future:
C Fitting with error weighting
C Finding a better parametrization of the strength function
C Initializing parameter values
      Emin=4000.0             ! Default lower Ex for 1.gen. spec.
      Emax=8000.0             ! Default higher Ex for 1.gen. spec.
      nit=10                  ! Default number of iterations
      Egmin=1000              ! Default lowest gamma energy

C Reading first-generation mama-matrix
      IDEST=1
      ITYPE=3
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 1 and the name of your input first-'
      WRITE(6,*)'generation matrix in the two next questions... '
      WRITE(6,*)' '
      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
      IF(bx+by.EQ.2.)THEN     
         aEg0=11.               !Defaults in case of no calibration
         aEg1=20.
         aEx0=9660.
         aEx1=-120.
      ELSE
         aEg0=cal(1,IDEST,1,1)  !Picks the spectrum calibration
         aEg1=cal(1,IDEST,1,2)
         aEx0=cal(1,IDEST,2,1)
         aEx1=cal(1,IDEST,2,2)
      ENDIF
      a1=ABS(aEx1)
      multiple=INT((120./a1)+0.5)   
      IF(a1.GT.150.)multiple=1
      a1=FLOAT(multiple)*a1

C An energy that was in the middle of a channel, shall still be in the 
C middle of a channel after change of calibration
      Eold=aEx0+aEx1*0                                  !Choosing old channel 0
      a0=(Eold/a1-INT(Eold/a1))*a1
      IF(a0.LT.0)a0=a0+a1

C Compressing (or stretching) along X and Y - axis
      DO j=0,YDIM-1                                     ! X-axis
         Sum=0.
         DO i=0,XDIM-1
            Fi(i)=rMAT(IDEST,i,j)                       ! Fi(i) and Ff(i) real type
            Sum=Sum+Fi(i)
         ENDDO
         IF(Sum.NE.0)THEN
            CALL ELASTIC(Fi,Ff,aEg0,aEg1,a0,a1,512,512) ! Modifies spectrum to give  
            DO i=0,XDIM-1                               ! calibration a0 and a1
               Fg(i,j)=Ff(i)
               Fi(i)=0.
            ENDDO
         ENDIF
      ENDDO
      DO i=0,XDIM-1                                     ! Y-axis
         Sum=0.
         DO j=0,YDIM-1
            Fi(j)=Fg(i,j)
            Sum=Sum+Fi(j)
         ENDDO
         IF(Sum.NE.0)THEN
            CALL ELASTIC(Fi,Ff,aEx0,aEx1,a0,a1,512,512)
            DO j=0,YDIM-1
               Fg(i,j)=Ff(j)
               Fi(j)=0.
            ENDDO
         ENDIF
      ENDDO 

C Replacing negative counts with 0 and finding dimension of Fg matrix
      XDIM=INT(FLOAT(XDIM)*ABS(aEg1/a1)+0.5)
      YDIM=INT(FLOAT(YDIM)*ABS(aEx1/a1)+0.5)
      dim=10
      DO j=0,YDIM
         DO i=0,XDIM
            IF(Fg(i,j).GT.0.AND.i.GT.dim)dim=i
            IF(Fg(i,j).GT.0.AND.j.GT.dim)dim=j
            IF(Fg(i,j).LT.0.)Fg(i,j)=0             !Delete negative numbers
         ENDDO
      ENDDO
      dim=MIN0(dim,XDIM,YDIM)

C Input lower limit for gammas used in the extraction
      WRITE(6,10)Egmin
 10   FORMAT('Lower limit of gamma energy (keV)      <',F7.1,'>:',$)
      CALL READF(5,Egmin)

C Zeroing spectra for Egamma < Egmin
      igmin=INT(((Egmin-a0)/a1)+0.5)
      Egmin=igmin*a1+a0
      DO j=0,YDIM
         DO i=0,igmin-1 
            Fg(i,j)=0.
         ENDDO
      ENDDO

C Input some parameters from keyboard
      WRITE(6,11)Emin
 11   FORMAT('Lower limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Emin)
      imin=INT(((Emin-a0)/a1)+0.5)
      Emin=a0+a1*imin
      WRITE(6,12)Emax
 12   FORMAT('Upper limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Emax)
      imax=INT(((Emax-a0)/a1)+0.5)
      imax=MIN0(imax,dim)
      Emax=a0+a1*imax
      IF(imax.GT.511)THEN
         WRITE(6,13)Emax,a1
 13      FORMAT('Sorry, Emax/a1 = ',F7.1,'/',F5.1,' > 511 channels')
         STOP
      ENDIF
      WRITE(6,14) nit
 14   FORMAT('Number of iterations ( < 100)              <',I3,'>:',$)
      CALL READI(5,nit)
      IF(nit.GE.100)nit=100
      WRITE(6,15)a0,a1,dim,dim,Emin,Emax,nit
 15   FORMAT('Common calibration is a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     + /,'Dimension is',I3,' x',I3,' with excitation region ',F5.0,'-',F5.0,'keV',
     + /,'Number of iterations is ',I3)
C Finding number of Eg and U spectra involved in summing
      DO ix=imin,imax 
         DO iu=0,ix-igmin 
            N_U(iu)=N_U(iu)+1   ! N(U) = number of 1.gen. spectra participating
         ENDDO                  ! to determine Rho at iu
         DO ig=igmin,ix
            N_Eg(ig)=N_Eg(ig)+1 ! N(Eg)= number of 1.gen. spectra participating
         ENDDO                  ! to determine Sig at ig
      ENDDO

C One has to have counts in every channel, else we divide by zeros.
C However, we will not put in more than resonable. We calulate average
C number of counts in each 1. gen. spectrum, and then fill in 5 procent
C of this in channels with content = < fill. New for version 2.8
      DO ix=imin,imax   !Finding number of counts in Fg(ig,ix) for each Ex
         sum=0.
         DO ig=igmin,ix
            sum=sum+Fg(ig,ix) 
         ENDDO
         IF(igmin.LE.ix)THEN
            fill=ABS(0.01*(sum/(ix-igmin+1)))
            DO ig=igmin,ix
               IF(Fg(ig,ix).LE.fill)THEN
                  Fg(ig,ix)=fill
               ENDIF
            ENDDO
         ENDIF
      ENDDO

C Finding number of counts in Fg(ig,ix) for each Ex 
      DO ix=imin,imax   
         SumFg(ix)=0.
         DO ig=igmin,ix
            SumFg(ix)=SumFg(ix)+Fg(ig,ix) 
         ENDDO
      ENDDO

C Normalizing Fg(Eg,Ex)
      DO ix=imin,imax         
         DO ig=igmin,ix
            IF(SumFg(ix).LE.0.)THEN
               FgN(ig,ix)=0. !This case would be disastrous, lets hope it will never occur
            ELSE
               FgN(ig,ix)=Fg(ig,ix)/SumFg(ix)
            ENDIF
         ENDDO
      ENDDO  

C Start value for RhoAve
      DO iu=0,imax-igmin
         RhoAve(0,iu)=1
      ENDDO
C Iteration starts here

      CALL Iteration(nit,FgN,RhoAve,SigAve)

C Statistical errors
C Estimating errorbars sSigAve and sRhoAve from uncertainties in counts sFg
      DO ix=imin,imax
         Ex=a0+a1*ix
         sFmax=0.
         TotM=MAX(1.,gM(Ex))            !Number of gammas for 1.gen. 
         BckM=MAX(0.,gM(Ex)-1.)         !Number og gammas from 2.+3.+... gen.
         DO ig=0,imax
            Fgv(ig,ix)=Fg(ig,ix)
C Fg=total-background NaI spectra, factor 3 due to unfolding
C Total and tackground error are assumed to be independent from each 
C other, therefore we use SQRT(tot+bck) instead of SQRT(tot)+SQRT(bck)
            sFg(ig,ix)=3.*SQRT((TotM+BckM)*Fg(ig,ix))
            IF(sFg(ig,ix).GT.sFmax)THEN
               isFEg=ig
               sFmax=sFg(ig,ix)
            ENDIF
         ENDDO

C The factor 3 due to unfolding is very uncertain, it could be anything...
C We treat sFg(ig,ix) from Eg=0 up to gamma energy (isFEg) for maximum 
C uncertainty (sFmax) in a special way (we have high sFg around Eg=0)
C We could additionally introduce a factor>1 in front of the expression
C (FLOAT(isFEg)-FLOAT(ig))/FLOAT(isFEg) which goes from 2 to 1 for ig=0 to isFEg
         IF(isFEg.GT.0)THEN
            DO ig=0,isFEg
               sFg(ig,ix)=sFmax*(1.+(FLOAT(isFEg)-FLOAT(ig))/FLOAT(isFEg))
            ENDDO
         ENDIF
      ENDDO

C Calculating number of degrees of freedom
C Number of data points fitted in first generation spectrum
      degrees=(imax-imin+1)*(imax+imin-2*igmin)/2

C Minus number of data points in the fit functions (rho and sigma)
      degrees=degrees-2*(imax-igmax)

C Finding extracted average 1. gen. spectra FgTeo and Chi**2
      DO it=1,nit
         Chi(it)=0.
         DO ix=imin,imax
            DO ig=igmin,ix
               iu=ix-ig
               FgTeo(ig,ix)=SigAve(it,ig)*RhoAve(it,iu)*SumFg(ix)
               Chi(it)=Chi(it)+(FgTeo(ig,ix)-Fg(ig,ix))**2./(sFg(ig,ix)*sFg(ig,ix))
            ENDDO 
         ENDDO
         Chi(it)=Chi(it)/FLOAT(degrees)
      ENDDO

C Calculating some formats
      nit2=nit/2
      nit21=nit2+1
      G1=CHAR(MOD(nit2,10)+48)
      IF(nit2.GE.10)G1=CHAR(MOD(nit2/10,10)+48)//G1
      IF(nit2.GE.100)G1=CHAR(MOD(nit2/100,10)+48)//G1
      G2=CHAR(MOD(nit21,10)+48)
      IF(nit21.GE.10)G2=CHAR(MOD(nit21/10,10)+48)//G2
      IF(nit21.GE.100)G2=CHAR(MOD(nit21/100,10)+48)//G2
      G2=CHAR(MOD(N1,10)+48)
      FORM1='A23,'//G1//'I6'
      FORM2='A18,F6.0,'//G2//'F6.2'
      FORM3='A24,'//G2//'I6'

C Showing indicators for how well the iteration converged
      i1MeV=imax/4
      i2MeV=imax/2
      i3MeV=3*imax/4
      e1=a0+FLOAT(i1MeV)*a1
      e2=a0+FLOAT(i2MeV)*a1
      e3=a0+FLOAT(i3MeV)*a1
      WRITE(6,*)'___________________________________________________________'
      WRITE(6,16)
 16   FORMAT(/,'        Convergence test using various indicators')
      WRITE(6,FORM1)'Indicator Iteration = 0',(it,it=2,nit,2)
      WRITE(6,FORM2)'Rho/Rho0   at U = ',e1,(RhoAve(it,i1MeV)/RhoAve(0,i1MeV),it=2,nit,2)
      WRITE(6,FORM2)'Rho/Rho0   at U = ',e2,(RhoAve(it,i2MeV)/RhoAve(0,i2MeV),it=2,nit,2)
      WRITE(6,FORM2)'Rho/Rho0   at U = ',e3,(RhoAve(it,i3MeV)/RhoAve(0,i3MeV),it=2,nit,2)
      WRITE(6,FORM2)'Sig/Sig1   at Eg =',e1,(SigAve(it,i1MeV)/SigAve(1,i1MeV),it=2,nit,2)
      WRITE(6,FORM2)'Sig/Sig1   at Eg =',e2,(SigAve(it,i2MeV)/SigAve(1,i2MeV),it=2,nit,2)
      WRITE(6,FORM2)'Sig/Sig1   at Eg =',e3,(SigAve(it,i3MeV)/SigAve(1,i3MeV),it=2,nit,2)
      WRITE(6,FORM3)'   Chi^2 for 1.gen.sp.  ',(Chi(it),it=2,nit,2)
      WRITE(6,*)'___________________________________________________________'

C Error calculation
C Finding standard deviation in Rho and Sig for each channel
      DO iu=0,imax
         Points=0.
         Rhow(iu)=0.
         IF(N_U(iu).GT.1)THEN
            DO ix=imax-(N_U(iu)-1),imax
               IF(Rho(iu,ix).GT.0.)THEN
                  Rhow(iu)=Rhow(iu)+(1.-Rho(iu,ix)/RhoAve(nit,iu))**2.
                  Points=Points+1.
               ENDIF
            ENDDO
            IF(Points.GT.0.)Rhow(iu)=SQRT(Rhow(iu)/Points)
         ENDIF
      ENDDO
      DO ig=0,imax
         Points=0.
         Sigw(ig)=0.
         IF(N_Eg(ig).GT.1)THEN
            DO ix=imax-(N_Eg(ig)-1),imax
               IF(Sig(ig,ix).GT.0.)THEN
                  Sigw(ig)=Sigw(ig)+(1.-Sig(ig,ix)/SigAve(nit,ig))**2.
                  Points=Points+1.
               ENDIF
            ENDDO
            IF(Points.GT.0.)Sigw(ig)=SQRT(Sigw(ig)/Points)
         ENDIF
      ENDDO

C Initializing new RhoAvev
      DO iu=0,imax
         RhoAvev(0,iu)=RhoAve(0,iu)
      ENDDO

C New iteration were we test the influence of Fg -> Fg + sFg
C One by one channel is changed and (SigAve-SigAve)**2
C and (RhoAve-RhoAver)**2 is summed up, and SigmaRho and SigmaF is found
      r=rand(time())               !seeding
      nunc=1000
      nStep=100
      DO i=1,nunc
         nTest=(i/nStep)*nStep
         IF(nTest.EQ.i)THEN
            ist=putc('.')
            CALL flush(6)
         ENDIF
         DO ix=imin,imax
            DO ig=igmin,ix
               IF(Fg(ig,ix).GT.0.)THEN      
                  r=rand(0)
                  z=Finvert(r)
                  Fgv(ig,ix)=Fg(ig,ix)+z*sFg(ig,ix) !Adding uncertainty
                  IF(Fgv(ig,ix).LT.0.)THEN !Zeroing negative numbers     
                     Fgv(ig,ix)=0.
                  ENDIF
               ELSE
                  Fgv(ig,ix)=0.
               ENDIF
            ENDDO
         ENDDO
         DO ix=imin,imax   !Finding number of counts in Fgv(ig,ix) for each Ex
            sum=0.
            DO ig=igmin,ix
               sum=sum+Fgv(ig,ix) 
            ENDDO
            IF(igmin.LE.ix)THEN
               fill=ABS(0.05*(sum/(ix-igmin+1)))
               DO ig=igmin,ix
                  IF(Fgv(ig,ix).LE.fill)THEN
                     Fgv(ig,ix)=fill
                  ENDIF
               ENDDO
            ENDIF
         ENDDO

C Finding number of counts in Fg(ig,ix) for each Ex 
         DO ix=imin,imax   
            SumFgv(ix)=0.
            DO ig=igmin,ix
               SumFgv(ix)=SumFgv(ix)+Fgv(ig,ix) 
            ENDDO
         ENDDO

C Normalizing Fgv(Eg,Ex)
         DO ix=imin,imax         
            DO ig=igmin,ix
               IF(SumFgv(ix).LE.0.)THEN
                  FgN(ig,ix)=0. !This case would be disastrous, lets hope it will never occur
               ELSE
                  FgN(ig,ix)=Fgv(ig,ix)/SumFgv(ix)
               ENDIF
            ENDDO
         ENDDO  
         CALL Iteration(nit,FgN,RhoAvev,SigAvev) !Iterating Fgv
         DO iu=0,imax
            sRhoAve(iu)=sRhoAve(iu)+(RhoAve(nit,iu)-RhoAvev(nit,iu))**2.
            sSigAve(iu)=sSigAve(iu)+(SigAve(nit,iu)-SigAvev(nit,iu))**2.
         ENDDO
      ENDDO
      DO iu=0,imax
         sRhoAve(iu)=SQRT(sRhoAve(iu)/nunc)
         sSigAve(iu)=SQRT(sSigAve(iu)/nunc)
      ENDDO

C The projection technique is good to find fine structure in level density 
C and strength function, whereas the gross features like temperature and 
C multipolarity of gamma transitions (n) cannot be determined. We try to 
C solve this problem now:
C Solutions:
C 1.) We multiply rho with exp(alpha*(E_x-E_gamma))
C           and sigma with exp(alpha*E_gamma)
C 2.) We fix alpha such that we get a reasonable level density parameter a
C     around the neutron binding energy
C Input energy intervall for the fit of rho-data
      WRITE(6,*)
      WRITE(6,*)'We will now fit the level density to litterature'
      Efitmin=Emax-4000.
      WRITE(6,21)Efitmin
 21   FORMAT('Lower limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmin)
      Efitmax=Emax-Egmin-1000.
      WRITE(6,22)Efitmax
 22   FORMAT('Upper limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmax)
      fitmin=INT((Efitmin-a0)/a1+0.5)
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      Efitmax=fitmax*a1+a0
      WRITE(6,23)Efitmin,Efitmax
 23   FORMAT('Fitting between ',F5.0,' keV and ',F5.0,' keV of excitation energy') 
C Input data of the nucleus
      mass=162.
      WRITE(6,24)mass
 24   FORMAT('Mass number of the resulting nucleus   <',F7.1,'>:',$)
      CALL READF(5,mass)
      density=mass/10.
      WRITE(6,25)density
 25   FORMAT('Level density parameter a in (MeV)^-1  <',F7.1,'>:',$)
      CALL READF(5,density)
C Level density formula from Gilbert Cameron Eq 5 using Eqs 9 and 11 for sigma
      factor=1./(12.*SQRT(0.1776*density)*(mass**(1./3.)))
      DO iu=0,imax-igmin
         Eu=a1*iu+a0
         Eu=Eu/1000.
         IF(Eu.GT.0.)THEN
            RhoBethe(iu)=factor*EXP(2.*SQRT(density*Eu))/(Eu**1.5)
         ELSE 
            RhoBethe(iu)=0.
         ENDIF
      ENDDO
      sumxx=0.
      sumxy=0.
      sumx=0.
      sumy=0.
      sum=0.
      DO iu=fitmin,fitmax
         IF(RhoAve(nit,iu).GT.0.)THEN
            RhoFit(iu)=RhoBethe(iu)/RhoAve(nit,iu)
         ELSE
            RhoFit(iu)=0.
         ENDIF
         IF(RhoFit(iu).GT.0.)THEN
            RhoLn(iu)=LOG(RhoFit(iu))
            Eu=a1*iu+a0
            weight=1.
         ELSE
            RhoLn(iu)=0.
            Eu=0.
            weight=0.
         ENDIF
         sumxx=sumxx+Eu*Eu
         sumxy=sumxy+RhoLn(iu)*Eu
         sumx=sumx+Eu
         sumy=sumy+RhoLn(iu)
         sum=sum+weight
      ENDDO
      alpha=(sum*sumxy-sumx*sumy)/(sum*sumxx-sumx*sumx)
      C=EXP((sumy-alpha*sumx)/sum)
      DO iu=0,imax-igmin
         Eu=a1*iu+a0
         RhoAve(nit,iu)=C*RhoAve(nit,iu)*EXP(Eu*alpha)
         sRhoAve(iu)=C*sRhoAve(iu)*EXP(Eu*alpha) 
      ENDDO
      WRITE(6,*)'We will now fit the level density to an exponential'
      Efitmin=1000.
      WRITE(6,26)Efitmin
 26   FORMAT('Lower limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmin)
      Efitmax=Emax-Egmin-1000.
      WRITE(6,27)Efitmax
 27   FORMAT('Upper limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmax)
      fitmin=INT((Efitmin-a0)/a1+0.5)
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      Efitmax=fitmax*a1+a0
      WRITE(6,28)Efitmin,Efitmax
 28   FORMAT('Fitting between ',F5.0,' keV and ',F5.0,' keV of excitation energy') 
      sumxx=0.
      sumxy=0.
      sumx=0.
      sumy=0.
      sum=0.
      DO iu=fitmin,fitmax
         IF(RhoFit(iu).GT.0.)THEN
            RhoLn(iu)=LOG(RhoFit(iu))
            Eu=a1*iu+a0
            weight=1.
         ELSE
            RhoLn(iu)=0.
            Eu=0.
            weight=0.
         ENDIF
         sumxx=sumxx+Eu*Eu
         sumxy=sumxy+RhoLn(iu)*Eu
         sumx=sumx+Eu
         sumy=sumy+RhoLn(iu)
         sum=sum+weight
      ENDDO
      T=(sum*sumxx-sumx*sumx)/(sum*sumxy-sumx*sumy)
      Ct=EXP((sumy-sumx/T)/sum)
      WRITE(6,29)T,Ct
 29   FORMAT('Best fit (Ct*EXP(Eu/T): T=',F6.2,' Ct=',E9.2)   
      DO iu=0,imax-igmin
         Eu=a1*iu+a0
         RhoFit(iu)=Ct*EXP(Eu/T)
      ENDDO
      signorm=SigAve(nit,igmin)*EXP((a1*igmin+a0)*alpha)
      DO ig=igmin,imax
         Eg=a1*ig+a0
C What we multiply on Rho, we have to multiply on Sig too
C Normalizing Sig to 1 at minimum gamma energy
         SigAve(nit,ig)=SigAve(nit,ig)*EXP(Eg*alpha)/signorm
         sSigAve(ig)=sSigAve(ig)*EXP(Eg*alpha)/signorm
      ENDDO

C Now calculating a GDR 
C Input data of the nucleus
      WRITE(6,*)'We will now calculate a GDR strength function'
      FWHM=5000.
      WRITE(6,30)FWHM
 30   FORMAT('Give FWHM of the GDR             <',F6.1,'>:',$)
      CALL READF(5,FWHM)
      Eres=15000.
      WRITE(6,31)Eres
 31   FORMAT('Give energy centroid of the GDR <',F7.1,'>:',$)
      CALL READF(5,Eres)
C Normalizing to 1 at Egmin
      Cgdr=((Eres**2.-Egmin**2.)**2.+(Egmin**2.)*(FWHM**2.))/((Egmin**4.)*(FWHM**2.))
      DO ig=igmin,imax
         Eg=a0+a1*ig
         SigGDR(ig)=Cgdr*(Eg**4.)*(FWHM**2.)/((Eres**2.-Eg**2.)**2.+(Eg**2.)*(FWHM**2.))
      ENDDO
C Now fitting the strength function with a E_gamma^n function
C This might be a stupid parametrization, but its tradition
C Input energy intervall for the fit of sig-data
      WRITE(6,*)'We will now fit a Cn*E_gamma**n to the strength function'
      Efitmin=Egmin+1000.
      WRITE(6,32)Efitmin
 32   FORMAT('Lower limit for the fit of sig (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmin)
      Efitmax=Emax-1000.
      WRITE(6,33)Efitmax
 33   FORMAT('Upper limit for the fit of sig (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmax)
      fitmin=INT((Efitmin-a0)/a1+0.5)
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      Efitmax=fitmax*a1+a0
      WRITE(6,34)Efitmin,Efitmax
 34   FORMAT('Fitting between ',F5.0,' keV and ',F5.0,' keV of gamma energy') 
      sumxx=0.
      sumxy=0.
      sumx=0.
      sumy=0.
      sum=0.
      DO ig=fitmin,fitmax 
         IF((SigAve(nit,ig).GT.0.).AND.((a1*ig+a0).GT.0.))THEN
            SigLn(ig)=LOG(SigAve(nit,ig))
            Eg=LOG(a1*ig+a0)
            weight=1.
         ELSE
            SigLn(ig)=0.
            Eg=0.
            weight=0.
         ENDIF
         sumxx=sumxx+Eg*Eg
         sumxy=sumxy+SigLn(ig)*Eg
         sumx=sumx+Eg
         sumy=sumy+SigLn(ig)
         sum=sum+weight
      ENDDO
      n=(sum*sumxy-sumx*sumy)/(sum*sumxx-sumx*sumx)
      Cn=EXP((sumy-n*sumx)/sum)
      WRITE(6,35)n,Cn
 35   FORMAT('Best fit (Cn*Eg**n): n=',F6.2,' Cn=',E9.2)   
      DO ig=0,imax
         SigFit(ig)=Cn*((a1*ig+a0)**n)
      ENDDO
C Writing matrices  out for mama
C Writing matrices to files
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=a0
      cal(1,1,2,2)=a1
      cal(1,1,2,3)=0.
      XDIM=dim
      YDIM=dim
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=Fg(ig,ix)
         ENDDO
      ENDDO
      outfile='fg.rsg'
      comment='Observed first gen. spectra for each Ex'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,36) outfile
 36   FORMAT('Observed first gen. spectra written to file:            ',A11)
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=FgTeo(ig,ix)  
         ENDDO
      ENDDO
      outfile='fgteo.rsg'
      comment='Theoretical first gen. spectra for each Ex'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,37) outfile
 37   FORMAT('Theoretical first gen. spectra written to file:      ',A11)
C Calculate relative level density and strength function
C Writing spectra out for PAW
      outfile='rhopaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO iu=0,dim-1
         WRITE(20,*)RhoAve(nit,iu)
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)sRhoAve(iu)
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)RhoAve(nit,iu)*Rhow(iu)
      ENDDO
      DO iu=0,dim-1
         IF(RhoFit(iu).GT.0.)THEN
            WRITE(20,*)RhoAve(nit,iu)/RhoFit(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         IF(RhoFit(iu).GT.0.)THEN
            WRITE(20,*)sRhoAve(iu)/RhoFit(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         IF(RhoFit(iu).GT.0.)THEN
            WRITE(20,*)RhoAve(nit,iu)*Rhow(iu)/RhoFit(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)RhoFit(iu)
      ENDDO
      DO iu=0,dim-1
         IF(RhoBethe(iu).GT.0.)THEN
            WRITE(20,*)RhoAve(nit,iu)/RhoBethe(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         IF(RhoBethe(iu).GT.0.)THEN
            WRITE(20,*)sRhoAve(iu)/RhoBethe(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         IF(RhoBethe(iu).GT.0.)THEN
            WRITE(20,*)RhoAve(nit,iu)*Rhow(iu)/RhoBethe(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)RhoBethe(iu)
      ENDDO
      CLOSE(20)
      WRITE(6,38) outfile
 38   FORMAT('Rho, stat, syst errors, rel. to fit, rel. to Bethe: ',A11)
      outfile='sigpaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO ig=0,dim-1
         WRITE(20,*)SigAve(nit,ig)
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)sSigAve(ig)
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)SigAve(nit,ig)*Sigw(ig)
      ENDDO
      DO ig=0,dim-1
         IF(SigFit(ig).GT.0.)THEN
            WRITE(20,*)SigAve(nit,ig)/SigFit(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         IF(SigFit(ig).GT.0.)THEN
            WRITE(20,*)sSigAve(ig)/SigFit(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         IF(SigFit(ig).GT.0.)THEN
            WRITE(20,*)SigAve(nit,ig)*Sigw(ig)/SigFit(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)SigFit(ig)
      ENDDO
      DO ig=0,dim-1
         IF(SigGDR(ig).GT.0.)THEN
            WRITE(20,*)SigAve(nit,ig)/SigGDR(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         IF(SigGDR(ig).GT.0.)THEN
            WRITE(20,*)sSigAve(ig)/SigGDR(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         IF(SigGDR(ig).GT.0.)THEN
            WRITE(20,*)SigAve(nit,ig)*Sigw(ig)/SigGDR(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)SigGDR(ig)
      ENDDO
      CLOSE(20)
      WRITE(6,39) outfile
 39   FORMAT('Sig, stat, syst errors, rel. to fit, rel. to GDR  : ',A11)
C Writing spectra out for mama
C Writting spectra to matrices
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      YDIM=11
      DO iu=0,XDIM-1
         rMAT(IDEST,iu,0)=RhoAve(nit,iu)
         rMAT(IDEST,iu,1)=sRhoAve(iu)
         rMAT(IDEST,iu,2)=RhoAve(nit,iu)*Rhow(iu)
         IF(RhoFit(iu).GT.0.)THEN
            rMAT(IDEST,iu,3)=RhoAve(nit,iu)/RhoFit(iu)
            rMAT(IDEST,iu,4)=sRhoAve(iu)/RhoFit(iu)
            rMAT(IDEST,iu,5)=RhoAve(nit,iu)*Rhow(iu)/RhoFit(iu)
         ELSE
            rMAT(IDEST,iu,3)=0.
            rMAT(IDEST,iu,4)=0.
            rMAT(IDEST,iu,5)=0.
         ENDIF
         rMAT(IDEST,iu,6)=RhoFit(iu)
         IF(RhoBethe(iu).GT.0.)THEN
            rMAT(IDEST,iu,7)=RhoAve(nit,iu)/RhoBethe(iu)
            rMAT(IDEST,iu,8)=sRhoAve(iu)/RhoBethe(iu)
            rMAT(IDEST,iu,9)=RhoAve(nit,iu)*Rhow(iu)/RhoBethe(iu)
         ELSE
            rMAT(IDEST,iu,7)=0.
            rMAT(IDEST,iu,8)=0.
            rMAT(IDEST,iu,9)=0.
         ENDIF
         rMAT(IDEST,iu,10)=RhoBethe(iu)
      ENDDO
      outfile='rhosp.rsg'
      comment='Rho, stat, syst errors, rel. to fit, rel. to Bethe'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,40) outfile
 40   FORMAT('Rho, stat, syst errors, rel. to fit, rel. to Bethe:  ',A11)
      YDIM=11
      DO ig=0,XDIM-1
         rMAT(IDEST,ig,0)=SigAve(nit,ig)
         rMAT(IDEST,ig,1)=sSigAve(ig)
         rMAT(IDEST,ig,2)=SigAve(nit,ig)*Sigw(ig)
         IF(SigFit(ig).GT.0.)THEN
            rMAT(IDEST,ig,3)=SigAve(nit,ig)/SigFit(ig)
            rMAT(IDEST,ig,4)=sSigAve(ig)/SigFit(ig)
            rMAT(IDEST,ig,5)=SigAve(nit,ig)*Sigw(ig)/SigFit(ig)
         ELSE
            rMAT(IDEST,ig,3)=0.
            rMAT(IDEST,ig,4)=0.
            rMAT(IDEST,ig,5)=0.
         ENDIF
         rMAT(IDEST,ig,6)=SigFit(ig)
         IF(SigGDR(ig).GT.0.)THEN
            rMAT(IDEST,ig,7)=SigAve(nit,ig)/SigGDR(ig)
            rMAT(IDEST,ig,8)=sSigAve(ig)/SigGDR(ig)
            rMAT(IDEST,ig,9)=SigAve(nit,ig)*Sigw(ig)/SigGDR(ig)
         ELSE
            rMAT(IDEST,ig,7)=0.
            rMAT(IDEST,ig,8)=0.
            rMAT(IDEST,ig,9)=0.
         ENDIF
         rMAT(IDEST,ig,10)=SigGDR(ig)
      ENDDO
      outfile='sigsp.rsg'
      comment='Sig, stat, syst errors, rel. to fit, rel. to GDR'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,41) outfile
 41   FORMAT('Sig, stat, syst errors, rel. to fit, rel. to GDR  :  ',A11)
      WRITE(6,*)'___________________________________________________________'
      WRITE(6,*)' '
      WRITE(6,*)'          Layout of single spectra in matrices'
      WRITE(6,*)'Row  rhosp.rsg      sigsp.rsg            Comment'
      WRITE(6,*)'___________________________________________________________'
      WRITE(6,*)'10   Bethe           GDR             Bethe/GDR'
      WRITE(6,*)' 9   syst.err.(rel)  syst.err.(rel)  systematic errors'
      WRITE(6,*)' 8   stat.err.(rel)  stat.err.(rel)  statistical errors'
      WRITE(6,*)' 7   Rho(rel)        Sig(rel)        relative to Bethe/GDR'
      WRITE(6,*)' 6   fit             fit             fit'
      WRITE(6,*)' 5   syst.err.(rel)  syst.err.(rel)  systematic errors'
      WRITE(6,*)' 4   stat.err.(rel)  stat.err.(rel)  statistical errors'
      WRITE(6,*)' 3   Rho(rel)        Sig(rel)        relative to fit'
      WRITE(6,*)' 2   syst error      syst error      systematic errors'
      WRITE(6,*)' 1   stat error      stat error      statistical errors'
      WRITE(6,*)' 0   Rho             Sig             Rho(U)/Sig(Eg)'
      STOP
 99   WRITE(6,*)'Could not open file for results and spectra'
      END
C -----------------------------------------------------------------------------
      FUNCTION gM(Ex)
C Gamma-Multiplicity as function of Ex (in keV)
      a=42.12/100.
      b=0.0467/100.
      c=-0.128868E-05/100.
      gM=0.
      IF(Eg.GT.0.AND.EG.LT.10000.)gM=a+b*Eg+c*Eg*Eg
      IF(Eg.GE.10000.)gM=a+b*10000.+c*10000.*10000.
      IF(gM.LT.0.)gM=0.
      RETURN
      END
C -----------------------------------------------------------------------------
      SUBROUTINE ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,Di,Df)
C The most magnificant stretch- and compress-routine
C ever created by a human beeing. It is complicated, but works! 
C The routine streches or compresses spectrum from initial
C calibration (A0i,A1i) to final (A0f,A1f). The dimensions
C of the real spectra are Di and Df. First channel is 0, so
C that the spectra occupy (0:D-1) channels.
C August 1994, Oslo Cyclotron Laboratory, Magne Guttormsen
      INTEGER Di,Df
      DIMENSION Fi(0:Di-1),Ff(0:Df-1)
C Testing
      IF(A1i.EQ.0.OR.A1f.EQ.0)RETURN
C Zeroing final spectrum
      DO i=0,Df-1
         Ff(i)=0.  
      ENDDO
C Case where no action is taken
      IF(A0i.EQ.A0f.AND.A1i.EQ.A1f)THEN
         DO i=0,MIN0(Di-1,Df-1)
            Ff(i)=Fi(i)
         ENDDO
      RETURN
      ENDIF

C Taking counts in initial spectrum and find where
C to put it in final spectrum. Then it is distributed on
C the channel(s). The loop goes through all the
C channels i of the initial spectrum
      IF(ABS(A1i/A1f).LT.2)THEN
         DO i=0,Di-1
            IF(Fi(i).EQ.0)GO TO 97
            EiL=A0i+A1i*(i-0.5)      !Step 1.0 chs left and right
            EiH=A0i+A1i*(i+0.5)
            CHf1=(EiL-A0f)/A1f       !CHf1 and CHf2 define limits where
            CHf2=(EiH-A0f)/A1f       !to put the counts in final spc.
            CHlength=ABS(CHf1-CHf2)  !Number of channels (float)
            CountCH=Fi(i)/CHlength   !Number of counts pr.ch unit
            CHfL=CHf1
            CHfH=CHf2
            IF(CHfL.GT.CHfH)THEN
               CHfL=CHf2
               CHfH=CHf1
            ENDIF
            j1=CHfL+0.5              
            j2=CHfH+0.5                     !filling with CHwidth*CountCH 
            IF(j1.GE.Df.OR.j2.LT.0)GO TO 97
            nch=j2-j1+1
            IF(nch.EQ.1)THEN                !One channel to fill
               IF(j1.GE.0)Ff(j1)=Ff(j1)+Fi(i)
               GO TO 97
            ENDIF
            IF(nch.GT.1)THEN                !Two or three channels to fill
               Counts=CountCH*(j1+0.5-CHfL) !Fraction of left channel
               IF(j1.GE.0)Ff(j1)=Ff(j1)+Counts       
               Counts=CountCH*(CHfH+0.5-j2) !Fraction of right channel
               IF(j2.LE.Df-1)Ff(j2)=Ff(j2)+Counts
               DO j=j1+1,j2-1               !Filling in for whole chs.
                  IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+CountCH
               ENDDO
            ENDIF
 97         CONTINUE
         ENDDO
      ELSE

C The counts will be distributed in the streching procedure as a triangle,
C which has its left and right tail overlaping with the center of the 
C next triangle. The heighth and basis of the triangle is called h and b:
C               x         x          x
C             x   x     x   x      x   x
C           x       x x       x   x      x
C         x          0          0          x
C       x          x   x      x  x           x
C     x          x       x  x      x           x
C   x          x           0         x           x
         b=2.0*A1i/A1f   !basis of triangle
         h=2.0/b         !height of triangle in order to get area=1
         alpha=h/(b/2.)  !slope of triangle tails
         DO i=0,Di-1
            IF(Fi(i).EQ.0)GO TO 98
            EiL=A0i+A1i*(i-1.)      !Step 1.0 chs left and right
            EiH=A0i+A1i*(i+1.)
            CHf1=(EiL-A0f)/A1f      !CHf1 and CHf2 define limits where
            CHf2=(EiH-A0f)/A1f      !to put the counts in final spc.
            CHfL=CHf1
            CHfH=CHf2
            IF(CHfL.GT.CHfH)THEN
               CHfL=CHf2
               CHfH=CHf1
            ENDIF
            j1=CHfL+1             
            j2=CHfH              
            IF(j1.GE.Df.OR.j2.LT.0)GO TO 98
            w=0.
            DO j=j1,j2
               IF(j.LT.CHfL+(b/2.))THEN
                  w=alpha*(j-CHfL)                !up going slope
               ELSE 
                  w=h-alpha*(j-(CHfL+(b/2.)))     !down going slope
               ENDIF
               IF(w.LT.-0.1)WRITE(6,*)'Warning, weight w < 0 : ',w
               IF(w.LT.0)w=0.
               IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+w*Fi(i)
            ENDDO
 98         CONTINUE
         ENDDO
      ENDIF
      END
C -----------------------------------------------------------------------------
      SUBROUTINE Iteration(nit,FgN,RhoAve,SigAve)
C The iteration procedure. See T.Tveter et al., Phys. Rev. Lett. 1996
      COMMON/iter1/N_Eg(0:511),N_U(0:511),imin,imax,igmin
      COMMON/iter2/Rho(0:511,0:511),Sig(0:511,0:511)
      REAL RhoAve(0:101,0:511),SigAve(0:101,0:511)
      REAL FgN(0:511,0:511)
      REAL RhoSum,SigSum

C Start iteration
      DO it=1,nit
         DO ix=imin,imax
            DO ig=igmin,ix
               iu=ix-ig
               IF(RhoAve(it-1,iu).LE.0)THEN
                  Sig(ig,ix)=0.
               ELSE
                  Sig(ig,ix)=FgN(ig,ix)/RhoAve(it-1,iu)
               ENDIF
            ENDDO
         ENDDO
         DO ig=igmin,imax
            SigSum=0.
            DO ix=imin,imax
               SigSum=SigSum+Sig(ig,ix)
            ENDDO
            SigAve(it,ig)=SigSum/N_Eg(ig)
         ENDDO
         DO ix=imin,imax         
            DO ig=igmin,ix
               iu=ix-ig
               IF(SigAve(it,ig).LE.0.)THEN
                  Rho(iu,ix)=0.
               ELSE
                  Rho(iu,ix)=FgN(ig,ix)/SigAve(it,ig)
               ENDIF                                  
            ENDDO
         ENDDO
         DO iu=0,imax-igmin
            RhoSum=0.
            DO ix=imin,imax
               RhoSum=RhoSum+Rho(iu,ix)
            ENDDO
            RhoAve(it,iu)=RhoSum/N_U(iu)
         ENDDO
      ENDDO
      RETURN
      END
C -----------------------------------------------------------------------------
      FUNCTION Finvert(y0)
C Inverting the monoton increasing function r=F(z) -> z=Finv(r) 
C This means to find the z-value giving the value y0 
C The function F is the cummulative Gauss function F=1/2(1+erf(z/sqrt(2)))
      REAL xl,xh,yl,yh,x,y,y0
      xl=-3.0
      xh=3.0
      x=0.0
      yl=0.0
      yh=1.0
      y=0.5
      DO WHILE(ABS(y-y0).GT.0.001)
         x=xl+(xh-xl)*(y0-yl)/(yh-yl)
         y=0.5*(1.+erf(x/1.414213562))
         IF(y.GT.y0)THEN
            yl=y
            xl=x
         ELSE
            yh=y
            xh=x
         ENDIF
      ENDDO
      Finvert=x
      RETURN
      END
