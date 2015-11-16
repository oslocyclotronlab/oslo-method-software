      PROGRAM RhoSigChi 
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
      COMMON/iter/imin,imax,igmin,nit
C Stuff for the rhosig iteration
      REAL Spec(0:8191),Calib(6)
      REAL Rho(0:100,0:511),Rhov(0:100,0:511)
      REAL Sig(0:100,0:511),Sigv(0:100,0:511)
      REAL SumFg(0:511),SumFgv(0:511)
      REAL Fg(0:511,0:511),FgTeo(0:511,0:511),FgN(0:511,0:511)
      REAL Fgv(0:511,0:511),sFg(0:511,0:511),sFgN(0:511,0:511)
      REAL sRho(0:511),sSig(0:511)
      REAL rRho(0:511),rSig(0:511),Sum,Sum2,err(1:1000),serr(1:1000)
      REAL RhoBethe(0:8191),RhoThermo(0:8191),SigGDR(0:511)
      REAL Chi(0:100),mass,density,factor,a1,a0,Delta
      REAL Emin,Emax,Emr,Emid,signorm,Cgdr,FWHM,Eres,Ex,Eg,Eu,Etheo
      REAL sum,sumx,sumxx,sumy,sumxy,Efitmax,Efitmin                  !Best fit
      REAL RhoLn(0:511),RhoFit(0:511),C,Ct,T,weight                   !Best fit
      REAL SigLn(0:511),SigFit(0:511),n,Cn,alpha                      !Best fit
      INTEGER time,nunc,degrees,deg,dim,ix,ig,iu,iend,iemr,step,in0
      INTEGER ism,istart,istop,ll,j
      REAL xfit(1:512),yfit(1:512),y0,y1,y2,x0,x1,x2,x3,x4,d0,d1,d2,d3
      REAL tmp(0:511),coef(3)
      INTEGER fitmin,fitmax                                           !Best fit
      DIMENSION Fi(0:511),Ff(0:511)
      CHARACTER*3 G1,G2
      CHARACTER*9 FORM1
      CHARACTER*16 FORM2
      CHARACTER*9 FORM3
      WRITE(6,*)' ______________________________________'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|       R H O S I G C H I  1.2.3       |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|  Program to calculate level density  |'
      WRITE(6,*)'| Rho, and gamma strength function Sig |'
      WRITE(6,*)'| from first-generation spectra, using |'
      WRITE(6,*)'|   FgNorm(Ex,Eg)=Rho(Ex-Eg)*Sig(Eg)   |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|      Oslo Cyclotron Laboratory       |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|        Created: 13/08 - 1999         |'
      WRITE(6,*)'|           Andreas Schiller           |'
      WRITE(6,*)'|  Lisbeth Bergholt, Magne Guttormsen  |'
      WRITE(6,*)'|                                      |'
      WRITE(6,*)'|        Improved: 01/05 - 2003        |'
      WRITE(6,*)'|           Alexander Voinov           |'
      WRITE(6,*)'|  Magne Guttormsen, Andreas Schiller  |'
      WRITE(6,*)'|        Dubna, Oslo, Livermore        |'
      WRITE(6,*)'|______________________________________|'
C Things that should be done in the future:
C Fitting with error weighting
C Finding a better parametrization of the strength function
C Initializing parameter values
      nit=50                  ! Number of iterations
      Emin=4000.0             ! Default lower Ex for 1.gen. spec.
      Emax=8000.0             ! Default higher Ex for 1.gen. spec.
      Egmin=1000              ! Default lowest gamma energy
      nunc=100                ! Number of extractions
C Reading first-generation mama-matrix
      IDEST=1
      ITYPE=3
      WRITE(6,*)'Please, answer 1 and the name of your input first-'
      WRITE(6,*)'generation matrix in the two next questions... '
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
C Input some parameters from keyboard
C Input lower limit for gammas used in the extraction
      igmin=INT(((Egmin-a0)/a1)+0.5)
      Egmin=a0+a1*igmin
      WRITE(6,10)Egmin
 10   FORMAT('Lower limit of gamma energy (keV)      <',F7.1,'>:',$)
      CALL READF(5,Egmin)
      igmin=INT(((Egmin-a0)/a1)+0.5)
      Egmin=a0+a1*igmin
      imin=INT(((Emin-a0)/a1)+0.5)
      imin=MAX(imin,igmin)
      Emin=a0+a1*imin
      WRITE(6,11)Emin
 11   FORMAT('Lower limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Emin)
      imin=INT(((Emin-a0)/a1)+0.5)
      Emin=a0+a1*imin
      IF(Emin.LT.Egmin)THEN
         WRITE(6,'(A29)')'Sorry, Emin<Egmin not allowed'
         STOP
      ENDIF
      imax=INT(((Emax-a0)/a1)+0.5)
      imax=MIN(imax,dim)
      Emax=a0+a1*imax
      WRITE(6,12)Emax
 12   FORMAT('Upper limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Emax)
      imax=INT(((Emax-a0)/a1)+0.5)
      imax=MIN(imax,dim)
      Emax=a0+a1*imax
      IF(imax.GT.511)THEN
         WRITE(6,13)Emax,a1
 13      FORMAT('Sorry, Emax/a1 = ',F7.1,'/',F5.1,' > 511 channels')
         STOP
      ENDIF
      Emr=Emax-Egmin
      iemr=INT(((Emr-a0)/a1)+0.5)
      Emr=a0+a1*iemr
      WRITE(6,14)a0,a1,dim,dim,Emin,Emax,a0,Emr,Egmin,Emax
 14   FORMAT('Common calibration is a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     + /,'Dimension is',I3,' x',I3, 
     + /,'excitation energy region is ',F5.0,'keV to ',F5.0,'keV',
     + /,'rho extracted from ',F5.0,'keV to ',F5.0,'keV',
     + /,'sig extracted from ',F5.0,'keV to ',F5.0,'keV')
C Finding number of counts in Fg(ig,ix) for each Ex 
      DO ix=imin,imax   
         SumFg(ix)=0.
         DO ig=igmin,ix
            SumFg(ix)=SumFg(ix)+Fg(ig,ix) 
         ENDDO
         DO ig=igmin,ix
            IF(SumFg(ix).LE.0.)THEN
               FgN(ig,ix)=0.
            ELSE
               FgN(ig,ix)=Fg(ig,ix)/SumFg(ix)
            ENDIF
         ENDDO
      ENDDO
C Start value for Rho and Sig
      DO iu=0,imax-igmin
         Rho(0,iu)=1.
      ENDDO
      DO ig=igmin,imax
         Sig(0,ig)=0.
         DO ix=MAX(imin,ig),imax
            Sig(0,ig)=Sig(0,ig)+FgN(ig,ix)
         ENDDO
      ENDDO
C Statistical errors
C Calculating statistical error of first generation spectra
      DO ix=imin,imax
         Ex=a0+a1*ix
         sFmax=0.
         isFEg=0
         TotM=MAX(1.,gM(Ex))            !Number of gammas for 1.gen. 
         BckM=MAX(0.,gM(Ex)-1.)         !Number og gammas from 2.+3.+... gen.
         DO ig=igmin,ix
C Fg=total-background NaI spectra, factor 2 due to unfolding
C Total and background error are assumed to be independent from each 
C other, therefore we use SQRT(tot+bck) instead of SQRT(tot)+SQRT(bck)
            sFg(ig,ix)=2.*SQRT((TotM+BckM)*Fg(ig,ix))
            IF(sFg(ig,ix).GT.sFmax)THEN
               isFEg=ig
               sFmax=sFg(ig,ix)
            ENDIF
         ENDDO
C The factor 2 due to unfolding is very uncertain, it could be anything...
C We treat sFg(ig,ix) from Eg=Egmin up to gamma energy (isFEg) for maximum 
C uncertainty (sFmax) in a special way (we have high sFg around Eg=0)
C The Factor 1 is again very uncertain
         IF(isFEg.GT.igmin)THEN
            DO ig=igmin,isFEg
               sFg(ig,ix)=sFmax*(1.+1.*(FLOAT(isFEg)-FLOAT(ig))/FLOAT(isFEg))
            ENDDO
         ENDIF
C As a further twist, we will smooth the errors (one-dimensionally) for each
C first generation spectrum. The smoothing is done over gamma energy intervals 
C of 3 MeV. This is new for version 1.2 of rhosigchi. Smoothing is performed 
C on the tmp() array.
         DO ig=igmin,ix
            tmp(ig)=MAX(2.,sFg(ig,ix))  ! no errors less than 2 counts
         ENDDO
         ism=NINT(1500./a1)               ! +/-ism defines fit-region in channels
C The smoothing procedure of Andreas starts, fasten seatbelts...
         DO ig=igmin,ix
            istart=ig-ism
            istop=ig+ism
            ll=0
            IF(istart.LT.igmin)istart=igmin
            IF(istop.GT.ix)istop=ix
            DO j=istart,istop
               ll=ll+1
               xfit(ll)=float(j)
               yfit(ll)=tmp(j)
            ENDDO
            IF(ll.GT.3)THEN
               y0=0.
               y1=0.
               y2=0.
               x0=0.
               x1=0.
               x2=0.
               x3=0.
               x4=0.
               DO j=1,ll
                  y0=y0+yfit(j)
                  y1=y1+yfit(j)*xfit(j)
                  y2=y2+yfit(j)*xfit(j)**2.
                  x0=x0+1.
                  x1=x1+xfit(j)
                  x2=x2+xfit(j)**2.
                  x3=x3+xfit(j)**3.
                  x4=x4+xfit(j)**4.
               ENDDO
               d0=x4*x2*x0+x3*x2*x1+x3*x2*x1-x4*x1*x1-x3*x3*x0-x2*x2*x2
               d1=y2*x2*x0+y1*x2*x1+y0*x3*x1-y2*x1*x1-y1*x3*x0-y0*x2*x2
               d2=y2*x2*x1+y1*x4*x0+y0*x3*x2-y2*x3*x0-y1*x2*x2-y0*x4*x1
               d3=y2*x3*x1+y1*x3*x2+y0*x4*x2-y2*x2*x2-y1*x4*x1-y0*x3*x3
               IF(d0.NE.0.)THEN
                  coef(1)=d1/d0
                  coef(2)=d2/d0
                  coef(3)=d3/d0
                  sFg(ig,ix)=coef(1)*float(ig)**2.+coef(2)*float(ig)+coef(3)
               ENDIF
            ENDIF
         ENDDO
C The first generation spectrum at high gamma energies is very uncertain
C due to unfolding. Usually we produce a lot of high energy gamma counts
C in the unfolding procedure. Some channel might nevertheless turn out to
C contain almost zero counts (a factor of 250 less counts than neighbouring 
C channels has been observed). Still these channels should have comparable
C errors to their neighbouring channels. This will be taken into account in 
C the following: From gamma energies greater than Emin, the error in the 
C first-generation spectrum must not change by more than +/-30% per increasing 
C gamma bin. No errors are less than 2 counts.
         DO ig=igmin+1,ix
            sFg(ig,ix)=MAX(0.7*sFg(ig-1,ix),sFg(ig,ix)) !to avoid large decrease in error
            sFg(ig,ix)=MIN(1.3*sFg(ig-1,ix),sFg(ig,ix)) !to avoid large increase in error
            sFg(ig,ix)=MAX(2.,sFg(ig,ix))               !to avoid too small errors
         ENDDO
      ENDDO
C Normalizing sFg(Eg,Ex)
      DO ix=imin,imax         
         DO ig=igmin,ix
            IF(SumFg(ix).LE.0.)THEN
               sFgN(ig,ix)=0.
            ELSE
               sFgN(ig,ix)=sFg(ig,ix)/SumFg(ix)
            ENDIF
         ENDDO
      ENDDO  
C Calculating number of degrees of freedom
C Number of data points fitted in first generation spectrum
      degrees=(imax-imin+1)*(imax+imin-2*igmin+2)/2
C Minus number of data points in the fit functions (rho and sigma)
      degrees=degrees-2*(imax-igmin+1)
C Iteration starts here
      CALL Iteration(FgN,sFgN,Rho,Sig)
C Finding extracted average 1. gen. spectra FgTeo and Chi**2
      DO it=0,nit
         deg=degrees
         Chi(it)=0.
         DO ix=imin,imax
            SumFg(ix)=0.
            DO ig=igmin,ix
               iu=ix-ig
               SumFg(ix)=SumFg(ix)+Sig(it,ig)*Rho(it,iu)
            ENDDO
            DO ig=igmin,ix
               iu=ix-ig
               IF(SumFg(ix).GE.0.)THEN
                  FgTeo(ig,ix)=Sig(it,ig)*Rho(it,iu)/SumFg(ix)
               ELSE 
                  FgTeo(ig,ix)=0.
               ENDIF
               IF(sFgN(ig,ix).GT.0.)THEN
                  Chi(it)=Chi(it)+((FgTeo(ig,ix)-FgN(ig,ix))/sFgN(ig,ix))**2.
               ELSE
                  deg=deg-1
C Minus one data point in the calculation of Chi^2
               ENDIF
            ENDDO 
         ENDDO
         IF(deg.GT.0)THEN
            Chi(it)=Chi(it)/FLOAT(deg)
         ELSE
            Chi(it)=0.
         ENDIF
      ENDDO
C Calculating some formats
      nit2=nit/step
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
      step=nit/5
      WRITE(6,*)'_______________________________________________________'
      WRITE(6,15)
 15   FORMAT('        Convergence test using various indicators')
      WRITE(6,FORM1)'Indicator Iteration = 0',(it,it=step,nit,step)
      IF(Rho(0,i1MeV).NE.0.)WRITE(6,FORM2)'Rho/Rho0 at  U=',e1,(Rho(it,i1MeV)/Rho(0,i1MeV),it=0,nit,step)
      IF(Rho(0,i2MeV).NE.0.)WRITE(6,FORM2)'Rho/Rho0 at  U=',e2,(Rho(it,i2MeV)/Rho(0,i2MeV),it=0,nit,step)
      IF(Rho(0,i3MeV).NE.0.)WRITE(6,FORM2)'Rho/Rho0 at  U=',e3,(Rho(it,i3MeV)/Rho(0,i3MeV),it=0,nit,step)
      IF(Sig(0,i1MeV).NE.0.)WRITE(6,FORM2)'Sig/Sig0 at Eg=',e1,(Sig(it,i1MeV)/Sig(0,i1MeV),it=0,nit,step)
      IF(Sig(0,i2MeV).NE.0.)WRITE(6,FORM2)'Sig/Sig0 at Eg=',e2,(Sig(it,i2MeV)/Sig(0,i2MeV),it=0,nit,step)
      IF(Sig(0,i3MeV).NE.0.)WRITE(6,FORM2)'Sig/Sig0 at Eg=',e3,(Sig(it,i3MeV)/Sig(0,i3MeV),it=0,nit,step)
      WRITE(6,FORM3)'Chi^2 for 1.gen.sp. ',(Chi(it),it=0,nit,step)
      WRITE(6,*)'_______________________________________________________'
C Error calculation
C Initializing new Rhov
      DO iu=0,imax-igmin
         Rhov(0,iu)=Rho(0,iu)
      ENDDO
C New iteration were we test the influence of Fg -> Fg + sFg
C One by one channel is changed and (Sig-Sigv)**2
C and (Rho-Rhov)**2 is summed up, and SigmaRho and SigmaF is found
C Initializing arrays
      r=rand(time())               !seeding
      nStep=nunc/10
      DO ix=0,imax-igmin
         sRho(ix)=0.
      ENDDO
      DO ig=igmin,imax
         sSig(ig)=0.
      ENDDO
      DO i=1,nunc
         nTest=(i/nStep)*nStep
         IF(nTest.EQ.i)THEN
            ist=putc('.')
            CALL flush(6)
         ENDIF
         DO ix=imin,imax
            DO ig=igmin,ix
               r=rand(0)
               z=Finvert(r)
               Fgv(ig,ix)=Fg(ig,ix)+z*sFg(ig,ix) !Adding uncertainty
               IF(Fgv(ig,ix).LT.0.)Fgv(ig,ix)=0. !Zeroing negative numbers
            ENDDO
         ENDDO
         DO ix=imin,imax   !Finding number of counts in Fgv(ig,ix) for each Ex
            SumFgv(ix)=0.
            DO ig=igmin,ix
               SumFgv(ix)=SumFgv(ix)+Fgv(ig,ix) 
            ENDDO
C Normalizing Fgv(Eg,Ex)
            DO ig=igmin,ix
               IF(SumFgv(ix).LE.0.)THEN
                  Fgv(ig,ix)=0.
               ELSE
                  Fgv(ig,ix)=Fgv(ig,ix)/SumFgv(ix)
               ENDIF
            ENDDO
         ENDDO
C Initializing new Sigv
         DO ig=igmin,imax
            Sigv(0,ig)=0.
            DO ix=MAX(imin,ig),imax
               Sigv(0,ig)=Sigv(0,ig)+Fgv(ig,ix)
            ENDDO
         ENDDO
         CALL Iteration(Fgv,sFgN,Rhov,Sigv) !Iterating Fgv
         DO ix=0,imax-igmin
            sRho(ix)=sRho(ix)+(Rhov(nit,ix)-Rho(nit,ix))**2.
         ENDDO
         DO ig=igmin,imax
            sSig(ig)=sSig(ig)+(Sigv(nit,ig)-Sig(nit,ig))**2.
         ENDDO
C Calculating relative errors
         Sum=0.
         Sum2=0.
         in0=0
         DO ix=0,imax-igmin
            IF(Rho(nit,ix).GT.0.)THEN
               rRho(ix)=SQRT(sRho(ix)/FLOAT(i))/Rho(nit,ix)
               in0=in0+1
            ELSE
               rRho(ix)=0.
            ENDIF
            Sum=Sum+rRho(ix)
         ENDDO
         DO ig=igmin,imax
            IF(Sig(nit,ig).GT.0.)THEN
               rSig(ig)=SQRT(sSig(ig)/FLOAT(i))/Sig(nit,ig)
               in0=in0+1
            ELSE
               rSig(ig)=0.
            ENDIF
            Sum=Sum+rSig(ig)
         ENDDO  
         IF(in0.GT.0)THEN
            Sum=Sum/FLOAT(in0)
         ELSE
            Sum=0.
         ENDIF
         DO ix=0,imax-igmin
            IF(Rho(nit,ix).GT.0.)Sum2=Sum2+(rRho(ix)-Sum)**2.
         ENDDO
         DO ig=igmin,imax
            IF(Sig(nit,ig).GT.0.)Sum2=Sum2+(rSig(ig)-Sum)**2.
         ENDDO
         err(i)=Sum
         IF(in0.GT.1)THEN
            serr(i)=SQRT(Sum2/FLOAT(in0-1))
         ELSE
            serr(i)=0
         ENDIF
C End Calculating relative errors
      ENDDO
      DO ix=0,imax-igmin
         sRho(ix)=SQRT(sRho(ix)/FLOAT(nunc))
      ENDDO
      DO ig=igmin,imax
         sSig(ig)=SQRT(sSig(ig)/FLOAT(nunc))
      ENDDO
C The iteration is good to find fine structure in level density and strength function, 
C whereas the gross features like temperature and multipolarity of gamma transitions (n) 
C cannot be determined. We try to solve this problem now:
C Solutions:
C 1.) We multiply rho(E_x-E_g) with C*exp(alpha*(E_x-E_g)) and sig(E_g) with D*exp(alpha*E_g)
C 2.) We fix C and alpha such that we get a reasonable level density parameter 'a' around 
C     the neutron binding energy
C Input energy intervall for the fit of rho-data
      WRITE(6,*)
      WRITE(6,*)'We will now fit the level density to literature'
      Efitmin=Emax-4000.
      fitmin=INT((Efitmin-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      WRITE(6,20)Efitmin
 20   FORMAT('Lower limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmin)
      Efitmax=Emr-1000.
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmax=fitmax*a1+a0
      WRITE(6,21)Efitmax
 21   FORMAT('Upper limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmax)
      fitmin=INT((Efitmin-a0)/a1+0.5)
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      Efitmax=fitmax*a1+a0
      IF((Efitmax.GT.Emr).OR.(Efitmin.GE.Efitmax))STOP 'Sorry, limits out of range'
      WRITE(6,22)Efitmin,Efitmax
 22   FORMAT('Fitting between ',F5.0,' keV and ',F5.0,' keV of excitation energy') 
C Input data of the nucleus
      mass=162.
      WRITE(6,23)mass
 23   FORMAT('Mass number of the resulting nucleus   <',F7.1,'>:',$)
      CALL READF(5,mass)
      density=mass/8.
      WRITE(6,24)density
 24   FORMAT('Level density parameter a in (MeV)^-1  <',F7.1,'>:',$)
      CALL READF(5,density)
      Delta=24./SQRT(mass)
      WRITE(6,25)Delta
 25   FORMAT('Backshift parameter Delta in MeV       <',F7.4,'>:',$)
      CALL READF(5,Delta)
C Level density formula from Gilbert Cameron Eq 5 using Eqs 9 and 11 for sigma
      factor=1./(12.*SQRT(0.1776)*(mass**(1./3.)))
      iend=0
      DO iu=0,(16*imax)
         Eu=a1*iu+a0
         Eu=Eu/1000.
         Eu=Eu-Delta
         IF(Eu.LE.(9./(4.*density)))THEN
            RhoBethe(iu)=factor*EXP(3.)*8.*density/27.
         ELSE IF((density*Eu).LT.1967.)THEN
C EXP(2.*SQRT(1967.))=<3.4E+38=biggest real fortran number
            RhoBethe(iu)=factor*EXP(2.*SQRT(density*Eu))/(Eu*SQRT(Eu*density))
            iend=iu
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
         IF(Rho(nit,iu).GT.0.)THEN
            RhoFit(iu)=RhoBethe(iu)/Rho(nit,iu)
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
         Rho(nit,iu)=C*Rho(nit,iu)*EXP(Eu*alpha)
         sRho(iu)=C*sRho(iu)*EXP(Eu*alpha) 
      ENDDO
      WRITE(6,*)'We will now fit the level density to an exponential'
      Efitmin=3500.
      fitmin=INT((Efitmin-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      WRITE(6,26)Efitmin
 26   FORMAT('Lower limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmin)
      Efitmax=Emr-1000.
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmax=fitmax*a1+a0
      WRITE(6,27)Efitmax
 27   FORMAT('Upper limit for the fit of rho (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmax)
      fitmin=INT((Efitmin-a0)/a1+0.5)
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      Efitmax=fitmax*a1+a0
      IF((Efitmax.GT.Emr).OR.(Efitmin.GE.Efitmax))STOP 'Sorry, limits out of range'
      WRITE(6,28)Efitmin,Efitmax
 28   FORMAT('Fitting between ',F5.0,' keV and ',F5.0,' keV of excitation energy') 
      sumxx=0.
      sumxy=0.
      sumx=0.
      sumy=0.
      sum=0.
      DO iu=fitmin,fitmax
         IF(Rho(nit,iu).GT.0.)THEN
            RhoLn(iu)=LOG(Rho(nit,iu))
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
      DO ig=igmin,imax
         Eg=a1*ig+a0
C What we multiply on Rho, we have to multiply on Sig too
         Sig(nit,ig)=Sig(nit,ig)*EXP(Eg*alpha)
         sSig(ig)=sSig(ig)*EXP(Eg*alpha)
      ENDDO
      signorm=Sig(nit,(igmin+imax)/2)
      Emid=a1*FLOAT((igmin+imax)/2)+a0
C Now calculating a GDR 
C Input data of the nucleus
      WRITE(6,*)'We will now calculate a GDR strength function'
      FWHM=5000.
      WRITE(6,30)FWHM
 30   FORMAT('FWHM of the GDR (keV)                  <',F7.1,'>:',$)
      CALL READF(5,FWHM)
      Eres=15000.
      WRITE(6,31)Eres
 31   FORMAT('Energy centroid of the GDR (keV)       <',F7.1,'>:',$)
      CALL READF(5,Eres)
C Normalizing to Sig(Eg) at Emid
      Cgdr=signorm*((Eres**2.-Emid**2.)**2.+(Emid**2.)*(FWHM**2.))/((Emid**4.)*(FWHM**2.))
      DO ig=igmin,imax
         Eg=a0+a1*ig
         SigGDR(ig)=Cgdr*(Eg**4.)*(FWHM**2.)/((Eres**2.-Eg**2.)**2.+(Eg**2.)*(FWHM**2.))
      ENDDO
C Now fitting the strength function with a E_gamma^n function
C This might be a stupid parametrization, but it is tradition
C Input energy intervall for the fit of sig-data
      WRITE(6,*)'We will now fit a Cn*E_gamma**n to the strength function'
      Efitmin=3500.
      fitmin=INT((Efitmin-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      WRITE(6,32)Efitmin
 32   FORMAT('Lower limit for the fit of sig (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmin)
      Efitmax=Emax-1000.
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmax=fitmax*a1+a0
      WRITE(6,33)Efitmax
 33   FORMAT('Upper limit for the fit of sig (keV)   <',F7.1,'>:',$)
      CALL READF(5,Efitmax)
      fitmin=INT((Efitmin-a0)/a1+0.5)
      fitmax=INT((Efitmax-a0)/a1+0.5)
      Efitmin=fitmin*a1+a0
      Efitmax=fitmax*a1+a0
      IF((Efitmax.GT.Emax).OR.(Efitmin.GE.Efitmax))STOP 'Sorry, limits out of range'
      WRITE(6,34)Efitmin,Efitmax
 34   FORMAT('Fitting between ',F5.0,' keV and ',F5.0,' keV of gamma energy') 
      sumxx=0.
      sumxy=0.
      sumx=0.
      sumy=0.
      sum=0.
      DO ig=fitmin,fitmax 
         IF((Sig(nit,ig).GT.0.).AND.((a1*ig+a0).GT.0.))THEN
            SigLn(ig)=LOG(Sig(nit,ig))
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
      DO ig=igmin,imax
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
            rMAT(IDEST,ig,ix)=FgN(ig,ix)
         ENDDO
      ENDDO
      outfile='fg.rsg'
      comment='Observed first generation matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,36)outfile
 36   FORMAT('Observed first generation matrix written to file:     ',A11)
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=sFgN(ig,ix)
         ENDDO
      ENDDO
      outfile='fgerr.rsg'
      comment='Estimated first generation error matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,37)outfile
 37   FORMAT('Estimated first gen. err. matrix written to file:  ',A11)
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=FgTeo(ig,ix)  
         ENDDO
      ENDDO
      outfile='fgteo.rsg'
      comment='Theoretical first generation matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,38)outfile
 38   FORMAT('Theoret. first generation matrix written to file:  ',A11)
C Calculate relative level density and strength function
C Writing spectra out for PAW
      outfile='rhopaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO iu=0,dim-1
         WRITE(20,*)Rho(nit,iu)
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)sRho(iu)
      ENDDO
      DO iu=0,dim-1
         IF(RhoFit(iu).GT.0.)THEN
            WRITE(20,*)Rho(nit,iu)/RhoFit(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         IF(RhoFit(iu).GT.0.)THEN
            WRITE(20,*)sRho(iu)/RhoFit(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)RhoFit(iu)
      ENDDO
      DO iu=0,dim-1
         IF(RhoBethe(iu).GT.0.)THEN
            WRITE(20,*)Rho(nit,iu)/RhoBethe(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         IF(RhoBethe(iu).GT.0.)THEN
            WRITE(20,*)sRho(iu)/RhoBethe(iu)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO iu=0,dim-1
         WRITE(20,*)RhoBethe(iu)
      ENDDO
      CLOSE(20)
      WRITE(6,39)outfile
 39   FORMAT('Rho, errors, fit, Bethes formula written to file: ',A11)
      outfile='sigpaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO ig=0,dim-1
         WRITE(20,*)Sig(nit,ig)
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)sSig(ig)
      ENDDO
      DO ig=0,dim-1
         IF(SigFit(ig).GT.0.)THEN
            WRITE(20,*)Sig(nit,ig)/SigFit(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         IF(SigFit(ig).GT.0.)THEN
            WRITE(20,*)sSig(ig)/SigFit(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)SigFit(ig)
      ENDDO
      DO ig=0,dim-1
         IF(SigGDR(ig).GT.0.)THEN
            WRITE(20,*)Sig(nit,ig)/SigGDR(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         IF(SigGDR(ig).GT.0.)THEN
            WRITE(20,*)sSig(ig)/SigGDR(ig)
         ELSE
            WRITE(20,*)0.
         ENDIF
      ENDDO
      DO ig=0,dim-1
         WRITE(20,*)SigGDR(ig)
      ENDDO
      CLOSE(20)
      WRITE(6,40)outfile
 40   FORMAT('Sigma, errors, fit, GDR  formula written to file: ',A11)
C Writing spectra out for mama
C Writting spectra to matrices
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.
      YDIM=8
      DO iu=0,XDIM-1
         rMAT(IDEST,iu,0)=Rho(nit,iu)
         rMAT(IDEST,iu,1)=sRho(iu)
         IF(RhoFit(iu).GT.0.)THEN
            rMAT(IDEST,iu,2)=Rho(nit,iu)/RhoFit(iu)
            rMAT(IDEST,iu,3)=sRho(iu)/RhoFit(iu)
         ELSE
            rMAT(IDEST,iu,2)=0.
            rMAT(IDEST,iu,3)=0.
         ENDIF
         rMAT(IDEST,iu,4)=RhoFit(iu)
         IF(RhoBethe(iu).GT.0.)THEN
            rMAT(IDEST,iu,5)=Rho(nit,iu)/RhoBethe(iu)
            rMAT(IDEST,iu,6)=sRho(iu)/RhoBethe(iu)
         ELSE
            rMAT(IDEST,iu,5)=0.
            rMAT(IDEST,iu,6)=0.
         ENDIF
         rMAT(IDEST,iu,7)=RhoBethe(iu)
      ENDDO
      outfile='rhosp.rsg'
      comment='Rho, errors, fit, Bethes formula'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,41)outfile
 41   FORMAT('Rho, errors, fit, Bethes formula written to file:  ',A11)
      YDIM=8
      DO ig=0,XDIM-1
         rMAT(IDEST,ig,0)=Sig(nit,ig)
         rMAT(IDEST,ig,1)=sSig(ig)
         IF(SigFit(ig).GT.0.)THEN
            rMAT(IDEST,ig,2)=Sig(nit,ig)/SigFit(ig)
            rMAT(IDEST,ig,3)=sSig(ig)/SigFit(ig)
         ELSE
            rMAT(IDEST,ig,2)=0.
            rMAT(IDEST,ig,3)=0.
         ENDIF
         rMAT(IDEST,ig,4)=SigFit(ig)
         IF(SigGDR(ig).GT.0.)THEN
            rMAT(IDEST,ig,5)=Sig(nit,ig)/SigGDR(ig)
            rMAT(IDEST,ig,6)=sSig(ig)/SigGDR(ig)
         ELSE
            rMAT(IDEST,ig,5)=0.
            rMAT(IDEST,ig,6)=0.
         ENDIF
         rMAT(IDEST,ig,7)=SigGDR(ig)
      ENDDO
      outfile='sigsp.rsg'
      comment='Sigma, errors, fit, GDR  formula'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,42)outfile
 42   FORMAT('Sigma, errors, fit, GDR  formula written to file:  ',A11)
      WRITE(6,*)'__________________________________________________________'
      WRITE(6,*)'          Layout of single spectra in matrices'
      WRITE(6,*)'Row  rhosp.rsg      sigsp.rsg            Comment'
      WRITE(6,*)'__________________________________________________________'
      WRITE(6,*)' 7   Bethe           GDR             Bethe/GDR'
      WRITE(6,*)' 6   stat.err.(rel)  stat.err.(rel)  statistical errors'
      WRITE(6,*)' 5   Rho(rel)        Sig(rel)        relative to Bethe/GDR'
      WRITE(6,*)' 4   fit             fit             fit'
      WRITE(6,*)' 3   stat.err.(rel)  stat.err.(rel)  statistical errors'
      WRITE(6,*)' 2   Rho(rel)        Sig(rel)        relative to fit'
      WRITE(6,*)' 1   stat error      stat error      statistical errors'
      WRITE(6,*)' 0   Rho             Sig             Rho(U)/Sig(Eg)'
      WRITE(6,*)'__________________________________________________________'
      WRITE(6,*)'We will now write a spectrum used by the Thermo program'
      Etheo=Emr
      itheo=INT(((Etheo-a0)/a1)+0.5)
      Etheo=a1*itheo+a0
      WRITE(6,43)Etheo
 43   FORMAT('Experimental / theoretical limit (keV) <',F7.1,'>:',$)
      CALL READF(5,Etheo)
      itheo=INT(((Etheo-a0)/a1)+0.5)
      Etheo=a1*itheo+a0
      IF(Etheo.GT.Emr)STOP 'Sorry, Etheo out of range'
      WRITE(6,44)FLOAT(a0),Etheo,(Etheo+FLOAT(a1)),(FLOAT(iend)*a1+a0)
 44   FORMAT('Experimental rho from ',F4.0,' keV to ',F5.0,' keV of excitation energy',
     + /,'Theoretical rho from ',F5.0,' keV to ',F6.0,' keV of excitation energy') 
      DO ix=0,itheo
         RhoThermo(ix)=Rho(nit,ix)
      ENDDO
      DO ix=(itheo+1),iend
         RhoThermo(ix)=RhoBethe(ix)
      ENDDO
C Writing spectrum out for PAW
      outfile='rhotmopaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO ix=0,(16*dim-1)
         WRITE(20,*)RhoThermo(ix)
      ENDDO
      CLOSE(20)
      WRITE(6,45)outfile
 45   FORMAT('Semi- experimental level density written to file: ',A13)
C Writing spectrum out for mama
      ITYPE=1
      Calib(1)=a0
      Calib(2)=a1
      Calib(3)=0.0
      MAXCH=16*dim-1
      DO ix=0,(16*XDIM-1)
         SPEC(ix)=RhoThermo(ix)
      ENDDO
      outfile='rhotmosp.rsg'
      comment='Semiexperimental level density'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw1dim(20,comment,(16*XDIM),Spec,Calib)
      CLOSE(20)
      WRITE(6,46)outfile
 46   FORMAT('Semi- experimental level density written to file:  ',A13)
C Writing spectrum out for PAW
      outfile='chipaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO it=0,nit
         WRITE(20,*)Chi(it)
      ENDDO
      CLOSE(20)
      WRITE(6,47)outfile
 47   FORMAT('Chi - square for every iteration written to file:    ',A13)
C Writing spectrum out for PAW
      outfile='erravepaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO i=1,nunc
         WRITE(20,*)err(i)
      ENDDO
      CLOSE(20)
      WRITE(6,48)outfile
 48   FORMAT('Av. rel. error for every extrac. written to file: ',A13)
C Writing spectrum out for PAW
      outfile='errsigpaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO i=1,nunc
         WRITE(20,*)serr(i)
      ENDDO
      CLOSE(20)
      WRITE(6,49)outfile
 49   FORMAT('St. dev. of err. for ev. extrac. written to file: ',A13)
      STOP
 99   WRITE(6,*)'Could not open file for results and spectra'
      STOP
      END
C -----------------------------------------------------------------------------
      FUNCTION gM(Ex)
C Gamma-Multiplicity as function of Ex (in keV)
      a=42.12/100.
      b=0.0467/100.
      c=-0.128868E-05/100.
      gM=0.
      IF(Ex.GT.0.AND.Ex.LT.10000.)gM=a+b*Ex+c*Ex*Ex
      IF(Ex.GE.10000.)gM=a+b*10000.+c*10000.*10000.
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
      SUBROUTINE Iteration(FgN,sFgN,Rho,Sig)
      COMMON/iter/imin,imax,igmin,nit
C Ansatz=FgN(E_x,E_gamma)=rho(E_x-E_gamma)*sigma(E_gamma)
      REAL Rho(0:100,0:511),Sig(0:100,0:511)
      REAL FgN(0:511,0:511),sFgN(0:511,0:511)
      REAL fun1(0:511),fun2(0:511),fun3(0:511)
      REAL nom(0:511,0:511),denom(0:511,0:511)
      REAL up,down,var
      INTEGER ii,it,ix,ig,iu
C Start iteration
      DO it=1,nit
         IF(it.LE.5)THEN
            var=1.2
         ELSE IF(it.LE.12)THEN
            var=1.1
         ELSE IF(it.LE.21)THEN
            var=1.05
         ELSE IF(it.LE.30)THEN
            var=1.025
         ELSE
            var=1.01
         ENDIF
C Calculating two matrices 'nom' and 'denom'
         DO ix=imin,imax
            fun1(ix)=0.
            fun2(ix)=0.
            fun3(ix)=0.
            DO ig=igmin,ix
               iu=ix-ig
               fun1(ix)=fun1(ix)+Sig(it-1,ig)*Rho(it-1,iu)
               IF(sFgN(ig,ix).GT.0.)THEN
                  fun2(ix)=fun2(ix)+(Sig(it-1,ig)*Rho(it-1,iu)/sFgN(ig,ix))**2.
                  fun3(ix)=fun3(ix)+Sig(it-1,ig)*Rho(it-1,iu)*FgN(ig,ix)/(sFgN(ig,ix)**2.)
               ENDIF
            ENDDO
            IF(fun1(ix).GT.0.)THEN
               fun2(ix)=fun2(ix)/(fun1(ix)**3.)
               fun3(ix)=fun3(ix)/(fun1(ix)**2.)
            ELSE
               fun2(ix)=0.
               fun3(ix)=0.
            ENDIF
            DO ig=igmin,ix
               IF((fun1(ix)*sFgN(ig,ix)).GT.0.)THEN
                  nom(ig,ix)=fun2(ix)-fun3(ix)+FgN(ig,ix)/(fun1(ix)*(sFgN(ig,ix)**2.))
                  denom(ig,ix)=1./(fun1(ix)*sFgN(ig,ix))**2.
               ELSE
                  nom(ig,ix)=0.
                  denom(ig,ix)=0.
               ENDIF
            ENDDO
         ENDDO
C Varying a point of sigma
         DO ig=igmin,imax
            up=0.
            down=0.
            ii=MAX(imin,ig)
            DO ix=ii,imax
               iu=ix-ig
               up=up+Rho(it-1,iu)*nom(ig,ix)
               down=down+(Rho(it-1,iu)**2.)*denom(ig,ix)
            ENDDO
            IF(down.GT.0.)THEN
               IF((up/down).GT.(var*Sig(it-1,ig)))THEN
                  Sig(it,ig)=var*Sig(it-1,ig)
               ELSE IF((up/down).LT.(Sig(it-1,ig)/var))THEN
                  Sig(it,ig)=Sig(it-1,ig)/var
               ELSE
                  Sig(it,ig)=up/down
               ENDIF
            ELSE 
               Sig(it,ig)=0.
            ENDIF
         ENDDO
C Varying a point of rho
         DO iu=0,imax-igmin
            up=0.
            down=0.
            ii=MAX(imin,iu+igmin)
            DO ix=ii,imax
               ig=ix-iu
               up=up+Sig(it-1,ig)*nom(ig,ix)
               down=down+(Sig(it-1,ig)**2.)*denom(ig,ix)
            ENDDO
            IF(down.GT.0.)THEN
               IF((up/down).GT.(var*Rho(it-1,iu)))THEN
                  Rho(it,iu)=var*Rho(it-1,iu)
               ELSE IF((up/down).LT.(Rho(it-1,iu)/var))THEN
                  Rho(it,iu)=Rho(it-1,iu)/var
               ELSE
                  Rho(it,iu)=up/down
               ENDIF
            ELSE 
               Rho(it,iu)=0.
            ENDIF
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
