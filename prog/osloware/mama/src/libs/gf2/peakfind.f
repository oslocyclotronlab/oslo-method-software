      SUBROUTINE PEAKFIND
C Routine that searches for peaks in matr./spc.  
      COMMON/PKFIND/xSPEC(8192),NOCH                     
      COMMON/PEAK/NoPeak,Peaks(200,5)

      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM           
      INTEGER XDIM,YDIM,dim                                              
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      CHARACTER APP*4
      CHARACTER*7 MCHAR
      CHARACTER ans*1                                                                                
      CHARACTER*20 DATTIM

      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      DIMENSION XCHA(400),YCHA(400),WG(400),C(5)
      REAL*8 EF(5,5)

      Idev =22 
      disc =0.5
      disct=disc
      Kmax =2
      fw0t =6.
      fw1t =0.
      ans  ='s'

      l1=0                 !Default is singles spectrum
      l2=0
      a0=cal(2,IDEST,1,1)
      a1=cal(2,IDEST,1,2)
      a2=cal(2,IDEST,1,3)
      dim=MAXCH+1
      ism=2
      IF(ITYPE.EQ.1)GO TO 555

      ans='x'              !It is a matrix, and we must ask for things
      ism=1
      WRITE(6,3)ans        
   3  FORMAT(/'Peakfind along x or y axis  <',A1,'>:',$)
      CALL READA1(5,ans)
      IF(ans.EQ.'X'.OR.ans.EQ.'x')THEN
        ans='x'
        dim=XDIM
        l1=0
        l2=YDIM-1
        WRITE(6,4)l1
   4    FORMAT(/'Lower spectrum on y-axis  <',I4,'>:',$)
        CALL READI(5,l1)
        WRITE(6,5)l2
   5    FORMAT( 'Higher spectrum on y-axis <',I4,'>:',$)
        CALL READI(5,l2)
        a0=cal(1,IDEST,1,1)
        a1=cal(1,IDEST,1,2)
        a2=cal(1,IDEST,1,3)
        IF(l1.LT.0.OR.l2.GT.511)Istatus=1
      ENDIF
      IF(ans.EQ.'Y'.OR.ans.EQ.'y')THEN
        ans='y'
        dim=YDIM
        l1=0
        l2=XDIM-1
        WRITE(6,6)l1
   6    FORMAT(/'Lower spectrum on x-axis  <',I4,'>:',$)
        CALL READI(5,l1)
        WRITE(6,7)l2
   7    FORMAT( 'Higher spectrum on x-axis <',I4,'>:',$)
        CALL READI(5,l2)
        a0=cal(1,IDEST,2,1)
        a1=cal(1,IDEST,2,2)
        a2=cal(1,IDEST,2,3)
        IF(l1.LT.0.OR.l2.GT.4095)Istatus=1
      ENDIF

555   IF(Istatus.NE.0)RETURN
      NOCH=dim

C Testing spectra for fwhm and disc=dA/A
C Making peakfind
      itry=0

556   itry=itry+1
      aveno=0
      ino  =0

      avefw=0
      ifw  =0

      fw0  =0
      fw1  =0
      ifws =0

      DO l=l1,l2
        lt=((l+1)/30)*30
        IF(lt.EQ.l+1)THEN
          ist=putc('.')
          call flush(6)
        ENDIF
        isum=0
        DO m=2,dim  
          xSPEC(m)=0
          IF(ans.EQ.'s')xSPEC(m)=rSPEC(IDEST,m-1)
          IF(ans.EQ.'x')xSPEC(m)=rMAT(IDEST,m-1,l)
          IF(ans.EQ.'y')xSPEC(m)=rMAT(IDEST,l,m-1)
          IF(xSPEC(m).LT.0)xSPEC(m)=0
          IF(xSPEC(m).GT.0)NOCH=m
          isum=isum+xSPEC(m)
        ENDDO
        IF(isum.GT.0)THEN
          CALL PEAKFI(fw0t,fw1t,disct,Kmax,' ')
          ino=ino+1
          aveno=aveno+NoPeak
          DO i=1,NoPeak
            cent =Peaks(i,1)
            dcent=Peaks(i,2)
            area =Peaks(i,3)
            darea=Peaks(i,4)
            width=Peaks(i,5)
            ifw=ifw+1
            avefw=avefw+width
            IF(NoPeak.GT.2)THEN
              XCHA(i)=cent
              YCHA(i)=width
              IF(dcent.LT.0.001)dcent=0.001
              WG(i)=(1./dcent)*(1./(darea/area))
            ENDIF
          ENDDO
C Fitting the width as a linear function of channel
          IF(NoPeak.GT.2)THEN
            CALL FITS(XCHA,YCHA,WG,C,EF,2,NoPeak,CHI)
            fw0e=C(1)
            fw1e=C(2)
            IF(fw0e.LT.0.5)fw0e=0.5
            IF(fw0e.GT.NOCH/3)fw0e=NOCH/3
            IF(fw0e+NOCH*fw1e.LT.0.5)   fw1e=(0.5     -fw0e)/NOCH
            IF(fw0e+NOCH*fw1e.GT.NOCH/3)fw1e=((NOCH/3)-fw0e)/NOCH
            fw0=fw0+fw0e
            fw1=fw1+fw1e
            ifws=ifws+1
          ENDIF
        ENDIF
      ENDDO

C Finding reasonable values for fwhm and disc=dA/A (default=0.5)
      IF(ifws.GE.1)THEN
        fw0=fw0/ifws
        fw1=fw1/ifws
      ELSE
        fw0=6.0/0.85
        fw1=0.
        IF(ifw.GT.0.AND.avefw.GT.0)fw0=avefw/ifw
      ENDIF
      aveno=aveno/ino
      IF(aveno.LT. 10)disct=disc/0.5       !disc=0.5/0.5=1, means dA=A
      IF(aveno.GT. 50)disct=disc/1.0
      IF(aveno.GT.100)disct=disc/2.0 
          
      IF(fw0.LT.0.5)   fw0=0.5
      IF(fw0.GT.NOCH/3)fw0=NOCH/3
      IF(fw0+NOCH*fw1.LT.0.5)   fw1=(0.5     -fw0)/NOCH
      IF(fw0+NOCH*fw1.GT.NOCH/3)fw1=((NOCH/3)-fw0)/NOCH
      fw0 =fw0*0.85         !Easier to find peaks with less fwhm
      fw1 =fw1*0.85

      IF(itry.EQ.1)THEN
        fw0t =fw0       
        fw1t =fw1
        GO TO 556             !Searches once more for good parameters
      ENDIF
  
      disc=disct
C Asking for fwhm and disc
      WRITE(6,*)' '
      WRITE(6,*)' '
      WRITE(6,17)
17    FORMAT('The peakfind routine needs an estimate of the',/,
     +       'experimental fwhm given as a linear function of',/,
     +       'channels by fwhm=fw0+fw1*channel, and the threshold',/, 
     +       'for when peaks are listed according to the uncertainty',/,
     +       'in counts: dA/A < threshold' )  

      WRITE(6,14)fw0
14    FORMAT(/'fw0 (ch) in peaksearch <',F7.2,'>:',$)
      CALL READF(5,fw0)
      WRITE(6,15)fw1
15    FORMAT( 'fw1 in peaksearch   <',F10.6,'>:',$)
      CALL READF(5,fw1)

      WRITE(6,16)disc
16    FORMAT( 'Peaks listed when dA/A less than   <',F5.2,'>:',$)
      CALL READF(5,disc)

C Opening output files and writing headings
      CALL DATETIME(DATTIM)
      WRITE(6,*)' '
      WRITE(6,24)
      WRITE(6,19)FNAME(ISM,IDEST),DATTIM(1:12)
      WRITE(6,20)fw0,fw1,disc
      WRITE(6,21)a0,a1,a2
      WRITE(6,22)
      WRITE(6,23)
      WRITE(6,24)

      OPEN (Idev,FILE='peakfit.out',ACCESS='APPEND',IOSTAT=IOS)
      IF(IOS.EQ.0)THEN
        WRITE(Idev,24)
        WRITE(Idev,19)FNAME(ISM,IDEST),DATTIM(1:12)
        WRITE(Idev,20)fw0,fw1,disc
        WRITE(Idev,21)a0,a1,a2
        WRITE(Idev,22)
        WRITE(Idev,23)
        WRITE(Idev,24)
      ENDIF

19    FORMAT('Peakfit results for file:     ',A,' at ',A)
20    FORMAT('fw0 = ',F7.1,' ch  and fw1= ',F7.3,'    (dA/A) < ',F5.2)
21    FORMAT('Calibration (a0,a1,a2)=',F8.1,F9.3,E12.4,/)
22    FORMAT('Spec Centroid  fwhm  Counts(dCount)  Energy(dEnergy)   fwhm')
23    FORMAT('(ch)   (ch)    (ch)                   (keV)    (keV)  (keV)')
24    FORMAT('===========================================================')
25    FORMAT(  I4,  F8.1,    F7.1,    I8,    I8,    F9.1,      F8.1,  F7.1)


C Making peakfind
      iPeaktest=0
      DO l=l1,l2
        isum=0
        DO m=2,dim  
          xSPEC(m)=0
          IF(ans.EQ.'s')xSPEC(m)=rSPEC(IDEST,m-1)
          IF(ans.EQ.'x')xSPEC(m)=rMAT(IDEST,m-1,l)
          IF(ans.EQ.'y')xSPEC(m)=rMAT(IDEST,l,m-1)
          IF(xSPEC(m).LT.0)xSPEC(m)=0
          IF(xSPEC(m).GT.0)NOCH=m
          isum=isum+xSPEC(m)
        ENDDO
        IF(isum.GT.0)THEN
          CALL PEAKFI(fw0,fw1,disc,Kmax,ans)
          DO i=1,NoPeak
            iPeaktest=1
            cent =Peaks(i,1)
            dcent=Peaks(i,2)
            area =Peaks(i,3)
            darea=Peaks(i,4)
            width=Peaks(i,5)
            iarea  =area+0.5
            idarea =darea+0.5
            energy =a0+a1*cent+a2*cent*cent
            denergy=(a1+2.0*a2*cent)*dcent
            efw    =(a1+2.0*a2*cent)*width
            WRITE(6,   25)l,cent,width,iarea,idarea,energy,ABS(denergy),ABS(efw)
            IF(IOS.EQ.0)WRITE(Idev,25)l,cent,width,iarea,idarea,energy,ABS(denergy),ABS(efw)
          ENDDO
          IF(NoPeak.GE.1)THEN
            WRITE(6   ,*)' '
            IF(IOS.EQ.0)WRITE(Idev,*)' '
          ENDIF
        ENDIF
      ENDDO
      IF(iPeaktest.EQ.0) WRITE(6,*)'No peaks found'

      CLOSE(Idev)
      IF(IOS.EQ.0)WRITE(6,*)'Results also written in file: peakfit.out'

C Display markers and energies at peak positions
      IF(ans.EQ.'s')THEN
        CALL INITG(NX,NY)
        L=10
        IXold=0
        IYold=0
        DO i= 1,NoPeak
          cent =Peaks(i,1)
          IF(cent.GE.LOCH.AND.cent.LE.HICH)THEN
            energy =a0+a1*cent+a2*cent*cent
            ICH = cent + 0.5
            Y = xSPEC(ICH+1)
            X = cent + 0.5
            WRITE(MCHAR,'(F7.1)')energy
            CALL CVXY(X,Y,IX,IY,1)
            IF(ABS(IYold-IY).LT.12.AND.ABS(IXold-IX).LT.40)THEN
              L=L+12+(IYold-IY)
              IF(L.GT.50)L=10            !Resets again (only 3 times stacked)
            ELSE
              L=10                       !Good place to write energy
            ENDIF 
            IF(IY.GT.NY-20)IY=NY-20
            CALL MSPOT(IX,IY+10)
            CALL IVECT(IX,IY+10+L)
            CALL MSPOT(IX,IY+15+L)
            CALL PUTG(MCHAR,7,5,1)
            IXold=IX
            IYold=IY+(L-10)
          ENDIF
        ENDDO
        CALL FINIG
      ENDIF
      RETURN
      END


      SUBROUTINE PEAKFI(fw0,fw1,disc,Kmax,ans)
C Taken from W.W.Black Nucl.Instr. and Methods 71(1969)317
C and A.L.Connelly and W.W.Black Nucl.Instr. and Methods 82(1970)141
C Peakfind routine implemented by Finn Ingebretsen 1977
C Modified for Mama by Magne Guttormsen 1995, new MATINV, new fwhm as
C function of channel, consistent fwhm of peak and correlation peak
C
C Experimental fwhm given by      fwhme=fw0+fw1*ch
C The fwhm of correlation peak is fwhmc=0.66667*fwhme

      DIMENSION SPEC(8192),F(400),F1(400)
      DIMENSION PKS(200),JMARK(200)
      INTEGER SPEC
      COMMON/PKFIND/xSPEC(8192),NOCH
      COMMON/PEAK/NoPeak,Peaks(200,5)
      REAL*8 EF(5,5)
      DIMENSION XCHA(400),YCHA(400),WG(400),C(5)
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      CHARACTER ans*1

C  MINST is smallest number of background channels on one side of peak
      DO I=1,NOCH
        SPEC(I)=0
      ENDDO
      N0=0
      cent=0.
      SumInt=0.
      PKS(1)=0.
      iLow =max0(INT(1+2*fw0),2)
      iHigh=min0(INT(NOCH-1-2*(fw0+fw1*NOCH)),NOCH-2)
      NOCHdelta=NOCH/20                     !Calculate 20 times the fwhm
      IF(NOCHdelta.EQ.0)NOCHdelta=1
      IF(fw0.LT.0.001)RETURN
      IF(NOCH.LT.5)RETURN
      IF(iLow.GE.iHigh-2)RETURN
      DO 10 I=iLow,iHigh                    !First big loop
C  Assumes Gaussian correlation shape
C  con1=2.*SQRT(ALOG(2.)/3.1415926)/fwhmc
C  con2=4.*ALOG(2.)/(fwhmc)**2
       
      IF(INT((I-iLow)/NOCHdelta)*NOCHdelta.EQ.(I-iLow))THEN
        fwhme=fw0+I*fw1
        IF(fwhme.GT.199)fwhme=199
        fwhmc=0.666667*fwhme
        con1 =0.939437/fwhmc
        con2 =2.772589/(fwhmc*fwhmc)
        K=fwhme+0.5
        NBCKGD=2*K
        MINST=NBCKGD/4
        IF(MINST.LT.2)MINST=2
        KK=2*K+1
        DO j=1,KK
          F(j) =con1*EXP(-con2*(j-K-1)**2)
          F1(j)=(1./KK-F(j))**2
        ENDDO

      ENDIF

      sum=0.
      VAR=0.
      IK1=max(I-K,1)
      IK2=min(I+K,NOCH)
      sum2=0.
C sum2 is the correlation function
      DO J=IK1,IK2
        TMP=xSPEC(J)
        sum=sum+TMP
        JIK1=J-I+K+1
        VAR=VAR+F1(JIK1)*TMP
        sum2=sum2+F(JIK1)*TMP
      ENDDO

      B=0
      IF(VAR.GT.0.01)B=1.6*SQRT(VAR)
      sum2=sum2-sum/KK-B

C Test if correlation is significant
      IF(sum2.LE.0.)GO TO 725

C Yes, was last  0 or -1
      IF(SPEC(I-1).EQ.1)GO TO 720

C  Last was 0 or -1, initiates store of this peak
      PEAK=sum2+B
      i1=I
      nPeak=1

C  nPeak is the peaknumber. i1 is first channel
 720  sum2=sum2+B
      nPeak=nPeak+1
      IF(nPeak.GT.200)nPeak=200
      PKS(nPeak)=sum2
      cent=cent+(I-1)*sum2
      SumInt=SumInt+sum2
      SPEC(I)=1
      IF(sum2.GT.PEAK)PEAK=sum2
      GO TO 10

C Insignificant correlation, was last time 1
  725 IF(SPEC(I-1).NE.1)GO TO 10

C Peak is just passed
      nPeak=nPeak+1
      IF(nPeak.GT.200)nPeak=200
      PKS(nPeak)=0.
      i2=I-1
      N0=N0+1
      Peaks(N0,1)=cent/SumInt
      cent=0.
      SumInt=0.

C  Finding width of correlation peak
      PEAK=PEAK*0.5
      KL=1
      DO 46 J=2,nPeak
      GO TO(47,48),KL
   47 IF(PKS(J).LT.PEAK)GO TO 46
      CH11=(J-1)+(PEAK-PKS(J-1))/(PKS(J)-PKS(J-1))
      KL=2
      GO TO 46
   48 IF(PKS(J).GT.PEAK)GO TO 46
      CH22=(J-1)+(PEAK-PKS(J-1))/(PKS(J)-PKS(J-1))
      GO TO 50
   46 CONTINUE

C  Storing width
   50 Peaks(N0,2)=(CH22-CH11)    

C  IGNORE ONE-CHANNEL "SPIKES"
      INTPK=Peaks(N0,2)*100.
      IF(INTPK.NE.100)GO TO 49
      N0=N0-1
      GO TO 10
  49  IF(N0.LT.200)GO TO 11
      IF(Idev.NE.0)WRITE(Idev,650)
  650 FORMAT('Peak array filled')
      GO TO 13
   11 MEXP=(KK+i1-i2-1)/2.+0.5
      IF(MEXP.LE.0)GO TO 10

C  The input width is larger than the calculated
C  Making forbidden region for background
      JJJ1=i1-MEXP
      JJJ2=i1-1
      DO J=JJJ1,JJJ2
        IF(SPEC(J).NE.1)SPEC(J)=-1
      ENDDO
      JJJ1=i2+1
      JJJ2=i2+MEXP
      DO J=JJJ1,JJJ2
        IF(SPEC(J).NE.1)SPEC(J)=-1
      ENDDO

   10 CONTINUE
   13 CONTINUE
      KSIGF=0
      iBac=0
      IMX=N0
      Peaks(N0+1,1)=100000.
      MARKR=1
      JMARK(1)=100000
      N0=0

      DO 40 II=1,IMX                    !Second big loop
      cent=Peaks(II,1)
      width=Peaks(II,2)
      icent=cent+0.5
      fwhme=fw0+cent*fw1
      IF(fwhme.GT.199)fwhme=199
      fwhmc=0.666667*fwhme
      K=fwhme+0.5
      NBCKGD=2*K
      MINST=NBCKGD/4
      IF(MINST.LT.2)MINST=2
      KK=2*K+1
 
      IB=icent+1
  749 IF(SPEC(IB).EQ.1) GO TO 750
      i1=IB
      GO TO 751
  750 IB=IB-1
      GO TO 749
  751 IB=icent
  752 IB=IB+1
      IF(SPEC(IB).EQ.1) GO TO 752
      i2=IB-1

C Finding area of real peak
      BCKGD=0.
      MEXP=(KK+i1-i2-1)/2.+0.5
      IF(MEXP.LT.0)MEXP=0
      JJJ3=i1-MEXP
      JJJ4=i2+MEXP
      IF(iBac.EQ.1)GO TO 73
      JJJ=i1-MEXP
      LCASE=1
      LBCGD=0
      DO 51 J=1,NBCKGD
   52 JJ=JJJ-J
      IF(JJ.LE.0)GO TO 40
      IF(SPEC(JJ).NE.0)GO TO 60
      XCHA(J)=JJ
      YCHA(J)=xSPEC(JJ)
      WG(J)=0.5
      IF(YCHA(J).GE.10.)WG(J)=1./YCHA(J)
      LBCGD=LBCGD+1
      GO TO 51
   60 IF(LBCGD.GE.MINST)GO TO 61
      LCASE=2
      JJJ=JJJ-1
      GO TO 52
   51 CONTINUE
   61 JJJ=i2+MEXP
      JLOW=JJ-1
      JSTR=LBCGD
      MCASE=1
      MBCGD=0
      DO 54 J=1,NBCKGD
   55 JJ=JJJ+J
      IF(JJ.GE.NOCH)GO TO 40
      IF(SPEC(JJ).NE.0)GO TO 70
      JARR=J+JSTR
      IF(JARR.GT.400)JARR=400
      XCHA(JARR)=JJ
      YCHA(JARR)=xSPEC(JJ)
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
      IF(NPTS.GT.400)NPTS=400
      JHIG=JJ-1
      DO 72 J=1,NPTS
   72 XCHA(J)=XCHA(J)-JJ
C Fitting the background
      CALL FITS(XCHA,YCHA,WG,C,EF,Kmax,NPTS,CHI)
   73 ARVAR=0.
      PKVAR=0.
      sum=0.

C  Check doublets and their integration limits
      iBac=0
      IF(II.EQ.1)GO TO 74
      ICDIF=Peaks(II-1,1)+0.5

C  Is low limit for plot below last peak
      IF(JLOW.GT.ICDIF)GO TO 74
      ICDIF=(icent-ICDIF)/2.-0.5             !Corrected by Magne
      IDIF2=icent-JJJ3+1

C  Check distance between adjcent peaks
      IF(ICDIF.GE.IDIF2)GO TO 74
      JJJ3=icent+1-ICDIF
   74 CONTINUE
      ICDIF=Peaks(II+1,1)+0.5
      IF(JHIG.LE.ICDIF)GO TO 75
      ICDIF=(ICDIF-icent)/2.-0.5
      IDIF2=JJJ4-1-icent
      IF(ICDIF.GE.IDIF2)GO TO 76
      JJJ4=icent+1+ICDIF
   76 iBac=1
   75 CONTINUE

      IF(ans.EQ.'s')THEN
        CALL SETCOLOR(20)         !black
        CALL INITG(NX,NY)
      ENDIF
      IF(JJJ3.LT.1)JJJ3=1
      DO J=JJJ3,JJJ4
        SPC=xSPEC(J)
        BCKGD=0.
        VARBCK=0.
        FAC1=1.
        DO N=1,Kmax
          FAC=FAC1
          DO K1=1,Kmax
            VARBCK=VARBCK+EF(N,K1)*FAC
            FAC=FAC*(J-JJ)
          ENDDO
          BCKGD=BCKGD+C(N)*FAC1
          FAC1=FAC1*(J-JJ)
        ENDDO
        VARBCK=VARBCK*CHI
        ARVAR=ARVAR+SPC+VARBCK
        PKVAR=PKVAR+(SPC+VARBCK)*(J-cent)**2
        IF(ans.EQ.'s')THEN
          IF(cent.GE.LDX.AND.cent.LE.HDX)THEN
            X = J
            Y=BCKGD
            CALL CVXY(X,Y,IX,IY,1)
            IF(J.EQ.JJJ3)CALL MSPOT(IX,IY)
            CALL IVECT(IX,IY)
          ENDIF
        ENDIF
        sum=sum+SPC-BCKGD
      ENDDO
      IF(ans.EQ.'s')THEN
        CALL FINIG
      ENDIF

      dsum=0
      IF(ARVAR.GT.0.01)dsum=SQRT(ARVAR)
      IF(sum.EQ.0.)sum=1.
      IF(ABS(dsum/sum).LE.disc.AND.sum.GT.4.)GO TO 65
      IF(KSIGF.EQ.1)GO TO 398
      GO TO 40
   65 N0=N0+1
      dcent=0
      IF(PKVAR.GT.0.01)dcent=SQRT(PKVAR)/sum
      JJ3=JJJ3-1
      JJ4=JJJ4-1
      JMARK(MARKR)=JJJ3
      JMARK(MARKR+1)=JJJ4
      MARKR=MARKR+2
      JMARK(MARKR)=100000
      KSIGF=1
      Peaks(N0,1)=cent
      Peaks(N0,2)=dcent
      Peaks(N0,3)=sum
      Peaks(N0,4)=dsum
      Peaks(N0,5)=width           
  398 CONTINUE
      MARKR=1
   40 CONTINUE
    
C Taking away narrow peaks, not significant
      wmax=1
      ii=0
      DO i=1,N0
        IF(Peaks(i,5).GT.0.01*wmax)THEN
          ii=ii+1
          Peaks(ii,1)=Peaks(i,1)
          Peaks(ii,2)=Peaks(i,2)
          Peaks(ii,3)=Peaks(i,3)
          Peaks(ii,4)=Peaks(i,4)
          Peaks(ii,5)=Peaks(i,5)*1.5 !Remember fwhmc=0.66667fwhme
        ENDIF
        IF(Peaks(i,5).GT.wmax)wmax=Peaks(i,5)
      ENDDO
      NoPeak=ii
      RETURN
      END

      SUBROUTINE FITS(X,Y,W,C,E,Kmax,NPTS,CHI)
C GENERAL SUBROUTINE FOR POLYNOMIAL FIT
C ENTER WITH X AND Y VALUES, WEIGHT, NO. OF WANTED PARAMETERS
C IN Kmax, AND NO. OF POINTS (.LE.200) IN NPTS.  RETURNS WITH COVAR
C MATRIX IN EF,PARAMETERS IN C AND NORMALLIZED CHISQUARED IN CHI
      REAL*8 E(5,5)
      DIMENSION X(400),Y(400),W(400),C(5),EF(5,5),RS(5)
      IDIM=5
      sum1=0.
      DO 2 K=1,Kmax
      DO 1 K1=1,Kmax
    1 E(K,K1)=0.
    2 RS(K)=0.
      DO 5 J=1,NPTS
      FAC1=W(J)
      DO 4 K=1,Kmax
      FAC=FAC1
      DO 3 K1=1,Kmax
      E(K,K1)=E(K,K1)+FAC
    3 FAC=FAC*X(J)
      RS(K)=RS(K)+FAC1*Y(J)
    4 FAC1=FAC1*X(J)
    5 sum1=sum1+W(J)*Y(J)*Y(J)
      DO 6 K=1,Kmax
      DO 6 K1=1,K
      EF(K,K1)=E(K,K1)
    6 EF(K1,K)=EF(K,K1)
      CALL MATINV(E,Kmax,IDIM)
      sum2=0.
      DO 8 K=1,Kmax
      sum=0.
      DO 7 K1=1,Kmax
    7 sum=sum+E(K,K1)*RS(K1)
      C(K)=sum
    8 sum2=sum*RS(K)+sum2
      sum3=0.
      DO 9 K=1,Kmax
      DO 9 K1=1,Kmax
    9 sum3=sum3+C(K)*C(K1)*EF(K,K1)
      NDIV=NPTS-Kmax
      IF(NDIV.LE.0)NDIV=1
      CHI=(sum1-2.*sum2+sum3)/NDIV
      RETURN
      END
