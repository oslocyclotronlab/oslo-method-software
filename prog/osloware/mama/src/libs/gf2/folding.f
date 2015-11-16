      FUNCTION ENEL(EG,THETA)
C Calculates energy of an electron undergoing a collision by a gamma-
C ray which is scattered an angle theta. See Canberra catalog ed.7, p.2
      ENEL=EG
        IF(EG.GT.0.1)THEN
        ENEL=EG*(EG/511.)*(1.-COS(THETA))/
     1  (1.+(EG/511.)*(1.-COS(THETA)))
      ENDIF
      END

      SUBROUTINE ExpThres
      INTEGER RDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60, ANS*1
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      DIMENSION EgamD(10),EffD(10),x(10),y(10)
      DATA EgamD/30.,80.,122.,183.,244.,294.,344.,562.,779.,1000./
      DATA EffD/ 0.0,0.1,0.51,0.66,0.78,0.85,0.89,0.99,1.00,1.000/

      ID=10
      write(6,*)' '
      write(6,*)'The efficiency at low energy (< 1000 keV) have to be given.'
      write(6,*)'It depends on various experimental conditions as thresholds'
      write(6,*)'on ADCs, gamma-absorber (2 mm Cu), timing etc. The program'
      write(6,*)'always assumes Eff = 0.0 at Egam < 30 keV and Eff = 1.00 at'
      write(6,*)'Egam > 1000 keV. However, in between these energies you can'
      write(6,*)'define a new shape of the discrimination.'
      write(6,*)' '

C Want to save EffD values as default. Therefore, defines x and y to work with.
      DO i=1,ID
        x(i)=EgamD(i)
        y(i)=EffD(i)
      ENDDO

C Finding x as a function of channels with energy calibration a0 and a1
 9999   DO I=0,511
        E=a0+FLOAT(I)*a1
        I1=1                      !finding interpolation points (I1 and I2)
        DO ii=1,ID
          IF(E.GT.x(ii))I1=ii
        ENDDO
        I2=I1+1
        IF(I1.EQ.ID) THEN
          I1=ID-1
          I2=ID
        ENDIF
        EffExp(i)=y(I1)+(y(I2)-y(I1))*(E-x(I1))/(x(I2)-x(I1))
        IF(EffExp(i).LE.0.)EffExp(i)=0.
        IF(E.LT. 30.)EffExp(i)=0.
        IF(E.GE.1408.)EffExp(i)=1.                           
      ENDDO

C Displaying discriminator function
      CALL DrawStars

C Should function be changed?
      ANS='n'
      WRITE(6,1)ANS
   1  FORMAT(/'Do you want to change the discriminator threshold <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(Istatus.NE.0)RETURN
      IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
        DO i=2,ID-1
          WRITE(6,2)x(i),y(i)
   2      FORMAT('Give efficiency at ',F6.1,' keV     <',F4.2,'>:',$)
          CALL READF(5,y(i))
          IF(Istatus.NE.0)RETURN
        ENDDO
        GO TO 9999
      ELSE

C Writting data to file:resp.dat
        Idev=27
        OPEN (Idev,FILE='resp.dat',ACCESS='APPEND',IOSTAT=IOS)
        IF(IOS.EQ.0)THEN
          WRITE(Idev,*)'Details on efficiency up to 1000 keV:'
          WRITE(Idev,22)
          WRITE(Idev,24)
22        FORMAT('    Egam  EffTot  EffExp  EffTot*EffExp')
24        FORMAT('=======================================')
25        FORMAT(   F8.1,  F8.3,    F8.3,     F8.3      )
          icha=((1000.-a0)/a1)+0.5
          iStep=1
          IF(icha.GT.100)iStep=icha/100
          DO i=0,icha,istep
            E=a0+FLOAT(i)*a1
            IF(E.GE.0)WRITE(Idev,25)E,EffTot(i),EffExp(i),EffTot(i)*EffExp(i)
          ENDDO
          CLOSE(Idev)
        ENDIF
      ENDIF
      RETURN
      END


      SUBROUTINE DrawStars
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      CHARACTER line*60,line1*64,line2*64

      ix=60
      iy=10

C Initializing the 2 lower lines
      line2='0.0|---------.---------.---------.---------.---------.---------.'
      line1='   0        100       200       300       400       500   E(keV)'
C Writing out curve of efficiency
      DO j=iy,1,-1
        DO i=1,ix
          line(i:i)=' '
        ENDDO
        eff=0.1*FLOAT(j)
        dist=100000
        DO i=1,ix
          E=FLOAT(i)*10.
          ie=(E-a0)/a1 +0.5
          IF(ie.LT.0)ie=0
          IF(ie.GT.511)ie=511
          IF(ABS(EffExp(ie)-eff).LE.0.04999)THEN
            line(i:i)='*'
          ENDIF
        ENDDO
        IF(j.EQ.2.OR.j.EQ.4.OR.j.EQ.6.OR.j.EQ.8)THEN
          write(6,10)eff,line(1:60)
        ELSE
          write(6,11)line(1:60)
        ENDIF
 10     FORMAT(F3.1,'|',A60)
 11     FORMAT(  '   |',A60)
        line(ii:ii)=' '        
      ENDDO
      WRITE(6,12)line2
      WRITE(6,12)line1
 12   FORMAT(A64)

      RETURN
      END


      FUNCTION Fluc(F,ChiLow,ChiHigh,a1)
C Calculates fluctuations in a spectrum F between ChiLow and ChiHigh
C by (F(i)-Faverage)/Faverage. The average is taken over fwhm=0.12
C at 662 keV, and so on (Oslo 4/3-1988 /M. Guttormsen)
      DIMENSION F(0:511)
      INTEGER ChiLow,ChiHigh
      Fluc=0.0
      nChannels=0
      i1=ChiLow
      Egam=(ChiLow+1)*a1
      iFW=0.12*(SQRT(662.*Egam))/a1
      IF(iFW.LT.2)iFW=2
      Egam=(ChiLow+iFW/2)*a1
      iFW=0.12*(SQRT(662.*Egam))/a1
      IF(iFW.LT.2)iFW=2
      i2=i1+iFW

      DO WHILE(i2.LE.ChiHigh)
        Sum=0
        DO I=i1,i2
          Sum=Sum+F(I)
        ENDDO
        Average=Sum/(i2-i1+1)
        IF(Average.LT.2.)Average=2.
        DO I=i1,i2
          nChannels=nChannels+1
          Fluc=Fluc+ABS(F(I)-Average)/Average
        ENDDO
        i1=i2+1
        Egam=(i1+iFW/2.)*a1
        iFW=0.12*(SQRT(662.*Egam))/a1
        IF(iFW.LT.2)iFW=2
        i2=i1+iFW
      ENDDO

      Fluc=Fluc/nChannels
      RETURN
      END


      SUBROUTINE Fold
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM

      IF(RDIM.EQ.0)THEN
        WRITE(6,*)'No response function present'
        WRITE(6,*)'Use the RM command to create one'
        Istatus=1
        RETURN
      ENDIF
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF

      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)
        WRITE(6,3)XDIM
 3      FORMAT(/'Dimension along x-axis <',I4,'>:',$)
        CALL READI(5,XDIM)
        WRITE(6,4)YDIM
 4      FORMAT( 'Dimension along y-axis <',I4,'>:',$)
        CALL READI(5,YDIM)
        IF(Istatus.NE.0)RETURN
        CALL FoldIt
        WRITE(6,5)a0,a1
 5      FORMAT('Calibration is a0=',F6.1,'keV, a1=',F7.2,'keV/ch')
        cal(1,IDEST,1,1)=a0
        cal(1,IDEST,1,2)=a1
        cal(1,IDEST,1,3)=0
        IF(a0+a1.NE.cal(1,ISP,1,1)+cal(1,ISP,1,2))THEN
          WRITE(6,*)'Warning, different energy calibration for'
          WRITE(6,*)'response matrix and source spectrum.' 
        ENDIF
      ELSE
        
        IMAXCH=Idim(2,ISP,1)
        IF(IMAXCH.GT.4096)IMAXCH=4096
        WRITE(6,7)IMAXCH
 7      FORMAT(/'Dimension of singles spectrum <',I4,'>:',$)
        CALL READI(5,IMAXCH)
        IF(Istatus.NE.0)RETURN
        MAXCH=IMAXCH-1
        CALL FoldIt
        WRITE(6,8)a0,a1
 8      FORMAT('Calibration is a0=',F6.1,'keV, a1=',F7.2,'keV/ch')
        cal(2,IDEST,1,1)=a0
        cal(2,IDEST,1,2)=a1
        cal(2,IDEST,1,3)=0
        IF(a0+a1.NE.cal(2,ISP,1,1)+cal(2,ISP,1,2))THEN
          WRITE(6,*)'Warning, different energy calibration for'
          WRITE(6,*)'response matrix and source spectrum.' 
        ENDIF
      ENDIF

C Updating comment in the heading of spectrum file
      IF(ITYPE.GT.1)THEN
        xcomm(1:3)='FO:'
        fname(1,IDEST)(1:8)='FO'//fname(1,ISP)(1:6)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ELSE
        xcomm(1:3)='FO:'
        fname(2,IDEST)(1:8)='FO'//fname(2,ISP)(1:6)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)
      ENDIF        
   
      END


      SUBROUTINE FoldIt
C        U      Unfolded spectrum
C        F      Folded spectrum, F=R(I,J)*U
C        a0,a1  Calibration for spectrum to be folded
C        LEN    Length of spectrum

      CHARACTER APP*4,ANS
      INTEGER XDIM,YDIM,RDIM,UPPER(0:511)
      INTEGER LOW,HIGH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      
      DIMENSION U(0:511),F(0:511)

      ISP=1
      IF(IDEST.EQ.1)ISP=2

      ANS='y'
      WRITE(6,133)ANS
 133  FORMAT(/,'Include total NaI-eff.    (y/n) <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(Istatus.NE.0)RETURN
      IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
        CALL ExpThres
      ENDIF


      EMIN=30.                         ! 30 keV is lower limit
      IF(ITYPE.GT.1)THEN
        LEN=XDIM
      ELSE
        LEN=MAXCH+1
      ENDIF
      IF(RDIM.LT.LEN)LEN=RDIM
      LOW=((EMIN-a0)/a1)+0.5
      IF(LOW.LT.0.OR.LOW.GE.LEN)LOW=0

C Zeroing destination spectrum
      IF(ITYPE.GT.1)THEN
        IYDIM=YDIM
        DO I=0,4095
          DO J=0,511
            rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO
      ENDIF
      IF(ITYPE.EQ.1)THEN
        IYDIM=1
        IY2=0
        DO I=0,8191
          rSPEC(IDEST,I)=0
        ENDDO
      ENDIF

C Defining upper border for the unfolding and chisq-test
      Ix1=LEN-1
      Ix2=LEN-1
      Iy1=0
      Iy2=Iydim-1
  
      IF(ITYPE.GT.1)THEN                !matrix
        WRITE(6,*)'Give upper limits for the unfolding. The boarder is'
        WRITE(6,*)'given by interpolation between (x1,y1) and (x2,y2)'
        WRITE(6,*)' '
        WRITE(6,*)'    (x2,y2)  second point'
        WRITE(6,*)'xxxxxxx'
        WRITE(6,*)'xxxxxxxxx'
        WRITE(6,*)'xxxxxxxxxxx'
        WRITE(6,*)'xx matrix xxx'
        WRITE(6,*)'xxxxxxxxxxxxxxx'
        WRITE(6,*)'            (x1,y1)  first point'
        WRITE(6,*)' '

        WRITE(6,123)Ix1
 123    FORMAT(/'First point x1  <',I5,'>:',$)
        CALL READI(5,Ix1)
        WRITE(6,124)Iy1
 124    FORMAT( 'First point y1  <',I5,'>:',$)
        CALL READI(5,Iy1)
        WRITE(6,125)Ix2
 125    FORMAT( 'Second point x2 <',I5,'>:',$)
        CALL READI(5,Ix2)
        WRITE(6,126)Iy2
 126    FORMAT( 'Second point y2 <',I5,'>:',$)
        CALL READI(5,Iy2)
      ELSE                              !singles
        WRITE(6,12)Ix1
 12     FORMAT(/'Give upper channel for folding    <',I5,'>:',$)
        CALL READI(5,Ix1)
        Ix2=Ix1
      ENDIF
      IF(Istatus.NE.0)RETURN

      CF=0.
      Dx12=Ix2-Ix1
      Dy12=Iy2-Iy1
      IF(Iy2.NE.Iy1)CF=Dx12/Dy12
      DO J=0,Iydim-1
        upper(J)=Ix1-CF*(FLOAT(Iy1-J))+0.5
        IF(upper(J).LT.low  )upper(J)=low
        IF(upper(J).GT.LEN-1)upper(J)=LEN-1
      ENDDO

      DO J=0,IYDIM-1                 !MAIN LOOP
        HIGH=UPPER(J)

C Getting the unfolded spectrum into U(i) and correcting for the total
C NaI response function for the unfolded data
        DO I=0,LEN-1
          U(I)=0.
          F(I)=0.
        ENDDO
        DO I=LOW,HIGH
          IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
            effi=EffTot(I)*EffExp(I)
            IF(ITYPE.GT.1)U(I)=rMAT(ISP,I,J)*effi
            IF(ITYPE.EQ.1)U(I)=rSPEC(ISP,I)*effi
          ELSE
            IF(ITYPE.GT.1)U(I)=rMAT(ISP,I,J)
            IF(ITYPE.EQ.1)U(I)=rSPEC(ISP,I)
          ENDIF
        ENDDO

C FOLDING : F(I)=R(I,J)*U(J)
        DO I=LOW,HIGH
C WE PUT THE INTEGRATION LIMIT BELOW I DUE TO DETECTOR RESOLUTION
          KLOW=I*0.8
          DO K=KLOW,HIGH
            F(I)=F(I)+R(I,K)*U(K)
          ENDDO
        ENDDO

C PUTTING THE FOLDED SPECTRUM INTO DESTINATION MATRIX OR SPECTRUM
        DO I=LOW,HIGH
          IF(ITYPE.GT.1)THEN
            rMAT(IDEST,I,J)=F(I)
          ELSE
            rSPEC(IDEST,I)=F(I)
          ENDIF        
        ENDDO
      ENDDO                             ! MAIN LOOP

      END


      FUNCTION GAUSS(E,EE,W)
C Calculates a Gaussian distribution with centroid at E and half-width W
      SIG=W/2.35
      A=-((EE-E)**2)/(2.*SIG**2)
      GAUSS= (1./(SIG*SQRT(2.*3.141592)))*EXP(A)
      END


      SUBROUTINE GaussNaI(x,y,l1,l2,factor,w0)
C Folding the NAI-detector with a Gauss distribution with sig=FWHM/2.35
C The response matrix is made with FWHMresp=0.5*FWHMexp. Thus, the resolution
C of the detector is FWHMexp=factor*FWHMresp, using factor =2 
C The w0 parameter is the resolution already present due to bin-width of channels
C It takes values between 1 and 2
      INTEGER RDIM
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      DIMENSION x(0:511),y(0:511)
      W=0
      DO i=0,511
        y(i)=0.
      ENDDO
      IF(w0.LT.0.OR.w0.GT.2)THEN
        write(6,*)'w0 = ',w0,' is out of range, changed to w0 = 1.0'
        w0=1.
      ENDIF
      w0=w0*ABS(a1)        !from channels to energy
      DO I=l1,l2
        E=a0+I*a1
        Wtot=Fwhm1(I)*(factor*FWHM/100.)*E
        xx=(Wtot*Wtot)-(w0*w0)
        IF(xx.GT.0         )W=SQRT(xx)
        IF(W .LT.ABS(a1)/5.)W=ABS(a1)/5.  

C Finding integration limits. Going 6*sigma to each side
        Kmin=((E-a0-(6.*W/2.35))/a1)+0.5
        Kmax=((E-a0+(6.*W/2.35))/a1)+0.5
        IF(Kmin.LT.l1)Kmin=l1
        IF(Kmax.LT.l1)Kmax=l1
        IF(Kmax.GT.l2)Kmax=l2
        yK=0                     !used as normalization (should be =1)
        DO K=Kmin,Kmax
          EE=a0+K*a1
          yK=yK+GAUSS(E,EE,W)*a1
        ENDDO
        IF(yK.LE.0)yK=10000000
        DO K=Kmin,Kmax
          EE=a0+K*a1
          y(K)=y(K)+((x(I)*GAUSS(E,EE,W)*a1)/yK)
        ENDDO
      ENDDO
      END


      SUBROUTINE GetResp
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM

      IF(RDIM.EQ.0)THEN
        WRITE(6,*)'No response function present'
        WRITE(6,*)'Use the RM command to create one'
        Istatus=1
        RETURN
      ENDIF
      IF(ITYPE.EQ.1)ITYPE=3
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2        !To prevent overwritting last spectrum
      IDEST=IDUM

      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1

      IF(Istatus.NE.0)RETURN
      DO I=0,4095
        IT=(I/400)*400
        IF(IT.EQ.I)THEN
          ist=putc('.')
          call flush(6)
        ENDIF
        DO J=0,511
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO

      DO I=0,RDIM-1
        IT=(I/200)*200
        IF(IT.EQ.I)THEN
          ist=putc('.')
          call flush(6)
        ENDIF
        DO J=0,RDIM-1
          rMAT(IDEST,I,J)=R(I,J)
        ENDDO
      ENDDO
      WRITE(6,*)
      cal(1,IDEST,1,1)=a0
      cal(1,IDEST,1,2)=a1
      cal(1,IDEST,1,3)=0.
      cal(1,IDEST,2,1)=a0
      cal(1,IDEST,2,2)=a1
      cal(1,IDEST,2,3)=0.

C Resets XDIM and YDIM to RDIM
      XDIM=RDIM
      YDIM=RDIM
      CALL Setmarker(1,1,1)

C Updating comment in the heading of spectrum file
      xcomm(1:4)='|GR:'
      fname(1,IDEST)='RespFunc'
      comm(1,IDEST)=''
      CALL AddComment(xcomm,4)

      END


      SUBROUTINE Resp
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response2/ETAB(14),FTAB(14),ER(10),FE(10),SE(10),DE(10),ANN(10),EW(10),FW(10),ENA(10)
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      CHARACTER ENA*5
      DATA RDIM/0/
      
C This block of data represents energies (ETAB) and the total efficience (FTAB)
C at that energy for 5"x5" NaI. Normalized to 1 at 1.33 MeV     
      DATA  ETAB   /
     +       0.    ,50 .     ,100.      ,200.      ,400.,
     +     500.    ,700.    ,1000.     ,1500.     ,2000.,
     +    5000.   ,7000.   ,10000.    ,20000./
      DATA  FTAB   /
     +    1.00,    1.00,     1.00,      1.00,      1.00,
     +    1.00,    1.00,     1.00,      0.97,      0.88,
     +    0.81,    0.81,     0.82,      0.95/ 

C This bolck of data represents energies (ER) for measured resp. func.,
C intensities of full-energy (FE), single-escape (SE) and double-escape (DE)
C peaks. The numbers are counts of the various RESP-EXP-122,...
C response functions. The counts are converted into probabilities
C pf, pc, ps, pd, pa in the RSPNAI routine.
      DATA ER/ 122.,   245.,    344.,   511.,  662.,  1173.,
     1        1333.,  1836.,   4439., 15110./
      DATA FE/ 1223,   3362,   13709,  44717,248445,  26670,
     1        25228,  33413,   69767,  30608/
      DATA SE/    0,      0,       0,      0,     0,      0,
     1            0,    898,   18594, 153009/
      DATA DE/    0,      0,       0,      0,     0,      0,
     1            0,      0,    2008, 122407/
      DATA ANN/   0,      0,       0,      0,     0,      0,
     1            0,    534,    3000,  30000/
      DATA ENA/'122',  '245',   '344',  '511', '662', '1173',
     1        '1333', '1836',  '4439','15110'/

C This block of data represents energies (EW) and half-width (FW) of the
C gamma-resolution in the spectra. (NB! Normalized to 1. for 1.33 MeV)
      DATA EW/  122.,   245.,    344.,   779.,  964.,  1332.,
     1         1408.,  2000.,   4000., 15000./
      DATA FW/  2.21,   1.83,   1.73,   1.18,  1.09,   1.00,   
     1          0.99,   0.84,   0.60,   0.32/

C THE NUMBER OF CALIBRATION POINTS
      IW  =10
      ITAB=14

C Reading dimension and energy calibration
      IITYPE=2
      IF(ITYPE.GT.1)IITYPE=1
      RDIM=Idim(IITYPE,IDEST,1)
      IF(RDIM.LE.10.OR.RDIM.GT.512)RDIM=512
      WRITE(6,1)RDIM
   1  FORMAT('Dimension of response-matrix <',I4,'>:',$)
      CALL READI(5,RDIM)
      IF(RDIM.GT.512)THEN
        WRITE(6,*)'Too big response matrix, maximum is 512 x 512'
        Istatus=1
        RETURN
      ENDIF

      a0=cal(IITYPE,IDEST,1,1)
      a1=cal(IITYPE,IDEST,1,2) 
      IF(a0+a1.EQ.1.)THEN  
        a0=0.0
        a1=20.0
      ENDIF
      FWHM=6.0
      WRITE(6,10)a0
  10  FORMAT(/'Cal. coeff. a0 (keV)     <',F8.1,'>:',$)
      CALL READF(5,a0)
      WRITE(6,11)a1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch)  <',F8.1,'>:',$)
      CALL READF(5,a1)

      IRSP=1
      WRITE(6,2)IRSP
   2  FORMAT(/'NaI(1) or Gauss(2) response function      <',I1,'>:',$)
      CALL READI(5,IRSP)
      IF(Istatus.NE.0)RETURN

      DO I=0,511
C Finding FWHMs as function of full energy, normalized to 1 at 1.33 MeV
        E=a0+I*a1
        I1=1
        DO K=1,IW
         IF(E.GT.EW(K))I1=K
        ENDDO
        I2=I1+1
        IF(I1.EQ.IW)THEN
          I1=IW-1
          I2=IW
        ENDIF
        W=FW(I1)+(FW(I2)-FW(I1))*(E-EW(I1))/(EW(I2)-EW(I1))
        IF(W.LE.0.00001)W=0.00001                           
        Fwhm1(I)=W
C Finding total efficiency as function of full energy
        I1=1
        DO K=1,ITAB
         IF(E.GT.ETAB(K))I1=K
        ENDDO
        I2=I1+1
        IF(I1.EQ.ITAB)THEN
          I1=ITAB-1
          I2=ITAB
        ENDIF
        EffTot(I)=FTAB(I1)+(FTAB(I2)-FTAB(I1))*(E-ETAB(I1))/(ETAB(I2)-ETAB(I1))
        IF(EffTot(I).LE.0.00001)EffTot(I)=0.00001                           
      ENDDO

      IF(IRSP.EQ.1)CALL RSPNAI
      IF(IRSP.EQ.2)CALL RSPGAUSS

      END


      SUBROUTINE RSPNAI
C     Routine for estimating NaI response-functions based on
C     interpolation between experimental spectra
C     with filenames RESP-EXP-511, RESP-EXP-1333, etc.
C     The measured response functions represent Compton and
C     back-scattering, only. The intensities of full-energy, single
C     escape, double escape and the annihilation radiation are
C     removed from the spectra. Their intensities are treated separate
C     and are listed in the DATA statement below.
C     In addition, the spectra are unfolded by their energy resolution. 
C     Therefore, one of the last task of this subroutine is to fold 
C     the interpolated spectrum with a Gaussian.
C     Some parameters are:
C        Egam   Fullenergy gamma-ray in keV
C        R(i,j) Response matrix. Intensity R in ch. i for fullenergy j
C        RDIM   Length of response spectrum
C        a0,a1  Energy-calibration of spectrum to be unfolded
C        b0,b1  Energy-calibration of original experimental response functions
C        FWHM   Half-width of spectrum at 1.33 MeV (is taken as 0.5*FWHMtrue)
C        ISTAT  Must be 0 by first call. Later it carries information
C               about what spectrum f1 and f2 which have been read
C               from disk. ISTAT=1 means 1. and 2. resp. func., ISTAT=2
C               means 2. AND 3., etc.
C     FE,SE,DE  Fullenergy, single-escape and double-escape at energy ER
C           FW  Halfwidth (FWHM) at energy EW (normalized to 1 for 1.33 MeV)
C     FEn,SEn,..is normalized intensities (not counts of raw specter)


      INTEGER XDIM,YDIM,RDIM
      CHARACTER ENA*5,APP*4,FILNAM*40,comment*60
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60

      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response2/ETAB(14),FTAB(14),ER(10),FE(10),SE(10),DE(10),ANN(10),EW(10),FW(10),ENA(10)
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      COMMON/response4/pf(0:511),pc(0:511),ps(0:511),pd(0:511),pa(0:511)
      DIMENSION FEn(10),SEn(10),DEn(10),ANNn(10)
      DIMENSION Calib(6)
      INTEGER dim
      DIMENSION Fs(0:4095),Fs1(0:4095),Fs2(0:4095),F1(0:4095),F2(0:4095)
      DIMENSION F(0:511),G(0:511), Spec(0:8191)
      DIMENSION Ffu(0:511),Gfu(0:511),Fsi(0:511),Gsi(0:511),Fdo(0:511),Gdo(0:511),Fan(0:511),Gan(0:511)
      CHARACTER*20 DATTIM

      ISTAT=0

C Number of response functions to interpolate between
      IR  =10
      
C The measured response functions have energy 
C calibration b0 (keV) and b1(keV/ch) 
C Response matrix R(i,j) has maximum dimensions 512x512
      b0=0.0
      b1=10.
      ical=6
      DO J=0,511
        DO I=0,511
          R(I,J)= 0.
        ENDDO
      ENDDO

C Reading FWHM at 1.33 MeV. It will be multiplied by the function
C Fwhm1(i), which is normalized to 1 at 1.33 MeV
      FWHM=6.0
      WRITE(6,12)FWHM
  12  FORMAT('Experimental FWHM at Egam=1.33 MeV (%)<',F5.1,'>:',$)
      CALL READF(5,FWHM)
      FWHM=FWHM*0.5          ! NB, we operate with half the FWHM value
      WRITE(6,13)
  13  FORMAT('(In the calculation the response function is obtained',
     +/'with a FWHM value of 0.5*(the exp. FWHM value just given) due',
     +/'to technical reasons in the unfolding procedure)')
      IF(Istatus.NE.0)RETURN

      EgamMin=30.                    ! Lower limit is 30 keV
      MinEgam=((EgamMin-a0)/a1)+0.5
      IF(MinEgam.LT.0)  MinEgam=0
      IF(MinEgam.GT.511)MinEgam=511

C Main loop starts
      DO J=0,RDIM-1
        JT=(J/30)*30
        IF(JT.EQ.J)THEN
          ist=putc('.')
          call flush(6)
        ENDIF
        DO I=0,511
          F(I)=0.
        ENDDO

        Egam=a0+(a1*FLOAT(J))
        IF(Egam.LT.EgamMin)GOTO 999

C Finding upper limit for the response function. Finding first the fwhm
C and take 6 times sigma above full-energy Egam
C (Remember, FWHM=0.5*FWHMexp, so it is not 6 but 3 times sigma)
        W=Fwhm1(J)*(FWHM/100.)*Egam
        IF(W.LT.ABS(a1)/5.)W=ABS(a1)/5.  
        EgamMax=Egam+(6.0*W/2.35)
        MaxEgam=((EgamMax-a0)/a1)+0.5
        IF(MaxEgam.GT.511)MaxEgam=511

C Finding the two response functions (I1 and I2) to use for the interpolation
        I1=1
        DO I=1,IR
          IF(Egam.GT.ER(I))I1=I
        ENDDO
        I2=I1+1
        IF(I1.EQ.IR) THEN
          I1=IR-1
          I2=IR
        ENDIF

C Reading the two spectra to use for interpolation 
C But first we check if they already have been read
        IF(I1.NE.ISTAT) THEN
          INP=20
          FILNAM='/user/schiller/osloware/mama/resp/RESP-EXP-'//ENA(I1)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
          dim=-1                      !Gives no headeroutput
          DO i=0,4095 
            Spec(i) = 0.
          ENDDO
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          DO i=0,4095 
            Fs1(i)=Spec(i)
          ENDDO
          CLOSE(INP)
          FILNAM='/user/schiller/osloware/mama/resp/RESP-EXP-'//ENA(I2)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=9999)
          dim=-1                      !Gives no headeroutput
          DO i=0,4095 
            Spec(i) = 0.
          ENDDO
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          DO i=0,4095 
            Fs2(i)=Spec(i)
          ENDDO
          CLOSE(INP)

C Trying to extrapolate down to zero energy
C Finding highest counts in Compton spectrum
C Then take 70 % and put in from channel 0 up to
C first time spectrum exeeds this threshold
          ih1=0
          ih2=0
          ipass1=0
          ipass2=0
          att1=0.7
          att2=0.7
          do i=0,4095
            if(Fs1(i).GT.ih1)ih1=Fs1(i)
            if(Fs2(i).GT.ih2)ih2=Fs2(i)
          enddo
          do i=0,4095
            if(Fs1(i).LT.ih1.and.ipass1.EQ.0)then
              Fs1(i)=att1*ih1
            else
              ipass1=1
            endif
            if(Fs2(i).LT.ih2.and.ipass2.EQ.0)then
              Fs2(i)=att2*ih2
            else
              ipass2=1
            endif
          enddo

C Streching/compressing Fs1 and Fs2 to fit calibration a0 and a1
          idim1=4096
          idim2=4096
          do i=0,4095
            Fs(i)=Fs1(i)
          enddo
          CALL ELASTIC(Fs,F1,b0,b1,a0,a1,idim1,idim2) !F1 is new spec.
          do i=0,4095
            Fs(i)=Fs2(i)
          enddo
          CALL ELASTIC(Fs,F2,b0,b1,a0,a1,idim1,idim2) !F2 is new spec.         

C Normalizing before interpolation (Trine)
C Compton background is normalized to one
          SumF1 = 0.
          SumF2 = 0.
          DO I = 0,4095
            SumF1 = SumF1 + F1(I)
            SumF2 = SumF2 + F2(I)
          ENDDO
          DO I = 0,4095
            F1(I) = F1(I)/SumF1
            F2(I) = F2(I)/SumF2
          ENDDO
          FEn(I1)  = FE(I1) /SumF1
          SEn(I1)  = SE(I1) /SumF1
          DEn(I1)  = DE(I1) /SumF1
          ANNn(I1) = ANN(I1)/SumF1
          FEn(I2)  = FE(I2) /SumF2
          SEn(I2)  = SE(I2) /SumF2
          DEn(I2)  = DE(I2) /SumF2
          ANNn(I2) = ANN(I2)/SumF2

C Finding upper limit for the two response functions. Finding first the fwhm
C and take 6 times sigma above full-energy of E1 and E2
C (Remember, FWHM=0.5*FWHMexp, so it is not 6 but 3 times sigma)
          E1=ER(I1)
          E2=ER(I2)
          ii1=(E1-a0)/a1+0.5
          W=Fwhm1(ii1)*(FWHM/100.)*E1
          IF(W.LT.ABS(a1)/5.)W=ABS(a1)/5.  
          E1Max=E1+(6.0*W/2.35)
          MaxE1=((E1Max-a0)/a1)+0.5
          IF(MaxE1.GT.4095)MaxE1=4095
          ii2=min0(511,INT((E2-a0)/a1+0.5))
          W=Fwhm1(ii2)*(FWHM/100.)*E2
          IF(W.LT.ABS(a1)/5.)W=ABS(a1)/5.  
          E2Max=E2+(6.0*W/2.35)
          MaxE2=((E2Max-a0)/a1)+0.5
          IF(MaxE2.GT.4095)MaxE2=4095
        ENDIF

        ISTAT=I1      ! to check against next time if new response
                      ! functions have to be read

C Finding energy-positions for backscattering (Ebsc) and
C Compton edge (Ecom). If we have to extrapolate to response functions
C with higher Egam than have been measured, we interpolate ch. by ch.
C up to 1000 keV
        Theta=3.141592654
        Ecom=EnEl(Egam,Theta)
        Ebsc=Egam-Ecom
        IF(Egam.GT.ER(IR).AND.Egam.GT.2000.)Ebsc=1000.
        Lbsc=((Ebsc-a0)/a1)+0.5
        Lcom=((Ecom-a0)/a1)+0.5
        IF(MinEgam.GT.Lbsc)Lbsc=MinEgam
        IF(MaxEgam.LT.Lcom)Lcom=MaxEgam
        Lcom=Lcom-1

C Starting to interpolate between spectra F1 and F2. This concerns the
C Compton and backscattering events, and is not easy: The main rule
C is that we interpolate between points in the spectra that represent
C the same angle theta of Compton scattering. However, up to back.sc. or
C at 0.1*Egamma, we interpolate between the same energies.
C One-to-one interpolation up to Lbsc
        DO I=MinEgam,Lbsc
          f1i=F1(I)
          f2i=F2(I)
          if1i=(0.5*MaxE1)+0.5
          if2i=(0.5*MaxE2)+0.5
          IF(I.GT.if1i)f1i=F1(if1i)
          IF(I.GT.if2i)f2i=F2(if2i)
          F(I)=F(I)+f1i+((f2i-f1i)*(Egam-E1)/(E2-E1))
          IF(F(I).LT.0.)F(I)=0.
        ENDDO
        i1Top=I-1
        i2Top=I-1
        iTop =I-1
C Interpolating between equal theta for 0 up to 180 dgrs.
        DO I=Lbsc+1,Lcom
          E=a0+I*a1
          IF(E.GT.0.1.AND.E.LT.Ecom)THEN
            if(ABS(Egam-E).GT.0.001)z=E/((Egam/511.)*(Egam-E))
            Theta=ACOS(1.-z)
            IF(Theta.GT.0.AND.Theta.LT.3.141592654)THEN
              IF1=((EnEl(E1,Theta)-a0))/a1+0.5
              IF2=((EnEl(E2,Theta)-a0))/a1+0.5
              IF(IF1.LT.Lbsc) IF1=Lbsc
              IF(IF2.GT.MaxE2)IF2=MaxE2
              FA   =F2(IF2)*  CORR(E2,Theta) - F1(IF1)*CORR(E1,Theta)
              FB   =F1(IF1)*CORR(E1,Theta) + FA*(Egam-E1)/(E2-E1)
              F(I) =F(I)+FB/CORR(Egam,Theta)
              i1Top=IF1
              i2Top=IF2
              iTop =I
            ENDIF
          ELSE
            write(6,*)'I,E,Egam,Theta,Ecom',I,E,Egam,Theta,Ecom
          ENDIF
          IF(F(I).LT.0.)F(I)=0.
        ENDDO

C Linear interpolation the last distance up to E+6*sigma
        s1=float(MaxE1-i1Top)/float(MaxEgam-iTop)
        s2=float(MaxE2-i2Top)/float(MaxEgam-iTop)
        DO I=iTop+1,MaxEgam
          IF1=i1Top+(s1*FLOAT(I-iTop))+0.5
          IF2=i2Top+(s2*FLOAT(I-iTop))+0.5
          IF(IF1.GT.MaxE1)IF1=MaxE1
          IF(IF2.GT.MaxE2)IF2=MaxE2
          F(I)=F(I)+F1(IF1)+(F2(IF2)-F1(IF1))*(Egam-E1)/(E2-E1)
          IF(F(I).LT.0.)F(I)=0.
        ENDDO
        SUMF=0.
        DO I = 0,511
          SUMF=SUMF+F(I)
        ENDDO
C Folding Compton with a Gauss distribution, where
C factor=1 means 0.5*FWHMexp and = 2 means 1.0*FWHMexp
C The width contribution from channel-bin is w0=1
C Array F is smoothed -> G
        factor=1.    
        w0=1.           
        CALL GaussNaI(F,G,MinEgam,MaxEgam,factor,w0) !smoothing Compton part

C Adding the counts of the full-energy, single-escape,
C double-escape and annihilation peaks to the spectrum.
C Generelly, the total counts (yl+yh) have to be distributed on 
C the two channels il and ih.
        DO i=0,511
          Ffu(i)=0.
          Gfu(i)=0.
          Fsi(i)=0.
          Gsi(i)=0.
          Fdo(i)=0.
          Gdo(i)=0.
          Fan(i)=0.
          Gan(i)=0.
        ENDDO

        A=FEn(I1)+( FEn(I2)- FEn(I1))*(Egam-E1)/(E2-E1)   !full-energy intensity
        IF(A.LT.0.)A=0.
        Egami=(Egam-a0)/a1
        il=Egami              ! Distribute counts on ch il and ih
        ih=il+1
        yl=(ih-Egami)*A
        yh=(Egami-il)*A
        IF(il.GE.0.AND.il.LE.MaxEgam) Ffu(il)=Ffu(il)+yl
        IF(ih.GE.0.AND.ih.LE.MaxEgam) Ffu(ih)=Ffu(ih)+yh
        ymax=AMAX1(yl,yh)
        ymin=AMIN1(yl,yh)
        IF(ymax.GT.0)THEN
          w0=(ymin+ymax)/ymax
        ELSE
          w0=1.
        ENDIF
        factor=1. 
        lim=MAX0((3.*(Fwhm1(il)*2.*FWHM/100.))*FLOAT(il)+0.5 , 1)
        m1=MAX0(il-lim,MinEgam)
        m2=MIN0(ih+lim,MaxEgam)
        CALL GaussNaI(Ffu,Gfu,m1,m2,factor,w0) !smoothing full energy peak

        B=0.
        C=0.
        D=0.
        IF(Egam.GT.1022)THEN
          AN=511.0
          SI=Egam-(1.*AN)
          DO=Egam-(2.*AN)
          SIi=(SI-a0)/a1
          DOi=(DO-a0)/a1
          ANi=(AN-a0)/a1
          B=SEn(I1) +( SEn(I2)- SEn(I1))*(Egam-E1)/(E2-E1)
          C=DEn(I1) +( DEn(I2)- DEn(I1))*(Egam-E1)/(E2-E1)
          D=ANNn(I1)+(ANNn(I2)-ANNn(I1))*(Egam-E1)/(E2-E1)

C Taking care of the limiting case around 2*mc2=1022 keV
          IF(E1.LT.1022.AND.E2.GT.1022)THEN
            B= SEn(I2)*(Egam-1022.)/(E2-1022.)
            C= DEn(I2)*(Egam-1022.)/(E2-1022.)
            D=ANNn(I2)*(Egam-1022.)/(E2-1022.)
          ENDIF

          IF(B.LT.0.)B=0.
          IF(C.LT.0.)C=0.
          IF(D.LT.0.)D=0.

          il=SIi
          ih=il+1
          yl=(ih-SIi)*B
          yh=(SIi-il)*B
          IF(il.GE.0.AND.il.LE.MaxEgam) Fsi(il)=Fsi(il)+yl
          IF(ih.GE.0.AND.ih.LE.MaxEgam) Fsi(ih)=Fsi(ih)+yh
          ymax=AMAX1(yl,yh)
          ymin=AMIN1(yl,yh)
          IF(ymax.GT.0)THEN
            w0=(ymin+ymax)/ymax
          ELSE
            w0=1.
          ENDIF
          lim=MAX0((3.*(Fwhm1(il)*2.*FWHM/100.))*FLOAT(il)+0.5 , 1)
          m1=MAX0(il-lim,MinEgam)
          m2=MIN0(ih+lim,MaxEgam)
          factor=1.               
          CALL GaussNaI(Fsi,Gsi,m1,m2,factor,w0) !smoothing s.esc. peak

          il=DOi              
          ih=il+1
          yl=(ih-DOi)*C
          yh=(DOi-il)*C
          IF(il.GE.0.AND.il.LE.MaxEgam) Fdo(il)=Fdo(il)+yl
          IF(ih.GE.0.AND.ih.LE.MaxEgam) Fdo(ih)=Fdo(ih)+yh
          ymax=AMAX1(yl,yh)
          ymin=AMIN1(yl,yh)
          IF(ymax.GT.0)THEN
            w0=(ymin+ymax)/ymax
          ELSE
            w0=1.
          ENDIF
          lim=MAX0((3.*(Fwhm1(il)*2.*FWHM/100.))*FLOAT(il)+0.5 , 1)
          m1=MAX0(il-lim,MinEgam)
          m2=MIN0(ih+lim,MaxEgam)
          factor=1.               
          CALL GaussNaI(Fdo,Gdo,m1,m2,factor,w0) !smoothing d.esc. peak

          il=ANi 
          ih=il+1
          yl=(ih-ANi)*D
          yh=(ANi-il)*D
          IF(il.GE.0.AND.il.LE.MaxEgam) Fan(il)=Fan(il)+yl
          IF(ih.GE.0.AND.ih.LE.MaxEgam) Fan(ih)=Fan(ih)+yh
          ymax=AMAX1(yl,yh)
          ymin=AMIN1(yl,yh)
          IF(ymax.GT.0)THEN
            w0=(ymin+ymax)/ymax
          ELSE
            w0=1.
          ENDIF
          lim=MAX0((3.*(Fwhm1(il)*2.*FWHM/100.))*FLOAT(il)+0.5 , 1)
          m1=MAX0(il-lim,MinEgam)
          m2=MIN0(ih+lim,MaxEgam)
          factor=1.               
          CALL GaussNaI(Fan,Gan,m1,m2,factor,w0) !smoothing ann. peak
        ENDIF

C We renormalize so that matrix R(i,j) have 1*RDIM counts
        SumR=0.
        DO I=0,RDIM-1
          R(I,J)=G(I)+Gfu(I)+Gsi(I)+Gdo(I)+Gan(I)
          IF(R(I,J).LT.0)R(I,J)=0
          SumR=SumR+R(I,J)
        ENDDO
        DO I=0,RDIM-1
          IF(SumR.GT.0)R(I,J)=R(I,J)/SumR
        ENDDO
 999    CONTINUE
      ENDDO

C Finding parameters as function of full energy
C Writing out to file: resp.dat
      DO I=0,511
        E=a0+I*a1
C The probabilities for full energy, Compton, single, double and annih. made:
        I1=1
        DO K=1,IR
         IF(E.GT.ER(K))I1=K
        ENDDO
        I2=I1+1
        IF(I1.EQ.IR)THEN
          I1=IR-1
          I2=IR
        ENDIF
        pf(I) = amax1(0,FEn(I1) +(FEn(I2) -FEn(I1)) *(E-ER(I1))/(ER(I2)-ER(I1)))
        pc(I) = 1.                                  !due to Trines normalization
        ps(I) = amax1(0,SEn(I1) +(SEn(I2) -SEn(I1)) *(E-ER(I1))/(ER(I2)-ER(I1)))
        pd(I) = amax1(0,DEn(I1) +(DEn(I2) -DEn(I1)) *(E-ER(I1))/(ER(I2)-ER(I1)))
        pa(I) = amax1(0,ANNn(I1)+(ANNn(I2)-ANNn(I1))*(E-ER(I1))/(ER(I2)-ER(I1)))
        total=pf(I)+pc(I)+ps(I)+pd(I)+pa(I)
        IF(total.LE.0.)total=10000000.
        pf(I) = pf(I)/total
        pc(I) = 1.   /total
        ps(I) = ps(I)/total
        pd(I) = pd(I)/total
        pa(I) = pa(I)/total
      ENDDO

  20  OPEN(UNIT=25,FILE='resp.dat', ERR=9998)
       
      ISM=1
      IF(ITYPE.EQ.1)ISM=2
      CALL DATETIME(DATTIM)
      WRITE(25,199)FNAME(ISM,IDEST),DATTIM(1:12)
      WRITE(25,210)a0,a1,FWHM*2.
199   FORMAT('Response function created for file:',A,' at ',A)
210   FORMAT('Calibration (a0,a1)=',F8.1,F9.3,'  FWHM=',F7.1,/)

      iStep=1
      IF(RDIM.GT.50)iStep=RDIM/50                  !steps for output
      WRITE(25,*)'Egam(keV) FWHM(keV) EffTot  Photo  Compton Single  Double   Annih'
      DO i=0,RDIM-1
        Egam=a0+i*a1
        IF(Egam.GT.0)THEN
          IF((i/iStep)*iStep.EQ.i)THEN
            fwh=0.
            IF(Egam.GT.0)fwh=Fwhm1(i)*(2.*FWHM/100.)*Egam
            IF(fwh.LT.ABS(a1)/5.)fwh=ABS(a1)/5.
            WRITE(25,14)Egam, fwh, EffTot(i),pf(i), pc(i), ps(i), pd(i), pa(i)
 14         FORMAT(     F8.1,F10.1,     F8.3, F8.4,  F8.4,  F8.4,  F8.4,  F8.4)
          ENDIF
        ENDIF
      ENDDO
      write(6,*)
      write(6,*)'Parameters for response function written to resp.dat'
      write(6,*)
      GOTO 9998
 9999 WRITE(6,*)'Cannot find the response function: ',FILENAM
 9998 CONTINUE
      CLOSE(24)
      CLOSE(25)

      END


      SUBROUTINE RSPGAUSS
C     Routine for producing response-functions for counters with
C     a Gaussian distribution
C     Some parameters are:
C        Egam   Energy in keV
C        R(I,J) Respone function. For a fullenergy j, you get
C               a spectrum with intensity R at channel i
C        RDIM   Length of respone spectrum
C        a0,a1  Energy-calibration of spectrum
C        FWHM   Half-width of spectrum at 1.33 mev
C        FW     Halfwidth (FWHM) at energy EW (NORMALIZED TO 1 FOR 1.33 MEV)
      CHARACTER*20 DATTIM
      CHARACTER APP*4
      INTEGER XDIM,YDIM,RDIM
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      DIMENSION F(0:511),G(0:511)

      FWHM=6.0

C Initializing response matrix
      DO J=0,511
        DO I=0,511
          R(I,J)= 0.
        ENDDO
      ENDDO

C Reading in various parameters to be used
c      IITYPE=2
c      IF(ITYPE.GT.1)IITYPE=1
c      a0=cal(IITYPE,IDEST,1,1)
c      a1=cal(IITYPE,IDEST,1,2) 
c      IF(a0+a1.EQ.1.)THEN  
c        a0=0.0
c        a1=80.0
c      ENDIF
c      WRITE(6,10)a0
c  10  FORMAT(/'Cal. coeff. a0 (keV)     <',F8.1,'>:',$)
c      CALL READF(5,a0)
c      WRITE(6,11)a1
c  11  FORMAT( 'Cal. coeff. a1 (keV/ch)  <',F8.1,'>:',$)
c      CALL READF(5,a1)

      IANS=1
      WRITE(6,12)IANS
  12  FORMAT(/'Define FWHM as function of channels (1)',
     1'or use the NaI-expression (2)<',I1,'>:',$)
      CALL READI(5,IANS)

      IF(IANS.EQ.2)THEN
        WRITE(6,13)FWHM
  13    FORMAT(/'FWHM at Egam=1.33 MeV (%)<',F8.3,'>:',$)
        CALL READF(5,FWHM)
        FWHM=0.5*FWHM
      ELSE
        IXL=RDIM*0.1+0.5
        IXH=RDIM*0.9+0.5
        FWXL=2.
        FWXH=2.
        WRITE(6,14)IXL,FWXL
  14    FORMAT(/'Write FWHM(ch) around ',I4,' <',F6.1,'>:',$)
        CALL READF(5,FWXL)
        WRITE(6,15)IXH,FWXH
  15    FORMAT( 'Write FWHM(ch) around ',I4,' <',F6.1,'>:',$)
        CALL READF(5,FWXH)
        FWXL=0.5*FWXL
        FWXH=0.5*FWXH

C Finding parametrization of fwhm.: FWHM = A + B * SQRT(ch)
        BX=(FWXL-FWXH)/(SQRT(float(IXL))-SQRT(float(IXH)))
        AX=FWXL-BX*SQRT(float(IXL))
        WRITE(6,*)'FWHM have been expressed by A + B * SQRT(ch):'
        WRITE(6,16)AX,BX
 16     FORMAT('Ax=',F8.4,'  Bx=',F8.4,/)
      ENDIF
      IF(Istatus.NE.0)RETURN

      WRITE(6,17)
 17   FORMAT('(In the calculation the response function is obtained',
     1/'with a FWHM value of 0.5*(the exp. FWHM value just given) due',
     1/'to technical reasons in the unfolding procedure)')

      EgamMin=30.
      MinEgam=(EgamMin-a0)/a1+0.5
      IF(MinEgam.LT.   0)MinEgam=0

C Main loop starts
      DO J=0,RDIM-1
        JT=(J/10)*10
        IF(JT.EQ.J)THEN
          ist=putc('.')
          call flush(6)
        ENDIF
        DO I=0,511
          F(I)=0.
          G(I)=0.
        ENDDO
        Egam=a0+a1*J
        IF(Egam.LT.EgamMin) GOTO 999

C Finding upper limit for the response function. Finding first the fwhm
C and take 6 times sigma above full-energy Egam
        W=Fwhm1(J)*(FWHM/100.)*Egam
        IF(IANS.EQ.1)W=(AX+BX*SQRT(float(J)))*a1
        IF(W.LT.ABS(a1)/5.)W=ABS(a1)/5.  
        EgamMax=Egam+(6.0*W/2.35)
        MaxEgam=((EgamMax-a0)/a1)+0.5
        IF(MaxEgam.GT.511)MaxEgam=511

C Putting a sharp peak into F(i=j)
        F(J)=10000.

C Folding detector resolution as a gauss distr. with sig=fwhm/2.35
        DO I=MinEgam,MaxEgam
          E=a0+I*a1
C Finding the fwhm (in keV) for that channel
          W=Fwhm1(i)*(FWHM/100.)*E
          IF(IANS.EQ.1)W=(AX+BX*SQRT(float(J)))*a1
          IF(W.LT.ABS(a1)/5.)W=ABS(a1)/5.  
C Finding integration limits. Going 6*sigma to each side.
          Kmin=((E-a0-(6.0*W/2.35))/a1)+0.5
          Kmax=((E-a0+(6.0*W/2.35))/a1)+0.5
          IF(Kmin.LT.MinEgam)Kmin=MinEgam
          IF(Kmax.LT.MinEgam)Kmax=MinEgam
          IF(Kmax.GT.MaxEgam)Kmax=MaxEgam
          IF(W.LT.ABS(a1)/5.)W=ABS(a1)/5.

          GK=0
          DO K=Kmin,Kmax
            EE=a0+K*a1
            GK=GK+GAUSS(E,EE,W)*a1
          ENDDO
          IF(GK.LE.0)GK=1000000
          DO K=Kmin,Kmax
            EE=a0+K*a1
            G(K)=G(K)+(F(I)*GAUSS(E,EE,W)*a1/GK)
          ENDDO
        ENDDO
        DO I=MinEgam,MaxEgam
          R(I,J)=G(I)
        ENDDO

C We renormalize so that matrix R(i,j) have integral 1*RDIM
        SUM1=0.
        DO I=0,RDIM-1
          SUM1=SUM1+R(I,J)
        ENDDO
        DO I=0,RDIM-1
          IF(SUM1.GT.0)R(I,J)=R(I,J)/SUM1
        ENDDO
 999    CONTINUE
      ENDDO

  20  OPEN(UNIT=25,FILE='resp.dat', ERR=6789)
      ISM=1
      IF(ITYPE.EQ.1)ISM=2
      CALL DATETIME(DATTIM)
      WRITE(25,199)FNAME(ISM,IDEST),DATTIM(1:12)
      WRITE(25,210)a0,a1,2.0*FWHM
      IF(IANS.EQ.1)WRITE(25,211)a0,a1,2.0*FWHL,2.0*FWHH
199   FORMAT('Response function created for file:',A,' at ',A)
210   FORMAT('Calibration (a0,a1)=',F8.1,F9.3,'  FWHM=',F7.1,/)
211   FORMAT('Calibration (a0,a1)=',F8.1,F9.3,'  FWHMl=',F7.1,' FWHMh=',F7.1,/)

      iStep=1
      IF(RDIM.GT.50)iStep=RDIM/50        !steps for output
      WRITE(25,*)'Egam(keV) FWHM(keV) EffTot'
      DO i=0,RDIM-1
        Egam=a0+i*a1
        IF(Egam.GT.0)THEN
          IF((i/iStep)*iStep.EQ.i)THEN
            fwh=0.
            IF(Egam.GT.0)THEN
              fwh=Fwhm1(i)*(2.*FWHM/100.)*Egam
              IF(IANS.EQ.1)fwh=(AX+BX*SQRT(float(i)))*a1
            ENDIF
            IF(fwh.LT.ABS(a1)/5.)fwh=ABS(a1)/5.
            xEffTot=1.
            WRITE(25,22)Egam,fwh,xEffTot
 22         FORMAT(F8.1,F10.2,F8.2)
          ENDIF
        ENDIF
      ENDDO
      CLOSE(25)
      write(6,*)
      write(6,*)'Parameters for response function written to resp.dat'
      write(6,*)
 6789 CONTINUE
      END
    
           
      SUBROUTINE SCORING(SoluP,Lsave,Lmax,SCORE,Isc,wfluc)
c Routine to calculate the score of each solution. The score-function,
c which should be as small as possible, is simply a weighted sum of
c chisquare and fluctuations

      DIMENSION SoluP(200,7),SC(200)
      CHARACTER SCORE(200)*4

      Isc=0
      DO I=1,200
        SCORE(I)='    '
        SC(200)=0
      ENDDO
      IF(wfluc.LT.0.OR.wfluc.GT.0.5)wfluc=0.2
      wchi=1.-wfluc

C Calculating the score-function
      THRES=0.3*SoluP(LSAVE,6)
      DO I=1,Lmax
        CHI=SoluP(I,6)
        FLU=SoluP(I,7)
        IF(FLU.LT.1.45)FLU=1.45
        SC(I)=wchi*CHI + wfluc*FLU
        EPS=SoluP(I,6)-SoluP(LSAVE,6)
        IF(EPS.GT.THRES)SC(I)=SC(I)+1000
      ENDDO

C Finding the favorit solution
      SCR=1000000
      DO I=1,Lmax
        IF(SC(I).LT.SCR)THEN
          SCR=SC(I)
          Isc=I
        ENDIF
      ENDDO
C Marking the solutions
      THRES1=0.1*THRES
      THRES2=0.3*THRES
      THRES3=0.7*THRES
      DO I=1,Lmax
        EPS=SC(I)-SC(Isc)
        IF(EPS.LT.THRES3)SCORE(I)=' *  '
        IF(EPS.LT.THRES2)SCORE(I)=' ** '
        IF(EPS.LT.THRES1)SCORE(I)=' ***'
      ENDDO

      SCORE(LSAVE)='<   '
      IF(LSAVE.EQ.Isc)SCORE(LSAVE)='<***'
      RETURN
      END


      SUBROUTINE UnFold
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60

      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
    
      IF(RDIM.EQ.0)THEN
        WRITE(6,*)'No response function present'
        WRITE(6,*)'Use the RM command to create one'
        Istatus=1
        RETURN
      ENDIF
        
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2
      IDEST=IDUM
      WRITE(6,1)IDEST
 1    FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      ISP=1
      IF(IDEST.EQ.1)ISP=2
      WRITE(6,2)ISP
 2    FORMAT( 'Source spectrum      <',I1,'>:',$)
      CALL READI(5,ISP)
      IF(ISP.EQ.IDEST)THEN
        WRITE(6,*)'Destination must be another spectrum'
        Istatus=1
        RETURN
      ENDIF
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1
      IF(ISP  .LT.1.OR.ISP  .GT.2)Istatus=1

      iVersion=0
      WRITE(6,6)iVersion
 6    FORMAT(/'New (0) or old (1) unfolding procedure <',I1,'>:',$)
      CALL READI(5,iVersion)

      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)
        IF(XDIM.GT.RDIM)XDIM=RDIM
        WRITE(6,3)XDIM
 3      FORMAT(/'Dimension along x-axis <',I4,'>:',$)
        CALL READI(5,XDIM)
        WRITE(6,4)YDIM
 4      FORMAT( 'Dimension along y-axis <',I4,'>:',$)
        CALL READI(5,YDIM)
        IF(Istatus.NE.0)RETURN

        CALL UnFoldit(iVersion)

        WRITE(6,5)a0,a1
 5      FORMAT('Calibration is a0=',F6.1,'keV, a1=',F7.2,'keV/ch')
        cal(1,IDEST,1,1)=a0
        cal(1,IDEST,1,2)=a1
        cal(1,IDEST,1,3)=0
        cal(1,IDEST,2,1)=cal(1,ISP,2,1)
        cal(1,IDEST,2,2)=cal(1,ISP,2,2)
        cal(1,IDEST,2,3)=cal(1,ISP,2,3)

        IF(a0+a1.NE.cal(1,ISP,1,1)+cal(1,ISP,1,2))THEN
          WRITE(6,*)'Warning, different energy calibration for'
          WRITE(6,*)'response matrix and source matrix.' 
        ENDIF 
      ELSE
        IMAXCH=Idim(2,ISP,1)
        IF(IMAXCH.GT.RDIM)IMAXCH=RDIM
        WRITE(6,7)IMAXCH
 7      FORMAT(/'Dimension of singles spectrum  <',I4,'>:',$)
        CALL READI(5,IMAXCH)
        IF(Istatus.NE.0)RETURN
        MAXCH=IMAXCH-1

        CALL UnFoldit(iVersion)

        WRITE(6,8)a0,a1
   8    FORMAT('Calibration is a0=',F6.1,'keV, a1=',F7.2,'keV/ch')
        cal(2,IDEST,1,1)=a0
        cal(2,IDEST,1,2)=a1
        cal(2,IDEST,1,3)=0
        IF(a0+a1.NE.cal(2,ISP,1,1)+cal(2,ISP,1,2))THEN
          WRITE(6,*)'Warning, different energy calibration for'
          WRITE(6,*)'response matrix and source spectrum.' 
        ENDIF
      ENDIF 

C Updating comment in the heading of spectrum file
      IF(ITYPE.GT.1)THEN
        xcomm(1:3)='UN:'
        fname(1,IDEST)(1:8)='UN'//fname(1,ISP)(1:6)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ELSE
        xcomm(1:3)='UN:'
        fname(2,IDEST)(1:8)='UN'//fname(2,ISP)(1:6)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)
      ENDIF  
      END



      SUBROUTINE UnFoldit(iVersion)
C A program for unfolding detector response effects from a continuos
C gamma ray spectrum. Modified version (Aug. 1995) which smoothes the 
C Compton contribution, which is to be subtracted.
C
C UCID - 30079
C Lawrence Livermore Laboratory
C William J. Oconnel September 1973
C
C UCRL - 9748
C A computer analysis for complex sodium iodide gamma spectra
c James F. Mollenauer   august 1961
C
C Oslo Cyclotron Laboratory version 1989,1993,1994,1995 M.G.
C See NIM paper of M. Guttormsen et al. 1996, in press
C        Raw    Raw spectrum
C        U      Unfolded spectrum
C        F      Folded spectrum, F=R(I,J)*U
C        a0,a1  Calibration coeff in units of keV and keV/ch
C        LEN    Channels per spectrum
C        mode   = -1 : Difference iteration (d)
C               = +1 : Ratio iteration      (r)
C        Iter   =Max number of iteration steps

      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4,waitname*8
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      COMMON/response1/R(0:511,0:511),RDIM,a0,a1,FWHM
      COMMON/response3/EffTot(0:511),Fwhm1(0:511),EffExp(0:511)
      COMMON/response4/pf(0:511),pc(0:511),ps(0:511),pd(0:511),pa(0:511)

      INTEGER low,high,ChiLow,ChiHigh,lower,upper
      INTEGER sum,Fsum,sumCH,FsumCH
      DIMENSION Raw(0:511),F(0:511),U(0:511),lower(0:511),upper(0:511)
      DIMENSION Fsave(0:511),Usave(0:511)
      DIMENSION SoluF(200,0:511),SoluP(200,7)
      DIMENSION rWAIT(0:8191)
      CHARACTER SCORE(200)*4,ANS*1
      CHARACTER modus*2,moil*2,moim*2,moir*2
      INTEGER COLORMAP(20),Color(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Color
      REAL Limit(0:19)
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      DIMENSION u0(0:511),su0(0:511),v(0:511),us(0:511),ud(0:511)
      DIMENSION ua(0:511),sua(0:511),w(0:511),sw(0:511),c(0:511),sc(0:511)

      ISP=1
      IF(IDEST.EQ.1)ISP=2
      Emin  =30.          ! 30 keV lowest limit
      EminCh=100.         ! 100 keV lower limit for Chi2-test
      Iter  =33           ! Number of max. iterations

C Using rSPEC(IDEST,i) to display spectra. Putting back later
      DO i=0,MAXCH
        rWAIT(i)=rSPEC(IDEST,i)
        rSPEC(IDEST,i)=0
      ENDDO
      waitname=fname(2,IDEST)
      IF(ITYPE.GT.1)fname(2,IDEST)=fname(1,ISP)
      IF(ITYPE.EQ.1)fname(2,IDEST)=fname(2,ISP)

C Zeroing destination spectrum
      IF(ITYPE.GT.1)THEN                !matrix
        LEN=XDIM
        IF(RDIM.LT.XDIM)LEN=RDIM
        Iydim=YDIM
        DO I=0,4095
          DO J=0,511
            rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO
      ELSE                              !singles
        LEN=MAXCH+1
        IF(RDIM.LT.LEN)LEN=RDIM
        Iydim=1
        DO I=0,8191
          rSPEC(IDEST,I)=0
        ENDDO
      ENDIF

C Lowest channel treated
      low=((Emin-a0)/a1)+0.5
      IF(low.LT.0.OR.low.GE.LEN)low=0
      lowCh=((EminCh-a0)/a1)+0.5
      IF(lowCh.LT.0.OR.lowCh.GE.LEN)lowCh=0

C Defining upper border for the unfolding and chisq-test
      Ix1=LEN-1
      Ix2=LEN-1
      Iy1=0
      Iy2=Iydim-1
  
      IF(ITYPE.GT.1)THEN                !matrix
        WRITE(6,*)'Give upper limits for the unfolding. The boarder is'
        WRITE(6,*)'given by interpolation between (x1,y1) and (x2,y2)'
        WRITE(6,*)' '
        WRITE(6,*)'    (x2,y2) second point'
        WRITE(6,*)'xxxxxxx'
        WRITE(6,*)'xxxxxxxxx'
        WRITE(6,*)'xxxxxxxxxxx'
        WRITE(6,*)'xx matrix xxx'
        WRITE(6,*)'xxxxxxxxxxxxxxx'
        WRITE(6,*)'             (x1,y1) first point'
        WRITE(6,*)' '

        WRITE(6,123)Ix1
 123    FORMAT(/'First point x1  <',I5,'>:',$)
        CALL READI(5,Ix1)
        WRITE(6,124)Iy1
 124    FORMAT( 'First point y1  <',I5,'>:',$)
        CALL READI(5,Iy1)
        WRITE(6,125)Ix2
 125    FORMAT( 'Second point x2 <',I5,'>:',$)
        CALL READI(5,Ix2)
        WRITE(6,126)Iy2
 126    FORMAT( 'Second point y2 <',I5,'>:',$)
        CALL READI(5,Iy2)
      ELSE                              !singles
        WRITE(6,12)Ix1
 12     FORMAT(/'Give upper channel for unfolding  <',I5,'>:',$)
        CALL READI(5,Ix1)
        Ix2=Ix1
      ENDIF
      IF(Istatus.NE.0)RETURN

      CF=0.
      Dx12=Ix2-Ix1
      Dy12=Iy2-Iy1
      IF(Iy2.NE.Iy1)CF=Dx12/Dy12
      DO J=0,Iydim-1
        upper(J)=Ix1-CF*(FLOAT(Iy1-J))+0.5
        IF(upper(J).LT.low  )upper(J)=low
        IF(upper(J).GT.LEN-1)upper(J)=LEN-1
      ENDDO

C Setting lower limits for the chisquare test
      WRITE(6,130)
 130  FORMAT('  Give limits for the chisquare-test:',/,
     +       '  Opt. 1: Recommended for NaI-spectra. For full-energy gammas',
     +/,     '          above 2 MeV, we set lower limit at 500 keV. Below,',
     +/,     '          the limit is 1/4 of the full-energy. Remember,'
     +/,     '          full-energy is taken from the upper unfolding limit',
     +/,     '  Opt. 2: A fixed lower limit for the chi-test is applied'
     +/,     '  Opt. 3: Return and set proper upper limits for unfolding',/)

      Iopt=1
      IF(Ix1.EQ.Ix2)Iopt=2
      WRITE(6,131)Iopt
 131  FORMAT(/'Option (1/2/3)                  <',I1,'>:',$)
      CALL READI(5,Iopt)
      IF(Istatus.NE.0)RETURN

      IF(Iopt.EQ.1)THEN
        DO J=0,Iydim-1
          Emax=a0+upper(J)*a1
          lower(J)=(500.-a0)/a1                     !500 keV IF E>2MeV
          IF(Emax.LT.2000)lower(J)=(Emax*.25-a0)/a1
          IF(lower(J).LT.low)lower(J)=low
        ENDDO
      ENDIF

      IF(Iopt.EQ.2)THEN
        WRITE(6,132)lowCh
 132    FORMAT(/'Lower channel for chi-test  <',I5,'>:',$)
        CALL READI(5,lowCh)
        DO J=0,Iydim-1
          lower(J)=lowCh
        ENDDO
      ENDIF
      IF(Istatus.NE.0)RETURN

      IF(Iopt.NE.1.AND.Iopt.NE.2)THEN
        WRITE(6,*)'No unfolding performed'
        Istatus=1
        RETURN
      ENDIF

      ANS='y'
      WRITE(6,133)ANS    
 133  FORMAT(/,'Include total NaI-eff.    (y/n) <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(Istatus.NE.0)RETURN
      IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
        CALL ExpThres
      ENDIF

      iter=33
      WRITE(6,134)iter   
 134  FORMAT(/,'Number of iterations ( <200 )  <',I2,'>:',$)
      CALL READI(5,iter)
      IF(iter.GT.200)iter=200
      IF(Istatus.NE.0)RETURN

      wfluc=0.2
      WRITE(6,*)' '
      WRITE(6,*)'The iteration is terminated when the folding of'
      WRITE(6,*)'the unfolded spectrum equals the raw spectrum. It'
      WRITE(6,*)'is however recommended to stop before the Chi2 is'
      WRITE(6,*)'at minimum. Thus, you can put a certain weight on'
      WRITE(6,*)'the importance not to have too strong fluctuations'
      WRITE(6,*)'in the final spectrum. We recommend a weight-factor'
      WRITE(6,*)'of 0.2 (valid range is 0.0 - 0.5)'
      WRITE(6,135)wfluc   
 135  FORMAT(/,'Weight on fluctuations <',F3.1,'>:',$)
      CALL READF(5,wfluc)
      IF(wfluc.GT.0.5)wfluc=0.2
      IF(Istatus.NE.0)RETURN

C Loop for spectrum J to be unfolded***************************
      DO J=0,Iydim-1
        high=   upper(J)
        ChiHigh=upper(J)
        ChiLow= lower(J)

C Getting the raw spectrum Raw(I)
        DO I=0,511
          IF(ITYPE.GT.1)Raw(I)=rMAT(ISP,I,J)
          IF(ITYPE.EQ.1)Raw(I)=rSPEC(ISP,I)
        ENDDO
        sum=   0
        sumCH= 0
        DO I=low,high
          sum  =sum + Raw(I)
        ENDDO
        DO I=ChiLow,ChiHigh
          sumCH=sumCH+Raw(I)
        ENDDO

C Initialize parameters
        DO I=1,Iter
          DO JJ=0,LEN-1
            SoluF(I,JJ)=0
          ENDDO
          DO JJ=1,7
            SoluP(I,JJ)=0
          ENDDO
        ENDDO

        DO I=0,LEN-1
          U(I)    =0.0
          F(I)    =0.0
          Fsave(I)=0.0
          Usave(I)=0.0
        ENDDO

        ModeChange=0
        CHIsave=1000000000
        CHIold =1000000000
                              
        FlucRaw=Fluc(Raw,ChiLow,ChiHigh,a1)
       
C The experimental pulse height spectrum is the first
C approximation to the unfolded gamma spectrum U(I)
        DO I=low,high
          U(I)=Raw(I)
        ENDDO

C Iteration loop for spectrum J. Folding the unfolded spectrum
C We have to start the integration somewhat below ch I, due to the
C detector resolution. We take conservative 20% resolution
        mode=0                      !starts with no method
        iTry=0
        DO L=1,Iter
          DO I=low,high
            F(I)=0.0
            Klow=I*0.8
            DO K=Klow,high
              F(I)=F(I)+R(I,K)*U(K)
            ENDDO
          ENDDO

C Calculate chisquare between folded and raw spectrum
          CHISQ=0.
          Ichi=0
          DO I=ChiLow,ChiHigh
            sig2=Raw(I)
            IF(sig2.LT.4.)sig2=4.
            CHISQ=CHISQ+((F(I)-Raw(I))**2)/sig2
            Ichi=Ichi+1
          ENDDO
          IF(Ichi.LT.1)Ichi=1
          IF(Ichi.GT.1)Ichi=Ichi-1
          CHISQ=CHISQ/Ichi

C Compute sum of counts
          Fsum  =0.
          FsumCH=0.
          DO I=low,high
            Fsum=Fsum+F(I)
          ENDDO
          DO I=ChiLow,ChiHigh
            FsumCH=FsumCH+F(I)
          ENDDO
 
          RelFluc=Fluc(U,ChiLow,ChiHigh,a1)/FlucRaw
          SoluP(L,1)=mode
          SoluP(L,2)=sum
          SoluP(L,3)=Fsum
          SoluP(L,4)=sumCH
          SoluP(L,5)=FsumCH
          SoluP(L,6)=CHISQ
          SoluP(L,7)=RelFluc
          DO JJ=low,high
            SoluF(L,JJ)=U(JJ)
          ENDDO
          Lmax=L

C Saves the best solution after at least 3 iterations
          IF(L.GT.3.AND.CHISQ.LT.CHIsave)THEN
            Lsave=L
            CHIsave=CHISQ
            DO I=low,high
              Usave(I)=U(I)
              Fsave(I)=F(I)
            ENDDO
          ENDIF

          IF(L.GT.3.AND.ABS( CHISQ-CHIold).LT.0.0006)       IterStop=1
          IF(L.GT.3.AND.ABS((CHISQ-CHIold)/CHISQ).LT.0.002) IterStop=1
          IF(iTry.GT.10.AND.CHISQ.GT.CHIold)                IterStop=1
          IF(iTry.GT.Iter/2.0 .AND.ModeChange.EQ.1)         IterStop=1
          iTry=iTry+1
          IF(IterStop.EQ.1)THEN
            IF(ModeChange.LT.10)THEN                   !Changing mode
              IterStop=0    !flags if going to end for one or another mode
              iTry    =0
              mode=mode*(-1)
              ModeChange=ModeChange+1
              DO I=low,high                            !Using the best solution
                F(I)=Fsave(I)                          !as initial function
                U(I)=Usave(I)
              ENDDO
            ELSE
              GO TO 100                                !End iteration          
            ENDIF
          ENDIF

C mode=-1 selects difference, mode=+1 ratio iteration
          IF(L.EQ.1)mode=-1                            !used for loop number 2
          IF(mode.EQ.-1) THEN
            DO I=low,high
              U(I)=U(I)+(Raw(I)-F(I))                  !difference mode
            ENDDO
          ELSE
            DO I=low,high
              IF(ABS(F(I)).GT.4)U(I)=U(I)*(Raw(I)/F(I))!ratio mode
            ENDDO
          ENDIF
          CHIold=CHISQ
        ENDDO

C Iteration loop for spectrum J ended

  100   CONTINUE

C Finding the best solution: It will be loop number Isc
        CALL SCORING(SoluP,Lsave,Lmax,SCORE,Isc,wfluc)

C Making compressed output in case of singles spectrum
        IF(ITYPE.EQ.1)THEN
          moil=' n'
          moim=' n'
          moir=' n'
          WRITE(6,26)
  26      FORMAT(30X,'  S U M M A R Y')
          WRITE(6,27)
          LLH=(L/3.)
          DO IL=1,LLH
            moil=' n'
            moim=' n'
            moir=' n'
            IM=IL+LLH
            IR=MIN0(IM+LLH,200)
            IF(SoluP(IL,1).EQ.-1)moil=' d'
            IF(SoluP(IL,1).EQ.+1)moil=' r'
            IF(SoluP(IM,1).EQ.-1)moim=' d'
            IF(SoluP(IM,1).EQ.+1)moim=' r'
            IF(SoluP(IR,1).EQ.-1)moir=' d'
            IF(SoluP(IR,1).EQ.+1)moir=' r'
            WRITE(6,28)IL,MOIL,SoluP(IL,6),SoluP(IL,7),SCORE(IL),
     +                 IM,MOIM,SoluP(IM,6),SoluP(IM,7),SCORE(IM),
     +                 IR,MOIR,SoluP(IR,6),SoluP(IR,7),SCORE(IR)
  27      FORMAT(' LP MD  CHISQ  Fluct SCR     LP MD  CHISQ  Fluct SCR     LP MD  CHISQ  Fluct SCR')
  28        FORMAT(I3,A2,F8.2,F7.2,A4,2(I7,A2,F8.2,F7.2,A4))
          ENDDO
        ENDIF

        modus=' n'
        IF(SoluP(Isc,1).EQ.-1)modus=' d'
        IF(SoluP(Isc,1).EQ.+1)modus=' r'
        K2=SoluP(Isc,5)+0.5
        K3=SoluP(Isc,4)+0.5

        WRITE(6,30)J,modus,K2,K3,SoluP(Isc,6),SoluP(Isc,7)
  30    FORMAT(' Row:',I4,'  Mode:',A2,'  Area:',I9,'(',I9,')  Chi:',F7.2,'  Fluct:',F6.2)

        IF(iVersion.EQ.1)THEN   !Dropping the Compton subtraction method
          DO i=0,LEN-1
            u(i)=0.0
          ENDDO
          DO i=low,high
            u(i)=SoluF(Isc,i)
          ENDDO
          GO TO 999  
        ENDIF



C***************************************************************(new begin)
C       New method: Compton Subtraction Method (Desember 1995/mg)
C       Reference: M. Guttormsen et al. NIM (1996), in press
C       The resolution in the unfolded spectrum u0 is about 0.87FWHM. 
C       Thus, it remains to smooth it with 0.5*FWHM.
C       Then we deduce S - U (rawspectrum - unfolded) to get the 
C       Compton-background etc., that is a smooth function of
C       energy (except for single+double+511 keV peaks). 
C       Therefore, we smooth
C       this differanse with FWHM, and calculate Ufinal=S-smooth(S-U). 
C       Remember, the FWHM parameter contains only 50% of
C       the value given by user: FWHM = FWHMresp = 0.5*FWHMexp
        DO i=0,LEN-1
          u0(i)   =0.0
          su0(i)  =0.0
          u(i)    =0.0
          v(i)    =0.0
          us(i)   =0.0
          ud(i)   =0.0           ! spectrum names as in NIM paper
          ua(i)   =0.0
          sua(i)  =0.0
          w(i)    =0.0
          sw(i)   =0.0
          c(i)    =0.0
          sc(i)   =0.0
        ENDDO

        CALL ERASE                 !erase the spectrum window
        icmap1=COLORMAP(1)         !saving this, and put back value at end
        ired  =COLORMAP(10)        !colors for spectra
        iblue =COLORMAP(1)
        igreen=COLORMAP(4)
        ITYPEwait=ITYPE            !have to simulate that we deal with
                                   !singles for the reason of displaying spectra
        ITYPE=1
        LOCH=0                     !display markers
        HICH=high

C Taking the best solution from earlier
        DO i=low,high
          u0(i)=SoluF(Isc,i)
        ENDDO

C Show spectrum u0
        DO i=low,high
          rSPEC(IDEST,i)=u0(i)
        ENDDO
        i1=1
        i2=2
        i3=0
        CALL SetMarker(0,2,0)
        COLORMAP(1)=iblue
        CALL DSPSP(i1,i2,i3,&333)
  333   CONTINUE

C Making us, ud and ua (single, double, annih) from unfolded spectrum
C Single and double escape peaks have the same shape as u0 (same FWHM).
        id511 =min0(high,( 511./a1)+0.5)
        id1022=min0(high,(1022./a1)+0.5)
        DO i=high,id1022,-1
          us(i- id511)=u0(i)*ps(i)
        ENDDO

        DO i=high,id1022,-1
          ud(i-id1022)=u0(i)*pd(i)
        ENDDO

        i511= min0(high,((511.-a0)/a1)+0.5)
        ua511=0.
        DO i=high,i511,-1
          ua511=ua511+u0(i)*pa(i)
        ENDDO
        Egami=(511.-a0)/a1
        il=Egami                          ! distributing counts on ch il and ih
        ih=il+1
        yl=(ih-Egami)*ua511
        yh=(Egami-il)*ua511
        IF(il.GE.0.AND.il.LE.high)ua(il)=ua(il)+yl
        IF(ih.GE.0.AND.ih.LE.high)ua(ih)=ua(ih)+yh
        ymax=AMAX1(yl,yh) !finding fwhm already present for 511 peak
        ymin=AMIN1(yl,yh)
        IF(ymax.GT.0)THEN
          w0=(ymin+ymax)/ymax
        ELSE
          w0=1.
        ENDIF
        factor=2.
        m1=max0((0.7*i511+0.5)-1,low)
        m2=min0((1.3*i511+0.5)+1,high)
        CALL GaussNaI(ua,sua,m1,m2,factor,w0)! have to be smoothed since ua is
                                             ! a spike in two channels around i511
C Making the us + ud spectrum
        DO i=low,high
          w(i)=us(i)+ud(i)
        ENDDO
C Smoothing with additional 0.5*FWHM
        factor=1.
        w0=1.           
        CALL GaussNaI(w,sw,low,high,factor,w0)
C Adding the sua spectrum to get final sw spectrum
        DO i=low,high
          sw(i)=sw(i)+sua(i)
        ENDDO
C Showing sw
        DO i=low,high
          rSPEC(IDEST,i)=sw(i)
        ENDDO
        i1=1
        i2=2
        i3=0
        CALL SetMarker(0,2,0)
        COLORMAP(1)=igreen
        CALL DSPSP(i1,i2,i3,&555)
555     continue
C Smoothing the u0 spectrum
        factor=1.
        w0=1.
        CALL GaussNaI(u0,su0,low,high,factor,w0)
C Multiplying down with pf
        DO i=low,high
          su0(i)=su0(i)*pf(i)
        ENDDO
C Making the v spectrum
        DO i=low,high
          v(i)=su0(i)+sw(i)
        ENDDO
C Making the Compton c spectrum
        DO i=low,high
          c(i)=Raw(i)-v(i)              !c is Compton-background
        ENDDO

C Showing Compton spectrum c
        i1=1
        i2=2
        i3=0
        DO i=low,high
          rSPEC(IDEST,i)=c(i)
        ENDDO
        COLORMAP(1)=ired
        CALL SetMarker(0,2,0)
        CALL DSPSP(i1,i2,i3,&666)
  666   CONTINUE

C Smoothing Compton with 1*FWHM
        factor=2.
        w0=1.
        CALL GaussNaI(c,sc,low,high,factor,w0)   !sc is smoothed Compton-background
C Showing original raw spectrum 
        i1=2
        i2=2
        i3=0
        DO i=low,high
          rSPEC(IDEST,i)=Raw(i)
        ENDDO
        COLORMAP(1)=ired
        CALL SetMarker(0,2,0)
        CALL DSPSP(i1,i2,i3,&777)
  777   CONTINUE
C Showing smoothed Compton sc+single+double+ann
        i1=2
        i2=2
        i3=0
        DO i=low,high
          rSPEC(IDEST,i)=sc(i)+sw(i)
        ENDDO
        COLORMAP(1)=igreen
        CALL DSPSP(i1,i2,i3,&888)
 888    CONTINUE

        DO i=low,high
          u(i)=Raw(i)-sc(i)-sw(i)         !u is the unfolded spectrum
        ENDDO

C Using the photo/total probability pf(i) to make u(i) contain all counts
        DO i=low,high
          IF(pf(i).GT.0.)THEN
            u(i)=u(i)/pf(i)
          ELSE
            u(i)=0.
          ENDIF
        ENDDO

C Showing final u/pf
        DO i=low,high
          rSPEC(IDEST,i)=u(i)
        ENDDO
        COLORMAP(1)=iblue
        CALL SetMarker(0,2,0)
        CALL DSPSP(i1,i2,i3,&999)

C Putting back what was in rSPEC(IDEST,i)
        DO i=0,MAXCH
          rSPEC(IDEST,i)=rWAIT(i)
        ENDDO
        ITYPE=ITYPEwait
C Putting back color
        COLORMAP(1)=icmap1

 999    CONTINUE


C********************************************************(new ended)

C Correcting for NaI efficiency as function of gamma-energy
        negstat=-1
        DO i=0,high
          rMAT(IDEST,I,J)=0
          rSPEC(IDEST,I)=0
          IF(u(i).GT.0)negstat=+1
          IF(i.lt.low)u(i)=0
          IF(negstat.EQ.-1)u(i)=0 !remove neg. counts in the first channels
          IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
            effi=EffTot(I)*EffExp(I)
            IF(ITYPE.GT.1.AND.effi.GT.0.)rMAT(IDEST,I,J)=u(i)/effi
            IF(ITYPE.EQ.1.AND.effi.GT.0.)rSPEC(IDEST,I)=u(i)/effi
          ELSE
            IF(ITYPE.GT.1)rMAT(IDEST,I,J)=u(i)
            IF(ITYPE.EQ.1)rSPEC(IDEST,I)=u(i)
          ENDIF
        ENDDO

      ENDDO                                !J-loop for all spectra ended
      fname(2,IDEST)=waitname              !Putting back old name
      END



