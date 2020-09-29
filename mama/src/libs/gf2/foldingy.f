C All the functions and subroutines below are taken from the
C folding.f program, but tailored for unfolding the y-axis,
C which is the excitation energy in the (Ex,Eg) matrix of
C the Oslo method. All routines, commands and COMMON end
C with y. In particular, the foldingy.f is made for
C uncomplete summing and beta absorption of total absorbtion
C spectrometers (TAS) like SuN (MSU).
C January 2017/Artemis Spyrou and Magne Guttormsen

      FUNCTION Flucy(F,ChiLow,ChiHigh,a1)
C Calculates fluctuations in a spectrum F between ChiLow and ChiHigh
C by (F(i)-Faverage)/Faverage. The average is taken over 5 cannels, and so on.
      DIMENSION F(0:2047)
      INTEGER ChiLow,ChiHigh
      iFW = 5
      Fluc = 0.0
      nChannels = 0
      i1=ChiLow
      i2=ChiLow + iFW

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
        i1 = i2 + 1
        i2 = i1 + iFW
      ENDDO
      Flucy=Fluc/nChannels
      RETURN
      END

      SUBROUTINE Foldy
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM

      IF(RDIM.EQ.0)THEN
        WRITE(6,*)'No response function present'
        WRITE(6,*)'Use the RY command to create one or take a cup of coffee'
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
        CALL FoldIty
        WRITE(6,5)a0,a1
 5      FORMAT('Calibration is a0=',F6.1,'keV, a1=',F7.2,'keV/ch')
        cal(1,IDEST,2,1)=a0
        cal(1,IDEST,2,2)=a1
        cal(1,IDEST,2,3)=0
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)
        IF(a0+a1.NE.cal(1,ISP,2,1)+cal(1,ISP,2,2))THEN
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
        CALL FoldIty
        WRITE(6,8)a0,a1
 8      FORMAT('Calibration is a0=',F8.1,'keV, a1=',F7.2,'keV/ch')
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
        xcomm(1:3)='FY:'
        fname(1,IDEST)(1:8)='FY'//fname(1,ISP)(1:6)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ELSE
        xcomm(1:3)='FY:'
        fname(2,IDEST)(1:8)='FY'//fname(2,ISP)(1:6)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)
      ENDIF        
   
      END


      SUBROUTINE FoldIty
C        U      Unfolded spectrum
C        F      Folded spectrum, F=R(I,J)*U
C        a0,a1  Calibration for spectrum to be folded
C        LEN    Length of spectrum

      CHARACTER APP*4,ANS
      INTEGER XDIM,YDIM,RDIM,UPPER(0:2047)
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM
      COMMON/responsey3/EffTot(0:2047),Fwhm1(0:2047),EffExp(0:2047)
      COMMON/responsey6/LOW, HIGH, Iter, ix1,iy1,ix2,iy2
      INTEGER LOW,HIGH

      INTEGER lower(0:2047)
      REAL U(0:2047),F(0:2047)

      ISP=1
      IF(IDEST.EQ.1)ISP=2

C Zeroing destination spectrum
      IF(ITYPE.GT.1)THEN
        IYDIM = YDIM
        IXDIM = XDIM
        DO I=0,4095
          DO J=0,2047
            rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO
      ENDIF
      IF(ITYPE.EQ.1)THEN
        IXDIM=1
        IYDIM=MAXCH
        IY2=0
        DO I=0,8191
          rSPEC(IDEST,I)=0
        ENDDO
      ENDIF

      IF(ITYPE.GT.1)THEN
        LEN = YDIM
      ELSE
        LEN = MAXCH+1
      ENDIF
      IF(RDIM.LT.LEN)LEN=RDIM
      HIGH = LEN-1

      ExMin = 30.                       ! Lower Ex limit = 30 keV
      LOW   = INT(((ExMin-a0)/a1)+0.5)
      IF(LOW.LT.0.OR.LOW.GE.HIGH)LOW = 0

C ****Begin, Fix for first run****
      IF(HIGH.EQ.0)THEN
        LOW  = 0
        HIGH = LEN-1
      ENDIF
      IF(Ix1+Ix2+Iy1+Iy2.LT.10)THEN
        Ix1=XDIM-1
        Ix2=XDIM-1
        Iy1=0
        Iy2=HIGH
      ENDIF
      IF(Iter.EQ.0)THEN
        Iter=50
      ENDIF
C ****End, Fix for first run****

      IF(ITYPE.GT.1)THEN                !matrix
        WRITE(6,*)'Give lower y-channel limits for the folding along y-axis.'
        WRITE(6,*)'It is given by interpolation between (x1,y1) and (x2,y2)'
        WRITE(6,*)' '
        WRITE(6,*)'            (x2,y2)  second point'
        WRITE(6,*)'xxxxxxxxxxxxxxx'
        WRITE(6,*)'xx matrix xxx'
        WRITE(6,*)'xxxxxxxxxxx'
        WRITE(6,*)'xxxxxxxxx'
        WRITE(6,*)'xxxxxxx'
        WRITE(6,*)'    (x1,y1)  first point'

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
        WRITE(6,13)LOW
 13     FORMAT(/'Give lower channel for folding    <',I5,'>:',$)
        CALL READI(5,LOW)
        WRITE(6,12)HIGH
 12     FORMAT(/'Give upper channel for folding    <',I5,'>:',$)
        CALL READI(5,HIGH)
      ENDIF
      IF(Istatus.NE.0)RETURN

      CF=0.
      Dx12=Ix2-Ix1
      Dy12=Iy2-Iy1
      IF(Ix2.NE.Ix1)CF=Dy12/Dx12

      DO I=0,IXDIM-1
        lower(I)=INT(Iy1+CF*(FLOAT(I-Ix1))+0.5)
        IF(CF.EQ.0.)lower(I)=LOW
        IF(lower(I).LT.0 )lower(I)=0
        IF(lower(I).GT.LEN-1)lower(I)=LEN-1
      ENDDO

CCCCCCCCCCCCCCCCCCCCCC
CCC
CCC Main loop starts
CCC
CCCCCCCCCCCCCCCCCCCCCC
      IF(ITYPE.GT.1)THEN  ! It is a matrix
        DO I=0,IXDIM-1
          LOWi=lower(I)
C Getting the unfolded spectrum into U(J) and correcting for the total
C detector response function for the unfolded data
          DO J=0,2047
            U(J)=0.
            F(J)=0.
          ENDDO
          DO J=LOWi,HIGH
            IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
              effi=EffTot(J)                        !*EffExp(J)
              U(J)=rMAT(ISP,I,J)*effi
            ELSE
              U(J)=rMAT(ISP,I,J)
            ENDIF
          ENDDO

C FOLDING : F(J)=R(I,J)*U(J)
          DO J=LOWi,HIGH
            DO K=LOWi,HIGH
              F(J)=F(J)+R(J,K)*U(K)
            ENDDO
          ENDDO
C PUTTING THE FOLDED SPECTRUM INTO DESTINATION MATRIX
          DO J=LOWi,HIGH
           rMAT(IDEST,I,J)=F(J)
          ENDDO
        ENDDO

      ELSE       !It is a singles Ex spectrum

        DO J=0,HIGH
          U(J)=0.
          F(J)=0.
        ENDDO
        DO J=LOW,HIGH
          IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
            effi=EffTot(J)                        !*EffExp(J)
            U(J)=rSPEC(ISP,J)*effi
          ELSE
            U(J)=rSPEC(ISP,J)
          ENDIF
        ENDDO

C FOLDING : F(J)=R(I,J)*U(J)
        DO J=LOW,HIGH
          DO K=LOW,HIGH
            F(J)=F(J)+R(J,K)*U(K)
          ENDDO
        ENDDO
C PUTTING THE FOLDED SPECTRUM INTO DESTINATION SPECTRUM
        DO J=LOW,HIGH
         rSPEC(IDEST,J)=F(J)
        ENDDO
      ENDIF
CCCCCCCCCCCCCCCCCCCCCC
CCC
CCC Main loop ended
CCC
CCCCCCCCCCCCCCCCCCCCCC
      END


      FUNCTION GAUSSy(E,EE,W)
C Calculates a Gaussian distribution with centroid at E and half-width W
      SIG=W/2.35
      A=-((EE-E)**2)/(2.*SIG**2)
      GAUSSy= (1./(SIG*SQRT(2.*3.141592)))*EXP(A)
      END


      SUBROUTINE GaussSmoothingy(x,y,l1,l2,factor,w0)
C Folding the y-axis with a Gauss distribution with sig=FWHM/2.35
C You may reduce the FWHM value with factor between 0 and 1
C The w0 parameter is the resolution already present due to bin-width of channels
C It takes values between 1 and 2
      INTEGER RDIM
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM
      COMMON/responsey3/EffTot(0:2047),Fwhm1(0:2047),EffExp(0:2047)
      DIMENSION x(0:2047),y(0:2047)
      W=0
      DO i=0,2047
        y(i)=0.
      ENDDO
      IF(w0.LT.0.OR.w0.GT.2)THEN
        write(6,*)'w0 = ',w0,' is out of range, changed to w0 = 1.0'
        w0=1.
      ENDIF
      w0=w0*ABS(a1)        !from channels to energy
      DO I=l1,l2
        E=a0+I*a1
        Wtot=Fwhm1(I)*(factor*FWHM/100.)*E  !The FWHM is the true relative width
        xx=(Wtot*Wtot)-(w0*w0)
        IF(xx.GT.0         )W=SQRT(xx)
        IF(W .LT.ABS(a1)/5.)W=ABS(a1)/5.  

C Finding integration limits. Going 3*sigma to each side
        Kmin=((E-a0-(3.*W/2.35))/a1)+0.5
        Kmax=((E-a0+(3.*W/2.35))/a1)+0.5
        IF(Kmin.LT.l1)Kmin=l1
        IF(Kmax.LT.l1)Kmax=l1
        IF(Kmax.GT.l2)Kmax=l2
        yK=0                     !used as normalization (should be =1)
        DO K=Kmin,Kmax
          EE=a0+K*a1
          yK=yK+GAUSSy(E,EE,W)*a1
        ENDDO
        IF(yK.LE.0)yK=10000000
        DO K=Kmin,Kmax
          EE=a0+K*a1
          y(K)=y(K)+((x(I)*GAUSSy(E,EE,W)*a1)/yK)
        ENDDO
      ENDDO
      END


      SUBROUTINE GetRespy
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM

      IF(RDIM.EQ.0)THEN
        WRITE(6,*)'No response matrix present'
        WRITE(6,*)'Use the RM command to create one'
        Istatus=1
        RETURN
      ENDIF
      IF(ITYPE.EQ.1)ITYPE=3
      IDUM=1
      IF(IDEST.EQ.1)IDUM=2        !To prevent overwriting previous spectrum
      IDEST=IDUM

      WRITE(6,1)IDEST
   1  FORMAT('Destination spectrum <',I1,'>:',$)
      CALL READI(5,IDEST)
      IF(IDEST.LT.1.OR.IDEST.GT.2)Istatus=1

      IF(Istatus.NE.0)RETURN
      DO I=0,4095
        IT=(I/400)*400
        IF(IT.EQ.I)THEN
          write(6,FMT='(A1,$)')'.'
          call flush(6)
        ENDIF
        DO J=0,2047
          rMAT(IDEST,I,J)=0
        ENDDO
      ENDDO

      DO I=0,RDIM-1
        IT=(I/200)*200
        IF(IT.EQ.I)THEN
          write(6,FMT='(A1,$)')'.'
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
      xcomm(1:4)='|GRy:'
      fname(1,IDEST)='RespFuncy'
      comm(1,IDEST)=''
      CALL AddComment(xcomm,4)

      END


      SUBROUTINE Respy
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      CHARACTER FILNAM*255
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM
      COMMON/responsey2/IR,IM,IQ,Qvalue,ETAB(18),FTAB(18),EW(18),FW(18),
     +Mex(18),ER(18),MUE(18),QVA(18),ENA(18),MNA(18),QNA(18)
      COMMON/responsey3/EffTot(0:2047),Fwhm1(0:2047),EffExp(0:2047)
      COMMON/responsey5/FE(1:18,1:5)
      COMMON/responsey6/LOW, HIGH, Iter, ix1,iy1,ix2,iy2
      INTEGER LOW,HIGH, ChiHigh

      CHARACTER ENA*5, MNA*2, QNA*2
      DIMENSION  Cnts(1:18,1:18,1:18)
      REAL mx, mx_old, Mex, MUE, QVA, Qvalue
      DATA RDIM/0/

      REAL ETAB1(18),FTAB1(18),ER1(18),FE1(18),MUE1(18)
      REAL FE1m1(18),FE1m2(18),FE1m3(18),FE1m4(18),FE1m5(18),EW1(18),FW1(18)
      CHARACTER ENA1(18)*5, MNA1(18)*2

      REAL ETAB2(18),FTAB2(18),ER2(18),FE2(18),MUE2(18),QVA2(18)
      REAL EW2(18),FW2(18)
      CHARACTER ENA2(18)*5, MNA2(18)*2, QNA2(18)*2

C*****************************************************************
C********************* SuN_70Ni_2016 *****************************
C*****************************************************************

C This block of data represents energies (ETAB) and the total efficience (FTAB)
C at that energy for 5"x5" NaI. Normalized to 1 at 1.33 MeV     
      DATA  ETAB1   /
     +   1000.,  2000.,  3000.,  4000.,   5000.,   6000.,
     +   7000.,  8000.,  9000., 10000.,  11000.,  12000.,
     +      0.,     0.,     0.,     0.,      0.,      0./
      DATA  FTAB1   /
     +      1.,     1.,     1.,     1.,      1.,      1.,
     +      1.,     1.,     1.,     1.,      1.,      1.,
     +      0.,     0.,     0.,     0.,      0.,      0./

C This bolck of data represents Ex energies (ER) for resp. func.,
C intensities of full-energy (FE) peak. The numbers are counts
C of the various response functions. The counts are converted into probabilities
C pf in the RSPDetectory routine. There are sets for multiplicities m=1,2,3,4,5
C Two last blocks are used to create names for the response function files
      DATA  ER1   /
     +   1000.,  2000.,  3000.,  4000.,   5000.,   6000.,
     +   7000.,  8000.,  9000., 10000.,  11000.,  12000.,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA  FE1m1   /
     +     2.16E+04,    2.10E+04,    2.03E+04,   1.72E+04,    1.72E+04,    1.59E+04,
     +     1.45E+04,    1.42E+04,    1.27E+04,   1.08E+04,    9.39E+03,    7.67E+03,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA  FE1m2   /
     +     1.82E+04,    1.84E+04,    1.83E+04,   1.68E+04,    1.50E+04,    1.36E+04,
     +     1.29E+04,    1.17E+04,    1.05E+04,   8.68E+03,    7.60E+03,    5.83E+03,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA  FE1m3   /
     +     1.67E+04,    1.69E+04,    1.64E+04,   1.55E+04,    1.44E+04,    1.38E+04,
     +     1.25E+04,    1.10E+04,    1.01E+04,   8.39E+03,    7.34E+03,    5.67E+03,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA  FE1m4   /
     +     1.56E+04,    1.66E+04,    1.57E+04,   1.43E+04,    1.37E+04,    1.32E+04,
     +     1.18E+04,    9.66E+03,    8.59E+03,   7.70E+03,    6.57E+03,    5.09E+03,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA  FE1m5   /
     +     1.69E+04,    1.56E+04,    1.52E+04,   1.38E+04,    1.33E+04,    1.11E+04,
     +     1.10E+04,    1.04E+04,    7.72E+03,   6.46E+03,    4.61E+03,    4.09E+03,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA MUE1/
     +      1.,  2.,  3.,   4.,  5.,  0.,
     +      0 ,  0 ,  0 ,   0 ,  0 ,  0 ,
     +      0 ,  0 ,  0 ,   0 ,  0 ,  0 /

      DATA ENA1/
     +     '01', '02', '03',  '04',  '05', '06',
     +     '07', '08', '09',  '10',  '11', '12',
     +     '0' , '0' , '0' ,  '0' ,  '0' ,  '0'/

      DATA MNA1/
     +     '01', '02', '03',  '04', '05', '00',
     +     '00', '00', '00',  '00', '00', '00',
     +     '00', '00', '00',  '00', '00', '00'/

C This block of data represents energies (EW) and half-widths (FW) of the
C summed gamma (Ex) resolution. Normalized to 1. for Ex = 1 MeV
      DATA  EW1   /
     +   1000.,  2000.,  3000.,  4000.,   5000.,   6000.,
     +   7000.,  8000.,  9000., 10000.,  11000.,  12000.,
     +      0.,     0.,     0.,     0.,      0.,      0./
      DATA  FW1   /
     +   1.000,  0.707,  0.626,  0.556,   0.527,   0.494,
     +   0.463,  0.424,  0.402,  0.370,   0.349,   0.346,
     +      0.,     0.,     0.,     0.,      0.,      0./


C*****************************************************************
C****************** SuN_variable_Qvalue_2018 *********************
C*****************************************************************

C This block of data represents energies (ETAB) and the total efficience (FTAB)
C at that energy for 5"x5" NaI. Normalized to 1 at 1.33 MeV
      DATA  ETAB2   /
     +   1000.,  2000.,  3000.,  4000.,   5000.,   6000.,
     +   7000.,  8000.,  9000., 10000.,  11000.,  12000.,
     +      0.,     0.,     0.,     0.,      0.,      0./
      DATA  FTAB2   /
     +      1.,     1.,     1.,     1.,      1.,      1.,
     +      1.,     1.,     1.,     1.,      1.,      1.,
     +      0.,     0.,     0.,     0.,      0.,      0./

C This bolck of data represents Ex energies (ER) for resp. func.
C The intensities (counts) of full-energy (FE) peak are read from file.
C The counts are converted into probabilities
C pf in the RSPDetectory routine.
C Then there are sets for multiplicities m=1,2,3,4,5
C And then there are sets for Q-values 5, 6, ...12 MeV
C Two last 3 blocks are used to create names for the response function files
      DATA  ER2   /
     +   1000.,  2000.,  3000.,  4000.,   5000.,   6000.,
     +   7000.,  8000.,  9000., 10000.,  11000.,  12000.,
     +      0.,     0.,     0.,     0.,      0.,      0./

      DATA MUE2/
     +      1.,  2.,  3.,  4.,  5.,  0.,
     +      0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
     +      0 ,  0 ,  0 ,  0 ,  0 ,  0 /

      DATA QVA2/
     +      5000.,  6000.,  7000.,  8000.,  9000.,  10000.,
     +     11000., 12000.,     0 ,     0 ,     0 ,      0 ,
     +         0 ,     0 ,     0 ,     0 ,     0 ,      0 /

      DATA ENA2/
     +     '01', '02', '03', '04', '05', '06',
     +     '07', '08', '09', '10', '11', '12',
     +     '00', '00', '00', '00', '00', '00'/

      DATA MNA2/
     +     '01', '02', '03', '04', '05', '00',
     +     '00', '00', '00', '00', '00', '00',
     +     '00', '00', '00', '00', '00', '00'/

      DATA QNA2/
     +     '05', '06', '07', '08', '09', '10',
     +     '11', '12', '00', '00', '00', '00',
     +     '00', '00', '00', '00', '00', '00'/

C This block of data represents energies (EW) and half-widths (FW) of the
C summed gamma (Ex) resolution. Normalized to 1. for Ex = 1 MeV
      DATA  EW2   /
     +   1000.,  2000.,  3000.,  4000.,   5000.,   6000.,
     +   7000.,  8000.,  9000., 10000.,  11000.,  12000.,
     +      0.,     0.,     0.,     0.,      0.,      0./
      DATA  FW2   /
     +   1.000,  0.707,  0.626,  0.556,   0.527,   0.494,
     +   0.463,  0.424,  0.402,  0.370,   0.349,   0.346,
     +      0.,     0.,     0.,     0.,      0.,      0./

C*****************************************************************
C*****************************************************************
C*****************************************************************

C*******************DEFAULTS******************
      Qvalue  = 12300
      facFWHM = 10.0    !We use only FWHM/facFWHM in the response function
      IRSP    = 2

      IF(ITYPE.GT.1)THEN
        LEN = YDIM
      ELSE
        LEN = MAXCH+1
      ENDIF
      HIGH = LEN-1
      ExMin = -300.                       ! Lower Ex limit = -300 keV
      LOW   = INT(((ExMin-a0)/a1)+0.5)
      IF(LOW.LT.0.OR.LOW.GE.HIGH)LOW = 0

      OPEN(23,FILE='input.uny',STATUS='old',ERR=77)
      READ(23,*,END=66,ERR=66)IRSP,Qvalue,facFWHM,FWHM
      READ(23,*,END=66,ERR=66)ix1,iy1,ix2,iy2
      READ(23,*,END=66,ERR=66)(Mex(i),i=1,18)
      READ(23,*,END=66,ERR=66)LOW,HIGH
      READ(23,*,END=66,ERR=66)Iter,ChiHigh
      GO TO 77
 66   WRITE(6,*)'Warning: Something wrong with your input.uny file'
 77   CLOSE(23)

C ****Begin, Fix for first run****
      IF(Qvalue.LT.0.001)Qvalue = 12300
      IF(HIGH.EQ.0.OR.HIGH.LT.ChiHigh)THEN
        LOW  = 0
        HIGH = LEN-1
      ENDIF
      IF(Ix1+Ix2+Iy1+Iy2.LT.10)THEN
        Ix1=XDIM-1
        Ix2=XDIM-1
        Iy1=0
        Iy2=HIGH
      ENDIF
      IF(Iter.EQ.0)THEN
        Iter=50
      ENDIF
      IF(Mex(1)+Mex(2)+Mex(3)+Mex(4)+Mex(5)+Mex(6).LT.0.1)THEN
        c0 = 0.00
        c1 = 0.75
        c2 = -0.03
        mx = 0
        mx_old = -1000.
        DO i=1,18                 ! Remember max 18 elements in vectors
          ex = ER2(i)/1000.
          mx = c0 + c1*ex + c2*ex*ex
          IF(mx.LE.mx_old)mx=mx_old + 0.1
          mx_old = mx
          Mex(i) = mx
        ENDDO
      ENDIF
C ****End, Fix for first run****

C Reading dimension and energy calibration
      IITYPE=2
      IF(ITYPE.GT.1)IITYPE=1
      RDIM=Idim(IITYPE,IDEST,2)
      IF(RDIM.LE.10.OR.RDIM.GT.2048)RDIM=2048
      WRITE(6,1)RDIM
   1  FORMAT('Dimension of response-matrix <',I4,'>:',$)
      CALL READI(5,RDIM)
      IF(RDIM.GT.2048)THEN
        WRITE(6,*)'Too big response matrix, maximum is 2048 x 2048'
        Istatus=1
        RETURN
      ENDIF

      IF(IITYPE.EQ.1)THEN
        a0=cal(IITYPE,IDEST,2,1)
        a1=cal(IITYPE,IDEST,2,2)
      ELSE
        a0=cal(IITYPE,IDEST,1,1)
        a1=cal(IITYPE,IDEST,1,2)
      ENDIF

      IF(a0+a1.EQ.1.)THEN  
        a0=0.0
        a1=20.0
      ENDIF

      WRITE(6,10)a0
  10  FORMAT(/'Cal. coeff. a0 (keV)     <',F8.1,'>:',$)
      CALL READF(5,a0)
      WRITE(6,11)a1
  11  FORMAT( 'Cal. coeff. a1 (keV/ch)  <',F8.1,'>:',$)
      CALL READF(5,a1)

      IF(a1.LE.0)THEN
         write(6,*)' Negative dispersion a1 not allowed. You have to invert the'
         write(6,*)' axis so that the energy increases with channel number.'
         write(6,*)' You may use the EL command for this.'
         RETURN
      ENDIF

      WRITE(6,*)' '
      IRSP=2
      WRITE(6,*)'List of response functions:'
      WRITE(6,*)'(1) SuN_70Ni_2016 (Q-value = 12.3 MeV is hardwired)'
      WRITE(6,*)'(2) SuN_variable_Qvalue_2018 (you are asked to give reaction Q-value)'
      WRITE(6,*)'(3) For future expansion...'

      WRITE(6,2)IRSP
   2  FORMAT(/'Choose your response function <',I1,'>:',$)
      CALL READI(5,IRSP)
      IF(Istatus.NE.0)RETURN
      IF(IRSP.GT.2)THEN
        write(6,*)'Response function not yet implemented, take a cup of tea and wait...'
        RETURN
      ENDIF

C Copy all vectors used for a specific response function (IRSP) into the
C general vectors to be used from now on in the subroutine RSPDetectory(IRSP)
      IF(IRSP.EQ.1)THEN
        IW  = 12
        ITAB= 12
        IR  = 12
        IM  =  5
        IQ  = -1  !Not interpolate between qValues(Qvalue hardwired in respons function)
        DO i=1,IW
          EW(i)=EW1(i)
          FW(i)=FW1(i)
        ENDDO
        DO i=1,ITAB
          ETAB(i)=ETAB1(i)
          FTAB(i)=FTAB1(i)
        ENDDO
        DO i=1,IR
          ER(i)  =ER1(i)
          ENA(i) =ENA1(i)
          FE(i,1)=FE1m1(i)
          FE(i,2)=FE1m2(i)
          FE(i,3)=FE1m3(i)
          FE(i,4)=FE1m4(i)
          FE(i,5)=FE1m5(i)
        ENDDO
        DO i=1,IM
          MNA(i)=MNA1(i)
          MUE(i)=MUE1(i)
        ENDDO
      ENDIF

C***********************************
      IF(IRSP.EQ.2)THEN

        IW  = 12 !Remember: none of these 5 numbers should be >18 (array dimensions)
        ITAB= 12
        IR  = 12
        IM  =  5
        IQ  =  8
        DO jQ = 1,18   !Zeroing matrix
          DO jM = 1,18
            DO jE = 1,18
              Cnts(jQ,jM,jE) = 0.
            ENDDO
          ENDDO
        ENDDO

      WRITE(6,12)Qvalue
  12  FORMAT( 'Give Q-value of beta decay (keV) <',F7.0,'>:',$)
      CALL READF(5,Qvalue)
      IF(Qvalue.LT.20.OR.Qvalue.GT.20000) write(6,*) 'Warning: OMG, what a bizarr Q-value!!!'

C Reading counts in the FE peak as function of Q-value and multiplicity M into a
C matrix called Cnts(jQ,jM,jE). The table is created from GEANT response functions
C via a root script called read_response_bOslo.pp (author Ann-Cecilie Larsen 2018)
        INF = 21
        call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/SuN_Exresp_sumpeak_parameters.txt",filnam)
        OPEN(INF,FILE=FILNAM,ACCESS='SEQUENTIAL',ERR=800)
        DO i = 1,IR*IM*IQ        ! too many, but terminate when Q, M and E are maximum
          READ(INF,*,ERR=800)i1,i2,i3,dum,dum,dum,counts
          jQ=i1-4
          jM=i2
          jE=i3
          Cnts(jQ,jM,jE)=counts
          IF(jQ.EQ.8.AND.jM.EQ.5.AND.jE.EQ.12) GOTO 800
        ENDDO
800     CONTINUE
        CLOSE(INF)

        DO jQ = 1,IQ
          DO jM = 1,IM
c            write(6,*)jQ,jM,(Cnts(jQ,jM,jE),jE=1,3)
          ENDDO
        ENDDO

        DO i=1,IW
          EW(i)=EW2(i)
          FW(i)=FW2(i)
        ENDDO
        DO i=1,ITAB
          ETAB(i)=ETAB2(i)
          FTAB(i)=FTAB2(i)
        ENDDO
        DO i=1,IM
          MNA(i)=MNA2(i)
          MUE(i)=MUE2(i)
        ENDDO
        DO i=1,IQ
          QNA(i)=QNA2(i)
          QVA(i)=QVA2(i)
        ENDDO
C Finding FE counts above and below the Qvalue and interpolate (extrapolate)
        Iq1=1
        DO I=1,IQ
          IF(Qvalue.GT.QVA(I))Iq1=I
        ENDDO
        Iq2=Iq1+1
        IF(Iq1.EQ.IQ) THEN
          Iq1=IQ-1
          Iq2=IQ
        ENDIF
        wQ1 = (QVA(Iq2)-Qvalue)/(QVA(Iq2)-QVA(Iq1))          ! the weight FE of Q1 spectrum
        wQ2 = 1. - wQ1                                       ! the weight FE of Q2 spectrum
        DO i=1,IR
          ER(i)  = ER2(i)
          ENA(i) = ENA2(i)
          FE(i,1)= Cnts(Iq1,1,i)*wQ1 + Cnts(Iq2,1,i)*wQ2  ! we have 5 multiplicities
          FE(i,2)= Cnts(Iq1,2,i)*wQ1 + Cnts(Iq2,2,i)*wQ2
          FE(i,3)= Cnts(Iq1,3,i)*wQ1 + Cnts(Iq2,3,i)*wQ2
          FE(i,4)= Cnts(Iq1,4,i)*wQ1 + Cnts(Iq2,4,i)*wQ2
          FE(i,5)= Cnts(Iq1,5,i)*wQ1 + Cnts(Iq2,5,i)*wQ2
C      if(i.eq.5)write(6,*)'c1,c2,iq1,iq2,Qvalue,F',Cnts(Iq1,3,i),Cnts(Iq2,3,i),iq1,iq2,Qvalue,FE(i,3)
        ENDDO
      ENDIF

C The response function along Ex (y-axis) depends on the gamma multiplicity.
C The user has to type in this multiplicity.
C Default suggestion made by a second degree polynom, rather arbitary M(Ex)
      WRITE(6,*)' '
      WRITE(6,*)'The reponse function along Ex (y-axis) depends on the gamma multiplicity.'
      WRITE(6,*)'The default values listed below are very tentative.'
      WRITE(6,*)'You may get more realistic values after using the command fg in the next step.'
      WRITE(6,*)'The multiplicity is then listed in figegaout.dat, and you should then make'
      WRITE(6,*)'a new unfolding based on the updated multiplicities.'
      WRITE(6,*)'One such iteration should be sufficient.'
      WRITE(6,*)''
      WRITE(6,*)'(You have to write multiplicities for all energies up to'
      WRITE(6,*)'12000 keV, even when you have a Q-beta value < 12000 keV.)'

      DO i=1,IR
        ex = ER(i)/1000.
        mx = Mex(i)
        WRITE(6,20)ER(i),mx
 20     FORMAT('Type multiplicity at Ex = ',F7.1,' keV: <',F6.2,'>:',$)
        CALL READF(5,mx)
        IF(Istatus.NE.0)RETURN
        Mex(i)=mx
      ENDDO

C  New feature implemented March 2013 to prevent oscillations for high energy peaks
C  We use small FWHM for the full energy
      WRITE(6,*)' '
      WRITE(6,*)'If you are unfolding/folding an (Eg,Ex) matrix or Ex spectrum,'
      WRITE(6,*)'you should create response functions with 10 times less FWHM' 
      WRITE(6,*)'than experimentally known, i.e. FWHM(response) = FWHM(real)/10,'
      WRITE(6,*)'which is optimal for the unfolding (UY) and folding (FY) procedures.'
      WRITE(6,*)'If you just want to view the response matrix with command GY,'
      WRITE(6,*)'then use a factor = 1, i.e. FWHM(response) = FWHM(real).'
      WRITE(6,3)facFWHM
   3  FORMAT(/'Response functions for unfolding/folding (10.) or viewing (1.) <',F4.1,'>:',$)
      CALL READF(5,facFWHM)
      IF(Istatus.NE.0)RETURN

      DO I=0,2047
C Finding FWHMs as function of full energy, normalized to 1 at Ex=1 MeV
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

      CALL RSPDetectory(IRSP)

C Writes parameters to disk, to be used for next run
      OPEN(23,FILE='input.uny',ACCESS='SEQUENTIAL',ERR=888)
      WRITE(23,*)IRSP,Qvalue,facFWHM,FWHM
      WRITE(23,*)ix1,iy1,ix2,iy2
      WRITE(23,*)(Mex(i),i=1,18)
      WRITE(23,*)LOW,HIGH
      WRITE(23,*)Iter,ChiHigh
      CLOSE(23)
888   CONTINUE

      END


      SUBROUTINE RSPDetectory(IRSP)
C     Routine for making detector response-functions based on
C     interpolation between experimental spectra or GEANT4 spectra.

C     The response function is called R(I,J). J is the
C     sharp Ex, and I is the response Ex (a distribution)

C     The response functions read from disk represent incomplete adding and
C     beta-energies. The intensities of the full-energy peak are
C     removed from the spectra. Their intensities are treated separately
C     and are given in the COMMON/responsey5/FE(Ex,M) as a peak in 1 or 2 channes.
C     One of the last task of this subroutine is to fold
C     the interpolated spectrum with a Gaussian.
C     Some parameters are:
C        Ex     Fullenergy Ex (in keV)
C        R(i,j) Response matrix. Intensity R in ch. i for full-energy j
C        RDIM   Length of response spectrum
C        a0,a1  Energy-calibration of spectrum to be unfolded
C        b0,b1  Energy-calibration of original experimental response functions
C        FWHM   Relative half-width (FWHM) of spectrum at 1.33 MeV
C        FE     Counts at in sum peak at Ex(ER)
C        FW     Half-width (FWHM) at energy EW (normalized to 1 for Ex = 1 MeV)


      INTEGER XDIM,YDIM,RDIM
      CHARACTER ENA*5,MNA*2,QNA*2,APP*4,FILNAM*255,comment*60
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM
      COMMON/responsey2/IR,IM,IQ,Qvalue,ETAB(18),FTAB(18),EW(18),FW(18),
     +Mex(18),ER(18),MUE(18),QVA(18),ENA(18),MNA(18),QNA(18)
      COMMON/responsey3/EffTot(0:2047),Fwhm1(0:2047),EffExp(0:2047)
      COMMON/responsey4/pf(0:2047),pc(0:2047)
      COMMON/responsey5/FE(1:18,1:5)
      COMMON/responsey6/LOW, HIGH, Iter, ix1,iy1,ix2,iy2
      INTEGER LOW,HIGH

      DIMENSION FEn(18)
      DIMENSION Calib(6)
      INTEGER dim
      DIMENSION Fe1m1(0:4095),Fe1m2(0:4095),Fe2m1(0:4095),Fe2m2(0:4095) !to be used for IRSP=1 modus
      DIMENSION Fe1m1q1(0:4095),Fe1m1q2(0:4095),Fe1m2q1(0:4095),Fe1m2q2(0:4095) !to be used for IRSP=2 modus
      DIMENSION Fe2m1q1(0:4095),Fe2m1q2(0:4095),Fe2m2q1(0:4095),Fe2m2q2(0:4095)
      DIMENSION Fs(0:4095),F1(0:4095),F2(0:4095),Flow(0:4095),Fhigh(0:4095)
      DIMENSION F(0:2047),G(0:2047), Spec(0:8191)
      DIMENSION Ffu(0:2047),Gfu(0:2047)
      CHARACTER*20 DATTIM
      REAL mx,Mex,MUE, mxv(0:2047),QVA,Qvalue

C The measured response functions have energy
C calibration b0 (keV) and b1(keV/ch) 
C Response matrix R(i,j) has maximum dimensions 2048x2048
      IF(IRSP.EQ.1)THEN
        b0  =1.0
        b1  =10.
        FWHM=6.75
      ENDIF

      IF(IRSP.EQ.2)THEN
        b0  =5.0
        b1  =10.
        FWHM=6.75
      ENDIF

      ical=6
      DO J=0,2047
        F(J)=0.
        DO I=0,2047
          R(I,J)= 0.
        ENDDO
      ENDDO

C Reading FWHM at Ex = 1. MeV. It will be multiplied by the function
C Fwhm1(i), which is normalized to 1 at Ex = 1. MeV
      WRITE(6,12)FWHM
  12  FORMAT('Real experimental relative FWHM value at Egam=1. MeV (%)<',F8.3,'>:',$)
      CALL READF(5,FWHM)
      FWHMunf=FWHM/facFWHM ! NB, we operate with FWHMunf = FWHM/facFWHM for unfolding
      WRITE(6,*)
      WRITE(6,13)facFWHM
  13  FORMAT('(In the calculation the response function is obtained',
     +/'with a FWHM value of (1/',F6.3,')*(the real experimental FWHM)'
     +/'due to technical reasons in the unfolding procedure)')
      IF(Istatus.NE.0)RETURN

C Finding lower and upper limit for the response function
      ExMin= 30.                     ! Lower Ex limit = 30 keV
      MinEx=INT(((ExMin-a0)/a1)+0.5)
      IF(MinEx.LT.0)  MinEx=0
      IF(MinEx.GT.2047)MinEx=2047
      iEx0 = INT(((0-a0)/a1)+0.5)      ! The ch where Ex = 0 keV
      IF(IRSP.EQ.1)Qvalue = 12300    ! Hardwired in response functions
      ExMax = Qvalue + 1000.       ! Upper limit for the response function (1000 keV above Q-value)
      MaxEx = INT(((ExMax-a0)/a1)+0.5)
      IF(MaxEx.LT.0)  MaxEx=0
      IF(MaxEx.GT.2047)MaxEx=2047

      ExMax = a0 + a1*MaxEx
      ExMin = a0 + a1*MinEx

      WRITE(6,30)ExMin,ExMax
 30   FORMAT(/'Energy region of response function is:',F8.1,' -',F8.1,' keV'/)

C Finding weighting for spectra with Q-value q1 and q2 in order to interpolate to Q-value
C Assume same weigthing to be used for different M1 and M2 or E1 and E2
      Iq1=1
      DO I=1,IQ
        IF(Qvalue.GT.QVA(I))Iq1=I
      ENDDO
      Iq2=Iq1+1
      IF(Iq1.EQ.IQ) THEN
        Iq1=IQ-1
        Iq2=IQ
      ENDIF
      wQ1 = (QVA(Iq2)-Qvalue)/(QVA(Iq2)-QVA(Iq1))          ! the weight FE of Q1 spectrum
      wQ2 = 1. - wQ1                                       ! the weight FE of Q2 spectrum

C Main loop starts. J corresponds to true excitation energy Ex
C We are now making the response matrix...
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC                      CCC
CCC                      CCC
CCC   BIG LOOP STARTS    CCC
CCC  Fasten seat belts   CCC
CCC                      CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCC

c         write(6,*) 'iEx0,  J,        ex, ifep1, ifep2, j+iex,          we1,we2'


      DO J=0,RDIM-1

        IT=(J/50)*50
        IF(IT.EQ.J)THEN
          write(6,FMT='(A1,$)')'.'
          call flush(6)
        ENDIF

        DO I=0,2047  ! Zeroing the response function for a sharp Ex (=J), F is re-used for every J
          F(I)=0.
        ENDDO
        Ex=a0 + a1 * FLOAT(J)
        IF(Ex.LT.ExMin.OR.Ex.GT.ExMax)GOTO 999

C Finding the response functions (Ie1,Ie2) and (Im1,Im2) to use for the interpolation
C If you have e.g. Ex = 2.7 MeV and multiplicity M(Ex = 2.7 MeV) = 1.9, you need four spectra:
C (Ie1,Ie2) and (Im1,Im2) = (2,3) and (1,2)
        Ie1=1
        DO I=1,IR
          IF(Ex.GT.ER(I))Ie1=I
        ENDDO
        Ie2=Ie1+1
        IF(Ie1.EQ.IR) THEN
          Ie1=IR-1
          Ie2=IR
        ENDIF

C Testing that we do not read response spectra with Ex > Q-value
        IF(IQ.GT.1) THEN
          Ie1=1
          DO I=1,IR
            IF(Ex.GT.ER(I))Ie1=I
            IF(QVA(Iq2).GT.ER(I))Ie2max=I+1
          ENDDO
          Ie2=Ie1+1
          IF(Ie1.EQ.IR) THEN
            Ie1=IR-1
            Ie2=IR
          ENDIF
          IF(Ie2.GT.Ie2max) THEN
            Ie2=Ie2max
            Ie1=Ie2-1
          ENDIF
        ENDIF

C This is a funny situation:
C If we want to use Ie1 and Ie2 for the lowest iq1, it may be that ER(Ie2)>QAV(Iq1)
C Then drop one unit on Ie1 and Ie2
C The Ie1L and Ie2L should be used for low Iq1
        IF(IQ.GT.1) THEN !Interpolations between Qvalues, have to test if Ex > Qvalue
          Ie1L = Ie1
          Ie2L = Ie2
          IF(ER(Ie2).GT.QVA(Iq1)) THEN
            Ie2L=Ie2-1
            Ie1L=Ie2-1
          ENDIF
        ENDIF
C         write(6,*)Iq1,Iq2,Ie1,Ie2,Ie2L,QVA(Iq1),QVA(Iq2),ER(Ie2),ER(Ie2L)

        ifep1 = INT(((ER(Ie1)-a0)/a1)+0.5)                                 ! the ch for Ex1
        ifep2 = INT(((ER(Ie2)-a0)/a1)+0.5)                                 ! the ch for Ex2
        wE1    =  (FLOAT(ifep2)-FLOAT(J))/(FLOAT(ifep2)-FLOAT(ifep1)) ! the weight for F1 spectrum
        wE2    =  1. - WE1                                                 ! the weight for F2 spectrum

C Finding the multiplicity (mx) at this excitation energy (ex) based on interpolation between IR points
        mx = Mex(Ie1)+(Mex(Ie2)-Mex(Ie1))*(Ex-ER(Ie1))/(ER(Ie2)-ER(Ie1))
        mxv(J) = mx
        Im1=1
        DO I=1,IM
          IF(mx.GT.MUE(I))Im1=I
        ENDDO
        Im2=Im1+1
        IF(Im1.EQ.IM) THEN
          Im1=IM-1
          Im2=IM
        ENDIF
        wM1 = (MUE(Im2)-mx)/(MUE(Im2)-MUE(Im1))                       ! the weight for M1 spectrum
        wM2 = 1. - wM1                                                ! the weight for M2 spectrum


C Now we have weighting factors for wQ, wM and wE.
C Will read in spectra and work through the weightening for 8 spectra (OMG):
C E1M1Q1, E1M1Q2, E1M2Q1, E1M2Q2,E2M1Q1, E2M1Q2, E2M2Q1, E2M2Q2
C Observed November 5 2019, that the responsefunctions for SuN have a large
C number of counts in ch 0. Now removed by statement Spec(0) = 0.

        IF(IRSP.EQ.1)THEN
          INP=20
C Reading the two multiplicity spectra for low Ex
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2016/b"//MNA(Im1)//"_"//ENA(Ie1),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          dim=-1                      !Gives no header output
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe1m1(i)=Spec(i)
          ENDDO
          CLOSE(INP)
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2016/b"//MNA(Im2)//"_"//ENA(Ie1),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          dim=-1                      !Gives no header output
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe1m2(i)=Spec(i)
          ENDDO
          CLOSE(INP)
C Making average spectrum for low Ex
          DO i=0,4095
            Flow(i)=wM1*Fe1m1(i)+wM2*Fe1m2(i)
          ENDDO

C Reading the two multiplicity spectra for high Ex
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2016/b"//MNA(Im1)//"_"//ENA(Ie2),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          dim=-1                      !Gives no header output
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe2m1(i)=Spec(i)
          ENDDO
          CLOSE(INP)
          IF(IRSP.EQ.1)call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2016/b"//MNA(Im2)//"_"//ENA(Ie2),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          dim=-1                      !Gives no header output
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe2m2(i)=Spec(i)
          ENDDO
          CLOSE(INP)
C Making average spectrum for high Ex
          DO i=0,4095
            Fhigh(i)=wM1*Fe2m1(i)+wM2*Fe2m2(i)
          ENDDO
        ENDIF


C////////////////////////////////////


        IF(IRSP.EQ.2)THEN
          INP=20

C We deal first with the low Ex
C Reading two Q-value spectra for low M and low Ex
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq1)//"_"//MNA(Im1)//"_"//ENA(Ie1L),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe1m1q1(i)=Spec(i)
          ENDDO
          CLOSE(INP)
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq2)//"_"//MNA(Im1)//"_"//ENA(Ie1),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe1m1q2(i)=Spec(i)
          ENDDO
          CLOSE(INP)
C Making average spectrum for low M and low Ex
          DO i=0,4095
             Fe1m1(i)=wQ1*Fe1m1q1(i)+wQ2*Fe1m1q2(i)
          ENDDO

C Reading two Q-value spectra for high M and low Ex
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq1)//"_"//MNA(Im1)//"_"//ENA(Ie1L),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe1m2q1(i)=Spec(i)
          ENDDO
          CLOSE(INP)
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq2)//"_"//MNA(Im1)//"_"//ENA(Ie1),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe1m2q2(i)=Spec(i)
          ENDDO
          CLOSE(INP)
C Making average spectrum for high M and low Ex
          DO i=0,4095
            Fe1m2(i)=wQ1*Fe1m2q1(i)+wQ2*Fe1m2q2(i)
          ENDDO
C Then making average spectrum for low Ex
          DO i=0,4095
            Flow(i)=wM1*Fe1m1(i)+wM2*Fe1m2(i)
          ENDDO

C Reading two Q-value spectra for low M and high Ex
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq1)//"_"//MNA(Im1)//"_"//ENA(Ie2L),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe2m1q1(i)=Spec(i)
          ENDDO
          CLOSE(INP)

          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq2)//"_"//MNA(Im1)//"_"//ENA(Ie2),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe2m1q2(i)=Spec(i)
          ENDDO
          CLOSE(INP)
C Making average spectrum for low M and low Ex
          DO i=0,4095
            Fe2m1(i)=wQ1*Fe2m1q1(i)+wQ2*Fe2m1q2(i)
          ENDDO

C Reading two Q-value spectra for high M and high Ex
          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq1)//"_"//MNA(Im2)//"_"//ENA(Ie2L),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe2m2q1(i)=Spec(i)
          ENDDO
          CLOSE(INP)

          call makepath("UIO_APPLICATIONS","mama/resp/sun_ex_2018/b"//QNA(iq2)//"_"//MNA(Im2)//"_"//ENA(Ie2),filnam)
          OPEN(INP,FILE=FILNAM,ACCESS='SEQUENTIAL',ACTION='READ',ERR=9999)
          DO i=0,4095
            Spec(i) = 0.
          ENDDO
          dim=-1                      !Gives no header output
          CALL norr1dim(INP,comment,dim,Spec,Calib)
          Spec(0) = 0.
          DO i=0,4095
            Fe2m2q2(i)=Spec(i)
          ENDDO
          CLOSE(INP)
C Making average spectrum for high M and low Ex
          DO i=0,4095
            Fe2m2(i)=wQ1*Fe2m2q1(i)+wQ2*Fe2m2q2(i)
          ENDDO
C Making average spectrum for high Ex
          DO i=0,4095
            Fhigh(i)=wM1*Fe2m1(i)+wM2*Fe2m2(i)
          ENDDO
        ENDIF

C Stretching/compressing Flow and Fhigh to fit calibration a0 and a1
        idim1=4096
        idim2=4096
        do i=0,4095
          Fs(i)=Flow(i)
        enddo
        CALL ELASTIC(Fs,F1,b0,b1,a0,a1,idim1,idim2) !F1 is new spectrum
        do i=0,4095
          Fs(i)=Fhigh(i)
        enddo
        CALL ELASTIC(Fs,F2,b0,b1,a0,a1,idim1,idim2) !F2 is new spectrum

C Total spectrum normalized to one
        SumF1 = 0.
        SumF2 = 0.
        DO I = 0,4095                              ! Background counts added
            SumF1 = SumF1 + F1(I)
            SumF2 = SumF2 + F2(I)
        ENDDO
        H1 = wM1*FE(Ie1,Im1) + wM2*FE(Ie1,Im2)
        H2 = wM1*FE(Ie2,Im1) + wM2*FE(Ie2,Im2)
        SumF1 = SumF1 + H1                         ! Full energy counts added
        SumF2 = SumF2 + H2
        DO I = 0,4095
            F1(I) = F1(I)/SumF1                    ! Normalizing energy bacground spectra
            F2(I) = F2(I)/SumF2
        ENDDO
        H1  = H1 /SumF1                            ! Normalizing full energy counts
        H2  = H2 /SumF2

C Starting to interpolate between spectra F1 and F2. This concerns the
C incomplete adding of events and the beta energy. The main rule
C is that we interpolate between points in the spectra below
C and above the full energy Ex peak. We assume the response function
C is mainly from ExMin = 30 keV to ExMax = Qvalue (and eventually higher)

C Zeroing
        DO I = 0, 2047
          F(I) = 0.
        ENDDO

C Interpolating from ch 0 up to ch for full-energy Ex
        b = a0/a1
        DO I = 0, J
           I1 = 2047
           I2 = 2047
           IF(b+FLOAT(J).NE.0.)THEN
            I1   = INT(-b + (b+FLOAT(I))*((b+FLOAT(ifep1))/(b+FLOAT(J)))+0.5)
            I2   = INT(-b + (b+FLOAT(I))*((b+FLOAT(ifep2))/(b+FLOAT(J)))+0.5)
            F(I) = wE1*F1(I1) + wE2*F2(I2)
          ENDIF
        ENDDO

C Interpolating from ch for full-energy Ex up to ExMax
        imax  = INT(((ExMax - a0)/a1)+0.5)
        DO I = J+1,imax
           I1 = 2047
           I2 = 2047
           IF(FLOAT(imax)-FLOAT(J).NE.0.)THEN
             I1    = INT(FLOAT(ifep1) + (FLOAT(I)-FLOAT(J))*(FLOAT(imax)-FLOAT(ifep1))/(FLOAT(imax)-FLOAT(J)) + 0.5)
             I2    = INT(FLOAT(ifep2) + (FLOAT(I)-FLOAT(J))*(FLOAT(imax)-FLOAT(ifep2))/(FLOAT(imax)-FLOAT(J)) + 0.5)
             F(I)  = wE1*F1(I1) + wE2*F2(I2)
           ENDIF
        ENDDO

C Interpolating to get the intensity of the full-energy peak in ch J
        H = wE1*H1 + wE2*H2  !full-energy intensity
        pf(J) = H

C Calculating the integral of F (full-energy peak not included)
        SUMF=0.
        DO I = 0,2047
          SUMF=SUMF+F(I)
        ENDDO
        pc(J) = SUMF

C Relative intensity of full-energy and background
        total = pf(J) + pc(J)
        if(total.LE.0)total = 1000000000.
        pf(J) = pf(J)/total
        pc(J) = pc(J)/total

C Folding background with a Gauss distribution, where
C factor=1 means (1/facFWHM)*FWHMexp and factor=facFWHM means FWHMexp
C The width contribution from channel-bin is w0=1
C Array F is smoothed -> G
        factor=1./facFWHM
        w0=1.
        CALL GaussSmoothingy(F,G,MinEx,MaxEx,factor,w0) !smoothing

C Adding the counts of the full-energy peak to the spectrum.
C The total counts (yl+yh) have to be distributed in general on
C the two channels il and ih.
        DO i=0,2047
          Ffu(i)=0.
          Gfu(i)=0.
        ENDDO

        Exi=(Ex-a0)/a1       ! real value for channel
        il=Exi               ! Distributing counts on two neighbouring channels: il and ih
        ih=il+1
        yl=(ih-Exi) * H
        yh=(Exi-il) * H
        IF(il.GE.0.AND.il.LE.MaxEx) Ffu(il)=Ffu(il)+yl
        IF(ih.GE.0.AND.ih.LE.MaxEx) Ffu(ih)=Ffu(ih)+yh
        ymax=AMAX1(yl,yh)
        ymin=AMIN1(yl,yh)
        IF(ymax.GT.0)THEN
          w0=(ymin+ymax)/ymax
        ELSE
          w0=1.
        ENDIF

        factor=1./facFWHM
        lim=MAX0(INT((6.*(Fwhm1(il)*FWHM/100.))*FLOAT(il)+0.5) , 1)
        m1=MAX0(il-lim,MinEx)
        m2=MIN0(ih+lim,MaxEx)
        CALL GaussSmoothingy(Ffu,Gfu,m1,m2,factor,w0) !smoothing full energy peak

C We renormalize so that matrix R(i,j) have 1*RDIM counts
        SumR=0.
        DO I=0,RDIM-1
          R(I,J)=G(I)+Gfu(I)
          IF(R(I,J).LT.0.)R(I,J)=0
          IF(I.EQ.0)R(I,J)=0
          SumR=SumR+R(I,J)
        ENDDO
        DO I=0,RDIM-1
          IF(SumR.GT.0)R(I,J)=R(I,J)/SumR
        ENDDO
 999    CONTINUE
      ENDDO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC                        CCC
CCC                        CCC
CCC     BIG LOOP ENDED     CCC
CCC   Release seat belts   CCC
CCC                        CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Writing out to file: respyout.dat
  20  OPEN(UNIT=25,FILE='respyout.dat', ERR=9998)
      ISM=1
      IF(ITYPE.EQ.1)ISM=2
      CALL DATETIME(DATTIM)
      WRITE(25,199)FNAME(ISM,IDEST),DATTIM(1:18)
      WRITE(25,210)a0,a1,FWHM
199   FORMAT('Response function created for file: ',A,' at ',A)
210   FORMAT('Calibration (a0,a1)=',F8.1,F9.3,'  FWHM (rel. at 1.33 MeV)=',F8.3,/)

      istep = INT((200./a1)+0.5)    !steps for output
      if(istep.LE.0)istep=1
      WRITE(25,*)'Ex(keV)  FWHM(keV)  EffTot  Full-energy  Incomplete+beta Multiplicity '
      Ex = 0.
      DO i=0,RDIM-1
        Ex=a0+i*a1
        IF(Ex.GE.0.AND.Ex.LT.ExMax)THEN
          IF((i/iStep)*iStep.EQ.i)THEN
            fwh=0.
            IF(Ex.GT.0)fwh=Fwhm1(i)*(facFWHM*FWHMunf/100.)*Ex
            IF(Ex.GT.0)fwh=Fwhm1(i)*(FWHM/100.)*Ex
            IF(fwh.LT.ABS(a1)/5.)fwh=ABS(a1)/5.
            WRITE(25,14)Ex, fwh, EffTot(i),pf(i), pc(i), mxv(i)
 14         FORMAT(     F8.1,F10.1,     F8.3, F12.4,  F14.4,  F14.2)
          ENDIF
        ENDIF
      ENDDO
      write(6,*)
      write(6,*)'Parameters for response function written to respyout.dat'
      write(6,*)
      GOTO 9998
 9999 WRITE(6,*)'Cannot find the response function: ',FILNAM
 9998 CONTINUE
      CLOSE(25)
      END
    

      SUBROUTINE SCORINGy(SoluP,Lsave,Lmax,SCORE,Isc,wfluc)
c Routine to calculate the score of each solution. The score-function,
c which should be as small as possible, is simply a weighted sum of
c chisquare and fluctuations

      DIMENSION SoluP(500,7),SC(500)
      CHARACTER SCORE(500)*4

      Isc=0
      DO I=1,500
        SCORE(I)='    '
        SC(I)=0
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


      SUBROUTINE UnFoldy
      INTEGER XDIM,YDIM,RDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,xcomm*60
      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM
      COMMON/responsey6/LOW, HIGH, Iter, ix1,iy1,ix2,iy2
      INTEGER LOW,HIGH
    
      IF(RDIM.EQ.0)THEN
        WRITE(6,*)'No response function present'
        WRITE(6,*)'Use the RY command to create one, or take a cup of coffee'
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


c      For the y-axis we use only the old method, there are no smooth compton along the y-axis
c      Also, the new method has not been tester yet, and may have coding errors etc

      iVersion = 1

      IF(ITYPE.GT.1)THEN
        XDIM=Idim(1,ISP,1)
        YDIM=Idim(1,ISP,2)
        IF(YDIM.GT.RDIM)YDIM=RDIM
        WRITE(6,3)XDIM
 3      FORMAT(/'Dimension along x-axis <',I4,'>:',$)
        CALL READI(5,XDIM)
        WRITE(6,4)YDIM
 4      FORMAT( 'Dimension along y-axis <',I4,'>:',$)
        CALL READI(5,YDIM)
        IF(Istatus.NE.0)RETURN

C Defining upper border for the unfolding and chisq-test
c        Ix1=XDIM-1
c        Ix2=XDIM-1
c        Iy1=0
c        Iy2=YDIM-1

        CALL UnFoldity(iVersion)

        WRITE(6,5)a0,a1
 5      FORMAT('Calibration is a0=',F6.1,'keV, a1=',F7.2,'keV/ch')
        cal(1,IDEST,2,1)=a0
        cal(1,IDEST,2,2)=a1
        cal(1,IDEST,2,3)=0
        cal(1,IDEST,1,1)=cal(1,ISP,1,1)
        cal(1,IDEST,1,2)=cal(1,ISP,1,2)
        cal(1,IDEST,1,3)=cal(1,ISP,1,3)

        IF(a0+a1.NE.cal(1,ISP,2,1)+cal(1,ISP,2,2))THEN
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

        CALL UnFoldity(iVersion)

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
        xcomm(1:3)='UY:'
        fname(1,IDEST)(1:8)='UY'//fname(1,ISP)(1:6)
        comm(1,IDEST)=comm(1,ISP)
        CALL AddComment(xcomm,3)
      ELSE
        xcomm(1:3)='UY:'
        fname(2,IDEST)(1:8)='UY'//fname(2,ISP)(1:6)
        comm(2,IDEST)=comm(2,ISP)
        CALL AddComment(xcomm,3)
      ENDIF  
      END


      SUBROUTINE UnFoldity(iVersion)
C A program for unfolding detector response effects for continoues
C spectra. Modified version (Aug. 1995), which smoothes the
C Compton contribution. Modified Jan 2017 for the beta-Oslo method
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
C        Iter   = Number of iteration steps

      INTEGER XDIM,YDIM,RDIM
      CHARACTER ENA*5, MNA*2, QNA*2, APP*4,waitname*8
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH

      COMMON/responsey1/R(0:2047,0:2047),RDIM,a0,a1,FWHM,facFWHM
      COMMON/responsey2/IR,IM,IQ,Qvalue,ETAB(18),FTAB(18),EW(18),FW(18),
     +Mex(18),ER(18),MUE(18),QVA(18),ENA(18),MNA(18),QNA(18)
      COMMON/responsey3/EffTot(0:2047),Fwhm1(0:2047),EffExp(0:2047)
      COMMON/responsey4/pf(0:2047),pc(0:2047)
      COMMON/responsey6/LOW, HIGH, Iter, ix1,iy1,ix2,iy2
      REAL mx, Mex, MUE, QVA, Qvalue

      INTEGER LOW,HIGH,ChiLow,ChiHigh,lower
      INTEGER sum,Fsum,sumCH,FsumCH
      DIMENSION Raw(0:2047),F(0:2047),U(0:2047),lower(0:2047)
      DIMENSION Fsave(0:2047),Usave(0:2047)
      DIMENSION SoluF(500,0:2047),SoluP(500,7)
      DIMENSION rWAIT(0:8191)
      CHARACTER SCORE(500)*4,ANS*1
      CHARACTER modus*2,moil*2,moim*2,moir*2
      INTEGER COLORMAP(20),Colorc(0:19)
      COMMON /COLORMAP/ COLORMAP,Limit,Colorc
      REAL Limit(0:19)
      LOGICAL DISP
      INTEGER            IYAXIS,LDX,HDX,LDY,HDY,LOCH,HICH
      COMMON/DISPLA/DISP,IYAXIS,LDX,HDX,LDY,HDY,LDZ,HDZ,LOCH,HICH,LOCNT,HICNT
      REAL LDZ,HDZ,LOCNT,HICNT
      DIMENSION u0(0:2047),su0(0:2047),v(0:2047), c(0:2047),sc(0:2047)

      ISP=1
      IF(IDEST.EQ.1)ISP=2

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
        LEN=YDIM
        IF(RDIM.LT.LEN) LEN=RDIM
        Ixdim=XDIM
        DO I=0,4095
          DO J=0,2047
            rMAT(IDEST,I,J)=0
          ENDDO
        ENDDO
      ELSE                              !singles
        LEN=MAXCH+1
        IF(RDIM.LT.LEN)LEN=RDIM
        Ixdim=1
        DO I=0,8191
          rSPEC(IDEST,I)=0
        ENDDO
      ENDIF

C Lowest channel treated
      Emin   = -300.                    ! 30 keV lowest limit
      EminCh =    0.                    ! Ex = 0 keV is lower limit for Chi2-test
      LOW=((Emin-a0)/a1)+0.5
      IF(LOW.LT.0.OR.LOW.GE.LEN)LOW=0
      lowCh=((EminCh-a0)/a1)+0.5
      IF(lowCh.LT.0.OR.lowCh.GE.LEN)lowCh=0

C Upper channel limits for the unfolding and chisq-test
      HIGH    = LEN-1
      ChiHigh = LEN-1

      OPEN(23,FILE='input.uny',STATUS='old',ERR=77)
      READ(23,*,END=66,ERR=66)IRSP,Qvalue,facFWHM,FWHM
      READ(23,*,END=66,ERR=66)ix1,iy1,ix2,iy2
      READ(23,*,END=66,ERR=66)
      READ(23,*,END=66,ERR=66)LOW,HIGH
      READ(23,*,END=66,ERR=66)Iter,ChiHigh
      GO TO 77
 66   WRITE(6,*)'Warning: Something wrong with your input.uny file'
 77   CLOSE(23)

C ****Begin, Fix for first run****
      IF(HIGH.GT.LEN-1)HIGH=LEN-1
      IF(Iy2.GT.HIGH)Iy2=HIGH
      IF(HIGH.LE.0)THEN
        LOW  = 0
        HIGH = LEN-1
      ENDIF
      IF(Ix1+Ix2+Iy1+Iy2.LT.10)THEN
        Ix1=XDIM-1
        Ix2=XDIM-1
        Iy1=0
        Iy2=HIGH
      ENDIF
      IF(Iter.EQ.0)THEN
        Iter=50
      ENDIF
      IF(ChiHigh.LE.0)THEN
        ChiHigh = LEN-1
      ENDIF
C ****End, Fix for first run****

      WRITE(6,121)ChiHigh
 121  FORMAT(/'Give upper y-channel for Chi**2 testing  <',I5,'>:',$)
      CALL READI(5,ChiHigh)
      IF(Istatus.NE.0)RETURN

      IF(ITYPE.GT.1)THEN                !matrix
        WRITE(6,*)'Give lower y-channel limits for the unfolding along y-axis'
        WRITE(6,*)'It is given by interpolation between (x1,y1) and (x2,y2)'
        WRITE(6,*)' '
        WRITE(6,*)'            (x2,y2)  second point'
        WRITE(6,*)'xxxxxxxxxxxxxxx'
        WRITE(6,*)'xx matrix xxx'
        WRITE(6,*)'xxxxxxxxxxx'
        WRITE(6,*)'xxxxxxxxx'
        WRITE(6,*)'xxxxxxx'
        WRITE(6,*)'    (x1,y1)  first point'

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
        WRITE(6,13)LOW
 13     FORMAT(/'Give lower channel for unfolding    <',I5,'>:',$)
        CALL READI(5,LOW)
        WRITE(6,12)HIGH
 12     FORMAT(/'Give upper channel for unfolding    <',I5,'>:',$)
        CALL READI(5,HIGH)
      ENDIF
      IF(Istatus.NE.0)RETURN

       CF=0.
       Dx12=Ix2-Ix1
       Dy12=Iy2-Iy1
       IF(Ix2.NE.Ix1)CF=Dy12/Dx12

       DO I=0,IXDIM-1
         lower(I)=INT(Iy1+CF*(FLOAT(I-Ix1))+0.5)
         IF(CF.EQ.0.)lower(I)=LOW
c         IF(lower(I).LT.LOW )lower(I)=LOW
         IF(lower(I).LT.0 )lower(I)=0
         IF(lower(I).GT.HIGH)lower(I)=HIGH

       ENDDO
      ANS='y'
      WRITE(6,133)ANS    
 133  FORMAT(/,'Include total detector efficiency (y/n) <',A1,'>:',$)
      CALL READA1(5,ANS)
      IF(Istatus.NE.0)RETURN
c      IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
c        CALL ExpThres
c      ENDIF

      
      WRITE(6,134)Iter
 134  FORMAT(/,'Number of iterations ( <500 )  <',I3,'>:',$)
      CALL READI(5,Iter)
      IF(Iter.GT.500)Iter=500
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

C Loop for spectrum I to be unfolded***************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC                      CCC
CCC                      CCC
CCC   BIG LOOP STARTS    CCC
CCC  Fasten seat belts   CCC
CCC                      CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO I=0,Ixdim-1
        ChiLow= lower(I)
C Getting the raw spectrum Raw(I)
        DO J=0,2047
          IF(ITYPE.GT.1)Raw(J)=rMAT(ISP,I,J)
          IF(ITYPE.EQ.1)Raw(J)=rSPEC(ISP,J)
        ENDDO
        sum=   0
        sumCH= 0
c        DO J=ChiLow,HIGH
        DO J=0,HIGH
          sum  =sum + Raw(J)
        ENDDO
        DO J=ChiLow,ChiHigh
          sumCH=sumCH+Raw(J)
        ENDDO

C Initialize parameters
        DO L=1,Iter
          DO JJ=0,LEN-1
            SoluF(L,JJ)=0
          ENDDO
          DO JJ=1,7
            SoluP(L,JJ)=0
          ENDDO
        ENDDO

        DO J=0,LEN-1
          U(J)    =0.0
          F(J)    =0.0
          Fsave(J)=0.0
          Usave(J)=0.0
        ENDDO

        ModeChange=0
        CHIsave=1000000000
        CHIold =1000000000
                              
        FlucRaw=Flucy(Raw,ChiLow,ChiHigh,a1)
       
C The experimental pulse height spectrum is the first
C approximation to the unfolded gamma spectrum U(I)
c        DO J=ChiLow,HIGH
        DO J=0,HIGH
          U(J)=Raw(J)
        ENDDO

C Iteration loop for spectrum I. Folding the unfolded spectrum
C We have to start the integration somewhat below ch J, due to the
C detector resolution. We take conservative 20% resolution
        mode=0                      !starts with no method
        iTry=0
        DO L=1,Iter
          DO J=0,HIGH
            F(J)=0.0
            DO K=LOW,HIGH
c            DO K=0,HIGH
              F(J)=F(J)+R(J,K)*U(K)
            ENDDO
          ENDDO

C Calculate chisquare between folded and raw spectrum
          CHISQ=0.
          Ichi=0
          DO J=ChiLow,ChiHigh
            sig2=Raw(J)
            IF(sig2.LT.4.)sig2=4.
            CHISQ=CHISQ+((F(J)-Raw(J))**2)/sig2
            Ichi=Ichi+1
          ENDDO
          IF(Ichi.LT.1)Ichi=1
          IF(Ichi.GT.1)Ichi=Ichi-1
          CHISQ=CHISQ/Ichi

C Compute sum of counts
          Fsum  =0.
          FsumCH=0.
          DO J=ChiLow,HIGH
            Fsum=Fsum+F(J)
          ENDDO
          DO J=ChiLow,ChiHigh
            FsumCH=FsumCH+F(J)
          ENDDO
 
          RelFluc=Flucy(U,ChiLow,ChiHigh,a1)/FlucRaw
          SoluP(L,1)=mode
          SoluP(L,2)=sum
          SoluP(L,3)=Fsum
          SoluP(L,4)=sumCH
          SoluP(L,5)=FsumCH
          SoluP(L,6)=CHISQ
          SoluP(L,7)=RelFluc
          DO JJ=LOW,HIGH
c          DO JJ=0,HIGH
            SoluF(L,JJ)=U(JJ)
          ENDDO
          Lmax=L

C Saves the best solution after at least 3 iterations
          IF(L.GT.3.AND.CHISQ.LT.CHIsave)THEN
            Lsave=L
            CHIsave=CHISQ
            DO J=LOW,HIGH
c            DO J=0,HIGH
              Usave(J)=U(J)
              Fsave(J)=F(J)
            ENDDO
          ENDIF

          IF(L.GT.3.AND.ABS( CHISQ-CHIold).LT.0.0003)       IterStop=1
          IF(L.GT.3.AND.ABS((CHISQ-CHIold)/CHISQ).LT.0.001) IterStop=1
          IF(iTry.GT.10.AND.CHISQ.GT.CHIold)                IterStop=1
          IF(iTry.GT.Iter/2.0 .AND.ModeChange.EQ.1)         IterStop=1
          iTry=iTry+1
          IF(IterStop.EQ.1)THEN
            IF(ModeChange.LT.10)THEN                   !Changing mode
              IterStop=0    !flags if going to end for one or another mode
              iTry    =0
              mode=mode*(-1)
              ModeChange=ModeChange+1
              DO J=LOW,HIGH                            !Using the best solution
c              DO J=0,HIGH                            !Using the best solution
                F(J)=Fsave(J)                          !as initial function
                U(J)=Usave(J)
              ENDDO
            ELSE
              GO TO 100                                !End iteration          
            ENDIF
          ENDIF

C mode=-1 selects difference, mode=+1 ratio iteration
          IF(L.EQ.1)mode=-1                            !used for loop number 2
          IF(mode.EQ.-1) THEN
            DO J=LOW,HIGH
c            DO J=0,HIGH
              U(J)=U(J)+(Raw(J)-F(J))                  !difference mode
            ENDDO
          ELSE
            DO J=LOW,HIGH
c            DO J=0,HIGH
              IF(ABS(F(J)).GT.4)U(J)=U(J)*(Raw(J)/F(J))!ratio mode
            ENDDO
          ENDIF
          CHIold=CHISQ
        ENDDO
C Iteration loop for spectrum I ended


  100   CONTINUE

C Finding the best solution: It will be loop number Isc
        CALL SCORINGy(SoluP,Lsave,Lmax,SCORE,Isc,wfluc)

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
  
        WRITE(6,30)I,modus,SoluP(Isc,5),SoluP(Isc,4),SoluP(Isc,6),SoluP(Isc,7),Isc
 30     FORMAT('x-ch:',I4,' Mode:',A2,' Area:',E10.3,'(',E10.3,') Chi:',F7.3,' Fluct:',F6.2,' Iter:',I3)

        IF(iVersion.EQ.1)THEN   !Dropping the Compton subtraction method
          DO J=0,HIGH
            u(J)=0.0
          ENDDO
          DO J=LOW,HIGH
c          DO J=0,HIGH
            SoluF(Isc,J)=SoluF(Isc,J)   !April 2013 *pf(i)
            u(J)=SoluF(Isc,J)
          ENDDO
          GO TO 999  
        ENDIF


C***************************************************************(new begin)
C       New method: Compton Subtraction Method (Desember 1995/mg)
C       Reference: M. Guttormsen et al. NIM (1996), in press
C       The resolution in the unfolded spectrum u0 is about 0.87FWHM. 
C       Thus, it remains to smooth it with (1/10)*FWHM.
C       Then we deduce S - U (rawspectrum - unfolded) to get the 
C       background etc., that is a smooth function of
C       energy.
C       Therefore, we smooth
C       this differanse with FWHM, and calculate Ufinal=S-smooth(S-U). 
C       Remember, the FWHM parameter contains only 50% of
C       the value given by user
        DO J=0,LEN-1
          u0(J)   =0.0
          su0(J)  =0.0
          u(J)    =0.0
          v(J)    =0.0
          c(J)    =0.0
          sc(J)   =0.0
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
        DO J=LOW,HIGH
          u0(J)=SoluF(Isc,J)
        ENDDO

C Show spectrum u0
        DO J=LOW,HIGH
          rSPEC(IDEST,J)=u0(J)
        ENDDO
        i1=1
        i2=2
        i3=0
        CALL SetMarker(0,2,0)
        COLORMAP(1)=iblue
        CALL DSPSP(i1,i2,i3,*333)
  333   CONTINUE


C Smoothing the u0 spectrum
        factor=1./facFWHM
        w0=1.
        CALL GaussSmoothingy(u0,su0,LOW,HIGH,factor,w0)
C Multiplying down with pf
        DO J=LOW,HIGH
          su0(J)=su0(J)*pf(J)        !April 2013 *pf(j)
        ENDDO
C Making the v spectrum
        DO J=LOW,HIGH
          v(J)=su0(J)
        ENDDO
C Making the background c spectrum
        DO J=LOW,HIGH
          c(J)=Raw(J)-v(J)              !c is background
        ENDDO
C Displaying background spectrum c
        i1=1
        i2=2
        i3=0
        DO J=LOW,HIGH
          rSPEC(IDEST,J)=c(J)
        ENDDO
        COLORMAP(1)=ired
        CALL SetMarker(0,2,0)
        CALL DSPSP(i1,i2,i3,*666)
  666   CONTINUE

C Smoothing background with 50% of FWHM
c        factor=facFWHM/2.
        factor=1./2.
        w0=1.
        CALL GaussSmoothingy(c,sc,LOW,HIGH,factor,w0)   !sc is smoothed background
C Displaying original raw spectrum 
        i1=2
        i2=2
        i3=0
        DO J=LOW,HIGH
          rSPEC(IDEST,J)=Raw(J)
        ENDDO
        COLORMAP(1)=ired
        CALL SetMarker(0,2,0)
        CALL DSPSP(i1,i2,i3,*777)
  777   CONTINUE

C Displaying smoothed Compton sc
        i1=2
        i2=2
        i3=0
        DO J=LOW,HIGH
          rSPEC(IDEST,J)=sc(J)
        ENDDO
        COLORMAP(1)=igreen
        CALL DSPSP(i1,i2,i3,*888)
 888    CONTINUE

        DO J=LOW,HIGH
          u(J)=Raw(J)-sc(J)         !u is the unfolded spectrum
        ENDDO

C Using the full-energy peak probability pf(i) to make u(i) contain all counts (intensity = 1)
        DO J=LOW,HIGH
          IF(pf(J).GT.0.)THEN
            u(J)=u(J)/pf(J)                 !April 2013 /pf(i)
          ELSE
            u(J)=0.
          ENDIF
        ENDDO

C Displaying final u/pf
        DO J=LOW,HIGH
          rSPEC(IDEST,J)=u(J)
        ENDDO
        COLORMAP(1)=iblue
        CALL SetMarker(0,2,0)
        CALL DSPSP(i1,i2,i3,*999)

C Putting back what was in rSPEC(IDEST,J)
        DO J=0,MAXCH
          rSPEC(IDEST,J)=rWAIT(J)
        ENDDO
        ITYPE=ITYPEwait
C Putting back color
        COLORMAP(1)=icmap1

 999    CONTINUE
C********************************************************(new ended)

C Correcting for detector efficiency as function of energy
        negstat=-1
        DO J=0,HIGH
          rMAT(IDEST,I,J)=0
          rSPEC(IDEST,I)=0
          IF(u(J).GT.0)negstat=+1
c          IF(J.lt.low)u(J)=0
          IF(negstat.EQ.-1)u(J)=0 !remove neg. counts in the first channels
          IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
            effi=EffTot(J)   !*EffExp(J)
            IF(ITYPE.GT.1.AND.effi.GT.0.)rMAT(IDEST,I,J)=u(J)/effi
            IF(ITYPE.EQ.1.AND.effi.GT.0.)rSPEC(IDEST,J)=u(J)/effi
          ELSE
            IF(ITYPE.GT.1)rMAT(IDEST,I,J)=u(J)
            IF(ITYPE.EQ.1)rSPEC(IDEST,J)=u(J)
          ENDIF
        ENDDO

      ENDDO                                !I-loop for all spectra ended

C Writes parameters to disk, to be used for next run
      OPEN(23,FILE='input.uny',ACCESS='SEQUENTIAL',ERR=88)
      WRITE(23,*)IRSP,Qvalue,facFWHM,FWHM
      WRITE(23,*)ix1,iy1,ix2,iy2
      WRITE(23,*)(Mex(i),i=1,18)
      WRITE(23,*)LOW,HIGH
      WRITE(23,*)Iter,ChiHigh
      CLOSE(23)
  88  CONTINUE


CCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC                      CCC
CCC                      CCC
CCC    BIG LOOP ENDS     CCC
CCC  Release seat belts  CCC
CCC                      CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
      fname(2,IDEST)=waitname              !Putting back old name
      END
