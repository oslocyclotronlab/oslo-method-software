      PROGRAM GBF
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      CHARACTER fname*8,comm*60
      CHARACTER APP*4,outfile*20,comment*60
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)

      COMMON/Fgexp/a0Eg,a1Eg,a0Ex,a1Ex
      COMMON/Fgteo/a,n,Cf,Cn,Cgdr,FWHM,Eres
      EXTERNAL FgExpInt, FgTeoInt
      REAL n
      REAL FexpC(0:511), FexpG(0:511), FteoC(0:511), FteoG(0:511)
      REAL Rexp(0:511), Rteo(0:511), TransM(0:511), valley(0:511)

      WRITE(6,*)'     _____________________________________________'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |         GroundBandFeeding 1.2 (GBF)         |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Program to calculate feeding to ground band |'
      WRITE(6,*)'    |   from first generation spectra and from    |'
      WRITE(6,*)'    |      theoretical Fermi gas predictions      |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    |          Oslo Cyclotron Laboratory          |'
      WRITE(6,*)'    |                                             |'
      WRITE(6,*)'    | Created:  24/03 - 1999                      |'
      WRITE(6,*)'    | Modified: 25/03 - 1999                      |'
      WRITE(6,*)'    | Magne Guttormsen                            |'
      WRITE(6,*)'    |_____________________________________________|'
      WRITE(6,*)' '

C History:
C 20 mars  1999:
  
C Initializing parameter values
      Ex1 = 0.     !Lower excitation energy
      Ex2 = 9000.  !Upper excitation energy
      Egmin=  430. !Lower Egam for 1.gen. spec.
      dE   =  400. !Max integration for ground band: Ex + dE
      a    = 0.018 !Level density parameter (1/keV)
      n    = 4.2   !Formula is Sig=Cn*Eg**n
      Cf   = 1.    !Constants for Fermi (Cf) and gamma-strength (Cn,Cgdr)
      Cn   = 0.20/((1000.)**4.2)
      Cgdr = 350./((1000.)**2.)
      FWHM = 5000.    ! Giant dipole parameters
      Eres =15000.

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
        a0Eg=11.               !Defaults in case of no calibration
        a1Eg=20.
        a0Ex=9660.
        a1Ex=-120.
      ELSE
        a0Eg=cal(1,IDEST,1,1)  !Picks the spectrum calibration
        a1Eg=cal(1,IDEST,1,2)
        a0Ex=cal(1,IDEST,2,1)
        a1Ex=cal(1,IDEST,2,2)
      ENDIF
             
C Giving energy calibrations
 22   CONTINUE
      WRITE(6,23) a0Eg
 23   FORMAT(/'Cal. coeff. a0Eg (keV) on x-axis    <',F8.1,'>:',$)
      CALL READF(5, a0Eg)
      WRITE(6,24) a1Eg
 24   FORMAT( 'Cal. coeff. a1Eg (keV/ch) on x-axis <',F8.3,'>:',$)
      CALL READF(5, a1Eg)
      WRITE(6,26) a0Ex
 26   FORMAT(/'Cal. coeff. a0Ex (keV) on y-axis    <',F8.1,'>:',$)
      CALL READF(5, a0Ex)
      WRITE(6,27) a1Ex
 27   FORMAT( 'Cal. coeff. a1Ex (keV/ch) on y-axis <',F8.2,'>:',$)
      CALL READF(5, a1Ex)

      Exx1=a0Ex+a1Ex*0
      Exx2=a0Ex+a1Ex*(YDIM-1)
      IF(Exx1.GT.Exx2)THEN
        wait=Exx1
        Exx1=Exx2
        Exx2=wait
      ENDIF
      Ex1=max(Ex1,Exx1)
      Ex2=min(Ex2,Exx2)
      WRITE(6,*)' '
      WRITE(6,28) Ex1
 28   FORMAT( 'Lower excitation energy (keV)        <',F7.1,'>:',$)
      CALL READF(5,Ex1)
      WRITE(6,29) Ex2
 29   FORMAT( 'Upper excitation energy (keV)        <',F7.1,'>:',$)
      CALL READF(5,Ex2)
      j1=((Ex1-a0Ex)/a1Ex)+0.5 
      j2=((Ex2-a0Ex)/a1Ex)+0.5
      IF(j1.LT.0.OR.j1.GT.YDIM-1.OR.j2.LT.0.OR.j2.GT.YDIM-1)THEN
        WRITE(6,*)'Ex1 and Ex2 gives channels ch1 and ch 2 outside the matrix'
        GO TO 22
      ENDIF
      jstep = 1
      IF(j1.GT.j2)THEN
        jstep = -1
      ENDIF
      JDIM = max(j1,j2)+10

C Giving integration limits
      WRITE(6,*)' '
      WRITE(6,*)'The integration of first generation spectra has limits:'
      WRITE(6,*)'Fermi gas region:   Egmin  -> valley'
      WRITE(6,*)'Ground band region: valley -> Ex + dE'
      WRITE(6,*)'The dE value is given by your resolution'
      WRITE(6,*)'Please, define Egmin, dE and valley:'
      WRITE(6,*)' '
      WRITE(6,10)Egmin
10    FORMAT( 'Lower limit of gamma energy Eg (keV)  <',F7.1,'>:',$)
      CALL READF(5,Egmin)
      WRITE(6,11)dE
11    FORMAT( 'Give total matrix resolution dE (keV) <',F7.1,'>:',$)
      CALL READF(5,dE)

      WRITE(6,*)' '
      WRITE(6,*)'Define the line in the valley that separates the'
      WRITE(6,*)'population of the ground band and the 1 MeV' 
      WRITE(6,*)'excitation region. Use CR-command to obtain the'
      WRITE(6,*)'channel numbers defining the valley. The line is'
      WRITE(6,*)'given by interpolation between (x1,y1) and (x2,y2)'
      WRITE(6,*)'in the first generation matrix:'
      WRITE(6,*)' '
      WRITE(6,*)'    (x2,y2)  second point'
      WRITE(6,*)'xxxxxxx'
      WRITE(6,*)'xxxxxxxxx'
      WRITE(6,*)'xxxxxxxxxxx'
      WRITE(6,*)'xx matrix xxx'
      WRITE(6,*)'xxxxxxxxxxxxxxx'
      WRITE(6,*)'            (x1,y1)  first point'
      WRITE(6,*)' '

      Ix1=((5300.-a0Eg)/a1Eg)+0.5
      Iy1=((6000.-a0Ex)/a1Ex)+0.5
      Ix2=((2500.-a0Eg)/a1Eg)+0.5
      Iy2=((3000.-a0Ex)/a1Ex)+0.5

      WRITE(6,12)Ix1
 12   FORMAT(/'First point x1  <',I5,'>:',$)
      CALL READI(5,Ix1)
      WRITE(6,13)Iy1
 13   FORMAT( 'First point y1  <',I5,'>:',$)
      CALL READI(5,Iy1)
      WRITE(6,14)Ix2
 14   FORMAT( 'Second point x2 <',I5,'>:',$)
      CALL READI(5,Ix2)
      WRITE(6,15)Iy2
 15   FORMAT( 'Second point y2 <',I5,'>:',$)
      CALL READI(5,Iy2)

      ratio=0.
      Dx12=Ix2-Ix1
      Dy12=Iy2-Iy1
      IF(Iy2.NE.Iy1)ratio=Dx12/Dy12
      DO j=0,YDIM-1
        valley(j)=Ix1-ratio*(FLOAT(Iy1-j))
        IF(valley(j).LT.0   )valley(j)=0
        IF(valley(j).GT.XDIM-1)valley(j)=XDIM-1
      ENDDO

C Giving theoretical Fermi gas parameters
      WRITE(6,30) a
30    FORMAT( 'Level density parameter a  (1/keV)       <',F6.3,'>:',$)
      CALL READF(5,a)
      WRITE(6,31) n
31    FORMAT( 'Gamma exponent n (Cn*Eg**n) (n = 0 GDR)    <',F4.1,'>:',$)
      CALL READF(5,n)
      IF(n.EQ.0)THEN
        WRITE(6,32)FWHM
32      FORMAT('Give FWHM of the GDR             <',F6.1,'>:',$)
        CALL READF(5,FWHM)
        WRITE(6,33)Eres
33      FORMAT('Give energy centroid of the GDR <',F7.1,'>:',$)
      CALL READF(5,Eres)
      ENDIF

C Now, fasten seatbelts
      WRITE(6,*)' '
      WRITE(6,*)' Ch    Ex      FexpC      FexpG       Rexp      FteoC      FteoG       Rteo      TransM'

      DO j=j1,j2,jstep
        Rexp(j)  = 0.
        Rteo(j)  = 0.
        TransM(j) = 0.
        Ex = a0Ex + j*a1Ex
        e1 = Egmin
        e2 = a0Eg + valley(j)*a1Eg - 0.5*a1Eg
        FexpC(j) = FgExpInt(Ex,e1,e2)
        FteoC(j) = FgTeoInt(Ex,e1,Ex-700.)
        e1 = max(a0Eg + valley(j)*a1Eg + 0.5*a1Eg,Egmin)
        e2 = Ex+dE
        FexpG(j) = FgExpInt(Ex,e1,e2)
        FteoG(j) = FgTeoInt(Ex,Ex-700.,e2)
        sumexp =FexpC(j)+FexpG(j)
        sumteo =FteoC(j)+FteoG(j)
        IF(sumexp.GT.0)Rexp(j) = FexpG(j)/sumexp
        IF(sumteo.GT.0)Rteo(j) = FteoG(j)/sumteo
        IF(Rteo(j) .GT.0)TransM(j) = Rexp(j)/Rteo(j)
        WRITE(6,40)j,Ex,FexpC(j),FexpG(j),Rexp(j),FteoC(j),FteoG(j),Rteo(j),TransM(j)
 40     FORMAT(I4,F8.1,7E11.3)
      ENDDO

C Writting spectra to matrices

      cal(IDEST,1,1,1)=a0Ex
      cal(IDEST,1,1,2)=a1Ex
      cal(IDEST,1,1,3)=0.
      cal(IDEST,1,2,1)=0.
      cal(IDEST,1,2,2)=1.
      cal(IDEST,1,2,3)=0.
      XDIM=JDIM
      YDIM=7
      
      DO i=0,XDIM-1
        rMAT(IDEST,i,0)=FexpC(i)
        rMAT(IDEST,i,1)=FexpG(i)
        rMAT(IDEST,i,2)=Rexp(i)
        rMAT(IDEST,i,3)=FteoC(i)
        rMAT(IDEST,i,4)=FteoG(i)
        rMAT(IDEST,i,5)=Rteo(i)
        rMAT(IDEST,i,6)=TransM(i)
      ENDDO
      CALL LENGDE(fname(1,1),LIN)
      outfile=fname(1,1)(1:LIN)//'.gbf'
      comment='FexpC FexpG Rexp FteoC FteoG Rteo TransM'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,72)outfile
72    FORMAT(' FexpC FexpG Rexp FteoC FteoG Rteo TransM to file: ' , A12)
      STOP

99    WRITE(6,*)'Could not open file for results and spectra'
      END


      FUNCTION FgExpInt(Ex,e1,e2)
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      CHARACTER fname*8,comm*60
      CHARACTER APP*4

      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      COMMON/Fgexp/a0Eg,a1Eg,a0Ex,a1Ex
      EXTERNAL Fermi

C Finding integral of exp 1.gen distribution between e1 and e2
      FgExpInt=0.
      j  = ((Ex-a0Ex)/a1Ex)+0.5
      i0 = ((0.-a0Eg)/a1Eg)+0.5
      i1 = ((e1-a0Eg)/a1Eg)+0.5
      i2 = ((e2-a0Eg)/a1Eg)+0.5
      ie = max(i0,i1)
      IF(j.LT.0.OR.j.GT.YDIM-1)RETURN
      IF(i2.LT.0.OR.i2.GT.XDIM-1)RETURN
      DO WHILE(ie.LT.i2)
        FgExpInt = FgExpInt + rMAT(IDEST,ie,j)
        ie = ie + 1
      ENDDO
      RETURN
      END


      FUNCTION FgTeoInt(Ex,e1,e2)
      COMMON/Fgteo/a,n,Cf,Cn,Cgdr,FWHM,Eres
      REAL n
      EXTERNAL Fermi

C Finding integral of teo 1.gen distribution between e1 and e2
      de=10.                  !steps with 10 keV
      FgTeoInt =0.
      e=max(de,e1)
      DO WHILE(e.LT.e2)

C Estimating the applied gamma-strength function and integral
        IF(n.GT.0.)THEN
          Sgam = Cn*(e**n)
        ELSE
          Sgam = Cgdr*(e**4)*(FWHM**2)/((Eres**2-e**2)**2+(e**2)*(FWHM**2))
        ENDIF
        FgTeoInt = FgTeoInt + Sgam*Fermi(Ex-e,Cf,a)*de
        e = e + de
      ENDDO
      RETURN
      END

      FUNCTION Fermi(e,Cf,a)
      Fermi=0.
      ee=e
      IF(ee.LT.500.)ee=500. !To prevent rho to increase again 
      y=a*ee
      IF(y.GT.0.)THEN
        z=2.0*SQRT(y)
        IF(z.GT.88.)z=88.       ! we have single precision E+/-38
        IF(z.GT.-88.)THEN
          Fermi=Cf*EXP(z)/(ee**(5./4.))
        ENDIF
      ENDIF
      RETURN
      END


