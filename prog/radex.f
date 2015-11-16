      PROGRAM radex
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4,infile*20
      REAL f(0:4095),sf(0:4095),fg(0:4095,0:511),fgerr(0:4095,0:511),projx(0:4095),projy(0:511)
      REAL rsf(0:4095,0:511),rsferr(0:4095,0:511)
      REAL ave(0:4095), rms(0:4095), distr(0:199)
      REAL rsftot(0:8192),rhotot(0:8192)
      REAL T0(0:8192),P(0:4095,0:511),dP(0:4095,0:511),Tv(0:4095,0:511)
      INTEGER XDIMsave,YDIMsave

      WRITE(6,*)'     ________________________________________________'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |                 R A D E X  1.6.4               |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |     Program to calculate radiative strength    |'
      WRITE(6,*)'    |   functions (RSF) f(E_i) and f(E_f) when the   |'
      WRITE(6,*)'    |      level density Rho is known. The files     |'
      WRITE(6,*)'    |   you need are fg, rhopaw.cnt, sigextpaw.cnt   |'
      WRITE(6,*)'    |   and rhotmopaw.cnt. Run first the rhosigchi   |'
      WRITE(6,*)'    |  program with full range e.g. -800 to 9000 keV |'
      WRITE(6,*)'    |     These new fg.rsg and fgerr.rsg are also    |'
      WRITE(6,*)'    |             input files to RADEX               |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    | Created:  Apr 2004                             |'
      WRITE(6,*)'    | Modified: Nov 2014:include Ex < 0 MeV,         |'
      WRITE(6,*)'    |                    better guess for Ex, Eg     |'
      WRITE(6,*)'    | Modified: Jun 2015:ch0 output                  |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |           Oslo Cyclotron Laboratory            |'
      WRITE(6,*)'    |                Magne Guttormsen                |'
      WRITE(6,*)'    |________________________________________________|'
      WRITE(6,*)' '

C Initializing
      DO j = 0,511
        projy(j) = 0.
      ENDDO
      DO i = 0,4095
        projx(i) = 0.
      ENDDO

      pi     = 3.14159
      xnorm  = 1.50000e+09
      Iread  = 0
      OPEN(23,FILE='input.rad',STATUS='old',ERR=666)
      READ(23,*,ERR=666)  xnorm, k
      READ(23,*,ERR=666) ei_low, ei_high
      READ(23,*,ERR=666) ef_low, ef_high
      Iread  = 1                          !defaults have been read
      GO TO 777
 666  WRITE(6,*)'Could not read input.rad, using default values'
 777  CLOSE(23)

C Reading total rsf and rho for normalization reasons 
      ich=0
      infile='sigextpaw.cnt'
      OPEN(22,FILE=infile,ACCESS='SEQUENTIAL',status='old',ERR=900)
      ich=0
      DO i=0,8192
        READ(22,*,END=100,ERR=900)xdumr
        rsftot(i)=xdumr
        ich=i
      ENDDO
100   IF(ich.LE.1)GO TO 900
      GO TO 990
900   WRITE(6,*)'Could not read:', infile
      CLOSE(22)
      STOP
990   WRITE(6,*)infile,' read with dimension =',ich+1
      CLOSE(22)
      iT0dim=ich+1

      ich=0
      infile='rhotmopaw.cnt'
      OPEN(22,FILE=infile,ACCESS='SEQUENTIAL',status='old',ERR=901)
      ich=0
      DO i=0,8192
        READ(22,*,END=101,ERR=901)xdumr
        rhotot(i)=xdumr
        ich=i
      ENDDO
101   IF(ich.LE.1)GO TO 901
      GO TO 991
901   WRITE(6,*)'Could not read:', infile
      CLOSE(22)
      STOP
991   WRITE(6,*)infile,' read with dimension =',ich+1
      CLOSE(22)

      ich=0
      infile='rhopaw.cnt'
      OPEN(22,FILE=infile,ACCESS='SEQUENTIAL',status='old',ERR=902)
      ich=0
      DO i=0,8192
        READ(22,*,END=102,ERR=902)xdumr
        f(i)=xdumr
        ich=i

      ENDDO
102   IF(ich.LE.1)GO TO 902
      GO TO 992
902   WRITE(6,*)'Could not read:', infile
      CLOSE(22)
      STOP
992   WRITE(6,*)infile,' read with dimension =',(ich+1)/2,'x2'
      CLOSE(22)
      iidim = (ich+1)/2
      DO i = 0,iidim-1
         sf(i)=f(i+iidim)
      ENDDO

C Reading experimental fg.rsg matrix from rhosigchi program
      IDEST=1
      ITYPE=3
      WRITE(6,*)' '
      WRITE(6,*)'Please, answer 1 and the name of your input fg.rsg'
      WRITE(6,*)'matrix (created by rhosig) in the two next questions... '
      WRITE(6,*)' '

      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
      IF(bx+by.EQ.2.)THEN     
        a0=0.                       !Defaults in case of no calibration
        a1=120.
      ELSE
        a0=cal(1,IDEST,1,1)         !Picks the spectrum calibration
        a1=cal(1,IDEST,1,2)
      ENDIF

      sumfg=0.
      DO i=0,XDIM-1                 !Copying the 1. gen matrix
         DO j=0,YDIM-1
            fg(i,j) = rMAT(IDEST,i,j)
            sumfg   = sumfg+fg(i,j)
            P(i,j)  = fg(i,j)       !The original fg stored for use in the RSF(Ef) part
         ENDDO
      ENDDO
      IF(sumfg.EQ.0)THEN
         WRITE(6,*)'No data read, check that fg.rsg exist in you directory' 
         STOP 
      ENDIF
      XDIMsave = XDIM
      YDIMsave = YDIM

C Reading experimental fgerr.rsg matrix from rhosigchi program
      IDEST=1
      ITYPE=3
      WRITE(6,*)'Please, answer 1 and the name of your input fgerr.rsg'
      WRITE(6,*)'matrix (created by rhosig) in the two next questions... '
      WRITE(6,*)' '

      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      CALL READFILE
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512

      sumfgerr=0.
      DO i=0,XDIM-1                     !Storing the 1. gen error matrix
         DO j=0,YDIM-1
            fgerr(i,j) = rMAT(IDEST,i,j)
            sumfgerr   = sumfgerr+fgerr(i,j)
            dP(i,j)    = fgerr(i,j)     !The original fgerr stored for use in the RSF(Ef) part
         ENDDO
      ENDDO
      IF(sumfgerr.EQ.0)THEN
         WRITE(6,*)'No data read, check that fgerr.rsg exist in you directory' 
         STOP
      ENDIF

      WRITE(6,1)a0
1     FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,2)a1
2     FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)

      iu0 = INT((ABS(a0/a1) + 0.5))

      WRITE(6,4)xnorm
4     FORMAT('Give normalization constant from the normalization program <',E14.7,'>:',$)
      CALL READF(5,xnorm)

C Finding x-markers
      mxLow=0
      mxHigh=XDIM-1
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            projx(i)=projx(i)+rMAT(IDEST,i,j)
         ENDDO
      ENDDO
      DO i=0,XDIM-1
         IF(projx(i).GT.0.)THEN
            mxLow=i
            write(6,*)i, mxlow, projx(i)

            GO TO 5
         ENDIF
      ENDDO
5     CONTINUE
      DO i=XDIM-1,mxlow,-1
         IF(projx(i).GT.0.)THEN
            mxHigh=i
            GO TO 7
         ENDIF
      ENDDO
7     CONTINUE
      ExLow  = a0+a1*FLOAT(mxLow)
      ExHigh = a0+a1*FLOAT(mxHigh)
      WRITE(6,8)XDIM,mxLow,mxHigh,ExLow,ExHigh
8     FORMAT('Dimension along x-axis:',I4,
     +/,'Region containing data is ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV') 

C Finding y-markers
      myLow  = 0
      myHigh = YDIM-1
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            projy(j)=projy(j)+rMAT(IDEST,i,j)
         ENDDO
      ENDDO
      DO j=0,YDIM-1
         IF(projy(j).GT.0.)THEN
            myLow=j
            GO TO 11
         ENDIF
      ENDDO
11    CONTINUE
      DO j=YDIM-1,mylow,-1
         IF(projy(j).GT.0.)THEN
            myHigh=j
            GO TO 12
         ENDIF
      ENDDO
12    CONTINUE
      EyLow  = a0+a1*FLOAT(myLow)
      EyHigh = a0+a1*FLOAT(myHigh)
      WRITE(6,13)YDIM,myLow,myHigh,EyLow,EyHigh
13    FORMAT('Dimension along y-axis:',I4,
     +/,'Region containing data is ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV')

      WRITE(6,30)EyLow
30    FORMAT('Give lower excitation energy (keV)     <',F7.1,'>:',$)
      CALL READF(5,EyLow)
      WRITE(6,31)EyHigh
31    FORMAT('Give higher excitation energy (keV)    <',F7.1,'>:',$)
      CALL READF(5,EyHigh)
      myLow  = INT(((EyLow-a0)/a1)+0.5)
      myHigh = INT(((EyHigh-a0)/a1)+0.5)
      EyLow  = a0+a1*FLOAT(myLow)
      EyHigh = a0+a1*FLOAT(myHigh)
      WRITE(6,32)myLow,myHigh,EyLow,EyHigh
32    FORMAT('Channels on y-axis chosen: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV')

C Finding mLow and mHigh markers
      mLow=0
      mHigh=XDIM-1
      DO i=1,XDIM-1
         IF(f(i-1).EQ.0.AND.f(i).NE.0)THEN
            mLow=i
            GO TO 20
         ENDIF
      ENDDO
20    CONTINUE
      DO i=XDIM-1,1,-1
         IF(f(i).EQ.0.AND.f(i-1).NE.0)THEN
            mHigh=i-1
            GO TO 21
         ENDIF
      ENDDO
21    CONTINUE
      ELow=a0+a1*FLOAT(mLow)
      EHigh=a0+a1*FLOAT(mHigh)
      WRITE(6,22)XDIM,mLow,mHigh,ELow,EHigh
22    FORMAT(/,'The rho spectrum (rhopaw.cnt) has dimension:',I4,
     +/,'Region containing data is ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV') 

C Normalizing fg(Eg,Ex) according to rho(Ex) and T(Eg) as function of Ex
C fg and fgerr will be normalized as the expression N(Ei)*P(Ei,Eg) in the 46Ti PRC paper
       DO j=myLow,myHigh
          sum = 0
          DO i=mxLow,j+iu0
             sum = sum + rhotot(j-i+iu0)*rsftot(i)
          ENDDO
          DO i=mxLow,j+iu0
             fg(i,j)    = fg(i,j)*sum
             fgerr(i,j) = fgerr(i,j)*sum
          ENDDO
       ENDDO

      WRITE(6,*)' ' 
      WRITE(6,3)a0,a1
3     FORMAT('Calibration along Ex and Eg axis is: a0=',F7.2,'keV and a1=',F7.2,'keV/ch')


C Asking for how to make average RSFs
      WRITE(6,*)''
      WRITE(6,*)'The RSF values will be written out in the matrix area'
      WRITE(6,*)'corresponding to the boarders of the fg.rsg matrix.'
      WRITE(6,*)'The matrices with individual RSF(Eg) as function of E is called:'
      WRITE(6,*)'rsfi.rad (RSF for initial excitation energies) and'
      WRITE(6,*)'rsff.rad (RSF for final excitation energies).'
      WRITE(6,*)'Also, the average RSFs are written in the 0th row of the matrices:'
      WRITE(6,*)'You will now be asked to enter the energy regions'
      WRITE(6,*)'from where you want to make the averages.'
      WRITE(6,*)''

      IF(Iread.EQ.0)THEN
         ei_low  = EyLow
         ei_high = EyHigh
         ef_low  = 500.
         ef_high = a0 + a1*(myHigh - mxLow +iu0)
      ENDIF

      WRITE(6,40)ei_low
40    FORMAT('Give lower initial excitation energy (keV)    <',F7.1,'>:',$)
      CALL READF(5,ei_low)
      WRITE(6,41)ei_high
41    FORMAT('Give higher initial excitation energy (keV)   <',F7.1,'>:',$)
      CALL READF(5,ei_high)
      iei_low  = INT(((ei_low -a0)/a1)+0.5)
      iei_high = INT(((ei_high-a0)/a1)+0.5)

      WRITE(6,42)ef_low
42    FORMAT('Give lower final excitation energy (keV)      <',F7.1,'>:',$)
      CALL READF(5,ef_low)
      WRITE(6,43)ef_high
43    FORMAT('Give higher final excitation energy (keV)     <',F7.1,'>:',$)
      CALL READF(5,ef_high)
      ief_low  = INT(((ef_low -a0)/a1)+0.5)
      ief_high = INT(((ef_high-a0)/a1)+0.5)

      ei_low  = a0 + a1*iei_low
      ei_high = a0 + a1*iei_high
      ef_low  = a0 + a1*ief_low
      ef_high = a0 + a1*ief_high

      WRITE(6,44)iei_low,iei_high,ei_low,ei_high
44    FORMAT(/,'Averaging RSF(Ei) ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV')
      WRITE(6,45)ief_low,ief_high,ef_low,ef_high
45    FORMAT(  'Averaging RSF(Ef) ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV')

C Asking for Ex binning
      WRITE(6,*)' '
      WRITE(6,*)'The various RSFs calculated cover some excitation bin dEx'
      WRITE(6,*)'that is by default a1 (about 100 keV). However, you may compress the'
      WRITE(6,*)'y-axis (Ex) by a factor k. The result will be a new'
      WRITE(6,*)'a1 which is a multiple of the old a1 value. Also a0 will thereby'
      WRITE(6,*)'be changed and assures that the new most upper Ex spectrum'
      WRITE(6,*)'is based on k spectra. This implies that the lowest spectrum'
      WRITE(6,*)'may be based on k, k-1,.. or even just 1 spectrum.'
      IF(Iread.EQ.0) k = 1
      WRITE(6,*)' '
      WRITE(6,24)k
24    FORMAT('Give compression value k <',I2,'>:',$)
      CALL READI(5,k)
      a1c   = FLOAT(k)*a1
      Exmax = ((a0+a1*myHigh)+(a0+a1*(myHigh-k+1)))/2. !Highest new Ex energy         
      a0c   = Exmax-a1c*myHigh
      WRITE(6,26)k,a0c,a1c
26    FORMAT('Excitation bins compressed',I3,'x giving a0=',F10.2,'keV and a1=',F7.2,'keV/ch')

      OPEN(UNIT=13,FILE='input.rad')
      WRITE(13,*,ERR=668) xnorm, k
      WRITE(13,*,ERR=668) ei_low, ei_high
      WRITE(13,*,ERR=668) ef_low, ef_high

      GO TO 778
 668  WRITE(6,*)'Could not write to input.rad'
 778  CLOSE(13)
     

C*****************************************
C                                        *
C     F A S T E N  S E A T  B E L T S    *
C                                        *
C*****************************************

CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                          C
C  Calculating RSF for Ei  C
C                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
C Calculating compressed Ex with compression k
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rsf(i,j)    = 0
            rsferr(i,j) = 0
         ENDDO
      ENDDO
      DO i = mxLow,mxHigh,1
         egam = (a0+a1*FLOAT(i))/1000.
         jnew = myHigh+1
         DO j = myHigh,myLow,-k
            sum = 0
            sumT = 0
            sumdT = 0
            jnew = jnew-1
            DO l = 1,k
               jj=j-(l-1)
               IF(i.LE.jj+iu0.AND.jj.GE.myLow.AND.f(jj-i+iu0).GT.0.)THEN
                  sum  = sum  + 1.
                  T    = 0.
                  T    = fg(i,jj)/f(jj-i+iu0)
                  sumT = sumT + T
                  dT1  = 0.00001
                  dT2  = 0.00001
                  IF(fg(i,jj).GT.0)    dT1 = (T*AMIN1(2.,ABS(fgerr(i,jj)/fg(i,jj))))**2  !not more than 2x
                  IF(f(jj-i+iu0) .GT.0)dT2 = (T*AMIN1(2.,ABS(sf(jj-i+iu0)/f(jj-i+iu0))))**2
                  sumdT = sumdT + dT1 + dT2
               ENDIF
            ENDDO
            IF(sum.GT.0)  rsf(i,jnew)    = sumT/(sum*xnorm*egam**3)
            IF(sumdT.GT.0)rsferr(i,jnew) = SQRT(sumdT)/(sum*xnorm*egam**3)
         ENDDO
      ENDDO

C Calculating total compression put in ch 0
      DO i = mxLow,mxHigh,1
         egam  = (a0+a1*FLOAT(i))/1000.
         sum   = 0
         sumT  = 0
         sumdT = 0
         DO j = iei_high,iei_low,-1
            IF(i.LE.j.AND.f(j-i+iu0).GT.0.)THEN
               sum  = sum  + 1.
               T    = fg(i,j)/f(j-i+iu0)
               sumT = sumT + T
               dT1  = 0.00001
               dT2  = 0.00001
               IF(fg(i,j).GT.0)   dT1 = (T*AMIN1(2.,ABS(fgerr(i,j)/fg(i,j))))**2
               IF(f(j-i+iu0).GT.0)dT2 = (T*AMIN1(2.,ABS(sf(j-i+iu0)/f(j-i+iu0))))**2
               sumdT = sumdT + dT1 + dT2
            ENDIF
         ENDDO
         IF(sum.GT.0)  rsf(i,0)    = sumT/(sum*xnorm*egam**3)
         IF(sumdT.GT.0)rsferr(i,0) = SQRT(sumdT)/(sum*xnorm*egam**3)
      ENDDO

C Writting spectra to matrix
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=a0c
      cal(1,1,2,2)=a1c
      cal(1,1,2,3)=0.
      XDIM = XDIMsave
      YDIM = YDIMsave

      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rMAT(IDEST,i,j) = rsf(i,j)
         ENDDO
      ENDDO
      outfile='rsfi.rad'
      comment='Total T(Eg) with compression k of Exi, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rMAT(IDEST,i,j) = rsferr(i,j)
         ENDDO
      ENDDO
      outfile='rsferri.rad'
      comment='Total dT(Eg) with compression k of Exi, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)

      OPEN(20,FILE='rsfi.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            WRITE(20,*)rsf(i,j)
         ENDDO
      ENDDO
      CLOSE(20)


      OPEN(20,FILE='rsferri.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            WRITE(20,*)rsferr(i,j)
         ENDDO
      ENDDO
      CLOSE(20)

      OPEN(20,FILE='sum_initial.rad',ACCESS='SEQUENTIAL',ERR=99)
      DO i=0,XDIM-1
         WRITE(20,*)a0 + a1*i, rsf(i,0), rsferr(i,0)
      ENDDO
      CLOSE(20)






CCCCCCCCCCCCCCCCCCCCCCCCCC
C                        C
C PT-fluctuations for Ei C
C                        C
CCCCCCCCCCCCCCCCCCCCCCCCCC

C Choosing 4 highest gates for 56Fe, ych 83,84,85,86 in rsfi.rad
C Ei = 9.25, 9.50, 9.75, 9.99 MeV
C with Eg = 2.1 - 10.0 MeV
      ej1 = 9249.5
      ej2 = 9497.5
      ej3 = 9745.5
      ej4 = 9993.5
      ei1 = 2119.5
      ei2 = 9993.5
      j1 = INT(((ej1 - a0c)/a1c) +0.5)
      j2 = INT(((ej2 - a0c)/a1c) +0.5)
      j3 = INT(((ej3 - a0c)/a1c) +0.5)
      j4 = INT(((ej4 - a0c)/a1c) +0.5)
      i1 = INT(((ei1 -  a0)/ a1) +0.5)
      i2 = INT(((ei2 -  a0)/ a1) +0.5)

      WRITE(6,*)'Fluctuations for Ei (keV)=',ej1,ej2,ej3,ej4
      WRITE(6,*)'Fluctuations for Ei (ch)=',j1,j2,j3,j4
      WRITE(6,*)'Fluctuations for Ei (keV)=',a0c+j1*a1c,a0c+j2*a1c,a0c+j3*a1c,a0c+j4*a1c
      WRITE(6,*)'Fluctuations for Eg (keV)=',ei1,ei2
      WRITE(6,*)'Fluctuations for Eg (ch)=',i1,i2
      WRITE(6,*)'Fluctuations for Eg (keV)=',a0+i1*a1,a0+i2*a1

C Finding average RSF for the gates and normalize all gates to the average
        sum1 = 0.
        sum2 = 0.
        sum3 = 0.
        sum4 = 0.
        sum1234  = 0.
      DO i = i1,i2
        sum1 = sum1 + rsf(i,j1)
        sum2 = sum2 + rsf(i,j2)
        sum3 = sum3 + rsf(i,j3)
        sum4 = sum4 + rsf(i,j4)
      ENDDO
      sum1234 = (sum1 + sum 2 + sum3 + sum4)/4.
      DO i = i1,i2
        rsf(i,j1) = rsf(i,j1)*sum1234/sum1
        rsf(i,j2) = rsf(i,j2)*sum1234/sum2
        rsf(i,j3) = rsf(i,j3)*sum1234/sum3
        rsf(i,j4) = rsf(i,j4)*sum1234/sum4
      ENDDO
      IF(sum1.GT.0.AND.sum2.GT.0.AND.sum3.GT.0.AND.sum4.GT.0.)THEN
      WRITE(6,*)'RSFs renormalized with =',sum1234/sum1,sum1234/sum2,sum1234/sum3,sum1234/sum4
      ENDIF
      DO i = i1,i2
        ave(i) = (rsf(i,j1) + rsf(i,j2)+ rsf(i,j3) + rsf(i,j4))/4.
      ENDDO

      DO i = i1,i2
        rsf(i,j1) = (rsf(i,j1)-ave(i))/ave(i)
        rsf(i,j2) = (rsf(i,j2)-ave(i))/ave(i)
        rsf(i,j3) = (rsf(i,j3)-ave(i))/ave(i)
        rsf(i,j4) = (rsf(i,j4)-ave(i))/ave(i)
        rsferr(i,j1) = (rsferr(i,j1)-0)/ave(i)
        rsferr(i,j2) = (rsferr(i,j2)-0)/ave(i)
        rsferr(i,j3) = (rsferr(i,j3)-0)/ave(i)
        rsferr(i,j4) = (rsferr(i,j4)-0)/ave(i)
      ENDDO

C Binning per 1 percent
      DO i = i1,i2
        ip1 = INT((rsf(i,j1)/0.01) + 0.5) + 100
        ip2 = INT((rsf(i,j2)/0.01) + 0.5) + 100
        ip3 = INT((rsf(i,j3)/0.01) + 0.5) + 100
        ip4 = INT((rsf(i,j4)/0.01) + 0.5) + 100
        if(ip1.GT.0.AND.ip1.LT.200) distr(ip1) = distr(ip1) + 1.
        if(ip2.GT.0.AND.ip2.LT.200) distr(ip2) = distr(ip2) + 1.
        if(ip3.GT.0.AND.ip3.LT.200) distr(ip3) = distr(ip3) + 1.
        if(ip4.GT.0.AND.ip4.LT.200) distr(ip4) = distr(ip4) + 1.
      ENDDO

      OPEN(20,FILE='fluct_distr.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO i = 0,199
        write(20,*)distr(i)
      ENDDO
      CLOSE(20)

      sum = 0.
      DO i = i1,i2
        rms(i) = rsf(i,j1)**2 + rsf(i,j2)**2 + rsf(i,j3)**2 + rsf(i,j4)**2
        rms(i) = SQRT(rms(i)/4.)
        sum    = sum + rms(i)
      ENDDO
      sum = sum/FLOAT((abs(i1-i2)+1))
      IF(sum.GT.0.) WRITE(6,*)'RMS is =',sum
C Writing matrices
      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      DO i=i1,i2
         DO j=j1,j4
            rMAT(IDEST,i,j) = rsf(i,j)
         ENDDO
      ENDDO
      outfile='flucti.rad'
      comment='Ratio (rsf-ave)/ave for Exi, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rMAT(IDEST,i,j) = rsferr(i,j)
         ENDDO
      ENDDO
      outfile='flucterri.rad'
      comment='Errors in Ratio (rsf-ave)/ave for Exi, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)

      OPEN(20,FILE='flucti.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            WRITE(20,*)rsf(i,j)
         ENDDO
      ENDDO
      CLOSE(20)


      OPEN(20,FILE='flucterri.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            WRITE(20,*)rsferr(i,j)
         ENDDO
      ENDDO
      CLOSE(20)


CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                          C
C  Calculating RSF for Ef  C
C                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Normalizing T0
      xT   = 2.*pi/xnorm
      DO i = 0,iT0dim-1
          T0(i)=rsftot(i)*xT
      ENDDO

      WRITE(6,*)''
      jfLow  = 0                            ! Lower and upper Ef
      jfHigh = myHigh - mxLow +iu0
      a1c    = FLOAT(k)*a1
      Exmax  = ((a0+a1*jfHigh)+(a0+a1*(jfHigh-k+1)))/2. !Highest new Exf energy
      a0c    = Exmax-a1c*jfHigh
      WRITE(6,27)k,a0c,a1c
27    FORMAT('Excitation bins compressed',I3,'x giving a0=',F10.2,'keV and a1=',F7.2,'keV/ch')

C Calculating compressed Exf with compression k
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rsf(i,j)    = 0
            rsferr(i,j) = 0
            Tv(i,j)     = T0(i)   ! initializing T(Eg,Ef) = T0(Eg)
         ENDDO
      ENDDO

      DO iter= 1,1       !DOES NOT USE ANY ITERATION, MAYBE FOR FUTURE
        DO i = mxLow,mxHigh,1
         egam  = (a0+a1*FLOAT(i))/1000.
         jnew  = jfHigh+1
         DO jf = jfHigh,jfLow,-k
            sum   = 0
            sumT  = 0
            sumdT = 0
            jnew  = jnew-1
            DO l  = 1,k
               jjf = jf-(l-1)
               ji  = jjf+(i-iu0)
               sumT0 = 0
               DO ii = mxLow,ji+iu0
                  sumT0 = sumT0 + rhotot(ji-ii+iu0)*Tv(ii,ji-ii+iu0)
               ENDDO
               IF(i.LE.ji+iu0.AND.ji.LE.myHigh.AND.ji.GE.myLow.AND.f(jjf).GT.0.)THEN
                  sum   = sum  + 1.
                  T     = 0.
                  T     = P(i,ji)*sumT0/f(jjf)
                  Tv(i,jjf) = T      
                  sumT  = sumT + T
                  dT1   = 0.00001
                  IF(P(i,ji).GT.0)dT1=(T*AMIN1(2.,ABS(dP(i,ji)/P(i,ji))))**2  !not more than 2x
                  sumdT = sumdT + dT1
               ENDIF
            ENDDO
            IF(sum.GT.0)  rsf(i,jnew)    = sumT/(sum*2.*pi*egam**3)
            IF(sumdT.GT.0)rsferr(i,jnew) = SQRT(sumdT)/(sum*2.*pi*egam**3)
         ENDDO
        ENDDO
      ENDDO
C Calculating total compression, put into ch 0
      DO i     = mxLow,mxHigh,1
         egam  = (a0+a1*FLOAT(i))/1000.
         sum   = 0
         sumT  = 0
         sumdT = 0
         DO jf = ief_high,ief_low,-1
            ji=jf+(i-iu0)
            sumT0 = 0
            DO ii = mxLow,ji
               sumT0 = sumT0 + rhotot(ji-ii+iu0)*Tv(ii,ji-ii+iu0)
            ENDDO
            IF(i.LE.ji.AND.ji.LE.myHigh.AND.ji.GE.myLow.AND.f(jf).GT.0.)THEN
               sum   = sum  + 1.
               T     = 0.
               T     = P(i,ji)*sumT0/f(jf)
               sumT  = sumT + T
               dT1   = 0.00001
               IF(P(i,ji).GT.0)dT1=(T*AMIN1(2.,ABS(dP(i,ji)/P(i,ji))))**2
               sumdT = sumdT + dT1
            ENDIF
         ENDDO
         IF(sum.GT.0)  rsf(i,0)    = sumT/(sum*2.*pi*egam**3)
         IF(sumdT.GT.0)rsferr(i,0) = SQRT(sumdT)/(sum*2.*pi*egam**3)
      ENDDO

C Writting spectra to matrix
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=a0c
      cal(1,1,2,2)=a1c
      cal(1,1,2,3)=0.
      XDIM = XDIMsave
      YDIM = YDIMsave

      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rMAT(IDEST,i,j) = rsf(i,j)
         ENDDO
      ENDDO
      outfile='rsff.rad'
      comment='Total T(Eg) with compression k of Exf, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      DO i=0,4095
         DO j=0,511
            rMAT(IDEST,i,j)=0
         ENDDO
      ENDDO
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            rMAT(IDEST,i,j) = rsferr(i,j)
         ENDDO
      ENDDO
      outfile='rsferrf.rad'
      comment='Total dT(Eg) with compression k of Exf, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)

      OPEN(20,FILE='rsff.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            WRITE(20,*)rsf(i,j)
         ENDDO
      ENDDO
      CLOSE(20)

      OPEN(20,FILE='rsferrf.paw',ACCESS='SEQUENTIAL',ERR=99)
      DO j=0,YDIM-1
         DO i=0,XDIM-1
            WRITE(20,*)rsferr(i,j)
         ENDDO
      ENDDO
      CLOSE(20)

      OPEN(20,FILE='sum_final.rad',ACCESS='SEQUENTIAL',ERR=99)
      DO i=0,XDIM-1
         WRITE(20,*)a0 + a1*i, rsf(i,0), rsferr(i,0)
      ENDDO
      CLOSE(20)

      GO TO 98
99    WRITE(6,*)'Could not write file'
98    CONTINUE
      END
