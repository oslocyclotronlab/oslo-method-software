      PROGRAM primex
C Read/write stuff (mama)
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4

      REAL fg(0:4095,0:511),fgerr(0:4095,0:511),projx(0:4095),projy(0:511)
      REAL prx(0:4095,0:511),prxerr(0:4095,0:511)
      REAL xNorm(0:511), x_best(0:511), area(0:511), chi, chi_best, sum
      INTEGER XDIMsave,YDIMsave

      WRITE(6,*)'     ________________________________________________'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |                P R I M E X  1.1                |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    |    Program to extract primary gamma-spectra    |'
      WRITE(6,*)'    |   with a certain Ex binning. The averages of   |'
      WRITE(6,*)'    |  these spectra are found by a few iterations.  |'
      WRITE(6,*)'    |  The file you need is fg.rsg and data for the  |'
      WRITE(6,*)'    |    Ex=2*Delta limit where the nuclear level    |'
      WRITE(6,*)'    |   density is assumed to follow the CT-model.   |'
      WRITE(6,*)'    |   The new fg.prx matrix is written out with    |'
      WRITE(6,*)'    | the average fg spectrum in ch 0. Various data  |'
      WRITE(6,*)'    | and Chi**2 values are listed in file output.prx|'
      WRITE(6,*)'    |   Files:                                       |'
      WRITE(6,*)'    |   Input: fg.rsg         Output: fg.prx         |'
      WRITE(6,*)'    |          (input.prx)            output.prx     |'
      WRITE(6,*)'    |                                                |'
      WRITE(6,*)'    | Created:  Jun 2018                             |'
      WRITE(6,*)'    | Modified: Sep 01 2018, refined Chi**2 fit      |'
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

C Default values (keV units)
      Epair    = 2000.0
      Ex_low   = 4000.0
      Ex_high  = 8000.0
      k        = 2
      Iread    = 0
      EgL_norm = 0.0
      EgH_norm = Ex_low-Epair

      OPEN(23,FILE='input.prx',STATUS='old',ERR=666)
      READ(23,*,ERR=666) Epair
      READ(23,*,ERR=666) Ex_low, Ex_high, k
      READ(23,*,ERR =666)EgL_norm, EgH_norm
      Iread  = 1     !defaults values are replaced by values from last run
      GO TO 777
 666  WRITE(6,*)'Could not read input.prx, using default values'
 777  CLOSE(23)

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
      IF(bx+by.EQ.2)THEN
        a0=0.                       !Defaults in case of no calibration
        a1=120.
      ELSE
        a0=cal(1,IDEST,1,1)         !Picks the spectrum calibration
        a1=cal(1,IDEST,1,2)
      ENDIF

      sum=0.
      DO i=0,XDIM-1                 !Copying the 1. gen matrix
         DO j=0,YDIM-1
            fg(i,j) = rMAT(IDEST,i,j)
            sum     = sum+fg(i,j)
         ENDDO
      ENDDO
      IF(sum.LE.0)THEN
         WRITE(6,*)'No data read, check that fg.rsg is present in you directory'
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

      sum=0.
      DO i=0,XDIM-1                     !Storing the 1. gen error matrix
         DO j=0,YDIM-1
            fgerr(i,j) = rMAT(IDEST,i,j)
            sum        = sum+fgerr(i,j)
         ENDDO
      ENDDO
      IF(sum.LE.0)THEN
         WRITE(6,*)'No data read, check that fgerr.rsg is present in you directory'
         STOP
      ENDIF

      WRITE(6,1)a0
 1    FORMAT('Give calibration constant a0(keV)       <',F7.2,'>:',$)
      CALL READF(5,a0)
      WRITE(6,2)a1
 2    FORMAT('Give calibration constant a1(keV/ch)    <',F7.2,'>:',$)
      CALL READF(5,a1)

C      iu0 = INT((ABS(a0/a1) + 0.5)) ! Channel for Eg=0keV

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
 5    CONTINUE
      DO i=XDIM-1,mxlow,-1
         IF(projx(i).GT.0.)THEN
            mxHigh=i
            GO TO 7
         ENDIF
      ENDDO
 7    CONTINUE
      ExLow  = a0+a1*FLOAT(mxLow)
      ExHigh = a0+a1*FLOAT(mxHigh)
      WRITE(6,8)XDIM,mxLow,mxHigh,ExLow,ExHigh
 8    FORMAT('Dimension along x-axis:',I4,
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
 11   CONTINUE
      DO j=YDIM-1,mylow,-1
         IF(projy(j).GT.0.)THEN
            myHigh=j
            GO TO 12
         ENDIF
      ENDDO
 12   CONTINUE
      EyLow  = a0+a1*FLOAT(myLow)
      EyHigh = a0+a1*FLOAT(myHigh)
      WRITE(6,13)YDIM,myLow,myHigh,EyLow,EyHigh
 13   FORMAT('Dimension along y-axis:',I4,
     +/,'Region containing data is ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV')

      WRITE(6,*)''
      WRITE(6,*)'We have to set a lower limit in Ex value, where the NLD'
      WRITE(6,*)'is not believed to follow the CT model. Typically, that'
      WRITE(6,*)'is the largest value of 2Delta_n and 2Delta_p.'
      WRITE(6,*)''
      WRITE(6,23)Epair
 23   FORMAT('Give the pairing value Epair (keV)    <',F7.1,'>:',$)
      CALL READF(5,Epair)
      iEg_H  = INT((Epair/a1)+0.5)

      WRITE(6,*)' ' 
      WRITE(6,3)a0,a1
 3    FORMAT('Calibration along Ex and Eg axis is: a0=',F7.2,'keV and a1=',F7.2,'keV/ch')


C Asking for how to make average fg
      WRITE(6,*)''
      WRITE(6,*)'The fg spectra will be written out in the matrix area'
      WRITE(6,*)'corresponding to the boarders of the fg.rsg matrix.'
      WRITE(6,*)'The matrix with individual fg(Eg) as function of Ex is called:'
      WRITE(6,*)'fg.prx (compressed fg for initial excitation energies). '
      WRITE(6,*)'Also, the average fg are written in the 0th row of the matrix.'
      WRITE(6,*)'You will now be asked to enter the energy regions'
      WRITE(6,*)'from where you want to make the averages.'
      WRITE(6,*)''

      IF(Iread.EQ.0)THEN
         Ex_Low  = EyLow
         Ex_High = EyHigh
      ENDIF

      WRITE(6,40)Ex_Low
 40   FORMAT('Give lower initial excitation energy (keV)    <',F7.1,'>:',$)
      CALL READF(5,Ex_Low)
      WRITE(6,41)Ex_High
 41   FORMAT('Give higher initial excitation energy (keV)   <',F7.1,'>:',$)
      CALL READF(5,Ex_High)
      iEx_Low  = INT(((Ex_Low -a0)/a1)+0.5)
      iEx_High = INT(((Ex_High-a0)/a1)+0.5)
c      myHigh = INT(((EyHigh-a0)/a1)+0.5)

      Ex_Low  = a0 + a1*iEx_Low
      Ex_High = a0 + a1*iEx_High

      WRITE(6,44)iEx_Low,iEx_High,Ex_Low,Ex_High
 44   FORMAT(/,'Averaging fg(Ei) ch.: ',I4,' - ',I4,'  Energy: ',F7.1,' - ',F7.1,' keV')

C Asking for Ex binning
      WRITE(6,*)' '
      WRITE(6,*)'The various fg cover some excitation bin dEx'
      WRITE(6,*)'that is by default a1 (about 120 keV). However, you may compress the'
      WRITE(6,*)'y-axis (Ex) by a factor k. The result will be a new'
      WRITE(6,*)'a1 which is a multiple of the old a1 value. Also a0 will thereby'
      WRITE(6,*)'be changed and assures that the new most upper Ex spectrum'
      WRITE(6,*)'is based on k spectra. This implies that the lowest spectrum'
      WRITE(6,*)'may be based on k, k-1,.. or even just 1 spectrum.'
      WRITE(6,*)'The calibration for Eg is kept the same as for the original fg.rsg matrix.'
      WRITE(6,*)' '

      WRITE(6,24)k
 24   FORMAT('Give compression factor k <',I2,'>:',$)
      CALL READI(5,k)
      a1c   = FLOAT(k)*a1
      Exmax = ((a0+a1*iEx_High)+(a0+a1*(iEx_High-k+1)))/2. !Highest new Ex energy
      a0c   = Exmax-a1c*iEx_High
      WRITE(6,26)k,a0c,a1c
 26   FORMAT('Excitation bins compressed',I3,'x giving a0=',F10.2,'keV and a1=',F7.2,'keV/ch')


C Asking for Eg-limits for normalizing the fg spectra to each other
      WRITE(6,*)' '
      WRITE(6,*)'The various fg spectra has to be normalized to each other.'
      WRITE(6,*)'By default, they are normalized to unity. But in order to'
      WRITE(6,*)'see if they are equal of shape, we normalize so that we have'
      WRITE(6,*)'the same counts (area) from EgL=0 keV up to EgH=Ex_min-Epair'
      WRITE(6,*)' '

      WRITE(6,50)EgL_norm
 50   FORMAT('Give lower EgL energy (keV)     <',F7.1,'>:',$)
      CALL READF(5,EgL_norm)
      WRITE(6,51)EgH_norm
 51   FORMAT('Give higher EgH energy (keV)    <',F7.1,'>:',$)
      CALL READF(5,EgH_norm)
      iLow  = INT(((EgL_norm-a0)/a1)+0.5)
      iHigh = INT(((EgH_norm-a0)/a1)+0.5)

      OPEN(UNIT=13,FILE='input.prx')
      WRITE(13,*,ERR=668)Epair
      WRITE(13,*,ERR=668)Ex_low, Ex_high, k
      WRITE(13,*,ERR=668)EgL_norm, EgH_norm
      GO TO 778
 668  WRITE(6,*)'Could not write to input.prx'
 778  CLOSE(13)
     

C*****************************************
C                                        *
C     F A S T E N  S E A T  B E L T S    *
C                                        *
C*****************************************

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                             C
C  Calculating compressed fg  C
C                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
C Calculating compressed Ex with compression factor k
      DO i=0,XDIM-1
         DO j=0,YDIM-1
            prx(i,j)    = 0
            prxerr(i,j) = 0
         ENDDO
      ENDDO
      DO i = mxLow,mxHigh,1
         jnew = iEx_High+1
         DO j = iEx_High,iEx_Low,-k
            sum    = 0
            sum_f  = 0
            sum_df = 0
            df     = 0
            jnew = jnew-1
            DO l = 1,k
               jj=j-(l-1)
C                   write(6,*)'i,jj, iEg_H',i,jj, iEg_H
               IF(i.LE.jj.AND.jj.GE.iEx_Low)THEN
                  sum  = sum  + 1.
                  f    = fg(i,jj)
                  sum_f = sum_f + f
                  IF(f.GT.0) df = (f*AMIN1(2.,ABS(fgerr(i,jj)/f)))**2  !not more than 2x
                  sum_df = sum_df + df
               ENDIF
c               if(i.eq.20)write(6,*)'jnew, j,jj',jnew,j,jj,a0c + a1c*jnew,a0 + a1*jj
            ENDDO
            IF(sum.GT.0)   prx(i,jnew)    = sum_f/sum
            IF(sum_df.GT.0)prxerr(i,jnew) = SQRT(sum_df)/sum
         ENDDO
      ENDDO

C New channels along y-axis (excitation energy Ex)
      iEx_Lowc  = INT(((Ex_Low -a0c)/a1c)+0.5)
      iEx_Highc = INT(((Ex_High-a0c)/a1c)+0.5)

C Renormalizing the fg spectra
      DO j = iEx_Lowc,iEx_Highc
          area(j) = 0
          DO i = iLow,iHigh
             area(j) = area(j) + prx(i,j)
          ENDDO
      ENDDO
      IF(area(iEx_Highc).LT.0.0000001)THEN
          area(iEx_Highc)=100000
          WRITE(6,*)'Warning: normalization raw has zero counts'
      ENDIF
      DO j = iEx_Lowc,iEx_Highc
         Ex = a0c + a1c*j
         IF(area(j).GT.0)xNorm(j) = area(iEx_Highc)/area(j)
         DO i = 0,XDIM-1
             Eg = a0 + a1*i
             prx(i,j)    = xNorm(j)*prx(i,j)
             prxerr(i,j) = xNorm(j)*prxerr(i,j)
             IF(Eg.GT.(Ex-Epair))THEN  !Only decay down well above Ex=0 (Ex=2*Epairs)
                prx(i,j)    = 0
                prxerr(i,j) = 0
             ENDIF
         ENDDO
      ENDDO

C Calculating total compression put in ch 0
      DO i = mxLow,mxHigh,1
         sum    = 0
         sum_f  = 0
         sum_df = 0
         DO j = iEx_Highc,iEx_Lowc,-1
            IF(i.LE.j)THEN
               f    = prx(i,j)
               IF(f.GT.0)THEN
                  df     = 0
                  sum    = sum  + 1.
                  sum_f  = sum_f + f
                  df     = (f*AMIN1(2.,ABS(prxerr(i,j)/f)))**2
                  sum_df = sum_df + df
               ENDIF
            ENDIF
         ENDDO
         IF(sum.GT.0)  prx(i,0)    = sum_f/sum
         IF(sum_df.GT.0)prxerr(i,0) = SQRT(sum_df)/sum
      ENDDO

C Refining Chi**2-fit by multiplying prx(i,j) and prxerr(i,j) extra scaling: 0.9-1.1 (Sep. 2018)
       dx = (1.1-0.9)/100.
       DO j=iEx_Lowc,iEx_Highc
         chi_best = 100000.
         DO n = -50,50
           x= 1.+(dx*float(n))
           sum  = 0.
           chi  = 0.
           DO i=0,XDIM-1
              IF(prx(i,0).GT.0.AND.prx(i,j).GT.0.0005.AND.prxerr(i,j).GT.0.0001)THEN
                chi= chi+(ABS(x*prx(i,j)-prx(i,0))/(x*prxerr(i,j)))**2
                sum = sum + 1.
              ENDIF
           ENDDO
           IF(sum.GT.0)chi=chi/sum
           IF(chi.LT.chi_best.AND.chi.GT.0)THEN
             chi_best=chi
             x_best(j)  = x
           ENDIF
         ENDDO
c          WRITE(6,*)j, x_best(j), chi_best
      ENDDO

      DO j=iEx_Lowc,iEx_Highc
         xNorm(j)    = xNorm(j)*x_best(j)
         DO i=0,XDIM-1
            prx(i,j)    = prx(i,j)    *x_best(j)
            prxerr(i,j) = prxerr(i,j) *x_best(j)
         ENDDO
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
            rMAT(IDEST,i,j) = prx(i,j)
         ENDDO
      ENDDO

      outfile='fg.prx'
      comment='fg(Eg) with compression k of Ex, sum in ch 0'
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
            rMAT(IDEST,i,j) = prxerr(i,j)
         ENDDO
      ENDDO

      outfile='fgerr.prx'
      comment='fgerr(Eg) with compression k of Ex, sum in ch 0'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)

      WRITE(6,*)'      jchan    energy (j)        area(j)        xNorm(j)        chi**2(j)'

      OPEN(20,FILE='output.prx',ACCESS='SEQUENTIAL',ERR=99)
      DO j=iEx_Lowc,iEx_Highc
         sum  = 0.
         chi = 0.
         DO i=0,XDIM-1
            IF(prx(i,0).GT.0.AND.prx(i,j).GT.0.0005.AND.prxerr(i,j).GT.0.0001)THEN
              chi= chi+(ABS(prx(i,j)-prx(i,0))/prxerr(i,j))**2
              sum = sum + 1.
            ENDIF
         ENDDO
         IF(sum.GT.0)chi=chi/sum
         WRITE(6,*)j, a0c + a1c*j, area(j), xNorm(j), chi
         WRITE(20,*)j, a0c + a1c*j, area(j), xNorm(j), chi

      ENDDO
      CLOSE(20)

      GO TO 98
99    WRITE(6,*)'Could not write file'
98    CONTINUE
      END
