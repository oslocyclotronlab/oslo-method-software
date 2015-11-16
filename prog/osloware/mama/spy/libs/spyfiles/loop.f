      SUBROUTINE LOOP
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      INTEGER XDIM, YDIM      
      CHARACTER APP*4

      COMMON/spy1/m1,m2,lx(0:23),hx(0:23),ly(0:23),hy(0:23),mon(0:23),specna(0:23)
      COMMON/spy2/dCounts(0:511,0:23)
      COMMON/spy3/OLhi(24),OLlc(24),OLhc(24)
      INTEGER lx,hx,ly,hy

      DIMENSION CountsNew(0:23),CountsOld(0:23)

      CHARACTER specna*8,specnam*8

      INTEGER *4,time,iT0,iSec(0:511)
      CHARACTER string*24
      INTEGER type,sleeptime,system,realtime

      DO m=1,24
        OLhi(m)=2.
        OLlc(m)=0.
        OLhc(m)=511.
        CountsNew(m-1)=0
        CountsOld(m-1)=0
      ENDDO

      DO m=0,23
        DO i=0,511
          dCounts(i,m)=0.
        ENDDO
      ENDDO

      ITYPE=3
      iD  =2                                        !OFFLINE matrices
      type=1
      IF(ITYPE.GT.1)type=2

      iT0=time()                                    !seconds after Jan 1. 1970
      sleeptime=60                                  !accumulate 60 seconds
      WRITE(6,10)sleeptime
 10   FORMAT(/,'Integration time for count rates ( > 10 sec) <',I2,'>:',$)
      CALL READI(5,sleeptime)
      IF(sleeptime.LT.10)sleeptime=10

      WRITE(6,12)sleeptime
 12   FORMAT(/,'Count rates are monitored every ',I4,' seconds. Please, wait...')
      realtime=sleeptime

      CALL INITG(nx,ny)

C Starts almost infinite loop
      lStep=10
      lmax=60.*24.*3600./FLOAT(realtime)            !two months
      DO l=1,lmax                                 
        DO m=m1,m2
          specnam=specna(m)
          IF(iD.EQ.1)THEN                           !reading SIRIUS matrix
            call sirius_spectra(type,specnam)
          ENDIF
          IF(iD.EQ.2)THEN                           !reading OFFLINE matrix
            call offline_spectra(type,specnam)
          ENDIF
          sum=0.                                    !summing counts      
          DO ii=lx(m),hx(m)
            DO jj=ly(m),hy(m)
              sum=sum+MAT(ii,jj)
            ENDDO
          ENDDO
          sum=sum/FLOAT(realtime)

          CountsOld(m)=CountsNew(m)
          CountsNew(m)=sum
          DO k=5,511                                !store last 506 count rates
            dCounts(k-1,m) =dCounts(k,m)
          ENDDO

          IF(l.GT.1)THEN                            !calculate last values
            dCounts(511,m)=CountsNew(m)-CountsOld(m)
            IF(m.EQ.0)xMon=dCounts(511,m)
            IF(m.GT.0.AND.xMon.GT.0.AND.mon(m).EQ.1)dCounts(511,m)=dCounts(511,m)/xMon
          ENDIF
          IF(dCounts(511,m).LT.0.)dCounts(511,m)=0.

          IF(l.LT.10.OR.((l/lStep)*lStep.EQ.l))THEN
            sum=0.
            xk =0.                                  !new average for last 10 ch
            DO k=501,511
              IF(dCounts(k,m).GT.0)THEN
                xk=xk+1.
                sum=sum+dCounts(k,m)
              ENDIF
            ENDDO
            IF(xk.GT.0.)Olhi(m+1)=2.0*(sum/xk)  
            IF(OLhi(m+1).LE.0.)OLhi(m+1)=2.
          ENDIF

        ENDDO
        DO k=1,511                                  !store last 512 times
          iSec(k-1) =iSec(k)
          iSec(k)   =time(0)-iT0
        ENDDO
        IF(l.LT.10.OR.((l/lStep)*lStep.EQ.l))THEN
          a1=FLOAT(realtime)/3600.                  !hours/channel
          kk=-1
          DO k=0,510
            IF(iSec(k).GT.0)THEN
              kk=k
              GO TO 97
            ENDIF
          ENDDO
97        IF(kk.GT.-1)realtime=(FlOAT(iSec(511)-iSec(kk))/(511.-FLOAT(kk)))+0.5
        ENDIF


        CALL DATETIME(string)                       !making time calibration
        READ(string(6:7),*)ihour
        READ(string(9:10),*)imin
        hour=FLOAT(ihour)+(FLOAT(imin)/60.)
        a0=hour-(511.*a1)
        cal(1,1,1,1)=a0
        cal(1,1,1,2)=a1
        cal(1,1,1,3)=0.

        IF(l.GT.1)CALL outlaySPY

        system=sleep(sleeptime)

      ENDDO

      write(6,*)'Spying on matrices has now been performed 2 months, and'
      write(6,*)'the spy procedure has been terminated. If you want to'
      write(6,*)'continue, start again by typing spy. Have a nice day!'

      RETURN
      END
