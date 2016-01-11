      SUBROUTINE loop
      COMMON/Sp1Dim/MSPEC(0:8191),MAXCH
      COMMON/Sp2Dim/MAT(0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM
      CHARACTER APP*4,FILNAM*255
      COMMON/update/iD,ixy,i1,i2,specna
      COMMON/OL/I3,iRC,m1,m2,Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                         OLlow,OLhigh,OLlocnt,OLhicnt
      CHARACTER specna*8
      integer*4 time,iT0,tNew,tOld,dDead,tDead,tStop,tt
      integer type,status
      CHARACTER TEX3*10,TEX4*13
      CHARACTER TEX5*7,computer*20
      
      COMMON/AXIS/iCE,itext,UNITx,UNITy,UNITx0,UNITy0
      CHARACTER UNITx*3,UNITy*3,UNITx0*3,UNITy0*3
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)


C Update mode tells how long to sleep between each refreshment of spectrum
C Update mode = -1   auto (vary between 0 and 20 seconds according to counts)
C Update mode = 10   10 sec. between each update

      mode=-1
      prc=0.1                                       !update if 1/10 more counts
      minTime=1                                     !min freezing time
      maxTime=3                                     !max freezing time
      nostop = 0

      type=1
      IF(ITYPE.GT.1)type=2
      iT0=time()              !seconds after January 1. 1970
      dDead=0                 !have to be subtracted to calculate average rates
      tDead=0
      laststartstop=0
      tStop=time()

      CALL INITG(NXold,NYold)


C Calculate when prc=10% more counts, sleep that time and then update spectra
C Starts estimating after the 3 first loops
      DO l=1,500000
C Checks if started or stopped
        laststartstop=istartstop
        istartstop=1                    !assumes started
        call makepath("UIO_APPLICATIONS","sirius/system/engine.lock",filnam)
        IF(iD.EQ.1)OPEN(UNIT=17,FILE=FILNAM,STATUS='OLD',ERR=99)
        call makepath("UIO_APPLICATIONS","sirius/offline/system/offline.lock",filnam)
        IF(iD.EQ.2)OPEN(UNIT=17,FILE=FILNAM,STATUS='OLD',ERR=99)
        GO TO 98
 99     istartstop=0                    !is stopped
 98     CLOSE(17)
        IF(laststartstop.EQ.0.AND.istartstop.EQ.0.AND.l.GT.1)THEN    !stop  -> stop
          nostop=nostop+1
          IF(nostop.GT.1)call zzzz(2)
          tt=time()
          dDead=tt-tStop
          tDead=tDead+dDead
          tStop=tt
          GO TO 44
        ENDIF
        IF(laststartstop.EQ.0.AND.istartstop.EQ.1)THEN               !stop  -> start
          dDead=0
          nostop=0
        ENDIF
        IF(laststartstop.EQ.1.AND.istartstop.EQ.0.AND.l.GT.1)THEN    !start -> stop
          tStop=time()
        ENDIF

        IF(  iTime.LE.0)iTime=minTime
        IF(dCounts.LT.1)iTime=maxTime
        IF(   mode.GT.0)iTime=mode

C Change y (z-scale) if incrementation of more than a factor of 2
        iScaleChange=0                       !no change
        IF(cNew.GT.2*Lastcount.OR.cNeW.LT.0.5*Lastcount)THEN
          iScaleChange=1                     !find new scale
          Lastcount=cNew
        ENDIF

        IF(l.EQ.1) iScaleChange=-1 
        IF(l.GT.1) call zzzz(iTime)       !waiting to update...zzz...

 44     CONTINUE

C Here comes the calls for display according to spectra
        IF(iD.EQ.1)THEN                      !SIRIUS spectrum
          call sirius_spectra(type,specna)
        ENDIF
        IF(iD.EQ.2)THEN                      !OFFLINE spectrum
          call offline_spectra(type,specna)
        ENDIF
		
C Tests if we have to project a matrix to make singles
        IF(ixy.EQ.1)THEN
          DO I=0,4095
            MSPEC(I)=0
            DO J=i1,i2
              MSPEC(I)=MSPEC(I)+MAT(I,J)
            ENDDO
          ENDDO
        ENDIF
        IF(ixy.EQ.2)THEN
          DO J=0,511
            MSPEC(j)=0
            DO I=i1,i2
              MSPEC(J)=MSPEC(J)+MAT(I,J)
            ENDDO
          ENDDO
        ENDIF

C Display spectrum
        IF(ITYPE.GT.1)THEN
          IF(Idistype.EQ.2)THEN
c            IF(l.GT.1)CALL ERASE
            call OUTLAY(icount,iScaleChange)
          ENDIF
          IF(Idistype.EQ.0.AND.ixy.EQ.0)THEN
            call DSPMA(icount,iScaleChange) 
          ENDIF
        ENDIF
        IF(ITYPE.EQ.1.OR.ixy.GT.0)THEN
          IF(l.GT.1)CALL ERASE
          call DSPSP(icount,iScaleChange)
        ENDIF

        cNew=icount
        tNeW=time()                             !seconds after January 1. 1970
        IF(l.EQ.1.OR.cNeW.LT.0.5*Lastcount)THEN !reset time and counts if spectrum is CLEARED
          iT0=tNew
          iC0=cNew
          dDead=0
          tDead=0
        ENDIF

        IF(l.GT.1)THEN                          !calculate how long to wait for new refresh
          dTime   =MAX(tNew-tOld-dDead,1)
          dCounts =MAX(cNew-cOld,0.0001)
          cAve    =(cNew+cOld)/2.0
          rate    =dCounts/dTime
          IF(dCounts.GE.1)THEN
            iTime   =(cAve*prc/rate)+0.5        !in seconds
            IF(iTime.LT.minTime)iTime=minTime
            IF(iTime.GT.maxTime)iTime=maxTime
          ENDIF
          tOld=tNew
          cOld=cNew
        ENDIF

        iDiff1=cNew
        iDiff2=max(tNew-iT0-tDead,0)
        averate=0.0
        IF(iDiff2.GT.0.)averate=FLOAT((MAX(INT(cNew)-iC0,0)))/FLOAT(iDiff2)

  33    laststartstop=istartstop

        CALL INITG(NX,NY)
        IF(NX.ne.NXold.OR.NY.ne.NYold)THEN  !have to go back and redraw spectra
          NXold=NX
          NYold=NY
          GO TO 44
        ENDIF

        IF(itext.EQ.1)THEN
          WRITE(TEX3,21)iDiff1
   21     FORMAT(I10)  
          TEX4=TEX3//'c'
          CALL MSPOT(NX-60,NY-20)
          CALL PUTG(TEX4,13,1,1)

          WRITE(TEX3,22)iDiff2
   22     FORMAT(I10)  
          TEX4=TEX3//'s'
          CALL MSPOT(NX-60,NY-30)
          CALL PUTG(TEX4,13,1,1)

          WRITE(TEX3,23)averate
   23     FORMAT(F10.2)  
          TEX4=TEX3//'c/s'
          CALL MSPOT(NX-60,NY-40)
          CALL PUTG(TEX4,13,1,1)

          WRITE(TEX3,24)rate
   24     FORMAT(F10.2)  
          TEX4=TEX3//'c/s'
          CALL MSPOT(NX-60,NY-50)
          CALL PUTG(TEX4,13,1,1)

          IF(istartstop.EQ.1)TEX5='started'
          IF(istartstop.EQ.0)TEX5='stopped'
          CALL MSPOT(NX-60,NY-60)
          CALL PUTG(TEX5,7,1,1)

        ENDIF
        CALL FINIG
      enddo

      write(6,*)'The update of spectra has now been performed 500000 times, and'
      write(6,*)'the automatic update mode has been terminated. If you want to'          
      write(6,*)'continue, type the UD command to get a new update process started.'
      write(6,*)'Have a nice day!'
      RETURN
      end

!     Routine to build filenames from an environment variable plus a
!     tail. Purpose is to avoid putting absolute paths into the
!     programs.
!
!     Example:
!     CHARACTER filnam*255
!     CALL MAKEPATH("UIO_APPLICATIONS","prog/dummydir/dummyfile",filnam)
!
!     In the special case of 'UIO_APPLICATIONS', "/Applications" is used
!     if the environment variable is not set.
      SUBROUTINE MAKEPATH(BASE,TAIL,FULL)
      CHARACTER BASE*(*), BASEENV*255, TAIL*(*), FULL*255
      INTEGER LB
      CALL getenv(BASE, BASEENV)
      CALL LENGDE(BASEENV,LB)
      if(LB.eq.0.and.base.eq.'UIO_APPLICATIONS') then
         BASEENV="/home/jorgenem/tools"
         CALL LENGDE(BASEENV,LB)
      end if
      FULL = BASEENV(1:LB)//'/'//TAIL
      RETURN
      END

       SUBROUTINE LENGDE(TEXT,LEN)
C Calculating the length of a string of characters             
       CHARACTER TEXT*80
       CHARACTER CH*1
       DO K=1,80
         CH=TEXT(K:K)
         IF(CH.EQ.' ')THEN
           LEN=K-1
           GO TO 20
         ENDIF
       ENDDO
   20  RETURN
       END


