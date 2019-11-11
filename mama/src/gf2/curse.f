      SUBROUTINE CURSE(IDATA)
C This program looks like a mess - and is a mess, but it works!!!

      INTEGER       MCH(2)
      REAL          PPOS(15)
      COMMON /MKRS/ MCH,PPOS

      CHARACTER*40 ANS
      DATA DX/0.0/
      COMMON /LUS/ IR,IW,IP,IG

      INTEGER XDIM,YDIM
      CHARACTER APP*4
      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
      COMMON/Sp2Dim/rMAT(2,0:4095,0:2047),APP(2048),XDIM,YDIM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/OL/I3,iRC,m1,m2
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh

C Assume you point at an icon
C  I-----I I-----I                          my1   
C  I     I I     I I--------I I--------I    my2
C  I-----I I-----I I--------I I--------I    my3
C   area    area     area       area        my4
C
C mx1   mx2 mx3 mx4 mx5    mx6 mx7    mx8 

      IDESTold=IDEST
      ITYPEold=ITYPE
      Istatus=-7     !to flag that green button is active
      CALL CLEANUP

      CALL INITG(nx,ny)             !Pixel size of window
      is=18

      mx1=2
      mx2=mx1+2*is
      mx3=mx2+4
      mx4=mx3+2*is
      mx5=mx4+4
      mx6=mx5+3*is
      mx7=mx6+4
      mx8=mx7+3*is
      mx10=nx-1
      mx9=mx10-13

      my1=ny+42
      my2=my1-0.7*is
      my3=my2-0.3*is
      my33=my3-4    !An extra -4 because we never get iy > spec. area

      IF(ITYPE.GT.1)WRITE(IW,1)
      IF(ITYPE.EQ.1)WRITE(IW,2)
 1    FORMAT('Type X or click on green button to exit')
 2    FORMAT('Type X or click on green button to exit'
     +     /,'Type T to give channel number')

C First case if typing e.g. CR 120
      IF(ITYPE.EQ.1.AND.IDATA.GT.0)THEN
        IF(IDATA.GE.0.AND.IDATA.LE.8191)THEN
          GO TO 75
        ENDIF
      ENDIF

C Loop with curser
999   CALL RETIC(X,Y,ANS)
      CALL CVXY(X,Y,ix,iy,1)
      IF(ANS(1:1).EQ.'X'.OR.ANS(1:1).EQ.'x') RETURN
      IF(ix.GE.mx9.AND.ix.LE.mx10.AND.iy.GE.my33) RETURN
      IF(ANS(1:1).EQ.'T'.OR.ANS(1:1).EQ.'t')THEN
c65      CALL ASK(21HGive channel number: ,21,ANS,NC)
65       CALL CASK('Give channel number: ',ANS,NC)
        IF (NC.EQ.0) GO TO 999
        CALL ININ(ANS,NC,IDATA,J1,J2,*65)
        ANS(1:1)=' '
      ENDIF
       
      IF(ix.GE.0.AND.iy.GE.my33.AND.ix.LE.mx8+2)THEN 
        IF(ix.GE.mx1-2.AND.ix.LE.mx2+2)THEN
          IDEST=1
          ITYPE=3
          XDIM=Idim(1,IDEST,1)
          YDIM=Idim(1,IDEST,2)
          Istatus=-7     !to flag that green button is active
          CALL SetMarker(-1,-1,-1)
          CALL CLEANUP
          CALL TOUCH
        ENDIF
        IF(ix.GE.mx3-2.AND.ix.LE.mx4+2)THEN
          IDEST=2
          ITYPE=3
          XDIM=Idim(1,IDEST,1)
          YDIM=Idim(1,IDEST,2)
          Istatus=-7     !to flag that green button is active
          CALL SetMarker(-1,-1,-1)
          CALL CLEANUP
          CALL TOUCH
        ENDIF
        IF(ix.GE.mx5-2.AND.ix.LE.mx6+2)THEN
          IDEST=1
          ITYPE=1
          MAXCH=Idim(2,IDEST,1)-1
          Istatus=-7     !to flag that green button is active
          CALL SetMarker(-1,-1,-1)
          CALL CLEANUP
          CALL TOUCH
        ENDIF
        IF(ix.GE.mx7-2.AND.ix.LE.mx8+2)THEN
          IDEST=2
          ITYPE=1
          MAXCH=Idim(2,IDEST,1)-1
          Istatus=-7     !to flag that green button is active
          CALL SetMarker(-1,-1,-1)
          CALL CLEANUP
          CALL TOUCH
        ENDIF
        GO TO 999
      ENDIF

C If it is an outlay spectrum or multiple spectrum, we return
      IF(Idistype.GT.2)THEN
        WRITE(6,69)
69      FORMAT('No information on number of counts for outlay (OL)'
     +     /,'or multiple display of many spectra and matrices.'
     +     /,'Please, display (DS) only one single spectrum or matrix')
        Istatus=1
        RETURN
      ENDIF

C Case if one has pointed on matrix/spectrum icon, goes back to original
      IF(IDEST.NE.IDESTold.OR.ITYPE.NE.ITYPEold)THEN
        IF(ITYPE.GT.1)THEN
          XDIM=Idim(1,IDEST,1)
          YDIM=Idim(1,IDEST,2)
        ELSE
          MAXCH=Idim(2,IDEST,1)-1
        ENDIF
        Istatus=-7     !to flag that green button is active
        CALL CLEANUP
      ENDIF

C Assume you point at a matrix
      IF(ITYPE.GT.1)THEN
        IF(IDATA.NE.0)RETURN
        iix=X
        iiy=Y
        Iregion=1            ! =1 means (x,y) within legal limits
        IF(iix.GE.0.AND.iix.LE.4095)THEN
      Ex=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)*iix+cal(1,IDEST,1,3)*iix*iix     
          WRITE(6,70)iix,Ex
70        FORMAT('x= ',I4,', energy = ',F10.3,' keV')
        ELSE
          Iregion=0
        ENDIF
        IF(iiy.GE.0.AND.iiy.LE.2047)THEN
      Ey=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)*iiy+cal(1,IDEST,2,3)*iiy*iiy     
          WRITE(6,71)iiy,Ey
71        FORMAT('y= ',I4,', energy = ',F10.3,' keV')
        ELSE
          Iregion=0 
        ENDIF
        IF(Iregion.EQ.1)WRITE(6,72)rMAT(IDEST,iix,iiy)
72      FORMAT('Number of counts= ',E13.6)
        GO TO 999
      ENDIF

C Assume you point at a spectrum
      IF(ITYPE.EQ.1.AND.IDATA.EQ.0)THEN
        iix=X
        IF(iix.GE.0.AND.iix.LE.8191)THEN
      Ex=cal(2,IDEST,1,1)+cal(2,IDEST,1,2)*iix+cal(2,IDEST,1,3)*iix*iix     
          WRITE(6,73)iix,rSPEC(IDEST,iix),Y,Ex
73        FORMAT('Ch =',I5,'  Counts =',E13.6,'  Y =',E13.6,'  Energy =',F9.2,' keV')
        ENDIF
        GO TO 999
      ENDIF

C Assume you have given the channel number by typing
75    IF(ITYPE.EQ.1.AND.IDATA.GT.0)THEN
        IF(IDATA.GE.0.AND.IDATA.LE.8191)THEN
          iix=IDATA
          ISAVE=MCH(1)
          MCH(1)=IDATA
          CALL DSPMKR(1)
          MCH(1)=ISAVE
      Ex=cal(2,IDEST,1,1)+cal(2,IDEST,1,2)*iix+cal(2,IDEST,1,3)*iix*iix     
          WRITE(6,74)iix,rSPEC(IDEST,iix),Ex
74        FORMAT('Ch =',I5,'  Counts =',E13.6,'  Energy =',F9.2,' keV')
        ENDIF
        IDATA=0
        GO TO 999
      ENDIF
      Istatus=0
      RETURN
      END

